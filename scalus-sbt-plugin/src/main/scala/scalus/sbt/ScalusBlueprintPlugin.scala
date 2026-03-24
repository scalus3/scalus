package scalus.sbt

import sbt.*
import sbt.Keys.*
import sbt.complete.DefaultParsers.*

/** sbt plugin that adds `blueprint` and `deploy` tasks for Cardano smart contracts.
  *
  *
  * - `sbt blueprint` writes each contract's blueprint to
  *   `META-INF/scalus/blueprints/<ContractName>.json` in the classes directory.
  * - `sbt "deploy <ContractName> --network preview --blockfrost-key <key> --mnemonic '<words>' --address <addr>"`
  *   deploys a validator as a reference script UTXO.
  *
  * Environment variables can substitute CLI flags:
  *   - `CARDANO_NETWORK` for `--network` (default: "preview")
  *   - `BLOCKFROST_API_KEY` for `--blockfrost-key`
  *   - `CARDANO_MNEMONIC` for `--mnemonic`
  */
object ScalusBlueprintPlugin extends AutoPlugin {

    object autoImport {
        val blueprint =
            taskKey[Seq[java.io.File]](
              "Generate CIP-57 blueprint JSON for all Contract implementations"
            )
        val deploy =
            inputKey[Unit](
              "Deploy a contract as a reference script UTXO"
            )
    }

    lazy val blueprintTask: Def.Initialize[Task[Seq[java.io.File]]] = Def.task {
        val _ = (Compile / compile).value
        val cp = (Compile / fullClasspath).value.files
        val classesDir = (Compile / classDirectory).value
        val log = streams.value.log

        val outDir = classesDir / "META-INF" / "scalus" / "blueprints"
        IO.createDirectory(outDir)

        loadContracts(classesDir, cp, log).map { case (className, json) =>
            val file = outDir / (simpleName(className) + ".json")
            IO.write(file, json, java.nio.charset.StandardCharsets.UTF_8)
            log.info(s"Wrote ${file.relativeTo(classesDir).getOrElse(file)}")
            file
        }
    }

    lazy val deployTask: Def.Initialize[InputTask[Unit]] = Def.inputTask {
        val _ = (Compile / compile).value
        val cp = (Compile / fullClasspath).value.files
        val classesDir = (Compile / classDirectory).value
        val log = streams.value.log
        val args = spaceDelimited("<args>").parsed

        val (contractName, network, blockfrostKey, mnemonicStr, address) = parseDeployArgs(args)

        val classNames = readManifestClassNames(classesDir)
        if (classNames.isEmpty) sys.error("No Contract implementations found. Run `compile` first.")
        val className = resolveContractClass(contractName, classNames)

        log.info(s"Deploying contract '${simpleName(className)}' to $network...")

        val urls = cp.map(_.toURI.toURL).toArray
        val cl = new java.net.URLClassLoader(urls, ClassLoader.getPlatformClassLoader)

        // Use the `deployer` via reflection. This is sad but necessary since plugins are better written as
        // Scala 2.13.x programs, while the rest of Scalus is written in Scala 3.
        try {
            val deployerClass =
                try {
                    cl.loadClass("scalus.cardano.deploy.Deployer")
                }
                catch {
                    case _: ClassNotFoundException =>
                        sys.error(
                            "scalus-cardano-ledger is required for deploy. " +
                                "Add: libraryDependencies += \"org.scalus\" %% \"scalus-cardano-ledger\" % scalusVersion"
                        )
                }
            val deployMethod = deployerClass.getMethod(
                "deploy",
                classOf[String],
                classOf[String],
                classOf[String],
                classOf[String],
                classOf[String]
            )
            val txHash = deployMethod
                .invoke(null, className, network, blockfrostKey, mnemonicStr, address)
                .asInstanceOf[String]
            log.info(s"Deployed successfully! Transaction hash: $txHash")
        } catch {
            case e: java.lang.reflect.InvocationTargetException =>
                val cause = e.getCause
                sys.error(s"Deploy failed: ${cause.getMessage}")
            case e: Exception =>
                sys.error(s"Deploy failed: ${e.getClass.getSimpleName}: ${e.getMessage}")
        } finally {
            cl.close()
        }
    }

    import autoImport.*

    override lazy val projectSettings: Seq[Setting[_]] = Seq(
      blueprint := blueprintTask.value,
      deploy := deployTask.evaluated
    )

    /** Derive a simple file name from a fully qualified class name. */
    private def simpleName(className: String): String =
        className.stripSuffix("$").split('.').last

    private def readManifestClassNames(classesDir: java.io.File): Seq[String] = {
        val manifest = classesDir / "META-INF" / "scalus" / "blueprint-modules"
        if (!manifest.exists()) Seq.empty
        else
            IO.readLines(manifest)
                .filter(_.nonEmpty)
                .map(_.split('\t').head)
                .distinct
    }

    private def loadContracts(
        classesDir: java.io.File,
        cp: Seq[java.io.File],
        log: sbt.util.Logger
    ): Seq[(String, String)] = {
        val classNames = readManifestClassNames(classesDir)
        if (classNames.isEmpty) {
            log.warn("No Contract implementations found")
            Seq.empty
        } else {
            val urls = cp.map(_.toURI.toURL).toArray
            val cl = new java.net.URLClassLoader(urls, ClassLoader.getPlatformClassLoader)

            try {
                classNames.flatMap { className =>
                    try {
                        val cls = cl.loadClass(className)
                        val instance = cls.getField("MODULE$").get(null)
                        val method = cls.getMethod("blueprintJson")
                        val json = method.invoke(instance).asInstanceOf[String]
                        Some((className, json))
                    } catch {
                        case e: java.lang.reflect.InvocationTargetException =>
                            log.error(
                              s"Failed to load Contract $className: ${e.getCause.getMessage}"
                            )
                            None
                        case e: Exception =>
                            log.error(s"Failed to load Contract $className: ${e.getMessage}")
                            None
                    }
                }
            } finally {
                cl.close()
            }
        }
    }

    private def resolveContractClass(
        contractName: String,
        classNames: Seq[String]
    ): String = {
        classNames
            .find(cn => simpleName(cn) == contractName || cn == contractName)
            .getOrElse(
              sys.error(
                s"Contract '$contractName' not found. Available: ${classNames.map(simpleName).mkString(", ")}"
              )
            )
    }

    private def parseDeployArgs(args: Seq[String]): (String, String, String, String, String) = {
        var contractName: Option[String] = None
        var network: Option[String] = None
        var blockfrostKey: Option[String] = None
        var mnemonic: Option[String] = None
        var address: Option[String] = None

        val iter = args.iterator
        while (iter.hasNext) {
            iter.next() match {
                case "--network" =>
                    if (!iter.hasNext) sys.error("--network requires a value")
                    network = Some(iter.next())
                case "--blockfrost-key" =>
                    if (!iter.hasNext) sys.error("--blockfrost-key requires a value")
                    blockfrostKey = Some(iter.next())
                case "--mnemonic" =>
                    if (!iter.hasNext) sys.error("--mnemonic requires a value")
                    mnemonic = Some(iter.next())
                case "--address" =>
                    if (!iter.hasNext) sys.error("--address requires a value")
                    address = Some(iter.next())
                case name if contractName.isEmpty =>
                    contractName = Some(name)
                case other =>
                    sys.error(s"Unknown argument: $other")
            }
        }

        (
          contractName.getOrElse(sys.error("Contract name is required as the first argument")),
          network
              .orElse(sys.env.get("CARDANO_NETWORK"))
              .getOrElse("preview"),
          blockfrostKey
              .orElse(sys.env.get("BLOCKFROST_API_KEY"))
              .getOrElse(sys.error("--blockfrost-key or BLOCKFROST_API_KEY env var is required")),
          mnemonic
              .orElse(sys.env.get("CARDANO_MNEMONIC"))
              .getOrElse(sys.error("--mnemonic or CARDANO_MNEMONIC env var is required")),
          address.getOrElse(sys.error("--address is required"))
        )
    }
}
