package scalus.sbt

import sbt.*
import sbt.Keys.*
import sbt.complete.DefaultParsers.*
// sbt2-compat shim: lets this single source set compile against both sbt 1 and sbt 2 APIs.
// In particular `toFiles` bridges the sbt 2 Classpath type change (Attributed[HashedVirtualFileRef]).
import sbtcompat.PluginCompat.*

/** sbt plugin that adds blueprint generation, pinning and deploy tasks for Cardano smart
  * contracts.
  *
  * Blueprint output has two layers:
  *
  *   - `blueprint` (automatic, cached): generates each contract's CIP-57 JSON into
  *     `resourceManaged/main/META-INF/scalus/blueprints/<package>/<Contract>.json` plus an
  *     aggregate document at `resourceManaged/main/plutus.json`. Registered as a resource
  *     generator, so `package`/`publish` embed both in the JAR (aggregate at `/plutus.json`);
  *     every JAR describes exactly its own code. Generation is skipped when a content
  *     fingerprint of `classDirectory` is unchanged. Git is never touched. Opt out with
  *     `blueprint / skip := true` or the `SCALUS_SKIP_BLUEPRINT` env var.
  *   - `blueprintPin` (deliberate): copies the current set to committed locations –
  *     `<base>/plutus.json` (the path Aiken tooling expects) and
  *     `<base>/blueprints/<package>/<Contract>.json`. Run it when a usable version should be
  *     pinned, then commit; git history is the pin history. `blueprintCheck` fails when pins
  *     are stale (for release pipelines). In cross-built projects set
  *     `blueprintScalaVersion := Some("3.3.7")` so only the primary Scala baseline is pinned.
  *
  * Version provenance lives inside the JSON, never in filenames: Scalus version in
  * `preamble.compiler.version`, Scala version in the top-level `scalus.scalaVersion`
  * extension key (valid CIP-57: the schema allows extra root keys).
  *
  * `sbt "deploy <ContractName> --network preview --blockfrost-key <key> --mnemonic '<words>'"`
  * deploys a validator as a reference script UTXO at the sender's own base address.
  *
  * Environment variables can substitute deploy CLI flags:
  *   - `CARDANO_NETWORK` for `--network` (default: "preview")
  *   - `BLOCKFROST_API_KEY` for `--blockfrost-key`
  *   - `CARDANO_MNEMONIC` for `--mnemonic`
  */
object ScalusSbtPlugin extends AutoPlugin {

    object autoImport {
        val blueprint =
            taskKey[Seq[java.io.File]](
              "Generate CIP-57 blueprint JSON for all Contract implementations"
            )
        val blueprintPin =
            taskKey[Seq[java.io.File]](
              "Copy generated blueprints to the committed pin locations (plutus.json and blueprints/)"
            )
        val blueprintCheck =
            taskKey[Unit](
              "Fail if the committed blueprint pins differ from freshly generated blueprints"
            )
        val blueprintScalaVersion =
            settingKey[Option[String]](
              "If set, blueprintPin refuses to run (and blueprintCheck skips) under any other Scala version"
            )
        val deploy =
            inputKey[Unit](
              "Deploy a contract as a reference script UTXO"
            )
    }

    import autoImport.*

    lazy val blueprintTask: Def.Initialize[Task[Seq[java.io.File]]] = Def.task {
        val _ = (Compile / compile).value
        implicit val conv: xsbti.FileConverter = fileConverter.value
        val classesDir = (Compile / classDirectory).value
        // Use dependencyClasspath + classDirectory, NOT fullClasspath: fullClasspath pulls
        // this project's exportedProducts -> products -> copyResources -> resources ->
        // resourceGenerators, which cycles once this task is registered as a resource
        // generator (Task 3). dependencyClasspath excludes the project's own products; we
        // add classDirectory so the project's compiled Contract classes are loadable.
        val cp = toFiles((Compile / dependencyClasspath).value) :+ classesDir
        val resourceRoot = (Compile / resourceManaged).value
        val cacheDir = streams.value.cacheDirectory / "scalus-blueprints"
        val log = streams.value.log
        generateBlueprints(
          classesDir,
          cp,
          resourceRoot,
          cacheDir,
          projectName = name.value,
          projectVersion = version.value,
          scalaVer = scalaVersion.value,
          log
        )
    }

    /** Resource-generator wrapper around `blueprint`. Runs as part of `resources`
      * (so `package`/`publish`/`run`/`test` embed the JSON), unless suppressed by
      * `blueprint / skip := true` or the `SCALUS_SKIP_BLUEPRINT` env var.
      *
      * Uses `Def.taskIf` so the gated branch is genuinely not evaluated: a plain
      * `Def.task { if (...) ... else blueprint.value }` would still run `blueprint`,
      * because `.value` lifts an unconditional task dependency.
      */
    lazy val blueprintGenerator: Def.Initialize[Task[Seq[java.io.File]]] = Def.taskIf {
        if ((blueprint / skip).value || sys.env.contains("SCALUS_SKIP_BLUEPRINT"))
            Seq.empty[java.io.File]
        else
            blueprint.value
    }

    /** Copy the freshly generated blueprints to the committed pin locations:
      * `<base>/plutus.json` (aggregate, the path Aiken tooling expects) and
      * `<base>/blueprints/<package>/<Contract>.json`. A deliberate act – run it when a
      * usable version should be pinned, then commit the result; git history is the pin
      * history. Stale pins of renamed/deleted contracts are pruned.
      */
    lazy val blueprintPinTask: Def.Initialize[Task[Seq[java.io.File]]] = Def.task {
        val generated = blueprint.value
        val log = streams.value.log
        blueprintScalaVersion.value.foreach { required =>
            if (scalaVersion.value != required)
                sys.error(
                  s"blueprintPin is restricted to Scala $required (blueprintScalaVersion), " +
                      s"but the current scalaVersion is ${scalaVersion.value}. " +
                      s"Run `++$required blueprintPin` instead."
                )
        }
        val resourceRoot = (Compile / resourceManaged).value
        val genDir = resourceRoot / "META-INF" / "scalus" / "blueprints"
        val base = baseDirectory.value
        val pinDir = base / "blueprints"
        val copied = scala.collection.mutable.ListBuffer.empty[java.io.File]
        val genAgg = resourceRoot / "plutus.json"
        if (genAgg.isFile) {
            IO.copyFile(genAgg, base / "plutus.json")
            copied += base / "plutus.json"
        }
        val perContract = generated.flatMap(f => IO.relativize(genDir, f).map(rel => (f, rel)))
        perContract.foreach { case (f, rel) =>
            val dest = pinDir / rel
            IO.copyFile(f, dest)
            copied += dest
        }
        BlueprintLayout
            .pruneStale(pinDir, perContract.map { case (_, rel) => pinDir / rel }.toSet)
            .foreach(f => log.info(s"Pruned stale pin $f"))
        log.info(
          s"Pinned ${copied.size} blueprint file(s); review and commit plutus.json and blueprints/"
        )
        copied.toList
    }

    /** Fail when the committed pins differ from freshly generated blueprints. Intended for
      * release pipelines (it cannot stay green between pins under git-derived snapshot
      * versions). Skips with a note under a non-primary Scala version in cross-builds, so
      * `+blueprintCheck` does not produce false failures.
      *
      * `Def.taskIf` so the skipped branch genuinely does not evaluate `blueprint`.
      */
    lazy val blueprintCheckTask: Def.Initialize[Task[Unit]] = Def.taskIf {
        if (blueprintScalaVersion.value.exists(_ != scalaVersion.value)) {
            streams.value.log.info(
              s"blueprintCheck skipped: pins are maintained under Scala " +
                  s"${blueprintScalaVersion.value.getOrElse("?")}, current is ${scalaVersion.value}"
            )
        } else {
            val generated = blueprint.value
            val log = streams.value.log
            val resourceRoot = (Compile / resourceManaged).value
            val genDir = resourceRoot / "META-INF" / "scalus" / "blueprints"
            val base = baseDirectory.value
            val pinDir = base / "blueprints"
            def sameBytes(a: java.io.File, b: java.io.File): Boolean =
                a.isFile && b.isFile && java.util.Arrays.equals(IO.readBytes(a), IO.readBytes(b))
            val problems = scala.collection.mutable.ListBuffer.empty[String]
            val genAgg = resourceRoot / "plutus.json"
            if (genAgg.isFile && !sameBytes(genAgg, base / "plutus.json"))
                problems += s"${base / "plutus.json"} is missing or stale"
            val perContract = generated.flatMap(f => IO.relativize(genDir, f).map(rel => (f, rel)))
            perContract.foreach { case (f, rel) =>
                if (!sameBytes(f, pinDir / rel)) problems += s"${pinDir / rel} is missing or stale"
            }
            val expected = perContract.map { case (_, rel) => (pinDir / rel).getCanonicalFile }.toSet
            BlueprintLayout.listFilesRecursively(pinDir).foreach { f =>
                if (f.getName.endsWith(".json") && !expected.contains(f.getCanonicalFile))
                    problems += s"$f is pinned but no longer generated"
            }
            if (problems.nonEmpty)
                sys.error(
                  ("Pinned blueprints are stale; run `blueprintPin` and commit:" +: problems.toList)
                      .mkString("\n  ")
                )
            log.info("Pinned blueprints are up to date")
        }
    }

    lazy val deployTask: Def.Initialize[InputTask[Unit]] = Def.inputTask {
        val _ = (Compile / compile).value
        implicit val conv: xsbti.FileConverter = fileConverter.value
        val cp = toFiles((Compile / fullClasspath).value)
        val classesDir = (Compile / classDirectory).value
        val log = streams.value.log
        val args = spaceDelimited("<args>").parsed

        val (contractName, network, blockfrostKey, mnemonicStr) = parseDeployArgs(args)

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
                classOf[String]
            )
            val txHash = deployMethod
                .invoke(null, className, network, blockfrostKey, mnemonicStr)
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

    override lazy val projectSettings: Seq[Setting[_]] = Seq(
      // Def.uncached opts these out of sbt 2's task cache: they return Seq[File] (not a
      // cacheable output type) and write files, and `deploy` performs network I/O. No-op on
      // sbt 1 (via sbt2-compat). Work-skipping for `blueprint` comes from its own content
      // fingerprint of classDirectory, not from sbt's task cache.
      blueprint := Def.uncached(blueprintTask.value),
      // Default-on; opt out per project with `blueprint / skip := true`. Defined at project (Zero)
      // config so the generator's Compile-scoped read delegates to it AND a user's Zero-scoped
      // override is honored.
      blueprint / skip := false,
      blueprintScalaVersion := None,
      blueprintPin := Def.uncached(blueprintPinTask.value),
      blueprintCheck := Def.uncached(blueprintCheckTask.value),
      // Embed blueprints in the JAR via the resources pipeline.
      Compile / resourceGenerators += blueprintGenerator.taskValue,
      deploy := Def.uncached(deployTask.evaluated)
    )

    /** Derive a simple file name from a fully qualified class name. */
    private def simpleName(className: String): String =
        className.stripSuffix("$").split('.').last

    /** Generate per-contract blueprints (package-nested) plus an aggregate `plutus.json`
      * under `resourceRoot`, skipping all work when the content fingerprint of `classesDir`
      * matches the previous run.
      *
      * Layout:
      *   - `resourceRoot/META-INF/scalus/blueprints/<package>/<Contract>.json` per contract
      *   - `resourceRoot/plutus.json` aggregate (JAR root; Aiken-style single document)
      *
      * Stale outputs from renamed/deleted contracts are pruned so the JAR always holds
      * exactly the current set. Returns the produced (or cached) files.
      */
    private def generateBlueprints(
        classesDir: java.io.File,
        cp: Seq[java.io.File],
        resourceRoot: java.io.File,
        cacheDir: java.io.File,
        projectName: String,
        projectVersion: String,
        scalaVer: String,
        log: sbt.util.Logger
    ): Seq[java.io.File] = {
        val outDir = resourceRoot / "META-INF" / "scalus" / "blueprints"
        val aggregateFile = resourceRoot / "plutus.json"
        val header =
            s"scheme=${BlueprintLayout.Scheme};scala=$scalaVer;version=$projectVersion;name=$projectName"
        val classFiles = BlueprintLayout.listFilesRecursively(classesDir)
        val fp = BlueprintLayout.fingerprint(header, classesDir, classFiles)
        val fpFile = cacheDir / "fingerprint"

        readCachedOutputs(fpFile, fp, resourceRoot) match {
            case Some(cached) =>
                log.debug("Blueprints are up to date; skipping generation")
                cached
            case None =>
                val classNames = readManifestClassNames(classesDir).sorted
                if (classNames.isEmpty) {
                    log.warn("No Contract implementations found")
                    Seq.empty
                } else {
                    IO.createDirectory(outDir)
                    val urls = cp.map(_.toURI.toURL).toArray
                    val cl = new java.net.URLClassLoader(urls, ClassLoader.getPlatformClassLoader)
                    try {
                        val jsons = loadContractJsons(cl, classNames, log)
                        val tool = BlueprintToolBridge.load(cl, log)
                        val utf8 = java.nio.charset.StandardCharsets.UTF_8
                        val perContract = jsons.map { case (className, json) =>
                            val file = outDir / BlueprintLayout.contractRelativePath(className)
                            IO.write(file, tool.stamp(json, scalaVer), utf8)
                            log.info(s"Wrote ${file.relativeTo(resourceRoot).getOrElse(file)}")
                            file
                        }
                        val aggregated =
                            tool.aggregate(jsons.map(_._2), projectName, projectVersion, scalaVer)
                                .map { json =>
                                    IO.write(aggregateFile, json, utf8)
                                    log.info(
                                      s"Wrote ${aggregateFile.relativeTo(resourceRoot).getOrElse(aggregateFile)}"
                                    )
                                    aggregateFile
                                }
                        val produced = perContract ++ aggregated.toSeq
                        BlueprintLayout
                            .pruneStale(outDir, perContract.toSet)
                            .foreach(f => log.info(s"Pruned stale blueprint $f"))
                        // Cache only complete, error-free runs so contracts that failed to load
                        // are retried on the next build instead of being silently skipped.
                        if (jsons.size == classNames.size)
                            writeCachedOutputs(fpFile, fp, resourceRoot, produced)
                        produced
                    } finally {
                        cl.close()
                    }
                }
        }
    }

    /** Load each Contract's blueprint JSON via reflection. Failures are logged and skipped. */
    private def loadContractJsons(
        cl: ClassLoader,
        classNames: Seq[String],
        log: sbt.util.Logger
    ): Seq[(String, String)] =
        classNames.flatMap { className =>
            try {
                val cls = cl.loadClass(className)
                val instance = cls.getField("MODULE$").get(null)
                val method = cls.getMethod("blueprintJson")
                val json = method.invoke(instance).asInstanceOf[String]
                Some((className, json))
            } catch {
                case e: java.lang.reflect.InvocationTargetException =>
                    log.error(s"Failed to load Contract $className: ${e.getCause.getMessage}")
                    None
                case e: Exception =>
                    log.error(s"Failed to load Contract $className: ${e.getMessage}")
                    None
            }
        }

    /** Cache file format: fingerprint on the first line, then one produced path per line,
      * relative to the managed-resource root. A hit requires an identical fingerprint AND
      * every recorded file still present on disk.
      */
    private def readCachedOutputs(
        fpFile: java.io.File,
        fp: String,
        resourceRoot: java.io.File
    ): Option[Seq[java.io.File]] =
        if (!fpFile.isFile) None
        else
            IO.readLines(fpFile) match {
                case `fp` :: rest =>
                    val files = rest.map(resourceRoot / _)
                    if (files.forall(_.isFile)) Some(files) else None
                case _ => None
            }

    private def writeCachedOutputs(
        fpFile: java.io.File,
        fp: String,
        resourceRoot: java.io.File,
        produced: Seq[java.io.File]
    ): Unit = {
        val rels = produced.flatMap(f => IO.relativize(resourceRoot, f))
        IO.writeLines(fpFile, (fp +: rels).toList)
    }

    /** Reflective bridge to scalus-core's `BlueprintTool`. Degrades gracefully when the
      * project depends on a scalus version that predates it: per-contract files are written
      * unstamped and no aggregate is produced.
      */
    private trait BlueprintToolBridge {
        def stamp(json: String, scalaVer: String): String
        def aggregate(
            jsons: Seq[String],
            title: String,
            version: String,
            scalaVer: String
        ): Option[String]
    }

    private object BlueprintToolBridge {
        def load(cl: ClassLoader, log: sbt.util.Logger): BlueprintToolBridge =
            try {
                val cls = cl.loadClass("scalus.cardano.blueprint.BlueprintTool$")
                val instance = cls.getField("MODULE$").get(null)
                val stampM = cls.getMethod("stampScalaVersion", classOf[String], classOf[String])
                val aggM = cls.getMethod(
                  "aggregate",
                  classOf[java.util.List[_]],
                  classOf[String],
                  classOf[String],
                  classOf[String]
                )
                new BlueprintToolBridge {
                    def stamp(json: String, scalaVer: String): String =
                        stampM.invoke(instance, json, scalaVer).asInstanceOf[String]
                    def aggregate(
                        jsons: Seq[String],
                        title: String,
                        version: String,
                        scalaVer: String
                    ): Option[String] = {
                        val list = new java.util.ArrayList[String]
                        jsons.foreach(j => list.add(j))
                        Some(aggM.invoke(instance, list, title, version, scalaVer).asInstanceOf[String])
                    }
                }
            } catch {
                case _: ClassNotFoundException | _: NoSuchMethodException =>
                    log.warn(
                      "scalus-core on the classpath predates BlueprintTool: blueprints are " +
                          "written without scalus.scalaVersion and no aggregate plutus.json is " +
                          "produced. Upgrade the scalus dependency for full output."
                    )
                    new BlueprintToolBridge {
                        def stamp(json: String, scalaVer: String): String = json
                        def aggregate(
                            jsons: Seq[String],
                            title: String,
                            version: String,
                            scalaVer: String
                        ): Option[String] = None
                    }
            }
    }

    private def readManifestClassNames(classesDir: java.io.File): Seq[String] = {
        val manifest = classesDir / "META-INF" / "scalus" / "blueprint-modules"
        if (!manifest.exists()) Seq.empty
        else
            IO.readLines(manifest)
                .filter(_.nonEmpty)
                .map(_.split('\t').head)
                .distinct
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

    private def parseDeployArgs(args: Seq[String]): (String, String, String, String) = {
        var contractName: Option[String] = None
        var network: Option[String] = None
        var blockfrostKey: Option[String] = None
        var mnemonic: Option[String] = None

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
              .getOrElse(sys.error("--mnemonic or CARDANO_MNEMONIC env var is required"))
        )
    }
}

/** Legacy alias for [[ScalusSbtPlugin]] — prefer `ScalusSbtPlugin` in new builds.
  * Existing builds that `enablePlugins(ScalusBlueprintPlugin)` keep working unchanged.
  *
  * Intentionally not `@deprecated`: sbt auto-imports every AutoPlugin's `autoImport`,
  * so deprecating this object would emit a warning in every downstream build — even
  * ones already on `ScalusSbtPlugin`. The alias is kept silently for back-compat.
  *
  * The settings are delegated rather than expressed via `requires`: in sbt,
  * enabling a plugin whose required plugins are not separately enabled is an
  * error, so delegation is what preserves backwards compatibility.
  */
object ScalusBlueprintPlugin extends AutoPlugin {
    val autoImport = ScalusSbtPlugin.autoImport
    override lazy val projectSettings: Seq[Setting[_]] = ScalusSbtPlugin.projectSettings
}
