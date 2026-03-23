package scalus.sbt

import sbt.*
import sbt.Keys.*

/** sbt plugin that adds a `blueprint` task for CIP-57 blueprint JSON.
  *
  * ```
  * enablePlugins(ScalusBlueprintPlugin)
  * ```
  *
  * Running `sbt blueprint` writes each contract's blueprint to
  * `META-INF/scalus/blueprints/<ContractName>.json` in the classes directory,
  * so it ends up in the published JAR.
  */
object ScalusBlueprintPlugin extends AutoPlugin {

    object autoImport {
        val blueprint =
            taskKey[Seq[java.io.File]](
              "Generate CIP-57 blueprint JSON for all Contract implementations"
            )
    }

    import autoImport.*

    override lazy val projectSettings: Seq[Setting[_]] = Seq(
      blueprint := blueprintTask.value
    )

    private def loadContracts(
        classesDir: java.io.File,
        cp: Seq[java.io.File],
        log: sbt.util.Logger
    ): Seq[(String, String)] = {
        val manifest = classesDir / "META-INF" / "scalus" / "blueprint-modules"
        if (!manifest.exists()) {
            log.warn("No Contract implementations found")
            Seq.empty
        } else {
            val classNames =
                IO.readLines(manifest)
                    .filter(_.nonEmpty)
                    .map(_.split('\t').head)
                    .distinct

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

    /** Derive a simple file name from a fully qualified class name. */
    private def simpleName(className: String): String =
        className.stripSuffix("$").split('.').last

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
}
