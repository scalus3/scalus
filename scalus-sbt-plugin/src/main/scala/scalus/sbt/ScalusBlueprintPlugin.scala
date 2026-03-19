package scalus.sbt

import sbt.*
import sbt.Keys.*

/** sbt plugin that adds a `blueprint` task for printing CIP-57 blueprint JSON.
  *
  * ```
  * enablePlugins(ScalusBlueprintPlugin)
  * ```
  */
object ScalusBlueprintPlugin extends AutoPlugin {

    object autoImport {
        val blueprint =
            taskKey[Unit](
              "Generate and print CIP-57 blueprint JSON for all Contract implementations"
            )
    }

    import autoImport.*

    override lazy val projectSettings: Seq[Setting[_]] = Seq(
      blueprint := blueprintTask.value
    )

    lazy val blueprintTask: Def.Initialize[Task[Unit]] = Def.task {
        val _ = (Compile / compile).value
        val cp = (Compile / fullClasspath).value.files
        val classesDir = (Compile / classDirectory).value
        val log = streams.value.log

        val manifest = classesDir / "META-INF" / "scalus" / "blueprint-modules"
        if (!manifest.exists()) {
            log.warn("No Contract implementations found")
        } else {
            val classNames =
                IO.readLines(manifest)
                    .filter(_.nonEmpty)
                    .map(_.split('\t').head)
                    .distinct

            val urls = cp.map(_.toURI.toURL).toArray
            val cl = new java.net.URLClassLoader(urls, ClassLoader.getPlatformClassLoader)

            try {
                classNames.foreach { className =>
                    try {
                        val cls = cl.loadClass(className)
                        val instance = cls.getField("MODULE$").get(null)
                        val method = cls.getMethod("blueprintJson")
                        val json = method.invoke(instance).asInstanceOf[String]
                        println(json)
                    } catch {
                        case e: java.lang.reflect.InvocationTargetException =>
                            log.error(
                              s"Failed to load Contract $className: ${e.getCause.getMessage}"
                            )
                        case e: Exception =>
                            log.error(s"Failed to load Contract $className: ${e.getMessage}")
                    }
                }
            } finally {
                cl.close()
            }
        }
    }
}
