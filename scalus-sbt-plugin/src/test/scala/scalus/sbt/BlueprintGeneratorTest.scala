package scalus.sbt

import org.scalatest.funsuite.AnyFunSuite
import sbt.io.IO
import sbt.util.{Level, Logger}
import scalus.sbt.fixture.DependencyEchoContract

import java.io.File
import java.nio.file.Files
import java.util.jar.{JarEntry, JarOutputStream}
import scala.collection.mutable.ArrayBuffer

/** The blueprint cache must not serve JSON from an older dependency. A contract's script comes
  * from the Scalus lowering and optimizer in a dependency, so a dependency change alone can change
  * the script while the project's own class files stay byte-identical.
  */
class BlueprintGeneratorTest extends AnyFunSuite {

    private val contractClass = classOf[DependencyEchoContract].getName

    /** A project as the `blueprint` task sees it: a classes directory that holds one contract, and
      * the directories the generator writes to.
      */
    private final class Project(root: File) {
        val classesDir = new File(root, "classes")
        val resourceRoot = new File(root, "resource_managed")
        val cacheDir = new File(root, "streams/scalus-blueprints")
        val blueprintFile =
            new File(resourceRoot, "META-INF/scalus/blueprints/scalus/sbt/fixture/DependencyEchoContract.json")
        val log = new RecordingLogger

        locally {
            val classFile = contractClass.replace('.', '/') + ".class"
            val in = getClass.getClassLoader.getResourceAsStream(classFile)
            try IO.transfer(in, new File(classesDir, classFile))
            finally in.close()
            IO.write(
              new File(classesDir, "META-INF/scalus/blueprint-modules"),
              s"$contractClass\tsrc/main/scala/DependencyEchoContract.scala\n"
            )
        }

        /** Runs the generator and returns the contract's blueprint JSON. */
        def blueprint(dependencies: Seq[File]): String = {
            BlueprintGenerator.generate(
              classesDir,
              dependencies,
              resourceRoot,
              cacheDir,
              projectName = "test-project",
              projectVersion = "0.1.0",
              scalaVer = "3.3.9",
              log
            )
            assert(blueprintFile.isFile, s"no blueprint was written; log:\n${log.lines.mkString("\n")}")
            IO.read(blueprintFile)
        }
    }

    private final class RecordingLogger extends Logger {
        val lines = ArrayBuffer.empty[String]
        def trace(t: => Throwable): Unit = lines += t.toString
        def success(message: => String): Unit = lines += message
        def log(level: Level.Value, message: => String): Unit = lines += s"[$level] $message"
    }

    /** A jar that provides the resource the contract echoes, as scalus-core provides the lowering. */
    private def dependencyJar(file: File, output: String): File = {
        IO.createDirectory(file.getParentFile)
        val out = new JarOutputStream(Files.newOutputStream(file.toPath))
        try {
            out.putNextEntry(new JarEntry(DependencyEchoContract.RESOURCE))
            out.write(output.getBytes("UTF-8"))
            out.closeEntry()
        } finally out.close()
        file
    }

    test("a dependency jar at a new path, as after a Scalus version bump, regenerates the blueprint") {
        IO.withTemporaryDirectory { root =>
            val project = new Project(root)
            val oldJar = dependencyJar(new File(root, "cache/scalus-core-1.0.jar"), "script-1.0")
            assert(project.blueprint(Seq(oldJar)) == """{"compiledCode":"script-1.0"}""")

            IO.delete(oldJar)
            val newJar = dependencyJar(new File(root, "cache/scalus-core-1.1.jar"), "script-1.1")
            assert(project.blueprint(Seq(newJar)) == """{"compiledCode":"script-1.1"}""")
        }
    }

    test("a dependency directory rebuilt at the same path regenerates the blueprint") {
        IO.withTemporaryDirectory { root =>
            // The class directory of a project this one `dependsOn`, rebuilt in place.
            val project = new Project(root)
            val dependency = new File(root, "core/classes")
            val output = new File(dependency, DependencyEchoContract.RESOURCE)
            IO.write(output, "script-a")
            assert(project.blueprint(Seq(dependency)) == """{"compiledCode":"script-a"}""")

            // Same size, so only the content and the later modification time differ.
            val builtAt = output.lastModified
            IO.write(output, "script-b")
            assert(output.setLastModified(builtAt + 2000))
            assert(project.blueprint(Seq(dependency)) == """{"compiledCode":"script-b"}""")
        }
    }

    test("class files rewritten with identical bytes keep the cached blueprint") {
        IO.withTemporaryDirectory { root =>
            val project = new Project(root)
            val dependency = dependencyJar(new File(root, "cache/scalus-core-1.0.jar"), "script-1.0")
            assert(project.blueprint(Seq(dependency)) == """{"compiledCode":"script-1.0"}""")

            // Only a regeneration replaces this marker.
            IO.write(project.blueprintFile, "from-cache")
            // A recompile that writes the same bytes again, later.
            BlueprintLayout.listFilesRecursively(project.classesDir).foreach { f =>
                val bytes = Files.readAllBytes(f.toPath)
                val writtenAt = f.lastModified
                Files.write(f.toPath, bytes)
                assert(f.setLastModified(writtenAt + 2000))
            }
            assert(project.blueprint(Seq(dependency)) == "from-cache")
        }
    }
}
