package scalus.compiler.plugin

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}

import java.nio.file.Files
import scala.collection.mutable.ListBuffer

/** Shared helper for negative-compilation tests of the Scalus plugin: compiles a source snippet
  * with the packaged plugin and returns the collected error diagnostics.
  *
  * The forked test JVM receives the packaged plugin jar and the scalus-core classpath as system
  * properties `scalus.plugin.jar` / `scalus.test.classpath` (wired in build.sbt).
  */
trait SnippetCompilation {

    private class ErrorCollector extends Reporter {
        val collected: ListBuffer[String] = ListBuffer.empty
        override def doReport(dia: Diagnostic)(using Context): Unit =
            if dia.level >= dotty.tools.dotc.interfaces.Diagnostic.ERROR then
                collected += dia.message
    }

    /** Compile `source` with the Scalus plugin and return the error messages produced. */
    protected def compileSnippet(source: String): List[String] =
        compileSnippetWithOutput(source)._1

    /** Compile `source` with the Scalus plugin and return the error messages together with the
      * class output directory, so tests can inspect the compiled classes (e.g. load the generated
      * `sirModule`).
      */
    protected def compileSnippetWithOutput(
        source: String
    ): (List[String], java.nio.file.Path) = {
        val pluginJar = sys.props("scalus.plugin.jar")
        val classpath = sys.props("scalus.test.classpath")
        val dir = Files.createTempDirectory("scalus-plugin-neg-test")
        val src = dir.resolve("snippet.scala")
        Files.writeString(src, source)
        val out = Files.createDirectories(dir.resolve("out"))
        val reporter = new ErrorCollector
        new Driver().process(
          Array(
            s"-Xplugin:$pluginJar",
            "-classpath",
            classpath,
            "-d",
            out.toString,
            src.toString
          ),
          reporter
        )
        (reporter.collected.toList, out)
    }
}
