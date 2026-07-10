package scalus.compiler.plugin

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.collection.mutable.ListBuffer

/** Audit finding E2: by-name parameters (`b: => T`) were silently compiled strict, so code passing
  * JVM tests behaved differently on-chain. They must be rejected with a positioned compile error
  * instead.
  *
  * These tests compile snippets with the packaged plugin (system properties `scalus.plugin.jar` /
  * `scalus.test.classpath` are set for the forked test JVM in build.sbt) and assert on the
  * collected diagnostics.
  */
class ByNameParamErrorTest extends AnyFunSuite {

    private class ErrorCollector extends Reporter {
        val collected: ListBuffer[String] = ListBuffer.empty
        override def doReport(dia: Diagnostic)(using Context): Unit =
            if dia.level >= dotty.tools.dotc.interfaces.Diagnostic.ERROR then
                collected += dia.message
    }

    private def compileSnippet(source: String): List[String] = {
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
        reporter.collected.toList
    }

    test("control: strict parameters compile without errors") {
        val errors = compileSnippet(
          """import scalus.compiler.Compile
            |@Compile
            |object Control {
            |    def strictOr(a: Boolean, b: Boolean): Boolean = if a then true else b
            |}
            |""".stripMargin
        )
        assert(errors.isEmpty, s"control snippet must compile cleanly, got: $errors")
    }

    test("by-name parameter in @Compile object def is rejected") {
        val errors = compileSnippet(
          """import scalus.compiler.Compile
            |@Compile
            |object Repro {
            |    def lazyOr(a: Boolean, b: => Boolean): Boolean = if a then true else b
            |}
            |""".stripMargin
        )
        assert(
          errors.exists(m => m.contains("By-name parameter") && m.contains("inline")),
          s"expected a by-name rejection error, got: $errors"
        )
    }

    test("by-name parameter in local def inside compile block is rejected") {
        val errors = compileSnippet(
          """import scalus.compiler.compile
            |object Wrapper {
            |    val sir = compile {
            |        def lazyOr(a: Boolean, b: => Boolean): Boolean = if a then true else b
            |        lazyOr(true, false)
            |    }
            |}
            |""".stripMargin
        )
        assert(
          errors.exists(m => m.contains("By-name parameter") && m.contains("inline")),
          s"expected a by-name rejection error, got: $errors"
        )
    }
}
