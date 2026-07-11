package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite

/** Audit finding E2: by-name parameters (`b: => T`) were silently compiled strict, so code passing
  * JVM tests behaved differently on-chain. They must be rejected with a positioned compile error
  * instead.
  *
  * These tests compile snippets with the packaged plugin (see [[SnippetCompilation]]) and assert on
  * the collected diagnostics.
  */
class ByNameParamErrorTest extends AnyFunSuite with SnippetCompilation {

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
