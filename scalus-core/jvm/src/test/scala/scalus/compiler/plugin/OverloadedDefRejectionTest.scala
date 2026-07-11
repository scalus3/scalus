package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite

/** Audit finding E5: overloaded methods in a compiled object are keyed by name with no signature,
  * so they collide silently (the second overwrites the first, or lowering dies later with a
  * baffling "Cannot unify result type of apply"). They must be rejected with a clear compile error.
  *
  * These tests compile snippets with the packaged plugin (see [[SnippetCompilation]]) and assert on
  * the collected diagnostics.
  */
class OverloadedDefRejectionTest extends AnyFunSuite with SnippetCompilation {

    test("control: distinctly named methods compile without errors") {
        val errors = compileSnippet(
          """import scalus.compiler.Compile
            |@Compile
            |object NotOverloaded {
            |    def f(a: BigInt): BigInt = a
            |    def g(a: BigInt): BigInt = a + BigInt(1)
            |}
            |""".stripMargin
        )
        assert(errors.isEmpty, s"control snippet must compile cleanly, got: $errors")
    }

    test("overloaded methods in a @Compile object are rejected") {
        val errors = compileSnippet(
          """import scalus.compiler.Compile
            |@Compile
            |object Overloaded {
            |    def f(a: BigInt): BigInt = a
            |    def f(a: BigInt, b: BigInt): BigInt = a + b
            |}
            |""".stripMargin
        )
        assert(
          errors.exists(_.contains("Overloaded definitions")),
          s"expected an overload rejection error, got: $errors"
        )
    }
}
