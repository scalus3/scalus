package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite

/** Audit finding H1 (compiler plugin audit): an extractor pattern whose subpattern count does not
  * match the constructor's parameter count (e.g. a boolean `unapply`) crashed the compiler with
  * `IllegalStateException: columnBinding.size != row.patterns.size` and a raw stack trace instead
  * of a positioned compile error.
  *
  * These tests compile snippets with the packaged plugin (see [[SnippetCompilation]]) and assert on
  * the collected diagnostics.
  */
class CustomUnapplyErrorTest extends AnyFunSuite with SnippetCompilation {

    test("control: case class constructor pattern compiles without errors") {
        val errors = compileSnippet(
          """import scalus.compiler.*
            |enum E:
            |    case A(s: String)
            |    case B
            |object W {
            |    val sir = compile { (e: E) =>
            |        e match
            |            case E.A(s) => BigInt(1)
            |            case _      => BigInt(0)
            |    }
            |}
            |""".stripMargin
        )
        assert(errors.isEmpty, s"control snippet must compile cleanly, got: $errors")
    }

    test("boolean unapply in pattern is rejected with an error, not a compiler crash") {
        val errors = compileSnippet(
          """import scalus.compiler.*
            |enum E:
            |    case A(s: String)
            |    case B
            |object OnlyX {
            |    def unapply(a: E.A): Boolean = a.s == "x"
            |}
            |object W {
            |    val sir = compile { (e: E) =>
            |        e match
            |            case OnlyX() => BigInt(1)
            |            case _       => BigInt(0)
            |    }
            |}
            |""".stripMargin
        )
        assert(errors.nonEmpty, "expected a compile error for a boolean unapply pattern")
        assert(
          errors.exists(_.contains("unapply")),
          s"expected an unapply-related error message, got: $errors"
        )
    }
}
