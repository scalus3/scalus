package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite

class AlternativeAliasErrorTest extends AnyFunSuite with SnippetCompilation {
    private val diagnostic = "Aliases on alternative patterns are not supported in Scalus scripts."

    for pattern <- List("x @ (\"a\" | \"b\")", "(x @ (\"a\" | \"b\")): String") do
        test(s"alternative alias $pattern reports the unsupported-pattern error") {
            val errors = compileSnippet(
              s"""import scalus.compiler.*
                 |object W {
                 |  val sir = compile { (s: String) =>
                 |    s match
                 |      case $pattern => x
                 |      case _ => "other"
                 |  }
                 |}
                 |""".stripMargin
            )
            assert(errors.size == 1, s"expected one diagnostic, got: $errors")
            assert(errors.head.startsWith(diagnostic), s"unexpected diagnostic: $errors")
        }

    test("nested alternative alias in a constructor guard is rejected without a compiler crash") {
        val errors = compileSnippet(
          """import scalus.compiler.*
            |case class Box(value: String)
            |object W {
            |  val sir = compile { (box: Box) =>
            |    box match
            |      case Box(x @ ("a" | "b")) if x == "a" => x
            |      case _ => "other"
            |  }
            |}
            |""".stripMargin
        )
        assert(errors.size == 1, s"expected one diagnostic, got: $errors")
        assert(errors.head.startsWith(diagnostic), s"unexpected diagnostic: $errors")
    }

    test("unaliased alternatives can return the original scrutinee") {
        val errors = compileSnippet(
          """import scalus.compiler.*
            |object W {
            |  val sir = compile { (s: String) =>
            |    s match
            |      case "a" | "b" => s
            |      case _ => "other"
            |  }
            |}
            |""".stripMargin
        )
        assert(errors.isEmpty, s"workaround must compile, got: $errors")
    }
}
