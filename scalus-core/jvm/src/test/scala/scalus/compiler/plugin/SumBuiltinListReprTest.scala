package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite

/** A builtin list holds constants only, and Plutus fails `mkCons` of a `constr` value. So
  * `@UplcRepr(SumBuiltinList(UplcConstr))` would compile to a script that fails on chain; the
  * plugin rejects it (F-83 in docs/internal/test-refactor/findings.md).
  */
class SumBuiltinListReprTest extends AnyFunSuite with SnippetCompilation {

    private def snippet(listRepr: String): String =
        s"""import scalus.compiler.{Compile, UplcRepr, UplcRepresentation}
           |import scalus.cardano.onchain.plutus.prelude.List
           |@Compile
           |object Defs {
           |    @UplcRepr(UplcRepresentation.UplcConstr)
           |    case class Tile(x: BigInt, y: BigInt)
           |    case class Board(@UplcRepr($listRepr) visited: List[Tile])
           |    def size(b: Board): BigInt = b.visited.length
           |}
           |""".stripMargin

    test("control: a native constr list of UplcConstr elements compiles") {
        val errors = compileSnippet(snippet("UplcRepresentation.UplcConstr"))
        assert(errors.isEmpty, s"control snippet must compile cleanly, got: $errors")
    }

    test("SumBuiltinList(UplcConstr) is rejected") {
        val errors = compileSnippet(
          snippet("UplcRepresentation.SumBuiltinList(UplcRepresentation.UplcConstr)")
        )
        assert(
          errors.exists(_.contains("SumBuiltinList(UplcConstr)) is not supported")),
          s"expected a SumBuiltinList(UplcConstr) rejection, got: $errors"
        )
    }
}
