package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{Compile, Options}
import scalus.cardano.onchain.plutus.prelude.{Eq, List}
import scalus.uplc.PlutusV3
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

case class CPoint(x: BigInt, y: BigInt)

@Compile
object CPoint:
    given Eq[CPoint] = Eq.derived

@Compile
object ContainsEqHelpers:
    // The element type `A` is abstract inside this generic method, so `xs.contains(y)` exercises
    // `equalsRepr` on an abstract TypeVar element (the case Phase 2 must handle without `eq`).
    def genericContains[A](xs: List[A], y: A)(using Eq[A]): Boolean = xs.contains(y)

/** Phase 2: the `contains` intrinsic drops its `eq` argument and compares via `equalsRepr`. Tests
  * that this is correct for concrete and abstract element types. See v3-eq-eliminating.md.
  */
class ContainsEqLoweringTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options.release.copy(noWarn = true)

    test("contains on a concrete case-class element (structural, eq dropped)") {
        val compiled = PlutusV3.compile {
            List.single(CPoint(BigInt(1), BigInt(2))).contains(CPoint(BigInt(1), BigInt(2)))
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("contains on a concrete case-class element — negative") {
        val compiled = PlutusV3.compile {
            List.single(CPoint(BigInt(1), BigInt(2))).contains(CPoint(BigInt(9), BigInt(9)))
        }
        assert(compiled.program.term.evaluate == false.asTerm)
    }

    test("contains via a generic function (abstract element type)") {
        val compiled = PlutusV3.compile {
            ContainsEqHelpers.genericContains(
              List.single(CPoint(BigInt(1), BigInt(2))),
              CPoint(BigInt(1), BigInt(2))
            )
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }
}
