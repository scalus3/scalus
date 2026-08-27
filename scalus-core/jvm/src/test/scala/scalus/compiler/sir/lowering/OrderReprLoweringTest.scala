package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.compiler.{Compile, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.{PlutusV3, Program, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{FromData, ToData}
import scalus.uplc.eval.{PlutusVM, Result}

/** `Order` carries `@UplcRepr(UplcRepresentation.UplcConstr)`, so its values are native
  * `constr N []` terms and matching on them is a single `case`. Before that annotation every
  * comparison round-tripped through `Data`, paying `unConstrData` + `fstPair` per call.
  */
@Compile
object OrderReprDefs {

    def threeWay(a: BigInt, b: BigInt): BigInt = (a <=> b) match
        case Order.Less    => BigInt(1)
        case Order.Equal   => BigInt(2)
        case Order.Greater => BigInt(3)

    def less(a: BigInt, b: BigInt): Boolean = (a <=> b).isLess

    def chain(a: BigInt, b: BigInt, c: BigInt, d: BigInt): Boolean =
        ((a <=> b) ifEqualThen (c <=> d)).isLess

    /** Comparing an `Order` against a bare nullary constructor. This is the shape that needs the
      * `ProdUplcConstr -> SumUplcConstr` arm in `ProductCaseEmitter.emitConvert`: the left side is
      * typed at the sum `Order`, the right side at the variant `Order.Less`.
      */
    def eqLiteral(a: BigInt, b: BigInt): Boolean = (a <=> b) === Order.Less

    /** `Order` values held in a container, then matched. Exercises element-repr inference and the
      * `SumUplcConstr` <-> container-element boundary rather than the direct scrutinee path.
      */
    def viaList(a: BigInt, b: BigInt, c: BigInt, d: BigInt): BigInt = {
        val orders = List.Cons(a <=> b, List.single(c <=> d))
        orders.foldLeft(BigInt(0)) { (acc, o) =>
            acc + (o match
                case Order.Less    => BigInt(1)
                case Order.Equal   => BigInt(2)
                case Order.Greater => BigInt(3))
        }
    }
}

/** A user enum whose `Ord` mixes delegated branches (`x <=> y`) with bare `Order` constructors —
  * the `NormalizedInterval` shape that first exposed the missing conversion.
  */
enum OrderReprKind derives ToData, FromData:
    case ClosedRange(lower: BigInt, upper: BigInt)
    case FromNegInf(upper: BigInt)
    case Always

@Compile
object OrderReprKindOps {
    import OrderReprKind.*

    given Ord[OrderReprKind] = (lhs: OrderReprKind, rhs: OrderReprKind) =>
        lhs match
            case ClosedRange(x1, x2) =>
                rhs match
                    case ClosedRange(y1, y2) => (x1 <=> y1) ifEqualThen (x2 <=> y2)
                    case _                   => Order.Less
            case FromNegInf(x1) =>
                rhs match
                    case ClosedRange(_, _) => Order.Greater
                    case FromNegInf(y1)    => x1 <=> y1
                    case _                 => Order.Less
            case Always =>
                rhs match
                    case Always => Order.Equal
                    case _      => Order.Greater

    def isLess(a: OrderReprKind, b: OrderReprKind): Boolean = (a <=> b) === Order.Less
}

class OrderReprLoweringTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options.release.copy(noWarn = true)

    private def eval(t: Term, args: Term*): Term =
        args.foldLeft(t)(_ $ _).evaluateDebug match
            case Result.Success(v, _, _, _) => v
            case other                      => fail(s"evaluation failed: $other")

    private def i(n: Int) = BigInt(n).asTerm

    // The whole point of `@UplcRepr(UplcConstr)` on `Order`: a comparison result is a native
    // `constr N []` and matching on it is one `case`. Before the annotation this was
    //   (case [fstPair [unConstrData
    //         (case [lessThanInteger a b]
    //             (case [lessThanInteger b a] (con data (Constr 1 [])) (con data (Constr 2 [])))
    //           (con data (Constr 0 [])))]] (con integer 1) (con integer 2) (con integer 3))
    // paying `unConstrData` + `fstPair` + two forces + two applies per comparison.
    //
    // Compiled untagged so the assertion covers the lowering alone, not the `_scalusTag` wrapper.
    test("Order lowers to native constr/case, not a Data round-trip") {
        given Options = Options.releaseUntagged.copy(noWarn = true)
        val actual = PlutusV3.compile(OrderReprDefs.threeWay).program
        val expected = Program
            .parseUplc("""
              (program 1.1.0
                (lam a (lam b
                  (case
                    (case [(builtin lessThanInteger) a b]
                      (case [(builtin lessThanInteger) b a] (constr 1) (constr 2))
                      (constr 0))
                    (con integer 1) (con integer 2) (con integer 3)))))
            """)
            .getOrElse(fail("expected UPLC does not parse"))
        assert(
          actual.deBruijnedProgram.term α_== expected.deBruijnedProgram.term,
          s"\nexpected:\n${expected.show}\nactual:\n${actual.show}"
        )
    }

    test("three-way match evaluates correctly") {
        val t = PlutusV3.compile(OrderReprDefs.threeWay).program.term
        assert(eval(t, i(1), i(2)) == i(1))
        assert(eval(t, i(2), i(2)) == i(2))
        assert(eval(t, i(3), i(2)) == i(3))
    }

    test("boolean projections and ifEqualThen evaluate correctly") {
        val l = PlutusV3.compile(OrderReprDefs.less).program.term
        assert(eval(l, i(1), i(2)) == true.asTerm)
        assert(eval(l, i(2), i(1)) == false.asTerm)

        val c = PlutusV3.compile(OrderReprDefs.chain).program.term
        assert(eval(c, i(1), i(1), i(2), i(3)) == true.asTerm)
        assert(eval(c, i(1), i(1), i(3), i(2)) == false.asTerm)
        assert(eval(c, i(2), i(1), i(2), i(3)) == false.asTerm)
    }

    // Regression: `ProductCaseEmitter.emitConvert` had no `ProdUplcConstr -> SumUplcConstr` arm,
    // so a bare `Order.Less` (typed at the variant) could not meet a value typed at the sum.
    test("comparing an Order against a bare nullary constructor") {
        val t = PlutusV3.compile(OrderReprDefs.eqLiteral).program.term
        assert(eval(t, i(1), i(2)) == true.asTerm)
        assert(eval(t, i(2), i(2)) == false.asTerm)
        assert(eval(t, i(3), i(2)) == false.asTerm)
    }

    test("user Ord mixing delegated branches and bare Order constructors") {
        val t = PlutusV3.compile(OrderReprKindOps.isLess).program.term
        import scalus.uplc.builtin.Data.toData
        def d(k: OrderReprKind) = Term.Const(scalus.uplc.Constant.Data(k.toData))
        val range = OrderReprKind.ClosedRange(100, 200)
        val negInf = OrderReprKind.FromNegInf(200)
        assert(eval(t, d(range), d(negInf)) == true.asTerm)
        assert(eval(t, d(negInf), d(range)) == false.asTerm)
        assert(eval(t, d(OrderReprKind.Always), d(range)) == false.asTerm)
    }

    test("Order values survive a round trip through a container") {
        val t = PlutusV3.compile(OrderReprDefs.viaList).program.term
        assert(eval(t, i(1), i(2), i(2), i(2)) == i(3)) // Less(1) + Equal(2)
        assert(eval(t, i(3), i(2), i(1), i(2)) == i(4)) // Greater(3) + Less(1)
    }

    test("non-V3 backends are unaffected by the annotation") {
        for backend <- Seq(
              TargetLoweringBackend.ScottEncodingLowering,
              TargetLoweringBackend.SumOfProductsLowering
            )
        do
            given Options = Options.release.copy(noWarn = true, targetLoweringBackend = backend)
            val t = PlutusV3.compile(OrderReprDefs.threeWay).program.term
            assert(eval(t, i(1), i(2)) == i(1), s"backend $backend")
            assert(eval(t, i(2), i(2)) == i(2), s"backend $backend")
            assert(eval(t, i(3), i(2)) == i(3), s"backend $backend")
    }
}
