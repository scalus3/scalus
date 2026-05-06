package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.prelude.*
import scalus.compiler.{compile, Compile, Options, UplcRepr, UplcRepresentation}
import scalus.compiler.UplcRepresentation.TypeVar
import scalus.compiler.UplcRepresentation.TypeVarKind.Transparent
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}

@Compile
object NativeFindModule {

    /** `@UplcRepr(TypeVar(Transparent))` makes `xs` lower with
      * `SumBuiltinList(TypeVarRepresentation(Transparent))` repr (non-PackedData), so the inner
      * `xs.find(...)` dispatches through `IntrinsicsNativeList.find`. The predicate doesn't
      * dereference the element so the test is independent of Transparent-A passthrough semantics at
      * the predicate boundary.
      */
    def transparentFindAny[@UplcRepr(TypeVar(Transparent)) A](
        xs: List[A]
    ): Boolean = xs.find(_ => true) match
        case Option.Some(_) => true
        case Option.None    => false
}

/** Exercises `IntrinsicsNativeList.find` dispatch — unreachable from the rest of the suite. */
class NativeListFindTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
    private given Options = Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV
    )

    test("transparentFindAny on non-empty list → true (Some branch)") {
        val sir = compile {
            val xs = List[BigInt](BigInt(10), BigInt(20))
            NativeFindModule.transparentFindAny[BigInt](xs)
        }
        sir.toUplc().evaluateDebug match
            case Result.Success(Term.Const(Constant.Bool(b), _), _, _, _) =>
                assert(b == true, s"Expected true (Some branch), got $b")
            case Result.Success(term, _, _, _) =>
                fail(s"Expected Bool, got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"transparentFindAny non-empty failed: $ex")
    }

    test("transparentFindAny on empty list → false (None branch)") {
        val sir = compile {
            NativeFindModule.transparentFindAny[BigInt](List.empty[BigInt])
        }
        sir.toUplc().evaluateDebug match
            case Result.Success(Term.Const(Constant.Bool(b), _), _, _, _) =>
                assert(b == false, s"Expected false (None branch), got $b")
            case Result.Success(term, _, _, _) =>
                fail(s"Expected Bool, got ${term.show}")
            case Result.Failure(ex, _, _, _) =>
                fail(s"transparentFindAny empty failed: $ex")
    }
}
