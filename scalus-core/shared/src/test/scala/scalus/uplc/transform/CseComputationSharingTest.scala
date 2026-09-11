package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.eval.PlutusVM
import scala.language.implicitConversions

class CseComputationSharingTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("CSE shares opaque calls even when the binding adds a few bits") {
        val term = λ(f => λ(x => Constr(Word64.Zero, List(f $ x, f $ x))))
        val optimized = CommonSubexpressionElimination(term)
        assert(optimized ~!=~ term)
        assert(
          CommonSubexpressionElimination.termBits(optimized) > CommonSubexpressionElimination
              .termBits(term)
        )
        val increment = λ(x => Builtin(DefaultFun.AddInteger) $ x $ 1)
        assert((optimized $ increment $ 7).evaluate α_== (term $ increment $ 7).evaluate)
        assert(CommonSubexpressionElimination(optimized) ~=~ optimized)
    }

    test("CSE shares repeated forcing of an unknown delayed computation") {
        val term = λ(d => Constr(Word64.Zero, List(Force(d), Force(d))))
        val optimized = CommonSubexpressionElimination(term)
        assert(optimized ~!=~ term)
        val delayed = Delay(Builtin(DefaultFun.AddInteger) $ 7 $ 1)
        assert((optimized $ delayed).evaluate α_== (term $ delayed).evaluate)
        assert(CommonSubexpressionElimination(optimized) ~=~ optimized)
    }

    test("CSE does not add bindings around cheap administrative lets") {
        val identityCall = λ(x => x) $ vr"y"
        val term = Constr(Word64.Zero, List(identityCall, identityCall))
        assert(CommonSubexpressionElimination(term) ~=~ term)
    }

    test("CSE still rejects duplicated small values") {
        val term = Constr(Word64.Zero, List(1.asTerm, 1.asTerm))
        assert(CommonSubexpressionElimination(term) ~=~ term)
    }
}
