package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.{compile, Compile, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}

@Compile
object MutualRecursionTestDefs {
    def isEven(n: BigInt): Boolean =
        if n == BigInt(0) then true else isOdd(n - 1)
    def isOdd(n: BigInt): Boolean =
        if n == BigInt(0) then false else isEven(n - 1)

    def rotA(n: BigInt): BigInt = if n == BigInt(0) then BigInt(0) else rotB(n - 1)
    def rotB(n: BigInt): BigInt = if n == BigInt(0) then BigInt(1) else rotC(n - 1)
    def rotC(n: BigInt): BigInt = if n == BigInt(0) then BigInt(2) else rotA(n - 1)

    /** Self-recursive AND cross-recursive member. */
    def evenSum(n: BigInt): BigInt =
        if n == BigInt(0) then BigInt(0)
        else if n == BigInt(1) then oddSkip(n)
        else n + evenSum(n - BigInt(2))
    def oddSkip(n: BigInt): BigInt = evenSum(n - 1)
}

class MutualRecursionTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    private val backends = List(
      TargetLoweringBackend.SirToUplcV3Lowering,
      TargetLoweringBackend.ScottEncodingLowering,
      TargetLoweringBackend.SumOfProductsLowering
    )

    private def opts(backend: TargetLoweringBackend) = Options(
      targetLoweringBackend = backend,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV
    )

    private def evalInt(sir: scalus.compiler.sir.SIR, backend: TargetLoweringBackend): BigInt =
        sir.toUplc(using opts(backend))().evaluateDebug match
            case s: Result.Success =>
                s.term match
                    case Term.Const(Constant.Integer(v), _) => v
                    case other                              => fail(s"not an integer: $other")
            case f => fail(s"backend $backend failed: $f")

    private def evalBool(sir: scalus.compiler.sir.SIR, backend: TargetLoweringBackend): Boolean =
        sir.toUplc(using opts(backend))().evaluateDebug match
            case s: Result.Success =>
                s.term match
                    case Term.Const(Constant.Bool(v), _) => v
                    case other                           => fail(s"not a boolean: $other")
            case f => fail(s"backend $backend failed: $f")

    test("even/odd 2-cycle works on all backends") {
        val sirTrue = compile { MutualRecursionTestDefs.isEven(BigInt(10)) }
        val sirFalse = compile { MutualRecursionTestDefs.isEven(BigInt(9)) }
        for backend <- backends do {
            assert(evalBool(sirTrue, backend), s"backend $backend")
            assert(!evalBool(sirFalse, backend), s"backend $backend")
        }
    }

    test("even/odd group renders via PrettyPrinter") {
        val sir = compile { MutualRecursionTestDefs.isEven(BigInt(2)) }
        val rendered = sir.show
        assert(rendered.contains("and"), rendered)
        assert(rendered.contains("isEven"), rendered)
        assert(rendered.contains("isOdd"), rendered)
    }

    test("3-cycle works on all backends") {
        // rotA(7): 7 steps around the cycle ends in rotB's base = 1
        val sir = compile { MutualRecursionTestDefs.rotA(BigInt(7)) }
        for backend <- backends do assert(evalInt(sir, backend) == BigInt(1), s"backend $backend")
    }

    test("self- and cross-recursive member works on all backends") {
        // evenSum(6) = 6 + 4 + 2 + 0 = 12
        val sir = compile { MutualRecursionTestDefs.evenSum(BigInt(6)) }
        for backend <- backends do assert(evalInt(sir, backend) == BigInt(12), s"backend $backend")
    }

    test("body calling only one member of the group works") {
        val sir = compile { MutualRecursionTestDefs.isOdd(BigInt(3)) }
        for backend <- backends do assert(evalBool(sir, backend), s"backend $backend")
    }

    test("V3 recursion budget stays sane") {
        val sir = compile { MutualRecursionTestDefs.isEven(BigInt(20)) }
        sir.toUplc(using opts(TargetLoweringBackend.SirToUplcV3Lowering))().evaluateDebug match
            case s: Result.Success =>
                // ~21 recursive calls; generous ceiling proves no pathological encoding.
                assert(s.budget.steps < 50_000_000L, s.budget)
            case f => fail(s"failed: $f")
    }
}
