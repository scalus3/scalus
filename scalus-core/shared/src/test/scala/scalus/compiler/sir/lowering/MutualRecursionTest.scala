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

/** Regression defs for the linker's slot-placement algorithm (topological sort of the SCC
  * condensation, not "merge a cyclic group at its earliest- or latest-completing member's slot" -
  * both of those are unsound; see SIRLinker.link).
  */
@Compile
object MutualRecursionOrderingTestDefs {
    // Bug repro: `isEven2`'s body references group member `isOdd2` *and then*, in the same
    // branch, plain helper `positive`. Linking completion order interleaves: isOdd2 completes
    // first (nested inside isEven2's own traversal), then positive completes (a second,
    // independent reference reached from isEven2's body after the isOdd2 call), then isEven2
    // itself completes last. A "merge the cyclic group at its earliest member's slot" scheme
    // places positive's let *inside* the group's body - out of lexical scope for isEven2's own
    // reference to it.
    def isEven2(n: BigInt): Boolean =
        if n == BigInt(0) then true else isOdd2(n - 1) && positive(n)
    def isOdd2(n: BigInt): Boolean =
        if n == BigInt(0) then false else isEven2(n - 1)
    def positive(n: BigInt): Boolean = n >= BigInt(0)

    // Sandwich shape: a plain def outside the cycle (viaOddHelper) depends on a group member
    // (oddS), but is reached only from an external caller (useBoth), never from within the
    // group's own bodies. It must still nest *inside* the group's Let so it can see oddS.
    def evenS(n: BigInt): Boolean = if n == BigInt(0) then true else oddS(n - 1)
    def oddS(n: BigInt): Boolean = if n == BigInt(0) then false else evenS(n - 1)
    def viaOddHelper(n: BigInt): Boolean = oddS(n)
    def useBoth(n: BigInt): BigInt =
        (if evenS(n) then BigInt(1) else BigInt(0)) + (if viaOddHelper(n) then BigInt(10)
                                                       else BigInt(0))

    // Two disjoint mutually recursive groups: {notParityA, parityA} and {oddB, evenB}. evenB
    // references parityA alongside its own cross-recursive call to oddB, so - exactly like the
    // isEven2 case above - a whole other SCC (parityA/notParityA) ends up sandwiched between
    // group B's two members in completion order instead of a single plain def.
    def parityA(n: BigInt): Boolean = if n == BigInt(0) then true else notParityA(n - 1)
    def notParityA(n: BigInt): Boolean = if n == BigInt(0) then false else parityA(n - 1)
    def oddB(n: BigInt): Boolean = if n == BigInt(0) then false else evenB(n - 1)
    def evenB(n: BigInt): Boolean =
        if n == BigInt(0) then true else oddB(n - 1) && parityA(n)
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

    test("group member calling a plain helper after the cross-recursive call stays in scope") {
        // isEven2(4): isOdd2(3) -> isEven2(2) -> isOdd2(1) -> isEven2(0) = true; every
        // positive() guard along the way is true, so this mirrors plain parity.
        val sirTrue = compile { MutualRecursionOrderingTestDefs.isEven2(BigInt(4)) }
        val sirFalse = compile { MutualRecursionOrderingTestDefs.isEven2(BigInt(3)) }
        for backend <- backends do {
            assert(evalBool(sirTrue, backend), s"backend $backend")
            assert(!evalBool(sirFalse, backend), s"backend $backend")
        }
    }

    test("non-group def depending on a group member, reached only from outside, works") {
        // useBoth(4) = (evenS(4)=true -> 1) + (viaOddHelper(4)=oddS(4)=false -> 0) = 1
        // useBoth(5) = (evenS(5)=false -> 0) + (viaOddHelper(5)=oddS(5)=true -> 10) = 10
        val sir4 = compile { MutualRecursionOrderingTestDefs.useBoth(BigInt(4)) }
        val sir5 = compile { MutualRecursionOrderingTestDefs.useBoth(BigInt(5)) }
        for backend <- backends do {
            assert(evalInt(sir4, backend) == BigInt(1), s"backend $backend")
            assert(evalInt(sir5, backend) == BigInt(10), s"backend $backend")
        }
    }

    test("second disjoint group's member referencing the first group's member works") {
        // evenB(4) = oddB(3) && parityA(4) = (evenB(2)) && true = ... = true (4 is even)
        // evenB(3) = oddB(2) && parityA(3) = (evenB(1)) && false = false (3 is odd)
        val sirTrue = compile { MutualRecursionOrderingTestDefs.evenB(BigInt(4)) }
        val sirFalse = compile { MutualRecursionOrderingTestDefs.evenB(BigInt(3)) }
        for backend <- backends do {
            assert(evalBool(sirTrue, backend), s"backend $backend")
            assert(!evalBool(sirFalse, backend), s"backend $backend")
        }
    }
}
