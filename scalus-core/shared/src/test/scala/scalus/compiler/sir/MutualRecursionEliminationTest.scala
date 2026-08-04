package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.Options
import scalus.compiler.sir.SirDSL.{*, given}
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}

class MutualRecursionEliminationTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    private val ann = AnnotationsDecl.empty
    private val intTp = SIRType.Integer
    private val boolTp = SIRType.Boolean
    private val intToBool = SIRType.Fun(intTp, boolTp)
    private val intToInt = SIRType.Fun(intTp, intTp)

    private def nVar = SIR.Var("n", intTp, ann)
    private def intConst(v: Int) = SIR.Const(Constant.Integer(v), intTp, ann)
    private def boolConst(v: Boolean) = SIR.Const(Constant.Bool(v), boolTp, ann)

    /** λn. if n == 0 then base else callee(n - 1) */
    private def stepBool(callee: String, base: Boolean): SIR =
        SIR.LamAbs(
          nVar,
          SIR.IfThenElse(
            extractAnnotated(SIRBuiltins.equalsInteger $ nVar $ intConst(0)),
            boolConst(base),
            SIR.Var(callee, intToBool, ann) $
                (SIRBuiltins.subtractInteger $ nVar $ intConst(1)),
            boolTp,
            ann
          ),
          List.empty,
          ann
        )

    private def evenOddGroup(body: AnnotatedSIR): AnnotatedSIR =
        SIR.Let(
          List(
            Binding("isEven", intToBool, stepBool("isOdd", base = true)),
            Binding("isOdd", intToBool, stepBool("isEven", base = false))
          ),
          body,
          SIR.LetFlags.Recursivity,
          ann
        )

    private def callIsEven(n: Int): AnnotatedSIR =
        extractAnnotated(SIR.Var("isEven", intToBool, ann) $ intConst(n))

    test("2-group: rewritten to nested single lets with $mutrec peer") {
        val out = MutualRecursionElimination(evenOddGroup(callIsEven(4)))
        out match
            case SIR.Let(List(Binding(oddP, oddTp, oddRhs)), inner, oddFlags, _) =>
                assert(oddP == "isOdd$mutrec")
                assert(!oddFlags.isRec)
                assert(oddTp == SIRType.Fun(intToBool, intToBool))
                oddRhs match
                    case SIR.LamAbs(param, _, _, _) => assert(param.name == "isEven")
                    case other                      => fail(s"expected LamAbs, got $other")
                inner match
                    case SIR.Let(List(Binding("isEven", _, _)), _, evenFlags, _) =>
                        assert(evenFlags.isRec)
                    case other => fail(s"expected isEven let, got $other")
            case other => fail(s"expected outer isOdd-mutrec let, got $other")
    }

    test("2-group: evaluates correctly on all three backends") {
        for backend <- TargetLoweringBackend.values do {
            val opts = Options(
              targetLoweringBackend = backend,
              targetProtocolVersion = MajorProtocolVersion.vanRossemPV
            )
            val sir = evenOddGroup(callIsEven(4))
            val uplc = sir.toUplc(using opts)()
            uplc.evaluateDebug match
                case s: Result.Success =>
                    assert(s.term == Term.Const(Constant.Bool(true)), s"backend $backend")
                case f => fail(s"backend $backend failed: $f")
        }
    }

    /** λn. if n == 0 then base else callee(n - 1), Int result */
    private def stepInt(callee: String, base: Int): SIR =
        SIR.LamAbs(
          nVar,
          SIR.IfThenElse(
            extractAnnotated(SIRBuiltins.equalsInteger $ nVar $ intConst(0)),
            intConst(base),
            SIR.Var(callee, intToInt, ann) $
                (SIRBuiltins.subtractInteger $ nVar $ intConst(1)),
            intTp,
            ann
          ),
          List.empty,
          ann
        )

    test("3-group: a -> b -> c -> a evaluates correctly on all backends") {
        val group = SIR.Let(
          List(
            Binding("rotA", intToInt, stepInt("rotB", base = 0)),
            Binding("rotB", intToInt, stepInt("rotC", base = 1)),
            Binding("rotC", intToInt, stepInt("rotA", base = 2))
          ),
          extractAnnotated(SIR.Var("rotA", intToInt, ann) $ intConst(7)),
          SIR.LetFlags.Recursivity,
          ann
        )
        // rotA(7)->rotB(6)->rotC(5)->rotA(4)->rotB(3)->rotC(2)->rotA(1)->rotB(0) = 1
        for backend <- TargetLoweringBackend.values do {
            val opts = Options(
              targetLoweringBackend = backend,
              targetProtocolVersion = MajorProtocolVersion.vanRossemPV
            )
            group.toUplc(using opts)().evaluateDebug match
                case s: Result.Success =>
                    assert(s.term == Term.Const(Constant.Integer(1)), s"backend $backend")
                case f => fail(s"backend $backend failed: $f")
        }
    }

    private def fourGroupCalling(entry: String, arg: Int): AnnotatedSIR =
        SIR.Let(
          List(
            Binding("d1", intToInt, stepInt("d4", base = 1)),
            Binding("d2", intToInt, stepInt("d3", base = 2)),
            Binding("d3", intToInt, stepInt("d4", base = 3)),
            Binding("d4", intToInt, SIR.LamAbs(nVar, intConst(999), List.empty, ann))
          ),
          extractAnnotated(SIR.Var(entry, intToInt, ann) $ intConst(arg)),
          SIR.LetFlags.Recursivity,
          ann
        )

    test(
      "4-group: distance-3 forward call (member 1 -> member 4) evaluates correctly on all backends"
    ) {
        for backend <- TargetLoweringBackend.values do {
            val opts = Options(
              targetLoweringBackend = backend,
              targetProtocolVersion = MajorProtocolVersion.vanRossemPV
            )
            // d1(3) -> d4(2) = 999: the distance-3 forward call (member 1 references
            // member 4 directly, skipping members 2/3).
            fourGroupCalling("d1", 3).toUplc(using opts)().evaluateDebug match
                case s: Result.Success =>
                    assert(s.term == Term.Const(Constant.Integer(999)), s"backend $backend, d1(3)")
                case f => fail(s"backend $backend, d1(3) failed: $f")
            // d2(1) -> d3(0) = 3: the adjacent chain members 2/3 use to terminate.
            fourGroupCalling("d2", 1).toUplc(using opts)().evaluateDebug match
                case s: Result.Success =>
                    assert(s.term == Term.Const(Constant.Integer(3)), s"backend $backend, d2(1)")
                case f => fail(s"backend $backend, d2(1) failed: $f")
        }
    }

    /** 10-member group: member 1 calls member 10 directly (distance 9), members 2..9 chain
      * adjacently to the next member, member 10 is a terminal constant.
      */
    private def tenGroup(arg: Int): AnnotatedSIR =
        SIR.Let(
          (1 to 10).map { k =>
              val rhs =
                  if k == 1 then stepInt("f10", base = 1)
                  else if k == 10 then SIR.LamAbs(nVar, intConst(10), List.empty, ann)
                  else stepInt(s"f${k + 1}", base = k)
              Binding(s"f$k", intToInt, rhs)
          }.toList,
          extractAnnotated(SIR.Var("f1", intToInt, ann) $ intConst(arg)),
          SIR.LetFlags.Recursivity,
          ann
        )

    test(
      "10-group: distance-9 forward call keeps eliminated SIR size polynomial, not exponential"
    ) {
        val eliminated = MutualRecursionElimination(tenGroup(0))
        val nodeCount = SIR.size(eliminated)
        // Pre-fix, a far reference recursively re-expanded every E(k) for k in 1..j-1,
        // including the far ones among them - exponential in the reference distance j - i.
        // Member 1's distance-9 call to member 10 alone would already cost on the order of
        // 2^8 (~256) applyChain/Apply nodes just for that one call site's argument-chain
        // re-expansion, on top of the rest of the group and the adjacent 2..9 chain (i.e.
        // comfortably over 2000 nodes total for the old encoding). With the eta-let fix,
        // each context's far-reference work is O(distance) and the whole group is O(N^2)
        // (N = 10 here); the measured count is 499 nodes. 2000 leaves ample slack above
        // that polynomial bound while staying far below the old exponential one.
        assert(nodeCount < 2000, s"eliminated SIR has $nodeCount nodes, expected < 2000")
    }

    test("10-group: distance-9 forward call evaluates correctly on all backends") {
        for backend <- TargetLoweringBackend.values do {
            val opts = Options(
              targetLoweringBackend = backend,
              targetProtocolVersion = MajorProtocolVersion.vanRossemPV
            )
            // f1(1) -> f10(0) = 10 (f10 ignores n and returns the constant 10).
            tenGroup(1).toUplc(using opts)().evaluateDebug match
                case s: Result.Success =>
                    assert(s.term == Term.Const(Constant.Integer(10)), s"backend $backend")
                case f => fail(s"backend $backend failed: $f")
        }
    }

    test("group of non-lambda values is rejected") {
        val group = SIR.Let(
          List(Binding("a", intTp, intConst(1)), Binding("b", intTp, intConst(2))),
          SIR.Var("a", intTp, ann),
          SIR.LetFlags.Recursivity,
          ann
        )
        val e = intercept[IllegalArgumentException] { MutualRecursionElimination(group) }
        assert(e.getMessage.contains("mutually recursive values"))
    }

    test("single-binding and non-rec lets are unchanged") {
        val singleRec = SIR.Let(
          List(Binding("f", intToInt, stepInt("f", base = 0))),
          extractAnnotated(SIR.Var("f", intToInt, ann) $ intConst(3)),
          SIR.LetFlags.Recursivity,
          ann
        )
        assert(MutualRecursionElimination(singleRec) == singleRec)
        val nonRec = SIR.Let(
          List(Binding("a", intTp, intConst(1)), Binding("b", intTp, intConst(2))),
          SIR.Var("a", intTp, ann),
          SIR.LetFlags.None,
          ann
        )
        assert(MutualRecursionElimination(nonRec) == nonRec)
    }
}
