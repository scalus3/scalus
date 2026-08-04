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
