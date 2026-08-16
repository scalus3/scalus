package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.{Language, MajorProtocolVersion}
import scalus.cardano.onchain.plutus.prelude.{log, require}
import scalus.compiler.sir.SirDSL.{*, given}
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, Binding, SIR, SIRBuiltins, SIRType, TargetLoweringBackend}
import scalus.compiler.{compile, Options}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.transform.V3Optimizer
import scalus.uplc.{Constant, PlutusV3, Term}
import scalus.uplc.builtin.Data

class UplcPipelineTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    private val releaseNoTag = Options(
      generateErrorTraces = false,
      removeTraces = true,
      optimizeUplc = true
    )

    test("CompiledPlutus.program and UplcPipeline.run produce the same term") {
        given Options = releaseNoTag
        val compiled = PlutusV3.compile { (d: Data) =>
            val x = d.to[BigInt]
            require(x > BigInt(0))
        }
        val direct = UplcPipeline.run(
          compiled.sir,
          compiled.options,
          Language.PlutusV3,
          new V3Optimizer(compiled.options.cseIterations, compiled.options.cceEnabled)
        )
        assert(compiled.program.term == direct)
    }

    private def fixtureSir = compile { (x: BigInt) => if x > BigInt(0) then x else -x }

    test("bug 1: the backend parameter switches the backend") {
        val v3 =
            fixtureSir.toUplc(using Options())(backend = TargetLoweringBackend.SirToUplcV3Lowering)
        val scott = fixtureSir.toUplc(using Options())(backend =
            TargetLoweringBackend.ScottEncodingLowering
        )
        assert(v3 != scott, "backend parameter must not be ignored")
    }

    test("bug 2: generateErrorTraces parameter reaches the V3 backend") {
        val sirWithReq = compile { (x: BigInt) => require(x > BigInt(0), "positive") }
        val traced = sirWithReq.toUplc(using Options())(generateErrorTraces = true)
        val untraced = sirWithReq.toUplc(using Options())(generateErrorTraces = false)
        assert(traced != untraced, "generateErrorTraces must not be ignored on V3 backend")
    }

    test("bug 3: V1-targeted optimization uses V1V2Optimizer (no Case/Constr terms)") {
        def containsCaseOrConstr(t: Term): Boolean = t match
            case Term.Case(_, _, _)   => true
            case Term.Constr(_, _, _) => true
            case Term.Apply(f, a, _)  => containsCaseOrConstr(f) || containsCaseOrConstr(a)
            case Term.LamAbs(_, b, _) => containsCaseOrConstr(b)
            case Term.Force(b, _)     => containsCaseOrConstr(b)
            case Term.Delay(b, _)     => containsCaseOrConstr(b)
            case _                    => false
        val opts = Options(
          targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering,
          targetLanguage = Language.PlutusV1,
          targetProtocolVersion = MajorProtocolVersion.plominPV
        )
        val term = fixtureSir.toUplc(using opts)(optimizeUplc = true)
        assert(!containsCaseOrConstr(term), "V1 scripts must not contain Case/Constr")
    }

    test("bug 4: given Options.release strips traces on the toUplc path") {
        val sirWithLog = compile { (x: BigInt) =>
            log("marker-string")
            x
        }
        val plain = sirWithLog.toUplc(using Options())()
        val release = sirWithLog.toUplc(using Options.release)()
        assert(plain.show.contains("marker-string"))
        assert(!release.show.contains("marker-string"), "removeTraces must be honored")
    }

    // --- hand-built even/odd fixture, copied from
    // scalus-core/shared/src/test/scala/scalus/compiler/sir/MutualRecursionEliminationTest.scala
    private val ann = AnnotationsDecl.empty
    private val intTp = SIRType.Integer
    private val boolTp = SIRType.Boolean
    private val intToBool = SIRType.Fun(intTp, boolTp)

    private def nVar = SIR.Var("n", intTp, ann)
    private def mVar = SIR.Var("m", intTp, ann)
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

    /** λn. if n == 0 then base else if n == 1 then cross(n - 1) else self(n - 2).
      *
      * Both self- and cross-recursive, which is what makes the group's `$mutrec` peer itself
      * recursive - and therefore a static-argument candidate (it re-passes the peer parameters
      * unchanged on every self-call).
      */
    private def stepBoolSelfAndCross(self: String, cross: String, base: Boolean): SIR =
        SIR.LamAbs(
          nVar,
          SIR.IfThenElse(
            extractAnnotated(SIRBuiltins.equalsInteger $ nVar $ intConst(0)),
            boolConst(base),
            SIR.IfThenElse(
              extractAnnotated(SIRBuiltins.equalsInteger $ nVar $ intConst(1)),
              extractAnnotated(
                SIR.Var(cross, intToBool, ann) $
                    (SIRBuiltins.subtractInteger $ nVar $ intConst(1))
              ),
              extractAnnotated(
                SIR.Var(self, intToBool, ann) $
                    (SIRBuiltins.subtractInteger $ nVar $ intConst(2))
              ),
              boolTp,
              ann
            ),
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
            Binding("isOdd", intToBool, stepBoolSelfAndCross("isOdd", "isEven", base = false))
          ),
          body,
          SIR.LetFlags.Recursivity,
          ann
        )

    /** λm. let rec isEven, isOdd in isEven(m) - open in `m`, so the optimizer cannot constant-fold
      * the whole recursion away before we can inspect it.
      */
    private def evenOddOpen: SIR =
        SIR.LamAbs(
          mVar,
          evenOddGroup(extractAnnotated(SIR.Var("isEven", intToBool, ann) $ mVar)),
          List.empty,
          ann
        )

    test("optimized mutual recursion gets its peer parameters lifted") {
        val optimized = evenOddOpen.toUplc(using Options())(optimizeUplc = true)
        val plain = evenOddOpen.toUplc(using Options())(optimizeUplc = false)
        // MRE peer binding survives in both; the SAT fixpoint only in the optimized one
        assert(optimized.show.contains("_sat") || optimized.show.contains("$sat"))
        assert(!(plain.show.contains("_sat") || plain.show.contains("$sat")))
        // isEven(4) -> isOdd(3) -> isOdd(1) -> isEven(0) -> true
        Term.Apply(optimized, Term.Const(Constant.Integer(4))).evaluateDebug match
            case s: Result.Success => assert(s.term == Term.Const(Constant.Bool(true)))
            case f                 => fail(s"optimized mutual recursion failed: $f")
    }
}
