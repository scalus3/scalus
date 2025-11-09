package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.ByteString
import scalus.cardano.ledger.Word64
import scalus.uplc.{Constant, DeBruijn}
import scalus.uplc.Term.*
import scalus.uplc.eval.ExBudget.given
import scalus.uplc.eval.Result.Success
import scalus.uplc.eval.{ExBudget, PlutusVM}

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.*

class CaseConstrApplyTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.ScottEncodingLowering
    )

    test("replace (apply (apply (apply f a) b) c) with (case (constr 0 [a, b, c]) f)") {
        val sir = compile(((a: BigInt) => (b: BigInt) => (c: ByteString) => c)(0)(1)(hex"1012"))
        val uplc = sir.toUplc()
        val (optimized, logs) = CaseConstrApply.extractPass(uplc)
        val expected = Case(
          Constr(
            Word64.Zero,
            List(
              Const(Constant.Integer(0)),
              Const(Constant.Integer(1)),
              Const(Constant.ByteString(hex"1012"))
            )
          ),
          λ("a", "b", "c")(vr"c") :: Nil
        )
        // println(s"optimized UPLC: ${optimized.pretty.render(100)}")
        // println(s"expected UPLC:  ${expected.pretty.render(100)}")
        val djOptimized = DeBruijn.deBruijnTerm(optimized)
        val djExpected = DeBruijn.deBruijnTerm(expected)
        assert(
          scalus.uplc.Term.alphaEq(djOptimized, djExpected),
        )
        assert(logs == Seq("Replacing 3 Apply with Case/Constr"))
        (uplc.evaluateDebug, optimized.evaluateDebug) match
            case (orig: Success, opt: Success) =>
                assert(orig.term == Const(Constant.ByteString(hex"1012")))
                assert(opt.term == Const(Constant.ByteString(hex"1012")))
                assert(orig.budget == ExBudget.fromCpuAndMemory(160100, 1100))
                assert(opt.budget == ExBudget.fromCpuAndMemory(144100, 1000))
                assert(opt.budget < orig.budget)
            case _ => fail("Evaluation failed")
    }
}
