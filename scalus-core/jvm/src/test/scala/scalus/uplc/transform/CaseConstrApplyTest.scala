package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.{compile, Options, TargetLoweringBackend}
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.cardano.ledger.ExUnits.given
import scalus.cardano.ledger.{ExUnits, Word64}
import scalus.uplc.Term.*
import scalus.uplc.eval.PlutusVM
import scalus.uplc.eval.Result.Success
import scalus.uplc.{Constant, DeBruijn}

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.*

class CaseConstrApplyTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering
    )

    test("replace (apply (apply (apply f a) b) c) with (case (constr 0 [a, b, c]) f)") {
        val sir = compile(((a: BigInt) => (b: BigInt) => (c: ByteString) => c)(0)(1)(hex"1012"))
        val uplc = sir.toUplc()
        val optimizer = new CaseConstrApply()
        val optimized = optimizer.apply(uplc)
        val logs = optimizer.logs
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
        assert(djOptimized α_== djExpected)
        assert(logs == Seq("Replacing 3 Apply with Case/Constr"))
        (uplc.evaluateDebug, optimized.evaluateDebug) match
            case (orig: Success, opt: Success) =>
                assert(orig.term == Const(Constant.ByteString(hex"1012")))
                assert(opt.term == Const(Constant.ByteString(hex"1012")))
                assert(orig.budget == ExUnits(memory = 1100, steps = 160100))
                assert(opt.budget == ExUnits(memory = 1000, steps = 144100))
                assert(opt.budget < orig.budget)
            case _ => fail("Evaluation failed")
    }
}
