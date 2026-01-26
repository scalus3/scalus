package scalus.prelude

import scalus.cardano.onchain.plutus.prelude.List

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.eval.PlutusVM
import scalus.uplc.*
import scalus.uplc.Constant.Integer

@scalus.Compile
object VarargsTestObj {

    def pos(args: BigInt*): List[BigInt] = {
        args.list.filter(_ > 0)
    }

    def apply[T](args: T*): List[T] = {
        args.list
    }

}

@scalus.Compile
object VarargsTestObj2 {

    def apply(args: BigInt*): BigInt = {
        args.list.indexOf(BigInt(3))
    }

}

class VarargsTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    given scalus.compiler.Options = scalus.compiler.Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("use varasg in non-apply") {

        val sir = scalus.compiler.compile {
            val p = VarargsTestObj.pos(BigInt(-1), BigInt(2), BigInt(-3), BigInt(4))
            p.head
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(term, budget, costs, logs) =>
                assert(term == Term.Const(Integer(2)))
            case _ =>
                fail(s"Expected success, but got: $result")

    }

    test("use varasg in apply") {

        val sir = scalus.compiler.compile {
            val p = VarargsTestObj(BigInt(-1), BigInt(2), BigInt(-3), BigInt(4))
            p.head
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(term, budget, costs, logs) =>
                assert(term == Term.Const(Integer(-1)))
            case _ =>
                fail(s"Expected success, but got: $result")

    }

    test("use varargs in apply without type parameters") {
        val sir = scalus.compiler.compile {
            VarargsTestObj2(BigInt(1), BigInt(2), BigInt(3), BigInt(4))
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(term, budget, costs, logs) =>
                assert(term == Term.Const(Integer(2)))
            case _ =>
                fail(s"Expected success, but got: $result")
    }

}
