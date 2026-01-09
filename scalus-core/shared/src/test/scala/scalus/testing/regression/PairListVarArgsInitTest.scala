package scalus.testing.regression

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.uplc.eval.*

case class PairListVarArgsInitCaseClass(name: String, pairs: scalus.prelude.List[(BigInt, BigInt)])

class PairListVarArgsInitTest extends AnyFunSuite {

    given scalus.compiler.Options = scalus.compiler.Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("initialization of struct  ehuch have list of pairs as varargs") {
        val sir = compile {
            val expected = PairListVarArgsInitCaseClass("test", scalus.prelude.List((1, 2), (3, 4)))
        }
        // println(sir.pretty.render(100))
        // val lw = sir.toLoweredValue()
        // println(lw.pretty.render(100))
        val uplc = sir.toUplc(generateErrorTraces = true)
        // println(uplc.pretty.render(100))
        val result = uplc.evaluateDebug
        // println(s"result=$result")
        assert(result.isSuccess)
    }

    test("initialization of case class with pairlist filds") {
        val sir = compile {
            val list = scalus.prelude.List((BigInt(1), BigInt(2)), (BigInt(3), BigInt(4)))
            val expected = PairListVarArgsInitCaseClass("test", list)
        }
        val uplc = sir.toUplc(generateErrorTraces = true)
        val result = uplc.evaluateDebug
        // println(s"result=$result")
        assert(result.isSuccess)
    }

}
