package scalus.misc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.uplc.*
import scalus.uplc.eval.PlutusVM

object InlineFib {

    inline def fib(inline n: Int): Int =
        inline if n <= 1 then n
        else fib(n - 1) + fib(n - 2)

}

class InlineFibTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    val compileMe = 1

    test("Compile inline fib") {
        val sir = compile {
            BigInt(InlineFib.fib(10))
        }
        // println(s"SIR:\n${sir.showHighlighted}")
        val expectedFib10 = 55
        val uplc = sir.toUplc()
        // println(s"UPLC:\n${uplc.showHighlighted}")
        val evalResult = uplc.evaluate
        assert(evalResult == Term.Const(Constant.Integer(BigInt(expectedFib10))))
    }

}
