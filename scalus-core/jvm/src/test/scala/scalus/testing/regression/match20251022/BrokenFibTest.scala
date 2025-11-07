package scalus.testing.regression.match20251022

import scalus.*
import org.scalatest.funsuite.AnyFunSuite

@Compile
object BrokenFib {
    import scalus.prelude.*
    import List.*
    def fib(n: BigInt): List[BigInt] = if n > 1 then
        (fib(n - 1): @unchecked) match
            case r @ Cons(a, Cons(b, _)) => Cons(a + b, r)
    else List(1, 0)
}

class BrokenFibTest extends AnyFunSuite {

    import scalus.builtin.Data
    import scalus.uplc.*
    import scalus.uplc.eval.PlutusVM
    private given PlutusVM = PlutusVM.makePlutusV2VM()

    test("Compile list of fib") {
        val sir = scalus.Compiler.compile {
            val result = BrokenFib.fib(10)
            Data.toData(result)
        }
        // println(s"SIR:\n${sir.showHighlighted}")
        val uplc = sir.toUplc()
        // println(s"UPLC:\n${uplc.showHighlighted}")
        val evalResult = uplc.evaluate

        // here we assube that representtion of List[BigInt] in backend is data.
        //   need be chnged if we will change backend representation
        val expectedResult =
            Term.Const(
              Constant.Data(
                Data.toData(
                  scalus.prelude.List(
                    BigInt(55),
                    BigInt(34),
                    BigInt(21),
                    BigInt(13),
                    BigInt(8),
                    BigInt(5),
                    BigInt(3),
                    BigInt(2),
                    BigInt(1),
                    BigInt(1),
                    BigInt(0)
                  )
                )
              )
            )
        assert(evalResult == expectedResult)
    }

}
