package scalus.testing.regression

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Data
import scalus.compiler.compile
import scalus.cardano.onchain.plutus.prelude.*

@Compile
object Min20250702 {

    def validate(param: Data): Unit = {
        val (q1, q2) = List
            .empty[BigInt]
            .foldLeft(BigInt(0), BigInt(0)) { case (a, value) =>
                (BigInt(0), value)
            }
    }

}

class GeneratingUnscopedVarsTest extends AnyFunSuite {

    given scalus.compiler.Options = scalus.compiler.Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("Min20250702 should compile and transform to UPLC") {
        // pending
        val sir = compile {
            Min20250702.validate
        }
        // println(sir.pretty.render(100))
        val uplc = sir.toUplc(generateErrorTraces = true)
        // println(uplc.pretty.render(100))
        // val result = script.evaluateDebug
        // assert(result.isSuccess)
    }

}
