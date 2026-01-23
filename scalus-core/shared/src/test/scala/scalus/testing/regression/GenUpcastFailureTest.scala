package scalus.testing.regression

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.cardano.onchain.plutus.v1.Value.getLovelace
import scalus.cardano.onchain.plutus.v3.*

class GenUpcastFailureTest extends AnyFunSuite {

    given scalus.compiler.Options = scalus.compiler.Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("Call of getLovelace should compile and transform to UPLC") {
        // pending
        val sir = compile { (value: Value) =>
            val lv = value.getLovelace
            if lv < 10 then scalus.cardano.onchain.plutus.prelude.fail("lv < 10")
        }
        // println(sir.pretty.render(100))
        val uplc = sir.toUplc(generateErrorTraces = true)
        // println(uplc.pretty.render(100))
        // val result = script.evaluateDebug
        // assert(result.isSuccess)
    }

}
