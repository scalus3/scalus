package scalus.testing.regression

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Data.toData
import scalus.compiler.{compile, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.cardano.onchain.plutus.v2.OutputDatum

class TypeVarMatchingTest extends AnyFunSuite:

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("DatumOption serialization") {
        val sir = compile {
            val d = OutputDatum.NoOutputDatum
            d.toData
        }
        /*
        This test failed when the `OutputDatum` was used as a type variable in the `ToData` instance.
            given [T <: scalus.cardano.onchain.plutus.v2.OutputDatum]: ToData[T] = (d: T) =>
                d match
                    case _ => ...
         */
        sir.toUplc() // should not fail
    }
