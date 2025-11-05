package scalus.testing.regression

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data.toData
import scalus.ledger.api.v2.OutputDatum

class TypeVarMatchingTest extends AnyFunSuite:

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
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
            given [T <: scalus.ledger.api.v2.OutputDatum]: ToData[T] = (d: T) =>
                d match
                    case _ => ...
         */
        sir.toUplc() // should not fail
    }
