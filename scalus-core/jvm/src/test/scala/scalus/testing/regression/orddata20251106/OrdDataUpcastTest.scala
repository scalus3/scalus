package scalus.testing.regression.orddata20251106

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Data
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.prelude.<=>
import scalus.prelude.Ord.{*, given}

/** Regression test for Data Ord upcasting issue found in cosmex contract.
  *
  * Error: Cannot upcast Unit -> scalus.prelude.Order$.Less to Unit -> scalus.prelude.Order
  *
  * The issue occurs in Ord.scala lines 64-66 where lambda return types are inferred as specific
  * Order enum cases (Order.Less, Order.Greater) instead of the general Order type.
  */
class OrdDataUpcastTest extends AnyFunSuite {

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false
    )

    test("Data comparison should compile without upcasting error") {
        // This should compile and lower to UPLC without errors
        val sir = compile { (d1: Data, d2: Data) =>
            d1 equiv d2
        }

        val uplc = sir.toUplc(generateErrorTraces = true)
        assert(uplc != null)
    }

    test("Data less-than comparison should compile") {
        val sir = compile { (d1: Data, d2: Data) =>
            d1 < d2
        }

        val uplc = sir.toUplc(generateErrorTraces = true)
        assert(uplc != null)
    }

    test("Data <=> operator should compile") {
        val sir = compile { (d1: Data, d2: Data) =>
            d1 <=> d2
        }

        val uplc = sir.toUplc(generateErrorTraces = true)
        assert(uplc != null)
    }
}
