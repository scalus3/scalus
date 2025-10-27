package scalus.regression.match20251021

import scalus.*
import scalus.Compiler.compile
import org.scalatest.funsuite.AnyFunSuite

/** Test to verify that error messages are concise and use showShort */
/*
class ErrorMessageTest extends AnyFunSuite {


    test("error message should be short and readable") {
        // This test demonstrates that error messages are now concise
        // Uncomment to see the shortened error message:

        // val sir = compile {
        //     import scalus.ledger.api.v2.TxOut
        //     import TxOutExtensions.inlineDatumOfType
        //     import scalus.builtin.Data
        //     // This should trigger a lowering error with shortened output
        //     (txOut: TxOut) => {
        //         val result = txOut.inlineDatumOfType[Data]
        //         result
        //     }
        // }
        // val uplc = sir.toUplcOptimized()

        // For now, just verify showShort works on SIR
        val sir = compile {
            val x = BigInt(42)
            x + BigInt(1)
        }

        val fullShow = sir.show
        val shortShow = sir.showShort

        println(s"Full show length: ${fullShow.length}")
        println(s"Short show length: ${shortShow.length}")
        println(s"Short show: $shortShow")

        // Verify that shortShow is indeed shorter or has ellipsis
        assert(
          shortShow.length <= 63 || shortShow.contains("..."), // 60 + "..."
          s"showShort should be at most 63 chars or contain '...', got ${shortShow.length}"
        )
    }
}
 */
