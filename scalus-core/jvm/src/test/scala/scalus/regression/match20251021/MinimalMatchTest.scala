package scalus.regression.match20251021

import scalus.*
import scalus.Compiler.compile
import org.scalatest.funsuite.AnyFunSuite

class MinimalMatchTest extends AnyFunSuite {

    test("compile TxOutExtensions with @unchecked pattern") {
        val sir = compile {
            import scalus.builtin.Data
            import scalus.ledger.api.v3.TxOut
            import TxOutExtensions.inlineDatumOfType
            (txOut: TxOut) => txOut.inlineDatumOfType[Data]
        }

        println("=== SIR OUTPUT ===")
        println(sir.pretty.render(120))
        println("==================")

        val uplc = sir.toUplcOptimized()
        val cbor = uplc.plutusV3.cborByteString

        assert(cbor.nonEmpty, "UPLC compilation should succeed")
    }
}
