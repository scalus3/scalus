package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.utils.Hex

class LedgerStateTest extends AnyFunSuite with Matchers {

    test("LedgerState") {
        val cbor =
            "828383a0a00084a0a0a0a08482a0a0a0a084a0a0000086a1582203170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c11131400005828001d6088028438394946279f9ed8d66d718679b82f26b75391cb6df8107c8f00cff7e8afb7928000000087828480808080808182a28200581c204c5f1bafe8ee289c881cb48d5f1b2abf97e8720d18526561d93be71903938200581c65d36c561e076d2a6fe08172619d48bdcfd9056e4ee79cbf2cfcbccb190393d81e8201018282782368747470733a2f2f63617264616e6f2d636f6e737469747574696f6e2e63727970746f5820e5f5f212e67762d5fc4727a8bf9cdfaaf9dc0ca8e0f5cf51f32a214a5242cd44581cfa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a645820fbdc3a32a537a9cdd07804bb4a903ecba465325a22bbb97e49527e4e2abc8f8c5820c4359a5ed626b4b0693cad1d3330c3b5f55a55c0199d725519f3d051ce2827998100828480a0a0a084878182a28200581c204c5f1bafe8ee289c881cb48d5f1b2abf97e8720d18526561d93be71903938200581c65d36c561e076d2a6fe08172619d48bdcfd9056e4ee79cbf2cfcbccb190393d81e8201018282782368747470733a2f2f63617264616e6f2d636f6e737469747574696f6e2e63727970746f5820e5f5f212e67762d5fc4727a8bf9cdfaaf9dc0ca8e0f5cf51f32a214a5242cd44581cfa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a645820fbdc3a32a537a9cdd07804bb4a903ecba465325a22bbb97e49527e4e2abc8f8c5820c4359a5ed626b4b0693cad1d3330c3b5f55a55c0199d725519f3d051ce28279900a0848080808080d9010280f4a000"
        val ledger = LedgerState.fromCbor(Hex.hexToBytes(cbor))
        println(pprint(ledger))
        assert(ledger.utxos.utxo.nonEmpty)
    }

    test("TransactionOutput - mempack parsing") {
        // This is a mempack-encoded Alonzo TxOut_AddrHash28_AdaOnly (optimized format)
        val hex =
            "02014046a2bdbdeaba0ee462b86223d87d74d9d0a361872572e515a8fb2579228b6e244a21a68d1b5067a43b0f63e67cea31d16844240100000019d5d0a400bd8440"
        val txOut = MempackParser.parseTransactionOutput(Hex.hexToBytes(hex))

        println(pprint(txOut))
        val addressStr = txOut.address.encode.getOrElse(txOut.address.toHex)
        addressStr should startWith("addr_test1") // Testnet address
        txOut.value.assets.isEmpty shouldBe true // ADA-only format
        txOut.value.coin.value shouldBe 1000000L // 1.0 ADA
    }

    test("TransactionOutput - mempack parsing - Shelley format") {
        // This is a mempack-encoded Shelley TxOut (tag 0)
        val hex = "001d608aca365a824153a032787d0f48e1026d903ddc963dfe696b8bec16f800cff7e8a9e5ead973"
        val txOut = MempackParser.parseTransactionOutput(Hex.hexToBytes(hex))

        println(pprint(txOut))
        val addressStr = txOut.address.encode.getOrElse(txOut.address.toHex)
        addressStr should startWith("addr_test1") // Testnet address
        txOut.value.assets.isEmpty shouldBe true // ADA-only format
    }

}
