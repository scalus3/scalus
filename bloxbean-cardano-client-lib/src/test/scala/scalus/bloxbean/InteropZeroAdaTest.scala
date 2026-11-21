package scalus.bloxbean

import com.bloxbean.cardano.client.transaction.spec.{Asset, MultiAsset, Transaction, TransactionBody, Value}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.SlotConfig
import scalus.uplc.builtin.Data.toData
import scalus.utils.Hex

import java.math.BigInteger
import java.util

/** The ledger keeps the ADA entry of an output value and of a V1/V2 fee when it holds zero lovelace
  * (`transCoinToValue` is a `singleton`, which keeps a zero amount). The Bloxbean translator must
  * give the same Data as `LedgerToPlutusTranslation`.
  */
class InteropZeroAdaTest extends AnyFunSuite {
    private val policy = "33" * 28

    private def cbor(value: scalus.cardano.onchain.plutus.v1.Value): String =
        Hex.bytesToHex(value.toData.toCbor)

    test("zero ADA output value keeps the ADA entry") {
        val value = Value.builder().coin(BigInteger.ZERO).build()
        assert(cbor(Interop.getValue(value)) == "a140a14000")
    }

    test("zero ADA precedes native assets in output value") {
        val asset = Asset.builder().name("0x44").value(BigInteger.valueOf(7)).build()
        val value = Value
            .builder()
            .coin(BigInteger.ZERO)
            .multiAssets(util.List.of(new MultiAsset(policy, util.List.of(asset))))
            .build()
        assert(cbor(Interop.getValue(value)) == "a240a14000581c" + policy + "a1414407")
    }

    test("positive ADA output value is unchanged") {
        val value = Value.builder().coin(BigInteger.ONE).build()
        assert(cbor(Interop.getValue(value)) == "a140a14001")
    }

    for (fee, expected) <- Seq(0L -> "a140a14000", 1L -> "a140a14001") do
        test(s"V1 and V2 fee Data keeps $fee lovelace") {
            val body = TransactionBody.builder().fee(BigInteger.valueOf(fee)).build()
            val tx = Transaction.builder().body(body).build()
            val v1 = Interop.getTxInfoV1(tx, "00" * 32, Seq.empty, Map.empty, SlotConfig.mainnet, 9)
            val v2 = Interop.getTxInfoV2(tx, "00" * 32, Seq.empty, Map.empty, SlotConfig.mainnet, 9)
            assert(cbor(v1.fee) == expected)
            assert(cbor(v2.fee) == expected)
        }
}
