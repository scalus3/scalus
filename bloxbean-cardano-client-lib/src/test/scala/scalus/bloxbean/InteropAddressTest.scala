package scalus.bloxbean

import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Pointer, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.{Bech32, Slot}
import scalus.uplc.builtin.ByteString

import java.math.BigInteger

class InteropAddressTest extends AnyFunSuite {

    test("toTransactionOutput reads a UTxO address as a node holds it") {
        // A pointer address with the slot 2^32, which an output written before version 9 can have
        val bytes = ByteString.fromHex("41" + "00" * 28 + "9080808000" + "0102").bytes
        val utxo = new Utxo()
        utxo.setTxHash("a" * 64)
        utxo.setOutputIndex(0)
        utxo.setAddress(Bech32.encode("addr", bytes))
        utxo.setAmount(java.util.List.of(Amount.lovelace(BigInteger.valueOf(1_000_000))))

        Interop.toTransactionOutput(utxo).address match
            case ShelleyAddress(_, _, ShelleyDelegationPart.Pointer(pointer)) =>
                assert(pointer == Pointer(Slot(0), 0, 0))
            case other => fail(s"expected a pointer address, got $other")
    }
}
