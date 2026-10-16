package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Pointer, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

/** Blockfrost UTxO addresses read as a node holds them. */
class BlockfrostAddressTest extends AnyFunSuite {

    test("a pointer that does not fit the ledger's widths reads as the zero pointer") {
        // A pointer address with the slot 2^32, which an output written before version 9 can have
        val bytes = ByteString.fromHex("41" + "00" * 28 + "9080808000" + "0102").bytes
        val utxo = ujson.Obj(
          "tx_hash" -> "a" * 64,
          "output_index" -> 0,
          "address" -> Bech32.encode("addr", bytes),
          "amount" -> ujson.Arr(ujson.Obj("unit" -> "lovelace", "quantity" -> "1000000"))
        )
        val utxos = BlockfrostProvider.parseUtxos(ujson.write(ujson.Arr(utxo)))
        utxos.values.head.address match
            case ShelleyAddress(_, _, ShelleyDelegationPart.Pointer(pointer)) =>
                assert(pointer == Pointer(Slot(0), 0, 0))
            case other => fail(s"expected a pointer address, got $other")
        assertThrows[IllegalArgumentException](Address.fromBytes(bytes))
    }
}
