package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.uplc.builtin.{Builtins, ByteString}

/** Blockfrost and Yaci inline datums keep the bytes the chain hashes. */
class BlockfrostDatumTest extends AnyFunSuite {

    // A non-minimal encoding of Constr 0 [10]: re-encoding it gives d8799f0aff
    private val probeHex = "d879811a0000000a"
    private val probeHash =
        DataHash.fromByteString(Builtins.blake2b_256(ByteString.fromHex(probeHex)))

    private def datumHashOf(datumFields: (String, ujson.Value)*): Option[DataHash] = {
        val utxo = ujson.Obj(
          "tx_hash" -> "a" * 64,
          "output_index" -> 0,
          "address" -> "addr1q9d34spgg2kdy47n82e7x9pdd6vql6d2engxmpj20jmhuc2047yqd4xnh7u6u5jp4t0q3fkxzckph4tgnzvamlu7k5psuahzcp",
          "amount" -> ujson.Arr(ujson.Obj("unit" -> "lovelace", "quantity" -> "1000000"))
        )
        datumFields.foreach((key, value) => utxo(key) = value)
        val utxos = BlockfrostProvider.parseUtxos(ujson.write(ujson.Arr(utxo)))
        utxos.values.head.datumOption.map(_.dataHash)
    }

    test("an inline_datum_cbor keeps its original bytes") {
        assert(datumHashOf("inline_datum_cbor" -> probeHex) == Some(probeHash))
    }

    test("a Yaci inline_datum hex string keeps its original bytes") {
        assert(datumHashOf("inline_datum" -> probeHex) == Some(probeHash))
    }
}
