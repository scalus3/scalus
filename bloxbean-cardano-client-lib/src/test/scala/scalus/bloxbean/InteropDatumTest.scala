package scalus.bloxbean

import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.DataHash
import scalus.uplc.builtin.{Builtins, ByteString}

import java.math.BigInteger

class InteropDatumTest extends AnyFunSuite {

    test("toTransactionOutput keeps the original bytes of an inline datum") {
        // A non-minimal encoding of Constr 0 [10]: re-encoding it gives d8799f0aff
        val probeHex = "d879811a0000000a"
        val utxo = new Utxo()
        utxo.setTxHash("a" * 64)
        utxo.setOutputIndex(0)
        utxo.setAddress(
          "addr1q9d34spgg2kdy47n82e7x9pdd6vql6d2engxmpj20jmhuc2047yqd4xnh7u6u5jp4t0q3fkxzckph4tgnzvamlu7k5psuahzcp"
        )
        utxo.setAmount(java.util.List.of(Amount.lovelace(BigInteger.valueOf(1_000_000))))
        utxo.setInlineDatum(probeHex)

        val output = Interop.toTransactionOutput(utxo)

        val expected = DataHash.fromByteString(Builtins.blake2b_256(ByteString.fromHex(probeHex)))
        assert(output.datumOption.map(_.dataHash) == Some(expected))
    }
}
