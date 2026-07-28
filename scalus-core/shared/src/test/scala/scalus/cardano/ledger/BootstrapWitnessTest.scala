package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class BootstrapWitnessTest extends AnyFunSuite {

    test("addrKeyHash matches cardano-ledger bootstrapWitKeyHash") {
        // Golden vector computed independently from the cardano-ledger spec
        // (Cardano/Ledger/Keys/Bootstrap.hs): blake2b_224(sha3_256(
        //   0x830082005840 ++ publicKey ++ chainCode ++ attributes))
        val publicKey =
            ByteString.fromHex("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
        val chainCode =
            ByteString.fromHex("202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f")
        val witness = BootstrapWitness(
          publicKey = publicKey,
          signature = ByteString.unsafeFromArray(new Array[Byte](64)),
          chainCode = chainCode,
          attributes = ByteString.fromHex("a0") // empty CBOR attribute map
        )
        assert(
          witness.addrKeyHash.toHex == "7b6f5f9cdca840850f0b23ea56010941f2553d358318aa4e5fdea218"
        )
    }
}
