package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString, JVMPlatformSpecific}

class VerifiedSignaturesInWitnessesPlatformTest extends AnyFunSuite {
    private val txId = TransactionHash.fromByteString(ByteString.fromHex("00" * 32))

    test("a missing native library is an error, not an invalid signature") {
        val noNativeLibrary = new JVMPlatformSpecific {
            override def verifyEd25519Signature(
                pk: ByteString,
                msg: ByteString,
                sig: ByteString
            ): Boolean = throw new IllegalStateException("native library not available")
        }
        assertThrows[IllegalStateException] {
            VerifiedSignaturesInWitnessesValidator.verifyWitnessSignature(
              noNativeLibrary,
              txId,
              ByteString.fromHex("00" * 32),
              ByteString.fromHex("00" * 64)
            )
        }
    }

    test("a key or signature of the wrong length is an invalid signature") {
        val validator = VerifiedSignaturesInWitnessesValidator
        val key = ByteString.fromHex("00" * 32)
        val sig = ByteString.fromHex("00" * 64)
        val shortKey = ByteString.fromHex("00" * 31)
        val shortSig = ByteString.fromHex("00" * 63)
        assert(!validator.verifyWitnessSignature(platform, txId, shortKey, sig))
        assert(!validator.verifyWitnessSignature(platform, txId, key, shortSig))
    }
}
