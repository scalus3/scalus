package scalus.crypto.ed25519

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString}
import scalus.utils.Hex

/** Cardano extended keys (BIP32-Ed25519, `kL ‖ kR`) on every platform give the same public key and
  * signature as the JVM, the reference.
  */
class Ed25519ExtendedKeyTest extends AnyFunSuite {

    /** The extended key at m/1852'/1815'/0'/0/0 of the cardano-addresses README mnemonic "nothing
      * heart matrix fly sleep slogan tomato pulse what roof rail since plastic false enlist" (the
      * vector of `Bip32Ed25519Test` in scalus-cardano-ledger).
      */
    private val xsk = ExtendedSigningKey.unsafeFromByteString(
      ByteString.fromHex(
        "b0bf46232c7f0f58ad333030e43ffbea7c2bb6f8135bd05fb0d343ade8453c5e" +
            "acc7ac09f77e16b635832522107eaa9f56db88c615f537aa6025e6c23da98ae8"
      )
    )

    /** Its public key, from the same cardano-addresses vector. */
    private val pk = VerificationKey.unsafeFromByteString(
      ByteString.fromHex("fbbbf6410e24532f35e9279febb085d2cc05b3b2ada1df77ea1951eb694f3834")
    )

    private val msg = ByteString.fromString("scalus extended key test")

    /** `signExtended(xsk, pk, msg)`, computed once with `JvmEd25519Signer` (bcprov). */
    private val expectedSig = ByteString.fromHex(
      "3981ac7360f2667c1d1085fda7242a81453cd7314f9d537953d65eaf2b813381" +
          "fb8a7155372095fd4008ed63f300c4dcad3fad4e0311967effb3a14e7423760d"
    )

    test("scalarMultiplyBase(kL) gives the cardano-addresses public key") {
        val kL = xsk.bytes.take(32)
        assert(Hex.bytesToHex(Ed25519Math.scalarMultiplyBase(kL)) == pk.toHex)
    }

    test("signExtended gives the JVM signature") {
        val sig = summon[Ed25519Signer].signExtended(xsk, pk, msg)
        assert(sig.toHex == expectedSig.toHex)
    }

    test("the signExtended signature verifies under the public key") {
        val sig = summon[Ed25519Signer].signExtended(xsk, pk, msg)
        assert(platform.verifyEd25519Signature(pk, msg, sig))
    }
}
