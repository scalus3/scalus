package scalus.crypto.ed25519

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin.{platform, ByteString}
import scalus.uplc.test.ArbitraryInstances

import scala.util.{Failure, Success, Try}

/** Ed25519 sign and verify of the platform: a signature verifies, and any 1-bit change of the
  * signature, the message or the public key makes it fail.
  */
class Ed25519PropertiesTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {
    override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 50)

    private val signer = summon[Ed25519Signer]

    /** A random signing key, its public key, a random message and its signature. */
    private case class Signed(pk: ByteString, msg: ByteString, sig: ByteString)

    private def genSigned(minMsgSize: Int): Gen[Signed] =
        for
            sk <- genByteStringOfN(32)
            msgSize <- Gen.choose(minMsgSize, 100)
            msg <- genByteStringOfN(msgSize)
        yield
            val pk = signer.derivePublicKey(SigningKey.unsafeFromByteString(sk))
            Signed(pk, msg, platform.signEd25519(sk, msg))

    /** A copy of `bs` with bit `bit` (0 is the low bit of byte 0) flipped. */
    private def flip(bs: ByteString, bit: Int): ByteString = {
        val bytes = bs.bytes.clone()
        bytes(bit / 8) = (bytes(bit / 8) ^ (1 << (bit % 8))).toByte
        ByteString.unsafeFromArray(bytes)
    }

    private def bitOf(bs: ByteString): Gen[Int] = Gen.choose(0, bs.size * 8 - 1)

    test("verify(pk, msg, sign(sk, msg)) is true") {
        forAll(genSigned(minMsgSize = 0)) { s =>
            assert(platform.verifyEd25519Signature(s.pk, s.msg, s.sig))
        }
    }

    test("flipping one bit of the signature makes verify false") {
        forAll(genSigned(minMsgSize = 0).flatMap(s => bitOf(s.sig).map((s, _)))) { (s, bit) =>
            assert(!platform.verifyEd25519Signature(s.pk, s.msg, flip(s.sig, bit)))
        }
    }

    test("flipping one bit of the message makes verify false") {
        forAll(genSigned(minMsgSize = 1).flatMap(s => bitOf(s.msg).map((s, _)))) { (s, bit) =>
            assert(!platform.verifyEd25519Signature(s.pk, flip(s.msg, bit), s.sig))
        }
    }

    test("flipping one bit of the public key never makes verify true") {
        // The flipped key may not decode to a point: verify may then return false or throw.
        var threw = 0
        var rejected = 0
        forAll(genSigned(minMsgSize = 0).flatMap(s => bitOf(s.pk).map((s, _)))) { (s, bit) =>
            Try(platform.verifyEd25519Signature(flip(s.pk, bit), s.msg, s.sig)) match
                case Success(ok) =>
                    assert(!ok)
                    rejected += 1
                case Failure(_) => threw += 1
        }
        info(s"flipped public keys: verify returned false $rejected times, threw $threw times")
    }
}
