package scalus.cardano.onchain.plutus.prelude.crypto.bls12_381

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.Option
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.Scalar.*

class ScalarTest extends AnyFunSuiteLike:

    private inline def s834 = Scalar.applyUnsafe(834884848)

    test("apply") {
        assert(Scalar(1) == Some(new Scalar(1)))
        assert(Scalar(Scalar.fieldPrime) == None)
        assert(Scalar(834884848) == Some(s834))
        assert(Scalar(BigInt(834884848)) == Some(s834))
        assert(Scalar("834884848") == Some(s834))
    }

    test("fromByteStringBigEndian") {
        assert(
          fromByteStringBigEndian(ByteString.fromHex("ffff00")) ==
              Scalar(16776960)
        )
    }

    test("fromByteStringBigEndianUnsafe") {
        assert(
          fromByteStringBigEndianUnsafe(ByteString.fromHex("ffff00")) ==
              Scalar(16776960).get
        )
    }

    test("fromByteStringLittleEndian") {
        assert(
          fromByteStringLittleEndian(ByteString.fromHex("ffff00")) ==
              Scalar(65535)
        )
    }

    test("fromByteStringLittleEndianUnsafe") {
        assert(
          fromByteStringLittleEndianUnsafe(ByteString.fromHex("ffff00")) ==
              Scalar(65535).get
        )
    }

    test("add") {
        assert(s834 + s834 == new Scalar(1669769696))
        assert(new Scalar(fieldPrime - 1) + new Scalar(1) == Scalar.zero)
        assert(new Scalar(3) + new Scalar(fieldPrime) == new Scalar(3))
    }

    test("mul") {
        val s = s834
        assert(s * s == Scalar("697032709419983104").get)
        assert(Scalar.zero * s834 == Scalar.zero)
        assert(
          new Scalar(fieldPrime - 1) * new Scalar(2) ==
              Scalar(
                "52435875175126190479447740508185965837690552500527637822603658699938581184511"
              ).get
        )
    }

    test("scale") {
        val s = s834
        assert(s.scale(-1) == Scalar.zero)
        assert(s.scale(0) == Scalar.one)
        assert(s.scale(1) == s)
        assert(s.scale(2) == Scalar("697032709419983104").get)
        assert(s.scale(3) == Scalar("581942047655130761945608192").get)
        assert(
          new Scalar(fieldPrime - 4).scale(200) ==
              Scalar(
                "12843927705572658539565969578937286576443167978938369866871449552629978143484"
              ).get
        )
    }

    test("scale2") {
        val s = s834
        assert(s.scale2(-1) == Scalar.zero)
        assert(s.scale2(0) == s)
        assert(s.scale2(1) == s.scale(2))
        assert(s.scale2(2) == s.scale(4))
        assert(s.scale2(3) == s.scale(8))
        assert(s.scale2(4) == s.scale(16))
    }

    test("div") {
        val s = s834
        assert(s / s == Some(Scalar.one))
        assert(s / Scalar.zero == None)
        assert(
          new Scalar(fieldPrime - 1) / new Scalar(2) ==
              Scalar(
                "26217937587563095239723870254092982918845276250263818911301829349969290592256"
              )
        )
    }

    test("neg") {
        assert(
          -s834 ==
              Scalar(
                "52435875175126190479447740508185965837690552500527637822603658699937746299665"
              ).get
        )
        assert(-Scalar.zero == Scalar.zero)
        assert(-Scalar.one == new Scalar(fieldPrime - 1))
    }

    test("recip") {
        assert(
          s834.recip ==
              Scalar(
                "35891248691642227249400403463796410930702563777316955162085759263735363466421"
              )
        )
        assert(Scalar.zero.recip == None)
    }

    test("sub") {
        val s = s834
        assert(s - s == Scalar.zero)
        assert(s - new Scalar(834884847) == Scalar.one)
        assert(Scalar.zero - new Scalar(fieldPrime) == Scalar.zero)
        assert(Scalar.zero - new Scalar(5) == new Scalar(fieldPrime - 5))
    }
