package scalus.crypto.ed25519

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class Ed25519LibsodiumRulesTest extends AnyFunSuite {
    import Ed25519LibsodiumRules.*

    private def hex(s: String): Array[Byte] = ByteString.fromHex(s).bytes

    // L = 2^252 + 27742317777372353535851937790883648493, little-endian
    private val L = hex("edd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010")
    private val LMinus1 = hex("ecd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010")
    private val basePoint = hex("5866666666666666666666666666666666666666666666666666666666666666")
    private val rfc8032Pk = hex("d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a")
    private val rfc8032Sig = hex(
      "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
    )

    test("scalar is canonical below L only") {
        assert(isCanonicalScalar(LMinus1))
        assert(!isCanonicalScalar(L))
        assert(!isCanonicalScalar(Array.fill(32)(0xff.toByte)))
        assert(isCanonicalScalar(new Array[Byte](32)))
    }

    /** libsodium's 7 small-order encodings, without the sign bit (`ed25519_ref10.c:1022-1053`). */
    private val smallOrder = Seq(
      "0000000000000000000000000000000000000000000000000000000000000000",
      "0100000000000000000000000000000000000000000000000000000000000000",
      "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
      "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
      "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
      "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
      "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
    ).map(hex)

    private def withSignBit(point: Array[Byte]): Array[Byte] =
        point.updated(31, (point(31) | 0x80).toByte)

    for (encoding, i) <- smallOrder.zipWithIndex do {
        test(s"small-order encoding $i matches without the sign bit") {
            assert(isSmallOrder(encoding))
        }
        test(s"small-order encoding $i matches with the sign bit") {
            assert(isSmallOrder(withSignBit(encoding)))
        }
    }

    test("points of large order are not small-order") {
        assert(!isSmallOrder(basePoint))
        assert(!isSmallOrder(withSignBit(basePoint)))
        assert(!isSmallOrder(rfc8032Pk))
    }

    test("point is canonical when y < p") {
        val pMinus1 = hex("ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
        val p = hex("edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
        val pSigned = hex("edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
        assert(isCanonicalPoint(pMinus1))
        assert(!isCanonicalPoint(p))
        assert(!isCanonicalPoint(pSigned))
        assert(isCanonicalPoint(basePoint))
    }

    test("point with y < p and the sign bit set is canonical") {
        assert(isCanonicalPoint(withSignBit(basePoint)))
    }

    test("pre-checks accept RFC 8032 test 1 and reject R = identity") {
        val good = rfc8032Sig
        val identityR = hex(
          "0100000000000000000000000000000000000000000000000000000000000000943d85c895b02c2a57afbba668e5641527063f11dff33f5a6d2de65e6b45f00e"
        )
        assert(passesPreChecks(rfc8032Pk, good))
        assert(!passesPreChecks(rfc8032Pk, identityR))
        assert(!passesPreChecks(rfc8032Pk, good.take(32) ++ L))
    }

    test("pre-checks reject a small-order public key") {
        assert(passesPreChecks(rfc8032Pk, rfc8032Sig))
        for encoding <- smallOrder do {
            assert(!passesPreChecks(encoding, rfc8032Sig))
            assert(!passesPreChecks(withSignBit(encoding), rfc8032Sig))
        }
    }

    test("pre-checks reject a non-canonical public key") {
        // y = p is also on the small-order list, so y = p + 2 tests the canonical check alone.
        val p = hex("edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
        val pPlus2 = hex("efffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
        assert(!isSmallOrder(pPlus2))
        assert(!passesPreChecks(p, rfc8032Sig))
        assert(!passesPreChecks(pPlus2, rfc8032Sig))
        assert(!passesPreChecks(withSignBit(pPlus2), rfc8032Sig))
    }
}
