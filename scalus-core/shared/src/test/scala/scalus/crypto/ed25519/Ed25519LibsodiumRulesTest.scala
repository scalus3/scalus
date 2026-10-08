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

    test("scalar is canonical below L only") {
        assert(isCanonicalScalar(LMinus1))
        assert(!isCanonicalScalar(L))
        assert(!isCanonicalScalar(Array.fill(32)(0xff.toByte)))
        assert(isCanonicalScalar(new Array[Byte](32)))
    }

    test("small-order list matches with and without the sign bit") {
        val order8 = hex("c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a")
        val order8Signed = hex("c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa")
        assert(isSmallOrder(order8))
        assert(isSmallOrder(order8Signed))
        assert(
          isSmallOrder(hex("0100000000000000000000000000000000000000000000000000000000000000"))
        )
        assert(
          isSmallOrder(hex("eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"))
        )
        assert(!isSmallOrder(basePoint))
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

    test("pre-checks accept RFC 8032 test 1 and reject R = identity") {
        val good = hex(
          "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
        )
        val identityR = hex(
          "0100000000000000000000000000000000000000000000000000000000000000943d85c895b02c2a57afbba668e5641527063f11dff33f5a6d2de65e6b45f00e"
        )
        assert(passesPreChecks(rfc8032Pk, good))
        assert(!passesPreChecks(rfc8032Pk, identityR))
        assert(!passesPreChecks(rfc8032Pk, good.take(32) ++ L))
    }
}
