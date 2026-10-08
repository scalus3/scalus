package scalus.crypto.ed25519

import scalus.uplc.builtin.ByteString

/** The checks libsodium 1.0.18 makes before it evaluates the Ed25519 equation
  * (`crypto_sign/ed25519/ref10/open.c:31-39`).
  *
  * The Cardano node verifies Ed25519 with libsodium, so Scalus rejects what libsodium rejects. Run
  * these checks before the cofactorless equation; the equation alone accepts a small-order R.
  *
  * Like libsodium, the checks compare 32-byte little-endian encodings byte by byte, from the most
  * significant byte down.
  */
private[scalus] object Ed25519LibsodiumRules {

    /** The group order L = 2^252 + 27742317777372353535851937790883648493, little-endian. */
    private val L: Array[Byte] =
        ByteString.fromHex("edd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010").bytes

    /** The field prime p = 2^255 - 19, little-endian. */
    private val P: Array[Byte] =
        ByteString.fromHex("edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f").bytes

    /** libsodium's list (`ed25519_ref10.c:1022-1053`), compared with the sign bit masked. */
    private val smallOrderEncodings: Array[Array[Byte]] = Array(
      "0000000000000000000000000000000000000000000000000000000000000000",
      "0100000000000000000000000000000000000000000000000000000000000000",
      "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
      "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
      "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
      "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
      "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
    ).map(ByteString.fromHex(_).bytes)

    /** Clears the sign bit (bit 7 of byte 31) of a point encoding. */
    private val SignBitMask = 0x7f
    private val NoMask = 0xff

    /** Byte `i` of the 32-byte encoding at `offset`, unsigned; byte 31 is masked with `topMask`. */
    private def byteAt(bytes: Array[Byte], offset: Int, i: Int, topMask: Int): Int =
        if i == 31 then bytes(offset + 31) & topMask else bytes(offset + i) & 0xff

    /** Whether the 32 little-endian bytes at `offset` are below `bound`. */
    private def lessThan(
        bytes: Array[Byte],
        offset: Int,
        bound: Array[Byte],
        topMask: Int
    ): Boolean = {
        var i = 31
        while i >= 0 do
            val a = byteAt(bytes, offset, i, topMask)
            val b = bound(i) & 0xff
            if a != b then return a < b
            i -= 1
        false
    }

    /** `sc25519_is_canonical`: the scalar at `offset` is below L. */
    private def isCanonicalScalarAt(bytes: Array[Byte], offset: Int): Boolean =
        lessThan(bytes, offset, L, NoMask)

    /** `ge25519_is_canonical`: the y coordinate at `offset`, without the sign bit, is below p. */
    private def isCanonicalPointAt(bytes: Array[Byte], offset: Int): Boolean =
        lessThan(bytes, offset, P, SignBitMask)

    /** `ge25519_has_small_order`: the point at `offset`, without the sign bit, is in the list. */
    private def isSmallOrderAt(bytes: Array[Byte], offset: Int): Boolean =
        smallOrderEncodings.exists { encoding =>
            var i = 0
            while i < 32 && byteAt(bytes, offset, i, SignBitMask) == (encoding(i) & 0xff) do i += 1
            i == 32
        }

    def isCanonicalScalar(scalar: Array[Byte]): Boolean = isCanonicalScalarAt(scalar, 0)

    def isCanonicalPoint(point: Array[Byte]): Boolean = isCanonicalPointAt(point, 0)

    def isSmallOrder(point: Array[Byte]): Boolean = isSmallOrderAt(point, 0)

    /** Rules 1-4 of libsodium, plus "R is canonical". The last one changes no verdict: libsodium
      * compares R with a canonical encoding, so a non-canonical R never matches.
      */
    def passesPreChecks(pk: Array[Byte], sig: Array[Byte]): Boolean =
        isCanonicalScalarAt(sig, 32) &&
            !isSmallOrderAt(sig, 0) && isCanonicalPointAt(sig, 0) &&
            isCanonicalPointAt(pk, 0) && !isSmallOrderAt(pk, 0)
}
