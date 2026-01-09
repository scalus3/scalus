package scalus.crypto

/** Pure Scala implementation of SHA-512 hash function.
  *
  * Implementation follows FIPS 180-4 (Secure Hash Standard). SHA-512 operates on 1024-bit blocks
  * and produces a 512-bit (64-byte) digest.
  *
  * @see
  *   https://csrc.nist.gov/publications/detail/fips/180/4/final
  */
object Sha512 {
    // Initial hash values (first 64 bits of fractional parts of square roots of first 8 primes)
    private val H0: Array[Long] = Array(
      0x6a09e667f3bcc908L, 0xbb67ae8584caa73bL, 0x3c6ef372fe94f82bL, 0xa54ff53a5f1d36f1L,
      0x510e527fade682d1L, 0x9b05688c2b3e6c1fL, 0x1f83d9abfb41bd6bL, 0x5be0cd19137e2179L
    )

    // Round constants (first 64 bits of fractional parts of cube roots of first 80 primes)
    private val K: Array[Long] = Array(
      0x428a2f98d728ae22L, 0x7137449123ef65cdL, 0xb5c0fbcfec4d3b2fL, 0xe9b5dba58189dbbcL,
      0x3956c25bf348b538L, 0x59f111f1b605d019L, 0x923f82a4af194f9bL, 0xab1c5ed5da6d8118L,
      0xd807aa98a3030242L, 0x12835b0145706fbeL, 0x243185be4ee4b28cL, 0x550c7dc3d5ffb4e2L,
      0x72be5d74f27b896fL, 0x80deb1fe3b1696b1L, 0x9bdc06a725c71235L, 0xc19bf174cf692694L,
      0xe49b69c19ef14ad2L, 0xefbe4786384f25e3L, 0x0fc19dc68b8cd5b5L, 0x240ca1cc77ac9c65L,
      0x2de92c6f592b0275L, 0x4a7484aa6ea6e483L, 0x5cb0a9dcbd41fbd4L, 0x76f988da831153b5L,
      0x983e5152ee66dfabL, 0xa831c66d2db43210L, 0xb00327c898fb213fL, 0xbf597fc7beef0ee4L,
      0xc6e00bf33da88fc2L, 0xd5a79147930aa725L, 0x06ca6351e003826fL, 0x142929670a0e6e70L,
      0x27b70a8546d22ffcL, 0x2e1b21385c26c926L, 0x4d2c6dfc5ac42aedL, 0x53380d139d95b3dfL,
      0x650a73548baf63deL, 0x766a0abb3c77b2a8L, 0x81c2c92e47edaee6L, 0x92722c851482353bL,
      0xa2bfe8a14cf10364L, 0xa81a664bbc423001L, 0xc24b8b70d0f89791L, 0xc76c51a30654be30L,
      0xd192e819d6ef5218L, 0xd69906245565a910L, 0xf40e35855771202aL, 0x106aa07032bbd1b8L,
      0x19a4c116b8d2d0c8L, 0x1e376c085141ab53L, 0x2748774cdf8eeb99L, 0x34b0bcb5e19b48a8L,
      0x391c0cb3c5c95a63L, 0x4ed8aa4ae3418acbL, 0x5b9cca4f7763e373L, 0x682e6ff3d6b2b8a3L,
      0x748f82ee5defb2fcL, 0x78a5636f43172f60L, 0x84c87814a1f0ab72L, 0x8cc702081a6439ecL,
      0x90befffa23631e28L, 0xa4506cebde82bde9L, 0xbef9a3f7b2c67915L, 0xc67178f2e372532bL,
      0xca273eceea26619cL, 0xd186b8c721c0c207L, 0xeada7dd6cde0eb1eL, 0xf57d4f7fee6ed178L,
      0x06f067aa72176fbaL, 0x0a637dc5a2c898a6L, 0x113f9804bef90daeL, 0x1b710b35131c471bL,
      0x28db77f523047d84L, 0x32caab7b40c72493L, 0x3c9ebe0a15c9bebcL, 0x431d67c49c100d4cL,
      0x4cc5d4becb3e42b6L, 0x597f299cfc657e2aL, 0x5fcb6fab3ad6faecL, 0x6c44198c4a475817L
    )

    // Block size in bytes (1024 bits)
    private val BlockSize = 128

    /** Compute SHA-512 hash.
      *
      * @param input
      *   the input bytes
      * @return
      *   64-byte hash
      */
    def hash(input: Array[Byte]): Array[Byte] = {
        // Initialize hash values
        val h = H0.clone()

        // Pre-processing: add padding
        val paddedMessage = pad(input)

        // Process each 1024-bit block
        var offset = 0
        while offset < paddedMessage.length do
            processBlock(h, paddedMessage, offset)
            offset += BlockSize

        // Produce final hash value (big-endian)
        val result = new Array[Byte](64)
        var i = 0
        while i < 8 do
            val v = h(i)
            val base = i * 8
            result(base) = (v >>> 56).toByte
            result(base + 1) = (v >>> 48).toByte
            result(base + 2) = (v >>> 40).toByte
            result(base + 3) = (v >>> 32).toByte
            result(base + 4) = (v >>> 24).toByte
            result(base + 5) = (v >>> 16).toByte
            result(base + 6) = (v >>> 8).toByte
            result(base + 7) = v.toByte
            i += 1
        result
    }

    /** Pad the message according to SHA-512 specification.
      *
      * Padding: append bit '1', then k zero bits, then 128-bit message length where k is the
      * minimum number >= 0 such that (message length + 1 + k + 128) is a multiple of 1024.
      */
    private def pad(message: Array[Byte]): Array[Byte] = {
        val msgLen = message.length
        val bitLen = msgLen.toLong * 8

        // Calculate padding length
        // We need: (msgLen + 1 + padLen + 16) % 128 == 0
        // So padLen = (128 - (msgLen + 1 + 16) % 128) % 128
        val padLen = (128 - (msgLen + 1 + 16) % 128) % 128

        val paddedLen = msgLen + 1 + padLen + 16
        val padded = new Array[Byte](paddedLen)

        // Copy message
        System.arraycopy(message, 0, padded, 0, msgLen)

        // Append '1' bit (0x80)
        padded(msgLen) = 0x80.toByte

        // Zero padding is already done (array initialized to zeros)

        // Append message length as 128-bit big-endian
        // Upper 64 bits (for messages < 2^64 bits, this is 0)
        // Lower 64 bits
        val lenOffset = paddedLen - 8
        padded(lenOffset) = (bitLen >>> 56).toByte
        padded(lenOffset + 1) = (bitLen >>> 48).toByte
        padded(lenOffset + 2) = (bitLen >>> 40).toByte
        padded(lenOffset + 3) = (bitLen >>> 32).toByte
        padded(lenOffset + 4) = (bitLen >>> 24).toByte
        padded(lenOffset + 5) = (bitLen >>> 16).toByte
        padded(lenOffset + 6) = (bitLen >>> 8).toByte
        padded(lenOffset + 7) = bitLen.toByte

        padded
    }

    /** Process a single 1024-bit block. */
    private def processBlock(h: Array[Long], block: Array[Byte], offset: Int): Unit = {
        // Message schedule array (80 64-bit words)
        val W = new Array[Long](80)

        // Prepare message schedule
        var t = 0
        while t < 16 do
            val i = offset + t * 8
            W(t) = ((block(i).toLong & 0xff) << 56) |
                ((block(i + 1).toLong & 0xff) << 48) |
                ((block(i + 2).toLong & 0xff) << 40) |
                ((block(i + 3).toLong & 0xff) << 32) |
                ((block(i + 4).toLong & 0xff) << 24) |
                ((block(i + 5).toLong & 0xff) << 16) |
                ((block(i + 6).toLong & 0xff) << 8) |
                (block(i + 7).toLong & 0xff)
            t += 1

        while t < 80 do
            W(t) = sigma1(W(t - 2)) + W(t - 7) + sigma0(W(t - 15)) + W(t - 16)
            t += 1

        // Initialize working variables
        var a = h(0)
        var b = h(1)
        var c = h(2)
        var d = h(3)
        var e = h(4)
        var f = h(5)
        var g = h(6)
        var hh = h(7)

        // Main loop (80 rounds)
        t = 0
        while t < 80 do
            val t1 = hh + Sigma1(e) + Ch(e, f, g) + K(t) + W(t)
            val t2 = Sigma0(a) + Maj(a, b, c)
            hh = g
            g = f
            f = e
            e = d + t1
            d = c
            c = b
            b = a
            a = t1 + t2
            t += 1

        // Compute intermediate hash value
        h(0) += a
        h(1) += b
        h(2) += c
        h(3) += d
        h(4) += e
        h(5) += f
        h(6) += g
        h(7) += hh
    }

    // Logical functions

    /** Ch(x, y, z) = (x AND y) XOR (NOT x AND z) */
    private inline def Ch(x: Long, y: Long, z: Long): Long =
        (x & y) ^ (~x & z)

    /** Maj(x, y, z) = (x AND y) XOR (x AND z) XOR (y AND z) */
    private inline def Maj(x: Long, y: Long, z: Long): Long =
        (x & y) ^ (x & z) ^ (y & z)

    /** Σ0(x) = ROTR^28(x) XOR ROTR^34(x) XOR ROTR^39(x) */
    private inline def Sigma0(x: Long): Long =
        java.lang.Long.rotateRight(x, 28) ^ java.lang.Long.rotateRight(x, 34) ^
            java.lang.Long.rotateRight(x, 39)

    /** Σ1(x) = ROTR^14(x) XOR ROTR^18(x) XOR ROTR^41(x) */
    private inline def Sigma1(x: Long): Long =
        java.lang.Long.rotateRight(x, 14) ^ java.lang.Long.rotateRight(x, 18) ^
            java.lang.Long.rotateRight(x, 41)

    /** σ0(x) = ROTR^1(x) XOR ROTR^8(x) XOR SHR^7(x) */
    private inline def sigma0(x: Long): Long =
        java.lang.Long.rotateRight(x, 1) ^ java.lang.Long.rotateRight(x, 8) ^ (x >>> 7)

    /** σ1(x) = ROTR^19(x) XOR ROTR^61(x) XOR SHR^6(x) */
    private inline def sigma1(x: Long): Long =
        java.lang.Long.rotateRight(x, 19) ^ java.lang.Long.rotateRight(x, 61) ^ (x >>> 6)
}
