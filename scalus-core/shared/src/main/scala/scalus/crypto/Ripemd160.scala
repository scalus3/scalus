package scalus.crypto

/** Pure Scala implementation of RIPEMD-160 hash function.
  *
  * Implementation follows the RIPEMD-160 specification by Dobbertin, Bosselaers, and Preneel.
  * Cross-checked with BouncyCastle and official test vectors.
  *
  * RIPEMD-160 processes 512-bit (64-byte) blocks and produces a 160-bit (20-byte) digest. It uses
  * two parallel processing lines (left and right) that are combined after 80 rounds.
  *
  * @see
  *   https://homes.esat.kuleuven.be/~bosselae/ripemd160.html
  */
object Ripemd160 {

    // ==================== Algorithm Constants ====================

    // Initial hash values (same as MD4/MD5)
    private val H0 = 0x67452301
    private val H1 = 0xefcdab89
    private val H2 = 0x98badcfe
    private val H3 = 0x10325476
    private val H4 = 0xc3d2e1f0

    // Left line constants (one per round group of 16)
    private val KL0 = 0x00000000
    private val KL1 = 0x5a827999
    private val KL2 = 0x6ed9eba1
    private val KL3 = 0x8f1bbcdc
    private val KL4 = 0xa953fd4e

    // Right line constants (one per round group of 16)
    private val KR0 = 0x50a28be6
    private val KR1 = 0x5c4dd124
    private val KR2 = 0x6d703ef3
    private val KR3 = 0x7a6d76e9
    private val KR4 = 0x00000000

    // Message word selection for left line (80 values)
    private val RL: Array[Int] = Array(
      // Round 0-15
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
      // Round 16-31
      7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8,
      // Round 32-47
      3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12,
      // Round 48-63
      1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2,
      // Round 64-79
      4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13
    )

    // Message word selection for right line (80 values)
    private val RR: Array[Int] = Array(
      // Round 0-15
      5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12,
      // Round 16-31
      6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2,
      // Round 32-47
      15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13,
      // Round 48-63
      8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14,
      // Round 64-79
      12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11
    )

    // Rotation amounts for left line (80 values)
    private val SL: Array[Int] = Array(
      // Round 0-15
      11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8,
      // Round 16-31
      7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12,
      // Round 32-47
      11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5,
      // Round 48-63
      11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12,
      // Round 64-79
      9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6
    )

    // Rotation amounts for right line (80 values)
    private val SR: Array[Int] = Array(
      // Round 0-15
      8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6,
      // Round 16-31
      9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11,
      // Round 32-47
      9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5,
      // Round 48-63
      15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8,
      // Round 64-79
      8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11
    )

    // ==================== Boolean Functions ====================

    // f1: x XOR y XOR z (rounds 0-15 left, 64-79 right)
    private inline def f1(x: Int, y: Int, z: Int): Int = x ^ y ^ z

    // f2: (x AND y) OR (NOT x AND z) (rounds 16-31 left, 48-63 right)
    private inline def f2(x: Int, y: Int, z: Int): Int = (x & y) | (~x & z)

    // f3: (x OR NOT y) XOR z (rounds 32-47 both lines)
    private inline def f3(x: Int, y: Int, z: Int): Int = (x | ~y) ^ z

    // f4: (x AND z) OR (y AND NOT z) (rounds 48-63 left, 16-31 right)
    private inline def f4(x: Int, y: Int, z: Int): Int = (x & z) | (y & ~z)

    // f5: x XOR (y OR NOT z) (rounds 64-79 left, 0-15 right)
    private inline def f5(x: Int, y: Int, z: Int): Int = x ^ (y | ~z)

    // Left rotation of 32-bit integer
    private inline def rotl(x: Int, n: Int): Int = (x << n) | (x >>> (32 - n))

    // ==================== Main Hash Function ====================

    /** Compute RIPEMD-160 hash.
      *
      * @param input
      *   the input bytes
      * @return
      *   20-byte hash
      */
    def ripemd160(input: Array[Byte]): Array[Byte] = {
        // Initialize hash values
        var h0 = H0
        var h1 = H1
        var h2 = H2
        var h3 = H3
        var h4 = H4

        // Pre-processing: add padding bits
        val originalLength = input.length
        val bitLength = originalLength.toLong * 8

        // Calculate padded length: message + 1 byte (0x80) + padding + 8 bytes (length)
        // Total must be multiple of 64 bytes
        val paddedLength = ((originalLength + 9 + 63) / 64) * 64
        val padded = new Array[Byte](paddedLength)

        // Copy original message
        System.arraycopy(input, 0, padded, 0, originalLength)

        // Append 0x80 byte
        padded(originalLength) = 0x80.toByte

        // Append length in bits as 64-bit little-endian integer
        val lenOffset = paddedLength - 8
        padded(lenOffset) = bitLength.toByte
        padded(lenOffset + 1) = (bitLength >>> 8).toByte
        padded(lenOffset + 2) = (bitLength >>> 16).toByte
        padded(lenOffset + 3) = (bitLength >>> 24).toByte
        padded(lenOffset + 4) = (bitLength >>> 32).toByte
        padded(lenOffset + 5) = (bitLength >>> 40).toByte
        padded(lenOffset + 6) = (bitLength >>> 48).toByte
        padded(lenOffset + 7) = (bitLength >>> 56).toByte

        // Process each 64-byte block
        val X = new Array[Int](16)
        var offset = 0
        while offset < paddedLength do
            // Convert block to 16 little-endian 32-bit words
            var i = 0
            while i < 16 do
                val pos = offset + i * 4
                X(i) = (padded(pos) & 0xff) |
                    ((padded(pos + 1) & 0xff) << 8) |
                    ((padded(pos + 2) & 0xff) << 16) |
                    ((padded(pos + 3) & 0xff) << 24)
                i += 1

            // Process block with two parallel lines
            var al = h0
            var bl = h1
            var cl = h2
            var dl = h3
            var el = h4

            var ar = h0
            var br = h1
            var cr = h2
            var dr = h3
            var er = h4

            // 80 rounds
            var j = 0
            while j < 80 do
                val jdiv16 = j / 16

                // Left line
                val fl = jdiv16 match
                    case 0 => f1(bl, cl, dl)
                    case 1 => f2(bl, cl, dl)
                    case 2 => f3(bl, cl, dl)
                    case 3 => f4(bl, cl, dl)
                    case 4 => f5(bl, cl, dl)

                val kl = jdiv16 match
                    case 0 => KL0
                    case 1 => KL1
                    case 2 => KL2
                    case 3 => KL3
                    case 4 => KL4

                var tl = al + fl + X(RL(j)) + kl
                tl = rotl(tl, SL(j)) + el
                al = el
                el = dl
                dl = rotl(cl, 10)
                cl = bl
                bl = tl

                // Right line - functions applied in reverse order
                val fr = jdiv16 match
                    case 0 => f5(br, cr, dr)
                    case 1 => f4(br, cr, dr)
                    case 2 => f3(br, cr, dr)
                    case 3 => f2(br, cr, dr)
                    case 4 => f1(br, cr, dr)

                val kr = jdiv16 match
                    case 0 => KR0
                    case 1 => KR1
                    case 2 => KR2
                    case 3 => KR3
                    case 4 => KR4

                var tr = ar + fr + X(RR(j)) + kr
                tr = rotl(tr, SR(j)) + er
                ar = er
                er = dr
                dr = rotl(cr, 10)
                cr = br
                br = tr

                j += 1

            // Final addition
            val t = h1 + cl + dr
            h1 = h2 + dl + er
            h2 = h3 + el + ar
            h3 = h4 + al + br
            h4 = h0 + bl + cr
            h0 = t

            offset += 64

        // Produce final hash value (little-endian)
        val output = new Array[Byte](20)
        encodeLE(h0, output, 0)
        encodeLE(h1, output, 4)
        encodeLE(h2, output, 8)
        encodeLE(h3, output, 12)
        encodeLE(h4, output, 16)
        output
    }

    // Encode 32-bit integer as little-endian bytes
    private inline def encodeLE(value: Int, output: Array[Byte], offset: Int): Unit = {
        output(offset) = value.toByte
        output(offset + 1) = (value >>> 8).toByte
        output(offset + 2) = (value >>> 16).toByte
        output(offset + 3) = (value >>> 24).toByte
    }
}
