package scalus.crypto

/** Pure Scala implementation of Keccak-256 and SHA3-256 hash functions.
  *
  * Implementation follows NIST FIPS 202 and is cross-checked with BouncyCastle.
  *
  * The Keccak-f[1600] permutation operates on a 1600-bit state (25 × 64-bit lanes). For 256-bit
  * output with capacity 512, the rate is 1088 bits (136 bytes).
  *
  * The only difference between Keccak-256 and SHA3-256 is the domain separation suffix:
  *   - Keccak-256: suffix 0x01
  *   - SHA3-256: suffix 0x06
  */
object Keccak {
    // Rate in bytes for 256-bit output: (1600 - 2*256) / 8 = 136
    private val Rate = 136

    // Round constants for iota step (24 rounds)
    private val RC: Array[Long] = Array(
      0x0000000000000001L, 0x0000000000008082L, 0x800000000000808aL, 0x8000000080008000L,
      0x000000000000808bL, 0x0000000080000001L, 0x8000000080008081L, 0x8000000000008009L,
      0x000000000000008aL, 0x0000000000000088L, 0x0000000080008009L, 0x000000008000000aL,
      0x000000008000808bL, 0x800000000000008bL, 0x8000000000008089L, 0x8000000000008003L,
      0x8000000000008002L, 0x8000000000000080L, 0x000000000000800aL, 0x800000008000000aL,
      0x8000000080008081L, 0x8000000000008080L, 0x0000000080000001L, 0x8000000080008008L
    )

    /** Compute Keccak-256 hash (used by Ethereum).
      *
      * @param input
      *   the input bytes
      * @return
      *   32-byte hash
      */
    def keccak256(input: Array[Byte]): Array[Byte] = {
        hash(input, 0x01.toByte)
    }

    /** Compute SHA3-256 hash (NIST FIPS 202).
      *
      * @param input
      *   the input bytes
      * @return
      *   32-byte hash
      */
    def sha3_256(input: Array[Byte]): Array[Byte] = {
        hash(input, 0x06.toByte)
    }

    /** Core hash function with configurable domain separation.
      *
      * @param input
      *   the input bytes
      * @param suffix
      *   domain separation suffix (0x01 for Keccak, 0x06 for SHA3)
      * @return
      *   32-byte hash
      */
    private def hash(input: Array[Byte], suffix: Byte): Array[Byte] =
        // Initialize state (25 lanes × 64 bits = 1600 bits)
        val state = new Array[Long](25)

        // Absorb phase: process input in Rate-byte blocks
        var offset = 0
        val len = input.length

        // Process complete blocks
        while offset + Rate <= len do
            absorbBlock(state, input, offset)
            keccakF(state)
            offset += Rate

        // Handle final block with padding
        val remaining = len - offset
        val lastBlock = new Array[Byte](Rate)

        // Copy remaining bytes
        if remaining > 0 then System.arraycopy(input, offset, lastBlock, 0, remaining)

        // Apply padding: suffix byte at remaining position, 0x80 at last position
        // If remaining == Rate - 1, both suffix and 0x80 go in same byte
        lastBlock(remaining) = suffix
        lastBlock(Rate - 1) = (lastBlock(Rate - 1) | 0x80).toByte

        absorbBlock(state, lastBlock, 0)
        keccakF(state)

        // Squeeze phase: extract 32 bytes (256 bits) of output
        val output = new Array[Byte](32)
        var i = 0
        while i < 4 do
            val lane = state(i)
            val base = i * 8
            output(base) = lane.toByte
            output(base + 1) = (lane >>> 8).toByte
            output(base + 2) = (lane >>> 16).toByte
            output(base + 3) = (lane >>> 24).toByte
            output(base + 4) = (lane >>> 32).toByte
            output(base + 5) = (lane >>> 40).toByte
            output(base + 6) = (lane >>> 48).toByte
            output(base + 7) = (lane >>> 56).toByte
            i += 1
        output

    /** Absorb a Rate-byte block into state (XOR and convert to little-endian longs). */
    private def absorbBlock(state: Array[Long], block: Array[Byte], offset: Int): Unit =
        // Rate = 136 bytes = 17 longs
        var i = 0
        var pos = offset
        while i < 17 do
            val lane =
                (block(pos).toLong & 0xffL) |
                    ((block(pos + 1).toLong & 0xffL) << 8) |
                    ((block(pos + 2).toLong & 0xffL) << 16) |
                    ((block(pos + 3).toLong & 0xffL) << 24) |
                    ((block(pos + 4).toLong & 0xffL) << 32) |
                    ((block(pos + 5).toLong & 0xffL) << 40) |
                    ((block(pos + 6).toLong & 0xffL) << 48) |
                    ((block(pos + 7).toLong & 0xffL) << 56)
            state(i) ^= lane
            i += 1
            pos += 8

    /** Keccak-f[1600] permutation - 24 rounds.
      *
      * Implementation follows BouncyCastle's optimized version with unrolled loops and inline
      * rho/pi combination.
      */
    private def keccakF(A: Array[Long]): Unit =
        var a00 = A(0)
        var a01 = A(1)
        var a02 = A(2)
        var a03 = A(3)
        var a04 = A(4)
        var a05 = A(5)
        var a06 = A(6)
        var a07 = A(7)
        var a08 = A(8)
        var a09 = A(9)
        var a10 = A(10)
        var a11 = A(11)
        var a12 = A(12)
        var a13 = A(13)
        var a14 = A(14)
        var a15 = A(15)
        var a16 = A(16)
        var a17 = A(17)
        var a18 = A(18)
        var a19 = A(19)
        var a20 = A(20)
        var a21 = A(21)
        var a22 = A(22)
        var a23 = A(23)
        var a24 = A(24)

        var round = 0
        while round < 24 do
            // Theta step
            var c0 = a00 ^ a05 ^ a10 ^ a15 ^ a20
            var c1 = a01 ^ a06 ^ a11 ^ a16 ^ a21
            val c2 = a02 ^ a07 ^ a12 ^ a17 ^ a22
            val c3 = a03 ^ a08 ^ a13 ^ a18 ^ a23
            val c4 = a04 ^ a09 ^ a14 ^ a19 ^ a24

            var d1 = (c1 << 1 | c1 >>> 63) ^ c4
            var d2 = (c2 << 1 | c2 >>> 63) ^ c0
            var d3 = (c3 << 1 | c3 >>> 63) ^ c1
            var d4 = (c4 << 1 | c4 >>> 63) ^ c2
            var d0 = (c0 << 1 | c0 >>> 63) ^ c3

            a00 ^= d1
            a05 ^= d1
            a10 ^= d1
            a15 ^= d1
            a20 ^= d1
            a01 ^= d2
            a06 ^= d2
            a11 ^= d2
            a16 ^= d2
            a21 ^= d2
            a02 ^= d3
            a07 ^= d3
            a12 ^= d3
            a17 ^= d3
            a22 ^= d3
            a03 ^= d4
            a08 ^= d4
            a13 ^= d4
            a18 ^= d4
            a23 ^= d4
            a04 ^= d0
            a09 ^= d0
            a14 ^= d0
            a19 ^= d0
            a24 ^= d0

            // Rho and Pi steps combined (from BouncyCastle)
            c1 = a01 << 1 | a01 >>> 63
            a01 = a06 << 44 | a06 >>> 20
            a06 = a09 << 20 | a09 >>> 44
            a09 = a22 << 61 | a22 >>> 3
            a22 = a14 << 39 | a14 >>> 25
            a14 = a20 << 18 | a20 >>> 46
            a20 = a02 << 62 | a02 >>> 2
            a02 = a12 << 43 | a12 >>> 21
            a12 = a13 << 25 | a13 >>> 39
            a13 = a19 << 8 | a19 >>> 56
            a19 = a23 << 56 | a23 >>> 8
            a23 = a15 << 41 | a15 >>> 23
            a15 = a04 << 27 | a04 >>> 37
            a04 = a24 << 14 | a24 >>> 50
            a24 = a21 << 2 | a21 >>> 62
            a21 = a08 << 55 | a08 >>> 9
            a08 = a16 << 45 | a16 >>> 19
            a16 = a05 << 36 | a05 >>> 28
            a05 = a03 << 28 | a03 >>> 36
            a03 = a18 << 21 | a18 >>> 43
            a18 = a17 << 15 | a17 >>> 49
            a17 = a11 << 10 | a11 >>> 54
            a11 = a07 << 6 | a07 >>> 58
            a07 = a10 << 3 | a10 >>> 61
            a10 = c1

            // Chi step (5 rows)
            c0 = a00 ^ (~a01 & a02)
            c1 = a01 ^ (~a02 & a03)
            a02 ^= ~a03 & a04
            a03 ^= ~a04 & a00
            a04 ^= ~a00 & a01
            a00 = c0
            a01 = c1

            c0 = a05 ^ (~a06 & a07)
            c1 = a06 ^ (~a07 & a08)
            a07 ^= ~a08 & a09
            a08 ^= ~a09 & a05
            a09 ^= ~a05 & a06
            a05 = c0
            a06 = c1

            c0 = a10 ^ (~a11 & a12)
            c1 = a11 ^ (~a12 & a13)
            a12 ^= ~a13 & a14
            a13 ^= ~a14 & a10
            a14 ^= ~a10 & a11
            a10 = c0
            a11 = c1

            c0 = a15 ^ (~a16 & a17)
            c1 = a16 ^ (~a17 & a18)
            a17 ^= ~a18 & a19
            a18 ^= ~a19 & a15
            a19 ^= ~a15 & a16
            a15 = c0
            a16 = c1

            c0 = a20 ^ (~a21 & a22)
            c1 = a21 ^ (~a22 & a23)
            a22 ^= ~a23 & a24
            a23 ^= ~a24 & a20
            a24 ^= ~a20 & a21
            a20 = c0
            a21 = c1

            // Iota step
            a00 ^= RC(round)

            round += 1

        A(0) = a00
        A(1) = a01
        A(2) = a02
        A(3) = a03
        A(4) = a04
        A(5) = a05
        A(6) = a06
        A(7) = a07
        A(8) = a08
        A(9) = a09
        A(10) = a10
        A(11) = a11
        A(12) = a12
        A(13) = a13
        A(14) = a14
        A(15) = a15
        A(16) = a16
        A(17) = a17
        A(18) = a18
        A(19) = a19
        A(20) = a20
        A(21) = a21
        A(22) = a22
        A(23) = a23
        A(24) = a24
}
