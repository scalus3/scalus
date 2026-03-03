package scalus.crypto.accumulator

import java.math.BigInteger as JBigInt
import scalus.cardano.onchain.plutus.prelude.bls12_381.Scalar

/** Fixed 256-bit Montgomery arithmetic for the BLS12-381 scalar field.
  *
  * Values are represented as 4 unsigned 64-bit limbs in little-endian order within a Long array.
  * Element i in a flat data array occupies offsets [i*4, i*4+3].
  *
  * All hot-path operations are allocation-free: scratch arrays are passed by the caller and reused
  * across millions of operations. This eliminates the ~36GB of temporary BigInteger/int[]
  * allocations per benchmark run that dominated GC pressure.
  */
private[accumulator] object MontField256 {

    /** Number of 64-bit limbs per field element. */
    val LIMBS = 4

    private val jPrime: JBigInt = Scalar.fieldPrime.bigInteger
    private val R_BIG: JBigInt = JBigInt.ONE.shiftLeft(256)

    /** BLS12-381 scalar field prime as 4 little-endian longs. */
    val PRIME: Array[Long] = bigIntToLimbs(jPrime)

    /** Lowest limb of -P^{-1} mod 2^{256} (only the lowest limb is needed for CIOS). */
    val N_PRIME_0: Long = jPrime.modInverse(R_BIG).negate().mod(R_BIG).longValue()

    /** R^2 mod P, used for converting to Montgomery form. */
    val R_SQUARED: Array[Long] = bigIntToLimbs(R_BIG.multiply(R_BIG).mod(jPrime))

    /** 1 in Montgomery form = R mod P. */
    val ONE_MONT: Array[Long] = bigIntToLimbs(R_BIG.mod(jPrime))

    /** 1 in standard form, used for fromMont conversion. */
    val ONE_STD: Array[Long] = Array(1L, 0L, 0L, 0L)

    private def bigIntToLimbs(bi: JBigInt): Array[Long] = {
        Array(
          bi.longValue(),
          bi.shiftRight(64).longValue(),
          bi.shiftRight(128).longValue(),
          bi.shiftRight(192).longValue()
        )
    }

    /** Convert a java.math.BigInteger to 4 little-endian longs at offset. */
    def fromBigInteger(bi: JBigInt, r: Array[Long], off: Int): Unit = {
        r(off) = bi.longValue()
        r(off + 1) = bi.shiftRight(64).longValue()
        r(off + 2) = bi.shiftRight(128).longValue()
        r(off + 3) = bi.shiftRight(192).longValue()
    }

    /** Convert 4 little-endian longs at offset to a java.math.BigInteger. */
    def toBigInteger(a: Array[Long], off: Int): JBigInt = {
        val bytes = new Array[Byte](32)
        var i = 0
        while i < 4 do
            val v = a(off + i)
            var j = 0
            while j < 8 do
                bytes(31 - i * 8 - j) = ((v >>> (j * 8)) & 0xffL).toByte
                j += 1
            i += 1
        new JBigInt(1, bytes)
    }

    /** Unsigned multiply high: high 64 bits of unsigned 64x64 multiply. */
    private inline def umh(x: Long, y: Long): Long = {
        val hi = Math.multiplyHigh(x, y)
        // Correction from signed to unsigned: if x is negative (high bit set), add y; same for y
        hi + (y & (x >> 63)) + (x & (y >> 63))
    }

    /** Test a >= b as unsigned 256-bit values. */
    private def geq256(a: Array[Long], aOff: Int, b: Array[Long], bOff: Int): Boolean = {
        var i = 3
        while i >= 0 do
            val cmp = java.lang.Long.compareUnsigned(a(aOff + i), b(bOff + i))
            if cmp > 0 then return true
            if cmp < 0 then return false
            i -= 1
        true // equal
    }

    /** r = (a + b) mod P. Inputs must be in [0, P). */
    def addMod(
        a: Array[Long],
        aOff: Int,
        b: Array[Long],
        bOff: Int,
        r: Array[Long],
        rOff: Int
    ): Unit = {
        var carry = 0L
        var i = 0
        while i < 4 do
            val ai = a(aOff + i)
            val s1 = ai + b(bOff + i)
            val c1 = if java.lang.Long.compareUnsigned(s1, ai) < 0 then 1L else 0L
            val s2 = s1 + carry
            val c2 = if java.lang.Long.compareUnsigned(s2, s1) < 0 then 1L else 0L
            r(rOff + i) = s2
            carry = c1 + c2 // at most 1 (proven: max sum is 2^65 - 1)
            i += 1
        if carry != 0L || geq256(r, rOff, PRIME, 0) then
            var borrow = 0L
            i = 0
            while i < 4 do
                val ri = r(rOff + i)
                val d1 = ri - PRIME(i)
                val b1 = if java.lang.Long.compareUnsigned(ri, PRIME(i)) < 0 then 1L else 0L
                val d2 = d1 - borrow
                val b2 = if java.lang.Long.compareUnsigned(d1, borrow) < 0 then 1L else 0L
                r(rOff + i) = d2
                borrow = b1 + b2
                i += 1
    }

    /** r = (a - b) mod P. Inputs must be in [0, P). */
    def subMod(
        a: Array[Long],
        aOff: Int,
        b: Array[Long],
        bOff: Int,
        r: Array[Long],
        rOff: Int
    ): Unit = {
        var borrow = 0L
        var i = 0
        while i < 4 do
            val ai = a(aOff + i)
            val d1 = ai - b(bOff + i)
            val b1 = if java.lang.Long.compareUnsigned(ai, b(bOff + i)) < 0 then 1L else 0L
            val d2 = d1 - borrow
            val b2 = if java.lang.Long.compareUnsigned(d1, borrow) < 0 then 1L else 0L
            r(rOff + i) = d2
            borrow = b1 + b2
            i += 1
        if borrow != 0L then
            var carry = 0L
            i = 0
            while i < 4 do
                val ri = r(rOff + i)
                val s1 = ri + PRIME(i)
                val c1 = if java.lang.Long.compareUnsigned(s1, ri) < 0 then 1L else 0L
                val s2 = s1 + carry
                val c2 = if java.lang.Long.compareUnsigned(s2, s1) < 0 then 1L else 0L
                r(rOff + i) = s2
                carry = c1 + c2
                i += 1
    }

    /** Montgomery multiplication using CIOS (Coarsely Integrated Operand Scanning).
      *
      * Computes r = a * b * R^{-1} mod P where R = 2^{256}.
      *
      * @param scratch
      *   must have length >= 6, reused across calls for zero allocation
      */
    def montMul(
        a: Array[Long],
        aOff: Int,
        b: Array[Long],
        bOff: Int,
        r: Array[Long],
        rOff: Int,
        scratch: Array[Long]
    ): Unit = {
        val t = scratch
        t(0) = 0L; t(1) = 0L; t(2) = 0L; t(3) = 0L; t(4) = 0L; t(5) = 0L

        var i = 0
        while i < 4 do
            val ai = a(aOff + i)

            // Step 1: t += a[i] * b
            var carry = 0L
            var j = 0
            while j < 4 do
                val bj = b(bOff + j)
                val prodLo = ai * bj
                val prodHi = umh(ai, bj)
                val s1 = t(j) + prodLo
                val c1 = if java.lang.Long.compareUnsigned(s1, t(j)) < 0 then 1L else 0L
                val s2 = s1 + carry
                val c2 = if java.lang.Long.compareUnsigned(s2, s1) < 0 then 1L else 0L
                t(j) = s2
                carry = prodHi + c1 + c2
                j += 1
            val s4 = t(4) + carry
            t(5) = if java.lang.Long.compareUnsigned(s4, t(4)) < 0 then 1L else 0L
            t(4) = s4

            // Step 2: t += m * P, then shift right by 64 bits
            val m = t(0) * N_PRIME_0 // only low 64 bits needed

            // j = 0: (carry, _) = t[0] + m * P[0]  (low word is zero by REDC construction)
            val mp0Lo = m * PRIME(0)
            val mp0Hi = umh(m, PRIME(0))
            val red0 = t(0) + mp0Lo
            carry = mp0Hi + (if java.lang.Long.compareUnsigned(red0, t(0)) < 0 then 1L else 0L)

            // j = 1..3: t[j-1] = low(t[j] + m*P[j] + carry)
            j = 1
            while j < 4 do
                val mpLo = m * PRIME(j)
                val mpHi = umh(m, PRIME(j))
                val s1 = t(j) + mpLo
                val c1 = if java.lang.Long.compareUnsigned(s1, t(j)) < 0 then 1L else 0L
                val s2 = s1 + carry
                val c2 = if java.lang.Long.compareUnsigned(s2, s1) < 0 then 1L else 0L
                t(j - 1) = s2
                carry = mpHi + c1 + c2
                j += 1
            val s5 = t(4) + carry
            val c5 = if java.lang.Long.compareUnsigned(s5, t(4)) < 0 then 1L else 0L
            t(3) = s5
            t(4) = t(5) + c5

            i += 1

        // Final conditional subtraction
        if t(4) != 0L || geq256(t, 0, PRIME, 0) then
            var borrow = 0L
            var k = 0
            while k < 4 do
                val tk = t(k)
                val d1 = tk - PRIME(k)
                val b1 = if java.lang.Long.compareUnsigned(tk, PRIME(k)) < 0 then 1L else 0L
                val d2 = d1 - borrow
                val b2 = if java.lang.Long.compareUnsigned(d1, borrow) < 0 then 1L else 0L
                r(rOff + k) = d2
                borrow = b1 + b2
                k += 1
        else
            r(rOff) = t(0); r(rOff + 1) = t(1); r(rOff + 2) = t(2); r(rOff + 3) = t(3)
    }

    /** Convert to Montgomery form: r = a * R mod P. */
    def toMont(
        a: Array[Long],
        aOff: Int,
        r: Array[Long],
        rOff: Int,
        scratch: Array[Long]
    ): Unit = {
        montMul(a, aOff, R_SQUARED, 0, r, rOff, scratch)
    }

    /** Convert from Montgomery form: r = a * R^{-1} mod P. */
    def fromMont(
        a: Array[Long],
        aOff: Int,
        r: Array[Long],
        rOff: Int,
        scratch: Array[Long]
    ): Unit = {
        montMul(a, aOff, ONE_STD, 0, r, rOff, scratch)
    }

    /** Check if value at offset is zero. */
    def isZero(a: Array[Long], off: Int): Boolean = {
        a(off) == 0L && a(off + 1) == 0L && a(off + 2) == 0L && a(off + 3) == 0L
    }

    /** Copy 4 limbs from src to dst. */
    def copy(src: Array[Long], srcOff: Int, dst: Array[Long], dstOff: Int): Unit = {
        dst(dstOff) = src(srcOff)
        dst(dstOff + 1) = src(srcOff + 1)
        dst(dstOff + 2) = src(srcOff + 2)
        dst(dstOff + 3) = src(srcOff + 3)
    }

    /** Set 4 limbs to zero. */
    def setZero(a: Array[Long], off: Int): Unit = {
        a(off) = 0L; a(off + 1) = 0L; a(off + 2) = 0L; a(off + 3) = 0L
    }
}
