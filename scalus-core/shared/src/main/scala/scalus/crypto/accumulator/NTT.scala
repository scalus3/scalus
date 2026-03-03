package scalus.crypto.accumulator

import scalus.cardano.onchain.plutus.prelude.bls12_381.Scalar

import java.math.BigInteger as JBigInt

/** Number Theoretic Transform over a prime field F_p.
  *
  * The prime p must have sufficient two-adicity for the desired NTT size: p - 1 must be divisible
  * by 2^k where 2^k >= n (the transform size). This ensures primitive n-th roots of unity exist in
  * F_p.
  *
  * Default configuration uses the BLS12-381 scalar field (two-adicity 32, supporting NTTs up to
  * 2^32 ≈ 4.3 billion elements).
  */
object NTT {

    /** BLS12-381 scalar field prime. */
    val defaultPrime: BigInt = Scalar.fieldPrime

    /** Primitive 2^32-th root of unity for BLS12-381 scalar field: 7^t mod q where t = (q-1)/2^32.
      */
    val defaultRootOfUnity: BigInt =
        BigInt("16a2a19edfe81f20d09b681922c813b4b63683508c2280b93829971f439f0d2b", 16)

    /** Two-adicity of BLS12-381 scalar field: q - 1 = 2^32 · t, max NTT size = 2^32. */
    val defaultTwoAdicity: Int = 32

    // Pre-computed java.math.BigInteger constants for the hot path
    private val jPrime: JBigInt = defaultPrime.bigInteger
    private val jRootOfUnity: JBigInt = defaultRootOfUnity.bigInteger
    private val jTwo: JBigInt = JBigInt.valueOf(2)

    /** Get primitive n-th root of unity (n must be power of 2).
      *
      * Computes omega_n = maxRootOfUnity ^ (2^(twoAdicity - log2(n))) mod prime.
      *
      * @param n
      *   NTT size, must be a power of 2 and <= 2^twoAdicity
      * @param maxRootOfUnity
      *   primitive 2^twoAdicity-th root of unity
      * @param twoAdicity
      *   the two-adicity of the field (p-1 = 2^s · t)
      * @param prime
      *   the field prime
      */
    def principalRoot(
        n: Int,
        maxRootOfUnity: BigInt = defaultRootOfUnity,
        twoAdicity: Int = defaultTwoAdicity,
        prime: BigInt = defaultPrime
    ): BigInt = {
        require(n > 0 && (n & (n - 1)) == 0, s"NTT size must be a power of 2, got $n")
        val logN = Integer.numberOfTrailingZeros(n)
        require(logN <= twoAdicity, s"NTT size 2^$logN exceeds two-adicity $twoAdicity")
        val exp = BigInt(1) << (twoAdicity - logN)
        maxRootOfUnity.modPow(exp, prime)
    }

    private def principalRootJ(n: Int): JBigInt = {
        val logN = Integer.numberOfTrailingZeros(n)
        val exp = JBigInt.ONE.shiftLeft(defaultTwoAdicity - logN)
        jRootOfUnity.modPow(exp, jPrime)
    }

    private def bitReverse(i: Int, logN: Int): Int = {
        var result = 0
        var x = i
        var k = logN
        while k > 0 do
            result = (result << 1) | (x & 1)
            x >>= 1
            k -= 1
        result
    }

    /** Forward NTT (Cooley-Tukey DIT butterfly), in-place.
      *
      * Transforms polynomial coefficients to evaluations at powers of omega.
      *
      * @param a
      *   array of coefficients, length must equal n
      * @param n
      *   transform size, must be a power of 2
      * @param omega
      *   primitive n-th root of unity
      * @param prime
      *   the field prime
      */
    def forward(a: Array[BigInt], n: Int, omega: BigInt, prime: BigInt = defaultPrime): Unit = {
        val logN = Integer.numberOfTrailingZeros(n)
        // Bit-reversal permutation
        var i = 0
        while i < n do
            val j = bitReverse(i, logN)
            if i < j then
                val tmp = a(i)
                a(i) = a(j)
                a(j) = tmp
            i += 1

        // Cooley-Tukey DIT butterflies
        var halfSize = 1
        while halfSize < n do
            val size = halfSize << 1
            val step = omega.modPow(n / size, prime)
            var k = 0
            while k < n do
                var w = BigInt(1)
                var j = 0
                while j < halfSize do
                    val u = a(k + j)
                    val v = a(k + j + halfSize) * w % prime
                    a(k + j) = (u + v) % prime
                    a(k + j + halfSize) = (u - v + prime) % prime
                    w = w * step % prime
                    j += 1
                k += size
            halfSize = size
    }

    /** Forward NTT using java.math.BigInteger for reduced allocation overhead. */
    private[accumulator] def forwardJ(
        a: Array[JBigInt],
        n: Int,
        omega: JBigInt,
        prime: JBigInt
    ): Unit = {
        val logN = Integer.numberOfTrailingZeros(n)
        // Bit-reversal permutation
        var i = 0
        while i < n do
            val j = bitReverse(i, logN)
            if i < j then
                val tmp = a(i)
                a(i) = a(j)
                a(j) = tmp
            i += 1

        // Cooley-Tukey DIT butterflies with pre-computed twiddle factors.
        // Use conditional add/subtract instead of mod where possible:
        // Since u, v are in [0, prime), u+v is in [0, 2*prime) — subtract prime if >= prime.
        // u-v is in (-prime, prime) — add prime if negative.
        var halfSize = 1
        while halfSize < n do
            val size = halfSize << 1
            val step = omega.modPow(JBigInt.valueOf(n / size), prime)
            // Pre-compute twiddle factors: twiddles(j) = step^j mod prime
            val twiddles = new Array[JBigInt](halfSize)
            twiddles(0) = JBigInt.ONE
            var t = 1
            while t < halfSize do
                twiddles(t) = twiddles(t - 1).multiply(step).mod(prime)
                t += 1
            var k = 0
            while k < n do
                var j = 0
                while j < halfSize do
                    val u = a(k + j)
                    val v = a(k + j + halfSize).multiply(twiddles(j)).mod(prime)
                    val sum = u.add(v)
                    a(k + j) = if sum.compareTo(prime) >= 0 then sum.subtract(prime) else sum
                    val diff = u.subtract(v)
                    a(k + j + halfSize) = if diff.signum() < 0 then diff.add(prime) else diff
                    j += 1
                k += size
            halfSize = size
    }

    /** Inverse NTT, in-place, scales by n^{-1}.
      *
      * Transforms evaluations back to polynomial coefficients.
      *
      * @param a
      *   array of evaluations, length must equal n
      * @param n
      *   transform size, must be a power of 2
      * @param omega
      *   primitive n-th root of unity (same as used in forward)
      * @param prime
      *   the field prime
      */
    def inverse(a: Array[BigInt], n: Int, omega: BigInt, prime: BigInt = defaultPrime): Unit = {
        val omegaInv = omega.modPow(prime - 2, prime)
        forward(a, n, omegaInv, prime)
        val nInv = BigInt(n).modPow(prime - 2, prime)
        var i = 0
        while i < n do
            a(i) = a(i) * nInv % prime
            i += 1
    }

    /** Inverse NTT using java.math.BigInteger. */
    private[accumulator] def inverseJ(
        a: Array[JBigInt],
        n: Int,
        omega: JBigInt,
        prime: JBigInt
    ): Unit = {
        val omegaInv = omega.modPow(prime.subtract(jTwo), prime)
        forwardJ(a, n, omegaInv, prime)
        val nInv = JBigInt.valueOf(n).modPow(prime.subtract(jTwo), prime)
        var i = 0
        while i < n do
            a(i) = a(i).multiply(nInv).mod(prime)
            i += 1
    }

    /** Multiply two polynomials via NTT: O(n log n).
      *
      * Coefficients are in ascending degree order. Uses BLS12-381 scalar field by default.
      *
      * @param a
      *   first polynomial coefficients
      * @param b
      *   second polynomial coefficients
      * @param prime
      *   the field prime
      * @param maxRootOfUnity
      *   primitive 2^twoAdicity-th root of unity
      * @param twoAdicity
      *   two-adicity of the field
      * @return
      *   product polynomial coefficients
      */
    def multiply(
        a: Vector[BigInt],
        b: Vector[BigInt],
        prime: BigInt = defaultPrime,
        maxRootOfUnity: BigInt = defaultRootOfUnity,
        twoAdicity: Int = defaultTwoAdicity
    ): Vector[BigInt] = {
        if a.isEmpty || b.isEmpty then return Vector.empty
        val resultLen = a.length + b.length - 1
        var n = 1
        while n < resultLen do n <<= 1

        val omega = principalRoot(n, maxRootOfUnity, twoAdicity, prime)

        val fa = Array.tabulate(n)(i => if i < a.length then a(i).mod(prime) else BigInt(0))
        val fb = Array.tabulate(n)(i => if i < b.length then b(i).mod(prime) else BigInt(0))

        forward(fa, n, omega, prime)
        forward(fb, n, omega, prime)

        var i = 0
        while i < n do
            fa(i) = fa(i) * fb(i) % prime
            i += 1

        inverse(fa, n, omega, prime)

        fa.take(resultLen).toVector
    }

    /** Multiply two polynomials via NTT using java.math.BigInteger arrays.
      *
      * Optimized for the BLS12-381 scalar field. Uses allocation-free 256-bit Montgomery arithmetic
      * (MontField256) with flat Long arrays to eliminate BigInteger/int[] GC pressure in the NTT
      * hot path.
      *
      * @param a
      *   first polynomial coefficients (elements must be in [0, prime))
      * @param aLen
      *   number of valid elements in a
      * @param b
      *   second polynomial coefficients (elements must be in [0, prime))
      * @param bLen
      *   number of valid elements in b
      * @return
      *   product polynomial coefficients as a new array, trimmed to result length
      */
    private[accumulator] def multiplyJ(
        a: Array[JBigInt],
        aLen: Int,
        b: Array[JBigInt],
        bLen: Int
    ): Array[JBigInt] = {
        if aLen == 0 || bLen == 0 then return Array.empty
        val resultLen = aLen + bLen - 1
        var n = 1
        while n < resultLen do n <<= 1

        val omega = principalRootJ(n)
        val L = MontField256.LIMBS // 4
        val scratch = new Array[Long](6) // CIOS accumulator, reused throughout

        // Convert inputs to flat Long arrays in Montgomery form
        val fa = new Array[Long](n * L)
        val fb = new Array[Long](n * L)
        val tmp = new Array[Long](L)
        var i = 0
        while i < aLen do
            MontField256.fromBigInteger(a(i), tmp, 0)
            MontField256.toMont(tmp, 0, fa, i * L, scratch)
            i += 1
        // Remaining elements are zero (already initialized to 0L)
        i = 0
        while i < bLen do
            MontField256.fromBigInteger(b(i), tmp, 0)
            MontField256.toMont(tmp, 0, fb, i * L, scratch)
            i += 1

        forwardMont256(fa, n, omega, scratch)
        forwardMont256(fb, n, omega, scratch)

        // Pointwise multiply in Montgomery domain
        val scratchV = new Array[Long](L)
        i = 0
        while i < n do
            MontField256.montMul(fa, i * L, fb, i * L, scratchV, 0, scratch)
            MontField256.copy(scratchV, 0, fa, i * L)
            i += 1

        inverseMont256(fa, n, omega, scratch)

        // Convert back from Montgomery form to BigInteger
        val result = new Array[JBigInt](resultLen)
        i = 0
        while i < resultLen do
            MontField256.fromMont(fa, i * L, tmp, 0, scratch)
            result(i) = MontField256.toBigInteger(tmp, 0)
            i += 1

        result
    }

    /** Forward NTT on flat Long array in Montgomery domain.
      *
      * Data is stored as n elements of 4 longs each (little-endian limbs) in Montgomery form. omega
      * is in normal (non-Montgomery) form.
      */
    private def forwardMont256(
        data: Array[Long],
        n: Int,
        omega: JBigInt,
        scratch: Array[Long]
    ): Unit = {
        val L = MontField256.LIMBS
        val logN = Integer.numberOfTrailingZeros(n)

        // Bit-reversal permutation
        val swapTmp = new Array[Long](L)
        var i = 0
        while i < n do
            val j = bitReverse(i, logN)
            if i < j then
                val iOff = i * L
                val jOff = j * L
                MontField256.copy(data, iOff, swapTmp, 0)
                MontField256.copy(data, jOff, data, iOff)
                MontField256.copy(swapTmp, 0, data, jOff)
            i += 1

        // Cooley-Tukey DIT butterflies with allocation-free Montgomery multiplication
        val scratchV = new Array[Long](L)
        val stepLimbs = new Array[Long](L)
        val stepMont = new Array[Long](L)
        var halfSize = 1
        while halfSize < n do
            val size = halfSize << 1
            val stepNormal = omega.modPow(JBigInt.valueOf(n / size), jPrime)
            MontField256.fromBigInteger(stepNormal, stepLimbs, 0)
            MontField256.toMont(stepLimbs, 0, stepMont, 0, scratch)

            // Pre-compute twiddle factors in Montgomery form
            val twiddles = new Array[Long](halfSize * L)
            MontField256.copy(MontField256.ONE_MONT, 0, twiddles, 0)
            var t = 1
            while t < halfSize do
                MontField256.montMul(twiddles, (t - 1) * L, stepMont, 0, twiddles, t * L, scratch)
                t += 1

            var k = 0
            while k < n do
                var j = 0
                while j < halfSize do
                    val evenOff = (k + j) * L
                    val oddOff = (k + j + halfSize) * L
                    val twOff = j * L

                    // v = montMul(data[odd], twiddle[j])
                    MontField256.montMul(data, oddOff, twiddles, twOff, scratchV, 0, scratch)
                    // data[odd] = u - v (compute before overwriting u at evenOff)
                    MontField256.subMod(data, evenOff, scratchV, 0, data, oddOff)
                    // data[even] = u + v (safe: evenOff still has original u)
                    MontField256.addMod(data, evenOff, scratchV, 0, data, evenOff)
                    j += 1
                k += size
            halfSize = size
    }

    /** Inverse NTT on flat Long array in Montgomery domain. */
    private def inverseMont256(
        data: Array[Long],
        n: Int,
        omega: JBigInt,
        scratch: Array[Long]
    ): Unit = {
        val L = MontField256.LIMBS
        val omegaInv = omega.modPow(jPrime.subtract(jTwo), jPrime)
        forwardMont256(data, n, omegaInv, scratch)
        val nInvNormal = JBigInt.valueOf(n).modPow(jPrime.subtract(jTwo), jPrime)
        val nInvLimbs = new Array[Long](L)
        val nInvMont = new Array[Long](L)
        MontField256.fromBigInteger(nInvNormal, nInvLimbs, 0)
        MontField256.toMont(nInvLimbs, 0, nInvMont, 0, scratch)
        val tmp = new Array[Long](L)
        var i = 0
        while i < n do
            MontField256.montMul(data, i * L, nInvMont, 0, tmp, 0, scratch)
            MontField256.copy(tmp, 0, data, i * L)
            i += 1
    }
}
