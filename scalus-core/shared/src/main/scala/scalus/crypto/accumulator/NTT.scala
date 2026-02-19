package scalus.crypto.accumulator

import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.Scalar

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
}
