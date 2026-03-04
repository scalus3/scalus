package scalus.crypto.accumulator

import scalus.cardano.onchain.plutus.prelude.bls12_381.Scalar

import java.math.BigInteger as JBigInt

/** Polynomial over the BLS12-381 scalar field (F_q) in ascending degree order [c₀, c₁, c₂, ...].
  *
  * Supports arithmetic operations (add, subtract, multiply, divide, extended GCD) needed for
  * bilinear accumulator proof generation. Multiplication automatically selects between naive O(n²)
  * and NTT-accelerated O(n log n) algorithms based on polynomial size.
  */
opaque type Poly = Vector[Scalar]

object Poly {

    /** Threshold for switching from naive to NTT multiplication. */
    private val NTT_THRESHOLD = 256

    val zero: Poly = Vector.empty
    val one: Poly = Vector(Scalar.one)

    /** Pre-computed java.math.BigInteger field prime for optimized internal operations. */
    private val jPrime: JBigInt = Scalar.fieldPrime.bigInteger
    private val jOne: JBigInt = JBigInt.ONE
    private val jZero: JBigInt = JBigInt.ZERO

    def apply(coeffs: BigInt*): Poly =
        stripTrailingZeros(coeffs.toVector.map(n => Scalar.applyUnsafe(n.mod(Scalar.fieldPrime))))

    /** Build a Poly from BigInt coefficients (already reduced mod p). */
    def fromBigInts(coeffs: Vector[BigInt]): Poly =
        stripTrailingZeros(coeffs.map(n => Scalar.applyUnsafe(n.mod(Scalar.fieldPrime))))

    /** Build a Poly from java.math.BigInteger array (elements must be in [0, prime)). */
    private def fromJBigIntArray(a: Array[JBigInt], len: Int): Poly = {
        // Strip trailing zeros and convert
        var last = len - 1
        while last >= 0 && a(last).signum() == 0 do last -= 1
        if last < 0 then Vector.empty
        else
            val builder = Vector.newBuilder[Scalar]
            builder.sizeHint(last + 1)
            var i = 0
            while i <= last do
                builder += Scalar.applyUnsafe(BigInt(a(i)))
                i += 1
            builder.result()
    }

    /** Compute product polynomial ∏(x + aᵢ) using a binary subproduct tree.
      *
      * O(n log²n) with NTT, vs O(n²) for iterative multiplication.
      *
      * Uses java.math.BigInteger arrays internally to avoid Scalar/BigInt wrapper overhead.
      */
    def productTree(elements: Vector[BigInt]): Poly = {
        if elements.isEmpty then return one
        val n = elements.length

        // Represent each polynomial as (Array[JBigInt], length).
        // Each linear polynomial (aᵢ + x) has coefficients [aᵢ, 1].
        var polys = new Array[Array[JBigInt]](n)
        var lengths = new Array[Int](n)
        var count = n

        var i = 0
        while i < n do
            val arr = new Array[JBigInt](2)
            arr(0) = elements(i).mod(Scalar.fieldPrime).bigInteger
            arr(1) = jOne
            polys(i) = arr
            lengths(i) = 2
            i += 1

        // Pairwise multiply up the tree
        while count > 1 do
            val newCount = (count + 1) / 2
            val newPolys = new Array[Array[JBigInt]](newCount)
            val newLengths = new Array[Int](newCount)

            i = 0
            var out = 0
            while i < count - 1 do
                val result = multiplyJ(polys(i), lengths(i), polys(i + 1), lengths(i + 1))
                newPolys(out) = result
                newLengths(out) = result.length
                i += 2
                out += 1

            // Odd element carried forward
            if i < count then
                newPolys(out) = polys(i)
                newLengths(out) = lengths(i)
                out += 1

            polys = newPolys
            lengths = newLengths
            count = newCount

        fromJBigIntArray(polys(0), lengths(0))
    }

    /** Compute product polynomial ∏(x + aᵢ).
      *
      * Uses subproduct tree for large inputs, iterative for small.
      */
    def product(elements: Vector[BigInt]): Poly = {
        if elements.length <= 32 then
            // For small inputs, iterative approach using optimized JBigInt path
            val n = elements.length
            // Start with polynomial [1]
            var poly = new Array[JBigInt](1)
            poly(0) = jOne
            var polyLen = 1

            var i = 0
            while i < n do
                val a = elements(i).mod(Scalar.fieldPrime).bigInteger
                // Multiply poly by (x + a): new[k] = poly[k-1] + a*poly[k]
                val newLen = polyLen + 1
                val newPoly = new Array[JBigInt](newLen)
                newPoly(0) = poly(0).multiply(a).mod(jPrime)
                var k = 1
                while k < polyLen do
                    newPoly(k) = poly(k - 1).add(poly(k).multiply(a)).mod(jPrime)
                    k += 1
                newPoly(polyLen) = poly(polyLen - 1)
                poly = newPoly
                polyLen = newLen
                i += 1

            fromJBigIntArray(poly, polyLen)
        else productTree(elements)
    }

    /** Naive polynomial multiplication using java.math.BigInteger.
      *
      * Accumulates products without intermediate mod reduction, then reduces once per coefficient.
      */
    private def naiveMultiplyJ(
        a: Array[JBigInt],
        aLen: Int,
        b: Array[JBigInt],
        bLen: Int
    ): Array[JBigInt] = {
        val resultLen = aLen + bLen - 1
        val result = new Array[JBigInt](resultLen)
        var k = 0
        while k < resultLen do
            result(k) = jZero
            k += 1

        var i = 0
        while i < aLen do
            val ai = a(i)
            var j = 0
            while j < bLen do
                result(i + j) = result(i + j).add(ai.multiply(b(j)))
                j += 1
            i += 1

        // Single mod reduction per coefficient
        i = 0
        while i < resultLen do
            result(i) = result(i).mod(jPrime)
            i += 1

        result
    }

    /** Internal multiply dispatching between naive and NTT based on polynomial size. */
    private def multiplyJ(
        a: Array[JBigInt],
        aLen: Int,
        b: Array[JBigInt],
        bLen: Int
    ): Array[JBigInt] = {
        if aLen < NTT_THRESHOLD || bLen < NTT_THRESHOLD then naiveMultiplyJ(a, aLen, b, bLen)
        else NTT.multiplyJ(a, aLen, b, bLen)
    }

    private def stripTrailingZeros(poly: Vector[Scalar]): Vector[Scalar] = {
        val idx = poly.lastIndexWhere(_ != Scalar.zero)
        if idx < 0 then Vector.empty else poly.take(idx + 1)
    }

    private def naiveMultiply(a: Poly, b: Poly): Poly = {
        val result = Array.fill(a.length + b.length - 1)(Scalar.zero)
        for
            i <- a.indices
            j <- b.indices
        do result(i + j) = result(i + j) + a(i) * b(j)
        stripTrailingZeros(result.toVector)
    }

    extension (a: Poly) {

        def coefficients: Vector[BigInt] = a.map(_.toInt)

        def degree: Int = a.length - 1

        def isZero: Boolean = a.isEmpty

        def +(b: Poly): Poly = {
            val len = math.max(a.length, b.length)
            val result = Vector.tabulate(len) { i =>
                val ai = if i < a.length then a(i) else Scalar.zero
                val bi = if i < b.length then b(i) else Scalar.zero
                ai + bi
            }
            stripTrailingZeros(result)
        }

        def -(b: Poly): Poly = {
            val len = math.max(a.length, b.length)
            val result = Vector.tabulate(len) { i =>
                val ai = if i < a.length then a(i) else Scalar.zero
                val bi = if i < b.length then b(i) else Scalar.zero
                ai - bi
            }
            stripTrailingZeros(result)
        }

        /** Multiply two polynomials. Auto-selects between naive and NTT based on size. */
        def *(b: Poly): Poly = {
            if a.isEmpty || b.isEmpty then return Vector.empty
            if a.length < NTT_THRESHOLD || b.length < NTT_THRESHOLD then naiveMultiply(a, b)
            else fromBigInts(NTT.multiply(a.coefficients, b.coefficients))
        }

        /** Polynomial long division returning (quotient, remainder). */
        def /%(b: Poly): (Poly, Poly) = {
            require(b.nonEmpty, "Division by zero polynomial")
            if a.length < b.length then return (zero, a)

            val bLeadInv = b.last.recip.get
            var remainder: Vector[Scalar] = a
            val quotDegree = a.length - b.length
            val quot = Array.fill(quotDegree + 1)(Scalar.zero)

            for i <- quotDegree to 0 by -1 do
                val coeff = remainder(i + b.length - 1) * bLeadInv
                quot(i) = coeff
                for j <- b.indices do
                    remainder = remainder.updated(i + j, remainder(i + j) - coeff * b(j))

            (stripTrailingZeros(quot.toVector), stripTrailingZeros(remainder))
        }

        def /(b: Poly): Poly = (a /% b)._1

        def %(b: Poly): Poly = (a /% b)._2

        /** Evaluate polynomial at x using Horner's method. */
        def eval(x: BigInt): BigInt = {
            if a.isEmpty then return BigInt(0)
            val sx = Scalar.applyUnsafe(x.mod(Scalar.fieldPrime))
            a.reverseIterator
                .foldLeft(Scalar.zero) { (acc, c) =>
                    acc * sx + c
                }
                .toInt
        }

        /** Extended Euclidean algorithm for polynomials.
          *
          * Returns (gcd, s, t) where s*a + t*b = gcd, with gcd monic.
          */
        def extGcd(b: Poly): (Poly, Poly, Poly) = {
            var oldR: Poly = a
            var r: Poly = b
            var oldS: Poly = one
            var s: Poly = zero
            var oldT: Poly = zero
            var t: Poly = one

            while r.nonEmpty do
                val (q, rem) = oldR /% r
                oldR = r
                r = rem
                val newS = oldS - q * s
                oldS = s
                s = newS
                val newT = oldT - q * t
                oldT = t
                t = newT

            // Normalize so gcd is monic (leading coefficient = 1)
            if oldR.nonEmpty then
                val leadInv = oldR.last.recip.get
                oldR = stripTrailingZeros(oldR.map(c => c * leadInv))
                oldS = stripTrailingZeros(oldS.map(c => c * leadInv))
                oldT = stripTrailingZeros(oldT.map(c => c * leadInv))

            (oldR, oldS, oldT)
        }
    }
}
