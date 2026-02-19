package scalus.crypto.accumulator

import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.Scalar

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

    def apply(coeffs: BigInt*): Poly =
        stripTrailingZeros(coeffs.toVector.map(n => Scalar.applyUnsafe(n.mod(Scalar.fieldPrime))))

    /** Build a Poly from BigInt coefficients (already reduced mod p). */
    def fromBigInts(coeffs: Vector[BigInt]): Poly =
        stripTrailingZeros(coeffs.map(n => Scalar.applyUnsafe(n.mod(Scalar.fieldPrime))))

    /** Compute product polynomial ∏(x + aᵢ) using a binary subproduct tree.
      *
      * O(n log²n) with NTT, vs O(n²) for iterative multiplication.
      */
    def productTree(elements: Vector[BigInt]): Poly = {
        if elements.isEmpty then return one
        var level: Vector[Poly] = elements.map(a => Poly(a, BigInt(1)))
        while level.length > 1 do
            level = level
                .grouped(2)
                .map {
                    case pair if pair.length == 2 => pair(0) * pair(1)
                    case single                   => single(0)
                }
                .toVector
        level.head
    }

    /** Compute product polynomial ∏(x + aᵢ).
      *
      * Uses subproduct tree for large inputs, iterative for small.
      */
    def product(elements: Vector[BigInt]): Poly = {
        if elements.length <= 32 then
            elements.foldLeft(one) { (acc, a) =>
                acc * Poly(a, BigInt(1))
            }
        else productTree(elements)
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
