package scalus.examples.bilinearAccumulator

import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.Scalar

/** Polynomial over the BLS12-381 scalar field in ascending degree order [c₀, c₁, c₂, ...].
  *
  * All arithmetic is performed modulo the scalar field prime.
  */
opaque type Poly = Vector[BigInt]

object Poly {

    private val p: BigInt = Scalar.fieldPrime

    val zero: Poly = Vector.empty
    val one: Poly = Vector(BigInt(1))

    def apply(coeffs: BigInt*): Poly = normalize(coeffs.toVector)

    /** Compute product polynomial ∏(x + aᵢ) — matches on-chain getFinalPoly. */
    def product(elements: Vector[BigInt]): Poly = {
        elements.foldLeft(one) { (acc, a) =>
            acc * Poly(a.mod(p), BigInt(1))
        }
    }

    private def stripTrailingZeros(poly: Vector[BigInt]): Vector[BigInt] = {
        val idx = poly.lastIndexWhere(_ != BigInt(0))
        if idx < 0 then Vector.empty else poly.take(idx + 1)
    }

    private def normalize(poly: Vector[BigInt]): Poly =
        stripTrailingZeros(poly.map(_.mod(p)))

    extension (a: Poly) {

        def coefficients: Vector[BigInt] = a

        def degree: Int = a.length - 1

        def isZero: Boolean = a.isEmpty

        def +(b: Poly): Poly = {
            val len = math.max(a.length, b.length)
            val result = Vector.tabulate(len) { i =>
                val ai = if i < a.length then a(i) else BigInt(0)
                val bi = if i < b.length then b(i) else BigInt(0)
                (ai + bi).mod(p)
            }
            stripTrailingZeros(result)
        }

        def -(b: Poly): Poly = {
            val len = math.max(a.length, b.length)
            val result = Vector.tabulate(len) { i =>
                val ai = if i < a.length then a(i) else BigInt(0)
                val bi = if i < b.length then b(i) else BigInt(0)
                (ai - bi).mod(p)
            }
            stripTrailingZeros(result)
        }

        def *(b: Poly): Poly = {
            if a.isEmpty || b.isEmpty then return Vector.empty
            val result = Array.fill(a.length + b.length - 1)(BigInt(0))
            for
                i <- a.indices
                j <- b.indices
            do result(i + j) = (result(i + j) + a(i) * b(j)).mod(p)
            stripTrailingZeros(result.toVector)
        }

        /** Polynomial long division returning (quotient, remainder). */
        def /%(b: Poly): (Poly, Poly) = {
            require(b.nonEmpty, "Division by zero polynomial")
            if a.length < b.length then return (zero, normalize(a))

            val bLeadInv = b.last.modPow(p - 2, p)
            var remainder = a.map(_.mod(p))
            val quotDegree = a.length - b.length
            val quot = Array.fill(quotDegree + 1)(BigInt(0))

            for i <- quotDegree to 0 by -1 do
                val coeff = (remainder(i + b.length - 1) * bLeadInv).mod(p)
                quot(i) = coeff
                for j <- b.indices do
                    remainder = remainder.updated(i + j, (remainder(i + j) - coeff * b(j)).mod(p))

            (stripTrailingZeros(quot.toVector), stripTrailingZeros(remainder))
        }

        def /(b: Poly): Poly = (a /% b)._1

        def %(b: Poly): Poly = (a /% b)._2

        /** Evaluate polynomial at x using Horner's method. */
        def eval(x: BigInt): BigInt = {
            if a.isEmpty then return BigInt(0)
            a.reverseIterator.foldLeft(BigInt(0)) { (acc, c) =>
                (acc * x + c).mod(p)
            }
        }

        /** Extended Euclidean algorithm for polynomials. Returns (gcd, s, t) where s*a + t*b = gcd,
          * with gcd monic.
          */
        def extGcd(b: Poly): (Poly, Poly, Poly) = {
            var oldR = normalize(a)
            var r = normalize(b)
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
                val leadInv = oldR.last.modPow(p - 2, p)
                oldR = normalize(oldR.map(c => (c * leadInv).mod(p)))
                oldS = normalize(oldS.map(c => (c * leadInv).mod(p)))
                oldT = normalize(oldT.map(c => (c * leadInv).mod(p)))

            (oldR, oldS, oldT)
        }
    }
}
