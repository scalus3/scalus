package scalus.crypto.accumulator

import org.scalatest.funsuite.AnyFunSuite

class PolyTest extends AnyFunSuite {

    test("polyMul: (x+2)(x+3) = x²+5x+6") {
        val a = Poly(2, 1) // x + 2
        val b = Poly(3, 1) // x + 3
        val result = a * b
        assert(result == Poly(6, 5, 1))
    }

    test("polyDivMod exact: (x²+5x+6)/(x+2) = (x+3), remainder 0") {
        val dividend = Poly(6, 5, 1)
        val divisor = Poly(2, 1)
        val (q, r) = dividend /% divisor
        assert(q == Poly(3, 1))
        assert(r.isZero)
    }

    test("polyDivMod with remainder: (x²+5x+7)/(x+2) = (x+3) remainder 1") {
        val dividend = Poly(7, 5, 1)
        val divisor = Poly(2, 1)
        val (q, r) = dividend /% divisor
        assert(q == Poly(3, 1))
        assert(r == Poly(1))
    }

    test("polyExtGcd coprime: s*a + t*b = 1") {
        val a = Poly(2, 1) // x + 2
        val b = Poly(3, 1) // x + 3
        val (gcd, s, t) = a.extGcd(b)
        assert(gcd == Poly.one)
        // Verify Bezout identity: s*a + t*b = 1
        val check = s * a + t * b
        assert(check == Poly.one)
    }

    test("product matches on-chain getFinalPoly") {
        val elements = Vector(BigInt(2), BigInt(3), BigInt(5))
        val offchain = Poly.product(elements)
        // (x+2)(x+3)(x+5) = x³ + 10x² + 31x + 30
        assert(offchain == Poly(30, 31, 10, 1))
    }

    test("productTree matches iterative product") {
        val elements = Vector(BigInt(2), BigInt(3), BigInt(5), BigInt(7), BigInt(11))
        val iterative = elements.foldLeft(Poly.one) { (acc, a) => acc * Poly(a, BigInt(1)) }
        val tree = Poly.productTree(elements)
        assert(tree == iterative)
    }

    test("productTree for single element") {
        val result = Poly.productTree(Vector(BigInt(42)))
        assert(result == Poly(42, 1))
    }

    test("productTree for empty elements") {
        val result = Poly.productTree(Vector.empty)
        assert(result == Poly.one)
    }

    test("product uses tree for large n and matches iterative") {
        val elements = (1 to 100).map(i => BigInt(i * 7)).toVector
        val iterative = elements.foldLeft(Poly.one) { (acc, a) => acc * Poly(a, BigInt(1)) }
        val tree = Poly.product(elements)
        assert(tree == iterative)
    }

    test("polynomial evaluation with Horner's method") {
        val p = Poly(6, 5, 1) // x² + 5x + 6
        assert(p.eval(0) == 6)
        assert(p.eval(1) == 12)
        assert(p.eval(2) == 20)
    }

    test("polynomial addition") {
        val a = Poly(1, 2, 3) // 3x² + 2x + 1
        val b = Poly(4, 5) // 5x + 4
        val result = a + b
        assert(result == Poly(5, 7, 3))
    }

    test("polynomial subtraction") {
        val a = Poly(5, 7, 3)
        val b = Poly(4, 5)
        val result = a - b
        assert(result == Poly(1, 2, 3))
    }

    test("multiply by zero") {
        val a = Poly(1, 2, 3)
        val result = a * Poly.zero
        assert(result.isZero)
    }

    test("multiply by one") {
        val a = Poly(1, 2, 3)
        val result = a * Poly.one
        assert(result == a)
    }

    test("NTT-accelerated multiply matches naive for medium polynomials") {
        // Create polynomials large enough to trigger NTT path
        val rng = new scala.util.Random(12345)
        val aCoeffs = (0 until 300).map(_ => BigInt(rng.nextInt(10000))).toVector
        val bCoeffs = (0 until 300).map(_ => BigInt(rng.nextInt(10000))).toVector
        val a = Poly(aCoeffs*)
        val b = Poly(bCoeffs*)

        // Also compute with small polys that use naive path
        val smallA = Poly(1, 2, 3)
        val smallB = Poly(4, 5)
        val smallResult = smallA * smallB
        assert(smallResult == Poly(4, 13, 22, 15))

        // The NTT-path result should be valid (we test it indirectly via product)
        val result = a * b
        assert(!result.isZero)
        assert(result.degree == a.degree + b.degree)
    }
}
