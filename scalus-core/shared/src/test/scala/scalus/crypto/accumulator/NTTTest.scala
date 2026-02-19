package scalus.crypto.accumulator

import org.scalatest.funsuite.AnyFunSuite

class NTTTest extends AnyFunSuite {

    val prime: BigInt = NTT.defaultPrime

    test("principalRoot returns correct n-th root of unity") {
        val omega4 = NTT.principalRoot(4)
        // omega4^4 = 1 mod p
        assert(omega4.modPow(4, prime) == BigInt(1))
        // omega4^2 != 1 mod p (primitive)
        assert(omega4.modPow(2, prime) != BigInt(1))
    }

    test("principalRoot for n=2") {
        val omega2 = NTT.principalRoot(2)
        assert(omega2.modPow(2, prime) == BigInt(1))
        assert(omega2 != BigInt(1))
    }

    test("principalRoot for n=1024") {
        val omega = NTT.principalRoot(1024)
        assert(omega.modPow(1024, prime) == BigInt(1))
        assert(omega.modPow(512, prime) != BigInt(1))
    }

    test("forward + inverse NTT is identity") {
        val n = 8
        val omega = NTT.principalRoot(n)
        val original = Array(
          BigInt(1),
          BigInt(2),
          BigInt(3),
          BigInt(4),
          BigInt(5),
          BigInt(6),
          BigInt(7),
          BigInt(8)
        )
        val data = original.clone()

        NTT.forward(data, n, omega)
        NTT.inverse(data, n, omega)

        for i <- 0 until n do assert(data(i) == original(i), s"Mismatch at index $i")
    }

    test("forward + inverse NTT is identity for large array") {
        val n = 256
        val omega = NTT.principalRoot(n)
        val original = Array.tabulate(n)(i => BigInt(i * 17 + 3) % prime)
        val data = original.clone()

        NTT.forward(data, n, omega)
        NTT.inverse(data, n, omega)

        for i <- 0 until n do assert(data(i) == original(i), s"Mismatch at index $i")
    }

    test("NTT multiply matches naive multiply for small polynomials") {
        val a = Vector(BigInt(1), BigInt(2), BigInt(3)) // 1 + 2x + 3x²
        val b = Vector(BigInt(4), BigInt(5)) // 4 + 5x
        // (1 + 2x + 3x²)(4 + 5x) = 4 + 13x + 22x² + 15x³

        val result = NTT.multiply(a, b)
        assert(result == Vector(BigInt(4), BigInt(13), BigInt(22), BigInt(15)))
    }

    test("NTT multiply matches naive multiply for degree-1 polynomials") {
        val a = Vector(BigInt(2), BigInt(1)) // x + 2
        val b = Vector(BigInt(3), BigInt(1)) // x + 3
        // (x+2)(x+3) = x² + 5x + 6

        val result = NTT.multiply(a, b)
        assert(result == Vector(BigInt(6), BigInt(5), BigInt(1)))
    }

    test("NTT multiply with zero polynomial") {
        val a = Vector(BigInt(1), BigInt(2))
        val result = NTT.multiply(a, Vector.empty)
        assert(result.isEmpty)
    }

    test("NTT multiply with constant polynomial") {
        val a = Vector(BigInt(1), BigInt(2), BigInt(3))
        val b = Vector(BigInt(7))
        val result = NTT.multiply(a, b)
        assert(result == Vector(BigInt(7), BigInt(14), BigInt(21)))
    }

    test("NTT multiply large polynomials matches naive") {
        val rng = new scala.util.Random(42)
        val size = 100
        val a = Vector.fill(size)(BigInt(rng.nextInt(10000)))
        val b = Vector.fill(size)(BigInt(rng.nextInt(10000)))

        val nttResult = NTT.multiply(a, b)

        // Naive multiplication
        val naive = Array.fill(a.length + b.length - 1)(BigInt(0))
        for
            i <- a.indices
            j <- b.indices
        do naive(i + j) = (naive(i + j) + a(i) * b(j)) % prime
        val naiveResult = naive.toVector

        assert(nttResult.length == naiveResult.length)
        for i <- nttResult.indices do
            assert(nttResult(i) == naiveResult(i), s"Mismatch at index $i")
    }

    test("NTT multiply with negative (field) coefficients") {
        // Test that modular arithmetic works correctly for coefficients near the prime
        val a = Vector(prime - 1, BigInt(1)) // -1 + x in the field
        val b = Vector(prime - 1, BigInt(1)) // -1 + x
        // (x - 1)² = x² - 2x + 1

        val result = NTT.multiply(a, b)
        assert(result == Vector(BigInt(1), prime - 2, BigInt(1)))
    }
}
