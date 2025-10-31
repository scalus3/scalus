package scalus.prelude

import scalus.prelude.Math.*
import scalus.prelude.Option.{None, Some}

class MathTest extends StdlibTestKit:

    test("abs"):
        checkEval: (x: BigInt) =>
            x.absolute >= 0 &&
            (x >= 0 && x.absolute == x ||
            x.absolute == -x)

        BigInt(0).absolute assertEvalEq BigInt(0)
        BigInt(5).absolute assertEvalEq BigInt(5)
        BigInt(-7).absolute assertEvalEq BigInt(7)


    test("min"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = min(x, y)
            (m <= x && m <= y) &&
            (m == x || m == y)

        min(BigInt(1), BigInt(2)) assertEvalEq BigInt(1)
        min(BigInt(-1), BigInt(-5)) assertEvalEq BigInt(-5)

    test("max"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = max(x, y)
            (m >= x && m >= y) &&
            (m == x || m == y)

        max(BigInt(1), BigInt(2)) assertEvalEq BigInt(2)
        max(BigInt(-1), BigInt(-5)) assertEvalEq BigInt(-1)

    test("clamp"):
        checkEval: (self: BigInt, min: BigInt, max: BigInt) =>
            val c = self.clamp(min, max)
            min > max || c >= min && c <= max

        BigInt(42).clamp(13, 37) assertEvalEq BigInt(37)
        BigInt(3).clamp(13, 17) assertEvalEq BigInt(13)
        BigInt(17).clamp(15, 25) assertEvalEq BigInt(17)
        BigInt(7).clamp(5, 7) assertEvalEq BigInt(7)

    test("gcd"):
        checkEval: (x: BigInt, y: BigInt) =>
            val g = gcd(x, y)
            g >= BigInt(0) &&
            (x == BigInt(0) || (x % g) == BigInt(0)) &&
            (y == BigInt(0) || (y % g) == BigInt(0))

        gcd(BigInt(0), BigInt(0)) assertEvalEq BigInt(0)
        gcd(BigInt(12), BigInt(18)) assertEvalEq BigInt(6)
        gcd(BigInt(-12), BigInt(18)) assertEvalEq BigInt(6)

    test("sqrt"):
        checkEval: (x: BigInt) =>
            sqrt(x) match
                case None => x < 0
                case Some(s) =>
                  s * s <= x && (s + 1) * (s + 1) > x &&
                  x.isSqrt(s)

        BigInt(0).sqRoot assertEvalEq Some(BigInt(0))
        BigInt(1).sqRoot assertEvalEq Some(BigInt(1))
        BigInt(17).sqRoot assertEvalEq Some(BigInt(4))
        BigInt(-1).sqRoot assertEvalEq None
        assertEval(17.isSqrt(4))

    test("pow"):
        checkEval: (base: BigInt, exp: BigInt) =>
            val p = pow(base, exp)
            exp < BigInt(0) && p == BigInt(0) ||
                (p >= BigInt(0) || base != BigInt(0)) &&
                (exp > BigInt(0) || p == BigInt(1))

        pow(BigInt(2), BigInt(0)) assertEvalEq BigInt(1)
        pow(BigInt(2), BigInt(3)) assertEvalEq BigInt(8)
        pow(BigInt(-2), BigInt(3)) assertEvalEq BigInt(-8)
        pow(BigInt(513), BigInt(3)) assertEvalEq BigInt(135005697)

    /*

    test("factorial") {
        check { (n: Int) =>
            whenever(n >= 0 && n <= 20) { // prevent BigInt overflow
                                          val f = factorial(n)
                assert(f >= 1)
            }
        }

        assert(factorial(0) == BigInt(1))
        assert(factorial(1) == BigInt(1))
        assert(factorial(5) == BigInt(120))
    }

    test("lcm") {
        check { (a: BigInt, b: BigInt) =>
            val l = lcm(a, b)
            if (a != 0 && b != 0) {
                assert(l % a == 0)
                assert(l % b == 0)
            } else {
                assert(l == BigInt(0))
            }
        }

        assert(lcm(BigInt(0), BigInt(5)) == BigInt(0))
        assert(lcm(BigInt(4), BigInt(6)) == BigInt(12))
    }

    test("signum") {
        check { (n: BigInt) =>
            val s = signum(n)
      assert(s == -1 || s == 0 || s == 1)
            if (n > 0) assert(s == 1)
            else if (n < 0) assert(s == -1)
            else assert(s == 0)
        }

        assert(signum(BigInt(0)) == 0)
        assert(signum(BigInt(10)) == 1)
        assert(signum(BigInt(-10)) == -1)
    }
*/
