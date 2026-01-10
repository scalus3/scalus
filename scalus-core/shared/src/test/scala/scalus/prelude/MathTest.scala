package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.prelude.Math.*
import scalus.prelude.Option.None
import scalus.prelude.Option.Some
import scalus.testing.kit.EvalTestKit

class MathTest extends AnyFunSuite with EvalTestKit:

    test("abs"):
        checkEval: (x: BigInt) =>
            x.absolute >= 0 &&
                (x >= 0 && x.absolute == x ||
                    x.absolute == -x)

        BigInt(0).absolute.evalEq(391986, 1802):
            BigInt(0)
        BigInt(5).absolute.evalEq(439986, 2102):
            BigInt(5)
        BigInt(-7).absolute.evalEq(557194, 2204):
            BigInt(7)

    test("min"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = min(x, y)
            (m <= x && m <= y) &&
            (m == x || m == y)

        min(BigInt(1), BigInt(2)).evalEq(439986, 2102):
            BigInt(1)
        min(BigInt(-1), BigInt(-5)).evalEq(439986, 2102):
            BigInt(-5)

    test("max"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = max(x, y)
            (m >= x && m >= y) &&
            (m == x || m == y)

        max(BigInt(1), BigInt(2)).evalEq(439986, 2102):
            BigInt(2)
        max(BigInt(-1), BigInt(-5)).evalEq(439986, 2102):
            BigInt(-1)

    test("clamp"):
        checkEval: (self: BigInt, min: BigInt, max: BigInt) =>
            val c = self.clamp(min, max)
            min > max || c >= min && c <= max

        BigInt(42)
            .clamp(13, 37)
            .evalEq(1010778, 4904):
                BigInt(37)
        BigInt(3)
            .clamp(13, 17)
            .evalEq(1010778, 4904):
                BigInt(13)
        BigInt(17)
            .clamp(15, 25)
            .evalEq(1010778, 4904):
                BigInt(17)
        BigInt(7)
            .clamp(5, 7)
            .evalEq(1010778, 4904):
                BigInt(7)

    test("gcd"):
        checkEval: (x: BigInt, y: BigInt) =>
            val g = gcd(x, y)
            g >= BigInt(0) &&
            (x == BigInt(0) || (x % g) == BigInt(0)) &&
            (y == BigInt(0) || (y % g) == BigInt(0))

        gcd(BigInt(0), BigInt(0)).evalEq(1208368, 6104):
            BigInt(0)
        gcd(BigInt(12), BigInt(18)).evalEq(3525304, 15713):
            BigInt(6)
        gcd(BigInt(-12), BigInt(18)).evalEq(3525304, 15713):
            BigInt(6)

    test("sqrt"):
        checkEval: (x: BigInt) =>
            sqrt(x) match
                case None => x < 0
                case Some(s) =>
                    s * s <= x && (s + 1) * (s + 1) > x &&
                    x.isSqrt(s)

        BigInt(0).sqRoot.evalEq(1382380, 6632):
            Some(BigInt(0))
        BigInt(1).sqRoot.evalEq(1382380, 6632):
            Some(BigInt(1))
        BigInt(17).sqRoot.evalEq(5734266, 22555):
            Some(BigInt(4))
        BigInt(-1).sqRoot.evalEq(1382380, 6632):
            None
        assertEval(17.isSqrt(4))

    test("pow"):
        pow(BigInt(0), BigInt(0)).evalEq(1209821, 6104):
            BigInt(1)
        pow(BigInt(2), BigInt(0)).evalEq(1209821, 6104):
            BigInt(1)
        pow(BigInt(2), BigInt(3)).evalEq(4865297, 18928):
            BigInt(8)
        pow(BigInt(-2), BigInt(3)).evalEq(4865297, 18928):
            BigInt(-8)
        pow(BigInt(7), BigInt(2)).evalEq(4545136, 18124):
            BigInt(49)
        pow(BigInt(513), BigInt(3)).evalEq(4865297, 18928):
            BigInt(135005697)
        pow(BigInt(2), BigInt(42)).evalEq(11215766, 42164):
            BigInt("4398046511104")

    test("exp2"):
        exp2(BigInt(-2)).evalEq(441439, 2102):
            BigInt(0)
        exp2(BigInt(0)).evalEq(3758628, 5309):
            BigInt(1)
        exp2(BigInt(1)).evalEq(3758628, 5309):
            BigInt(2)
        exp2(BigInt(4)).evalEq(3758628, 5309):
            BigInt(16)
        exp2(BigInt(42)).evalEq(3758628, 5309):
            BigInt("4398046511104")
        exp2(BigInt(256)).evalEq(3939663, 5319):
            BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639936")

    test("log2"):
        log2(BigInt(1)).evalEq(2879362, 6923):
            BigInt(0)
        log2(BigInt(2)).evalEq(3208701, 8225):
            BigInt(1)
        log2(BigInt(3)).evalEq(3208701, 8225):
            BigInt(1)
        log2(BigInt(4)).evalEq(3538040, 9527):
            BigInt(2)
        log2(BigInt(256)).evalEq(2879362, 6923):
            BigInt(8)
        log2(BigInt(257)).evalEq(2879362, 6923):
            BigInt(8)
        log2(BigInt(511)).evalEq(2879362, 6923):
            BigInt(8)
        log2(BigInt(1025)).evalEq(3538040, 9527):
            BigInt(10)

    test("log"):
        log(BigInt(10), base = BigInt(2)).evalEq(3944756, 16619):
            BigInt(3)
        log(BigInt(42), base = BigInt(2)).evalEq(5805710, 23829):
            BigInt(5)
        log(BigInt(42), base = BigInt(3)).evalEq(3944756, 16619):
            BigInt(3)
        log(BigInt(5), base = BigInt(0)).evalEq(535986, 2702):
            BigInt(0)
        log(BigInt(4), base = BigInt(4)).evalEq(2083802, 9409):
            BigInt(1)
        log(BigInt(4), base = BigInt(42)).evalEq(1153325, 5804):
            BigInt(0)
