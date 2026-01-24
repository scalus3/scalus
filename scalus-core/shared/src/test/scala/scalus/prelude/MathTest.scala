package scalus.prelude

import scalus.cardano.onchain.plutus.prelude.`???`

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.prelude.Math.*
import scalus.cardano.onchain.plutus.prelude.Option.None
import scalus.cardano.onchain.plutus.prelude.Option.Some
import scalus.testing.kit.EvalTestKit

class MathTest extends AnyFunSuite with EvalTestKit:

    test("abs"):
        checkEval: (x: BigInt) =>
            x.absolute >= 0 &&
                (x >= 0 && x.absolute == x ||
                    x.absolute == -x)

        assertEvalWithinBudget(
          BigInt(0).absolute,
          BigInt(0),
          ExUnits(memory = 1802, steps = 391986)
        )
        assertEvalWithinBudget(
          BigInt(5).absolute,
          BigInt(5),
          ExUnits(memory = 2102, steps = 439986)
        )
        assertEvalWithinBudget(
          BigInt(-7).absolute,
          BigInt(7),
          ExUnits(memory = 2204, steps = 557194)
        )

    test("min"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = min(x, y)
            (m <= x && m <= y) &&
            (m == x || m == y)

        assertEvalWithinBudget(
          min(BigInt(1), BigInt(2)),
          BigInt(1),
          ExUnits(memory = 2102, steps = 439986)
        )
        assertEvalWithinBudget(
          min(BigInt(-1), BigInt(-5)),
          BigInt(-5),
          ExUnits(memory = 2102, steps = 439986)
        )

    test("max"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = max(x, y)
            (m >= x && m >= y) &&
            (m == x || m == y)

        assertEvalWithinBudget(
          max(BigInt(1), BigInt(2)),
          BigInt(2),
          ExUnits(memory = 2102, steps = 439986)
        )
        assertEvalWithinBudget(
          max(BigInt(-1), BigInt(-5)),
          BigInt(-1),
          ExUnits(memory = 2102, steps = 439986)
        )

    test("clamp"):
        checkEval: (self: BigInt, min: BigInt, max: BigInt) =>
            val c = self.clamp(min, max)
            min > max || c >= min && c <= max

        assertEvalWithinBudget(
          BigInt(42).clamp(13, 37),
          BigInt(37),
          ExUnits(memory = 4904, steps = 1010778)
        )
        assertEvalWithinBudget(
          BigInt(3).clamp(13, 17),
          BigInt(13),
          ExUnits(memory = 4904, steps = 1010778)
        )
        assertEvalWithinBudget(
          BigInt(17).clamp(15, 25),
          BigInt(17),
          ExUnits(memory = 4904, steps = 1010778)
        )
        assertEvalWithinBudget(
          BigInt(7).clamp(5, 7),
          BigInt(7),
          ExUnits(memory = 4904, steps = 1010778)
        )

    test("gcd"):
        checkEval: (x: BigInt, y: BigInt) =>
            val g = gcd(x, y)
            g >= BigInt(0) &&
            (x == BigInt(0) || (x % g) == BigInt(0)) &&
            (y == BigInt(0) || (y % g) == BigInt(0))

        assertEvalWithinBudget(
          gcd(BigInt(0), BigInt(0)),
          BigInt(0),
          ExUnits(memory = 6104, steps = 1208368)
        )
        assertEvalWithinBudget(
          gcd(BigInt(12), BigInt(18)),
          BigInt(6),
          ExUnits(memory = 15713, steps = 3525304)
        )
        assertEvalWithinBudget(
          gcd(BigInt(-12), BigInt(18)),
          BigInt(6),
          ExUnits(memory = 15713, steps = 3525304)
        )

    test("sqrt"):
        checkEval: (x: BigInt) =>
            sqrt(x) match
                case None => x < 0
                case Some(s) =>
                    s * s <= x && (s + 1) * (s + 1) > x &&
                    x.isSqrt(s)

        assertEvalWithinBudget(
          BigInt(0).sqRoot,
          Some(BigInt(0)),
          ExUnits(memory = 6632, steps = 1382380)
        )
        assertEvalWithinBudget(
          BigInt(1).sqRoot,
          Some(BigInt(1)),
          ExUnits(memory = 6632, steps = 1382380)
        )
        assertEvalWithinBudget(
          BigInt(17).sqRoot,
          Some(BigInt(4)),
          ExUnits(memory = 22555, steps = 5734266)
        )
        assertEvalWithinBudget(BigInt(-1).sqRoot, None, ExUnits(memory = 6632, steps = 1382380))
        assertEval(17.isSqrt(4))

    test("pow"):
        assertEvalWithinBudget(
          pow(BigInt(0), BigInt(0)),
          BigInt(1),
          ExUnits(memory = 6104, steps = 1209821)
        )
        assertEvalWithinBudget(
          pow(BigInt(2), BigInt(0)),
          BigInt(1),
          ExUnits(memory = 6104, steps = 1209821)
        )
        assertEvalWithinBudget(
          pow(BigInt(2), BigInt(3)),
          BigInt(8),
          ExUnits(memory = 18928, steps = 4865297)
        )
        assertEvalWithinBudget(
          pow(BigInt(-2), BigInt(3)),
          BigInt(-8),
          ExUnits(memory = 18928, steps = 4865297)
        )
        assertEvalWithinBudget(
          pow(BigInt(7), BigInt(2)),
          BigInt(49),
          ExUnits(memory = 18124, steps = 4545136)
        )
        assertEvalWithinBudget(
          pow(BigInt(513), BigInt(3)),
          BigInt(135005697),
          ExUnits(memory = 18928, steps = 4865297)
        )
        assertEvalWithinBudget(
          pow(BigInt(2), BigInt(42)),
          BigInt("4398046511104"),
          ExUnits(memory = 42164, steps = 11215766)
        )

    test("exp2"):
        assertEvalWithinBudget(exp2(BigInt(-2)), BigInt(0), ExUnits(memory = 2102, steps = 441439))
        assertEvalWithinBudget(exp2(BigInt(0)), BigInt(1), ExUnits(memory = 5309, steps = 3758628))
        assertEvalWithinBudget(exp2(BigInt(1)), BigInt(2), ExUnits(memory = 5309, steps = 3758628))
        assertEvalWithinBudget(exp2(BigInt(4)), BigInt(16), ExUnits(memory = 5309, steps = 3758628))
        assertEvalWithinBudget(
          exp2(BigInt(42)),
          BigInt("4398046511104"),
          ExUnits(memory = 5309, steps = 3758628)
        )
        assertEvalWithinBudget(
          exp2(BigInt(256)),
          BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639936"),
          ExUnits(memory = 5319, steps = 3939663)
        )

    test("log2"):
        assertEvalWithinBudget(log2(BigInt(1)), BigInt(0), ExUnits(memory = 6923, steps = 2879362))
        assertEvalWithinBudget(log2(BigInt(2)), BigInt(1), ExUnits(memory = 8225, steps = 3208701))
        assertEvalWithinBudget(log2(BigInt(3)), BigInt(1), ExUnits(memory = 8225, steps = 3208701))
        assertEvalWithinBudget(log2(BigInt(4)), BigInt(2), ExUnits(memory = 9527, steps = 3538040))
        assertEvalWithinBudget(
          log2(BigInt(256)),
          BigInt(8),
          ExUnits(memory = 6923, steps = 2879362)
        )
        assertEvalWithinBudget(
          log2(BigInt(257)),
          BigInt(8),
          ExUnits(memory = 6923, steps = 2879362)
        )
        assertEvalWithinBudget(
          log2(BigInt(511)),
          BigInt(8),
          ExUnits(memory = 6923, steps = 2879362)
        )
        assertEvalWithinBudget(
          log2(BigInt(1025)),
          BigInt(10),
          ExUnits(memory = 9527, steps = 3538040)
        )

    test("log"):
        assertEvalWithinBudget(
          log(BigInt(10), base = BigInt(2)),
          BigInt(3),
          ExUnits(memory = 16619, steps = 3944756)
        )
        assertEvalWithinBudget(
          log(BigInt(42), base = BigInt(2)),
          BigInt(5),
          ExUnits(memory = 23829, steps = 5805710)
        )
        assertEvalWithinBudget(
          log(BigInt(42), base = BigInt(3)),
          BigInt(3),
          ExUnits(memory = 16619, steps = 3944756)
        )
        assertEvalWithinBudget(
          log(BigInt(5), base = BigInt(0)),
          BigInt(0),
          ExUnits(memory = 2702, steps = 535986)
        )
        assertEvalWithinBudget(
          log(BigInt(4), base = BigInt(4)),
          BigInt(1),
          ExUnits(memory = 9409, steps = 2083802)
        )
        assertEvalWithinBudget(
          log(BigInt(4), base = BigInt(42)),
          BigInt(0),
          ExUnits(memory = 5804, steps = 1153325)
        )
