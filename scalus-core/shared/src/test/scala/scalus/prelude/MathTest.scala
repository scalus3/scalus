package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.prelude.Math.*
import scalus.testing.kit.EvalTestKit

class MathTest extends AnyFunSuite with EvalTestKit:

    test("abs"):
        checkEval: (x: BigInt) =>
            x.absolute >= 0 &&
                (x >= 0 && x.absolute == x ||
                    x.absolute == -x)

        assertEvalWithBudget(
          BigInt(0).absolute,
          BigInt(0),
          ExUnits(memory = 1702, steps = 375986)
        )
        assertEvalWithBudget(
          BigInt(5).absolute,
          BigInt(5),
          ExUnits(memory = 1702, steps = 375986)
        )
        assertEvalWithBudget(
          BigInt(-7).absolute,
          BigInt(7),
          ExUnits(memory = 2104, steps = 541194)
        )

    test("min"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = min(x, y)
            (m <= x && m <= y) &&
            (m == x || m == y)

        assertEvalWithBudget(
          min(BigInt(1), BigInt(2)),
          BigInt(1),
          ExUnits(memory = 1502, steps = 343986)
        )
        assertEvalWithBudget(
          min(BigInt(-1), BigInt(-5)),
          BigInt(-5),
          ExUnits(memory = 1502, steps = 343986)
        )

    test("max"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = max(x, y)
            (m >= x && m >= y) &&
            (m == x || m == y)

        assertEvalWithBudget(
          max(BigInt(1), BigInt(2)),
          BigInt(2),
          ExUnits(memory = 1502, steps = 343986)
        )
        assertEvalWithBudget(
          max(BigInt(-1), BigInt(-5)),
          BigInt(-1),
          ExUnits(memory = 1502, steps = 343986)
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

        assertEvalWithBudget(
          gcd(BigInt(0), BigInt(0)),
          BigInt(0),
          ExUnits(memory = 6104, steps = 1208368)
        )
        assertEvalWithBudget(
          gcd(BigInt(12), BigInt(18)),
          BigInt(6),
          ExUnits(memory = 15113, steps = 3429304)
        )
        assertEvalWithBudget(
          gcd(BigInt(-12), BigInt(18)),
          BigInt(6),
          ExUnits(memory = 15113, steps = 3429304)
        )

    test("sqrt"):
        checkEval: (x: BigInt) =>
            if x < 0 then true // sqrt requires non-negative input
            else
                val s = sqrt(x)
                s * s <= x && (s + 1) * (s + 1) > x && x.isSqrt(s)

        assertEvalWithBudget(
          BigInt(0).sqRoot,
          BigInt(0),
          ExUnits(memory = 6204, steps = 1215872)
        )
        assertEvalWithBudget(
          BigInt(1).sqRoot,
          BigInt(1),
          ExUnits(memory = 6204, steps = 1215872)
        )
        assertEvalWithBudget(
          BigInt(17).sqRoot,
          BigInt(4),
          ExUnits(memory = 32665, steps = 12331018)
        )
        assertEvalFailsWithMessage[RequirementError]("sqrt: negative radicand")(BigInt(-1).sqRoot)
        assertEval(17.isSqrt(4))

        // Large input budget tests
        assertEvalWithBudget(
          sqrt(BigInt("1000000000000")), // 10^12
          BigInt("1000000"),
          ExUnits(memory = 34669, steps = 12893696)
        )
        assertEvalWithBudget(
          sqrt(BigInt("1000000000000000000000000000000")), // 10^30
          BigInt("1000000000000000"),
          ExUnits(memory = 39576, steps = 14324672)
        )
        assertEvalWithBudget(
          sqrt(BigInt("1000000000000000000000000000000000000000000000000000000000000")), // 10^60
          BigInt("1000000000000000000000000000000"),
          ExUnits(memory = 50713, steps = 17650167)
        )

    test("pow"):
        assertEvalWithBudget(
          pow(BigInt(0), BigInt(0)),
          BigInt(1),
          ExUnits(memory = 6004, steps = 1193821)
        )
        assertEvalWithBudget(
          pow(BigInt(2), BigInt(0)),
          BigInt(1),
          ExUnits(memory = 6004, steps = 1193821)
        )
        assertEvalWithBudget(
          pow(BigInt(2), BigInt(3)),
          BigInt(8),
          ExUnits(memory = 17828, steps = 4689297)
        )
        assertEvalWithBudget(
          pow(BigInt(-2), BigInt(3)),
          BigInt(-8),
          ExUnits(memory = 17828, steps = 4689297)
        )
        assertEvalWithBudget(
          pow(BigInt(7), BigInt(2)),
          BigInt(49),
          ExUnits(memory = 17024, steps = 4369136)
        )
        assertEvalWithBudget(
          pow(BigInt(513), BigInt(3)),
          BigInt(135005697),
          ExUnits(memory = 17828, steps = 4689297)
        )
        assertEvalWithBudget(
          pow(BigInt(2), BigInt(42)),
          BigInt("4398046511104"),
          ExUnits(memory = 39064, steps = 10719766)
        )

    test("exp2"):
        assertEvalWithBudget(exp2(BigInt(-2)), BigInt(0), ExUnits(memory = 2302, steps = 473439))
        assertEvalWithBudget(exp2(BigInt(0)), BigInt(1), ExUnits(memory = 5109, steps = 3726628))
        assertEvalWithBudget(exp2(BigInt(1)), BigInt(2), ExUnits(memory = 5109, steps = 3726628))
        assertEvalWithBudget(exp2(BigInt(4)), BigInt(16), ExUnits(memory = 5109, steps = 3726628))
        assertEvalWithBudget(
          exp2(BigInt(42)),
          BigInt("4398046511104"),
          ExUnits(memory = 5109, steps = 3726628)
        )
        assertEvalWithBudget(
          exp2(BigInt(256)),
          BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639936"),
          ExUnits(memory = 5119, steps = 3907663)
        )

    test("log2"):
        assertEvalWithinBudget(
          log2(BigInt(1)),
          BigInt(0),
          ExUnits(memory = 6923, steps = 2879362)
        )
        assertEvalWithinBudget(
          log2(BigInt(2)),
          BigInt(1),
          ExUnits(memory = 8225, steps = 3208701)
        )
        assertEvalWithinBudget(
          log2(BigInt(3)),
          BigInt(1),
          ExUnits(memory = 8225, steps = 3208701)
        )
        assertEvalWithinBudget(
          log2(BigInt(4)),
          BigInt(2),
          ExUnits(memory = 9527, steps = 3538040)
        )
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
        assertEvalWithBudget(
          log(BigInt(10), base = BigInt(2)),
          BigInt(3),
          ExUnits(memory = 16019, steps = 3848756)
        )
        assertEvalWithBudget(
          log(BigInt(42), base = BigInt(2)),
          BigInt(5),
          ExUnits(memory = 22829, steps = 5645710)
        )
        assertEvalWithBudget(
          log(BigInt(42), base = BigInt(3)),
          BigInt(3),
          ExUnits(memory = 16019, steps = 3848756)
        )
        assertEvalWithBudget(
          log(BigInt(5), base = BigInt(0)),
          BigInt(0),
          ExUnits(memory = 2902, steps = 567986)
        )
        assertEvalWithBudget(
          log(BigInt(4), base = BigInt(4)),
          BigInt(1),
          ExUnits(memory = 9209, steps = 2051802)
        )
        assertEvalWithBudget(
          log(BigInt(4), base = BigInt(42)),
          BigInt(0),
          ExUnits(memory = 5804, steps = 1153325)
        )
