package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.prelude.Math.*
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.testing.kit.EvalTestKit

class MathTest extends AnyFunSuite with EvalTestKit:

    // Disable optimizer: partial evaluation folds these closed expressions
    // to constants, making budget assertions meaningless (just measuring startup cost).
    override protected def compilerOptions: Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("abs-properties"):
        checkEval: (x: BigInt) =>
            x.absolute >= 0 &&
                (x >= 0 && x.absolute == x ||
                    x.absolute == -x)

    test("abs - 0 - budget"):
        assertEvalWithBudgets(
          BigInt(0).absolute,
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 1101, steps = 203937)
          )
        )

    test("abs - positive - budget"):
        assertEvalWithBudgets(
          BigInt(5).absolute,
          BigInt(5),
          Seq(
            compilerOptions -> ExUnits(memory = 1101, steps = 203937)
          )
        )

    test("abs - negative - budget"):
        assertEvalWithBudgets(
          BigInt(-7).absolute,
          BigInt(7),
          Seq(
            compilerOptions -> ExUnits(memory = 1503, steps = 369145)
          )
        )

    test("min"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = min(x, y)
            (m <= x && m <= y) &&
            (m == x || m == y)

        assertEvalWithBudgets(
          min(BigInt(1), BigInt(2)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 1401, steps = 251937)
          )
        )
        assertEvalWithBudgets(
          min(BigInt(-1), BigInt(-5)),
          BigInt(-5),
          Seq(
            compilerOptions -> ExUnits(memory = 1401, steps = 251937)
          )
        )

    test("max"):
        checkEval: (x: BigInt, y: BigInt) =>
            val m = max(x, y)
            (m >= x && m >= y) &&
            (m == x || m == y)

        assertEvalWithBudgets(
          max(BigInt(1), BigInt(2)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 1401, steps = 251937)
          )
        )
        assertEvalWithBudgets(
          max(BigInt(-1), BigInt(-5)),
          BigInt(-1),
          Seq(
            compilerOptions -> ExUnits(memory = 1401, steps = 251937)
          )
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

        assertEvalWithBudgets(
          gcd(BigInt(0), BigInt(0)),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 4102, steps = 736270)
          )
        )
        assertEvalWithBudgets(
          gcd(BigInt(12), BigInt(18)),
          BigInt(6),
          Seq(
            compilerOptions -> ExUnits(memory = 9808, steps = 2_202292)
          )
        )
        assertEvalWithBudgets(
          gcd(BigInt(-12), BigInt(18)),
          BigInt(6),
          Seq(
            compilerOptions -> ExUnits(memory = 9808, steps = 2_202292)
          )
        )

    test("sqrt"):
        checkEval: (x: BigInt) =>
            if x < 0 then true // sqrt requires non-negative input
            else
                val s = sqrt(x)
                s * s <= x && (s + 1) * (s + 1) > x && x.isSqrt(s)

        assertEvalWithBudgets(
          BigInt(0).sqRoot,
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 4102, steps = 727774)
          )
        )
        assertEvalWithBudgets(
          BigInt(1).sqRoot,
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 4102, steps = 727774)
          )
        )
        assertEvalWithBudgets(
          BigInt(17).sqRoot,
          BigInt(4),
          Seq(
            compilerOptions -> ExUnits(memory = 25453, steps = 10_270129)
          )
        )
        assertEvalFailsWithMessage[RequirementError]("sqrt: negative radicand")(BigInt(-1).sqRoot)
        assertEval(17.isSqrt(4))

        // Large input budget tests
        assertEvalWithBudgets(
          sqrt(BigInt("1000000000000")), // 10^12
          BigInt("1000000"),
          Seq(
            compilerOptions -> ExUnits(memory = 26355, steps = 10_504709)
          )
        )
        assertEvalWithBudgets(
          sqrt(BigInt("1000000000000000000000000000000")), // 10^30
          BigInt("1000000000000000"),
          Seq(
            compilerOptions -> ExUnits(memory = 30563, steps = 11_903433)
          )
        )
        assertEvalWithBudgets(
          sqrt(BigInt("1000000000000000000000000000000000000000000000000000000000000")), // 10^60
          BigInt("1000000000000000000000000000000"),
          Seq(
            compilerOptions -> ExUnits(memory = 38395, steps = 14_343288)
          )
        )

    test("pow"):
        assertEvalWithBudgets(
          pow(BigInt(0), BigInt(0)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 4102, steps = 737723)
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(2), BigInt(0)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 4102, steps = 737723)
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(2), BigInt(3)),
          BigInt(8),
          Seq(
            compilerOptions -> ExUnits(memory = 12922, steps = 3_450647)
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(-2), BigInt(3)),
          BigInt(-8),
          Seq(
            compilerOptions -> ExUnits(memory = 12922, steps = 3_450647)
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(7), BigInt(2)),
          BigInt(49),
          Seq(
            compilerOptions -> ExUnits(memory = 12118, steps = 3_130486)
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(513), BigInt(3)),
          BigInt(135005697),
          Seq(
            compilerOptions -> ExUnits(memory = 12922, steps = 3_450647)
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(2), BigInt(42)),
          BigInt("4398046511104"),
          Seq(
            compilerOptions -> ExUnits(memory = 28150, steps = 7_916012)
          )
        )

    test("exp2"):
        assertEvalWithBudgets(
          exp2(BigInt(-2)),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 1401, steps = 253390)
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(0)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 4608, steps = 3_571401)
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(1)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 4608, steps = 3_571401)
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(4)),
          BigInt(16),
          Seq(
            compilerOptions -> ExUnits(memory = 4608, steps = 3_571401)
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(42)),
          BigInt("4398046511104"),
          Seq(
            compilerOptions -> ExUnits(memory = 4608, steps = 3_571401)
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(256)),
          BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639936"),
          Seq(
            compilerOptions -> ExUnits(memory = 4618, steps = 3_752436)
          )
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
        assertEvalWithBudgets(
          log(BigInt(10), base = BigInt(2)),
          BigInt(3),
          Seq(
            compilerOptions -> ExUnits(memory = 10714, steps = 2_621744)
          )
        )
        assertEvalWithBudgets(
          log(BigInt(42), base = BigInt(2)),
          BigInt(5),
          Seq(
            compilerOptions -> ExUnits(memory = 15322, steps = 3_915422)
          )
        )
        assertEvalWithBudgets(
          log(BigInt(42), base = BigInt(3)),
          BigInt(3),
          Seq(
            compilerOptions -> ExUnits(memory = 10714, steps = 2_621744)
          )
        )
        assertEvalWithBudgets(
          log(BigInt(5), base = BigInt(0)),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 1701, steps = 299937)
          )
        )
        assertEvalWithBudgets(
          log(BigInt(4), base = BigInt(4)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6106, steps = 1_328066)
          )
        )
        assertEvalWithBudgets(
          log(BigInt(4), base = BigInt(42)),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 3802, steps = 681227)
          )
        )
