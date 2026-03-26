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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1802, steps = 391986),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 1802,
              steps = 391_986
            )
          )
        )

    test("abs - positive - budget"):
        assertEvalWithBudgets(
          BigInt(5).absolute,
          BigInt(5),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1802, steps = 391986),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 1802,
              steps = 391_986
            )
          )
        )

    test("abs - negative - budget"):
        assertEvalWithBudgets(
          BigInt(-7).absolute,
          BigInt(7),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 2204, steps = 557194),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 2204,
              steps = 557_194
            )
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 2102, steps = 439986),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 2102,
              steps = 439_986
            )
          )
        )
        assertEvalWithBudgets(
          min(BigInt(-1), BigInt(-5)),
          BigInt(-5),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 2102, steps = 439986),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 2102,
              steps = 439_986
            )
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 2102, steps = 439986),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 2102,
              steps = 439_986
            )
          )
        )
        assertEvalWithBudgets(
          max(BigInt(-1), BigInt(-5)),
          BigInt(-1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 2102, steps = 439986),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 2102,
              steps = 439_986
            )
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6104, steps = 1208368),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 6104,
              steps = 1_208_368
            )
          )
        )
        assertEvalWithBudgets(
          gcd(BigInt(12), BigInt(18)),
          BigInt(6),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 15713, steps = 3525304),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 15713,
              steps = 3_525_304
            )
          )
        )
        assertEvalWithBudgets(
          gcd(BigInt(-12), BigInt(18)),
          BigInt(6),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 15713, steps = 3525304),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 15713,
              steps = 3_525_304
            )
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6104, steps = 1199872),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 6104,
              steps = 1_199_872
            )
          )
        )
        assertEvalWithBudgets(
          BigInt(1).sqRoot,
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6104, steps = 1199872),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 6104,
              steps = 1_199_872
            )
          )
        )
        assertEvalWithBudgets(
          BigInt(17).sqRoot,
          BigInt(4),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 35665, steps = 12811018),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 35665,
              steps = 12_811_018
            )
          )
        )
        assertEvalFailsWithMessage[RequirementError]("sqrt: negative radicand")(BigInt(-1).sqRoot)
        assertEval(17.isSqrt(4))

        // Large input budget tests
        assertEvalWithBudgets(
          sqrt(BigInt("1000000000000")), // 10^12
          BigInt("1000000"),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 37969, steps = 13421696),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 37969,
              steps = 13_421_696
            )
          )
        )
        assertEvalWithBudgets(
          sqrt(BigInt("1000000000000000000000000000000")), // 10^30
          BigInt("1000000000000000"),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 42676, steps = 14820672),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 42676,
              steps = 14_820_672
            )
          )
        )
        assertEvalWithBudgets(
          sqrt(BigInt("1000000000000000000000000000000000000000000000000000000000000")), // 10^60
          BigInt("1000000000000000000000000000000"),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 55213, steps = 18370167),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 55213,
              steps = 18_370_167
            )
          )
        )

    test("pow"):
        assertEvalWithBudgets(
          pow(BigInt(0), BigInt(0)),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6104, steps = 1209821),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 6104,
              steps = 1_209_821
            )
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(2), BigInt(0)),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6104, steps = 1209821),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 6104,
              steps = 1_209_821
            )
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(2), BigInt(3)),
          BigInt(8),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 18928, steps = 4865297),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 18928,
              steps = 4_865_297
            )
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(-2), BigInt(3)),
          BigInt(-8),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 18928, steps = 4865297),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 18928,
              steps = 4_865_297
            )
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(7), BigInt(2)),
          BigInt(49),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 18124, steps = 4545136),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 18124,
              steps = 4_545_136
            )
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(513), BigInt(3)),
          BigInt(135005697),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 18928, steps = 4865297),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 18928,
              steps = 4_865_297
            )
          )
        )
        assertEvalWithBudgets(
          pow(BigInt(2), BigInt(42)),
          BigInt("4398046511104"),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 42164, steps = 11215766),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 42164,
              steps = 11_215_766
            )
          )
        )

    test("exp2"):
        assertEvalWithBudgets(
          exp2(BigInt(-2)),
          BigInt(0),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 2102, steps = 441439),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 2102,
              steps = 441_439
            )
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(0)),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5309, steps = 3758628),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 5309,
              steps = 3_758_628
            )
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(1)),
          BigInt(2),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5309, steps = 3758628),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 5309,
              steps = 3_758_628
            )
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(4)),
          BigInt(16),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5309, steps = 3758628),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 5309,
              steps = 3_758_628
            )
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(42)),
          BigInt("4398046511104"),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5309, steps = 3758628),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 5309,
              steps = 3_758_628
            )
          )
        )
        assertEvalWithBudgets(
          exp2(BigInt(256)),
          BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639936"),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5319, steps = 3939663),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 5319,
              steps = 3_939_663
            )
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 16619, steps = 3944756),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 16619,
              steps = 3_944_756
            )
          )
        )
        assertEvalWithBudgets(
          log(BigInt(42), base = BigInt(2)),
          BigInt(5),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 23829, steps = 5805710),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 23829,
              steps = 5_805_710
            )
          )
        )
        assertEvalWithBudgets(
          log(BigInt(42), base = BigInt(3)),
          BigInt(3),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 16619, steps = 3944756),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 16619,
              steps = 3_944_756
            )
          )
        )
        assertEvalWithBudgets(
          log(BigInt(5), base = BigInt(0)),
          BigInt(0),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 2702, steps = 535986),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 2702,
              steps = 535_986
            )
          )
        )
        assertEvalWithBudgets(
          log(BigInt(4), base = BigInt(4)),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 9409, steps = 2083802),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 9409,
              steps = 2_083_802
            )
          )
        )
        assertEvalWithBudgets(
          log(BigInt(4), base = BigInt(42)),
          BigInt(0),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5804, steps = 1153325),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 5804,
              steps = 1_153_325
            )
          )
        )
