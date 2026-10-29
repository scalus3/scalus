package scalus.cardano.onchain.plutus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.{OnchainError, RequirementError}
import scalus.cardano.onchain.plutus.prelude.Math.*
import scalus.cardano.onchain.plutus.prelude.{divCeil, divFloor, Math}
import scalus.testing.budget.BudgetTable
import scalus.testing.kit.EvalTestKit
import scalus.uplc.{DeBruijn, PlutusV3}
import scalus.uplc.Constant.given
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.{Log, PlutusVM, RestrictingBudgetSpender}

import scala.util.Try

class MathTest extends AnyFunSuite with EvalTestKit {

    override protected def budgetTable: BudgetTable = MathTestBudgets

    test("abs-properties") {
        checkEval: (x: BigInt) =>
            x.absolute >= 0 &&
                (x >= 0 && x.absolute == x ||
                    x.absolute == -x)
    }

    test("abs - 0 - budget") {
        assertEvalEq(BigInt(0))((x: BigInt) => x.absolute).at(0)
    }

    test("abs - positive - budget") {
        assertEvalEq(BigInt(5))((x: BigInt) => x.absolute).at(5)
    }

    test("abs - negative - budget") {
        assertEvalEq(BigInt(7))((x: BigInt) => x.absolute).at(-7)
    }

    test("min") {
        checkEval: (x: BigInt, y: BigInt) =>
            val m = min(x, y)
            (m <= x && m <= y) &&
            (m == x || m == y)

        assertEvalEq(BigInt(1))(min).at(1, 2)
        assertEvalEq(BigInt(-5))(min).at(-1, -5)
    }

    test("max") {
        checkEval: (x: BigInt, y: BigInt) =>
            val m = max(x, y)
            (m >= x && m >= y) &&
            (m == x || m == y)

        assertEvalEq(BigInt(2))(max).at(1, 2)
        assertEvalEq(BigInt(-1))(max).at(-1, -5)
    }

    test("clamp") {
        checkEval: (self: BigInt, min: BigInt, max: BigInt) =>
            val c = self.clamp(min, max)
            min > max || c >= min && c <= max

        assertEvalEq(BigInt(37))(Math.clamp).at(42, 13, 37)
        assertEvalEq(BigInt(13))(Math.clamp).at(3, 13, 17)
        assertEvalEq(BigInt(17))(Math.clamp).at(17, 15, 25)
        assertEvalEq(BigInt(7))(Math.clamp).at(7, 5, 7)
    }

    test("gcd") {
        checkEval: (x: BigInt, y: BigInt) =>
            val g = gcd(x, y)
            g >= BigInt(0) &&
            (x == BigInt(0) || (x % g) == BigInt(0)) &&
            (y == BigInt(0) || (y % g) == BigInt(0))

        assertEvalEq(BigInt(0))(gcd).at(0, 0)
        assertEvalEq(BigInt(6))(gcd).at(12, 18)
        assertEvalEq(BigInt(6))(gcd).at(-12, 18)
    }

    test("sqrt") {
        checkEval: (x: BigInt) =>
            if x < 0 then true // sqrt requires non-negative input
            else
                val s = sqrt(x)
                s * s <= x && (s + 1) * (s + 1) > x && x.isSqrt(s)

        assertEvalEq(BigInt(0))(sqrt).at(0)
        assertEvalEq(BigInt(1))(sqrt).at(1)
        assertEvalEq(BigInt(4))(sqrt).at(17)
        assertEvalFailsWithMessage[RequirementError]("sqrt: negative radicand")(BigInt(-1).sqRoot)
        assertEval(17.isSqrt(4))

        // Large inputs
        assertEvalEq(BigInt("1000000"))(sqrt).at(BigInt("1000000000000"))
        assertEvalEq(BigInt("1000000000000000"))(sqrt).at(BigInt("1000000000000000000000000000000"))
        assertEvalEq(BigInt("1000000000000000000000000000000"))(sqrt)
            .at(BigInt("1000000000000000000000000000000000000000000000000000000000000"))
    }

    test("pow") {
        assertEvalEq(BigInt(1))(pow).at(0, 0)
        assertEvalEq(BigInt(1))(pow).at(2, 0)
        assertEvalEq(BigInt(8))(pow).at(2, 3)
        assertEvalEq(BigInt(-8))(pow).at(-2, 3)
        assertEvalEq(BigInt(49))(pow).at(7, 2)
        assertEvalEq(BigInt(135005697))(pow).at(513, 3)
        assertEvalEq(BigInt("4398046511104"))(pow).at(2, 42)
    }

    test("exp2") {
        assertEvalEq(BigInt(0))(exp2).at(-2)
        assertEvalEq(BigInt(1))(exp2).at(0)
        assertEvalEq(BigInt(2))(exp2).at(1)
        assertEvalEq(BigInt(16))(exp2).at(4)
        assertEvalEq(BigInt("4398046511104"))(exp2).at(42)
        assertEvalEq(
          BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639936")
        )(exp2).at(256)
    }

    test("log2") {
        assertEvalEq(BigInt(0))(log2).at(1)
        assertEvalEq(BigInt(1))(log2).at(2)
        assertEvalEq(BigInt(1))(log2).at(3)
        assertEvalEq(BigInt(2))(log2).at(4)
        assertEvalEq(BigInt(8))(log2).at(256)
        assertEvalEq(BigInt(8))(log2).at(257)
        assertEvalEq(BigInt(8))(log2).at(511)
        assertEvalEq(BigInt(10))(log2).at(1025)
    }

    test("log") {
        assertEvalEq(BigInt(3))(log).at(10, 2)
        assertEvalEq(BigInt(5))(log).at(42, 2)
        assertEvalEq(BigInt(3))(log).at(42, 3)
        assertEvalEq(BigInt(0))(log).at(5, 0)
        assertEvalEq(BigInt(1))(log).at(4, 4)
        assertEvalEq(BigInt(0))(log).at(4, 42)
    }

    test("log with base 1 fails") {
        // On chain first, under a small budget: the old code ran until the budget was exhausted,
        // which also fails, but without the message.
        val compiled = PlutusV3.compile((n: Data) => log(n.to[BigInt], base = BigInt(1)))
        val applied = compiled.program.term $ BigInt(5).toData.asTerm
        val logger = Log()
        val result = Try(
          summon[PlutusVM].evaluateDeBruijnedTerm(
            DeBruijn.deBruijnTerm(applied),
            RestrictingBudgetSpender(ExUnits(memory = 1_000_000, steps = 1_000_000_000)),
            logger
          )
        )
        assert(result.isFailure)
        assert(logger.getLogs.exists(_.contains(Math.LogBaseOne)), logger.getLogs.toList)
        assertEvalFailsWithMessage[OnchainError](Math.LogBaseOne)(log(BigInt(1), BigInt(1)))
    }

    test("divFloor and divCeil") {
        checkEval: (a: BigInt, b: BigInt) =>
            if b == BigInt(0) then true
            else
                val floor = Math.divFloor(a, b)
                val ceil = Math.divCeil(a, b)
                // floor <= a/b <= ceil, differing by exactly one when the division is inexact.
                val floorBracketsQuotient =
                    if b > 0 then floor * b <= a && a < (floor + 1) * b
                    else floor * b >= a && a > (floor + 1) * b
                val ceilBracketsQuotient =
                    if b > 0 then ceil * b >= a && a > (ceil - 1) * b
                    else ceil * b <= a && a < (ceil - 1) * b
                floorBracketsQuotient && ceilBracketsQuotient &&
                ((a % b == BigInt(0)) == (floor == ceil)) &&
                (floor == ceil || ceil - floor == BigInt(1))

        // The four sign cases, where Scala's `/` truncates toward zero.
        assertEvalEq[BigInt](3)(Math.divFloor(7, 2))
        assertEvalEq[BigInt](-4)(Math.divFloor(-7, 2))
        assertEvalEq[BigInt](-4)(Math.divFloor(7, -2))
        assertEvalEq[BigInt](3)(Math.divFloor(-7, -2))
        assertEvalEq[BigInt](4)(Math.divCeil(7, 2))
        assertEvalEq[BigInt](-3)(Math.divCeil(-7, 2))
        assertEvalEq[BigInt](-3)(Math.divCeil(7, -2))
        assertEvalEq[BigInt](4)(Math.divCeil(-7, -2))
        assertEvalEq[BigInt](3)(Math.divCeil(6, 2))
        assertEvalEq[BigInt](3)(Math.divFloor(6, 2))
        // Infix form.
        assertEvalEq[BigInt](4)(BigInt(7) divCeil BigInt(2))
        assertEvalEq[BigInt](-4)(BigInt(-7) divFloor BigInt(2))
    }

    test("budget: divFloor") {
        assertEvalEq(BigInt(-4))(Math.divFloor).at(-7, 2)
    }

    test("budget: divCeil") {
        assertEvalEq(BigInt(-3))(Math.divCeil).at(-7, 2)
    }
}
