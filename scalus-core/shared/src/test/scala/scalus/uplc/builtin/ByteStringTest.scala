package scalus.uplc.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.budget.BudgetTable
import scalus.uplc.builtin.ByteString.*
import scalus.testing.kit.EvalTestKit
import scalus.uplc.eval.BuiltinException

class ByteStringTest extends AnyFunSuite with EvalTestKit {
    override protected def budgetTable: BudgetTable = ByteStringTestBudgets

    test("fromBigIntBigEndian") {
        assertEvalEq(hex"0f4240")((n: BigInt) => fromBigIntBigEndian(n, 3)).at(1_000_000)
        assertEvalEq(hex"00000f4240")((n: BigInt) => fromBigIntBigEndian(n, 5)).at(1_000_000)
        assertEvalEq(hex"0000000000000000")((n: BigInt) => fromBigIntBigEndian(n, 8)).at(0)
        assertEvalFails[BuiltinException](fromBigIntBigEndian(1_000_000, 1))
    }

    test("fromBigIntLittleEndian") {
        assertEvalEq(hex"40420f")((n: BigInt) => fromBigIntLittleEndian(n, 3)).at(1_000_000)
        assertEvalEq(hex"40420f0000")((n: BigInt) => fromBigIntLittleEndian(n, 5)).at(1_000_000)
        assertEvalEq(hex"0000000000000000")((n: BigInt) => fromBigIntLittleEndian(n, 8)).at(0)
        assertEvalFails[BuiltinException](fromBigIntLittleEndian(1_000_000, 1))
    }

    test("utf8 string interpolator") {
        assertEvalEq(utf8"hello", fromString("hello"))
        assertEvalEq(utf8"Hello, World!", fromString("Hello, World!"))
        assertEvalEq(utf8"Hello, 世界", fromString("Hello, 世界"))
        assertEvalEq(utf8"", fromString(""))
        assertEvalEq(utf8"123!@#$$%^&*()", fromString("123!@#$%^&*()"))
    }
}
