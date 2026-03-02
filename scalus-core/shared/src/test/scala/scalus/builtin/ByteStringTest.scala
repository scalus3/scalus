package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString.*
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.EvalTestKit
import scalus.uplc.eval.BuiltinException

class ByteStringTest extends AnyFunSuite with EvalTestKit {

    test("fromBigIntBigEndian") {
        assertEvalWithBudget(
          (n: BigInt) => fromBigIntBigEndian(n, 3),
          1_000_000,
          hex"0f4240",
          ExUnits(memory = 1233, steps = 1_519451)
        )
        assertEvalWithBudget(
          (n: BigInt) => fromBigIntBigEndian(n, 5),
          1_000_000,
          hex"00000f4240",
          ExUnits(memory = 1233, steps = 1_519451)
        )
        assertEvalWithBudget(
          (n: BigInt) => fromBigIntBigEndian(n, 8),
          0,
          hex"0000000000000000",
          ExUnits(memory = 1233, steps = 1_519451)
        )
        assertEvalFails[BuiltinException](fromBigIntBigEndian(1_000_000, 1))
    }

    test("fromBigIntLittleEndian") {
        assertEvalWithBudget(
          (n: BigInt) => fromBigIntLittleEndian(n, 3),
          1_000_000,
          hex"40420f",
          ExUnits(memory = 1233, steps = 1_519451)
        )
        assertEvalWithBudget(
          (n: BigInt) => fromBigIntLittleEndian(n, 5),
          1_000_000,
          hex"40420f0000",
          ExUnits(memory = 1233, steps = 1_519451)
        )
        assertEvalWithBudget(
          (n: BigInt) => fromBigIntLittleEndian(n, 8),
          0,
          hex"0000000000000000",
          ExUnits(memory = 1233, steps = 1_519451)
        )
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
