package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString.*
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.EvalTestKit
import scalus.uplc.eval.BuiltinException

class ByteStringTest extends AnyFunSuite with EvalTestKit:

    test("fromBigIntBigEndian"):
        assertEvalWithBudget(
          fromBigIntBigEndian(1_000_000, 3),
          hex"0f4240",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalWithBudget(
          fromBigIntBigEndian(1_000_000, 5),
          hex"00000f4240",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalWithBudget(
          fromBigIntBigEndian(0, 8),
          hex"0000000000000000",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalFails[BuiltinException](fromBigIntBigEndian(1_000_000, 1))

    test("fromBigIntLittleEndian"):
        assertEvalWithBudget(
          fromBigIntLittleEndian(1_000_000, 3),
          hex"40420f",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalWithBudget(
          fromBigIntLittleEndian(1_000_000, 5),
          hex"40420f0000",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalWithBudget(
          fromBigIntLittleEndian(0, 8),
          hex"0000000000000000",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalFails[BuiltinException](fromBigIntLittleEndian(1_000_000, 1))

    test("utf8 string interpolator"):
        // Test simple ASCII string
        assertEvalWithBudget(
          utf8"hello",
          fromString("hello"),
          ExUnits(memory = 200, steps = 16100)
        )

        // Test with special characters
        assertEvalWithBudget(
          utf8"Hello, World!",
          fromString("Hello, World!"),
          ExUnits(memory = 200, steps = 16100)
        )

        // Test with Unicode
        assertEvalWithBudget(
          utf8"Hello, 世界",
          fromString("Hello, 世界"),
          ExUnits(memory = 200, steps = 16100)
        )

        // Test empty string
        assertEvalWithBudget(utf8"", fromString(""), ExUnits(memory = 200, steps = 16100))

        // Test numbers and symbols
        assertEvalWithBudget(
          utf8"123!@#$$%^&*()",
          fromString("123!@#$%^&*()"),
          ExUnits(memory = 200, steps = 16100)
        )
