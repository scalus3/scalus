package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.*
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.EvalTestKit
import scalus.uplc.eval.BuiltinException

class ByteStringTest extends AnyFunSuite with EvalTestKit:

    test("fromBigIntBigEndian"):
        assertEvalWithinBudget(
          fromBigIntBigEndian(1_000_000, 3),
          hex"0f4240",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalWithinBudget(
          fromBigIntBigEndian(1_000_000, 5),
          hex"00000f4240",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalWithinBudget(
          fromBigIntBigEndian(0, 8),
          hex"0000000000000000",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalFailsWithinBudget[BuiltinException](
          fromBigIntBigEndian(1_000_000, 1),
          ExUnits(memory = 1401, steps = 1530707)
        )

    test("fromBigIntLittleEndian"):
        assertEvalWithinBudget(
          fromBigIntLittleEndian(1_000_000, 3),
          hex"40420f",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalWithinBudget(
          fromBigIntLittleEndian(1_000_000, 5),
          hex"40420f0000",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalWithinBudget(
          fromBigIntLittleEndian(0, 8),
          hex"0000000000000000",
          ExUnits(memory = 1401, steps = 1530707)
        )
        assertEvalFailsWithinBudget[BuiltinException](
          fromBigIntLittleEndian(1_000_000, 1),
          ExUnits(memory = 1401, steps = 1530707)
        )

    test("utf8 string interpolator"):
        // Test simple ASCII string
        assertEvalWithinBudget(
          utf8"hello",
          fromString("hello"),
          ExUnits(memory = 1401, steps = 1530707)
        )

        // Test with special characters
        assertEvalWithinBudget(
          utf8"Hello, World!",
          fromString("Hello, World!"),
          ExUnits(memory = 1401, steps = 1530707)
        )

        // Test with Unicode
        assertEvalWithinBudget(
          utf8"Hello, 世界",
          fromString("Hello, 世界"),
          ExUnits(memory = 1401, steps = 1530707)
        )

        // Test empty string
        assertEvalWithinBudget(utf8"", fromString(""), ExUnits(memory = 1401, steps = 1530707))

        // Test numbers and symbols
        assertEvalWithinBudget(
          utf8"123!@#$$%^&*()",
          fromString("123!@#$%^&*()"),
          ExUnits(memory = 1401, steps = 1530707)
        )
