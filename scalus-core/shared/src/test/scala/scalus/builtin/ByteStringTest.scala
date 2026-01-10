package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.*
import scalus.testing.kit.EvalTestKit
import scalus.uplc.eval.BuiltinException

class ByteStringTest extends AnyFunSuite with EvalTestKit:

    test("fromBigIntBigEndian"):
        fromBigIntBigEndian(1_000_000, 3).evalEq(1530707, 1401):
            hex"0f4240"
        fromBigIntBigEndian(1_000_000, 5).evalEq(1530707, 1401):
            hex"00000f4240"
        fromBigIntBigEndian(0, 8).evalEq(1530707, 1401):
            hex"0000000000000000"
        assertEvalFails[BuiltinException](1530707, 1401):
            fromBigIntBigEndian(1_000_000, 1)

    test("fromBigIntLittleEndian"):
        fromBigIntLittleEndian(1_000_000, 3).evalEq(1530707, 1401):
            hex"40420f"
        fromBigIntLittleEndian(1_000_000, 5).evalEq(1530707, 1401):
            hex"40420f0000"
        fromBigIntLittleEndian(0, 8).evalEq(1530707, 1401):
            hex"0000000000000000"
        assertEvalFails[BuiltinException](1530707, 1401):
            fromBigIntLittleEndian(1_000_000, 1)

    test("utf8 string interpolator"):
        // Test simple ASCII string
        utf8"hello".evalEq(1530707, 1401):
            fromString("hello")

        // Test with special characters
        utf8"Hello, World!".evalEq(1530707, 1401):
            fromString("Hello, World!")

        // Test with Unicode
        utf8"Hello, 世界".evalEq(1530707, 1401):
            fromString("Hello, 世界")

        // Test empty string
        utf8"".evalEq(1530707, 1401):
            fromString("")

        // Test numbers and symbols
        utf8"123!@#$$%^&*()".evalEq(1530707, 1401):
            fromString("123!@#$%^&*()")
