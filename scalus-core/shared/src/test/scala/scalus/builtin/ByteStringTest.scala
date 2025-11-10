package scalus.builtin

import scalus.builtin.ByteString.*
import scalus.prelude.StdlibTestKit
import scalus.uplc.eval.BuiltinException

class ByteStringTest extends StdlibTestKit:

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
