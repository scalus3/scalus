package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.uplc.Term.*

/** Tests for the sliceByteString builtin.
  *
  * Plutus unlifts `from` and `len` as a machine `Int`: outside the Int64 range evaluation fails,
  * within it the result is `take len (drop from bs)`. The conformance corpus only covers small
  * arguments, so the Int64 edges are tested here.
  */
class SliceByteStringTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val bs = hex"1234567890abcdef"

    test("sliceByteString clamps a negative start and length") {
        assert(sliceByteString(-5, 3, bs) == hex"123456")
        assert(sliceByteString(2, -1, bs) == ByteString.empty)
        assert(sliceByteString(0, 8, bs) == bs)
    }

    test("sliceByteString with a start past 2^31 returns an empty bytestring") {
        // `toInt` wraps 2^31 to Int.MinValue and 2^32 to 0, which dropped nothing.
        assert(sliceByteString(BigInt(1) << 31, 1, bs) == ByteString.empty)
        assert(sliceByteString(BigInt(1) << 32, 8, bs) == ByteString.empty)
        assert(sliceByteString(Long.MaxValue, Long.MaxValue, bs) == ByteString.empty)
    }

    test("sliceByteString with a length past 2^31 takes the rest") {
        // `toInt` wraps 2^32 + 2 to 2 and Long.MaxValue to -1.
        assert(sliceByteString(4, (BigInt(1) << 32) + 2, bs) == hex"90abcdef")
        assert(sliceByteString(6, Long.MaxValue, bs) == hex"cdef")
    }

    test("sliceByteString fails outside the Int64 range") {
        val tooBig = BigInt(Long.MaxValue) + 1
        val tooSmall = BigInt(Long.MinValue) - 1
        assertThrows[BuiltinException](sliceByteString(tooBig, 1, bs))
        assertThrows[BuiltinException](sliceByteString(0, tooBig, bs))
        assertThrows[BuiltinException](sliceByteString(tooSmall, 1, bs))
        assertThrows[BuiltinException](sliceByteString(0, tooSmall, bs))
    }

    private def evalSlice(from: BigInt, len: BigInt): Result =
        Apply(
          Apply(
            Apply(Builtin(DefaultFun.SliceByteString), Const(Constant.Integer(from))),
            Const(Constant.Integer(len))
          ),
          Const(Constant.ByteString(bs))
        ).evaluateDebug

    test("sliceByteString UPLC: a start of 2^32 returns an empty bytestring") {
        evalSlice(BigInt(1) << 32, 8) match
            case Result.Success(term, _, _, _) =>
                assert(term == Const(Constant.ByteString(ByteString.empty)))
            case Result.Failure(e, _, _, _) => fail(s"Evaluation failed: $e")
    }

    test("sliceByteString UPLC: a start outside the Int64 range fails") {
        evalSlice(BigInt(Long.MaxValue) + 1, 1) match
            case Result.Success(term, _, _, _) => fail(s"Expected failure, got $term")
            case Result.Failure(e, _, _, _) =>
                e match
                    case err: BuiltinError => assert(err.cause.isInstanceOf[BuiltinException])
                    case other             => fail(s"Expected a BuiltinError, got $other")
    }
}
