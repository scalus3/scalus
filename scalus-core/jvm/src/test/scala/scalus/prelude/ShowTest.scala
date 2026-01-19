package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.compiler.{compileWithOptions, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.testing.kit.EvalTestKit
import scalus.uplc.Term.asTerm
import scalus.toUplc

class ShowTest extends AnyFunSuite with EvalTestKit {

    test("Show[Unit] is ()") {
        assertEvalEq(().show, "()")
    }

    test("Show[Boolean] is correct") {
        assertEvalEq(true.show, "True")
        assertEvalEq(false.show, "False")
    }

    test("Show[ByteString] is a hex string") {
        assertEvalEq(hex"00112233".show, "\"00112233\"")
    }

    test("Show[String] is correct") {
        assertEvalEq("Hello, World!".show, "\"Hello, World!\"")
        assertEvalEq("".show, "\"\"")
    }

    test("Show[BigInt] is correct") {
        assertEvalEq(BigInt(0).show, "0")
        assertEvalEq(BigInt(123456789).show, "123456789")
        assertEvalEq(BigInt(-123456789).show, "-123456789")
    }

    test("Show[Data] is correct") {
        assertEvalEq(BigInt(0).toData.show, "0")
        assertEvalEq(hex"0011".toData.show, "\"0011\"")
        assertEvalEq(
          List(BigInt(0).toData, hex"0011".toData).toData.show,
          "[0, \"0011\"]"
        )
        given options: Options = Options.default
            .copy(targetLoweringBackend = TargetLoweringBackend.SumOfProductsLowering)
        val sir = compileWithOptions(
          options,
          AssocMap(List((BigInt(0).toData, hex"0011".toData))).toData.show
        )
        val uplc = sir.toUplc(optimizeUplc = true)
        val result = uplc.evaluateDebug
        assert(result.success.term == "{0: \"0011\"}".asTerm)
        assert(result.budget == ExUnits(70865, 15662246))
        assertEvalEq(
          Rational(1, 2).toData.show,
          "<0, [1, 2]>" // Rational is represented as a pair of BigInts
        )
    }

}
