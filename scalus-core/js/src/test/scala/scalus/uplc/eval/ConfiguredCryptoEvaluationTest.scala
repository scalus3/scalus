package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.*
import scalus.uplc.builtin.{ByteString, NodeJsPlatformSpecific}
import scalus.uplc.builtin.bls12_381.G1Element
import scala.language.implicitConversions
import scala.scalajs.js

class ConfiguredCryptoEvaluationTest extends AnyFunSuite {
    private val vm = PlutusVM.makePlutusV3VM()

    // Our own argument checks now throw IllegalArgumentException, like the JVM's `require`.
    // A rejection from Noble keeps its native identity, because nothing wraps it.
    for (group, length, uncompress, hash) <- Seq(
          ("G1", 48, Bls12_381_G1_uncompress, Bls12_381_G1_hashToGroup),
          ("G2", 96, Bls12_381_G2_uncompress, Bls12_381_G2_hashToGroup)
        )
    do {
        val short = "00" * (length - 1)
        val flags = "f0" + "00" * (length - 1)
        val point = "80" + "00" * (length - 1)
        val invalidPointMessage =
            if group == "G1" then "bad point: not in prime-order subgroup"
            else "Cannot find square root"
        val ownCheck = "java.lang.IllegalArgumentException: "
        val fromNoble = "scala.scalajs.js.JavaScriptException: Error: "
        val fixtures: Seq[(String, Term, String, String)] = Seq(
          (
            "length",
            uncompress $ ByteString.fromHex(short),
            s"Invalid length of bytes for compressed point of $group: expected $length, actual: ${length - 1}, byteString: \"$short\"",
            ownCheck
          ),
          (
            "flags",
            uncompress $ ByteString.fromHex(flags),
            s"invalid encoding for compressed zero point of $group, byteString: \"$flags\"",
            ownCheck
          ),
          ("point", uncompress $ ByteString.fromHex(point), invalidPointMessage, fromNoble),
          (
            "dst",
            hash $ ByteString.empty $ ByteString.fromHex("00" * 256),
            s"Invalid length of bytes for dst parameter of hashToGroup of $group, expected: <= 255, actual: 256",
            ownCheck
          )
        )
        fixtures.foreach { (name, term, message, causePrefix) =>
            test(s"$group $name rejects input and preserves legacy diagnostics") {
                val program = term.plutusV3.deBruijnedProgram
                val legacy = vm.evaluateScriptDebug(program).asInstanceOf[Result.Failure]
                val expectedCause = s"$causePrefix$message"
                assert(legacy.exception.asInstanceOf[BuiltinError].cause.toString == expectedCause)
                assert(legacy.exception.getMessage == s"Builtin error: ${
                        if name == "dst" then hash else uncompress
                    } $term, caused by $expectedCause")
                assert(legacy.logs.isEmpty)
                val failure = vm
                    .runWithBudgetTracking(
                      program.term,
                      new CountingBudgetSpender,
                      profiling = false,
                      tracing = true,
                      validateResult = false
                    )
                    .asInstanceOf[Result.Failure]
                assert(failure.exception.asInstanceOf[BuiltinError].cause.toString == expectedCause)
                assert(failure.exception.getMessage == legacy.exception.getMessage)
                assert(failure.budget == legacy.budget)
                assert(failure.logs.isEmpty)
            }
        }
    }

    test("an unrelated native JS crypto error is reported as a failure, keeping its identity") {
        val sentinel = js.JavaScriptException(js.Error("unexpected crypto fault"))
        val faultyPlatform = new NodeJsPlatformSpecific {
            override def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): G1Element =
                throw sentinel
        }
        val faultyVm = new PlutusVM(
          vm.language,
          vm.machineParams,
          vm.semanticVariant,
          faultyPlatform,
          vm.protocolVersion
        )
        val program =
            (Bls12_381_G1_hashToGroup $ ByteString.empty $ ByteString.empty).plutusV3.deBruijnedProgram
        val configured = faultyVm
            .runWithBudgetTracking(
              program.term,
              new CountingBudgetSpender,
              profiling = false,
              tracing = true,
              validateResult = false
            )
            .asInstanceOf[Result.Failure]
        assert(
          configured.exception
              .asInstanceOf[BuiltinError]
              .cause
              .asInstanceOf[js.JavaScriptException]
              .exception == sentinel.exception
        )
        val legacy = faultyVm.evaluateScriptDebug(program).asInstanceOf[Result.Failure]
        assert(
          legacy.exception
              .asInstanceOf[BuiltinError]
              .cause
              .asInstanceOf[js.JavaScriptException]
              .exception == sentinel.exception
        )
    }
}
