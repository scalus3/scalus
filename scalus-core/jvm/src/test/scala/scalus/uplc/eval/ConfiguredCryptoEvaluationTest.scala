package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.*
import scalus.uplc.builtin.{ByteString, JVMPlatformSpecific}
import scalus.uplc.builtin.bls12_381.G1Element
import scala.language.implicitConversions

class ConfiguredCryptoEvaluationTest extends AnyFunSuite {
    private val vm = PlutusVM.makePlutusV3VM()

    for (fun, length, diagnostic) <- Seq(
          (Bls12_381_G1_uncompress, 48, "BLST_ERROR: point is not in group"),
          (Bls12_381_G2_uncompress, 96, "BLST_ERROR: point is not on curve")
        )
    do {
        test(s"$fun parser rejection retains legacy diagnostics") {
            val term = fun $ ByteString.fromHex("80" + "00" * (length - 1))
            val program = term.plutusV3.deBruijnedProgram
            val legacy = vm.evaluateScriptDebug(program).asInstanceOf[Result.Failure]
            val expected =
                s"Builtin error: $fun $term, caused by java.lang.RuntimeException: $diagnostic"
            assert(legacy.exception.getMessage == expected)
            val failure = vm
                .runWithBudgetTracking(
                  program.term,
                  new CountingBudgetSpender,
                  profiling = false,
                  tracing = true,
                  validateResult = false
                )
                .asInstanceOf[Result.Failure]
            assert(failure.exception.getMessage == expected)
            // blst's own exception reaches the result untouched: there is no longer a wrapper
            // type between the library and the failure the caller sees.
            assert(failure.exception.asInstanceOf[BuiltinError].cause.getMessage == diagnostic)
            assert(failure.budget == legacy.budget)
        }
    }

    test("a fault inside a builtin is reported as a failure, keeping its identity") {
        // A caller that asked for a result gets one, even when the builtin's implementation
        // throws something the machine does not own. `BuiltinError.cause` is that exception.
        val sentinel = new IllegalStateException("crypto provider unavailable")
        val faultyPlatform = new JVMPlatformSpecific {
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
        assert(configured.exception.asInstanceOf[BuiltinError].cause eq sentinel)
        assert(
          faultyVm
              .evaluateScriptDebug(program)
              .asInstanceOf[Result.Failure]
              .exception
              .asInstanceOf[BuiltinError]
              .cause eq sentinel
        )
    }
}
