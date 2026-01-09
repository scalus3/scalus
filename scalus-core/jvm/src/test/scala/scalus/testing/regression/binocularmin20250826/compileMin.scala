package scalus.testing.regression.binocularmin20250826

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.*
import scalus.compiler.{compile, Options, TargetLoweringBackend}
import scalus.uplc.Program

class CompileMin extends AnyFunSuite {

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("compile call of byteStringToInteger to UPLC") {
        // val sir = scalus.compiler.compile(BitcoinValidator.validate)
        val sir = compile {

            // val powLimit: BigInt =
            byteStringToInteger(
              true,
              hex"00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            )
            // powLimit
        }
        //    println(sir.showHighlighted)
        sir.toUplcOptimized().plutusV3
    }

}
