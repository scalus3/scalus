package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.*
import scalus.compiler.sir.TargetLoweringBackend
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.PlutusV3

class DumpUplcTest extends AnyFunSuite {
    given PlutusVM = PlutusVM.makePlutusV3VM()
    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
    )
    ignore("dump minting singleton UPLC") {
        val compiled = PlutusV3.compile {
            SortedMap.singleton(hex"484f534b59", BigInt("1000000000000000"))
        }
        println("=== LOWERED ===")
        val lowered = scalus.toLoweredValue(compiled.sir)(true, false)
        println(lowered.show)
        println("=== UPLC ===")
        println(compiled.program.term.pretty.render(120))
        val r = compiled.program.term.evaluateDebug
        println("=== EVAL ===")
        r match {
            case s: Result.Success =>
                println(s"Budget: ${s.budget}")
            case f: Result.Failure =>
                println(s"FAILED: ${f.exception}")
        }
        println("=== END ===")
    }
}
