package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.*
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.sir.lowering.{IntrinsicResolver, SirToUplcV3Lowering}
import scalus.cardano.onchain.plutus.prelude.List
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.{PlutusV3, Term}
import scalus.uplc.Term.asTerm

class DumpUplcTest extends AnyFunSuite {
    given PlutusVM = PlutusVM.makePlutusV3VM()
    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
    )
    test("dump singleton toList UPLC") {
        import scalus.cardano.onchain.plutus.prelude.SortedMap
        val compiled = PlutusV3.compile { (m: SortedMap[BigInt, BigInt]) =>
            m.toList
        }
        println("=== SIR ===")
        println(compiled.sir.pretty.render(120))
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
                println(s"Result: ${s.term}")
            case f: Result.Failure =>
                println(s"FAILED: ${f.exception}")
        }
        println("=== END ===")
    }
}
