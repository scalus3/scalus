package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.*
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.sir.lowering.{SirToUplcV3Lowering, IntrinsicResolver}
import scalus.cardano.onchain.plutus.prelude.List
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{ToData, toData, FromData}
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
    test("dump count UPLC") {
        val compiled = PlutusV3.compile { (d: Data) =>
            val list = d.to[List[BigInt]]
            list.count(_ > 0)
        }
        println("=== UPLC ===")
        println(compiled.program.term.pretty.render(120))
        val arg = toData[List[BigInt]](List.single(BigInt(1)))
        val applied = Term.Apply(compiled.program.term, arg.asTerm)
        val r = applied.evaluateDebug
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
