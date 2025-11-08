package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.ledger.api.v1.PubKeyHash
import scalus.prelude.*
import scalus.testing.kit.ScalusTest
import scalus.uplc.*
import scalus.uplc.eval.*

import scala.language.implicitConversions

class HelloCardanoTest extends AnyFunSuite with ScalusTest {

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("Hello Cardano") {
        val ownerPubKey = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val message = "Hello, World!".toData
        val context = makeSpendingScriptContext(
          datum = ownerPubKey.toData,
          redeemer = message,
          signatories = List(ownerPubKey)
        )

        val compilerOptions = summon[Compiler.Options]
        val scalusBudget =
            if compilerOptions.targetLoweringBackend == Compiler.TargetLoweringBackend.SirToUplcV3Lowering
            then {
                // S3 lowering backend with lambda barriers (safer optimization)
                if compilerOptions.generateErrorTraces then
                    ExBudget(ExCPU(10653903L), ExMemory(33894L))
                else ExBudget(ExCPU(0L), ExMemory(0L))
            } else
                //  Simple backend.  TODO: test for all backends
                ExBudget(ExCPU(61_329752L), ExMemory(233876L))

        val sir = compile(HelloCardano.validate)

        // val lw = sir.toLoweredValue()
        // println("lw: " + lw.show)

        val result = sir.runScript(context)
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "HelloCardanoTest.Hello Cardano",
          scalusBudget = scalusBudget,
          refBudget = ExBudget(ExCPU(9375627L), ExMemory(28554L)),
          isPrintComparison = false
        )
    }
}
