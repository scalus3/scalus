package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.ledger.api.v1.PubKeyHash
import scalus.prelude.*
import scalus.testing.kit.ScalusTest

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
                    ExUnits(memory = 32994L, steps = 10_509903)
                else ExUnits(memory = 0L, steps = 0L)
            } else
                //  Simple backend.  TODO: test for all backends
                ExUnits(memory = 233876L, steps = 61_329752L)

        val sir = compile(HelloCardano.validate)

        // val lw = sir.toLoweredValue()
        // println("lw: " + lw.show)

        val result = sir.runScript(context)
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "HelloCardanoTest.Hello Cardano",
          scalusBudget = scalusBudget,
          refBudget = ExUnits(memory = 28554L, steps = 9375627L),
          isPrintComparison = false
        )
    }
}
