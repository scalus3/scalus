package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.TargetLoweringBackend
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.ledger.api.v1.PubKeyHash
import scalus.prelude.*
import scalus.testing.kit.ScalusTest
import scalus.uplc.PlutusV3

import scala.language.implicitConversions

class HelloCardanoTest extends AnyFunSuite with ScalusTest {

    given Compiler.Options = Compiler.Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
    )

    private val contract = PlutusV3.compile(HelloCardano.validate)

    test(s"Hello Cardano script size is ${contract.script.script.size} bytes") {
        assert(contract.script.script.size == 626)
    }

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
            if compilerOptions.targetLoweringBackend == TargetLoweringBackend.SirToUplcV3Lowering
            then {
                // S3 lowering backend with lambda barriers (safer optimization)
                if compilerOptions.generateErrorTraces then
                    ExUnits(memory = 31394L, steps = 10_253903)
                else ExUnits(memory = 31094L, steps = 10_205903L)
            } else
                //  Simple backend.  TODO: test for all backends
                ExUnits(memory = 233876L, steps = 61_329752L)

        val applied = contract.program $ context.toData
        val result = applied.evaluateDebug
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
