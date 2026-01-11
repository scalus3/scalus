package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.ledger.api.v1.PubKeyHash
import scalus.prelude.*
import scalus.testing.kit.ScalusTest

import scala.language.implicitConversions

class HelloCardanoTest extends AnyFunSuite with ScalusTest {

    private val contract = HelloCardanoContract.withErrorTraces

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

        // Using HelloCardanoContract.withErrorTraces (Options.release with generateErrorTraces=true)
        val scalusBudget = ExUnits(memory = 31394L, steps = 10_253903)

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
