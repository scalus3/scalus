package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v3.PubKeyHash
import scalus.cardano.onchain.plutus.prelude.*
import scalus.testing.budget.{BudgetPinsSuite, BudgetTable}
import scalus.testing.kit.ScalusTest

import scala.language.implicitConversions

class HelloCardanoTest extends AnyFunSuite with ScalusTest with BudgetPinsSuite {
    override protected def budgetTable: BudgetTable = HelloCardanoTestBudgets

    // Lowering of the prelude's `===` macro-spliced application trips the
    // not-provably-default Eq heuristic; the warning is a known false positive here.
    private given scalus.compiler.Options = scalus.compiler.Options.default.copy(noWarn = true)

    private val contract = HelloCardanoContract.compiled.withErrorTraces

    test("Hello Cardano script size") {
        pinSize(contract.script.script.size)
    }

    test("Hello Cardano") {
        val ownerPubKey = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val message = "Hello, World!".toData
        val context = makeSpendingScriptContext(
          datum = ownerPubKey.toData,
          redeemer = message,
          signatories = List(ownerPubKey)
        )

        // Using HelloCardanoContract.compiled.withErrorTraces (Options.release with generateErrorTraces=true)
        val applied = contract.program $ context.toData
        val result = applied.evaluateDebug
        assertSuccess(result, contract.program)

        // Reference budget of the original benchmark: ExUnits(memory = 28554L, steps = 9375627L)
    }
}
