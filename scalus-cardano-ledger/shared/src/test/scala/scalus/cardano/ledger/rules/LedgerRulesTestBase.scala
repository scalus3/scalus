package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.TransactionException
import scalus.cardano.ledger.*
import scalus.cardano.node.{EmulatorBase, SubmitError}

/** Shared test base for ledger rule validation tests.
  *
  * This trait contains all the test logic that runs against the Emulator. Platform-specific
  * implementations (JVM/JS) extend this trait and provide:
  *   - `createEmulator`: Factory method for platform-specific Emulator
  *   - `generateKeyPair`: Platform-specific Ed25519 key generation
  */
trait LedgerRulesTestBase extends AnyFunSuite with ArbitraryInstances {

    def makeEmulator(
        initialUtxos: Utxos = Map.empty,
        initialContext: Context = Context.testMainnet(),
        validators: Seq[STS.Validator],
        mutators: Seq[STS.Mutator]
    ): EmulatorBase

    test("AllInputsMustBeInUtxo") {
        val tests = AllInputsMustBeInUtxoValidatorTest.allTests

        tests.foreach(
          _.runAgainst(
            AllInputsMustBeInUtxoValidator,
            (utxos, ctx, validator) => makeEmulator(utxos, ctx, Seq(validator), Seq.empty)
          )
        )
    }
}

case class TestCase(
    state: State,
    context: Context,
    tx: Transaction,
    assertion: Either[SubmitError, TransactionHash] => Assertion
) {

    type EmulatorFactory = (Utxos, Context, STS.Validator) => EmulatorBase

    def runAgainst(validator: STS.Validator, emulatorFactory: EmulatorFactory) = {
        val emulator = emulatorFactory(state.utxos, context, validator)
        val result = emulator.submitSync(tx)
        assertion(result)
    }
}
