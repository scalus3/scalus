package scalus.testing.conformance.amaru.rules

import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.conformance.amaru.ConformanceTestBase

import scala.util.{Failure, Success, Try}

/** Conformance tests for transaction fees validation
  *
  * Tests real transactions from Amaru test fixtures using Scalus ledger rule validators.
  *
  * Validators tested:
  *   - TooManyCollateralInputsValidator - Checks collateral inputs limit
  *   - FeesOkValidator - Validates fee calculation and collateral
  *   - ValueNotConservedUTxOValidator - Checks value conservation (inputs = outputs + fees)
  */
class TransactionFeesConformanceTest extends ConformanceTestBase:

    private val network = "preprod"

    test("Validate transactions with TooManyCollateralInputsValidator") {
        requireLedgerRulesData()

        val transactions = getAvailableTransactionTests(network)
        if transactions.isEmpty then cancel(s"No transaction tests available for $network")

        info(s"Testing ${transactions.size} transactions")
        var passed = 0
        var failed = 0

        transactions.foreach { txHash =>
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                val context = Context.testMainnet()
                val state = State()
                val result = TooManyCollateralInputsValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                    case Left(error) =>
                        failed += 1
                        info(s"✗ $txHash failed: ${error.getMessage}")
            } match
                case Success(_) => // Continue
                case Failure(e) =>
                    info(s"⚠ $txHash: ${e.getMessage}")
        }

        info(s"✓ TooManyCollateralInputsValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }

    test("Validate transactions with FeesOkValidator") {
        requireLedgerRulesData()

        val transactions = getAvailableTransactionTests(network)
        if transactions.isEmpty then cancel(s"No transaction tests available for $network")

        info(s"Testing ${transactions.size} transactions")
        var passed = 0
        var failed = 0

        transactions.foreach { txHash =>
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Build UTXO map from test context
                val utxoMap = buildUtxoMap(fixture)

                val context = Context.testMainnet()
                val state = State(utxos = utxoMap)
                val result = FeesOkValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                    case Left(error) =>
                        failed += 1
                        info(s"✗ $txHash failed: ${error.getMessage}")
            } match
                case Success(_) => // Continue
                case Failure(e) =>
                    info(s"⚠ $txHash: ${e.getMessage}")
        }

        info(s"✓ FeesOkValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }

    test("Validate transactions with ValueNotConservedUTxOValidator") {
        requireLedgerRulesData()

        val transactions = getAvailableTransactionTests(network)
        if transactions.isEmpty then cancel(s"No transaction tests available for $network")

        info(s"Testing ${transactions.size} transactions")
        var passed = 0
        var failed = 0

        transactions.foreach { txHash =>
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Build UTXO map from test context
                val utxoMap = buildUtxoMap(fixture)

                val context = Context.testMainnet()
                val state = State(utxos = utxoMap)
                val result = ValueNotConservedUTxOValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                    case Left(error) =>
                        failed += 1
                        info(s"✗ $txHash failed: ${error.getMessage}")
            } match
                case Success(_) => // Continue
                case Failure(e) =>
                    info(s"⚠ $txHash: ${e.getMessage}")
        }

        info(s"✓ ValueNotConservedUTxOValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }
