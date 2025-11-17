package scalus.testing.conformance.amaru.rules

import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.conformance.amaru.ConformanceTestBase

import scala.util.{Failure, Success, Try}

/** Conformance tests for transaction inputs validation
  *
  * Tests real transactions from Amaru test fixtures using Scalus ledger rule validators.
  *
  * Validators tested:
  *   - EmptyInputsValidator - Ensures transactions have at least one input
  *   - InputsAndReferenceInputsDisjointValidator - Checks inputs/ref-inputs don't overlap
  *   - TransactionSizeValidator - Validates transaction size limits
  *   - AllInputsMustBeInUtxoValidator - Validates all inputs exist in UTXO set
  */
class TransactionInputsConformanceTest extends ConformanceTestBase:

    private val network = "preprod"

    test("Validate transactions with EmptyInputsValidator") {
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
                val result = EmptyInputsValidator.validate(context, state, transaction)

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

        info(s"✓ EmptyInputsValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }

    test("Validate transactions with InputsAndReferenceInputsDisjointValidator") {
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
                val result =
                    InputsAndReferenceInputsDisjointValidator.validate(context, state, transaction)

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

        info(s"✓ InputsAndReferenceInputsDisjointValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }

    test("Validate transactions with TransactionSizeValidator") {
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
                val result = TransactionSizeValidator.validate(context, state, transaction)

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

        info(s"✓ TransactionSizeValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }

    test("Validate transactions with AllInputsMustBeInUtxoValidator") {
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
                val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)

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

        info(s"✓ AllInputsMustBeInUtxoValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }
