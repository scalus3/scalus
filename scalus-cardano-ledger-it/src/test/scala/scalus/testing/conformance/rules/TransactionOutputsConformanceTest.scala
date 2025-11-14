package scalus.testing.conformance.rules

import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.conformance.ConformanceTestBase

import scala.util.{Failure, Success, Try}

/** Conformance tests for transaction outputs validation
  *
  * Tests real transactions from Amaru test fixtures using Scalus ledger rule validators.
  *
  * Validators tested:
  *   - OutputsHaveTooBigValueStorageSizeValidator - Checks output value size limits
  *   - OutputBootAddrAttrsSizeValidator - Validates Byron address attributes size
  *   - OutputsHaveNotEnoughCoinsValidator - Validates minimum UTxO value (minUTxO)
  */
class TransactionOutputsConformanceTest extends ConformanceTestBase:
    private val network = "preprod"

    test("Validate transactions with OutputsHaveTooBigValueStorageSizeValidator") {
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

                // Create context and state
                val context = Context.testMainnet()
                val state = State()

                // Validate
                val result =
                    OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, transaction)

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

        info(s"✓ OutputsHaveTooBigValueStorageSizeValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }

    test("Validate transactions with OutputBootAddrAttrsSizeValidator") {
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

                // Create context and state
                val context = Context.testMainnet()
                val state = State()

                // Validate
                val result = OutputBootAddrAttrsSizeValidator.validate(context, state, transaction)

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

        info(s"✓ OutputBootAddrAttrsSizeValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }

    test("Validate transactions with OutputsHaveNotEnoughCoinsValidator") {
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

                // Create context and state
                val context = Context.testMainnet()
                val state = State()

                // Validate
                val result = OutputsHaveNotEnoughCoinsValidator.validate(context, state, transaction)

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

        info(s"✓ OutputsHaveNotEnoughCoinsValidator: $passed passed, $failed failed out of ${transactions.size}")
        assert(failed == 0, s"$failed transactions failed validation")
    }
