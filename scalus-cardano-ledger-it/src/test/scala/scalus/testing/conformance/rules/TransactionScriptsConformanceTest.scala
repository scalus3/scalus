package scalus.testing.conformance.rules

import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.conformance.ConformanceTestBase

import scala.util.{Failure, Success, Try}

/** Conformance tests for transaction scripts validation
  *
  * Tests real transactions from Amaru test fixtures using Scalus ledger rule validators.
  *
  * Validators tested:
  *   - ExactSetOfRedeemersValidator - Checks redeemers match spending/minting/etc actions
  *   - ScriptsWellFormedValidator - Validates script well-formedness
  *   - ExUnitsTooBigValidator - Checks execution units limits
  *   - MissingOrExtraScriptHashesValidator - Validates required script witnesses
  *   - MissingRequiredDatumsValidator - Checks all required datums are provided
  *   - NativeScriptsValidator - Validates native scripts execution
  */
class TransactionScriptsConformanceTest extends ConformanceTestBase:

    private val network = "preprod"

    test("Validate transactions with ExactSetOfRedeemersValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        var passed = 0
        var failed = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Create context and state
                val context = Context.testMainnet()
                val state = State()

                // Validate
                val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        val redeemerCount = transaction.witnessSet.redeemers.size
                        info(s"✓ ExactSetOfRedeemersValidator passed for $txHash")
                        info(s"  - Transaction has ${redeemerCount} redeemers")
                        passed += 1
                    case Left(error) =>
                        info(s"✗ ExactSetOfRedeemersValidator failed for $txHash: ${error.getMessage}")
                        failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    info(s"✗ Failed to run test for $txHash: ${e.getMessage}")
                    failed += 1

        info(s"ExactSetOfRedeemersValidator: $passed passed, $failed failed")
        assert(failed == 0)
    }

    test("Validate transactions with ScriptsWellFormedValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        var passed = 0
        var failed = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Create context and state
                val context = Context.testMainnet()
                val state = State()

                // Validate
                val result = ScriptsWellFormedValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        info(s"✓ ScriptsWellFormedValidator passed for $txHash")
                        passed += 1
                    case Left(error) =>
                        info(s"✗ ScriptsWellFormedValidator failed for $txHash: ${error.getMessage}")
                        failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    info(s"✗ Failed to run test for $txHash: ${e.getMessage}")
                    failed += 1

        info(s"ScriptsWellFormedValidator: $passed passed, $failed failed")
        assert(failed == 0)
    }

    test("Validate transactions with ExUnitsTooBigValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        var passed = 0
        var failed = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Create context and state
                val context = Context.testMainnet()
                val state = State()

                // Validate
                val result = ExUnitsTooBigValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        info(s"✓ ExUnitsTooBigValidator passed for $txHash")
                        passed += 1
                    case Left(error) =>
                        info(s"✗ ExUnitsTooBigValidator failed for $txHash: ${error.getMessage}")
                        failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    info(s"✗ Failed to run test for $txHash: ${e.getMessage}")
                    failed += 1

        info(s"ExUnitsTooBigValidator: $passed passed, $failed failed")
        assert(failed == 0)
    }

    test("Validate transactions with MissingOrExtraScriptHashesValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        var passed = 0
        var failed = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Build UTXO map from test context
                val utxoMap = buildUtxoMap(fixture)

                // Create context and state with UTXO map
                val context = Context.testMainnet()
                val state = State(utxos = utxoMap)

                // Validate
                val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        info(s"✓ MissingOrExtraScriptHashesValidator passed for $txHash")
                        info(s"  - All required scripts are present")
                        passed += 1
                    case Left(error) =>
                        info(s"✗ MissingOrExtraScriptHashesValidator failed for $txHash: ${error.getMessage}")
                        failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    info(s"✗ Failed to run test for $txHash: ${e.getMessage}")
                    failed += 1

        info(s"MissingOrExtraScriptHashesValidator: $passed passed, $failed failed")
        assert(failed == 0)
    }

    test("Validate transactions with MissingRequiredDatumsValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        var passed = 0
        var failed = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Build UTXO map from test context
                val utxoMap = buildUtxoMap(fixture)

                // Create context and state with UTXO map
                val context = Context.testMainnet()
                val state = State(utxos = utxoMap)

                // Validate
                val result = MissingRequiredDatumsValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        info(s"✓ MissingRequiredDatumsValidator passed for $txHash")
                        info(s"  - All required datums are provided")
                        passed += 1
                    case Left(error) =>
                        info(s"✗ MissingRequiredDatumsValidator failed for $txHash: ${error.getMessage}")
                        failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    info(s"✗ Failed to run test for $txHash: ${e.getMessage}")
                    failed += 1

        info(s"MissingRequiredDatumsValidator: $passed passed, $failed failed")
        assert(failed == 0)
    }

    test("Validate transactions with NativeScriptsValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        var passed = 0
        var failed = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Build UTXO map from test context
                val utxoMap = buildUtxoMap(fixture)

                // Create context and state with UTXO map
                val context = Context.testMainnet()
                val state = State(utxos = utxoMap)

                // Validate
                val result = NativeScriptsValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        info(s"✓ NativeScriptsValidator passed for $txHash")
                        info(s"  - Native scripts validation passed")
                        passed += 1
                    case Left(error) =>
                        info(s"✗ NativeScriptsValidator failed for $txHash: ${error.getMessage}")
                        failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    info(s"✗ Failed to run test for $txHash: ${e.getMessage}")
                    failed += 1

        info(s"NativeScriptsValidator: $passed passed, $failed failed")
        assert(failed == 0)
    }
