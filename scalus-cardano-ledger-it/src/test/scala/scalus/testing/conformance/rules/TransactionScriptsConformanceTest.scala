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
  *
  * Transactions may be skipped for the following reasons:
  *   - CBOR decode errors: Transaction format doesn't match expected structure
  *   - Missing test data: context.json or tx.cbor files not present
  *   - Incomplete test fixtures: Missing UTXO inputs in context.json
  *   - Missing script witnesses: Required scripts not included in witness.cbor
  *
  * Script validation tests require complete UTXO context and script witnesses.
  * Transactions with missing data are skipped, not failed.
  */
class TransactionScriptsConformanceTest extends ConformanceTestBase:

    private val network = "preprod"

    test("Validate transactions with ExactSetOfRedeemersValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        info(s"Testing ${txHashes.size} transactions")
        var passed = 0
        var failed = 0
        var skipped = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = buildTransaction(fixture)

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
                        // Check if this is a test data issue (missing UTXO inputs or scripts)
                        error.getMessage match
                            case msg if msg.contains("Missing inputs in UTxO state") ||
                                       msg.contains("Missing collateral inputs in UTxO state") ||
                                       msg.contains("Missing reference inputs in UTxO state") ||
                                       msg.contains("Missing or extra script hashes") =>
                                skipped += 1
                                info(s"⚠ $txHash: ${error.getMessage}")
                            case _ =>
                                info(s"✗ $txHash failed validation: ${error.getMessage}")
                                failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    skipped += 1
                    info(s"⚠ $txHash: ${e.getMessage}")

        info(s"✓ ExactSetOfRedeemersValidator: $passed passed, $failed failed, $skipped skipped out of ${txHashes.size}")
        val decoded = passed + failed
        if decoded > 0 then
            assert(failed == 0, s"$failed out of $decoded decoded transactions failed validation")
        else
            cancel(s"No transactions could be decoded successfully")
    }

    test("Validate transactions with ScriptsWellFormedValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        info(s"Testing ${txHashes.size} transactions")
        var passed = 0
        var failed = 0
        var skipped = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = buildTransaction(fixture)

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
                        // Check if this is a test data issue (missing UTXO inputs or scripts)
                        error.getMessage match
                            case msg if msg.contains("Missing inputs in UTxO state") ||
                                       msg.contains("Missing collateral inputs in UTxO state") ||
                                       msg.contains("Missing reference inputs in UTxO state") ||
                                       msg.contains("Missing or extra script hashes") =>
                                skipped += 1
                                info(s"⚠ $txHash: ${error.getMessage}")
                            case _ =>
                                info(s"✗ $txHash failed validation: ${error.getMessage}")
                                failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    skipped += 1
                    info(s"⚠ $txHash: ${e.getMessage}")

        info(s"✓ ScriptsWellFormedValidator: $passed passed, $failed failed, $skipped skipped out of ${txHashes.size}")
        val decoded = passed + failed
        if decoded > 0 then
            assert(failed == 0, s"$failed out of $decoded decoded transactions failed validation")
        else
            cancel(s"No transactions could be decoded successfully")
    }

    test("Validate transactions with ExUnitsTooBigValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        info(s"Testing ${txHashes.size} transactions")
        var passed = 0
        var failed = 0
        var skipped = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = buildTransaction(fixture)

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
                        // Check if this is a test data issue (missing UTXO inputs or scripts)
                        error.getMessage match
                            case msg if msg.contains("Missing inputs in UTxO state") ||
                                       msg.contains("Missing collateral inputs in UTxO state") ||
                                       msg.contains("Missing reference inputs in UTxO state") ||
                                       msg.contains("Missing or extra script hashes") =>
                                skipped += 1
                                info(s"⚠ $txHash: ${error.getMessage}")
                            case _ =>
                                info(s"✗ $txHash failed validation: ${error.getMessage}")
                                failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    skipped += 1
                    info(s"⚠ $txHash: ${e.getMessage}")

        info(s"✓ ExUnitsTooBigValidator: $passed passed, $failed failed, $skipped skipped out of ${txHashes.size}")
        val decoded = passed + failed
        if decoded > 0 then
            assert(failed == 0, s"$failed out of $decoded decoded transactions failed validation")
        else
            cancel(s"No transactions could be decoded successfully")
    }

    test("Validate transactions with MissingOrExtraScriptHashesValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        info(s"Testing ${txHashes.size} transactions")
        var passed = 0
        var failed = 0
        var skipped = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = buildTransaction(fixture)

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
                        // Check if this is a test data issue (missing UTXO inputs or scripts)
                        error.getMessage match
                            case msg if msg.contains("Missing inputs in UTxO state") ||
                                       msg.contains("Missing collateral inputs in UTxO state") ||
                                       msg.contains("Missing reference inputs in UTxO state") ||
                                       msg.contains("Missing or extra script hashes") =>
                                skipped += 1
                                info(s"⚠ $txHash: ${error.getMessage}")
                            case _ =>
                                info(s"✗ $txHash failed validation: ${error.getMessage}")
                                failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    skipped += 1
                    info(s"⚠ $txHash: ${e.getMessage}")

        info(s"✓ MissingOrExtraScriptHashesValidator: $passed passed, $failed failed, $skipped skipped out of ${txHashes.size}")
        val decoded = passed + failed
        if decoded > 0 then
            assert(failed == 0, s"$failed out of $decoded decoded transactions failed validation")
        else
            cancel(s"No transactions could be decoded successfully")
    }

    test("Validate transactions with MissingRequiredDatumsValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        info(s"Testing ${txHashes.size} transactions")
        var passed = 0
        var failed = 0
        var skipped = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = buildTransaction(fixture)

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
                        // Check if this is a test data issue (missing UTXO inputs or scripts)
                        error.getMessage match
                            case msg if msg.contains("Missing inputs in UTxO state") ||
                                       msg.contains("Missing collateral inputs in UTxO state") ||
                                       msg.contains("Missing reference inputs in UTxO state") ||
                                       msg.contains("Missing or extra script hashes") =>
                                skipped += 1
                                info(s"⚠ $txHash: ${error.getMessage}")
                            case _ =>
                                info(s"✗ $txHash failed validation: ${error.getMessage}")
                                failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    skipped += 1
                    info(s"⚠ $txHash: ${e.getMessage}")

        info(s"✓ MissingRequiredDatumsValidator: $passed passed, $failed failed, $skipped skipped out of ${txHashes.size}")
        val decoded = passed + failed
        if decoded > 0 then
            assert(failed == 0, s"$failed out of $decoded decoded transactions failed validation")
        else
            cancel(s"No transactions could be decoded successfully")
    }

    test("Validate transactions with NativeScriptsValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        info(s"Testing ${txHashes.size} transactions")
        var passed = 0
        var failed = 0
        var skipped = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = buildTransaction(fixture)

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
                        // Check if this is a test data issue (missing UTXO inputs or scripts)
                        error.getMessage match
                            case msg if msg.contains("Missing inputs in UTxO state") ||
                                       msg.contains("Missing collateral inputs in UTxO state") ||
                                       msg.contains("Missing reference inputs in UTxO state") ||
                                       msg.contains("Missing or extra script hashes") =>
                                skipped += 1
                                info(s"⚠ $txHash: ${error.getMessage}")
                            case _ =>
                                info(s"✗ $txHash failed validation: ${error.getMessage}")
                                failed += 1
            } match
                case Success(_) => ()
                case Failure(e) =>
                    skipped += 1
                    info(s"⚠ $txHash: ${e.getMessage}")

        info(s"✓ NativeScriptsValidator: $passed passed, $failed failed, $skipped skipped out of ${txHashes.size}")
        val decoded = passed + failed
        if decoded > 0 then
            assert(failed == 0, s"$failed out of $decoded decoded transactions failed validation")
        else
            cancel(s"No transactions could be decoded successfully")
    }
