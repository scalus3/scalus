package scalus.testing.conformance.rules

import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.conformance.ConformanceTestBase

import scala.util.{Failure, Success, Try}

/** Conformance tests for transaction metadata and other validations
  *
  * Tests real transactions from Amaru test fixtures using Scalus ledger rule validators.
  *
  * Validators tested:
  *   - MetadataValidator - Validates auxiliary data and metadata
  *   - OutsideValidityIntervalValidator - Checks transaction validity intervals
  *   - WrongNetworkInTxBodyValidator - Validates network IDs in transaction body
  *   - VerifiedSignaturesInWitnessesValidator - Validates VKey witness signatures
  *   - MissingKeyHashesValidator - Checks all required key hashes are present
  *   - WrongNetworkValidator - Validates network IDs in addresses
  *   - WrongNetworkWithdrawalValidator - Validates network IDs in withdrawals
  *   - OutsideForecastValidator - Checks slot is within forecast range
  *   - ProtocolParamsViewHashesMatchValidator - Validates protocol parameter view hashes
  */
class TransactionMetadataConformanceTest extends ConformanceTestBase:

    private val network = "preprod"

    test("Validate transactions with MetadataValidator") {
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
                val result = MetadataValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                        val hasMetadata = transaction.auxiliaryData.isDefined
                        info(s"✓ [$txHash] MetadataValidator passed (metadata: ${hasMetadata})")
                    case Left(error) =>
                        failed += 1
                        info(s"✗ [$txHash] MetadataValidator failed: ${error.getMessage}")
            } match
                case Success(_) => ()
                case Failure(e) =>
                    failed += 1
                    info(s"✗ [$txHash] Exception: ${e.getMessage}")

        info(s"\n=== MetadataValidator Summary ===")
        info(s"Passed: $passed, Failed: $failed, Total: ${txHashes.length}")
        assert(failed == 0, s"MetadataValidator failed for $failed transactions")
    }

    test("Validate transactions with OutsideValidityIntervalValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        var passed = 0
        var failed = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Create context and state with a slot number
                val context = Context.testMainnet(slot = 50000000)
                val state = State()

                // Validate
                val result = OutsideValidityIntervalValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                        info(s"✓ [$txHash] OutsideValidityIntervalValidator passed (TTL: ${transaction.body.value.ttl}, start: ${transaction.body.value.validityStartSlot})")
                    case Left(error) =>
                        failed += 1
                        info(s"✗ [$txHash] OutsideValidityIntervalValidator failed: ${error.getMessage}")
            } match
                case Success(_) => ()
                case Failure(e) =>
                    failed += 1
                    info(s"✗ [$txHash] Exception: ${e.getMessage}")

        info(s"\n=== OutsideValidityIntervalValidator Summary ===")
        info(s"Passed: $passed, Failed: $failed, Total: ${txHashes.length}")
        assert(failed == 0, s"OutsideValidityIntervalValidator failed for $failed transactions")
    }

    test("Validate transactions with WrongNetworkInTxBodyValidator") {
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
                val result = WrongNetworkInTxBodyValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                        info(s"✓ [$txHash] WrongNetworkInTxBodyValidator passed")
                    case Left(error) =>
                        failed += 1
                        info(s"✗ [$txHash] WrongNetworkInTxBodyValidator failed: ${error.getMessage}")
            } match
                case Success(_) => ()
                case Failure(e) =>
                    failed += 1
                    info(s"✗ [$txHash] Exception: ${e.getMessage}")

        info(s"\n=== WrongNetworkInTxBodyValidator Summary ===")
        info(s"Passed: $passed, Failed: $failed, Total: ${txHashes.length}")
        assert(failed == 0, s"WrongNetworkInTxBodyValidator failed for $failed transactions")
    }

    test("Validate transactions with VerifiedSignaturesInWitnessesValidator") {
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
                val result = VerifiedSignaturesInWitnessesValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                        info(s"✓ [$txHash] VerifiedSignaturesInWitnessesValidator passed")
                    case Left(error) =>
                        failed += 1
                        info(s"✗ [$txHash] VerifiedSignaturesInWitnessesValidator failed: ${error.getMessage}")
            } match
                case Success(_) => ()
                case Failure(e) =>
                    failed += 1
                    info(s"✗ [$txHash] Exception: ${e.getMessage}")

        info(s"\n=== VerifiedSignaturesInWitnessesValidator Summary ===")
        info(s"Passed: $passed, Failed: $failed, Total: ${txHashes.length}")
        assert(failed == 0, s"VerifiedSignaturesInWitnessesValidator failed for $failed transactions")
    }

    test("Validate transactions with MissingKeyHashesValidator") {
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
                val result = MissingKeyHashesValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                        info(s"✓ [$txHash] MissingKeyHashesValidator passed")
                    case Left(error) =>
                        failed += 1
                        info(s"✗ [$txHash] MissingKeyHashesValidator failed: ${error.getMessage}")
            } match
                case Success(_) => ()
                case Failure(e) =>
                    failed += 1
                    info(s"✗ [$txHash] Exception: ${e.getMessage}")

        info(s"\n=== MissingKeyHashesValidator Summary ===")
        info(s"Passed: $passed, Failed: $failed, Total: ${txHashes.length}")
        assert(failed == 0, s"MissingKeyHashesValidator failed for $failed transactions")
    }

    test("Validate transactions with WrongNetworkValidator") {
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
                val result = WrongNetworkValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                        info(s"✓ [$txHash] WrongNetworkValidator passed")
                    case Left(error) =>
                        failed += 1
                        info(s"✗ [$txHash] WrongNetworkValidator failed: ${error.getMessage}")
            } match
                case Success(_) => ()
                case Failure(e) =>
                    failed += 1
                    info(s"✗ [$txHash] Exception: ${e.getMessage}")

        info(s"\n=== WrongNetworkValidator Summary ===")
        info(s"Passed: $passed, Failed: $failed, Total: ${txHashes.length}")
        assert(failed == 0, s"WrongNetworkValidator failed for $failed transactions")
    }

    test("Validate transactions with WrongNetworkWithdrawalValidator") {
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
                val result = WrongNetworkWithdrawalValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                        info(s"✓ [$txHash] WrongNetworkWithdrawalValidator passed")
                    case Left(error) =>
                        failed += 1
                        info(s"✗ [$txHash] WrongNetworkWithdrawalValidator failed: ${error.getMessage}")
            } match
                case Success(_) => ()
                case Failure(e) =>
                    failed += 1
                    info(s"✗ [$txHash] Exception: ${e.getMessage}")

        info(s"\n=== WrongNetworkWithdrawalValidator Summary ===")
        info(s"Passed: $passed, Failed: $failed, Total: ${txHashes.length}")
        assert(failed == 0, s"WrongNetworkWithdrawalValidator failed for $failed transactions")
    }

    test("Validate transactions with OutsideForecastValidator") {
        requireLedgerRulesData()

        val txHashes = getAvailableTransactionTests(network)
        var passed = 0
        var failed = 0

        for txHash <- txHashes do
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = Transaction.fromCbor(fixture.txCbor)

                // Create context and state with special slot parameter
                val context = Context.testMainnet(slot = 50000000)
                val state = State()

                // Validate
                val result = OutsideForecastValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                        info(s"✓ [$txHash] OutsideForecastValidator passed")
                    case Left(error) =>
                        failed += 1
                        info(s"✗ [$txHash] OutsideForecastValidator failed: $error")
            } match
                case Success(_) => ()
                case Failure(e) =>
                    failed += 1
                    info(s"✗ [$txHash] Exception: ${e.getMessage}")

        info(s"\n=== OutsideForecastValidator Summary ===")
        info(s"Passed: $passed, Failed: $failed, Total: ${txHashes.length}")
        assert(failed == 0, s"OutsideForecastValidator failed for $failed transactions")
    }

    test("Validate transactions with ProtocolParamsViewHashesMatchValidator") {
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
                val result = ProtocolParamsViewHashesMatchValidator.validate(context, state, transaction)

                result match
                    case Right(_) =>
                        passed += 1
                        info(s"✓ [$txHash] ProtocolParamsViewHashesMatchValidator passed")
                    case Left(error) =>
                        failed += 1
                        info(s"✗ [$txHash] ProtocolParamsViewHashesMatchValidator failed: ${error.getMessage}")
            } match
                case Success(_) => ()
                case Failure(e) =>
                    failed += 1
                    info(s"✗ [$txHash] Exception: ${e.getMessage}")

        info(s"\n=== ProtocolParamsViewHashesMatchValidator Summary ===")
        info(s"Passed: $passed, Failed: $failed, Total: ${txHashes.length}")
        assert(failed == 0, s"ProtocolParamsViewHashesMatchValidator failed for $failed transactions")
    }
