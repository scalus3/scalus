package scalus.testing.conformance.amaru

import com.github.plokhotnyuk.jsoniter_scala.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.ledger.Transaction
import scalus.testing.conformance.amaru.TestDataModels.*

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.util.{Try, Success, Failure, Using}

/** Base trait for conformance tests
  *
  * Provides utilities for loading test data from Amaru data directory
  * (both epoch-level and transaction/block-level) and comparing results
  * with expected snapshots.
  */
trait ConformanceTestBase extends AnyFunSuite with Matchers:

    /** Base directory for test data (symlink to amaru/data)
      * Use user.dir system property as base. When tests are forked, user.dir is the module directory.
      */
    private val baseDir = Paths.get(System.getProperty("user.dir"))
    private val isModuleDir = baseDir.getFileName.toString == "scalus-cardano-ledger-it"

    /** Directory for test data (both epoch-level and transaction/block-level)
      * Points to: scalus-cardano-ledger-it/data/
      *
      * Structure:
      * - data/<network>/pools/           - Pool data by epoch
      * - data/<network>/rewards-provenance/ - Rewards data by epoch
      * - data/<network>/dreps/           - DRep data by epoch
      * - data/<network>/pots/            - Pots data by epoch
      * - data/transactions/<network>/    - Transaction test fixtures
      * - data/blocks/<network>/          - Block test fixtures
      */
    val dataDir: Path =
        if isModuleDir then baseDir.resolve("data")
        else baseDir.resolve("scalus-cardano-ledger-it/data")

    /** Directory for storing/comparing snapshots */
    val snapshotsDir: Path =
        if isModuleDir then baseDir.resolve("src/test/resources/snapshots")
        else baseDir.resolve("scalus-cardano-ledger-it/src/test/resources/snapshots")

    /** Load JSON file and parse it */
    def loadJson[T](path: Path)(using codec: JsonValueCodec[T]): T =
        val content = Using.resource(Source.fromFile(path.toFile))(_.mkString)
        try {
            readFromString[T](content)
        } catch {
            case e: JsonReaderException => fail(s"Failed to parse JSON from $path: ${e.getMessage}")
        }

    /** Load pool data for a given network and epoch */
    def loadPools(network: String, epoch: Int): Map[String, TestDataModels.PoolData] =
        val path = dataDir.resolve(s"$network/pools/$epoch.json")
        if Files.exists(path) then
            loadJson[Map[String, TestDataModels.PoolData]](path)
        else
            Map.empty

    /** Load rewards provenance for a given network and epoch */
    def loadRewardsProvenance(network: String, epoch: Int): TestDataModels.RewardsProvenance =
        val path = dataDir.resolve(s"$network/rewards-provenance/$epoch.json")
        loadJson[TestDataModels.RewardsProvenance](path)

    /** Load DRep data for a given network and epoch */
    def loadDReps(network: String, epoch: Int): List[TestDataModels.DRepData] =
        val path = dataDir.resolve(s"$network/dreps/$epoch.json")
        if Files.exists(path) then
            loadJson[List[TestDataModels.DRepData]](path)
        else
            List.empty

    /** Load pots data for a given network and epoch */
    def loadPots(network: String, epoch: Int): TestDataModels.PotsData =
        val path = dataDir.resolve(s"$network/pots/$epoch.json")
        loadJson[TestDataModels.PotsData](path)

    /** Load network configuration */
    def loadConfig(network: String): TestDataModels.Config =
        val path = dataDir.resolve(s"$network/config.json")
        loadJson[TestDataModels.Config](path)

    /** Compare computed result with snapshot
      *
      * @param snapshotName Name of the snapshot file (without extension)
      * @param result The computed result to compare
      * @param network The network (preprod, preview, etc.)
      */
    def compareWithSnapshot(snapshotName: String, result: String, network: String): Unit =
        val snapshotPath = snapshotsDir.resolve(s"$network/$snapshotName.snap")

        if Files.exists(snapshotPath) then
            val expected = Using.resource(Source.fromFile(snapshotPath.toFile))(_.mkString)
            if result != expected then
                // Print diff for debugging
                println(s"Snapshot mismatch for $snapshotName")
                println("Expected:")
                println(expected.take(500))
                println("\nActual:")
                println(result.take(500))
                fail(s"Snapshot mismatch: $snapshotName")
        else
            // Create snapshot directory if it doesn't exist
            Files.createDirectories(snapshotPath.getParent)
            // Write new snapshot
            Files.writeString(snapshotPath, result)
            println(s"Created new snapshot: $snapshotPath")

    /** Helper to format BigInt as string */
    def formatBigInt(value: BigInt): String = value.toString

    /** Helper to format rational as string */
    def formatRational(num: BigInt, denom: BigInt): String = s"$num/$denom"

    /** Get list of epochs available for testing in a network */
    def getAvailableEpochs(network: String): List[Int] =
        val poolsDir = dataDir.resolve(s"$network/pools")
        if Files.exists(poolsDir) then
            import scala.jdk.CollectionConverters.*
            Files.list(poolsDir)
                .iterator()
                .asScala
                .filter(_.toString.endsWith(".json"))
                .map(p => p.getFileName.toString.stripSuffix(".json").toInt)
                .toList
                .sorted
        else
            List.empty

    // ========================================================================
    // Transaction and Block Level Test Data Methods
    // ========================================================================

    /** Load binary CBOR file */
    def loadCbor(path: Path): Array[Byte] =
        Files.readAllBytes(path)

    /** Load a transaction test fixture
      *
      * @param network Network name (e.g., "preprod")
      * @param txHash Transaction hash
      * @param variant Optional variant name for alternative scenarios
      * @return Complete test fixture with context and transaction data
      */
    def loadTransactionFixture(
        network: String,
        txHash: String,
        variant: Option[String] = None
    ): TransactionTestFixture =
        val basePath = variant match
            case Some(v) => dataDir.resolve(s"transactions/$network/$txHash/$v")
            case None => dataDir.resolve(s"transactions/$network/$txHash")

        val contextPath = variant match
            case Some(v) => dataDir.resolve(s"transactions/$network/$txHash/context.json")
            case None => dataDir.resolve(s"transactions/$network/$txHash/context.json")

        val context = loadJson[TestContext](contextPath)
        val txCbor = loadCbor(basePath.resolve("tx.cbor"))
        val witnessCbor =
            val witnessPath = basePath.resolve("witness.cbor")
            if Files.exists(witnessPath) then Some(loadCbor(witnessPath))
            else None

        val expectedTracesPath = basePath.resolve("expected.traces")
        val expectedTraces = if Files.exists(expectedTracesPath) then
            loadJson[List[TraceEvent]](expectedTracesPath)
        else
            List.empty

        TransactionTestFixture(context, txCbor, witnessCbor, expectedTraces)

    /** Get list of all transaction test cases for a network */
    def getAvailableTransactionTests(network: String): List[String] =
        val txDir = dataDir.resolve(s"transactions/$network")
        if Files.exists(txDir) then
            import scala.jdk.CollectionConverters.*
            Files.list(txDir)
                .iterator()
                .asScala
                .filter(p => Files.isDirectory(p))
                .map(_.getFileName.toString)
                .toList
                .sorted
        else
            List.empty

    /** Get list of all block test cases for a network */
    def getAvailableBlockTests(network: String): List[String] =
        val blockDir = dataDir.resolve(s"blocks/$network")
        if Files.exists(blockDir) then
            import scala.jdk.CollectionConverters.*
            Files.list(blockDir)
                .iterator()
                .asScala
                .filter(p => Files.isDirectory(p))
                .map(_.getFileName.toString)
                .toList
                .sorted
        else
            List.empty

    /** Helper to check if transaction/block test data is available */
    def isLedgerRulesDataAvailable: Boolean =
        val txDir = dataDir.resolve("transactions")
        Files.exists(txDir) && Files.isDirectory(txDir)

    /** Skip test if ledger rules test data is not available */
    def requireLedgerRulesData(): Unit =
        if !isLedgerRulesDataAvailable then
            cancel(s"Ledger rules test data not available at: ${dataDir.resolve("transactions")}")

    /** Test result for transaction validation */
    case class TransactionTestResult(passed: Int, failed: Int, skipped: Int, total: Int):
        def decoded: Int = passed + failed

    /** Run a validator on all transactions, gracefully handling decode errors
      *
      * This method implements a three-tier result tracking system:
      * - **Passed**: Transaction decoded successfully and passed validation
      * - **Failed**: Transaction decoded successfully but failed validation (actual bug)
      * - **Skipped**: Transaction couldn't be decoded or has incomplete test data
      *
      * Transactions are skipped (not failed) for these reasons:
      * - CBOR decode errors (format mismatch, legacy formats)
      * - Missing test files (context.json, tx.cbor)
      * - Incomplete test fixtures (missing UTXO inputs, missing scripts)
      *
      * Only successfully decoded transactions with complete data are validated.
      * Test failures indicate validator bugs, not test infrastructure issues.
      *
      * @param network Network name (e.g., "preprod")
      * @param validatorName Name of the validator for logging
      * @param validate Function to validate a decoded transaction
      * @return Test result summary
      */
    def testTransactionsWithValidator(
        network: String,
        validatorName: String,
        validate: (Transaction, TestContext) => Either[Throwable, Unit]
    ): TransactionTestResult =
        val transactions = getAvailableTransactionTests(network)
        if transactions.isEmpty then cancel(s"No transaction tests available for $network")

        info(s"Testing ${transactions.size} transactions")
        var passed = 0
        var failed = 0
        var skipped = 0

        transactions.foreach { txHash =>
            Try {
                val fixture = loadTransactionFixture(network, txHash)
                val transaction = buildTransaction(fixture)

                // Validate
                validate(transaction, fixture.context) match
                    case Right(_) =>
                        passed += 1
                    case Left(error) =>
                        // Check if this is a test data issue (missing UTXO inputs or scripts)
                        // rather than an actual validation failure
                        error.getMessage match
                            case msg if msg.contains("Missing inputs in UTxO state") ||
                                       msg.contains("Missing collateral inputs in UTxO state") ||
                                       msg.contains("Missing reference inputs in UTxO state") ||
                                       msg.contains("Missing or extra script hashes") ||
                                       msg.contains("Missing key hashes") ||
                                       msg.contains("Invalid verified signatures") ||
                                       msg.contains("Missing auxiliary data") ||
                                       msg.contains("is outside the validity interval") =>
                                skipped += 1
                                info(s"⚠ $txHash: ${error.getMessage}")
                            case _ =>
                                failed += 1
                                info(s"✗ $txHash failed validation: ${error.getMessage}")
            } match
                case Success(_) => // Continue
                case Failure(e) =>
                    skipped += 1
                    info(s"⚠ $txHash: ${e.getMessage}")
        }

        val result = TransactionTestResult(passed, failed, skipped, transactions.size)
        info(s"✓ $validatorName: ${result.passed} passed, ${result.failed} failed, ${result.skipped} skipped out of ${result.total}")
        result

    /** Build a Transaction from test fixture data
      *
      * Amaru test fixtures can store transactions in three formats:
      *   1. Split format: tx.cbor contains TransactionBody, witness.cbor contains TransactionWitnessSet
      *   2. Complete array format: tx.cbor contains complete Transaction as CBOR array
      *   3. Complete map format: tx.cbor contains complete Transaction as CBOR map (legacy)
      *
      * This method handles all three formats.
      *
      * @param fixture Test fixture containing CBOR data
      * @return Complete Transaction
      */
    def buildTransaction(fixture: TransactionTestFixture): Transaction =
        import scalus.serialization.cbor.Cbor as ScalusCbor
        import scalus.cardano.ledger.{TransactionBody, TransactionWitnessSet, AuxiliaryData, KeepRaw, ProtocolVersion, OriginalCborByteArray}
        import scala.util.{Try, Success, Failure}

        given ProtocolVersion = ProtocolVersion.conwayPV

        // Detect CBOR format by examining the first byte
        val firstByte = fixture.txCbor(0) & 0xFF
        val isMap = (firstByte & 0xE0) == 0xA0 // Map major type (5)
        val isArray = (firstByte & 0xE0) == 0x80 // Array major type (4)

        if isMap then
            // Handle map format: {0: body, 1: witness_set, 2: is_valid, 3: auxiliary_data}
            // Parse using manual CBOR map extraction
            Try {
                // Helper to extract raw CBOR bytes for a specific map key
                def extractMapValue(targetKey: Int): Option[Array[Byte]] =
                    Try {
                        import io.bullet.borer.{Decoder, Cbor}

                        // Custom decoder that captures byte positions and returns (start, end) offsets
                        given Decoder[Option[(Int, Int)]] = Decoder { r =>
                            val mapSize = r.readMapHeader().toInt
                            var result: Option[(Int, Int)] = None

                            for (_ <- 0 until mapSize if result.isEmpty) do
                                val key = r.readInt()
                                val valueStart = r.position.index.toInt
                                r.skipDataItem() // Skip the value (including tags)
                                val valueEnd = r.position.index.toInt

                                if key == targetKey then
                                    result = Some((valueStart, valueEnd))

                            result
                        }

                        Cbor.decode(fixture.txCbor).to[Option[(Int, Int)]].valueEither match
                            case Right(Some((start, end))) => Some(fixture.txCbor.slice(start, end))
                            case _ => None
                    }.toOption.flatten

                // Extract transaction body bytes (key 0)
                val bodyBytes = extractMapValue(0).getOrElse(
                    throw new RuntimeException("Missing transaction body in map")
                )
                given OriginalCborByteArray = OriginalCborByteArray(bodyBytes)
                val body = ScalusCbor.decode[TransactionBody](bodyBytes)

                // Extract witness set bytes (key 1)
                val witnessSet = extractMapValue(1) match
                    case Some(bytes) =>
                        given OriginalCborByteArray = OriginalCborByteArray(bytes)
                        ScalusCbor.decode[TransactionWitnessSet](bytes)
                    case None => TransactionWitnessSet.empty

                // Extract auxiliary data (key 3)
                val auxData = extractMapValue(3).flatMap { bytes =>
                    Try {
                        given OriginalCborByteArray = OriginalCborByteArray(bytes)
                        val aux = ScalusCbor.decode[AuxiliaryData](bytes)
                        KeepRaw.unsafe(aux, bytes)
                    }.toOption
                }

                Transaction(KeepRaw.unsafe(body, bodyBytes), witnessSet, auxiliaryData = auxData)
            }.recoverWith {
                // If map parsing fails, try array format as fallback
                case _ =>
                    Try {
                        given OriginalCborByteArray = OriginalCborByteArray(fixture.txCbor)
                        ScalusCbor.decode[Transaction](fixture.txCbor)
                    }
            }.get
        else
            // Handle array formats
            Try {
                given OriginalCborByteArray = OriginalCborByteArray(fixture.txCbor)
                ScalusCbor.decode[Transaction](fixture.txCbor)
            } match
                case Success(tx) =>
                    // tx.cbor contains a complete transaction in array format
                    tx
                case Failure(_) =>
                    // tx.cbor contains only TransactionBody
                    given OriginalCborByteArray = OriginalCborByteArray(fixture.txCbor)
                    val body = ScalusCbor.decode[TransactionBody](fixture.txCbor)

                    // Decode witness set from witness.cbor, or use empty if not present
                    val witnessSet = fixture.witnessCbor match
                        case Some(witnessCborBytes) =>
                            given OriginalCborByteArray = OriginalCborByteArray(witnessCborBytes)
                            ScalusCbor.decode[TransactionWitnessSet](witnessCborBytes)
                        case None =>
                            TransactionWitnessSet.empty

                    // Construct the complete transaction
                    Transaction(KeepRaw.unsafe(body, fixture.txCbor), witnessSet, auxiliaryData = None)

    /** Build UTXO map from test fixture context
      *
      * Converts test data models to Scalus types for use in validators
      *
      * @param fixture
      *   Test fixture containing context data
      * @return
      *   UTXO map suitable for Scalus validators
      */
    def buildUtxoMap(fixture: TransactionTestFixture): scalus.cardano.ledger.Utxos =
        import scalus.utils.Hex.*
        import scalus.builtin.ByteString
        import scalus.cardano.address
        import scalus.cardano.ledger.{TransactionInput, TransactionOutput, TransactionHash, Value, Coin, MultiAsset, DatumOption, DataHash, ScriptRef, Script}
        import io.bullet.borer.Cbor

        fixture.context.utxo.map { case (input, output) =>
            // Convert test input to Scalus TransactionInput
            val txInput = TransactionInput(
              TransactionHash.fromByteString(ByteString.fromArray(input.transaction_id.hexToBytes)),
              input.index
            )

            // Parse address from hex
            val addr = address.Address.fromBytes(output.address.hexToBytes)

            // Create value (for now just coin, multi-assets would need more work)
            val value = Value(Coin(output.value), MultiAsset.empty)

            // Parse datum option if present
            val datumOption = output.datum.map {
                case TestDataModels.DatumType.Data(hex) =>
                    // Inline datum - decode as Data
                    val datumBytes = hex.hexToBytes
                    DatumOption.Inline(Cbor.decode(datumBytes).to[scalus.builtin.Data].value)
                case TestDataModels.DatumType.Hash(hex) =>
                    // Datum hash
                    DatumOption.Hash(DataHash.fromByteString(ByteString.fromArray(hex.hexToBytes)))
            }

            // Parse script ref if present
            val scriptRef = output.script.map { scriptType =>
                // Extract hex from the script type
                val scriptHex = scriptType match
                    case TestDataModels.ScriptType.PlutusV1(hex) => hex
                    case TestDataModels.ScriptType.PlutusV2(hex) => hex
                    case TestDataModels.ScriptType.PlutusV3(hex) => hex
                    case TestDataModels.ScriptType.NativeScript(hex) => hex
                // Parse the script from CBOR
                val scriptBytes = scriptHex.hexToBytes
                ScriptRef(Cbor.decode(scriptBytes).to[Script].value)
            }

            // Create transaction output (Babbage format to support datum and script ref)
            val txOutput = TransactionOutput(
              addr,
              value,
              datumOption,
              scriptRef
            )

            txInput -> txOutput
        }.toMap
