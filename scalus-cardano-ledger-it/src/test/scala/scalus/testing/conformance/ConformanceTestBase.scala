package scalus.testing.conformance

import io.circe.parser.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.testing.conformance.TestDataModels.*

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.util.Using

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
    def loadJson[T](path: Path)(using decoder: io.circe.Decoder[T]): T =
        val content = Using.resource(Source.fromFile(path.toFile))(_.mkString)
        decode[T](content) match
            case Right(value) => value
            case Left(error) => fail(s"Failed to parse JSON from $path: $error")

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
            val datumOption = output.datum.map { datumHex =>
                // For now, treat it as a datum hash
                DatumOption.Hash(DataHash.fromByteString(ByteString.fromArray(datumHex.hexToBytes)))
            }

            // Parse script ref if present
            val scriptRef = output.script.map { scriptHex =>
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
