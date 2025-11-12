package scalus.testing.conformance

import io.circe.parser.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.util.Using

/** Base trait for conformance tests
  *
  * Provides utilities for loading test data from Amaru data directory
  * and comparing results with expected snapshots.
  */
trait ConformanceTestBase extends AnyFunSuite with Matchers:

    /** Base directory for test data (symlink to amaru/data)
      * Use user.dir system property as base. When tests are forked, user.dir is the module directory.
      */
    private val baseDir = Paths.get(System.getProperty("user.dir"))
    private val isModuleDir = baseDir.getFileName.toString == "scalus-cardano-ledger-it"

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
