package scalus.testing.conformance

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import java.nio.file.Path

/** JSON schema models for Cardano Ledger Conformance Test Suite
  *
  * This implements the test data format for cross-implementation ledger conformance testing
  * as described in https://github.com/IntersectMBO/cardano-ledger/issues/4892
  */
object ConformanceTestSchema {

  /** Test suite metadata */
  case class TestSuiteMetadata(
      name: String,
      version: String,
      description: String,
      source: String, // e.g., "mainnet", "testnet", "synthetic"
      generatedAt: Option[String] = None,
      generatedBy: Option[String] = None
  )

  /** Genesis configuration for a test case */
  case class GenesisConfig(
      /** Network magic */
      networkMagic: Int,
      /** Genesis time (Unix timestamp in seconds) */
      startTime: Long,
      /** Protocol version */
      protocolVersion: ProtocolVersionConfig,
      /** Shelley genesis parameters */
      shelleyGenesis: Option[ShelleyGenesisConfig] = None,
      /** Byron genesis parameters */
      byronGenesis: Option[ByronGenesisConfig] = None
  )

  case class ProtocolVersionConfig(
      major: Int,
      minor: Int
  )

  case class ShelleyGenesisConfig(
      activeSlotsCoeff: String, // Rational as "numerator/denominator"
      epochLength: Int,
      slotLength: Int, // milliseconds
      maxLovelaceSupply: String,
      securityParam: Int,
      slotsPerKESPeriod: Int,
      maxKESEvolutions: Int,
      updateQuorum: Int
  )

  case class ByronGenesisConfig(
      blockVersionData: Option[String] = None  // JSON string for now
  )

  /** Initial ledger state */
  case class InitialLedgerState(
      /** Slot number for the initial state */
      slot: Long,
      /** Block number for the initial state */
      blockNo: Long,
      /** Block hash */
      blockHash: String,
      /** Initial UTXO set */
      utxos: List[UtxoEntry],
      /** Stake pools state */
      stakePools: Option[List[StakePoolState]] = None,
      /** DReps state */
      dreps: Option[List[DRepState]] = None,
      /** Accounts (stake addresses) */
      accounts: Option[List[AccountState]] = None,
      /** Treasury balance */
      treasury: Option[String] = None, // Lovelace as string
      /** Reserves balance */
      reserves: Option[String] = None, // Lovelace as string
      /** Protocol parameters */
      protocolParams: Option[ProtocolParamsState] = None
  )

  case class UtxoEntry(
      txHash: String,
      outputIndex: Int,
      address: String,
      value: String, // Lovelace as string
      datum: Option[DatumRef] = None,
      referenceScript: Option[ScriptRef] = None,
      assets: Option[Map[String, Map[String, String]]] = None // policyId -> assetName -> amount
  )

  case class DatumRef(
      datumType: String, // "inline" or "hash"
      datum: String // hex-encoded
  )

  case class ScriptRef(
      scriptType: String, // "PlutusV1", "PlutusV2", "PlutusV3", "NativeScript"
      script: String // hex-encoded
  )

  case class StakePoolState(
      poolId: String,
      vrfKeyHash: String,
      pledge: String,
      cost: String,
      margin: String, // "numerator/denominator"
      rewardAccount: String,
      owners: List[String],
      relays: List[RelayInfo],
      metadata: Option[PoolMetadataRef],
      blocksProduced: Option[Int] = None
  )

  case class RelayInfo(
      relayType: String, // "single-host-name", "single-host-address", "multi-host-name"
      hostname: Option[String] = None,
      port: Option[Int] = None,
      ipv4: Option[String] = None,
      ipv6: Option[String] = None
  )

  case class PoolMetadataRef(
      url: String,
      hash: String
  )

  case class DRepState(
      drepType: String, // "registered", "abstain", "no_confidence"
      drepId: Option[String] = None,
      anchor: Option[AnchorInfo] = None,
      deposit: Option[String] = None,
      expiryEpoch: Option[Int] = None
  )

  case class AnchorInfo(
      url: String,
      hash: String
  )

  case class AccountState(
      stakeAddress: String,
      delegation: Option[DelegationInfo] = None,
      rewards: Option[String] = None, // Lovelace as string
      deposit: Option[String] = None
  )

  case class DelegationInfo(
      poolId: Option[String] = None,
      drepId: Option[String] = None
  )

  case class ProtocolParamsState(
      minFeeA: Int,
      minFeeB: Int,
      maxBlockBodySize: Int,
      maxTxSize: Int,
      maxBlockHeaderSize: Int,
      keyDeposit: String,
      poolDeposit: String,
      minPoolCost: String,
      priceMem: String,
      priceStep: String,
      maxTxExMem: String,
      maxTxExSteps: String,
      maxBlockExMem: String,
      maxBlockExSteps: String,
      maxValSize: Int,
      collateralPercentage: Int,
      maxCollateralInputs: Int,
      coinsPerUTxOByte: String,
      costModels: Option[Map[String, List[Int]]] = None
  )

  /** Expected ledger state after applying a transaction or block */
  case class ExpectedLedgerState(
      /** Expected slot number */
      slot: Long,
      /** Expected block number */
      blockNo: Long,
      /** Expected block hash */
      blockHash: Option[String] = None,
      /** Expected UTXO set (None means don't validate) */
      utxos: Option[List[UtxoEntry]] = None,
      /** Expected UTXO set changes (additions/deletions) */
      utxoChanges: Option[UtxoChanges] = None,
      /** Expected stake pools state */
      stakePools: Option[List[StakePoolState]] = None,
      /** Expected DReps state */
      dreps: Option[List[DRepState]] = None,
      /** Expected accounts state */
      accounts: Option[List[AccountState]] = None,
      /** Expected treasury balance */
      treasury: Option[String] = None,
      /** Expected reserves balance */
      reserves: Option[String] = None,
      /** Expected errors/validation failures */
      expectedErrors: Option[List[ExpectedError]] = None
  )

  case class UtxoChanges(
      added: List[UtxoEntry],
      removed: List[UtxoRef]
  )

  case class UtxoRef(
      txHash: String,
      outputIndex: Int
  )

  case class ExpectedError(
      errorType: String,
      message: Option[String] = None,
      validator: Option[String] = None
  )

  /** Transaction test case */
  case class TransactionTestCase(
      id: String,
      description: String,
      tags: List[String],
      genesis: GenesisConfig,
      initialState: InitialLedgerState,
      /** Transaction CBOR hex */
      transaction: String,
      expectedState: ExpectedLedgerState,
      /** Whether the transaction is expected to be valid */
      shouldSucceed: Boolean,
      /** Additional context (witnesses, datums, etc.) */
      context: Option[TransactionContext] = None
  )

  case class TransactionContext(
      witnesses: Option[List[String]] = None, // hex-encoded
      supplementalDatums: Option[List[String]] = None, // hex-encoded
      referenceInputs: Option[List[UtxoRef]] = None
  )

  /** Block test case */
  case class BlockTestCase(
      id: String,
      description: String,
      tags: List[String],
      genesis: GenesisConfig,
      initialState: InitialLedgerState,
      /** Block CBOR hex */
      block: String,
      expectedState: ExpectedLedgerState,
      /** Whether the block is expected to be valid */
      shouldSucceed: Boolean
  )

  /** Test result for a transaction */
  case class TransactionTestResult(
      testId: String,
      passed: Boolean,
      executionTimeMs: Long,
      errors: List[String] = List.empty,
      stateDiff: Option[StateDiff] = None
  )

  /** Test result for a block */
  case class BlockTestResult(
      testId: String,
      passed: Boolean,
      executionTimeMs: Long,
      transactionResults: List[TransactionTestResult],
      errors: List[String] = List.empty,
      stateDiff: Option[StateDiff] = None
  )

  /** State differences for debugging */
  case class StateDiff(
      utxoDiff: Option[UtxoSetDiff] = None,
      treasuryDiff: Option[ValueDiff] = None,
      reservesDiff: Option[ValueDiff] = None,
      accountsDiff: Option[List[AccountDiff]] = None
  )

  case class UtxoSetDiff(
      missing: List[UtxoRef],
      unexpected: List[UtxoEntry],
      mismatched: List[UtxoMismatch]
  )

  case class UtxoMismatch(
      ref: UtxoRef,
      expected: UtxoEntry,
      actual: UtxoEntry
  )

  case class ValueDiff(
      expected: String,
      actual: String
  )

  case class AccountDiff(
      address: String,
      field: String,
      expected: String,
      actual: String
  )

  /** Test suite containing multiple test cases */
  case class ConformanceTestSuite(
      metadata: TestSuiteMetadata,
      transactionTests: List[TransactionTestCase] = List.empty,
      blockTests: List[BlockTestCase] = List.empty
  )

  // JSON codecs using jsoniter-scala
  given JsonValueCodec[TestSuiteMetadata] = JsonCodecMaker.make
  given JsonValueCodec[GenesisConfig] = JsonCodecMaker.make
  given JsonValueCodec[ProtocolVersionConfig] = JsonCodecMaker.make
  given JsonValueCodec[ShelleyGenesisConfig] = JsonCodecMaker.make
  given JsonValueCodec[ByronGenesisConfig] = JsonCodecMaker.make

  given JsonValueCodec[InitialLedgerState] = JsonCodecMaker.make
  given JsonValueCodec[UtxoEntry] = JsonCodecMaker.make
  given JsonValueCodec[DatumRef] = JsonCodecMaker.make
  given JsonValueCodec[ScriptRef] = JsonCodecMaker.make
  given JsonValueCodec[StakePoolState] = JsonCodecMaker.make
  given JsonValueCodec[RelayInfo] = JsonCodecMaker.make
  given JsonValueCodec[PoolMetadataRef] = JsonCodecMaker.make
  given JsonValueCodec[DRepState] = JsonCodecMaker.make
  given JsonValueCodec[AnchorInfo] = JsonCodecMaker.make
  given JsonValueCodec[AccountState] = JsonCodecMaker.make
  given JsonValueCodec[DelegationInfo] = JsonCodecMaker.make
  given JsonValueCodec[ProtocolParamsState] = JsonCodecMaker.make

  given JsonValueCodec[ExpectedLedgerState] = JsonCodecMaker.make
  given JsonValueCodec[UtxoChanges] = JsonCodecMaker.make
  given JsonValueCodec[UtxoRef] = JsonCodecMaker.make
  given JsonValueCodec[ExpectedError] = JsonCodecMaker.make

  given JsonValueCodec[TransactionTestCase] = JsonCodecMaker.make
  given JsonValueCodec[TransactionContext] = JsonCodecMaker.make
  given JsonValueCodec[BlockTestCase] = JsonCodecMaker.make

  given JsonValueCodec[TransactionTestResult] = JsonCodecMaker.make
  given JsonValueCodec[BlockTestResult] = JsonCodecMaker.make
  given JsonValueCodec[StateDiff] = JsonCodecMaker.make
  given JsonValueCodec[UtxoSetDiff] = JsonCodecMaker.make
  given JsonValueCodec[UtxoMismatch] = JsonCodecMaker.make
  given JsonValueCodec[ValueDiff] = JsonCodecMaker.make
  given JsonValueCodec[AccountDiff] = JsonCodecMaker.make

  given JsonValueCodec[ConformanceTestSuite] = JsonCodecMaker.make

  /** Helper to load a test suite from a JSON file */
  def loadTestSuite(path: Path): ConformanceTestSuite = {
    val json = scala.io.Source.fromFile(path.toFile).mkString
    readFromString[ConformanceTestSuite](json)
  }

  /** Helper to save a test suite to a JSON file */
  def saveTestSuite(suite: ConformanceTestSuite, path: Path): Unit = {
    val json = writeToString(suite, WriterConfig.withIndentionStep(2))
    java.nio.file.Files.write(path, json.getBytes(java.nio.charset.StandardCharsets.UTF_8))
  }

  /** Helper to load a single transaction test case from JSON */
  def loadTransactionTest(path: Path): TransactionTestCase = {
    val json = scala.io.Source.fromFile(path.toFile).mkString
    readFromString[TransactionTestCase](json)
  }

  /** Helper to load a single block test case from JSON */
  def loadBlockTest(path: Path): BlockTestCase = {
    val json = scala.io.Source.fromFile(path.toFile).mkString
    readFromString[BlockTestCase](json)
  }
}
