package scalus.testing.conformance

import scalus.testing.conformance.ConformanceTestSchema.*

/** Example conformance test cases demonstrating the test schema
  *
  * These examples show how to construct conformance test cases manually.
  * In practice, most tests will be imported from cardano-ledger test vectors.
  */
object ConformanceTestExamples {

  /** Example genesis configuration for mainnet */
  val exampleMainnetGenesis: GenesisConfig = GenesisConfig(
    networkMagic = 764824073,
    startTime = 1506203091, // Mainnet start time
    protocolVersion = ProtocolVersionConfig(major = 9, minor = 0),
    shelleyGenesis = Some(
      ShelleyGenesisConfig(
        activeSlotsCoeff = "1/20",
        epochLength = 432000,
        slotLength = 1000,
        maxLovelaceSupply = "45000000000000000",
        securityParam = 2160,
        slotsPerKESPeriod = 129600,
        maxKESEvolutions = 62,
        updateQuorum = 5
      )
    ),
    byronGenesis = None
  )

  /** Example genesis configuration for testnet */
  val exampleTestnetGenesis: GenesisConfig = GenesisConfig(
    networkMagic = 1,
    startTime = 1563999616, // Testnet start time
    protocolVersion = ProtocolVersionConfig(major = 9, minor = 0),
    shelleyGenesis = Some(
      ShelleyGenesisConfig(
        activeSlotsCoeff = "1/20",
        epochLength = 432000,
        slotLength = 1000,
        maxLovelaceSupply = "45000000000000000",
        securityParam = 2160,
        slotsPerKESPeriod = 129600,
        maxKESEvolutions = 62,
        updateQuorum = 5
      )
    ),
    byronGenesis = None
  )

  /** Example: Simple payment transaction test case */
  val simplePaymentTest: TransactionTestCase = TransactionTestCase(
    id = "simple-payment-001",
    description = "Simple ADA payment from one address to another",
    tags = List("positive-test", "simple-payment", "conway"),
    genesis = exampleMainnetGenesis,
    initialState = InitialLedgerState(
      slot = 1000,
      blockNo = 100,
      blockHash = "0" * 64,
      utxos = List(
        UtxoEntry(
          txHash = "a" * 64,
          outputIndex = 0,
          address = "addr1qx...",
          value = "10000000", // 10 ADA
          datum = None,
          referenceScript = None,
          assets = None
        )
      )
    ),
    transaction = "84a3..." // CBOR hex would go here
    ,
    expectedState = ExpectedLedgerState(
      slot = 1000,
      blockNo = 100,
      utxoChanges = Some(
        UtxoChanges(
          removed = List(UtxoRef(txHash = "a" * 64, outputIndex = 0)),
          added = List(
            UtxoEntry(
              txHash = "b" * 64,
              outputIndex = 0,
              address = "addr1qy...",
              value = "5000000", // 5 ADA to recipient
              datum = None,
              referenceScript = None,
              assets = None
            ),
            UtxoEntry(
              txHash = "b" * 64,
              outputIndex = 1,
              address = "addr1qx...",
              value = "4800000", // 4.8 ADA change
              datum = None,
              referenceScript = None,
              assets = None
            )
          )
        )
      )
    ),
    shouldSucceed = true,
    context = None
  )

  /** Example: Native asset minting test case */
  val nativeAssetMintingTest: TransactionTestCase = TransactionTestCase(
    id = "native-asset-mint-001",
    description = "Minting native assets with policy script",
    tags = List("positive-test", "minting", "native-assets", "conway"),
    genesis = exampleMainnetGenesis,
    initialState = InitialLedgerState(
      slot = 2000,
      blockNo = 200,
      blockHash = "1" * 64,
      utxos = List(
        UtxoEntry(
          txHash = "c" * 64,
          outputIndex = 0,
          address = "addr1qx...",
          value = "5000000",
          datum = None,
          referenceScript = None,
          assets = None
        )
      )
    ),
    transaction = "84a4...", // CBOR hex with minting
    expectedState = ExpectedLedgerState(
      slot = 2000,
      blockNo = 200,
      utxoChanges = Some(
        UtxoChanges(
          removed = List(UtxoRef(txHash = "c" * 64, outputIndex = 0)),
          added = List(
            UtxoEntry(
              txHash = "d" * 64,
              outputIndex = 0,
              address = "addr1qx...",
              value = "4800000",
              datum = None,
              referenceScript = None,
              assets = Some(
                Map(
                  "policy123..." -> Map(
                    "TokenA" -> "1000",
                    "TokenB" -> "500"
                  )
                )
              )
            )
          )
        )
      )
    ),
    shouldSucceed = true,
    context = None
  )

  /** Example: Plutus script execution test case */
  val plutusScriptTest: TransactionTestCase = TransactionTestCase(
    id = "plutus-v2-001",
    description = "Spending output locked by PlutusV2 script",
    tags = List("positive-test", "plutus", "plutus-v2", "conway"),
    genesis = exampleMainnetGenesis,
    initialState = InitialLedgerState(
      slot = 3000,
      blockNo = 300,
      blockHash = "2" * 64,
      utxos = List(
        UtxoEntry(
          txHash = "e" * 64,
          outputIndex = 0,
          address = "addr1w...", // Script address
          value = "10000000",
          datum = Some(DatumRef(datumType = "inline", datum = "d8799f...")),
          referenceScript = None,
          assets = None
        ),
        UtxoEntry(
          txHash = "f" * 64,
          outputIndex = 0,
          address = "addr1qx...", // Collateral
          value = "5000000",
          datum = None,
          referenceScript = None,
          assets = None
        )
      )
    ),
    transaction = "84a6...", // CBOR hex with redeemers
    expectedState = ExpectedLedgerState(
      slot = 3000,
      blockNo = 300,
      utxoChanges = Some(
        UtxoChanges(
          removed = List(UtxoRef(txHash = "e" * 64, outputIndex = 0)),
          added = List(
            UtxoEntry(
              txHash = "g" * 64,
              outputIndex = 0,
              address = "addr1qy...",
              value = "9800000", // Minus fee
              datum = None,
              referenceScript = None,
              assets = None
            )
          )
        )
      )
    ),
    shouldSucceed = true,
    context = Some(
      TransactionContext(
        witnesses = Some(List("5820...", "5820...")), // VKey witnesses
        supplementalDatums = None,
        referenceInputs = None
      )
    )
  )

  /** Example: Failed transaction test case (insufficient funds) */
  val insufficientFundsTest: TransactionTestCase = TransactionTestCase(
    id = "insufficient-funds-001",
    description = "Transaction fails due to insufficient input funds",
    tags = List("negative-test", "validation-error", "conway"),
    genesis = exampleMainnetGenesis,
    initialState = InitialLedgerState(
      slot = 4000,
      blockNo = 400,
      blockHash = "3" * 64,
      utxos = List(
        UtxoEntry(
          txHash = "h" * 64,
          outputIndex = 0,
          address = "addr1qx...",
          value = "1000000", // Only 1 ADA
          datum = None,
          referenceScript = None,
          assets = None
        )
      )
    ),
    transaction = "84a3...", // Transaction trying to send 5 ADA
    expectedState = ExpectedLedgerState(
      slot = 4000,
      blockNo = 400,
      utxos = None, // State unchanged
      expectedErrors = Some(
        List(
          ExpectedError(
            errorType = "ValueNotConservedException",
            message = Some("Insufficient input value"),
            validator = Some("UTXO")
          )
        )
      )
    ),
    shouldSucceed = false,
    context = None
  )

  /** Example: Complete test suite with multiple test cases */
  val exampleTestSuite: ConformanceTestSuite = ConformanceTestSuite(
    metadata = TestSuiteMetadata(
      name = "Scalus Example Conformance Tests",
      version = "1.0.0",
      description = "Example conformance test cases demonstrating the test schema",
      source = "synthetic",
      generatedAt = Some("2024-01-01T00:00:00Z"),
      generatedBy = Some("Scalus ConformanceTestExamples")
    ),
    transactionTests = List(
      simplePaymentTest,
      nativeAssetMintingTest,
      plutusScriptTest,
      insufficientFundsTest
    ),
    blockTests = List.empty
  )

  /** Save example test suite to file for reference
    *
    * @param outputPath
    *   Path to output JSON file
    */
  def saveExampleTestSuite(outputPath: java.nio.file.Path): Unit = {
    ConformanceTestExporter.exportTestSuite(exampleTestSuite, outputPath)
  }

  /** Create minimal example for quick testing */
  val minimalExample: TransactionTestCase = TransactionTestCase(
    id = "minimal-001",
    description = "Minimal test case with required fields only",
    tags = List("example"),
    genesis = GenesisConfig(
      networkMagic = 1,
      startTime = 0,
      protocolVersion = ProtocolVersionConfig(major = 9, minor = 0)
    ),
    initialState = InitialLedgerState(
      slot = 0,
      blockNo = 0,
      blockHash = "0" * 64,
      utxos = List.empty
    ),
    transaction = "84a0...",
    expectedState = ExpectedLedgerState(
      slot = 0,
      blockNo = 0
    ),
    shouldSucceed = true
  )
}
