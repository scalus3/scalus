# Cardano Ledger Conformance Test Suite

This directory contains the Scalus implementation of the Cardano Ledger Conformance Test Suite, designed to validate ledger implementations across different programming languages.

## Overview

The conformance test suite provides:
- **Implementation-independent testing**: Tests work across Rust, Go, C++, Scala, and other implementations
- **Comprehensive state modeling**: Full ledger state including UTXOs, pools, DReps, governance
- **Automatic categorization**: Smart tagging by era, features, and expected outcomes
- **Multiple export formats**: JSON, CSV, HTML reports
- **Integration with cardano-ledger**: Direct import of test vectors from the reference implementation

## Architecture

### Components

```
conformance/
├── ConformanceTestSchema.scala       # JSON schema and data models
├── CardanoLedgerVectorImporter.scala # Import cardano-ledger test vectors
├── ConformanceTestRunner.scala       # Test execution engine
├── ConformanceTestExporter.scala     # Export/convert utilities
├── ConformanceTestExamples.scala     # Example test cases
├── StateComparator.scala             # State comparison and diff generation
├── CborParser.scala                  # CBOR parsing utilities
├── BlockValidator.scala              # Block-level validation
├── ScriptDatumValidator.scala        # Reference scripts and inline datums
├── CardanoLedgerConformanceTest.scala # ScalaTest suite
└── README.md                         # This file
```

### Key Classes

#### ConformanceTestSchema
Defines the complete JSON schema for conformance tests:
- `TransactionTestCase` - Individual transaction test
- `BlockTestCase` - Block-level test (multiple transactions)
- `GenesisConfig` - Network genesis parameters
- `InitialLedgerState` - Starting ledger state
- `ExpectedLedgerState` - Expected state after transaction/block
- `TransactionTestResult` - Test execution results
- `StateDiff` - Detailed state differences for debugging

#### CardanoLedgerVectorImporter
Imports test vectors from cardano-ledger dump format:
- Loads raw vectors from directory trees
- Automatic test categorization and tagging
- Comprehensive statistics generation
- Conversion to `TransactionTestCase` format

#### ConformanceTestRunner
Executes conformance tests:
- Basic and full validation modes
- Tag-based filtering
- Detailed timing and reporting
- JSON result export/import

#### ConformanceTestExporter
Converts and exports test data:
- Export to JSON, CSV, HTML formats
- Aggregate results from multiple runs
- Generate summary reports
- Convert between formats

#### StateComparator
Compares ledger states and generates diffs:
- Compare expected vs actual UTXO sets
- Compare treasury and reserves balances
- Compare account states and delegations
- Generate detailed diff reports for debugging

#### CborParser
CBOR parsing utilities:
- Parse transaction CBOR
- Parse ledger state CBOR
- Extract UTXO maps, metadata, scripts
- Validate CBOR structure

#### BlockValidator
Block-level validation:
- Validate entire blocks with multiple transactions
- Sequential transaction processing
- Block header validation
- Block body hash verification

#### ScriptDatumValidator
Reference scripts and inline datums validation:
- Validate reference scripts in UTXOs
- Validate inline datums
- Check script and datum availability
- Verify script purpose and redeemer pairing

## Usage

### Running Conformance Tests

```scala
import scalus.testing.conformance.*

// Load and run test vectors
val results = ConformanceTestRunner.runConformanceTests(
  vectorsPath = Paths.get("/path/to/test-vectors"),
  validators = List.empty,  // Add validators as needed
  maxTests = Some(100),     // Limit test count
  filterTags = Set("conway", "plutus")  // Filter by tags
)

// Print summary
println(s"Passed: ${results.passedTests}/${results.totalTests}")
println(s"Pass rate: ${results.passRate}%")

// Export results
ConformanceTestRunner.saveResults(
  results,
  Paths.get("conformance-results.json")
)
```

### Importing Test Vectors

```scala
import scalus.testing.conformance.CardanoLedgerVectorImporter.*

// Load vectors from directory
val vectors = loadAllVectors(
  Paths.get("/path/to/conformance-test-vectors")
)

// Generate statistics
val stats = computeStats(vectors)
println(stats.report)
```

### Converting to Test Suite

```scala
import scalus.testing.conformance.ConformanceTestExporter.*

convertVectorsToTestSuite(
  vectorsPath = Paths.get("/path/to/test-vectors"),
  outputPath = Paths.get("test-suite.json"),
  suiteName = "Scalus Conformance Tests",
  genesis = ConformanceTestExamples.exampleMainnetGenesis
)
```

### Generating Reports

```scala
import scalus.testing.conformance.ConformanceTestExporter.*

// Generate summary report
val summary = generateSummaryReport(results)
println(summary)

// Export to HTML
exportResultsToHTML(results, Paths.get("report.html"))

// Export to CSV
exportResultsToCSV(results, Paths.get("results.csv"))
```

## Test Vector Format

### Raw Test Vector (from cardano-ledger)

```json
{
  "cbor": "84a400...",
  "oldLedgerState": "828383...",
  "newLedgerState": "838484...",
  "success": true,
  "testState": "Conway.Imp.UtxoSpec.ValidTransaction"
}
```

### Conformance Test Case (Scalus format)

```json
{
  "id": "test-001",
  "description": "Simple payment transaction",
  "tags": ["positive-test", "conway", "simple-payment"],
  "genesis": {
    "networkMagic": 764824073,
    "startTime": 1506203091,
    "protocolVersion": { "major": 9, "minor": 0 }
  },
  "initialState": {
    "slot": 1000,
    "blockNo": 100,
    "blockHash": "00000...",
    "utxos": [...]
  },
  "transaction": "84a3...",
  "expectedState": {
    "slot": 1000,
    "blockNo": 100,
    "utxoChanges": {
      "removed": [...],
      "added": [...]
    }
  },
  "shouldSucceed": true
}
```

## Test Tags

Tests are automatically tagged by:

### Era Tags
- `conway` - Conway era
- `babbage` - Babbage era
- `alonzo` - Alonzo era
- `allegra` - Allegra era

### Feature Tags
- `plutus`, `plutus-v1`, `plutus-v2`, `plutus-v3` - Plutus scripts
- `metadata` - Transaction metadata
- `collateral` - Collateral handling
- `ex-units` - Execution units
- `staking` - Staking operations
- `pools` - Stake pool operations
- `governance` - Governance features (DReps, proposals)
- `treasury` - Treasury operations
- `withdrawals` - Reward withdrawals
- `certificates` - Stake certificates

### Outcome Tags
- `positive-test` - Expected to succeed
- `negative-test` - Expected to fail

## Examples

See `ConformanceTestExamples.scala` for complete examples:
- Simple payment transaction
- Native asset minting
- Plutus script execution
- Validation failure cases

## Integration with cardano-ledger

Test vectors can be sourced from the cardano-ledger repository:

```bash
# Clone cardano-ledger repo
git clone https://github.com/IntersectMBO/cardano-ledger

# Generate test vectors (requires Haskell toolchain)
cd cardano-ledger
cabal run conformance-test-dump

# Import into Scalus
```

Alternatively, use pre-generated vectors from:
```
/Users/sergiy/work/lantr/cardano-blueprint/src/ledger/conformance-test-vectors
```

## Running Tests

### Via SBT

```bash
# Run all conformance tests
sbt "scalusCardanoLedgerIt/testOnly *ConformanceTest"

# Run with specific tags
sbt "scalusCardanoLedgerIt/testOnly *ConformanceTest -- -n conformance"
```

### Via ScalaTest

```scala
class MyConformanceTests extends AnyFunSuite {
  test("run conformance tests") {
    val results = ConformanceTestRunner.runConformanceTests(...)
    assert(results.passRate > 95.0, "Expected >95% pass rate")
  }
}
```

## Extending the Suite

### Adding Custom Validators

```scala
import scalus.cardano.ledger.rules.STS

val customValidator = STS.Validator { (context, state, tx) =>
  // Your validation logic
  Right(())
}

val results = ConformanceTestRunner.runWithValidation(
  vectorsPath = ...,
  validators = List(STS.UTXO, STS.UTXOW, customValidator),
  initialContext = context,
  maxTests = Some(100)
)
```

### Creating Custom Test Cases

```scala
val myTest = TransactionTestCase(
  id = "custom-001",
  description = "My custom test",
  tags = List("custom"),
  genesis = ConformanceTestExamples.exampleMainnetGenesis,
  initialState = InitialLedgerState(...),
  transaction = "84a3...",
  expectedState = ExpectedLedgerState(...),
  shouldSucceed = true
)
```

## Implemented Features

- [x] State comparison and diff generation
- [x] CBOR transaction parsing utilities
- [x] Ledger state parsing (oldLedgerState/newLedgerState)
- [x] Block-level validation framework
- [x] Reference scripts and inline datums validation
- [x] Script and datum availability checking
- [x] Block header and body validation
- [x] Multi-format export (JSON, CSV, HTML)

## TODO / Future Work

- [ ] Complete CBOR decoder integration with existing Scalus parsers
- [ ] Implement governance state validation (DReps, proposals, voting)
- [ ] Add stake pool rewards calculation
- [ ] Implement full block hash computation (Blake2b-256)
- [ ] Integrate with cardano-cli for generating test vectors
- [ ] Add native script evaluation
- [ ] Implement Plutus script execution within conformance tests
- [ ] Add detailed metrics and statistics tracking

## References

- [Cardano Ledger Conformance Tests Proposal](https://github.com/IntersectMBO/cardano-ledger/issues/4892)
- [SundaeSwap Conformance Tests](https://github.com/SundaeSwap-finance/cardano-ledger-conformance-tests)
- [Scalus Documentation](https://github.com/nau/scalus)

## License

Same as Scalus project.
