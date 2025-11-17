# Cardano Ledger Rules Conformance Tests

This package contains conformance tests for Cardano ledger rules validation based on test data from real Cardano transactions and blocks.

## Overview

These tests verify that Scalus's ledger implementation correctly validates transactions and blocks according to Cardano's ledger rules. The tests use real transaction data from Cardano testnets (preprod, preview) to ensure conformance with actual blockchain behavior.

## Test Data Location

The tests read test fixtures from the local data directory:
```
scalus-cardano-ledger-it/data/
```

### Test Data Structure

```
scalus-cardano-ledger-it/data/
├── transactions/
│   └── <network>/
│       └── <tx-hash>/
│           ├── tx.cbor           - CBOR-encoded transaction body
│           ├── witness.cbor      - Transaction witness set (optional)
│           ├── context.json      - UTXO context (available inputs)
│           └── expected.traces   - Expected trace events
└── blocks/
    └── <network>/
        └── <block-number>/
            └── valid.cbor        - CBOR-encoded block
```

### Variant Tests

Many transactions have variant test cases for testing invalid scenarios:
- `<tx-hash>/invalid-transaction/` - Invalid transaction variant
- `<tx-hash>/collateral-underflow/` - Collateral validation failure
- `<tx-hash>/unknown-input/` - Unknown UTXO input
- etc.

## Test Classes

### Transaction Rule Tests

#### `TransactionInputsConformanceTest`
Tests transaction inputs validation including:
- UTXO resolution
- Input/reference input disjointness
- Byron address handling
- Empty input set rejection

Reference: Amaru's `amaru-ledger/src/rules/transaction/inputs.rs`
Test cases: 5+ transactions with 5+ variant scenarios

#### `TransactionFeesConformanceTest`
Tests transaction fees validation including:
- Fee calculation
- Collateral handling
- Collateral return validation
- Invalid transaction fee rules

Reference: Amaru's `amaru-ledger/src/rules/transaction/fees.rs`
Test cases: 1 base transaction with 3 variant scenarios

#### `TransactionScriptsConformanceTest`
Tests script validation including:
- Required scripts presence
- Datum availability
- Script witness validation
- Plutus script execution

Reference: Amaru's `amaru-ledger/src/rules/transaction/scripts.rs`
Test cases: Multiple transactions with script validation

#### `TransactionOutputsConformanceTest`
Tests output validation including:
- Minimum UTxO value (minUTxO)
- Output value size constraints
- Address format validation
- Datum handling in outputs

Reference: Amaru's `amaru-ledger/src/rules/transaction/outputs.rs`
Test cases: 3+ transactions

#### `TransactionMetadataConformanceTest`
Tests metadata validation including:
- Metadata size limits
- Metadata hash matching
- Auxiliary data validation

Reference: Amaru's `amaru-ledger/src/rules/transaction/metadata.rs`

### Block Rule Tests

#### `BlockRulesConformanceTest`
Tests block validation including:
- Block header size limits
- Block body size limits
- Execution units (ExUnits) limits

Reference: Amaru's `amaru-ledger/src/rules/block/`
Test cases: 2 preprod blocks

## Test Infrastructure

### `ConformanceTestBase`
Base trait (shared with epoch-level conformance tests) providing utilities for:
- Loading transaction test fixtures
- Loading block test fixtures
- Loading epoch-level data (pools, rewards, governance)
- Parsing JSON context data
- Reading CBOR binary data
- Discovering available test cases
- Snapshot comparison

### `TestDataModels`
Data models for parsing test data:
- `TransactionInput` - Transaction input reference
- `TransactionOutput` - Transaction output
- `TestContext` - UTXO context and required witnesses
- `TraceEvent` - Expected validation trace events
- `TransactionTestFixture` - Complete test fixture

## Running the Tests

### Run all ledger rules conformance tests:
```bash
sbtn scalusCardanoLedgerIt/testOnly scalus.testing.conformance.rules.*
```

### Run specific test class:
```bash
sbtn scalusCardanoLedgerIt/testOnly scalus.testing.conformance.rules.TransactionInputsConformanceTest
```

### Setting Up Test Data

To populate the test data directory, you can:

**Option 1: Copy from Amaru repository** (if available):
```bash
# Copy transaction test fixtures
mkdir -p scalus-cardano-ledger-it/data
cp -r ../amaru/crates/amaru-ledger/tests/data/transactions \
     scalus-cardano-ledger-it/data/

# Copy block test fixtures
cp -r ../amaru/crates/amaru-ledger/tests/data/blocks \
     scalus-cardano-ledger-it/data/
```

**Option 2: Generate from mainnet/testnet data** (future capability)

If test data is not available, tests will be automatically skipped with a cancellation message.

## Test Coverage

Currently, the tests cover:

**Transaction Rules:**
- ✅ Inputs validation (11 test cases)
- ✅ Fees validation (4 test cases)
- ✅ Scripts validation (14 test cases)
- ✅ Outputs validation (6 test cases)
- ✅ Metadata validation (5 test cases)
- 🔄 Certificates (TBD)
- 🔄 Voting procedures (6 test cases)
- 🔄 Proposals (1 test case)
- 🔄 Withdrawals (4 test cases)
- 🔄 Minting (1 test case)
- 🔄 VKey witness (13 test cases)

**Block Rules:**
- ✅ Header size validation (1 test case)
- ✅ Body size validation (2 test cases)
- ✅ Execution units validation (2 test cases)

**Total Transaction Test Fixtures:** 25 unique transactions
**Total Block Test Fixtures:** 2 blocks

## Current Implementation Status

The current tests focus on:
1. **Loading test fixtures** - Verifying we can parse test data
2. **Data structure validation** - Ensuring test context is correct
3. **Test discovery** - Finding all available test cases

**Next Steps:**
1. Implement actual ledger rule validators in Scalus
2. Parse CBOR transaction/block data
3. Execute validation rules against test data
4. Compare results with expected traces
5. Handle all variant test scenarios

## Test Data Format

### context.json
```json
{
  "utxo": [
    [
      {"transaction_id": "hash", "index": 0},
      {"address": "hex", "value": 1000000, "datum": null, "script": null}
    ]
  ],
  "required_signers": ["keyhash"],
  "required_scripts": ["scripthash"],
  "required_bootstrap_roots": ["bootstraphash"]
}
```

### expected.traces
```json
[
  {"name": "require_script_witness_span", "hash": "scripthash"},
  {"name": "require_vkey_witness_span", "hash": "keyhash"}
]
```

## References

- [Amaru Repository](https://github.com/pragma-org/amaru)
- [Amaru Ledger Rules](https://github.com/pragma-org/amaru/tree/main/crates/amaru-ledger/src/rules)
- [Cardano Ledger Specification](https://github.com/IntersectMBO/cardano-ledger)
