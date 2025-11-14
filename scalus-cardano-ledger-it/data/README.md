# Ledger Rules Test Data

This directory contains test fixtures for Cardano ledger rules conformance tests.

## Structure

```
data/
├── transactions/
│   ├── preprod/
│   │   └── <tx-hash>/
│   │       ├── tx.cbor           - CBOR-encoded transaction body
│   │       ├── witness.cbor      - Transaction witness set (optional)
│   │       ├── context.json      - UTXO context (available inputs)
│   │       └── expected.traces   - Expected trace events
│   └── preview/
│       └── ...
└── blocks/
    ├── preprod/
    │   └── <block-number>/
    │       └── valid.cbor        - CBOR-encoded block
    └── preview/
        └── ...
```

## Populating Test Data

### From Amaru Repository

If you have the Amaru repository cloned as a sibling directory:

```bash
# Copy transaction test fixtures
cp -r ../amaru/crates/amaru-ledger/tests/data/transactions .

# Copy block test fixtures
cp -r ../amaru/crates/amaru-ledger/tests/data/blocks .
```

### Verify Data Structure

After copying, verify the structure:

```bash
# Should list transaction test cases
ls transactions/preprod/

# Should list block test cases
ls blocks/preprod/
```

## Test Data Sources

The test data comes from real Cardano blockchain transactions and blocks:
- **preprod** testnet - Preprod Cardano testnet
- **preview** testnet - Preview Cardano testnet

Each transaction test case includes:
- **tx.cbor** - The transaction body in CBOR format
- **context.json** - The UTXO context needed for validation
- **expected.traces** - Expected trace events from validation
- **witness.cbor** (optional) - Transaction witness set

Test cases may have variants for testing invalid scenarios:
- `invalid-transaction/` - Invalid version of the transaction
- `collateral-underflow/` - Collateral validation failure
- `unknown-input/` - Missing UTXO reference
- etc.

## References

- [Amaru Test Data](https://github.com/pragma-org/amaru/tree/main/crates/amaru-ledger/tests/data)
