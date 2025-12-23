# Milestone 3 Proof of Achievement Report

## TxBuilder API implementation on JavaScript with Lucid Evolution

---

### A. Transaction Construction Coverage (100% of Conway CDDL types)

**Output:** Full implementation of all Conway era transaction types compiled to JavaScript via
Scala.js, with comprehensive CBOR serialization support.

**Acceptance criteria:** 100% implementation of transaction types defined in Conway CDDL; Automated
test suite covering creation of all 5 types of transactions; GitHub Action CI job produces a test
report for that automated test suite.

**Evidence:**

- Conway transaction types implementation (shared code compiled to
  JS): https://github.com/nau/scalus/tree/v0.14.1/scalus-core/shared/src/main/scala/scalus/cardano/ledger
- CBOR serialization roundtrip tests (63+ property-based tests on
  JS): https://github.com/nau/scalus/blob/v0.14.1/scalus-core/shared/src/test/scala/scalus/cardano/ledger/CborSerializationTest.scala
- TxBuilder integration tests with Yaci DevKit covering 9 transaction types (payment, minting, stake
  registration, stake delegation, DRep registration, vote delegation, proposal submission, voting,
  native script
  minting): https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger-it/src/test/scala/scalus/testing/integration/TxBuilderIntegrationTest.scala
- Reading of 100,000+ mainnet transaction
  in [BlockValidation test](https://github.com/scalus3/scalus/blob/v0.14.1/scalus-cardano-ledger-it/src/test/scala/scalus/testing/integration/BlocksValidation.scala)
- CI-JVM workflow: https://github.com/scalus3/scalus/actions/runs/20439948518
- CI-JS workflow: https://github.com/scalus3/scalus/actions/runs/20439948523
- CI Integraton Tests workflow: https://github.com/scalus3/scalus/actions/runs/20454191734

---

### B. Quality Metrics (ScalaDoc and CI Pipeline)

**Output:** All public APIs documented with ScalaDoc; CI pipeline enforcing compilation and tests
passing on JavaScript platform.

**Acceptance criteria:** All public APIs documented with ScalaDoc; CI pipeline enforcing compilation
and tests passing on JavaScript platform.

**Evidence:**

- TxBuilder API with
  ScalaDoc: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TxBuilder.scala
- JScalus JavaScript API facade with
  documentation: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/scala/scalus/uplc/eval/JScalus.scala
- NPM package README with comprehensive API
  documentation: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/npm/README.md
- TypeScript type
  definitions: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/npm/scalus.d.ts

---

### C. Fee and Script Budget Calculation

**Output:** Plutus script evaluation with budget calculation working on JavaScript platform,
validated against mainnet execution metrics.

**Acceptance criteria:** Benchmark suite comparing calculated vs actual execution costs for 20+
Plutus scripts; Integration tests validating script budgets against cardano-node execution metrics.

**Evidence:**

- JScalus.evalPlutusScripts() implementation calculating execution budgets on
  JS: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/scala/scalus/uplc/eval/JScalus.scala
- Test with real mainnet transaction and UTxO CBOR validating redeemer
  budgets: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/test/scala/scalus/uplc/eval/JScalusTest.scala
- Plutus Conformance tests (130+ test cases) on JavaScript
  platform: https://github.com/nau/scalus/blob/v0.14.1/scalus-core/js/src/test/scala/scalus/uplc/eval/PlutusConformanceJsTest.scala
- MinTransactionFee calculation (shared code working on
  JS): https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/utils/MinTransactionFee.scala

---

### D. Transaction Serialization (100+ Mainnet Transactions)

**Output:** 100+ mainnet transactions successfully deserialized and re-serialized with byte-by-byte
comparison on JavaScript platform.

**Acceptance criteria:** +100 mainnet transactions successfully deserialized and re-serialized;
Byte-by-byte comparison of re-serialized transactions matches original; Integration test suite
validating successful submission of constructed transactions to testnet node.

**Evidence:**

- CBOR serialization roundtrip tests in shared code (63 property-based tests running on
  JS): https://github.com/nau/scalus/blob/v0.14.1/scalus-core/shared/src/test/scala/scalus/cardano/ledger/CborSerializationTest.scala
- Reading of 100,000+ mainnet transaction
  in [BlockValidation test](https://github.com/scalus3/scalus/blob/v0.14.1/scalus-cardano-ledger-it/src/test/scala/scalus/testing/integration/BlocksValidation.scala)
- CI-JVM workflow: https://github.com/scalus3/scalus/actions/runs/20439948518
- CI-JS workflow: https://github.com/scalus3/scalus/actions/runs/20439948523
- CI Integraton Tests workflow: https://github.com/scalus3/scalus/actions/runs/20454191734

---

### E. Transaction Balancing

**Output:** Transaction balancing implementation with input selection, change calculation, and fee
balancing working on JavaScript platform.

**Acceptance criteria:** Test suite with 10+ scenarios covering input selection, change calculation
and fee balancing; 100% success rate for submitting balanced transactions to testnet.

**Evidence:**

- TxBuilder with balancing logic (shared, works on
  JS): https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TxBuilder.scala
- UtxoPool for UTXO
  selection: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/UtxoPool.scala
- Change output
  handling: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/Change.scala
- TxBalance
  validation: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/utils/TxBalance.scala

---

### F. Lucid Evolution Integration

**Output:** Full integration with Anastasia Labs Lucid Evolution TypeScript library for wallet
management, key derivation, and transaction signing on JavaScript platform.

**Acceptance criteria:** Working integration with Lucid Evolution for transaction signing and
submission on JavaScript.

**Evidence:**

- LucidAccount for mnemonic-based wallet derivation using
  @lucid-evolution/wallet: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/scala/scalus/cardano/wallet/LucidAccount.scala
- Lucid Evolution JavaScript bindings (walletFromSeed, CML
  integration): https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/scala/scalus/cardano/wallet/Lucid.scala
- Transaction signing tests with
  Lucid: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/test/scala/scalus/cardano/txbuilder/TxBuilderJsTest.scala
- HTLC integration test using
  LucidAccount: https://github.com/nau/scalus/blob/v0.14.1/scalus-examples/js/src/test/scala/scalus/examples/htlc/HtlcIntegrationTestJs.scala

---

### G. Published NPM Package

**Output:** Published scalus npm package (version 0.14.0) with JavaScript bundle and TypeScript
definitions.

**Acceptance criteria:** NPM package available for JavaScript/TypeScript developers.

**Evidence:**

- npm package: https://www.npmjs.com/package/scalus
- NPM package.json (version
  0.14.0): https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/npm/package.json
- JavaScript bundle (
  scalus.js): https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/npm/scalus.js
- TypeScript definitions (
  scalus.d.ts): https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/npm/scalus.d.ts
- README with usage
  examples: https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger/js/src/main/npm/README.md

---

### H. Video Demonstration

A [video](https://www.youtube.com/watch?v=yoLFbIcs3oU) demonstrating that mainnet transactions can
be deserialized and serialized back on
JavaScript platform, and all types of transactions can be constructed and successfully submitted to
a real Cardano node using Yaci DevKit

The
[TxBuilderIntegrationTest](https://github.com/nau/scalus/blob/v0.14.1/scalus-cardano-ledger-it/src/test/scala/scalus/testing/integration/TxBuilderIntegrationTest.scala)
demonstrates successful submission of 9 transaction types to a real Cardano node via Yaci DevKit.
