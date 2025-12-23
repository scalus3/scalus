# Milestone 4 Proof of Achievement Report

## Documentation, Examples, and Final Report

---

### A. ScalaDoc Documentation of the API

**Output:** ScalaDoc documentation of the TxBuilder API available on Scalus website scalus.org and
GitHub repository.

**Acceptance criteria:** ScalaDoc Documentation of the API is available on Scalus website scalus.org

**Evidence:**

- API Documentation on Scalus
  website: [https://scalus.org/api/index.html](https://scalus.org/api/index.html)
- TxBuilder API
  ScalaDoc: [https://scalus.org/api/scalus/cardano/txbuilder/TxBuilder.html](https://scalus.org/api/scalus/cardano/txbuilder/TxBuilder.html)
- Source code with ScalaDoc
  comments: [https://github.com/scalus3/scalus/blob/v0.14.1/scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TxBuilder.scala](https://github.com/scalus3/scalus/blob/v0.14.1/scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TxBuilder.scala)

---

### B. TxBuilder API Usage Examples

**Output:** TxBuilder API usage examples available on Scalus website scalus.org with comprehensive
documentation covering all transaction types.

**Acceptance criteria:** TxBuilder API usage examples is available on Scalus website scalus.org

**Evidence:**

- Transaction Builder documentation overview: [https://scalus.org/docs/transaction-builder](https://scalus.org/docs/transaction-builder)
- Building First
  Transaction: [https://scalus.org/docs/transaction-builder/building-first-transaction](https://scalus.org/docs/transaction-builder/building-first-transaction)
- Payment
  Methods: [https://scalus.org/docs/transaction-builder/payment-methods](https://scalus.org/docs/transaction-builder/payment-methods)
- Spending
  UTxOs: [https://scalus.org/docs/transaction-builder/spending-utxos](https://scalus.org/docs/transaction-builder/spending-utxos)
- Minting & Burning
  Assets: [https://scalus.org/docs/transaction-builder/minting-burning-assets](https://scalus.org/docs/transaction-builder/minting-burning-assets)
- Staking &
  Rewards: [https://scalus.org/docs/transaction-builder/staking-rewards](https://scalus.org/docs/transaction-builder/staking-rewards)
- Governance (DRep
  delegation): [https://scalus.org/docs/transaction-builder/governance](https://scalus.org/docs/transaction-builder/governance)
- Documentation source
  files: [https://github.com/scalus3/scalus/tree/v0.14.1/scalus-site/content/transaction-builder](https://github.com/scalus3/scalus/tree/v0.14.1/scalus-site/content/transaction-builder)

---

### C. Tx Builder API in Scalus Starter Project

**Output:** Demonstration of TxBuilder API usage in the Scalus Starter project, showing real-world
minting and burning transactions.

**Acceptance criteria:** Tx Builder API is used in Scalus Starter project

**Evidence:**

- Scalus Starter Project repository: [https://github.com/scalus3/scalus-starter](https://github.com/scalus3/scalus-starter)
- Transactions.scala using TxBuilder for minting and
  burning: [https://github.com/scalus3/scalus-starter/blob/main/src/main/scala/starter/Transactions.scala](https://github.com/scalus3/scalus-starter/blob/main/src/main/scala/starter/Transactions.scala)

The Transactions.scala file demonstrates TxBuilder usage:

```scala
TxBuilder(ctx.cardanoInfo)
    .spend(utxos)
    .collaterals(firstUtxo)
    .mint(script = ..., assets = ..., redeemer = Data.unit, ...)
    .payTo(ctx.address, mintedValue)
    .complete(ctx.provider, ctx.address)
    .sign(ctx.signer)
    .transaction
```

---

### D. Final Close-out Report

**Output:** Final close-out report publicly available on Scalus GitHub repository.

**Acceptance criteria:** Final closeout report is publicly available on Scalus Github

**Evidence:**

- Close-out
  Report: [https://github.com/scalus3/scalus/blob/master/docs/Catalyst/Close-out%20Report%20-%20TxBuilder%201300009.md](https://github.com/scalus3/scalus/blob/master/docs/Catalyst/Close-out%20Report%20-%20TxBuilder%201300009.md)

---

### E. Final Close-out Video

**Output:** Final close-out video demonstrating TxBuilder API functionality on JavaScript platform
and transaction submission to real Cardano node.

**Acceptance criteria:** Final closeout video is publicly available

**Evidence:**

- Close-out Video: [https://www.youtube.com/watch?v=yoLFbIcs3oU](https://www.youtube.com/watch?v=yoLFbIcs3oU)

The video demonstrates:

- Mainnet transactions deserialized and serialized back on JavaScript platform
- All types of transactions constructed and successfully submitted to a real Cardano node using Yaci
  DevKit
- TxBuilder integration tests covering 9 transaction types (payment, minting, stake registration,
  stake delegation, DRep registration, vote delegation, proposal submission, voting, native script
  minting)
