# Emulator as a node: mempool, blocks and forks – design

Status: draft, for review. Date: 2026-10-21. Branch: `feat/emulator-chain`.

## 1. Purpose and scope

The Scalus Emulator behaves like a small Cardano node. A submitted tx waits in a mempool. A block makes it visible. A fork can roll it back. Today a valid `submit` changes the UTxO set at once, and the chain never rolls back. Real providers do not behave like that, so dApp code that passes against the Emulator can fail on a real network.

In scope (sub-projects 1 and 2):

- A block tree with real `Block` values, a mempool, block forging, chain selection and rollback (sections 4 to 11).
- The `Instant` preset, which keeps today's behaviour and is the default (section 5).
- The `Manual` preset, where only an explicit call forges a block (section 5).
- ChainSync events for `StreamingEmulator` (section 12).
- The JVM, Java, JS and `ImmutableEmulator` surfaces (section 13).
- The work plan (section 17).

Out of scope, with a roadmap entry in section 18:

- Epoch transition rules: POOLREAP, deposits, snapshots, rewards, reserves.
- Governance rules: GOV checks, RATIFY, ENACT.
- PV11 (Dijkstra) tx rules and Plutus V4.
- A random block schedule, forging on the wall clock, and an N2C socket server.

## 2. Definitions

| Term | Meaning |
|---|---|
| Emulator | `EmulatorBase` and its JVM and JS subclasses. |
| node layer | One of: ledger, mempool, ChainDB, LedgerDB, forge. Named after the Haskell node (section 3). |
| snapshot | A `LedgerSnapshot`: the ledger `State`, the `Context` and the datum store after a block or a mempool tx. |
| block tree | All blocks the Emulator holds, keyed by block id. |
| block id | blake2b-256 of the CBOR of the block header. |
| genesis | The virtual root of the block tree. Height 0. It holds the initial snapshot and no `Block`. Its point is `ChainPoint.origin`, its tip `ChainTip.origin`. |
| tip | The block at the end of the selected chain, or genesis. |
| selected chain | The path from genesis to the tip. |
| branch | A path from genesis to a block that is not on the selected chain. A branch is named by the `ChainPoint` of its last block. |
| height | The number of blocks from genesis. The first block has height 1. |
| depth | For a block on the selected chain: `tip.height - block.height + 1`. The tip has depth 1. |
| current slot | The slot the Emulator reports in `currentSlot`. It can be ahead of the tip slot. |
| mempool | The ordered list of pending txs, oldest first. |
| forge | To make a new block and add it to the block tree. |
| block limits | `maxBlockBodySize` and `maxBlockExecutionUnits` of the current protocol parameters. |
| mempool capacity | Two times the block limits, per dimension. |
| k | `ChainConfig.securityParam`. The deepest rollback the Emulator allows. |
| orphan tx | A tx in a block that left the selected chain and is not on the new selected chain. |
| preset | The value of `ChainConfig.blockProduction`: `Instant` or `Manual`. |

## 3. Node layers

Each node layer maps to a component of the Haskell node. Each one reuses existing Scalus classes.

| Node layer | Emulator part | Reuses |
|---|---|---|
| ledger | `applyTx` (LEDGER), `applyBlock` (block limits, then txs), `tick` (TICK) | `rules.State`, `Context`, `UtxoEnv`, `STS` validators and mutators |
| mempool | `Mempool`, revalidation | `NetworkSubmitError.MempoolFull` |
| ChainDB | block tree, chain selection, rollback | `Block`, `BlockHeader`, `ChainPoint`, `ChainTip` |
| LedgerDB | one snapshot per block for the last k blocks | `EmulatorState` contents |
| forge | `produceBlock` | `BlockHeaderBody`, `VrfCert`, `OperationalCert` |

The public API maps to the local mini-protocols of a node:

| Mini-protocol | Emulator API |
|---|---|
| LocalTxSubmission | `submit` |
| LocalStateQuery | `findUtxos`, and `findUtxos(query, view)` (section 9) |
| LocalTxMonitor | `ChainView.Mempool`, `mempoolTxs` |
| ChainSync | `ChainEvent.RollForward` and `RollBackward` (section 12) |

**Why:** The node is the reference. When a question comes up that this spec does not answer, the node's behaviour is the default answer. The mapping also leaves room for an N2C server later, without code for it now.

- The ledger layer MUST run the same `STS` validators and mutators as today. `[NL-1]`
- The `tick` transition MUST do what `EmulatorBase.atSlot` does today, and nothing more. `[NL-2]`
- A transition MUST NOT write `Context.fee`. `[NL-3]`

**Why:** `[NL-2]`: `tick` is where the epoch transition plugs in later (section 18). `[NL-3]`: a snapshot shares its `Context` with later snapshots, so one write would leak into every snapshot. Today no rule writes `fee`.

## 4. Block identity

- `BlockHeader.hash` MUST return blake2b-256 of the header CBOR. `[BH-1]`
- `Block.hash` MUST return `header.hash`. `[BH-2]`
- The forge MUST compute `blockBodyHash` as the ledger does for Alonzo and later eras. `[BH-3]`
- The forge MUST set `blockBodySize` to the byte size of the encoded body. `[BH-4]`
- The forge MUST set `blockNumber` to the block height. `[BH-5]`
- The forge MUST set `prevHash` to the parent block id, or to `None` when the parent is genesis. `[BH-6]`
- The forge MUST set `protocolVersion` from the current protocol parameters. `[BH-7]`
- The forge MUST fill `issuerVkey`, `vrfVkey`, `vrfResult`, `operationalCert` and `bodySignature` with fixed bytes of the correct size. `[BH-8]`
- The forge MUST put the index of each tx with `isValid = false` in `invalidTransactions`. `[BH-9]`

*Example, illustrative,* for `[BH-2]`. Two empty blocks at slot 10, one on parent A and one on parent B:

| | Today (`blockBodyHash`) | `[BH-2]` (header hash) |
|---|---|---|
| block on A | `h(empty body)` | `h(header with prevHash = A)` |
| block on B | `h(empty body)` – the same | `h(header with prevHash = B)` – different |

**Why:** `[BH-1]`, `[BH-2]`: today `Block.hash` returns the body hash. Cardano names a block by its header hash. Two empty blocks on two branches would collide in the block tree. No code in the repo uses `Block.hash` today. `[BH-3]` to `[BH-9]`: a forged block is a real Conway block. Other tools can decode it, and a future N2C server can serve it. Nothing verifies consensus fields, so fixed bytes are enough.

*Consequence:* two forged blocks with the same parent, slot and txs have the same block id. They are the same block. A test that wants two rival blocks gives them different txs or different slots.

## 5. Configuration

`ChainConfig` is one value with four fields:

| Field | Type | Default |
|---|---|---|
| `blockProduction` | `BlockProduction.Instant` or `BlockProduction.Manual` | `Instant` |
| `awaitTx` | `AwaitTxPolicy.ProduceBlock` or `AwaitTxPolicy.WaitOnly` | `ProduceBlock` |
| `orphanTxs` | `OrphanTxPolicy.ReturnToMempool` or `OrphanTxPolicy.Drop` | `ReturnToMempool` |
| `securityParam` | `Long`, at least 1 | 2160 |

- `ChainConfig` MUST NOT have default parameters. `[CF-1]`
- `ChainConfig` MUST provide `ChainConfig.instant` and `ChainConfig.manual` with the defaults above. `[CF-2]`
- `ChainConfig` MUST provide one `with<Field>` copy method per field. `[CF-3]`
- Every existing Emulator factory MUST use `ChainConfig.instant`. `[CF-4]`
- Every Emulator factory MUST have an overload that takes a `ChainConfig`. `[CF-5]`

**Why:** `[CF-1]`, `[CF-3]`: the Emulator is an interop package, and Java and JS cannot use Scala default parameters. `[CF-4]`: hundreds of tests in `scalus-examples`, `scalus-utxo-cell` and user projects depend on submit applying at once.

## 6. State

`EmulatorState` holds the block tree, the mempool, the current slot, the config, and the tx status map.

- The block tree MUST hold, for each block: the `Block`, its block id, its parent id, its height, and its snapshot. `[ST-1]`
- The block tree MUST keep a snapshot for each block of depth at most k on the selected chain. `[ST-2]`
- The block tree MUST keep a snapshot for each branch block whose fork point has depth at most k. `[ST-3]`
- The Emulator MUST drop the snapshot of a selected-chain block deeper than k. `[ST-4]`
- The Emulator MUST keep the `Block` of a selected-chain block deeper than k. `[ST-5]`
- The Emulator MUST remove every branch whose fork point is deeper than k. `[ST-6]`
- Each mempool entry MUST hold the tx and the snapshot after it. `[ST-7]`
- `EmulatorState.ledger`, `context` and `binaryDatums` MUST return the values of the tip snapshot. `[ST-8]`
- Every transition MUST be a pure function from `EmulatorState` to `EmulatorState`. `[ST-9]`

**Why:** `[ST-2]`, `[ST-3]`: a rollback or a branch switch needs the snapshot at the fork point. Immutable maps share structure, so a snapshot per block costs little. `[ST-5]`: `getTransaction` and the history still need old txs. `[ST-8]` keeps today's readers working. `[ST-9]`: `EmulatorBase.modifyState` may re-run a transition on the JVM.

### 6.1 Direct edits

The direct edits are `addUtxo`, `removeUtxo`, `addDatum` and `addRewards`. They bypass validation, as today.

- A direct edit MUST change the tip snapshot in place. `[DE-1]`
- A direct edit MUST NOT forge a block. `[DE-2]`
- After a direct edit, the Emulator MUST run revalidation. `[DE-3]`
- `clearAppliedTxs` MUST clear only the imported log of `[TS-5]`. `[DE-4]`
- `clearAppliedTxs` MUST be deprecated. `[DE-5]`

*Consequence:* a rollback past the tip block also removes the direct edits made on it. `[TE-1]` holds only for a chain without direct edits.

**Why:** `[DE-1]`: a direct edit seeds a test state. It is not a chain event, so it gets no block. `[DE-3]`: a `removeUtxo` can make a pending tx invalid. `[DE-4]`, `[DE-5]`: with `[TS-5]` the selected chain is the log, so the Emulator cannot forget it without forgetting blocks. `StreamingEmulator` already throws after `clearAppliedTxs`.

## 7. Submit and the mempool

**Purpose:** accept a tx into the mempool.
**Who:** a caller of `submit`.
**Trigger:** any time.

| Input | Output |
|---|---|
| `tx: Transaction` | `Either[SubmitError, TransactionHash]` |

Checks enforced here:

- `submit` MUST validate `tx` against the snapshot of the last mempool entry, or the tip snapshot if the mempool is empty. `[MP-1]`
- `submit` MUST validate at the current slot. `[MP-2]`
- `submit` MUST return the `SubmitError` of the first failed rule, as today. `[MP-3]`
- `submit` MUST reject a tx that would push the mempool past the mempool capacity, with `MempoolFull`. `[MP-4]`
- `submit` MUST reject a tx that is already in the mempool or on the selected chain. `[MP-5]`
- On success, `submit` MUST append the tx and its snapshot to the mempool. `[MP-6]`
- On success, `submit` MUST set the tx status to `Pending`. `[MP-7]`
- In the `Instant` preset, `submit` MUST then forge as section 8 says. `[MP-8]`
- `submit` MUST be one atomic transition, including the forge of `[MP-8]`. `[MP-9]`

*Example, illustrative,* for `[MP-4]`. Block limit 90 112 bytes, so the mempool capacity is 180 224 bytes. Each tx is 16 000 bytes:

| Mempool before | `submit` result |
|---|---|
| 10 txs, 160 000 bytes | accepted, 176 000 ≤ 180 224 |
| 11 txs, 176 000 bytes | rejected, 192 000 > 180 224 |

**Why:** `[MP-1]`: the node validates a new tx on top of the mempool, so a tx can spend the outputs of a pending tx (chained txs). `[MP-3]`: a node rejects an invalid tx at the door, so this keeps today's error contract. `[MP-4]`: the node's default mempool capacity is two blocks. `[MP-5]`: the node rejects a duplicate because its inputs are spent.

## 8. Forge

**Purpose:** make a block from mempool txs, or from given txs on any branch.
**Who:** a caller of `produceBlock`. In the `Instant` preset, also `submit` and every transition that leaves the mempool non-empty.
**Trigger:** any time.

| Operation | Input | Output |
|---|---|---|
| `produceBlock()` | none | `ChainPoint` of the new block |
| `produceBlock(on, txs)` | `on: ChainPoint`, `txs: Seq[Transaction]` | `Either[ForgeError, ChainPoint]` |
| `produceBlock(on, slot, txs)` | as above, plus `slot: SlotNo` | `Either[ForgeError, ChainPoint]` |

### 8.1 From the mempool

- In the `Manual` preset, `produceBlock()` MUST forge at slot `max(current slot, tip.slot + 1)`. `[FG-1]`
- In the `Instant` preset, the forge MUST use slot `max(current slot, tip.slot)`. `[FG-2]`
- In the `Manual` preset, before it forges, the forge MUST move the current slot to the forge slot and run revalidation (section 10). `[FG-3]`
- In the `Instant` preset, the forge MUST NOT change the current slot. `[FG-3a]`
- The forge MUST take the longest prefix of the mempool that fits the block limits. `[FG-4]`
- The block limits MUST hold for the body size, the memory units and the CPU steps, each on its own. `[FG-5]`
- The forge MUST use the snapshot of the last tx in the prefix as the block snapshot. `[FG-6]`
- The forge MUST remove the prefix from the mempool. `[FG-7]`
- `produceBlock()` with an empty mempool MUST forge an empty block. `[FG-8]`
- In the `Instant` preset, the forge MUST repeat until the mempool is empty. `[FG-9]`
- In the `Instant` preset, the automatic forge of `[MP-8]` and `[FG-9]` MUST NOT forge an empty block. An explicit `produceBlock()` follows `[FG-8]`. `[FG-10]`

*Example, illustrative,* for `[FG-4]`. Block limit 90 112 bytes. Mempool txs, oldest first: 40 000, 40 000, 16 000, 5 000 bytes:

| Block | Txs | Bytes |
|---|---|---|
| 1 | tx 1, tx 2 | 80 000. Tx 3 would make 96 000, so the prefix stops. |
| 2 | tx 3, tx 4 | 21 000 |

Tx 4 fits in block 1 but stays behind tx 3. The forge takes a prefix, not a best fit.

*Example, illustrative,* for `[FG-1]` and `[FG-2]`. The tip is at slot 50:

| Preset | Current slot | Forge slot |
|---|---|---|
| `Manual` | 50 | 51 |
| `Manual` | 70 | 70 |
| `Instant` | 50 | 50 |
| `Instant` | 40, after `setSlot(40)` | 50 |

**Why:** `[FG-1]`: a real chain has at most one block per slot. `[FG-2]`: in the `Instant` preset several submits at one slot must keep their slot, or the validity intervals of existing tests break. So `Instant` allows blocks with the same slot. This is a documented deviation. `[FG-3]`: a tx valid at the current slot can be expired at the forge slot. Revalidation first means the cached snapshots of the prefix are valid at the forge slot, so the forge runs no script again. `[FG-4]`: the node forges the longest prefix that fits. `[FG-10]`: an `Instant` Emulator must not grow blocks the caller never asked for.

### 8.2 On a branch

- `produceBlock(on, txs)` MUST forge at slot `on.slot + 1`. `[FG-11]`
- `produceBlock(on, slot, txs)` MUST reject a `slot` not greater than `on.slot`, with `ForgeError.SlotNotAfterParent`. `[FG-12]`
- `produceBlock(on, …)` MUST reject an `on` that is not in the block tree, with `ForgeError.UnknownPoint`. `[FG-13]`
- `produceBlock(on, …)` MUST reject an `on` deeper than k, with `ForgeError.TooDeep`. `[FG-14]`
- `produceBlock(on, …)` MUST apply `txs` in order to the snapshot of `on`, at the forge slot. `[FG-15]`
- If a tx in `txs` fails, `produceBlock(on, …)` MUST return `ForgeError.InvalidTx` with its index and its `SubmitError`. `[FG-16]`
- If `produceBlock(on, …)` returns an error, the state MUST NOT change. `[FG-17]`
- `produceBlock(on, …)` MUST reject `txs` that exceed the block limits, with `ForgeError.BlockTooBig`. `[FG-18]`
- After a successful forge on a branch, the Emulator MUST run chain selection (section 11). `[FG-19]`
- `produceBlock(on, …)` MUST NOT read or change the mempool, except through chain selection. `[FG-20]`
- `produceBlock(on, …)` MUST NOT move the current slot backwards. `[FG-21]`

**Why:** a branch block models a block from another pool. Its txs come from the test, not from this node's mempool. The typical case is a rival tx that double-spends an input of a pending tx.

## 9. Views

`ChainView` is `Tip`, `Mempool` or `Stable(depth: Int)`.

- `findUtxos(query)` MUST read the tip snapshot. `[VW-1]`
- Every `BlockchainReader` method except `currentSlot` MUST read the tip snapshot. `[VW-2]`
- The Emulator MUST provide `findUtxos(query, view)`. `[VW-3]`
- `ChainView.Mempool` MUST read the snapshot of the last mempool entry, or the tip snapshot if the mempool is empty. `[VW-4]`
- `ChainView.Stable(d)` MUST read the snapshot of the selected-chain block of depth `d`. `[VW-5]`
- `ChainView.Stable(d)` MUST fail for `d < 1` or for a `d` greater than the tip height plus 1. `[VW-6]`
- `ChainView.Stable(d)` MUST fail for a block whose snapshot `[ST-4]` dropped. `[VW-7]`
- `ChainView.Stable(d)` for `d` equal to the tip height plus 1 MUST read genesis, if its snapshot is kept. `[VW-8]`
- `currentSlot` MUST return the current slot. `[VW-9]`

*Example, illustrative.* Tip at height 5. Tx A is in block 4. Tx B is in the mempool:

| View | Sees A's outputs | Sees B's outputs |
|---|---|---|
| `Tip` | yes | no |
| `Mempool` | yes | yes |
| `Stable(1)` | yes | no |
| `Stable(2)` | yes | no |
| `Stable(3)` | no | no |

**Why:** `[VW-1]`, `[VW-2]`: Blockfrost, Koios and Kupo show only block-confirmed state. Right after a submit, the inputs still look unspent and the outputs are absent. dApp code must cope with that. In the `Instant` preset the mempool is empty after each submit, so `Tip` equals `Mempool` and today's tests see no change. `[VW-5]`: `Stable` lets a dApp test wait for N confirmations.

## 10. Revalidation

**Purpose:** remove mempool txs that are no longer valid, as the node's `syncWithLedger` does.
**Who:** the Emulator.
**Trigger:** every change of the tip, every `setSlot` and every `tick`.

- Revalidation MUST re-apply every mempool tx in order, on the tip snapshot, at the current slot. `[RV-1]`
- Revalidation MUST remove a tx that is now on the selected chain. `[RV-2]`
- Revalidation MUST remove a tx that fails, and set its status to `Dropped`. `[RV-3]`
- The drop reason MUST be `Expired` when the validity interval rule failed. `[RV-4]`
- The drop reason MUST be `InputsSpent` when the inputs rule failed. `[RV-5]`
- For any other failed rule, the drop reason MUST be `Invalid`, with the `SubmitError`. `[RV-6]`
- Revalidation MUST replace the snapshot of each kept tx. `[RV-7]`
- In the `Manual` preset, `setSlot` MUST reject a slot below the tip slot. `[RV-8]`

*Example, illustrative,* for `[RV-4]`. A mempool tx has `invalidHereafter = 100`. The ledger accepts the tx when `slot < 100`:

| `setSlot` | Result |
|---|---|
| 99 | kept |
| 100 | dropped, `Expired` |

**Why:** the node drops a tx that the new ledger state makes invalid. The two common cases are a TTL that passed and an input that another tx spent. `[RV-2]`: a tx that a branch block included is confirmed, not dropped. `[RV-8]`: a `Manual` chain must not have a current slot behind its own tip.

## 11. Chain selection and rollback

### 11.1 Chain selection

- Chain selection MUST pick the longest chain in the block tree. `[CS-1]`
- When two chains have the same height, chain selection MUST keep the current selected chain. `[CS-2]`
- Chain selection MUST NOT switch to a chain whose fork point is deeper than k. `[CS-3]`
- After a switch, the current slot MUST be `max(current slot, new tip slot)`. `[CS-4]`
- After a switch, the Emulator MUST handle each orphan tx by the `orphanTxs` policy. `[CS-5]`
- After a switch, the Emulator MUST run revalidation. `[CS-6]`
- Chain selection MUST run after every change to the block tree: a forge, a rollback and a pruning. `[CS-7]`

*Example, illustrative.* Selected chain G–1–2–3. A test forges block 2' on block 1:

| Step | Selected chain | Why |
|---|---|---|
| forge 2' on 1 | G–1–2–3 | the branch has height 2, which is less than 3 |
| forge 3' on 2' | G–1–2–3 | the same height: keep the current chain |
| forge 4' on 3' | G–1–2'–3'–4' | the branch is longer |

**Why:** Praos picks the longest chain and breaks ties for the chain it already has. So a fork happens only when a test makes a longer branch. That is how a real dApp sees it.

### 11.2 Orphan txs

- With `ReturnToMempool`, the Emulator MUST put the orphan txs before the existing mempool txs. `[OR-1]`
- With `ReturnToMempool`, the orphan txs MUST keep their order on the old chain. `[OR-2]`
- With `ReturnToMempool`, the status of an orphan tx MUST be `Pending` until revalidation decides. `[OR-3]`
- With `Drop`, the status of an orphan tx MUST be `RolledBack`. `[OR-4]`
- In the `Instant` preset, the forge of `[FG-9]` MUST run after the orphan txs return to the mempool. `[OR-5]`

*Example, illustrative.* The old chain had block 2 with txs A, B. The new chain has a rival tx R that spends one input of B. The mempool held C:

| Policy | Mempool after `[OR-1]` | After revalidation | Statuses |
|---|---|---|---|
| `ReturnToMempool` | A, B, C | A, C | A `Pending`, B `Dropped(InputsSpent)`, C `Pending` |
| `Drop` | C | C | A `RolledBack`, B `RolledBack`, C `Pending` |

**Why:** `ReturnToMempool` is not node behaviour. A node does not put back the txs of a rolled-back block. But other nodes in the network still hold those txs in their mempools, so on mainnet most orphan txs land again in a later block. `ReturnToMempool` models that network effect, and `Drop` models the worst case. Do not "fix" `ReturnToMempool` to match one node.

### 11.3 Rollback

**Purpose:** remove the last n blocks, as if the node adopted a fork whose blocks the test forges next.
**Who:** a caller of `rollback`.
**Trigger:** any time.

| Input | Output |
|---|---|
| `n: Int` | `Either[RollbackError, ChainPoint]` of the new tip |

- `rollback(n)` MUST reject `n < 1`, with `RollbackError.NotPositive`. `[RB-1]`
- `rollback(n)` MUST reject `n > k`, with `RollbackError.TooDeep`. `[RB-2]`
- `rollback(n)` MUST reject `n` greater than the tip height, with `RollbackError.PastGenesis`. `[RB-3]`
- `rollback(n)` MUST remove the last n blocks of the selected chain, and every block that descends from them, from the block tree. `[RB-4]`
- `rollback(n)` MUST set the tip to the block of depth `n + 1`, or to genesis, before chain selection. `[RB-5]`
- `rollback(n)` MUST return the tip after chain selection. `[RB-9]`

*Example, illustrative,* for `[RB-9]`. Selected chain G–1–2–3–4. A branch G–1–2' exists. `rollback(3)` removes 2, 3 and 4. The tip is 1, at height 1. The branch 2' has height 2, so chain selection switches to it. `rollback(3)` returns the point of 2'.
- `rollback(n)` MUST NOT change the current slot. `[RB-6]`
- After `rollback(n)`, the Emulator MUST handle the txs of the removed blocks as orphan txs (section 11.2). `[RB-7]`
- After `rollback(n)`, the Emulator MUST run revalidation. `[RB-8]`

*Example, illustrative,* for `[OR-5]` with `[RB-7]`. In the `Instant` preset with `ReturnToMempool`, `rollback(1)` removes block 3 with tx A, then the forge puts A in a new block 3. The UTxO set looks the same, and the stream sees `RollBackward` then `RollForward`. To see A disappear, use `Drop`.

**Why:** `[RB-4]`: a removed block must leave the tree. Otherwise the old chain stays longer than the new tip, and the next chain selection switches back. `[RB-6]`: time does not move back in a real rollback.

## 12. Tx status and ChainSync

`TxStatus` is one of:

| Case | Meaning |
|---|---|
| `Pending` | in the mempool |
| `Confirmed(depth, point)` | in a block on the selected chain |
| `Dropped(reason)` | removed from the mempool. `reason` is `Expired`, `InputsSpent` or `Invalid(error)`. |
| `RolledBack` | in a removed block, and the `Drop` policy applied |
| `NotFound` | never seen |

- The Emulator MUST provide `txStatus(hash): TxStatus`. `[TS-1]`
- `Confirmed.depth` MUST be the depth of the block at the moment of the call. `[TS-2]`
- `checkTransaction` MUST map `Pending` to `Pending` and `Confirmed` to `Confirmed`. `[TS-3]`
- `checkTransaction` MUST map every other case to `NotFound`. `[TS-4]`
- `appliedTxLog` MUST return the imported log, then the txs of the selected chain in order. `[TS-5]`
- The `slot` of an `AppliedTx` MUST be the slot at which the ledger last validated the tx. `[TS-6]`
- The Emulator MUST derive `Confirmed` from membership of the selected chain, not from a stored status. `[TS-7]`

*Example, illustrative,* for `[TS-6]`. The tip is at slot 50:

| Case | `AppliedTx.slot` |
|---|---|
| `Instant`, `setSlot(40)`, then `submit` | 40, as today. The block slot is 50 by `[FG-2]`. |
| `Manual`, current slot 60, `produceBlock()` | 60, the block slot, by `[FG-3]` |
| branch block at slot 55 | 55 |

**Why:** `TransactionStatus` has no payload, and it lives in a MiMa-stable package. A new type avoids a breaking change. `[TS-6]` keeps `EmulatorParityTest` "restoring from a log" exact. `[TS-7]`: a tx can reach the selected chain without the mempool, in a branch block. `[TS-5]`: the selected chain is the log, so a rollback removes entries. The imported log is the `appliedTxLog` argument of `EmulatorState.initial`, so a restored Emulator keeps it.

### 12.1 ChainSync

- The Emulator MUST provide `diff(from: ChainTip, to: ChainTip): Seq[ChainEvent]`. `[SY-1]`
- `diff` MUST return one `RollBackward` to the common ancestor when `from` is not an ancestor of `to`. When the ancestor is genesis, the target MUST be `ChainTip.origin`. `[SY-2]`
- `diff` MUST then return one `RollForward` per block from the ancestor to `to`, in height order. `[SY-3]`
- Each `RollForward` MUST carry an `AppliedBlock` with the `Block`. `[SY-4]`
- `StreamingEmulator` MUST emit the events of `diff` after each transition that changes the tip. `[SY-5]`
- `StreamingEmulator` MUST pass k to `SubscriptionHub` as its rollback horizon. `[SY-6]`
- `StreamingEmulator` MUST NOT build its own blocks. `[SY-7]`
- `StreamingEmulator` MUST report a mempool tx as `Pending`. `[SY-8]`

**Why:** `[SY-6]`: today `StreamingEmulator` passes `securityParam = 0`, so the hub refuses every rollback. `[SY-7]`: today it builds one fake block per tx, with hashes from the height. Real blocks replace them.

## 13. Surfaces

### 13.1 Scala and JVM

- `EmulatorBase` MUST expose `submit`, every `produceBlock` overload, `rollback`, `txStatus`, `findUtxos(query, view)`, `mempoolTxs`, `tip` and `diff`. `[SF-1]`
- `pollForConfirmation` MUST NOT sleep. `[SF-2]`
- With `ProduceBlock`, `pollForConfirmation` MUST forge until the tx is no longer `Pending`, at most `maxAttempts` blocks. `[SF-3]`
- With `WaitOnly`, `pollForConfirmation` MUST return the status as it is. `[SF-4]`
- `EmulatorJavaApi` MUST expose the members of `[SF-1]` with Java types. `[SF-5]`

**Why:** `[SF-2]`: the Emulator has virtual time, and a sleep only slows tests. `[SF-3]`: off-the-shelf dApp code calls `awaitTx`, so with `Manual` it must still finish. `[SF-4]`: a test that drives blocks itself can observe a pending tx.

### 13.2 JS

- `JEmulator` options MUST accept `blockProduction`, `awaitTx`, `orphanTxs` and `securityParam`, as strings and a number. `[SF-6]`
- `JEmulator` MUST expose `produceBlock`, `produceBlockOn`, `rollback`, `getMempool`, `getTip`, `getTxStatus` and `awaitTx`. `[SF-7]`
- `getTransactionStatus` MUST return `"Pending"`, `"Confirmed"`, `"Dropped"`, `"RolledBack"` or `"NotFound"`. `[SF-8]`
- `getUtxos` MUST accept an optional view: `"tip"`, `"mempool"` or `{ stable: n }`. `[SF-9]`
- `scalus.d.ts` MUST be regenerated, not edited by hand. `[SF-10]`

### 13.3 ImmutableEmulator

- `ImmutableEmulator` MUST wrap an `EmulatorState`. `[SF-11]`
- `ImmutableEmulator` MUST keep its validators and mutators as parameters. `[SF-12]`
- The existing `ImmutableEmulator` factories MUST keep running only `PlutusScriptsTransactionMutator`. `[SF-13]`
- `ImmutableEmulator` MUST expose `produceBlock` and `rollback` as pure methods. `[SF-14]`

**Why:** `[SF-11]` removes the second copy of the state machine. `[SF-13]`: `Scenario` depends on today's mutator list.

## 14. Impact on the Lucid provider spec

The 2026-10-07 spec (`2026-10-07-scalus-emulator-provider-design.md`) assumes no mempool. With `Instant`, nothing in it changes. With `Manual`:

| Rule there | Change |
|---|---|
| `[PR-14]` `awaitTx` = `hasTx` | `awaitTx` calls `JEmulator.awaitTx`. |
| `[PR-15]` `awaitTx` does not move the clock | With `ProduceBlock`, `awaitTx` can move the current slot by `[FG-1]`. |
| `[TM-3]`, `[TM-5]` 20 slots per block | `blockHeight` can return `getTip().height`. `awaitBlock(n)` can tick `20 * n - 1` slots, then forge. |
| `[PR-18]` to `[PR-20]` | `confirmations` comes from `Confirmed.depth`. |

This spec does not change the Lucid repo. The table is input for a revision of that spec.

## 15. Compatibility

- Every existing Emulator test MUST pass unchanged with `ChainConfig.instant`. `[CP-1]`
- The MiMa filters that this change needs MUST be listed in the PR, one line each, with the reason. `[CP-2]`
- Deprecated members MUST use `@deprecated("…", "1.4.1")`. `[CP-3]`

**Why:** `[CP-1]` is the promise of `[CF-4]`. `[CP-2]`: `EmulatorState` changes its fields, and its `unapply` and `_1` to `_6` are public.

## 16. Tests

- A property test MUST check that the tip snapshot equals the fold of the selected chain's blocks from genesis. `[TE-1]`
- A property test MUST check that the mempool snapshot equals the tip snapshot plus the mempool txs, applied in order. `[TE-2]`
- A property test MUST check that `rollback(n)` and a re-forge of the same txs at the same slots give the same block ids and the same state. `[TE-3]`
- A property test MUST check that two runs of the same operations give the same block ids. `[TE-4]`
- A scenario test MUST cover the delay case: after `submit`, `findUtxos` does not change. After `produceBlock`, it does. `[TE-5]`
- A scenario test MUST cover a TTL drop at the boundary of `[RV-4]`. `[TE-6]`
- A scenario test MUST cover a rival double spend on a longer branch, with `ReturnToMempool` and with `Drop`. `[TE-7]`
- A scenario test MUST cover a mempool that needs two blocks, as in the example of `[FG-4]`. `[TE-8]`
- A stream test MUST check that a branch switch delivers `RollBackward` to a subscriber. `[TE-9]`
- A JS test MUST run the delay case of `[TE-5]` through a Lucid-style provider and `awaitTx`. `[TE-10]`
- A test MUST check `[BH-1]` against the header and the block hash of a mainnet block. `[TE-11]`
- A test MUST recompute `blockBodyHash` of a decoded mainnet block and compare it with its header, for `[BH-3]`. `[TE-13]`
- Tests MUST cite the rule IDs they check, as `// spec [XX-n]`. `[TE-12]`

## 17. Work plan

Each milestone ends green on `sbtn quick`, with the gate below. Each milestone is one or more commits on `feat/emulator-chain`.

| M | Content | Rules | Gate |
|---|---|---|---|
| 0 | `BlockHeader.hash`, fix of `Block.hash`, body hash function | BH-1, BH-2, BH-3 | TE-11, TE-13 |
| 1 | Pure core: `LedgerSnapshot`, block tree, forge from the mempool, `Instant` preset, new `EmulatorState`, direct edits; old API on top | NL, BH-4–9, CF, ST, DE, MP, FG-1–10, VW-1–2, TS-5–7 | CP-1; TE-1, TE-4; MiMa filter list |
| 2 | `Manual` preset, block limits, mempool capacity, revalidation, `TxStatus`, views, `pollForConfirmation` | MP-4, VW-3–9, RV, TS-1–4, SF-1–4 | TE-2, TE-5, TE-6, TE-8 |
| 3 | Branch forge, chain selection, rollback, orphan policy, pruning | FG-11–21, CS, OR, RB | TE-3, TE-7 |
| 4 | ChainSync: `diff`, `StreamingEmulator` on real blocks | SY | TE-9; `StreamingEmulatorConformanceTest` |
| 5 | Java API, `JEmulator`, `scalus.d.ts`, `ImmutableEmulator` wrapper | SF-5–14 | TE-10; `ci-js` |
| 6 | Site docs, `scalus:local-development` skill, CHANGELOG | – | review |

Files that M1 to M5 touch:

- `scalus-core/shared/.../ledger/Block.scala`, `BlockHeader.scala`
- `scalus-cardano-ledger/shared/.../node/EmulatorBase.scala`, plus new files in `node/` for the block tree, the mempool and the forge
- `scalus-cardano-ledger/jvm/.../node/Emulator.scala`, `EmulatorJavaApi.scala`
- `scalus-cardano-ledger/js/.../node/Emulator.scala`, `JEmulator.scala`
- `scalus-cardano-ledger/shared/.../node/stream/StreamingEmulator.scala`
- `scalus-testkit/shared/.../testing/ImmutableEmulator.scala`
- `build.sbt` (MiMa filters)

## 18. Roadmap

Each item gets its own spec. None of them has code in this spec.

1. **Epoch transition.** NEWEPOCH in `tick`: POOLREAP, promotion of `futureStakePoolParams`, deposit refunds, SNAP, rewards, reserves and treasury. Test target: the 13 EPOCH and 24 DELEG vector dirs that `vectors.tar.gz` already ships but no test runs.
2. **Governance.** The 14 missing GOV checks, proposals, RATIFY, ENACT, protocol parameter updates, committee state. Test target: the GOV (56), RATIFY (46) and ENACT (16) vector dirs.
3. **PV11 tx rules.** Dijkstra failures, sub-transactions, Plutus V4 evaluation.
4. **Network realism.** A seeded random block schedule (f = 0.05), forging on the wall clock for JS dev servers, a tx that never reaches the forging node.
5. **N2C server.** LocalTxSubmission, LocalStateQuery, LocalTxMonitor and ChainSync over a socket, so cardano-cli, Ogmios and Kupo can connect.

## 19. Decisions

| # | Decision | Rejected |
|---|---|---|
| D1 | Scope: sub-projects 1 and 2 in full, 3 to 5 as a roadmap | 1, 2 and 3 in one spec; 1 alone |
| D2 | One engine with an `Instant` default preset | two emulators; a realistic default |
| D3 | Blocks only on an explicit call | a seeded virtual schedule; the wall clock |
| D4 | `awaitTx` is configurable, default `ProduceBlock` | always forge; never forge |
| D5 | Queries read the tip; other views are opt-in | tip only; the lucid-evolution half model |
| D6 | Block tree, automatic chain selection, `rollback(n)` | linear rollback only; random forks |
| D7 | Orphan tx policy, default `ReturnToMempool` | always drop; always re-add |
| D8 | Architecture A: a chain layer inside `EmulatorState`, shaped as node layers | a wrapper class; event-sourced replay |
| D9 | `Instant` allows blocks with the same slot | `slot + 1` per block; one open block per slot |
