# Scalus Emulator provider for Lucid Evolution – design

Status: draft, for review. Date: 2026-10-07.

## 1. Purpose and scope

Lucid users can run their tests against the Scalus ledger emulator instead of the Lucid Emulator. The Scalus ledger emulator applies full ledger rules, Plutus evaluation and the PV 11 cost models.

In scope:

- `ScalusEmulator`, a Lucid `Provider` that wraps the Scalus `Emulator` (sections 3 to 10).
- The Scalus changes that this provider needs (section 13). They ship in a Scalus release before the Lucid work starts.
- Tests, divergence reports and docs (sections 11 and 12).

Out of scope:

- A Lucid adapter inside the Scalus npm package. Decision A: the adapter lives in Lucid.
- The stale cost models in `PROTOCOL_PARAMETERS_DEFAULT`. A separate Lucid PR updates them (section 14).
- CIP-159 direct deposits. Scalus does not model PV 12 yet, and the provider needs no API for them (section 13.4).

## 2. Definitions

| Term | Meaning |
|---|---|
| Scalus Emulator | The `Emulator` class of the `scalus` npm package. |
| ScalusEmulator | The Lucid class this spec defines. It wraps one Scalus Emulator. |
| Lucid Emulator | The `Emulator` class in `packages/provider/src/emulator.ts`. |
| manual clock | The Scalus Emulator slot moves only when a caller moves it. |
| wall clock | The Scalus Emulator slot follows `Date.now()`, forwards only. |
| block | 20 slots, as in the Lucid Emulator. |
| errorRule | The `SubmitResult.errorRule` string of the Scalus Emulator. |
| emulator brand | The property `Symbol.for("@lucid-evolution/provider/Emulator")` set to `true`. |
| divergence | A test that passes against the Lucid Emulator and fails against ScalusEmulator. |

## 3. Packaging

- The module MUST be the subpath export `@lucid-evolution/scalus-uplc/emulator`. `[PK-1]`
- The subpath MUST build as ESM only. `[PK-2]`
- The subpath MUST import `scalus` statically. `[PK-3]`
- The root export of `scalus-uplc` MUST keep its ESM and CJS builds. `[PK-4]`
- The root export MUST NOT import the emulator module. `[PK-5]`
- `scalus-uplc` MUST declare exactly one `scalus` version range. `[PK-6]`

**Why:** Node loads an ESM module once per resolved path, and pnpm and npm keep one copy of a package for one compatible range. One package with one range gives one `scalus.js` on disk and in memory. A static import makes the constructor synchronous, so `new ScalusEmulator(accounts, params)` works where `new Emulator(accounts)` does. `[PK-5]` keeps the evaluator users' CJS path unchanged. `[PK-4]`: every Lucid package builds ESM and CJS, and `@lucid-evolution/lucid` will depend on `scalus-uplc`. CJS adds about 4.5 KB, and `scalus.js` is shared, not bundled.

## 4. Construction

### 4.1 Manual-clock constructor

- Signature: `new ScalusEmulator(accounts: EmulatorAccount[], protocolParameters: ProtocolParameters, treasury?: Lovelace, options?: ScalusEmulatorOptions)`. `[CO-1]`
- `accounts` and `treasury` MUST mean what they mean for the Lucid Emulator. `[CO-2]`
- *Withdrawn.* `protocolParameters` has no default. `[CO-3]`
- `treasury` MUST default to `0n`. `[CO-4]`
- ScalusEmulator MUST create the Scalus Emulator with network `"testnet"`. `[CO-5]`
- ScalusEmulator MUST use `SlotConfig(t0, 0, 1000)`, where `t0` is `Date.now()` at construction. `[CO-6]`
- ScalusEmulator MUST start the Scalus Emulator at slot 0. `[CO-7]`
- A manual-clock ScalusEmulator MUST carry the emulator brand. `[CO-8]`

`ScalusEmulatorOptions`:

| Field | Type | Meaning |
|---|---|---|
| `pools` | `PoolId[]` | Bech32 pool ids to register at start (`[SC-8]`). |

**Why:** `[CO-6]` and `[CO-7]` copy the Lucid Emulator: slot 0 starts at construction time, and one slot is one second. Lucid reads only `slot` and `now()` from a branded provider (`slotConfig.ts:32`, `LucidEvolution.ts:270`). So `[CO-8]` makes Lucid use the emulator clock, and Lucid itself does not change. `[CO-1]` requires `protocolParameters`: the only default available, `PROTOCOL_PARAMETERS_DEFAULT`, carries stale cost models (166/175/297 entries against 332/332/350 on PV 11). A silent default would test against parameters that no network runs.

### 4.2 Genesis UTxOs

- ScalusEmulator MUST create one UTxO for each account, at `txHash = "00".repeat(32)` and `outputIndex` equal to the account's index. `[CO-9]`
- ScalusEmulator MUST throw when an account sets more than one of `outputData.hash`, `outputData.asHash` and `outputData.inline`. `[CO-10]`
- For `outputData.asHash`, the output MUST carry the hash of that datum. `[CO-11]`
- For `outputData.asHash`, ScalusEmulator MUST add that datum to the Scalus Emulator `datums`. `[CO-12]`

**Why:** `[CO-9]` to `[CO-11]` copy the Lucid Emulator. `[CO-12]` is new: `getDatum` then finds the datum, as a chain indexer would. The Lucid Emulator returns `undefined` there.

### 4.3 Wall-clock factory

- Signature: `ScalusEmulator.forNetwork(network: "Mainnet" | "Preprod" | "Preview", options: { utxos: UTxO[]; protocolParameters?: ProtocolParameters; treasury?: Lovelace } & ScalusEmulatorOptions)`. `[CO-13]`
- `forNetwork` MUST default `protocolParameters` to `protocolParameters(network)`. `[CO-13a]`
- `forNetwork` MUST take network and slot config from the matching `CardanoInfo` preset. `[CO-14]`
- `forNetwork` MUST start at the slot that contains `Date.now()`. `[CO-15]`
- A wall-clock ScalusEmulator MUST NOT carry the emulator brand. `[CO-16]`
- `protocolParameters(network)` MUST return the `CardanoInfo` preset of `network`, mapped by `[PP-4]`. `[CO-17]`

**Why:** A test written for Blockfrost calls `Lucid(provider, "Preprod")`. Lucid then takes time from `Date.now()` and the static slot config of `"Preprod"`. `[CO-16]` keeps that path. With the brand, Lucid would read the emulator clock instead. `[CO-13a]`: here the network names the parameters, so the default is not hidden.

*Example, illustrative,* for `[CO-1]` and `[CO-17]`:

```ts
new ScalusEmulator(accounts, protocolParameters("Preprod")); // live PV 11 preset from Scalus
new ScalusEmulator(accounts, PROTOCOL_PARAMETERS_DEFAULT);    // what the Lucid Emulator uses
```

## 5. Time

### 5.1 Manual clock

- `slot` MUST return the Scalus Emulator `getSlot()`. `[TM-1]`
- `now()` MUST return the Scalus Emulator `getTime()`. `[TM-2]`
- `blockHeight` MUST return `Math.floor(slot / 20)`. `[TM-3]`
- `awaitSlot(n = 1)` MUST advance the Scalus Emulator by `n` slots. `[TM-4]`
- `awaitBlock(n = 1)` MUST advance the Scalus Emulator by `20 * n` slots. `[TM-5]`

*Example, illustrative.* Constructed at `t0`:

| Call | `slot` | `now()` | `blockHeight` |
|---|---|---|---|
| (none) | 0 | t0 | 0 |
| `awaitBlock(3)` | 60 | t0 + 60 000 | 3 |
| `awaitSlot(5)` | 65 | t0 + 65 000 | 3 |
| `awaitSlot(15)` | 80 | t0 + 80 000 | 4 |

### 5.2 Wall clock

- Before `submitTx`, a wall-clock ScalusEmulator MUST move the Scalus Emulator to the slot that contains `Date.now()`. `[TM-6]`
- Before `evaluateTx`, a wall-clock ScalusEmulator MUST do the same. `[TM-7]`
- A wall-clock ScalusEmulator MUST NOT move the clock backwards. `[TM-8]`
- On a wall-clock ScalusEmulator, `awaitSlot` and `awaitBlock` MUST throw. `[TM-9]`

*Example, illustrative.* The tx has `validFrom(Date.now())`, built 3 s after construction:

| Mode | Result |
|---|---|
| no sync (the Scalus Emulator as it is today) | `TransactionExpired` |
| wall clock, `[TM-6]` | accepted |

**Why:** In a Blockfrost-style test, Lucid builds validity intervals from `Date.now()`. The ledger slot must follow the wall clock, or a lower bound lands in the future. `[TM-9]`: if a test moved the clock ahead, Lucid would keep building intervals from the wall clock, and they would be in the past. Throwing is clearer than that silent mismatch.

*Implementation status:* `[SC-12]` moves `[TM-6]` to `[TM-8]` into the Scalus Emulator. Until then, ScalusEmulator calls `setTime` itself.

## 6. Provider methods

- `getProtocolParameters` MUST return the parameters mapped back from the Scalus Emulator (section 7). `[PR-1]`
- `getUtxos(address)` MUST query `getUtxos({ address })`. `[PR-2]`
- `getUtxos(credential)` MUST query `getUtxos({ paymentCredential, paymentCredentialType })`. `[PR-3]`
- `paymentCredentialType` MUST be `"key"` for `Key` and `"script"` for `Script`. `[PR-4]`
- `getUtxosWithUnit` MUST add `unit` to the same filter. `[PR-5]`
- `getUtxosByOutRef` MUST query `getUtxos({ outRefs })`. `[PR-6]`
- `getUtxoByUnit` MUST query `getUtxos({ unit, limit: 2 })`. `[PR-7]`
- `getUtxoByUnit` MUST throw `"Unit needs to be an NFT or only held by one address."` for two matches. `[PR-8]`
- `getUtxoByUnit` MUST resolve to `undefined` for no match. `[PR-9]`
- `getDelegation` MUST return `{ poolId, rewards }`, with a bech32 `pool1…` id or `null`. `[PR-10]`
- `getRewardAccount` MUST return `{ registered, poolId, rewards }` from `getAccount` (`[SC-10a]`). `[PR-11]`
- `getDatum` MUST return the hex of the Scalus Emulator `getDatum`. `[PR-12]`
- `getTreasury` MUST return the Scalus Emulator `getTreasury()` (`[SC-7b]`). `[PR-13]`
- `awaitTx` MUST resolve to the Scalus Emulator `hasTx(txHash)`. `[PR-14]`
- `awaitTx` MUST NOT move the clock. `[PR-15]`
- `getTransactionStatus` MUST reject with the abort reason when `options.signal` is aborted. `[PR-16]`
- `getTransactionStatus` MUST return `not_found` for a tx the Scalus Emulator never applied. `[PR-17]`
- For an applied tx, `getTransactionStatus` MUST return `confirmed`, with `slot` set to the applied slot. `[PR-18]`
- In that result, `blockHeight` MUST be `Math.floor(appliedSlot / 20)`. `[PR-19]`
- In that result, `confirmations` MUST be `currentBlockHeight - blockHeight + 1`. `[PR-20]`
- ScalusEmulator MUST NOT implement `getUtxosWithPolicy`. `[PR-21]`

*Example, illustrative,* for `[PR-18]` to `[PR-20]`. A tx applied at slot 45, current slot 100:

| Field | Value |
|---|---|
| `slot` | 45 |
| `blockHeight` | 2 |
| `confirmations` | 5 − 2 + 1 = 4 |

**Why:** The Scalus Emulator applies a tx at once, so no tx is ever `pending`. `[PR-15]`: the Lucid Emulator advances a block in `awaitTx` only to confirm its mempool. With no mempool, that time step has no purpose, and it would hide validity-interval bugs. `[PR-10]` uses bech32 because the Lucid Emulator and Blockfrost both return `pool1…` ids. `[PR-21]`: the Scalus Emulator has no policy filter, and the method is optional.

## 7. Mappings

### 7.1 UTxO

- Lucid to Scalus: ScalusEmulator MUST reuse the `toScalusUtxo` of `scalus-uplc`. `[UM-1]`
- Scalus to Lucid: ScalusEmulator MUST map each field as the table below states. `[UM-2]`
- For a reference script, ScalusEmulator MUST use `Utxo.script` (`[SC-11]`). `[UM-3]`
- A conversion MUST NOT allocate a CML object outside `withCMLScope`. `[UM-4]`

*Mapping, normative,* for `[UM-2]`:

| Lucid `UTxO` | Scalus `Utxo` |
|---|---|
| `txHash` | `txHash` |
| `outputIndex` | `outputIndex` |
| `address` | `address` |
| `assets.lovelace` | `value.coin` |
| `assets[unit]` | `quantity` of each `value.assets` entry, keyed by its `unit` |
| `datumHash` | `datumHash ?? null` |
| `datum` | hex of `inlineDatum`, or `null` |
| `scriptRef` | `script ?? null` |

*Implementation status:* before `[SC-11]` ships, `[UM-3]` decodes `scriptRef` with CML inside `withCMLScope`, and only when `scriptRef` is present.

**Why:** `toScalusUtxo` already maps a Lucid UTxO without CML, and `withScriptRef` takes Lucid's `{ type, script }` shape. One mapping in each direction keeps one place to fix.

### 7.2 Protocol parameters

- ScalusEmulator MUST pass the Lucid parameters to the Scalus Emulator as `EmulatorOptions.protocolParams` (`[SC-9]`). `[PP-1]`
- ScalusEmulator MUST map each field as the table below states. `[PP-2]`
- ScalusEmulator MUST set `protocolMajorVersion` to 11 when the Lucid parameters carry none. `[PP-3]`
- `getProtocolParameters` MUST map the Scalus Emulator parameters back through the same table. `[PP-4]`

*Mapping, normative,* for `[PP-2]` and `[PP-4]`:

| Lucid | Scalus | Lucid | Scalus |
|---|---|---|---|
| `minFeeA` | `txFeePerByte` | `maxTxExMem` | `maxTxExecutionMemory` |
| `minFeeB` | `txFeeFixed` | `maxTxExSteps` | `maxTxExecutionSteps` |
| `maxTxSize` | `maxTxSize` | `coinsPerUtxoByte` | `utxoCostPerByte` |
| `maxValSize` | `maxValueSize` | `collateralPercentage` | `collateralPercentage` |
| `keyDeposit` | `stakeAddressDeposit` | `maxCollateralInputs` | `maxCollateralInputs` |
| `poolDeposit` | `stakePoolDeposit` | `minFeeRefScriptCostPerByte` | `minFeeRefScriptCostPerByte` |
| `drepDeposit` | `dRepDeposit` | `protocolMajorVersion` | `protocolMajorVersion` |
| `govActionDeposit` | `govActionDeposit` | `costModels` | `costModels` |
| `priceMem` | `priceMemory` | `priceStep` | `priceSteps` |

*Implementation status:* before `[SC-9]` ships, `[PP-1]` patches the preprod preset's `toBlockfrostJson()` and reads it back with `fromBlockfrostJson`. A probe confirmed this works with Lucid's cost models (166/175/297 entries).

**Why:** `[PP-3]` matches `DEFAULT_PROTOCOL_MAJOR_VERSION` in the Lucid Emulator. Parameters that Lucid does not carry (governance, pool, block limits) come from the preset, never from zero.

### 7.3 Redeemer tags

- ScalusEmulator MUST map redeemer tags with `mapScalusTag` from `scalus-uplc`. `[RT-1]`

## 8. Evaluation

- `evaluateTx(tx, additionalUTxOs)` MUST call the Scalus Emulator `evaluateTx(bytes, additionalUTxOs.map(toScalusUtxo))`. `[EV-1]`
- ScalusEmulator MUST expose `evaluator: EvaluatorAdapter`. `[EV-2]`
- `evaluator.evaluate` MUST call the same Scalus Emulator `evaluateTx`. `[EV-3]`
- A script failure MUST reject with the `PlutusScriptEvaluationError` that Scalus throws. `[EV-4]`

**Why:** Lucid evaluates locally by default and never calls `provider.evaluateTx`. With `Lucid(emulator, "Custom", { evaluator: emulator.evaluator })`, completion and submission use one evaluator, with one UTxO set and one cost model. `[EV-4]` keeps the redeemer, traces and `code` that Scalus reports.

## 9. Submission

- `submitTx` MUST pass the tx bytes to the Scalus Emulator `submitTx`. `[SU-1]`
- On success, `submitTx` MUST resolve to `txHash`. `[SU-2]`
- On failure, `submitTx` MUST reject with a `ScalusEmulatorError`. `[SU-3]`
- `ScalusEmulatorError` MUST carry `errorRule`, `error` and `logs`, unchanged. `[SU-4]`
- The message of `ScalusEmulatorError` MUST start with `errorRule`. `[SU-5]`

*Example, illustrative,* for `[SU-5]`: `ValueNotConserved: Value not conserved for transaction "…"`.

**Why:** Tests assert on `errorRule`, which the Scalus d.ts declares stable. `[SU-5]` makes a plain `rejects.toThrow("FeesOk")` work as well.

## 10. Test helpers

- `distributeRewards(rewards)` MUST pay `rewards` to every account that `getStakeDistribution()` lists with a pool. `[RW-1]`
- `distributeRewards` MUST pay through the Scalus Emulator `addRewards` (`[SC-10]`). `[RW-2]`
- `distributeRewards` MUST then call `awaitBlock()`. `[RW-3]`
- ScalusEmulator MUST expose `scalus`, the wrapped Scalus Emulator. `[RW-4]`
- `snapshot()` MUST return a new ScalusEmulator over the Scalus Emulator `snapshot()`. `[RW-5]`
- `addUtxo(utxo: UTxO)` MUST add the UTxO through the Scalus Emulator `addUtxo`. `[RW-6]`
- ScalusEmulator MUST NOT expose `ledger`, `mempool`, `chain`, `datumTable` or `transactionHistory`. `[RW-7]`

**Why:** `[RW-1]` and `[RW-3]` copy the Lucid Emulator: registered and delegated accounts receive the rewards, then a block passes. `[RW-7]`: those fields are writable internals of the Lucid Emulator. Emulating them would mean a second ledger beside the Scalus one. Tests that write `emulator.ledger[...]` switch to `addUtxo`.

## 11. Divergences

- The suite run MUST record every divergence in the README, in one of two tables. `[DV-1]`
- The "API surface" table MUST list tests that use members excluded by `[RW-7]`. `[DV-2]`
- The "Ledger rules" table MUST list tests that the Scalus Emulator rejects by a ledger rule. `[DV-3]`
- Each "Ledger rules" row MUST name the test, the `errorRule` and the real-ledger rule. `[DV-4]`
- Each "Ledger rules" row MUST have a targeted test that asserts that `errorRule`. `[DV-5]`
- A divergence MUST NOT be resolved by weakening a Scalus check. `[DV-6]`
- A divergence MAY be resolved by fixing the test, when the test breaks a real-ledger rule. `[DV-7]`

Known divergences, from code reading and probes, before the suite run:

| Behaviour | Lucid Emulator | Real ledger | Scalus Emulator after section 13 |
|---|---|---|---|
| `ttl` bound | `slot > ttl` rejects | `slot >= ttl` rejects | as the real ledger |
| Extra vkey witness | rejects | accepts | accepts |
| Treasury donation | added at once | added at the epoch boundary | at the epoch boundary |
| Delegation to an unregistered pool | accepts | rejects | rejects |
| Fee, value and min-ADA rules | not checked | checked | checked |
| Phase-2 scripts | not run | run | run |

## 12. Tests

- A vitest project in `scalus-uplc` MUST run the emulator test files of `packages/lucid/test` and `packages/provider/test`. `[TS-1]`
- That project MUST replace the Lucid Emulator with ScalusEmulator through a `resolve.alias` shim. `[TS-2]`
- The shim MUST pass `PROTOCOL_PARAMETERS_DEFAULT` when a test passes no parameters. `[TS-2a]`
- The shim MUST also catch the relative import `../../../provider/src` in `service.ts`. `[TS-3]`
- That project MUST pass `evaluator: emulator.evaluator` to every `Lucid(...)` call it controls. `[TS-4]`
- The project MUST exclude `emulator-ledger-compat.test.ts` and `emulator-ledger-index.test.ts`. `[TS-5]`
- The 7 test sites that write `emulator.ledger[...]` MUST switch to an `addUtxo` helper that works on both emulators. `[TS-6]`
- Each mapping rule in section 7 MUST have a test that fails under the mutation named below. `[TS-7]`

| Rule | Mutation the test must catch |
|---|---|
| `[UM-2]`, `[UM-3]` | drop `scriptRef`; swap `datum` and `datumHash` |
| `[PP-2]` | swap `minFeeA` and `minFeeB`; swap `priceMem` and `priceStep` |
| `[RT-1]` | map one of the 6 tags wrongly |
| `[TM-5]` | advance 19 slots per block |
| `[PR-8]`, `[PR-9]` | return the first of two matches |
| `[PR-19]`, `[PR-20]` | off by one in `blockHeight` |
| `[SU-4]` | throw a plain `Error` |
| `[TM-6]`, `[TM-8]` | no sync; sync backwards |

**Why:** `[TS-2]` runs the existing tests unchanged, so a pass means drop-in. `[TS-2a]` gives the suite the parameters the Lucid Emulator uses, so a divergence comes from the rules, not from the parameters. `[TS-5]`: those files test only the internals of the Lucid Emulator. `[TS-4]` follows decision 2: the suite runs with the emulator's own evaluator.

## 13. Scalus changes

They ship in Scalus 1.4, before the Lucid build starts (decision 7). Section 16 gives the order.

### 13.0 Direction and compatibility

- Each change MUST move the ledger state toward the shape of the Haskell ledger state. `[SC-19]`
- Each change MUST use the Haskell type and field names where a Haskell counterpart exists. `[SC-19a]`
- A change MUST NOT add a part of the Haskell state that no rule or test of this spec reads. `[SC-19b]`
- 1.4 MAY break the ledger-rule APIs. `[SC-20]`
- A changed public API SHOULD keep its old form as a deprecated alias. `[SC-20a]`
- Each breaking change MUST be listed in the 1.4 changelog, with its migration. `[SC-20b]`

**Why:** The long-term direction is a faithful mirror of the Haskell ledger state, for a fast in-process emulator. This spec does not build that mirror. Each step is small and useful on its own, and `[SC-19]` makes the steps add up without a rewrite. `[SC-19b]` keeps out state that nothing exercises, so it cannot drift silently.

*Breaking changes in 1.4, and how each stays compatible:*

| Change | Rule | Compatible path |
|---|---|---|
| `DelegationState` stores one record per account | `[SC-15]` | The old maps stay as deprecated read-only views. A deprecated `apply` and constructor take the 4 old maps; the `apply` keeps the 1.3 defaults, an approved exception to the interop rule in `CLAUDE.md`. `copy` and the product accessors break. |
| `DatumOption.Inline` keeps the datum bytes | `[SC-13b]` | `Inline(data)` and `case Inline(d)` keep working, with `d: Data`. |
| The emulator datum store keeps the datum bytes | `[SC-13k]` | `EmulatorState.binaryDatums` holds `KeepRaw[Data]`. `EmulatorState.datums` stays as a deprecated decoded view. `EmulatorBase.datums` is unchanged. A deprecated testkit `ImmutableEmulator.apply` takes the old parameter list. |
| `UtxoEnv` carries `treasury` | `[SC-6]` | A new field. A deprecated `apply` and constructor keep the old 4-argument form, with `Coin.zero`. No default parameter, per the interop rule in `CLAUDE.md`. `copy` breaks. |
| `StakeCertificatesException` reports unregistered delegation targets | `[SC-4]`, `[SC-5]` | 2 new fields. A deprecated `apply` and constructor take the 6 old ones. `copy` breaks. |
| Withdrawals keep the account | `[SC-3]` | Behaviour change only. |
| Certificates and withdrawals ignore a phase-2-failed tx | `[SC-3b]`, `[SC-3e]` | Behaviour change only. |
| `DefaultMutators.all` iterates in ledger order | `[SC-3a]` | The type stays `Set`. |
| One mutator applies all certificates, in tx order | `[SC-22]` | `StakeCertificatesMutator`, `StakePoolCertificatesMutator` and `VotingCertificatesMutator` stay, deprecated. `DefaultMutators.all` no longer lists them. A mutator set with `CertsMutator` skips them. |

### 13.0a Conformance comparison

- The conformance test MUST compare the resulting state with `newLedgerState`, for every vector that expects success. `[SC-14]`
- The comparison MUST report each mismatching field by name. `[SC-14a]`
- A field that Scalus does not model yet MAY be excluded, in one explicit list. `[SC-14b]`
- Each excluded field MUST name the step that will remove it from the list. `[SC-14c]`
- An excluded field that no step of this spec models MUST say so in its entry. `[SC-14d]`

*Fields compared:* `utxo`, `deposited`, `fees`, `donation`, `instantStake`, the accounts, the pools and the DReps. `govState` starts on the exclusion list.

*Implementation status:* the first run (523 cases) excluded `govState`, `instantStake`, `deposited`, `dreps.delegates` and `dreps.expiry` under `[SC-14d]`: no rule writes them yet. It excluded `accounts` for the "Not validating CERT script" vectors, removed by `[SC-3e]`. The first run also found 258 `utxo` mismatches, all from two bugs in the test decoder (credential tags, multi-assets), since fixed.

**Why:** Today the vectors check pass or fail only. `newLedgerState` is decoded and never compared (`CardanoLedgerVectors.scala:152`). The comparison turns guesses about wrong transitions into a list of failing fields. The vectors carry a `LedgerState` only, so they cannot check the treasury or the epoch boundary. Targeted tests cover those.

### 13.0b Accounts

- `DelegationState` MUST store `accounts: Map[Credential, ConwayAccountState]`. `[SC-15]`
- `ConwayAccountState` MUST carry `balance`, `deposit`, `stakePoolDelegation: Option[PoolKeyHash]` and `dRepDelegation: Option[DRep]`. `[SC-15a]`
- An account MUST count as registered if and only if `accounts` contains its credential. `[SC-16]`
- `DelegationState` MUST keep `rewards`, `deposits`, `stakePools` and `dreps` as deprecated read-only views. `[SC-17]`
- The `DelegationState` companion MUST keep a deprecated `apply` that takes the 4 old maps. `[SC-17a]`
- `ConwayAccountState` and its Haskell-state CBOR codec MUST move from conformance test code to `shared` main code. `[SC-18]`
- The conformance decoder MUST decode into that shared type. `[SC-18a]`

**Why:** Today registration is "the `rewards` map contains the credential". That is the root cause of the withdrawal bug of 13.1. Haskell keeps one record per account (`ConwayAccountState`, `C/State/Account.hs:60`). The conformance code already decodes it (`LedgerState.scala:538`), in both the old UMap and the new map formats. `getAccount` (`[SC-10a]`) becomes one map lookup.
- Deregistering a DRep MUST clear the `dRepDelegation` of every account that delegates to it. `[SC-21]`

**Why `[SC-21]`:** Haskell GOVCERT does it on `ConwayUnRegDRep` (`C/Rules/GovCert.hs:246-254`), so `getAccount` must not report a DRep that no longer exists. Haskell finds the accounts through the DRep's `drepDelegs`; no Scalus rule writes `DRepState.delegates` yet (13.0a), so Scalus finds them by their `dRepDelegation`.

### 13.1 Withdrawals (bug)

- `DefaultMutators.all` MUST include `CertsMutator`. `[SC-1]`
- `applyWithdrawals` MUST set the balance to `balance - amount`. `[SC-2]`
- `applyWithdrawals` MUST keep the account registered. `[SC-3]`
- `[SC-1]`, `[SC-2]` and `[SC-3]` MUST ship in one change. `[SC-3d]`
- The mutator pipeline MUST apply withdrawals before certificates, by an explicit order. `[SC-3a]`
- The mutator pipeline MUST NOT apply withdrawals when `isValid` is false. `[SC-3b]`
- The mutator pipeline MUST NOT apply certificates when `isValid` is false. `[SC-3e]`
- When `isValid` is false, the validators of withdrawals and certificates MUST NOT run. `[SC-3f]`
- The mutator pipeline MUST apply the certificates in one pass, in tx order, each to the state that the earlier ones leave. `[SC-22]`
- Unless the protocol major version is 9, a validator MUST reject a tx with `isValid` true that withdraws from a key-hash account with no DRep delegation in the state before the tx's certificates, with errorRule `WithdrawalsNotDelegatedToDRep`. `[SC-23]`
- When `isValid` is true, a validator MUST reject a tx whose reference scripts, one per UTxO in the union of its inputs and reference inputs, total more than 200 KiB, with errorRule `TxRefScriptsSizeTooBig`. `[SC-24]`
- Tests MUST cover each case of the table below. `[SC-3c]`

*Test cases, normative,* for `[SC-3c]`:

| Case | Expected result |
|---|---|
| Withdraw zero, twice, from a registered account | Both accepted. The account stays registered with balance 0. |
| Withdraw the full balance, and deregister, in one tx | Accepted. The account is gone, and the deposit is refunded. |
| Withdraw the full balance, then delegate, in a later tx | Both accepted. |
| The same 7 ADA withdrawn twice | The 2nd is rejected with `WithdrawalsNotInRewards`. |
| `isValid = false`, with a withdrawal and a certificate | Only collateral is taken. Balance and registration do not change. |

`CertsMutatorTest.scala:72` MUST change: it asserts the deletion (`rewards.contains(credential) shouldBe false`). After the fix, the account stays registered with balance 0.

*Evidence:* probe 5 withdrew the same 7 ADA twice. `getStakeReward` stayed at 7 ADA, and deregistration then failed with `non-zero reward accounts`. Cause: `rules/DefaultValidators.scala:46` omits `CertsMutator`, and `CertsValidator.scala:52` removes the account (`acc - credential`).

*History:* `5e53595df` (2026-02-02) added `CertsMutator` and left the stake rules out under `TODO: enable the stake validators and state transitions. Consider order independency and validator idempotency`. `15c8c9ded` (2026-02-04) enabled the validators and dropped the TODO. `CertsMutator` was never wired in. No commit states why.

**Why:**

- `balance - amount` is correct now, where the rules require a full drain, and under CIP-159, where partial withdrawals are allowed.
- `[SC-3d]`: the `rewards` map records which accounts are registered (`StakeCertificatesMutator.scala:56`, `:62`). Wiring `CertsMutator` with today's delete semantics would deregister an account on every withdrawal. The withdraw-zero trick would then fail on its 2nd use.
- `[SC-3a]`: mutators run in name order, and each validates again against the state of the one before it. The Haskell ledger applies withdrawals before certificates. Name sorting matches that only by accident.
- `[SC-22]`: Haskell CERTS folds CERT over the certificates one at a time (`C/Rules/Certs.hs:242-246`). One pass per kind made `[UnregDRep X, RegDRep X, VoteDeleg c X]` end with no DRep for `c`.
- `[SC-23]`: Haskell LEDGER runs `validateWithdrawalsDelegated` outside the bootstrap phase, on the cert state before CERTS, so an account can unregister and withdraw in one tx (`C/Rules/Ledger.hs:373-380`, `:473-488`).
- `[SC-24]`: Haskell LEDGER runs `validateRefScriptSize` when `isValid` is true (`C/Rules/Ledger.hs:361`, `:456-471`); it sums `txNonDistinctRefScriptsSize` (`C/UTxO.hs:183-187`) against the Conway constant `ppMaxRefScriptSizePerTxG = 200 * 1024` (`C/PParams.hs:981`). *Implementation status:* the limit stays a constant until Dijkstra. Dijkstra (protocol version 12) makes it the protocol parameter `maxRefScriptSizePerTx` (`eras/dijkstra/impl/src/Cardano/Ledger/Dijkstra/PParams.hs:475-485`). Scalus does not model Dijkstra yet.
- `[SC-3f]`: Conway LEDGER runs CERTS and the withdrawal checks only when `isValid` is true (`C/Rules/Ledger.hs:361`). A phase-2-invalid tx with a bad certificate is accepted, and only its collateral is taken.
- `[SC-3b]`, `[SC-3e]`: for a tx whose scripts fail, the ledger takes only the collateral. Today `StakeCertificatesMutator` ignores `isValid`. *Implementation status:* inferred from the code, not yet confirmed by a test.

### 13.2 Delegation targets (bug)

- `StakeCertificatesValidator` MUST reject a delegation to an unregistered pool. `[SC-4]`
- `StakeCertificatesValidator` MUST reject a vote delegation to an unregistered DRep. `[SC-5]`

*Evidence:* probe 1 delegated to `pool1nmfr5j5…` with no pool registered, and it was accepted. The real ledger rejects with `DelegateeStakePoolNotRegisteredDELEG` and `DelegateeDRepNotRegisteredDELEG`.

### 13.3 Treasury

- `UtxoEnv` MUST carry `treasury: Coin`, the treasury as of the last epoch boundary. `[SC-6]`
- A validator MUST reject a tx whose `currentTreasuryValue` differs from `env.treasury`, with errorRule `TreasuryValueMismatch`. `[SC-7]`
- That validator MUST run only when `isValid` is true. `[SC-7e]`
- Applying a tx MUST NOT change the treasury. `[SC-7d]`
- The Scalus Emulator MUST move `donation` into the treasury when the slot crosses an epoch boundary. `[SC-7a]`
- At that move, the Scalus Emulator MUST reset `donation` to zero. `[SC-7f]`
- `snapshot()` and every emulator conversion MUST keep `fees`, `donation` and the treasury. `[SC-7c]`
- JS: `EmulatorOptions.treasury?: bigint` and `Emulator.getTreasury(): bigint`. `[SC-7b]`

*Revised 2026-10-07:* `[SC-6]` first put `treasury` in `rules.State`. It now sits in the env, as in Haskell.

*Example, illustrative.* The treasury is 1000 at the start of epoch e:

| Event | `currentTreasuryValue` | Result | Treasury after |
|---|---|---|---|
| Donate 5 in epoch e | 1000 | accepted | 1000 |
| Donate 7 in epoch e | 1000 | accepted | 1000 |
| Donate 3 in epoch e | 1005 | `TreasuryValueMismatch` | 1000 |
| Boundary e to e + 1 | – | – | 1012 |

*Evidence:* probe 2 submitted `current_treasury_value` 0 and 123. Both were accepted. `State` has only `donation` (`rules/Entities.scala:26`). `snapshot()` drops `fees` and `donation` (`EmulatorBase.scala:97-108`, `418`).

**Why:** Haskell passes the treasury to LEDGER through `LedgerEnv`, built from `esChainAccountState` (`S/API/Mempool.hs:371`, `C/Rules/Ledger.hs:442-453`). The EPOCH rule moves donations at the boundary (`C/Rules/Epoch.hs:350`). So within one epoch, two donations see the same value. Other treasury changes (rewards, POOLREAP, enacted withdrawals, proposal deposits) need epoch rules that Scalus does not model yet, so `[SC-19b]` keeps them out.

### 13.4 Rewards API

- JS: `EmulatorOptions.poolRegistrations` MUST also accept `{ poolId: string }`, a hex pool key hash. `[SC-8]`
- JS: `EmulatorOptions.protocolParams?: Partial<ProtocolParamsLike>` MUST override the given fields on `info.protocolParams`. `[SC-9]`
- JS: `Emulator.addRewards(rewardAddressBech32: string, lovelace: bigint): void` MUST throw for an unregistered account. `[SC-10]`
- JS: `Emulator.getAccount(rewardAddressBech32: string)` MUST return `{ balance, deposit, poolId?, drep? }`, or `undefined` for an unregistered account. `[SC-10a]`
- JS: `StakeDistributionEntry` MUST carry `rewardAddress`, in bech32. `[SC-10b]`

**Why:**

- `[SC-8]`: a full pool certificate is heavy to build in a test.
- `[SC-9]`: this changes only the JS layer, because `ProtocolParams` lives in shared code. It reuses `JBalancer.paramsOf`, refactored to take a base instead of `JsProtocolParams.zero`.
- `[SC-10]` models an epoch reward payout. Withdraw scenarios use it with `[SC-1]` to `[SC-3]`.
- CIP-159 direct deposits and partial withdrawals arrive through `submitTx` and change the same balance. So they need no new Emulator API.
- `[SC-10b]`: `credential` has no key/script type, so a reward address cannot be built from it.

### 13.5 UTxO and datum fidelity

- JS: `Utxo.script` MUST return `{ type, script }`, the inverse of `withScriptRef`. `[SC-11]`
- In `Utxo.script`, `script` MUST be double-CBOR hex for Plutus and CBOR hex for Native. `[SC-11a]`
- `PlainUtxo` MUST carry the same `script` field. `[SC-11b]`
- The ledger MUST keep the original CBOR of an inline datum in stored outputs. `[SC-13]`
- `Utxo.fromCbor`, `withInlineDatum` and `addUtxo` MUST keep the original CBOR of an inline datum. `[SC-13a]`
- `DatumOption.Inline` MUST hold the datum as `KeepRaw[Data]`. `[SC-13b]`
- The `DatumOption` decoder MUST keep the bytes inside tag 24 as the raw bytes of that `KeepRaw`. `[SC-13c]`
- The `DatumOption` encoder MUST write those raw bytes. `[SC-13d]`
- `DatumOption.Inline(data: Data)` MUST keep working as a constructor. `[SC-13e]`
- The pattern `case DatumOption.Inline(d)` MUST keep binding `d: Data`. `[SC-13f]`
- Two `Inline` values with the same `Data` and different bytes MUST NOT be equal. `[SC-13g]`
- For two `Inline` values, `contentEquals` MUST keep comparing `Data` only. `[SC-13h]`
- For a `Hash` and an `Inline`, `contentEquals` MUST compare the hash with the `[SC-13i]` hash of the inline datum. `[SC-13l]`
- The datum hash of an inline datum MUST be the hash of its original bytes. `[SC-13i]`
- The emulator datum store MUST key an inline datum by that hash. `[SC-13j]`
- The emulator datum store MUST keep the original bytes of every datum, so `getDatum(h)` returns bytes whose hash is `h`. `[SC-13k]`
- `Script.Native` MUST keep the original CBOR of its timelock. `[SC-13m]`
- The hash and the size of a native script MUST be computed from those bytes. `[SC-13n]`

*Evidence:* probes 3 and 6. `d879811a0000000a` came back as `d8799f0aff`, both from `Utxo.fromCbor` and from an output created by a submitted tx. The cause is `DatumOption.scala`: the decoder reads the tag-24 bytes, decodes `Data`, and drops the bytes. The encoder runs `Cbor.encode(data)` again.

**Why:**

- `KeepRaw` already does this job for witness datums (`TransactionWitnessSet.scala:31`). Its `equals` compares the bytes, and its encoder writes them.
- `[SC-13c]` needs no `OriginalCborByteArray`: the bytes are the tag-24 payload itself, so `KeepRaw.unsafe(data, bytes)` is exact here.
- `[SC-13e]` and `[SC-13f]` keep the 60 existing uses of `DatumOption.Inline(` compiling. An enum case cannot define its own `apply` and `unapply`, so `DatumOption` becomes a sealed trait.
- `[SC-13g]` matches Haskell, where `MemoBytes` equality compares the bytes.
- `[SC-13m]`, `[SC-13n]`: Haskell hashes `0x00 ++` the original bytes of a timelock (`Core.hs:598-603`) and sizes it by those bytes for the fee and the 200 KiB limit (`Alonzo/Scripts.hs:519`). Re-encoding a non-canonical timelock changes its hash, so its address and witness no longer match.
- `[SC-13i]`: Haskell hashes the original bytes (`hashData` over `MemoBytes`). Hashing re-encoded `Data` gives a different key for a non-minimal datum, so `getDatum` would miss it.

*Scope decisions:*

- `withScriptRef`, `scriptHash` and `PlainScript` also accept `PlutusV4`. **Why:** `withScriptRef(utxo.script)` must type-check for every script `Utxo.script` returns (`[SC-11]`), and the ledger already supports V4 scripts.
- `EmulatorBase.binaryDatums` is `private[scalus]`. **Why:** only `JEmulator.getDatum`, the testkit's `ImmutableEmulator` and the tests read it; `getDatum` stays the public read.

*Not in this step:* the whole output is re-encoded too. `Sized[TransactionOutput]` keeps only the size, not the bytes (`Types.scala:892`). Haskell memoizes the whole `TxOut`. A later step MAY store outputs with their bytes, when a test shows a visible difference (`[SC-19b]`).

### 13.6 Clock

- JS: `EmulatorOptions.clock?: "manual" | "wall"`, with default `"manual"`. `[SC-12]`
- With `"wall"`, the Scalus Emulator MUST move to the slot that contains `Date.now()` before it validates or evaluates a tx. `[SC-12a]`
- With `"wall"`, the Scalus Emulator MUST NOT move the clock backwards. `[SC-12b]`

### 13.7 Verified, no change

- The `CardanoInfo` presets carry 332/332/350 cost model entries. Live preprod and mainnet at PV 11.0 have the same (Koios `cli_protocol_params`, 2026-10-07).

## 14. Separate Lucid change

- `PROTOCOL_PARAMETERS_DEFAULT` carries 166/175/297 cost model entries with PV 11 semantics. A separate PR SHOULD update them to the PV 11 mainnet values.

## 15. Decisions

| # | Question | Decision |
|---|---|---|
| A | Where the adapter lives | Lucid |
| 1 | Module format | ESM only |
| 2 | Evaluation in the suite | The emulator's evaluator |
| 3 | `awaitTx` | `hasTx` only, no time step |
| 4 | Lucid detection | Reuse the `Symbol.for` brand |
| 5 | Package | Subpath of `scalus-uplc` |
| 6 | Default parameters | None: `protocolParameters` is required. `forNetwork` defaults to the network's preset. |
| 7 | Order | Scalus release first |
| 8 | Scope of the Scalus work | Small steps toward the Haskell state shape. No data node, no full mirror. |
| 9 | Accounts refactor | Before the withdrawals fix, and before Lucid |
| 10 | Breaking changes | Allowed in 1.4, with deprecated aliases where possible |
| 11 | Datum bytes | `KeepRaw[Data]` in `DatumOption.Inline` |

## 16. Order of work

1. Scalus, one PR per step:
   1. 13.0a, conformance comparison. Test code only.
   2. 13.0b, accounts.
   3. 13.1, withdrawals.
   4. 13.2, delegation targets.
   5. 13.3, treasury.
   6. 13.5, `[SC-13]` to `[SC-13h]`, datum bytes.
   7. JS: 13.4, `[SC-11]` to `[SC-11b]` and 13.6.
   8. Release 1.4.
2. Lucid: sections 3 to 7, with the mapping tests of `[TS-7]`.
3. Lucid: sections 8 to 10.
4. Lucid: the suite run, and the divergence tables of section 11.
5. Lucid: the README and the changeset.
