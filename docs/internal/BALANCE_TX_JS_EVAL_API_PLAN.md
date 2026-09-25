# Integrating `balanceTx` into the js-eval-api surface

## What exists

`measure/balance-export` (worktree `.claude/worktrees/balance-export`, based on master, 2 commits)
exports the Scalus transaction balancer to JavaScript. It wraps
`TransactionBuilder.balanceContext`: evaluate scripts, set the fee, adjust one change output, to a
fixpoint, capped at 20 iterations. The caller keeps coin selection and change placement.

Measured against MeshJS on master:

| Claim | Evidence |
|---|---|
| Fee is the ledger's true minimum | 168,537 vs MeshJS's 168,581; signed tx passes `FeesOk` |
| CBOR round trip is lossless | byte-identical against a `CardanoSDKSerializer` tx, legacy array outputs |
| Bundle cost | +36,192 raw / +10,225 gzipped, **measured on the merged master** (was +18,898/+10,245 on pre-merge master) |
| `extraSigners` prices witnesses | +4,444 lovelace per signer = 101 bytes = one vkey witness; duplicates dedupe |

Two defects the measurement caught, both already fixed: `balanceFeeAndChange` never adds dummy
signatures (only `balanceContext` does), so the first version underpriced by a witness and the
signed transaction was rejected; and signers were being derived from reference inputs, which are
read-only and carry no witness.

## The redesign

This is not a port. `refactor/js-eval-api` establishes conventions the prototype violates.

| Prototype (master) | js-eval-api convention | Source |
|---|---|---|
| `utxoCborBytes: Uint8Array`, a CBOR map | `readonly (string \| Uint8Array \| Utxo)[]`, CIP-30 pairs | `JEvaluator.scala:94`; design doc, "One UTxO is `[input, output]`" |
| `cardanoInfo: JsCardanoInfo` | explicit `slotConfig`, `costModels`, `protocolMajorVersion` | design doc, "no silent default protocol version" |
| `extraSigners = js.undefined` | two overloads | CLAUDE.md interop guide; `evaluateTx` does this |
| bare `@JSExportTopLevel("balanceTx")` | namespaced object, `bindExports(this)` | `JEvaluator.scala:14-16` |
| `js.Error("balanceTx failed: …")` | typed error carrying `code` | design doc, "Branch on `code`, not on `message`" |

Reuse `evaluateTxWithin`'s UTxO parser rather than writing a second one.

**New requirement.** Balancing needs fee parameters `evaluateTx` does not. Seven measured as
load-bearing: `txFeePerByte`, `txFeeFixed`, `priceMemory`, `priceSteps`, `utxoCostPerByte`,
`collateralPercentage`, `minFeeRefScriptCostPerByte`. `network` was proven irrelevant (identical fee
declaring the same params mainnet vs preview). The `max*` limits matter only to validation.

## Steps

1. ~~Rebase onto the js-eval-api base.~~ **Done.** js-eval-api was merged to master
   (`031232016`); `measure/balance-export` is rebased onto it, 3 commits, diff preserved
   (identical hash before and after). It still compiles: `scalusCardanoLedgerJS/compile` is green,
   so no facade API the prototype uses was removed.
2. ~~Re-measure the bundle.~~ **Done, and it fits.**

   | | raw | gzip |
   |---|---|---|
   | baseline | 2,305,505 | 598,296 |
   | with `balanceTx` | 2,341,697 | 608,521 |
   | delta | +36,192 | +10,225 (+1.71%) |

   The 2.3 MiB cap is 2,411,724 bytes, so **68 KiB of headroom remains**. No cap raise needed.
   The gzipped delta is within 20 bytes of the pre-merge measurement, so dropping tzdb did not
   change what `TransactionBuilder` pulls in.

   One trap worth recording: the first attempt reported a plausible size with `EXIT=1`. The
   ts-exporter had failed on stale class dirs from the pre-rebase build, and the size came from a
   leftover bundle. `scalusJS/clean scalusCardanoLedgerJS/clean` first; always read the exit code
   before the number.
3. Add `ProtocolParamsLike` beside the existing `*Like` traits, and check the `ProtocolParams`
   handle satisfies it structurally.
4. Rewrite the export as `balancer.balanceTx(...)` with `bindExports(this)`, utxos through
   `JEvaluator.utxoMapOf`, overloads instead of defaults.
5. `TxBalancingError extends Error` with `code: "INSUFFICIENT_FUNDS" | "NOT_CONVERGED"`.
6. Validate `changeOutputIndex` against `outputs.length`, `TypeError` naming the argument.
7. **Balance a transaction with a Plutus script.** Everything measured so far is a plain payment, so
   the ExUnits path, redeemer install and `scriptDataHash` recomputation are unexercised. This is the
   reason the function exists; it is a required step, not a follow-up.
8. Port the smoke test to `__tests__/balance.test.ts` under vitest: the round-trip case, the
   unbalanced-input case, and the `extraSigners` arithmetic. The first version of this test could not
   fail, because MeshJS's transaction was already at the fixpoint. Assert that the unbalanced case
   changes the bytes and lands on the ledger minimum.
9. **Pass `maxTxExecutionUnits` as `initialBudget`, matching TxBuilder.** Settled by the reference
   implementation, not an open choice. See below.
10. Update `docs/design/js-api.md`: a surface row, and a decision paragraph on why balancing takes
   fee params and a change index.

## Answered by scalus-af

Its owner has not ruled yet, but these are its recommendations, and they are specific enough to build against.

1. **It owns `.claude/worktrees/js-eval-api`.** Use my own worktree. Branch is 12 commits, tip
   `031232016`, rebased on master `6697f2dd4`, unpushed. It rewrote history today and may again, so
   expect to rebase.
2. **Follow-up branch**, not the same branch.
3. **`balancer.balanceTx(...)`**, mirroring `evaluator.evaluateTx`. Not `tx.*`: `const tx = …` is in
   every caller and would shadow `import { tx } from "scalus"`. Call `bindExports(this)` first.
4. **`ProtocolParamsLike` record** beside the other `*Like` traits: `@TsName` trait, fields
   `number | bigint`, read via `js.Dynamic` + `longOf`/`intOf`, `TypeError` naming the field. Make
   the `ProtocolParams` handle satisfy it structurally so both work. `slotConfig`, `costModels`,
   `protocolMajorVersion` stay explicit args as on `evaluateTx`; utxos via `JEvaluator.utxoMapOf`.
5. **`TxBalancingError extends Error`** with `readonly code: "INSUFFICIENT_FUNDS" | "NOT_CONVERGED"`.
   Script failures pass through as `PlutusScriptEvaluationError` unchanged. Unreadable input is a
   `TypeError`.

Raising the bundle cap is its owner's call; it asked for the re-measured size on that branch.

## For the user

**Same branch or follow-up?** A release-scope call. The design doc scopes the surface to applying
params, evaluating scripts, and evaluating transactions. Balancing is a new capability, and
`refactor/js-eval-api` is 12 commits, unpushed, with eleven `backup/js-eval-api-pre-*` refs behind
it. Recommendation: a follow-up branched off it, so this neither bloats nor blocks that branch.

## Risks

- No script transaction has been balanced yet. Step 7 exists to close that.
- `balanceContext` handles collateral return and min-ada inside the loop, but neither has been
  exercised by a test.
- **`initialBudget`: the prototype diverges from every other caller in the codebase.** It passes
  `ExUnits(Long.MaxValue, Long.MaxValue)`, which disables budget enforcement. Everything else passes
  the protocol maximum:

  | Caller | `initialBudget` |
  |---|---|
  | `PlutusScriptEvaluator(cardanoInfo, mode)` (:403) | `protocolParams.maxTxExecutionUnits` |
  | `TxBuilder(env)` (:2122) | that overload, so the same |
  | `PlutusScriptsTransactionMutator` (:26) | `maxTxExecutionUnits` |
  | `IsValidFlagTransactionSetter` (:18) | `maxTxExecutionUnits` |
  | prototype `balanceTx` | `Long.MaxValue` |

  In `EvaluateAndComputeCost` mode the scripts of a transaction draw down one shared
  `remainingBudget` that starts at `initialBudget` (:535, decremented :639), so exceeding the
  protocol maximum fails evaluation. With `Long.MaxValue` it never fails, and the loop happily
  converges on a transaction the chain will reject. Fix: take the maximum from `ProtocolParamsLike`.
  `TxBuilder.withConstMaxBudgetEvaluator` (:2134) is the deliberate escape hatch for skipping real
  costing, so there is no need to invent another.

  This changes when the loop fails, so it needs its own test: a script transaction over the
  protocol maximum must fail during balancing, not balance.
