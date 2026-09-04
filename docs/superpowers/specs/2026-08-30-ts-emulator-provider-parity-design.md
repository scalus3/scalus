# Making the Scalus JS/TS Emulator the one worth integrating

**Date:** 2026-08-30 (revision 2)
**Status:** Design — awaiting owner review. Research complete, Stage 0 spike run and reverted.
**Scope:** The `scalus` npm package's emulator and script-evaluation surface, the Scala-side
changes it implies, and how lucid-evolution, the IntersectMBO Evolution SDK and MeshJS integrate
with it.
**Related:** `2026-08-03-scalajs-typescript-definitions-generator-design.md` (the generator),
`2026-08-28-typescript-surface-module-design.md` (module split — superseded on the payload
question by §3 below).

---

## 0. Goal and decisions

**The goal is default status, not availability.** Success is that lucid-evolution, MeshJS and the
IntersectMBO Evolution SDK each use Scalus as their **default local transaction evaluator** and as
their **emulator**. Everything below is judged against that, not against API tidiness: §1.4 sizes
the incumbents, and §1.5 states what we must produce to displace them.

1. **No module split.** One `scalus` entry point stays. Measured: splitting buys 0–7% of payload
   and makes the wire cost worse. §3.
2. **M1 is provider parity.** The emulator becomes self-sufficient: it answers protocol
   parameters, evaluates transactions against its own UTxO set, answers indexed queries, and hands
   back real ledger values. §4.
3. **Decoded values are handle classes wrapping the real Scala ledger values**, not structural
   interfaces. Emitting the Scala types themselves is impossible; handles make round-trips free.
   §4.2.
4. **`CardanoInfo` — protocol parameters, network and slot config together — is the construction
   input**, through presets and a `custom` factory. There is no network-name string door: a name
   cannot express Yaci DevKit's slot config, so offering one would only mislead. §4.3.
5. **No mempool and no manual block production.** Immediate application stays. Blaze's mempool
   buys its own test suite nothing and costs it a line after every submit. §6.1.
6. **Scalus-canonical value conventions:** `bigint` quantities, hex strings for hashes and policy
   IDs, `Uint8Array` for opaque payloads, `undefined` never `null`. §4.1.
7. **Adapters stay upstream.** MeshJS, Anastasia Labs and IntersectMBO own their adapter packages;
   Scalus ships the API, the reference integration tests, and the PRs. §7.
8. **Wallets, signing, rollbacks and reward payout are M2.** §6.
9. Breaking the 1.x JS API is acceptable: no downstream consumer is on 1.x yet (§1.3), and the two
   published adapters are already broken against it for unrelated reasons. Each changed signature
   still needs a reviewed MiMa filter — the JS facade is MiMa-checked.

---

## 1. Where Scalus.js is used today

### 1.1 The three integrations

| Package | Version | Depends on | What it does |
|---|---|---|---|
| `@meshsdk/scalus-emulator` | 1.9.1 | `scalus ^0.17.0` | `ScalusEmulator` implementing Mesh's `IFetcher` + `ISubmitter` + `IEvaluator` |
| `@lucid-evolution/scalus-uplc` | 0.1.3 | `scalus ^0.18.1`, `cbor-x`, CML nodejs **and** browser | `createScalusEvaluator` — a script-cost evaluator for the lucid transaction builder |
| `@evolution-sdk/scalus-emulator` | unpublished (PR #249) | `scalus ^0.18.0` | `ScalusEmulatorProvider` implementing the Evolution SDK `Provider` interface |

`@meshsdk/core-cst@1.9.0-beta.101` also declares `"scalus": "^0.14.2"`, but its published `dist/`
contains no import of it — the dependency is vestigial. Mesh's real usage is entirely in
`@meshsdk/scalus-emulator`.

The source of `@lucid-evolution/scalus-uplc` is not in the `main` branch of
`Anastasia-Labs/lucid-evolution`, and its npm metadata carries no `repository` field. Locating it
is step zero of that PR (§7).

### 1.2 What each of them hand-rolls

Read from the published sources, not inferred:

| Layer | evolution-sdk `EmulatorProvider.ts` | mesh `ScalusEmulator` | lucid `scalus-uplc` |
|---|---|---|---|
| UTxO CBOR **decode** | own `decodeUtxoEntry` over effect `Schema`s | own `decodeUtxoEntry`, `decodeValue`, `addressBytesToBech32` | – |
| UTxO CBOR **encode** | `buildUtxoMapCBOR` | `utxosToCborMap` from `core-cst` | hand-written canonical CBOR: `cborMapHeader`, `compareBytes` byte-sorted keys, plus CML to build each input and output |
| Protocol parameters | `DEFAULT_PROTOCOL_PARAMETERS` + `DEFAULT_COST_MODELS` hardcoded in the adapter | `DEFAULT_PROTOCOL_PARAMETERS` from `@meshsdk/common` | taken from the caller's builder context |
| Redeemer tag mapping | own `REDEEMER_TAG_MAP` | own `tagMap` | own `SCALUS_TAGS` |

Three independent implementations of the same two codecs, each able to drift from the ledger CDDL
on its own schedule.

### 1.3 Consequences we can measure

- **Both published adapters are broken against `scalus` 1.x.** Mesh does `require("scalus")`;
  lucid does `import ScalusLib from "scalus"`, a default import. The built `scalus.js` in 1.1.1 ends
  in `export{... as Emulator, ... as SlotConfig, ...}` — named exports only, no default export —
  and the package has no `require` condition. So `require` throws `ERR_REQUIRE_ESM` and the default
  import lands `undefined`. Both worked on 0.17/0.18 because the bundle was CommonJS then.
- **The emulator validates with parameters nobody built against.** Its `UtxoEnv` is `testMainnet`
  or `default`, both carrying `CardanoInfo.mainnet.protocolParams` — regardless of whether the
  caller passed `SlotConfig.preview`. Meanwhile each adapter hands its transaction builder a
  hardcoded parameter set of its own. Nothing keeps the two in agreement.
- **`evaluateTx` round-trips the whole ledger.** Both providers call `getAllUtxos()`, decode every
  entry into their own type, re-encode the lot to CBOR, and pass it to the static
  `evalPlutusScripts` — even though the emulator holds those UTxOs already.
- **The current API invites specific mistakes.** Mesh's `evaluateTx` builds
  `new SlotConfig(this.slotConfig.slotToTime(0), 0, 1000)` — a hardcoded 1000 ms slot length,
  discarding the configured one. Lucid guesses the protocol version with
  `costModels.PlutusV3.length >= 350 ? 11 : undefined`. Neither is a careless adapter author; both
  are filling a hole the API leaves open.
- **Queries are client-side full scans.** `getUtxos` by credential, `getUtxoByUnit`,
  `getUtxosByOutRef` and `fetchUTxOs` each walk `getAllUtxos()` and decode every entry, although
  `EmulatorBase.findUtxos(UtxoQuery)` already evaluates these predicates in Scala.

### 1.4 What default status means we must displace

Today Scalus is an opt-in adapter in all three SDKs. Measured 2026-08-30 from published registry
metadata and the local `evolution-sdk` checkout:

| SDK | Default local evaluator today | In-process emulator today |
|---|---|---|
| lucid-evolution | `@lucid-evolution/uplc@0.2.22`, Rust → wasm, 1,938,640 B unpacked (both targets) | in-house `Emulator` |
| Evolution SDK | `@evolution-sdk/aiken-uplc`, Aiken's uplc → wasm, 953,628 B per target | **none — `node-emulator` is Scalus by design** |
| MeshJS | `@meshsdk/core-csl` → `whisky-evaluator@0.1.1` (13,115,227 B unpacked) with `@sidan-lab/whisky-js-*` (9,275,893 B); every `IEvaluator` in `@meshsdk/provider` is a remote service — Blockfrost, Koios, Maestro, Ogmios, U5C, Yaci | **none** |

**The emulator half is nearly won.** The Evolution SDK's `ClientImpl.ts` says outright *"Register
the factory that constructs a Scalus-backed emulator provider"* and its `node-emulator` provider
throws an error telling the user to install `@evolution-sdk/scalus-emulator`; their
`@evolution-sdk/devnet` is Docker-based and not a competitor. MeshJS ships no in-process emulator
at all except ours. Only lucid-evolution has an in-house `Emulator` to displace, and it validates
far less than ours does.

**The evaluator half is contested, and size is the honest disadvantage — though less so since
2026-08-30:**

| Artifact | over the wire |
|---|---:|
| `aiken_uplc_bg.wasm` | 319,195 B gz |
| `scalus.js`, before the size work | 699,761 B gz |
| `scalus.js`, on master today | **589,835 B gz** |

1.85×, down from 2.19×. Master cut 15.7% off the gzipped bundle by removing dependencies rather
than by repackaging: the IANA timezone database reached through `java.time`, `scribe`, and
`upickle` — see `docs/internal/JS_BUNDLE_SIZE.md`, which also lists the levers left and the
attribution of what remains.

**Packaging, by contrast, still buys nothing.** The module-split spike (§3) shows an eval-only entry
is within 4% of the whole bundle, and externalising `@noble/*` rather than inlining it saves 103,758 B
minified / 40,676 B gzipped — both measured. What is left is compiled Scalus: per
`JS_BUNDLE_SIZE.md`, the Scala stdlib, the cost-model machinery, the ledger domain with its borer
codecs, and the CEK machine itself.

### 1.5 What actually wins the default, then

Four arguments, all true, none of them "smaller":

1. **No wasm.** One JavaScript file: no `.wasm` asset, no separate node and browser builds, no
   bundler configuration, no instantiation step. It runs where wasm is awkward or capped — edge
   workers, React Native, restrictive CSP. Every incumbent ships two wasm targets and a loader.
2. **Diagnostics nothing else has**: trace logs on failure, CEK profiling data
   (`evaluateScriptProfile`), and debug-script replay, which re-runs a failing release script with
   its debug build so the failure carries a message instead of nothing.
3. **One implementation for evaluate and for submit.** The evaluator that prices a transaction is
   the same code the emulator's phase-2 runs, so "evaluated fine, rejected on submit" cannot come
   from two evaluators disagreeing. A wasm evaluator bolted onto a different emulator can.
4. **Conformance we already have, and do not say out loud.** `PlutusConformanceJsTest extends
   PlutusConformanceTest` runs in `ci-js` via `js/test`, so the *JavaScript* build is checked
   against the vendored plutus-conformance corpus on every CI run: 999 evaluation cases, asserting
   term α-equivalence **and the exact CPU and memory budget** against each `.budget.expected`, under
   Plutus's reference variant-E builtin cost model and CEK machine costs. Three cases are skipped,
   all three for a JVM `blst` binding bug (supranational/blst#232) — not a Scalus defect, and
   arguably not a JS one at all (§7.1).

   This is the strongest single argument for the default and it appears nowhere a maintainer would
   see it. Publishing it is an M1 work item, not a testing one.

---

## 2. Capability comparison

`@blaze-cardano/emulator` and the lucid-evolution `Emulator` are the alternatives a developer picks
between.

| Capability | Blaze | lucid-evolution | Scalus JS today | Scalus M1 | Scalus M2 |
|---|---|---|---|---|---|
| Phase-1 validation | hand-written checks, some `TODO` | partial | **full node UTxO rule set (28 STS validators, 4 mutators)** | same | same |
| Phase-2 Plutus execution | via pluggable evaluator | via `uplc` wasm | **own CEK machine, exact budgets, PV11 cost models** | same | same |
| CEK profiling data | no | no | **yes** (`evaluateScriptProfile`) | yes | per-submit profiling |
| Debug-script replay on failure | no | no | **yes** (`submitTx(tx, debugScripts)`) | yes | yes |
| State snapshot / branching | no | no | **yes** (`snapshot()`) | yes | yes |
| Protocol parameters exposed | `params` field | `getProtocolParameters` | **no** | **yes** | yes |
| Emulator-side tx evaluation | yes | `evaluateTx` | **no** (static function only) | **yes** | yes |
| Indexed UTxO queries | `utxos()` + filtering | by address / unit / outRef | **address only, CBOR out** | **address, credential, unit, outRef, txHash** | same |
| Ledger values as objects | yes (CML classes) | yes (plain objects) | **no, CBOR blobs** | **yes (handles)** | yes |
| Datum store | yes | yes | yes | yes | yes |
| Delegation and rewards state | yes | yes | partial | yes | yes |
| Stake distribution | snapshots, for voting power | no | no | **yes** | yes |
| Reward payout at epoch boundary | **no** (`// TODO (?)` at emulator.ts:1586) | `distributeRewards` | no | no | **yes** |
| Mempool / manual block production | yes | `awaitBlock` | no | no (deliberate, §6.1) | no |
| Chain rollbacks for follower testing | no | no | vocabulary only | no | **yes** |
| Named wallets, signing, `as(label)` | **yes** | accounts | no | no | **yes** |
| `expectValidTransaction` / `expectScriptFailure` | **yes** | no | no | no | **yes** |
| `publishScript` / `lookupScript` | **yes** | no | no | no | **yes** |
| Epoch stepping | **yes** | no | no | no | **yes** |
| Governance tallies, committee | **yes** | partial | seeded DReps only | seeded DReps only | later |

Scalus's edge is fidelity: it runs the node's real rules and a real Plutus VM, and it is the only
one of the three that can hand back a profile, replay a failing script with a debug build, or fork
its state. Its deficit is ergonomics, which is what M1 fixes, plus test-harness sugar, which is M2.

### 2.1 Method-level parity with Blaze

Blaze's public surface read from `packages/blaze-emulator/src/emulator.ts` on `main`, 2026-08-31.
"today" means the `scalus` 1.1.1 npm API.

**Ledger state and queries**

| Blaze | What it does | Scalus | When |
|---|---|---|---|
| `utxos()` | Every UTxO, as `TransactionUnspentOutput[]` | `getUtxos()` → `Utxo[]` | M1 |
| — | Filtered queries: by address, payment credential, unit, out-ref, tx hash | `getUtxos(filter)` | M1 |
| `getOutput(input)` | One output by input, or `undefined` | `getUtxos({ outRefs: [ref] })` | M1 |
| `addUtxo(utxo)` | Insert a UTxO directly, bypassing a transaction | `addUtxo(utxo)` | M1 |
| `removeUtxo(input)` | Delete a UTxO directly | `removeUtxo(outRef)` | M1 |
| `datumHashes` (field) | Datum lookup table | `getDatum(hashHex)` | today, hex in M1 |
| `params` (field) | Protocol parameters, mutable | `getProtocolParameters()` | M1 |
| `chainId` (field) | Network id | `getCardanoInfo().network` | M1 |
| `accounts` (field) | Reward accounts and balances | `getDelegation(rewardAddress)`, `getStakeReward(rewardAddress)` | today, bech32 in M1 |
| — | Whole UTxO set as one CBOR map | `getUtxosCbor()` | today |

**Time and blocks**

| Blaze | What it does | Scalus | When |
|---|---|---|---|
| `unixToSlot(ms)` / `slotToUnix(slot)` | Convert between POSIX time and slot | `SlotConfig.timeToSlot` / `slotToTime` | today |
| `clock` (field) | Slot, block, epoch, time | `getSlot()`, `getTime()`, `getCardanoInfo().slotConfig` | today / M1 |
| `stepForwardToSlot(slot)` | Advance to a slot, materialising the mempool | `setSlot(slot)`, `tick(n)` | today |
| `stepForwardToUnix(ms)` | Advance to a wall-clock time | `setTime(ms)` | M1 |
| `stepForwardBlock()` | Produce one block | — | **not planned** (§6.1) |
| `startEventLoop()` / `stopEventLoop()` | Produce blocks on a timer | — | **not planned** (§6.1) |
| `awaitTransactionConfirmation(txId)` | Block until a transaction leaves the mempool | `getTransactionStatus(txHashHex)`; confirmation is immediate, so nothing to await | M1 |
| `stepForwardToNextEpoch()` | Advance to the next epoch boundary | `stepForwardToNextEpoch()` | **M2** |

**Submission and evaluation**

| Blaze | What it does | Scalus | When |
|---|---|---|---|
| `submitTransaction(tx)` | Validate and accept a transaction | `submitTx(txCbor)` | today |
| `evaluator` (field) | Pluggable script-cost evaluator | `evaluateTx(txCbor)`, `evaluateTx(txCbor, extraUtxos)` — not pluggable, it is the same VM that validates | M1 |
| — | Rule name on rejection, for assertions | `SubmitResult.errorRule` | M1 |

**Wallets and test harness** — the whole group is M2; Scalus.js has no key handling today

| Blaze | What it does | Scalus | When |
|---|---|---|---|
| `register(label, value?, datum?)` | Create a named wallet and fund it | `register(label, value?)` | **M2** |
| `fund(label, value?, datum?)` | Mint a genesis UTxO to a named wallet | `fund(label, value)` | **M2** |
| `addressOf(label)` | That wallet's change address | `addressOf(label)` | **M2** |
| `as(label, callback)` | Run a callback as that wallet | `as(label, callback)` | **M2** |
| `mockedWallets` (field) | Label → wallet | — | **M2** |
| `expectValidTransaction(blaze, tx)` | Complete, sign, submit; throw on rejection | `expectValidTransaction(tx)` | **M2** |
| `expectValidMultisignedTransaction(signers, tx)` | The same with several co-signers | `expectValidMultisignedTransaction(signers, tx)` | **M2** |
| `expectScriptFailure(tx, pattern?)` | Assert completion fails, optionally matching the message | `expectScriptFailure(tx, pattern?)` | **M2** |

**Scripts**

| Blaze | What it does | Scalus | When |
|---|---|---|---|
| `publishScript(script)` | Attach a script as a reference UTxO | `publishScript(script)` | **M2** |
| `lookupScript(script)` | Find the reference UTxO carrying it | `lookupScript(scriptHashHex)` | **M2** |

**Stake, rewards and pots**

| Blaze | What it does | Scalus | When |
|---|---|---|---|
| `snapshots` (field) | Per-epoch stake snapshots, for voting power | `getStakeDistribution()` — live, not snapshotted per epoch | M1 |
| `isKnownStakePool(keyHash)` | Whether a pool is registered | `getStakeDistribution()` entries carry `pool`; a dedicated predicate if asked for | M1 |
| — | Reward payout at the epoch boundary | Blaze does not do this either — `// TODO (?)` at `emulator.ts:1586` | **M2** (§6.3) |
| `treasury`, `depositPot`, `feePot` (fields) | Running pot balances | — | not planned |
| `getCurrentTreasuryFeeShare()` | Treasury's share of fees this epoch | — | not planned |

**Governance** — Blaze is well ahead here and M1 does not chase it

| Blaze | What it does | Scalus | When |
|---|---|---|---|
| `setCommitteeState(committee, options?)` | Replace the constitutional committee | — | later |
| `setCommitteeHotCredential(coldHash, cred?)` | Assign or clear a hot credential | — | later |
| `getCommitteeHotCredential(coldHash)` | Read one back | — | later |
| `getGovernanceProposalStatus(actionId)` | Where a proposal stands | — | later |
| `getTallies(actionId)` | Ratification tallies and active CC members | — | later |
| `dreps`, `cc`, `constitution`, `enactQueue`, `bootstrapMode` (fields) | Governance state | seeded DReps only, via `EmulatorOptions.drepRegistrations` | today |

**What only Scalus has**

| Scalus | What it does | When |
|---|---|---|
| `snapshot()` | Fork the whole ledger state; the copy and the original diverge from then on, so one expensive setup branches into many scenarios | today |
| `submitTx(txCbor, debugScripts)` | Replay a failing release script with its debug build, so the failure carries a message instead of nothing | today |
| `evaluateScriptProfile(script)` | CEK profiling data: cost per source location, per builtin, and the transition edges | today |
| `getTransaction(txHashHex)`, `getAppliedTxs()` | Chain history — the transactions applied and the slot each landed at | M1 |
| `getUtxosCbor()` | The whole set as one CBOR map, for callers that want the bytes | today |
| Full node UTxO rule set | 28 STS validators and 4 mutators, the node's own rules rather than hand-written checks | today |
| Conformance-checked VM | 999 plutus-conformance cases in CI, asserting the exact execution budget (§1.5) | today |

---

## 3. Stage 0 spike: the module split, measured and rejected

Run 2026-08-29 in a worktree, all edits reverted. Method: annotate exports with
`@JSExportTopLevel(name, moduleID)`, run `scalusCardanoLedgerJS/fullLinkJS` inside
`nix develop .#ci`, then bundle each public module with the same esbuild invocation
`prepareNpmPackage` uses. gz is `gzip -9`.

> **The absolute numbers below are pre-2026-08-30**, taken before master removed `java.time`,
> `scribe` and `upickle` from the bundle. Every figure here is a *ratio between entries of the same
> build*, so the conclusion is unaffected — and `docs/internal/JS_BUNDLE_SIZE.md` independently
> lists entry-point splitting under "Rejected / already falsified – do not re-test", citing this
> spike.

| Configuration | Entry | min (B) | gz (B) | vs baseline |
|---|---|---:|---:|---:|
| Baseline: today, one entry | `scalus.js` | 3,109,903 | 697,742 | — |
| 2 modules, default `FewestModules` | `eval` | 2,979,905 | 667,336 | −4.2% |
| | `emulator` | 3,107,453 | 698,189 | −0.1% |
| 3 modules, `SmallModulesFor(List("scalus"))` | `uplc` | 2,998,263 | 701,646 | −3.6% |
| | `eval` | 2,998,222 | 701,564 | −3.6% |
| | `emulator` | 3,128,136 | 734,564 | +0.6% |
| UPLC-only link (emulator and `evalPlutusScripts` not linked at all) | `uplc` | 2,904,288 | 679,508 | −6.6% |

Raw linker output, which is what "publish `dist/` instead of the bundle" would ship:

| Split style | .js modules | total min (B) | total gz (B) |
|---|---:|---:|---:|
| Single entry (baseline) | 1 | 7,786,217 | ~1,000,000 |
| `FewestModules`, 2 entries | 3 | 9,039,455 | 1,082,864 |
| `SmallModulesFor("scalus")`, 3 entries | 945 | 11,445,161 | 1,661,042 |

**The 3.1 MB is the Plutus VM's dependency cone, not the emulator.** An entry holding only
`evaluateScript`, `evaluateScriptProfile` and `applyDataArgToScript`, with the emulator and
`evalPlutusScripts` absent from the link, still costs 2.90 MB. The emulator, the 28 validators and
the ledger CBOR codecs together add roughly 200 KB. Splitting cannot recover what is not there, and
publishing the linker output to make subpaths work costs 55% more over the wire (1.08 MB gz against
0.70 MB gz), because esbuild's `--splitting` is documented as a work in progress.

Mechanism questions, answered so they need not be asked again:

1. `@JSExportTopLevel(name, moduleID)` compiles and the linker emits one public module per ID.
2. The moduleID literal **does** survive into TASTy: a probe rendered
   `new scala.scalajs.js.annotation.JSExportTopLevel("Renames", "spike")` and read both literals out
   of `Apply(_, List(Literal(StringConstant(…)), Literal(StringConstant(…))))`.
   `ExportCollector.annotStringArg` keeps matching, since it takes the head; the golden test passed
   unchanged, so a future generator change would be additive.
3. `@JSExportStatic` members follow their class's module — `SlotConfig.mainnet` and friends resolved
   through the class module that `eval.js` imports, with no compiler error.
4. Internal shared chunks are content-hashed (`internal-<40 hex>.js`), so packaging would need globs
   rather than fixed paths.
5. Entries in a single link share per-class modules: `uplc.js` imports the `JScalus` object module,
   which carries `evalPlutusScripts` because the `eval` entry needs it. Per-entry bundling therefore
   cannot drop a sibling entry's methods. Genuine per-entry minimality would need separate link
   tasks, i.e. separate packages — and that is the 6.6% row.

**Consequence for this design:** name collisions with `@meshsdk/common` (`UTxO`, `Value`, `Asset`,
`Transaction`, `Protocol`) are handled by `import * as Scalus from "scalus"` and by import renaming,
the two idioms every TS developer already knows. Not by subpaths.

---

## 4. M1: the TypeScript API

Everything below is additive to, or a replacement of, members on the existing `Emulator` class in
`scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/JEmulator.scala`.

Most shapes used here are already supported and in use: exported classes returning exported classes
is the `SlotConfig` pattern, `js.Array` of a chased trait is exercised by
`JEmulatorInitialState.stakeRegistrations`, `TypeMapper` maps `js.Array[T]` by recursing into `T`,
and overloads are already emitted as `TsOverload`.

**A handle has exactly one constructor, and it is public.** An earlier draft of this section
specified a private primary constructor taking the Scala value beside a public secondary taking JS
values. **That does not compile**, established 2026-08-31 by implementation and independently
reproduced: Scala.js rejects it with *"Private methods in non-native JS classes cannot be
overloaded"*, and `private[pkg]` fails identically. The mechanism is
`PrepJSInterop.scala:901-909` — inside a non-native JS class, any private `Method` whose name is
overloaded is an error; a constructor is a `Method` named `<init>`, a second constructor overloads
that name, and constructors cannot be renamed, so the rule's own suggested remedy is unsatisfiable.
The restriction binds *only* non-native JS classes; a plain `@JSExportTopLevel` Scala class escapes
it, but forfeits the prototype-only property in §4.2, since its fields become ordinary enumerable
own properties.

So the shape is: one public constructor taking JS values, and a **private `var`** holding the Scala
value, which a companion `wrap` overwrites immediately after allocating the handle — the idiom
`JEmulator.snapshot` already uses via `replaceEmulator`. Verified to preserve everything §4.2 asks
for: Scala.js stores the private var under a JS *symbol*, so `Object.keys`, spread,
`JSON.stringify` and `toEqual` still see `{}`; `wrap` assigns a reference, so it stays zero-copy;
and no JS caller can reach the field. The cost is that immutability is now maintained by convention
within each handle's file rather than by the type — every handle must keep `wrap` as its only
writer, and must write only to a handle it just allocated.

Two further rules the same restriction implies, both of which will otherwise cost a compile error:
a qualified-private member of a non-native JS class **must be `final`**; and a `private[pkg]` member
is invisible to JavaScript but is **still emitted into the `.d.ts`**, because `visibleMember`
(`ExportCollector.scala:332`) filters only `Flags.Private`, which dotty does not set for
qualified-private symbols. Keep the wrapped Scala value out of the declarations by holding it in a
class-private field and exposing it through a companion extension method, not a member.

**The generator still gains secondary-constructor collection (§5.10), on a different rationale.**
It is not what makes handles work — nothing shipped here has a secondary constructor. It is a
soundness fix: Scala.js accepts `new Ctors(a, b)` at runtime for a class with public overloaded
constructors, while `ExportCollector` emitted a `.d.ts` that rejected it. That was a mistyping of a
legal shape.

`scalus.d.ts` is generated, not written: run `scalusCardanoLedgerJS/generateDts` and commit the
result; `checkDtsUpToDate` gates drift in `ci-js`.

### 4.1 Conventions

| Concern | Rule |
|---|---|
| Quantities: lovelace, asset amounts, deposits | `bigint`. Forced, not aesthetic: max ADA supply is 4.5e16 lovelace, `Number.MAX_SAFE_INTEGER` is 9.0e15 |
| Slots, epochs, indices, sizes, byte counts, POSIX ms | `number` |
| Hashes, policy IDs, asset names, credentials | lowercase hex `string` |
| Opaque payloads: transaction CBOR, datum CBOR, script CBOR | `Uint8Array` |
| Absence | `undefined` |
| Addresses | bech32 `string` |
| Naming | TS names carry no `J`/`Js` prefix; classes and interfaces `PascalCase`, methods and statics `camelCase` |
| Default parameters | none on public entry points — explicit overloads instead |

**Why `undefined` and not `null`.** Only `undefined` participates in optional properties and
optional parameters:

```ts
// undefined
interface SubmitResult { isSuccess: boolean; txHash?: string; error?: string }
return { isSuccess: true, txHash };               // type-checks

// null
interface SubmitResult { isSuccess: boolean; txHash: string | null; error: string | null }
return { isSuccess: true, txHash, error: null };  // every absent field must be spelled out
```

`js.UndefOr[T]` is also the natural Scala.js mapping of `Option[T]`, and the exporter already emits
`T | undefined` for it. The ecosystem agrees: Blaze's `getOutput(): TransactionOutput | undefined`,
lucid's `scriptRef ? … : undefined`, mesh's `dataHash?: string`. The one real argument for `null` is
that `JSON.stringify` drops `undefined` keys and keeps `null` — which does not apply, because we
hand back objects rather than JSON. The defect today is using both: `getStakeReward(): bigint | null`
next to `txHash?: string`.

**The error model.** A TypeScript library has to say, per method, whether failure throws or comes
back in the result. The rule here:

| Kind of failure | How it surfaces |
|---|---|
| A transaction the ledger rejects — the expected outcome of a negative test | `SubmitResult` with `isSuccess: false`, `error`, `errorRule` (§4.7). Never throws |
| A Plutus script that fails during `evaluateTx` | **throws** `PlutusScriptEvaluationError`, carrying `message` and `logs` — matching what `evalPlutusScripts` already does |
| Malformed input: bad bech32, undecodable CBOR, a hex string of the wrong length, malformed parameter JSON | **throws** `TypeError` with the offending value named |
| A query that matches nothing | an empty array, or `undefined` for a single-result lookup. Never throws |

Every thrown error class **extends `Error`**, so `instanceof Error`, `.stack` and an unhandled
rejection's console output all behave. Today's `PlutusScriptEvaluationError` does not — its own
doc comment warns *"this is a plain object (not a subclass of `Error`), so check it by shape or name
rather than `instanceof Error`"*. Fixing that is part of M1.

### 4.2 Ledger values: handle classes, not structural interfaces

**Emitting the Scala ledger types as the TS types is impossible.** Verified:

- `@JSExportTopLevel` lives in `scala.scalajs.js.annotation`, which does not exist on JVM or Native,
  so it can never be written on a type in `shared/` — where all these types live.
- `TransactionOutput` is a `sealed trait` with nested `Shelley`/`Babbage` case classes. The compiler
  refuses both: *"You may not export a trait"*, *"You may not export a nested class"*.
- `MultiAsset` is `case class MultiAsset private (assets: SortedMap[PolicyId, SortedMap[AssetName,
  Long]])` — a private constructor, plus `SortedMap` and `Long`, and `TypeMapper` hard-errors on
  `Long` (*"no JavaScript representation"*).
- `Value`'s own fields are `Coin` (opaque over `Long`) and `MultiAsset`.

**So the JS type is a handle class in a `js/` sourceset that holds the real Scala value.** This is
better than a structural interface, not merely what is possible:

- **Round-trips cost nothing.** `emulator.addUtxo(u)` takes back the same object, and the ledger
  value inside it was never disassembled. A structural interface would have to re-parse bech32, hex
  and CBOR on the way back in.
- **Marshalling is lazy.** Reading `.address` decodes neither the value nor the datum.
- Identity survives a round trip, so `Object.is` works.

**What this does not buy, contrary to an earlier draft of this section:** it is not what deletes
lucid's `buildUtxoMapCbor` or mesh's `utxosToCborMap`. Those die because §4.5 resolves inputs
emulator-side, so no UTxO crosses the boundary at all. And in the fetcher direction an adapter reads
every field anyway to build its own SDK type, so laziness buys little there. The handle earns its
place on the two points above; the rest of the case is that emitting the Scala types is impossible.

```ts
class Utxo {
  /** Adapters build UTxOs to hand in through this constructor. */
  constructor(txHash: string, outputIndex: number, address: string, value: Value);
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly value: Value;
  readonly datumHash?: string;
  readonly inlineDatum?: Uint8Array;
  readonly scriptRef?: Uint8Array;
  readonly scriptLanguage?: "Native" | "PlutusV1" | "PlutusV2" | "PlutusV3" | "PlutusV4";
  withDatumHash(hash: string): Utxo;
  withInlineDatum(cbor: Uint8Array): Utxo;
  withScriptRef(cbor: Uint8Array): Utxo;
  /** A plain object with the same fields. Assert on this, not on the handle — see below. */
  toObject(): PlainUtxo;
  toCbor(): Uint8Array;
  static fromCbor(cbor: Uint8Array): Utxo;
}

interface PlainUtxo {
  txHash: string;
  outputIndex: number;
  address: string;
  value: PlainValue;
  datumHash?: string;
  inlineDatum?: Uint8Array;
  scriptRef?: Uint8Array;
  scriptLanguage?: "Native" | "PlutusV1" | "PlutusV2" | "PlutusV3" | "PlutusV4";
}

interface PlainValue { coin: bigint; assets: PlainAsset[] }
interface PlainAsset { policyId: string; assetName: string; quantity: bigint; unit: string }

class Value {
  constructor(coin: bigint);
  readonly coin: bigint;
  readonly assets: Asset[];
  plus(other: Value): Value;
  static ada(ada: bigint): Value;
  static of(coin: bigint, assets: Asset[]): Value;
}

class Asset {
  constructor(policyId: string, assetName: string, quantity: bigint);
  readonly policyId: string;
  readonly assetName: string;
  readonly quantity: bigint;
  /** policyId + assetName, the concatenation mesh and lucid call a "unit". */
  readonly unit: string;
}
```

Three constraints the implementation must respect, and the second is the one that will bite:

1. **Every exported handle has exactly one constructor, public, taking JS values.** Scala.js refuses
   to export a class with only private constructors, a constructor parameter of a Scala type would
   surface a non-exportable type in the `.d.ts`, and — established by implementation — a private
   constructor cannot coexist with a public one at all (§4 preamble). The wrapped Scala value lives
   in a private `var` that a companion `wrap` overwrites. Constructor parameters cannot share a name
   with an accessor, so expect `lovelace` where the accessor is `coin`.
2. **Deep-equality assertions silently pass.** Handle accessors are prototype members, not own
   enumerable properties, so `JSON.stringify`, `{...utxo}`, `console.log` and — the dangerous one —
   vitest's and jest's `toEqual` all see an empty object. `expect(utxoA).toEqual(utxoB)` **passes
   for two different UTxOs**: a vacuously green test, in exactly the adapter suites this design asks
   people to write. Hence `toObject()` on every handle, and documentation that asserts through it:
   `expect(utxo.toObject()).toEqual({ txHash: "…", outputIndex: 0, … })`.
3. **One accessor convention across every handle.** All accessors are Scala `def`s, so all handles
   behave identically. A mix of `val`s and `def`s would make some handles spreadable and comparable
   while their neighbours are not, which is worse than a uniform limitation.

`datumHash` and `inlineDatum` are separate optional fields rather than a tagged union because the
generator cannot emit a standalone `type X = A | B` alias today (§8). A union is the better shape and
should replace this when the generator grows alias support.

### 4.3 Construction: `CardanoInfo`, not a network name

A network name cannot express the networks people actually emulate. The ledger distinguishes
`Network.Mainnet | Testnet | Other(id)`; slot config and protocol parameters vary independently of
it. Yaci DevKit is the standing counter-example: custom `zeroTime`, `slotLength` and a short
`epochLength`, all expressible through `new SlotConfig(...)` and none of them nameable.

So the construction input is `CardanoInfo` — protocol parameters, network and slot config as one
coherent triple, which is already the Scala type.

```ts
type NetworkId = "mainnet" | "testnet";

class CardanoInfo {
  static mainnet(): CardanoInfo;
  static preprod(): CardanoInfo;
  static preview(): CardanoInfo;
  /** Yaci DevKit, private testnets, anything else. */
  static custom(network: NetworkId, slotConfig: SlotConfig,
                protocolParams: ProtocolParams): CardanoInfo;
  readonly network: NetworkId;
  readonly slotConfig: SlotConfig;
  readonly protocolParams: ProtocolParams;
  withProtocolParams(params: ProtocolParams): CardanoInfo;
}

interface EmulatorOptions {
  utxos?: Utxo[];
  slot?: number;                       // defaults to the slot for Date.now()
  stakeRegistrations?: StakeRegistration[];
  poolRegistrations?: PoolRegistration[];
  drepRegistrations?: DRepRegistration[];
  datums?: DatumEntry[];
}

class Emulator {
  static create(info: CardanoInfo): Emulator;
  static create(info: CardanoInfo, options: EmulatorOptions): Emulator;
  getCardanoInfo(): CardanoInfo;
  getProtocolParameters(): ProtocolParams;
}
```

Two deliberate narrowings against the ledger's own type. `Network` in Scala is
`Mainnet | Testnet | Other(id)`, but `Other` selects no cost models, no slot config and no
parameters — nothing in the emulator does anything different with it — so `NetworkId` is the pair
that means something here, and `custom` can construct every value the accessor can return. If a use
for `Other(id)` appears, it arrives as a third member of `NetworkId`, not as an unconstructible one.

And `custom` and `withProtocolParams` take a `ProtocolParams`, not a JSON string: §4.4 already
defines that type with `fromBlockfrostJson` and `fromCardanoCliJson` statics, so a JSON caller
writes `CardanoInfo.custom(net, slots, ProtocolParams.fromBlockfrostJson(json))` and a caller who
built parameters another way is not forced to serialise them first.

This also fixes the current mismatch where a `SlotConfig.preview` emulator validates with mainnet
parameters under a testnet network id. The existing `new Emulator(utxosCbor, slotConfig)`,
`Emulator.withAddresses` and `Emulator.withState` remain, deprecated.

### 4.4 Protocol parameters, and how they reach mesh and lucid

`ProtocolParams` is a `shared/` case class with 31 `Long`/`Double` fields and nested `CostModels`,
`ExUnitPrices` and voting thresholds, so — as in §4.2 — the TS type is a handle over the real one,
with typed accessors under the ledger's own field names.

For *integration*, do not try to be either SDK's shape. They are not each other's either:

| | mesh `Protocol` | evolution/lucid `ProtocolParameters` |
|---|---|---|
| deposits | `keyDeposit: number` | `keyDeposit: bigint` |
| min pool cost | `minPoolCost: string` | absent |
| execution units | `maxTxExMem: string` | `maxTxExMem: bigint` |
| per-byte UTxO cost | `coinsPerUtxoSize: number` | `coinsPerUtxoByte: bigint` |
| cost models | positional `number[][]` | `Record<string, number>` per language |

**Blockfrost JSON is the interchange.** Verified: mesh's providers already build
`castProtocol({ minFeeA: data.min_fee_a, … })` from Blockfrost JSON, and the Evolution SDK provider
does the equivalent. So `params.toBlockfrostJson()` lets each adapter reuse mapping code it already
ships, and `ProtocolParams.fromBlockfrostJson` / `fromCardanoCliJson` already exist for the input
direction.

```ts
class ProtocolParams {
  readonly txFeePerByte: number; readonly txFeeFixed: number;
  readonly maxTxSize: number; readonly maxValueSize: number;
  readonly stakeAddressDeposit: bigint; readonly stakePoolDeposit: bigint;
  readonly dRepDeposit: bigint; readonly govActionDeposit: bigint;
  readonly utxoCostPerByte: bigint;
  readonly priceMemory: number; readonly priceSteps: number;
  readonly maxTxExecutionMemory: bigint; readonly maxTxExecutionSteps: bigint;
  readonly collateralPercentage: number; readonly maxCollateralInputs: number;
  readonly minFeeRefScriptCostPerByte: number;
  readonly protocolMajorVersion: number;
  readonly costModels: { PlutusV1: number[]; PlutusV2: number[]; PlutusV3: number[] };
  toBlockfrostJson(): string;
  static fromBlockfrostJson(json: string): ProtocolParams;
  toObject(): PlainProtocolParams;
}
```

**`fromCardanoCliJson` is deliberately absent from the JavaScript surface**, though the Scala static
remains for JVM-side tooling. Two reasons, established 2026-08-31. Nothing justifies it to a
JavaScript caller: `cardano-cli query protocol-parameters` output comes from a Haskell binary in a
devops or local-node context, and unlike the Blockfrost path — which the codebase justifies by
naming MeshJS and the Evolution SDK as consumers — no equivalent consumer exists. And it is the
expensive one: `cardanoCliParamsReadWriter` ends in `macroRW`, whose 30-field derivation drags
`CostModels`, `ExUnits`, `ProtocolVersion`, `UnitInterval`, `ExUnitPrices` and the voting thresholds
into the JavaScript bundle with it, while `blockfrostParamsReadWriter` is hand-written and cheap.

**Every JSON entry point here links `upickle` into `scalus.js`**, which master removed on 2026-08-30
as one of three size levers. Measured on this branch: exporting them costs **+232,336 B minified,
+53,501 B gzipped (+9.1%)**, moving the gap to the aiken wasm evaluator (§1.4) from 1.85× to 2.02×.
`toBlockfrostJson()` is an export root too, being a public member of an exported class, so keeping it
alone would keep the dependency. §5.12 ports the remaining codec to jsoniter to recover this.

**`toBlockfrostJson` does not exist yet** — only the readers do. Roughly 30 fields, plus Blockfrost's
string-or-number quirks and its named cost-model maps. Small but real work, and it needs a
`fromBlockfrostJson(toBlockfrostJson(p)) == p` round-trip test.

Named cost models also replace today's positional `costModels[0] = V1` array, which forces callers to
`Object.values` a record and hope the field order is right.

### 4.5 Evaluation

```ts
evaluateTx(txCbor: Uint8Array): RedeemerBudget[];
evaluateTx(txCbor: Uint8Array, additionalUtxos: Utxo[]): RedeemerBudget[];
```

Resolves inputs against the emulator's own UTxO set, using its own slot config, cost models and
protocol major version. This single method removes, from every adapter: the full-set decode, the
canonical CBOR re-encode, the cost-model reshuffle, the slot-config reconstruction and the
protocol-version guess.

The standalone `evalPlutusScripts` stays for callers with no emulator — lucid's evaluator is one —
and gains an overload taking `ProtocolParams` in place of the positional cost-model arrays and the
version integer, so the version guessing has a supported alternative:

```ts
evalPlutusScripts(txCbor: Uint8Array, utxoCbor: Uint8Array,
                  slotConfig: SlotConfig, params: ProtocolParams): RedeemerBudget[];
```

Its existing form keeps the `protocolMajorVersion` default parameter, against §4.1's own rule. That
is deliberate: the default exports correctly (the linker emits a `...rest` arity check, verified
2026-08-29), the new overload is the recommended door, and breaking the old signature would churn
the one integration that works today for no gain. New entry points take no defaults.

### 4.6 Queries

```ts
/** Fields are ANDed: a UTxO must satisfy every one given. An empty filter matches everything. */
interface UtxoFilter {
  address?: string;
  paymentCredential?: string;   // hex payment-credential hash
  unit?: string;                // "lovelace", or policyId + assetName as hex
  outRefs?: OutRef[];           // matches any of them
  txHash?: string;
  minLovelace?: bigint;
  limit?: number;               // applied last, after filtering
}

getUtxos(): Utxo[];
getUtxos(filter: UtxoFilter): Utxo[];
getUtxosCbor(): Uint8Array;     // the whole set as one CBOR map, unchanged
```

`UtxoFilter` maps onto the existing `UtxoSource` / `UtxoFilter` / `UtxoQuery` algebra in
`scalus-cardano-ledger/shared/.../UtxoQuery.scala`, so filtering happens in Scala and only matching
UTxOs are marshalled. `getUtxosForAddress` and `getAllUtxos` stay, deprecated.

**This is the one part of §4 that is not a pure facade.** The existing algebra has sources
`FromAddress`, `FromAsset`, `FromInputs`, `FromTransaction` and filters `HasAsset`, `HasDatum`,
`HasDatumHash`, `MinLovelace`, `AtInputs`. `paymentCredential` has no counterpart and needs either a
new shared `UtxoSource.FromPaymentCredential` case — which every `BlockchainProvider` implementation
then has to answer — or facade-side matching on `address.paymentCredential`, which keeps the change
local at the cost of not helping the Blockfrost provider. `unit: "lovelace"` is likewise not a
source; it degrades to "every UTxO", which is correct but worth stating.

### 4.7 Submission and results

```ts
interface SubmitResult {
  isSuccess: boolean;
  txHash?: string;
  error?: string;
  errorRule?: string;   // the ledger rule that rejected it
  logs: string[];       // always an array; empty when there are none
}
```

`errorRule` is what lets a test assert *which* rule fired instead of matching message text. `logs`
stops being optional so callers do not need `?? []`.

**`errorRule` must be a literal, not a reflected class name.** Deriving it from `getSimpleName`
would put a stability promise on top of `withMinify(true)`, which is exactly the thing that can
rename it. `SubmitError.fromException` already pattern-matches every case, so each branch names its
rule in source. The set is closed and belongs in the documentation: `BadAllInputsUTxO`,
`BadInputsUTxO`, `BadCollateralInputsUTxO`, `BadReferenceInputsUTxO`, `OutsideValidityInterval`,
`ValueNotConservedUTxO`, `NativeScripts`, `PlutusScriptValidation`, and `ValidationError` for
anything the mapping does not name individually.

### 4.8 State the emulator already holds

Each is a facade over something `EmulatorBase` already computes, except `getStakeDistribution`
(§6.3) and `getTransactionStatus`, which are small additions.

```ts
getSlot(): number;  setSlot(slot: number): void;  tick(n: number): void;
getTime(): number;  setTime(posixMillis: number): void;

hasTx(txHashHex: string): boolean;                  // hex, not raw bytes (today's outlier)
getTransactionStatus(txHashHex: string): "Confirmed" | "NotFound";
getTransaction(txHashHex: string): Uint8Array | undefined;
getAppliedTxs(): AppliedTxInfo[];                   // { txHash, slot }

getDatum(datumHashHex: string): Uint8Array | undefined;
getDelegation(rewardAddressBech32: string): DelegationInfo;
getStakeReward(rewardAddressBech32: string): bigint | undefined;
getStakeDistribution(): StakeDistributionEntry[];   // { credential, pool?, stake, rewards }

addUtxo(utxo: Utxo): void;
removeUtxo(outRef: OutRef): void;

snapshot(): Emulator;
```

`getDelegation` taking a bech32 reward address replaces today's hand-built credential CBOR
(`CBOR.toCBORBytes(cred._tag === "KeyHash" ? [0n, cred.hash] : [1n, cred.hash])` in the Evolution SDK
provider). `hasTx` and `getDatum` move from raw bytes to hex, matching every other identifier here.

**`clearAppliedTxs` is deliberately not exposed**, though the Scala emulator has it. Its two uses
are bounding the memory the applied-tx log holds — every `AppliedTx` retains the whole transaction
and the UTxOs it consumed — and clearing setup transactions out of the way before assertions. From
JavaScript both are better served without it: a fresh `Emulator.create` costs almost nothing, and a
test can record `getAppliedTxs().length` after setup and slice from there. Against that it carries a
hazard our own code documents — `StreamingEmulator` reads `spent` out of the applied-tx record and
throws *"Do not call clearAppliedTxs() on an emulator wrapped by StreamingEmulator"* — so exposing it
would ship a method whose contract is "not if you also use the M2 streaming API".

---

## 5. M1: the Scala-side changes it implies

The TypeScript surface is a facade, but three of its promises need work underneath.

1. **Unify the JVM and JS emulator implementations.** `Emulator.scala` exists twice, near-identical.
   Move the state machine into `EmulatorBase` over an abstract state cell (JVM: synchronized/atomic;
   JS: a plain `var`). Today the two can drift, which is a real risk for a product whose central
   claim is that the JS emulator runs the same rules as the JVM one.
2. **`CardanoInfo`-driven construction** (§4.3), replacing the `Context.testMainnet()` defaults that
   produced the mainnet-parameters-under-a-testnet-id mismatch.
3. **Synchronous queries.** `findUtxos` returns `Future` because `BlockchainProvider` demands it; the
   emulator is synchronous. Add `findUtxosSync`, which both Java and JS want.
4. **`SubmitError` gains a `rule` field**, so all three languages can assert on which rule fired.
5. **`ProtocolParams.toBlockfrostJson`** (§4.4), with a round-trip test.
6. **`UtxoSource.FromPaymentCredential`**, or the local alternative (§4.6).
7. **`getStakeDistribution`**: sum UTxO value plus reward balance per stake credential, grouped by
   delegated pool. The data is already in `utxos` and `certState.dstate`. Pointer addresses can be
   ignored.
8. **No default arguments on the new public entry points**; Java-style builders for the option
   objects, following the existing `EmulatorInitialState.Builder`.
9. **MiMa.** The JS facade is MiMa-checked — there are already filters for `JEmulator.submitTx`
   against `1.1.0`. Every changed signature needs a reviewed filter entry per the build.sbt policy.
10. **Secondary constructors in `scalus-ts-exporter`.** `ExportCollector.classMembers` collects
    `sym.primaryConstructor` and nothing else, skipping it when private. Collect public secondary
    constructors too, merged into the same `TsMember.Ctor` as additional overloads — `TsMember.Ctor`
    already holds a list of parameter lists, and `TsOverload` handling exists for methods. This is a
    soundness fix, not a prerequisite for handles (§4 preamble): the runtime accepts
    `new C(a, b)` for public overloaded constructors while the emitted `.d.ts` rejected it. Reach
    them with `Symbol.declarations.filter(_.isClassConstructor)` — `declaredMethods` excludes
    constructors by construction (`QuotesImpl`'s `isMethod` has `&& !sym.isConstructor`), so it is
    necessarily empty. Add a fixture with public overloaded constructors, and a golden-file case.
    One consequence to accept deliberately: a public secondary constructor with an unmappable
    parameter is now collected and raises an `ExportError` rather than being silently ignored.
12. **Port `blockfrostParamsReadWriter` off `upickle` to jsoniter-scala.** Exporting the Blockfrost
    entry points to JavaScript re-links `upickle` into the bundle — measured at +232,336 B minified
    and +53,501 B gzipped, which is almost exactly the third size lever master landed on 2026-08-30
    undone (`docs/internal/JS_BUNDLE_SIZE.md`). The port is contained: that codec is already
    hand-written as a `bimap` over `ujson.Value`, building every nested type by hand rather than
    summoning a `ReadWriter`, so it becomes one `JsonValueCodec[ProtocolParams]` in one file,
    carrying over the same ~40 field mappings and the existing string-or-number coercion. Task 3's
    round-trip test and `ProtocolParamsJsonTest` are its oracle. Do **not** attempt the same for
    `cardanoCliParamsReadWriter`: it ends in `macroRW` and sprawls into six nested ledger types —
    it stays on `upickle`, and stays off the JavaScript surface (§4.4).

11. **`PlutusScriptEvaluationError extends js.Error`**, so `instanceof Error` works (§4.1). Its own
    doc comment currently warns that it does not.

---

## 6. Deliberately not in M1

### 6.1 No mempool, no manual block production

Blaze has both. Its own test suite shows what they buy. `packages/blaze-emulator/test/Emulator.test.ts`
is 520 lines and steps blocks 16 times; fifteen of those are:

```ts
const txHash = await signAndSubmit(tx, blaze);
emulator.awaitTransactionConfirmation(txHash);   // make the UTxO spendable
```

The sixteenth asserts that `stepForwardBlock()` moves `clock.slot` from 100 to 105. No test inspects
the mempool, exercises two pending transactions, or hits a block limit.

That follows from the design rather than the test style: Blaze's `#ledger` is mutated only inside
`stepForwardToSlot`, so a transaction spending the previous one's output cannot validate until a
block is produced. The mempool creates the problem that `awaitTransactionConfirmation` solves.

Judged one behaviour at a time:

| Behaviour | Needs a mempool? |
|---|---|
| Chaining — spend the output just created | No. Immediate application is strictly better, and is what Blaze's 15 `awaitTransactionConfirmation` calls work around |
| Confirmation/retry logic in off-chain code | No. Needs a Pending/Confirmed distinction, which `StreamingEmulator.checkTransaction` already provides in Scala |
| Double-spend races between concurrent builders | No, and immediate application tests it **better**: we reject the second transaction by rule name, while Blaze accepts both — `acceptTransaction` never consults `#mempool` before parking a transaction (read from `main`, 2026-08-30) |
| Block-level limits (`maxBlockExecutionUnits`, `maxBlockBodySize`) | Yes — and we cannot serve it either. Our rule set is per-transaction; there is no BBODY rule and no block execution-unit check anywhere in `rules/`. That is a milestone, not a rider |

So M1 keeps immediate application and adds no mempool.

### 6.2 Rollbacks — the thing actually worth building, in M2

`scalus.cardano.node.stream` already ships `UtxoEvent.Created | Spent | RolledBack(to: ChainPoint) |
Idle`, `TransactionEvent`, `BlockEvent`, a `StreamingEmulator` that synthesises one block per accepted
transaction plus `newEmptyBlock()`, a `ChainTip`, and `StreamCapabilities.rollbackHorizon`. The
rollback vocabulary exists and nothing ever emits a rollback — the class says so: *"a linear emulator
never forks, so nothing ever needs to settle."*

An emulator that can fork and roll back would let people test indexers and chain followers offline —
a real Scalus audience, and something neither Blaze nor lucid-evolution has. It needs block history
the emulator can rewind, and `StreamingEmulator` exposed to TypeScript. That is M2.

### 6.3 Stake distribution now, reward payout later

Blaze keeps stake **snapshots** for governance voting power and explicitly does not pay rewards:
`// TODO (?): Handle stake rewards distribution` at `emulator.ts:1586`. So shipping a payout would
put Scalus ahead of Blaze rather than at parity — which is why it deserves its own milestone.

`RewardsCalculation.scala` exists but is dead code: nothing except its own test references it. It also
deviates from the ledger in four ways, checked against `cardano-ledger`:

| Scalus `RewardsCalculation` | `Cardano.Ledger.Shelley.Rewards` |
|---|---|
| `distributePoolRewards`: reward proportional to stake | `maxPool'(a0, nOpt, R, σ, p)` with `z0 = 1/nOpt` saturation and pledge influence, then `mkApparentPerformance`, and **zero if `pledge > selfDelegatedOwnersStake`** |
| leader `= cost + m·(f − cost)` | `calcStakePoolOperatorReward = cost + (f − cost)·(m + (1 − m)·s/σ)` — the owner-stake term is missing, so the operator is underpaid |
| when `f ≤ cost`, leader receives `cost`, more than the pool earned | leader receives `f`; members receive nothing |
| members share `(f − cost)(1 − m)` pro rata — **this matches** | but `rewardOnePoolMember` excludes pool owners via `notPoolOwner`; ours does not |

**M1** ships `getStakeDistribution()` (§4.8). **M2** ships reward payout with a rewritten calculator
checked against those four formulas.

### 6.4 Test-harness sugar, in M2

Named wallets and signing (`register`, `fund`, `addressOf`, `as(label, callback)`),
`expectValidTransaction` / `expectValidMultisignedTransaction` / `expectScriptFailure`,
`publishScript` / `lookupScript`, `stepForwardToNextEpoch`, governance tallies — the M2 column of
§2.1. Scalus.js exposes no key handling at all today, so the wallet layer is new surface rather than
a facade, and the assertion helpers are only worth having once it exists. In M1 the lucid and mesh
wallets do the signing.

Keeping Blaze's method names for this group is deliberate: it is the surface someone migrating a
Blaze test suite touches first, and there is nothing to gain from renaming it.

Also M2: per-submit CEK profiling (`submitTx` returning profile JSON). Attractive — nothing else on
Cardano can do it — but a differentiator, not parity.

---

## 7. Rollout

M1 ships as one Scalus release plus three upstream pull requests. Each PR carries two asks: the
repair, and then the default (§0). The repair is unarguable — their current adapter does not work
against `scalus` 1.x at all. The default is argued with §1.5 and the evidence from §7.1.

1. **In this repo.** §4 and §5, the docs page update
   (`scalus-site/content/testing/js-emulator.mdx`), and integration tests (§7.1).
2. **MeshJS/mesh**, `packages/mesh-scalus-emulator`: move to `scalus ^1.x`; replace `require()` with
   a static ESM import; delete `decodeUtxoEntry`, `decodeValue`, `addressBytesToBech32`,
   `hexToBytes`, `bytesToHex`; take protocol parameters and cost models from
   `getProtocolParameters().toBlockfrostJson()` through their existing `castProtocol` mapping; call
   `emulator.evaluateTx`, which also removes the hardcoded 1000 ms slot length. One wrinkle for
   whoever writes it: the package publishes a CJS build (`main: ./dist/index.cjs`), so its CJS
   consumers transitively `require` an ESM-only `scalus`. That works on Node ≥ 22.12 via
   `require(esm)` and fails below it, so the PR should either drop the CJS build or state the Node
   floor.
   For the emulator half, lucid's in-house `Emulator` is the one being displaced, so the PR has to
   answer its users directly: `awaitBlock` and `awaitSlot` become `tick` and `setSlot`, and under
   immediate application awaiting a block is a no-op rather than a requirement; `distributeRewards`
   has no counterpart until M2 (§6.3), which is the one real regression and should be said plainly
   rather than discovered. Everything else — `getUtxos`, `getUtxosWithUnit`, `getUtxoByUnit`,
   `getUtxosByOutRef`, `getDatum`, `getDelegation`, `getProtocolParameters`, `evaluateTx` — maps
   onto §4 directly.

3. **`@lucid-evolution/scalus-uplc`**: first locate the source — it is not in the `main` branch of
   `Anastasia-Labs/lucid-evolution` and the npm metadata has no `repository` field, so this starts
   with an issue asking Anastasia Labs. Then: named import instead of default import; delete
   `buildUtxoMapCbor`, `cborMapHeader`, `compareBytes` and with them both CML dependencies; pass
   protocol parameters through the new overload instead of sniffing
   `costModels.PlutusV3.length >= 350`.
4. **IntersectMBO/evolution-sdk PR #249**: finish against the new API. The provider shrinks to thin
   mapping — `getUtxos` becomes one `getUtxos(filter)` call, `evaluateTx` one call,
   `getProtocolParameters` one call — and `DefaultCostModels.ts` and `DEFAULT_PROTOCOL_PARAMETERS`
   are deleted.

### 7.1 The evidence that supports the default ask

- **Publish the conformance result we already produce.** `ci-js` runs the whole corpus against the
  Scala.js build with exact budget assertions (§1.5). What is missing is a citable claim — a line in
  the npm README and on the site: *"the JavaScript build passes 996 of 999 plutus-conformance
  evaluation cases, matching the reference execution budget exactly; the three skips are a JVM
  `blst` binding bug."* That sentence is the ask's whole foundation and costs an afternoon.
- **Make `ignoredCases` platform-specific, and check whether JS passes all 999.** The three skips
  exist for `supranational/blst#232`, a Java binding bug. The JS build does not use blst — it goes
  through `@noble`. If those cases pass on JS, the claim becomes 999/999 and the ignore list stops
  overstating the problem on the platform we are pitching. Verify before claiming.
- **Differential test against `aiken-uplc` at the *transaction* level.** The corpus checks the CEK
  machine on bare UPLC terms; it does not cover `evalPlutusScripts` — script-context construction,
  redeemer indexing, per-language cost-model selection, or the protocol-version switch. That is
  exactly the layer an evaluator adapter exercises and the layer where two implementations can
  disagree while both being conformant. Compare verdict and budget script-by-script over a
  transaction corpus. This one is genuinely new work.

### 7.2 How parity is proven here, not in three other repos

- npm tests that drive the emulator through `@meshsdk/core` (already a devDependency of
  `scalus-cardano-ledger/js/src/main/npm`) and through `@lucid-evolution/lucid`: build, sign, submit,
  query, evaluate.
- An evaluation-parity test: the same transaction through `emulator.evaluateTx` and through
  `evalPlutusScripts` with the emulator's parameters must return identical budgets.
- A `ProtocolParams` round-trip test (§4.4).
- A JVM/JS parity test over the unified emulator (§5.1).
- A collision test importing `@meshsdk/core` and `scalus` in one file, to keep the `import * as`
  story honest.
- A real bundle-size check, which today's is not. `bundle-size.test.ts` reads the **committed**
  `scalus.js`, and `ci-js` never regenerates it — as `JS_BUNDLE_SIZE.md` puts it, tightening the
  number against a stale artifact proves nothing. Today's committed bundle predates the size work.
  So: regenerate and commit `scalus.js` as part of the M1 release, and only then tighten the guard,
  to ~2.75 MB against the 2.59 MB the size work produced. Better still, make the check read the
  freshly linked output rather than the committed file, so it measures the build under test.

---

## 8. Risks and open questions

- **Handle-class ergonomics, and one sharp edge.** Handles are opaque to `JSON.stringify`, to
  spread, and to `toEqual` — and that last one fails *open*: `expect(a).toEqual(b)` passes for two
  different UTxOs, because both sides look like `{}`. `toObject()` must ship with the first handle,
  be used in every example we write, and be the documented way to assert. This is the single most
  likely way for this design to waste someone's afternoon.
- **Marshalling cost.** `getUtxos()` with no filter on a large ledger now allocates a handle per
  UTxO. Pushing filters into Scala keeps the set small, but measure before assuming it does not
  matter.
- **No discriminated unions yet.** The generator cannot emit `type X = A | B`, so datum options and
  submit results use optional fields instead. Fixing that in `scalus-ts-exporter` would improve this
  API in place.
- **Measurement environments differ slightly across these documents.** The "before" bundle appears
  as 697,742 B gz in §3, 699,761 B in §1.4 and 704,461 B in `JS_BUNDLE_SIZE.md`; the "after" as
  589,835 B here and 592,165 B there. Different esbuild builds and gzip invocations, roughly 0.4%
  apart. No figure is wrong; compare within a document, not across them, and re-measure before
  quoting one publicly.
- **`toBlockfrostJson` is new code, not a facade** (§4.4), and it is the load-bearing piece of the
  integration story.
- **Two vocabularies for parameters.** `ProtocolParams` uses ledger names; mesh and lucid use
  Blockfrost-ish names. Adapters translate either way, so the tie-break is internal consistency with
  Scala — a taste call the owner can overrule.
- **MiMa churn.** Unifying the emulators and changing signatures means a batch of reviewed filter
  entries (§5.9).
- **Upstream release cadence is not ours.** The API can be perfect and still reach nobody until three
  other projects cut releases. The mitigation is that the PRs delete more code than they add.
- **The size gap may still decide it**, though it narrowed on 2026-08-30 from 2.19× to 1.85×
  (590 KB gz against 319 KB gz). For a browser-first SDK that remains a legitimate reason to keep a
  wasm default. Packaging cannot close the rest (§1.4); dependency removal already did what it
  could, and `JS_BUNDLE_SIZE.md` lists what is left. If a maintainer still says no, the fallback ask
  is "default in Node, opt-in in the browser" — worth having ready rather than improvising.
- **Differential testing may find our bugs, not theirs** (§7.1). That is a good outcome and should
  be planned for: the transaction-level comparison happens before the PRs, not alongside them.
  Conformance at the CEK level is already green, so any disagreement found there is in the
  script-context or cost-model-selection layer — the layer the adapters actually use.
- **`@meshsdk/core-cst`'s vestigial `scalus` dependency** should be dropped upstream, or it will keep
  pinning old majors into consumers' trees.
- **Facade drift.** The generator kills declaration drift, not semantic drift between a facade and
  the Scala method it wraps. Shared behavioural tests are the only real answer.

---

## 9. Sources

Read firsthand on 2026-08-29 and 2026-08-30:

- `@meshsdk/scalus-emulator` source: `https://raw.githubusercontent.com/MeshJS/mesh/main/packages/mesh-scalus-emulator/src/index.ts`
- `@lucid-evolution/scalus-uplc@0.1.3` published `dist/index.js`, from the npm tarball
- `@evolution-sdk/scalus-emulator` and `@evolution-sdk/scalus-uplc` sources in the local
  `evolution-sdk` checkout (PR #249 branch)
- `@meshsdk/common` and `@meshsdk/provider` `dist/`, installed in the npm test tree
- Registry metadata for `scalus`, `@meshsdk/scalus-emulator`, `@lucid-evolution/scalus-uplc`,
  `@evolution-sdk/scalus-emulator`
- Blaze emulator and its tests: `packages/blaze-emulator/src/emulator.ts`,
  `packages/blaze-emulator/test/Emulator.test.ts`, `butaneprotocol/blaze-cardano` `main`
- lucid-evolution emulator docs: `docs/pages/documentation/deep-dives/emulator.mdx`
- `cardano-ledger`: `eras/shelley/impl/src/Cardano/Ledger/Shelley/Rewards.hs`,
  `libs/cardano-ledger-core/src/Cardano/Ledger/State/SnapShots.hs`
- Scalus: `JEmulator.scala`, `JScalus.scala`, `SlotConfig.scala`, `EmulatorBase.scala`,
  `Emulator.scala` (jvm and js), `UtxoQuery.scala`, `ProtocolParams.scala`, `CardanoInfo.scala`,
  `Value.scala`, `TransactionOutput.scala`, `Types.scala`, `RewardsCalculation.scala`,
  `stream/StreamingEmulator.scala`, `stream/StreamEvents.scala`, `stream/StreamCapabilities.scala`,
  `rules/DefaultValidators.scala`, `build.sbt`, and the built `scalus.js` 1.1.1
- Spike measurements: this repo, worktree `ts-emulator-plan`, `fullLinkJS` + esbuild, §3
