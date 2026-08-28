# Scaling the Scalus TypeScript surface: module design

**Date:** 2026-08-28
**Status:** Proposal — research complete, decisions pending. NOT approved for implementation.
**Scope:** How to organize a much larger TypeScript surface (txbuilder, ledger domain, ledger
rules) on top of the generator delivered by
`docs/superpowers/specs/2026-08-03-scalajs-typescript-definitions-generator-design.md`.

> Every bundle-size figure in §3.6 is an ESTIMATE. Stage 0 (§5) exists to replace them with
> measurements before anything here is committed to.

---


## 0. The constraint that outranks everything else: Scalus already has downstream npm consumers

Verified 2026-08-28 against published registry metadata and against the copy installed in our own npm test tree:

- **`@meshsdk/core` depends on `scalus`.** The installed `@meshsdk/core-cst@1.9.0-beta.101` declares `"scalus": "^0.14.2"`; the currently published `@meshsdk/core@1.9.1` declares `"scalus": "^0.17.0"`. MeshJS also publishes `@meshsdk/scalus-emulator` ("Scalus emulator utilities for Mesh SDK") and lists "Scalus Emulator" on its homepage.
- **`@lucid-evolution/scalus-uplc@0.1.3`** is published: "Scalus-backed local UPLC evaluator adapter for Lucid Evolution", depending on `scalus ^0.18.1`.

Two consequences that constrain the design:

1. **Adding an `exports` map is a breaking change in itself.** Node's docs: *"When the `"exports"` field is defined, all subpaths of the package are encapsulated and no longer available to importers... `require('pkg/subpath.js')` throws an `ERR_PACKAGE_PATH_NOT_EXPORTED` error."* Today `scalus` has **no `exports` field**, so any consumer may legally deep-import `scalus/scalus.js`. The moment we add `exports`, that stops working. Before shipping, audit what Mesh and lucid-evolution actually import and, if needed, keep an explicit `"./scalus.js"` entry in the map.
2. **The root `.` entry can never change shape.** §3.2 already freezes it; this raises that from good hygiene to a hard compatibility requirement, and it means the deprecation of the root exports has no realistic end date.

## 1. The problem, sized

### 1.1 What exists today

| Artifact | Fact |
|---|---|
| npm package | `scalus` 1.1.1, `"type": "module"`, single `"exports"` entry `"."` |
| Bundle | `scalus.js` **3,118,918 B** minified, **707,344 B** gzipped |
| Linker output before esbuild | one public module `main.js`, **7,809,878 B** (1,000,914 B gz) |
| Declarations | `scalus.d.ts`, 278 lines: 6 classes, 7 interfaces, 1 const, 4 functions |
| Runtime exports | 13 names: `Emulator, EvaluationResult, ExUnits, PlutusScriptEvaluationError, Redeemer, RedeemerBudget, Result, Scalus, SlotConfig, applyDataArgToScript, evalPlutusScripts, evaluateScript, evaluateScriptProfile` |
| Generator | `scalus-ts-exporter`, TASTy inspector, N roots → **one** file; hard-errors on non-exportable types |

### 1.2 The Scala surface being proposed for export

Counts are top-level declarations (column 0), from `rg` over `shared/src/main`.

| Area | Files | Lines | case class | class | enum | sealed trait | object | opaque | alias | given |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `scalus.cardano.ledger` (scalus-**core**) | 56 | 9,734 | 46 | 2 | 17 | 5 | 92 | 9 | 21 | 176* |
| `scalus.cardano.ledger` (scalus-cardano-ledger) | 5 | 1,834 | 7 | 2 | 1 | 0† | 8 | 0 | 3 | – |
| `scalus.cardano.ledger.utils` | 14 | 2,010 | 0 | 0 | 0 | 0 | 14 | 0 | 0 | – |
| `scalus.cardano.address` | 1 | 1,357 | 5 | 0 | 3 | 2 | 10 | 0 | 2 | 12* |
| `scalus.cardano.txbuilder` | 9 | 5,927 | 9 | 1 | 4 | 6 | 15 | 0 | 2 | 1* |
| `scalus.cardano.ledger.rules` | 37 | 2,314 | 3 | 0 | 0 | 1 | 39 | 0 | 0‡ | 0 |
| `scalus.uplc` (+ eval/builtin/transform) | 63 | 22,269 | 36 | 54 | 10 | 3 | 71 | 1 | 6 | 85* |
| **Total** | **185** | **45,445** | **106** | **59** | **35** | **17** | **249** | **10** | **34** | **274** |

\* `given`s are all inside companions, not top level. † one `sealed abstract class` (`TransactionException`, ~35 nested subtypes). ‡ 46 `type Error = …` refinements inside the STS objects.

Nested declarations multiply this: 102 enum cases in core/ledger, 58 in `address`, 15 `Certificate` cases, 19 `TransactionBuilderStep` cases, 28 `StepError` cases, 13 `Constant` subtypes, **101 `DefaultFun` cases**. A naive 1:1 export is well over 600 TS declarations.

**A structural surprise worth stating up front:** `scalus.cardano.ledger` is split across two sbt modules. 56 files (Transaction, Value, ProtocolParams, hashes, certificates…) live in **scalus-core**; only 5 top-level files (`TransactionException`, ledger state, `PlutusScriptEvaluator`) plus `rules/` and `utils/` live in **scalus-cardano-ledger**. `Address`/`Network` are in the sibling `scalus.cardano.address` (one 1,357-line file). The generator already takes N `--tasty-root`s, so this is not a blocker — but "one TS module per sbt module" is not a viable mapping.

### 1.3 Collision-prone names — the real list

**Already colliding with ourselves.** `ExUnits` and `Redeemer` are *already taken* at the npm top level by JS-shaped shim classes in `JScalus.scala:61,79`. The ledger `ExUnits(memory: Long, steps: Long)` (`Types.scala:481`) and `Redeemer(tag, index, data, exUnits)` (`Redeemer.scala:43`) are different types with the same names. `Result` is also taken (deprecated alias for `EvaluationResult`), and `scalus.uplc.eval.Result` is a third meaning.

**Colliding across Scalus's own packages** (same simple name, incompatible shape):

| Name | Meanings |
|---|---|
| `Value` | `cardano.ledger.Value(coin, assets)` vs `onchain.plutus.v1.Value(SortedMap[PolicyId, SortedMap[TokenName, BigInt]])` |
| `Address` | `cardano.address.Address` (sealed trait) vs `plutus.v1.Address` (case class) |
| `Hash` / `ScriptHash` / `PolicyId` | ledger opaque types vs `plutus.v1` aliases of `ByteString` |
| `Redeemer` | ledger case class vs `plutus.v1.Redeemer = Data` |
| `Credential`, `DRep`, `Voter`, `Vote`, `Constitution`, `ProtocolVersion`, `ProposalProcedure` | ledger vs `plutus.v3` |
| `Context` | `ledger.rules.Context` vs `txbuilder.TransactionBuilder.Context` |
| `State` | `ledger.rules.State` vs `serialization.flat.HashConsed.State` |
| `Program` | `uplc.Program` vs `compiler.sir` |
| `Datum` | `plutus.v1.Datum = Data` vs `txbuilder.Datum` (sealed) |
| `TsType` | `scalus.interop.TsType` (annotation) vs `scalus.tsexport.TsType` |

**Colliding with the JS ecosystem.** This is not hypothetical: `@meshsdk/core` is already a devDependency of our own npm test tree, and reading its installed `.d.ts` shows it exports flat top-level `Transaction`, `Value`, `Address`, `Asset`, `UTxO`, `Data`, `Script`, `Certificate`, `Protocol` and `Network` (122 top-level declarations in `@meshsdk/common` alone). Every one of those is a name Scalus wants. The full Scalus list that would clash with lucid-evolution / CSL / CML / Mesh: `Transaction`, `TransactionBody`, `TransactionInput`, `TransactionOutput`, `TransactionWitnessSet`, `Value`, `Address`, `Network`, `Script`, `ScriptRef`, `Block`, `Certificate`, `Coin`, `Hash`, `Slot`, `Credential`, `Redeemer`, `MultiAsset`, `Mint`, `AssetName`, `PolicyId`, `Utxo`, `ProtocolParams`, `ExUnits`, `Anchor`, `Relay`, `Vote`, `Voter`, `Withdrawals`, `Timelock`, `RewardAccount`, `Metadata`, `Language`, `Era`, `GovAction`, `DRep`, `Program`, `Data`.

**Colliding with JS/TS globals** — the worst offenders, and they are *nested cases* so they'd need flattened names anyway: `Redeemers.Array`, `Redeemers.Map`, `Metadatum.Map`, `Metadatum.List`, `Metadatum.Int`, `Metadatum.Bytes`, `Metadatum.Text`, `Term.Error`, `Term.Apply`, `Constant.String`, `Credential.ScriptHash`, plus the aliases `Input` and `Output` (`TransactionInput.scala:41`, `TransactionOutput.scala:334`).

### 1.4 What is already in the 3.1 MB bundle

Distinct FQN strings present in the linker output, by package:

| Package | classes present |
|---|---:|
| `scalus.cardano.ledger` | **416** |
| `scalus.uplc.eval` | 186 |
| `scalus.uplc` | 83 |
| `scalus.cardano.ledger.rules` | 48 |
| `scalus.cardano.address` | 26 |
| `scalus.cardano.txbuilder` | **0** |

This reframes the bundle-size argument. `Emulator.submitTx` runs the full STS pipeline, so the **entire ledger domain and rule set is already linked in and paid for**. Exporting ledger types costs approximately nothing in bytes. TxBuilder is the one genuinely new payload. The value of splitting is therefore *not* "make today's bundle smaller" — it is (a) let a script-evaluation-only consumer stop paying for the emulator, and (b) stop every future addition from being charged to every consumer.

### 1.5 The hard constraint everyone underestimates: Scala.js cannot export these types

From the Scala 3 compiler's `PrepJSExports` (error strings extracted from `scala3-compiler_3-3.3.8.jar`):

- `You may not export a trait`
- `You may not export a nested class. Create an exported factory method in the outer class to work around this limitation.`
- `You may not export an abstract class`
- `You may not export a class that has only private constructors`
- `Only static objects may export their members to the top level`
- `Only a static object whose companion class is a non-native JS class may export its members as static.`
- `You may not export a lazy val to the top level`
- `You may not export a getter or a setter to the top level`
- `You may not export an inline method`

Apply that to Scalus:

- `TransactionOutput`, `Address`, `Script`, `PlutusScript`, `Redeemers`, `Metadatum`, `Network` are **traits** → not exportable.
- `Certificate` (15), `GovAction` (7), `Credential` (2), `DatumOption` (2), `Timelock` (6), `Script.PlutusV1/2/3`, `TransactionOutput.Shelley/Babbage` are **nested** → not exportable.
- `Data`, `TransactionException`, `DefaultUni` are **abstract** → not exportable.
- `MultiAsset`, `KeepRaw`, `Sized`, `DeBruijnedProgram` have **private constructors** → not exportable.
- And every remaining type carries `Long`, `Option`, `SortedMap`, `Tagged*`, `KeepRaw` or `given` in its signature, all of which the generator's `TypeMapper` rejects outright (`TypeMapper.scala:126–151`).
- Finally, `@JSExportTopLevel` lives in `scala.scalajs.js.annotation`, which does not exist on JVM or Native — so it can never be written on a class in `shared/`.

**Conclusion: there is no path where the shared Scala types become the TS types.** Every design below is a design for a *curated JS-facing facade layer* in `js/` sourcesets. The only open question is how that layer is organized, not whether it exists.

---

## 2. Prior art

| Library | Organization (verified from published artifacts) | Why | Consumer import |
|---|---|---|---|
| **viem 2.56.0** | Flat root barrel **plus 27 `exports` subpaths** (`./accounts`, `./actions`, `./chains`, `./chains/utils`, `./ens`, `./utils`, `./op-stack`, `./zksync`, `./siwe`, `./window`, …). Root `src/index.ts` is a **hand-written explicit barrel**: 1973 lines, 296 `export` statements, **zero `export *` and zero `export * as`**, headed `// biome-ignore lint/performance/noBarrelFile: entrypoint module`. `"sideEffects": false`. Three output trees (`_esm`/`_cjs`/`_types`), each subpath a 3-condition object with `types` first. | *"Maintaining a low bundle size is critical... End users should not be required to download a module of over 100kB in order to interact with Ethereum."* / *"viem is tree-shakable, meaning only the modules you use are included in your final bundle."* `src/chains/index.ts` has **738 export lines** — that is why chains live behind a subpath. Published benchmark: viem **31.08 kB** vs ethers **83.61 kB** (2.69x) vs web3 **157.49 kB** (5.08x), min+gzip. | `import { createPublicClient, http } from 'viem'` + `import { mainnet } from 'viem/chains'` |
| **viem/actions — the escape hatch that matters to us** | A subpath of free functions `fn(client, args)` mirroring the decorated client methods | Docs, "Tree-shaking": *"You can use the Client as-is, with no decorated Actions, to maximize tree-shaking in your app... instead of calling `getBlock` from the Public Client, we are importing the Action directly."* | `import { getBlock } from 'viem/actions'; await getBlock(client, {…})` |
| **ethers 6.17.0** | Umbrella root exporting **both** flat names and an `ethers` namespace object; 12 `exports` subpaths (`./abi`, `./providers`, `./utils`, `./wallet`, …). `src.ts/ethers.ts` is a 221-line explicit named barrel with **zero `export * as`**. **No `types` condition and no top-level `types` field** — declarations resolve only because `.d.ts` files sit beside the `.js`. `"sideEffects": false`. | Migration guide: *"In v6 all imports are available in the root package, and for those who wish to have finer-grained control, the `pkg.exports` makes certain folders available directly."* The v5 runtime namespaces `ethers.utils.*` / `ethers.constants.*` / `ethers.providers.*` were **removed**, members hoisted and **renamed for global uniqueness**: `hexZeroPad`→`zeroPadValue`, `arrayify`→`getBytes`, `hexDataSlice`→`dataSlice`, `AddressZero`→`ZeroAddress`, `Web3Provider`→`BrowserProvider`, `commify` deleted. | `import { BrowserProvider, parseUnits } from 'ethers'` or `import { HDNodeWallet } from 'ethers/wallet'` |
| **@solana/web3.js v1 (1.98.4, still `latest`)** | Class-heavy single namespace: `Connection`, `Transaction`, `PublicKey` | Pre-ESM design | `import { Connection } from '@solana/web3.js'` |
| **@solana/kit 8.1.0** (the 2.x line, renamed 2025-08-27) | Thin umbrella over **27 sibling packages** (`@solana/rpc`, `keys`, `transactions`, `codecs`, `accounts`, `errors`, `signers`, …). Its own `exports` map has only **2** subpaths — **granularity lives in the package split, not in subpaths.** `"sideEffects": false`. No classes except `SolanaError`. | *"The object-oriented design of the web3.js (1.x) API prevents optimizing compilers from being able to 'tree-shake'... One example... is the `Connection` class. It has dozens of methods, but because it's a _class_ you have no choice but to include every method in your application's final bundle."* Also, directly relevant to our §3.5: *"Class-based architecture also presents unique risks to developers who trigger the dual-package hazard... It arises when two copies of the same class are present in the dependency tree, causing checks like `instanceof` to fail."* | `import { generateKeyPair } from '@solana/kit'` or `import { getU64Encoder } from '@solana/codecs-numbers'` |
| **@lucid-evolution/lucid 0.6.2** | 13 published packages but **exactly ONE `exports` key**; single bundled entrypoint (`index.js` 227 KB, `index.d.ts` 157 KB); flat `export *` from every sub-package, plus the whole wasm CML re-exposed as `export { CML }`. **No `sideEffects` field.** Most Cardano "types" are string aliases (`type Address = string`, `type Transaction = string`). | *"Installing the `lucid` package will automatically export all other packages in the library."* No tree-shaking or bundle-size claim found anywhere. | `import { Lucid, Koios } from '@lucid-evolution/lucid'` |
| **@evolution-sdk/evolution 0.5.13 (IntersectMBO)** — the closest precedent | **ONE package, 136 top-level modules**, `exports` = `.`, a **`"./*": "./dist/*.js"` wildcard**, three domain subpaths (`./blueprint`, `./cose`, `./plutus`) and **`null` blockers** (`"./plutus/*": null`, `"./internal/*": null`) that forbid deep imports. Root `index.d.ts` is only 6.7 KB: a **`export * as X from "./X.js"` namespace barrel**. `"sideEffects": []`, built with `babel --plugins annotate-pure-calls`. | Published rationale doc: *"Direct module imports are better for code that lives in a codebase for a long time... Bundlers can tree-shake individual modules independently."* / *"Blocking deep imports... lets the SDK reorganize internals without breaking consumers. The public entry points are the stable surface; what lives inside them is an implementation detail."* | `import { Cardano } from "@evolution-sdk/evolution"` · `import * as Address from "@evolution-sdk/evolution/Address"` · `import { Value } from "@evolution-sdk/evolution/plutus"` |
| **CSL 17.0.0 / CML 6.2.0** | **No `exports` map at all** — bare `main` + `types`. One flat `.d.ts`: CSL **4751 lines, 197 `export class`, 197 `free(): void`, zero namespaces**; CML **15 105 lines, 234 classes**. `sideEffects` explicitly *lists* the shim, i.e. deliberately not tree-shakeable. Shipped as separate packages per build target (`-nodejs`/`-browser`/`-asmjs`, ×2 for GC variants); asmjs build is 52.77 MB unpacked. | wasm-bindgen emits one flat `.d.ts`; classes use `private constructor()` + static factories; memory freed via `.free()` or `FinalizationRegistry` | `import { Value, BigNum, Transaction } from '@emurgo/cardano-serialization-lib-nodejs'` |
| **@meshsdk/core 1.9.1** | ~13 packages, **exactly ONE `exports` key**; `dist/index.js` is an 8.7 KB re-export shell doing flat `export *` from `common`/`provider`/`transaction`/`wallet` plus `export * as cst`. **No `sideEffects` field.** **Depends on `scalus`.** | Homepage: *"The core package is under 60kB. Tree-shakeable imports keep your bundle lean."* (vendor claim; `@meshsdk/core-cst` alone is 9.37 MB unpacked) | `import { MeshTxBuilder, BlockfrostProvider } from '@meshsdk/core'` |
| **Kotlin/JS `@JsExport` (Kotlin 2.4.10)** | **One `.d.ts` per module; the Gradle plugin's `PackageJson` model has `main` and `types` and no `exports` field at all.** Non-ESM targets wrap output in nested `namespace` blocks and leak the Kotlin stdlib package tree; **ESM output flattens packages entirely** — *"When targeting ECMAScript Modules (ESM), package information is not preserved to improve the application bundle size and match the typical layout of ESM packages."* Still `@ExperimentalJsExport` after 6 years. | Restricted exportable subset with hard errors: *"All other Kotlin types, except for those marked with `@JsExport` — Not supported."* `Long`→`BigInt` only behind `-Xes-long-as-bigint`; collections surface as `KtList`/`KtMap`/`KtSet` wrappers, not JS arrays. **Flattening produced a name-collision diagnostic, `EXPORTING_JS_NAME_CLASH_ES`, and an open bug KT-66524** — exactly the failure mode a flattened Scalus would hit. | one `.d.ts`, flat |

**The one published before/after that is worth quoting in full** (anza-xyz/kit README, "Statistics" — note these are *not* in the Anza blog posts, which carry no kB figures):

| | web3.js 1.x | @solana/kit | change |
|---|---:|---:|---:|
| Minified size of library | 81 KB | 57.5 KB | −29% |
| ...when the runtime supports Ed25519 | 81 KB | 53 KB | −33% |
| **Bundled size of an app that transfers lamports** | **111 KB** | **23.9 KB** | **−78%** |
| ...when the runtime supports Ed25519 | 111 KB | 18.2 KB | −83% |
| Solana Explorer first-load | 311 KB | **228 KB** | −26% |

The −78% row is the important one for us: the *library* shrank 29%, but the *application* shrank 78%. That gap is entirely attributable to entry-point granularity — which is exactly what subpaths buy and what a single 3.1 MB entry cannot.

**The pattern is unambiguous:** every library that grew past ~50 exported names and cared about bundle size ended up at *ESM subpath entrypoints declared in package.json `"exports"`*. Nobody chose TS `namespace`. Nobody chose prefixing.

### The five options, judged

| Option | Tree-shaking | `import * as` | Editor auto-import | Docs discoverability | Collision handling | Verdict |
|---|---|---|---|---|---|---|
| **Flat named exports, one entrypoint** (today) | Only if the bundler can prove no side effects across one 3 MB file — in practice, no | `import * as Scalus from "scalus"` gives one giant object | Excellent — every name suggested | Degrades badly past ~50 names | Consumer must rename every import | Does not scale past ~50 names |
| **ESM subpath exports** (`scalus/ledger`) | Real: separate entry graphs, distinct linker modules | `import * as Ledger from "scalus/ledger"` — per-area, useful | Good; TS suggests from subpaths listed in `exports` when `moduleResolution` is `node16`/`bundler` | Excellent — subpath *is* the doc grouping | Rename at import, or `import * as` | **Recommended** |
| **Namespace objects** (`export * as Ledger from "./ledger"` in one entry) | Poor — everything still lives in one module graph; the barrel forces it all in | Same ergonomics as subpaths | Poor — TS rarely auto-imports a member through a namespace object | Fine | Same as subpaths | Gives the ergonomics without the payload win |
| **TS `namespace`** | None — `namespace` is a TS-only construct that predates ESM | n/a | Poor | Fine | Same | Rejected: TS handbook says use modules; `typescript-eslint/no-namespace` flags it; the 2026-08-03 design already removed the `namespace Scalus` wrapper |
| **Prefixing** (`LedgerTransaction`) | Same as flat | n/a | Excellent | Poor — `Ledger*` prefix noise everywhere | Solves collisions by making names ugly | Rejected: 300+ types × a prefix is unreadable, and it doesn't help payload at all |

---

## 3. Recommendation

### 3.1 One package, N ES-module subpaths, generated from `@JSExportTopLevel(name, moduleID)`

The mechanism already exists in Scala.js and is the single source of truth for both runtime layout and declarations:

```scala
// scalajs-library 1.22.0, JSExportTopLevel.scala
class JSExportTopLevel(name: String) extends scala.annotation.StaticAnnotation {
  def this(name: String, moduleID: String) = this(name)
}
```

- The linker groups entry points into **public modules by `moduleID`**, and the default `OutputPatterns` file name is `%s.js` → `moduleID = "ledger"` produces `ledger.js`.
- The uniqueness key is verified to be **per module**: `AnalyzerRun._topLevelExportInfos: mutable.Map[(ModuleSet.ModuleID, String), TopLevelExportInfo]`. So `Value` in `ledger` and a different `Value` in `uplc` are both legal.
- The generator's `annotStringArg` currently reads only the first literal (`ExportCollector.scala:51`). Reading the second is a one-pattern change.

**Do not invent a `@TsModule` annotation for exported types, and do not use a config-file package→module map.** Either would create a second source of truth that can silently disagree with the linker's actual module layout — the exact drift `generateDts`/`checkDtsUpToDate` was built to eliminate. (One narrow exception, §4.)

### 3.2 The proposed module map

Modules are cut by **task**, not by Scala package. Eight subpaths:

| Subpath | moduleID | Contents |
|---|---|---|
| `scalus` (`.`) | `main` | Frozen: today's 13 exports, all `@deprecated` pointing at the new homes. Never grows again. |
| `scalus/eval` | `eval` | `evaluateScript`, `evaluateScriptProfile`, `applyDataArgToScript`, `evalPlutusScripts`, `EvaluationResult`, `ExUnits`, `RedeemerBudget`, `PlutusScriptEvaluationError` |
| `scalus/ledger` | `ledger` | Data + codecs: `Transaction`, `TransactionOutput`, `Value`, `Address`, `Utxo`, `Coin`, hashes, `Certificate`, `Script`, `Datum`, `ProtocolParams`, `CardanoInfo`, `SlotConfig`, `Network` |
| `scalus/txbuilder` | `txbuilder` | `TxBuilder` + its errors |
| `scalus/emulator` | `emulator` | `Emulator`, `EmulatorInitialState`, `SubmitResult` |
| `scalus/rules` | `rules` | `validateTransaction`, `applyTransaction` — *not* the STS zoo |
| `scalus/uplc` | `uplc` | `Program`, `Data`, `PlutusVM`, flat/CBOR codecs |
| `scalus/wallet` | `wallet` | `HdAccount`, signers, `Bech32` (last) |

`scalus/ledger` deliberately merges three Scala packages (`cardano.ledger` from two sbt modules + `cardano.address`) because a JS user does not care where `Address` lives.

**No root barrel.** Do not add `export * as Ledger from "./ledger"` to `scalus`. A barrel makes every importer of the root pay for every module and undoes the split. This is a taste call the owner should confirm; the recommendation is *no barrel*.

### 3.3 The import experience

**Build a transaction:**

```ts
import { TxBuilder } from "scalus/txbuilder";
import { CardanoInfo, Value, Address, Utxo, Input, decodeUtxos } from "scalus/ledger";

const info = CardanoInfo.mainnet();
const alice = Address.fromBech32("addr1q9...");
const bob   = Address.fromBech32("addr1qyn...");
const utxos = decodeUtxos(utxoCbor);

const tx = new TxBuilder(info)
  .payTo(bob, Value.ada(10n))
  .spend(utxos)
  .build(alice)          // changeTo
  .sign(signer)
  .transaction();        // -> Transaction

console.log(tx.idHex(), tx.toCbor().length);
```

**Inspect a Value:**

```ts
import { Value, type Asset } from "scalus/ledger";

const v: Value = tx.outputs()[0].value;
v.coin;                       // bigint  (lovelace)
v.assets;                     // readonly Asset[]  -> { policyIdHex, assetNameHex, quantity: bigint }
v.plus(Value.ada(5n)).coin;
Value.ada(2n).coin === 2_000_000n;
```

**Run the ledger rules:**

```ts
import { validateTransaction } from "scalus/rules";
import { CardanoInfo, decodeTransaction, decodeUtxos } from "scalus/ledger";

const errors = validateTransaction(
  decodeTransaction(txCbor),
  decodeUtxos(utxoCbor),
  { info: CardanoInfo.preprod(), slot: 12345 },
);
if (errors.length > 0) console.error(errors.map(e => `${e.rule}: ${e.message}`));
```

**Collision with another Cardano library — both mechanisms:**

```ts
// (a) namespace import: zero renaming, reads well
import * as Ledger from "scalus/ledger";
import { Transaction, Address } from "@lucid-evolution/lucid";
function bridge(l: Address): Ledger.Address { … }

// (b) import-rename: for one or two names
import { Transaction as ScalusTransaction } from "scalus/ledger";
```

We handle collisions by **partitioning into subpaths** (so `Value` and `Context` never sit in the same module) and by a **generator lint** (§4.5) that hard-errors on any exported name shadowing a JS global. We do **not** prefix names, and we do not ask consumers to do anything unusual — `import * as` and import-renaming are the two idioms every TS developer already knows.

### 3.4 How Scala types surface in TS

Because Scala.js cannot export traits, nested classes, abstract classes or private-ctor classes (§1.5), the shape is forced, and it happens to match the modern consensus:

| Scala shape | TS shape | Mechanism |
|---|---|---|
| ADT / sealed trait / enum (`TransactionOutput`, `Certificate`, `Script`, `DatumOption`, `Credential`) | **Discriminated union of plain interfaces** with a `kind` literal field | facade `js.Object` traits, chased by the generator into `export interface`; `type Certificate = RegCert \| UnregCert \| …` via one `@TsType` alias |
| Small data with behavior (`Value`, `SlotConfig`, `ExUnits`, `Coin`) | **class with statics** | facade class `extends js.Object` + `@JSExportStatic` on the companion (verified working: `SlotConfig.mainnet`) |
| Large aggregate, mostly read (`Transaction`, `TransactionBody`, `TransactionWitnessSet`) | **opaque handle class** with accessor methods and `toCbor()` / static `fromCbor()`, *not* a 20-field structural interface | facade class wrapping the Scala value; accessors marshal lazily |
| Stateful builder (`TxBuilder`, `Emulator`, `PlutusVM`) | **class with methods** | facade class |
| `object` of functions (`utils/*`, `ProgramFlatCodec`) | **free functions** | `@JSExportTopLevel("decodeTransaction", "ledger")` on defs in a top-level object |
| `given` codecs (274 of them) | **free functions** `encodeX` / `decodeX` | never exported as givens; matches the interop guide's Tier-0 "non-implicit entry point" rule |

**The strongest objection to this table, stated plainly.** Solana's rewrite names *classes* as the thing that defeats tree-shaking — *"because it's a _class_ you have no choice but to include every method in your application's final bundle"* — and viem shipped `viem/actions` as free functions for the same reason. My recommendation puts `TxBuilder`, `Emulator` and `PlutusVM` back into classes. The defence is specific to Scala.js and should be checked, not assumed: our JS classes are thin facades over a Scala program that the **Scala.js linker** has already dead-code-eliminated at whole-program, per-method granularity before any bundler sees it. A consumer's bundler cannot shake *into* that output either way, so converting the facade to free functions would move method-level DCE from the linker (where it works) to the bundler (where it cannot reach). If a facade class ever grows a method that drags in a large, otherwise-unreachable subsystem, that method belongs in a separate free function in its own module — not in the class. Worth measuring at Stage 2.

Rationale for handle-vs-interface on `Transaction`: `TransactionBody` has 20 fields, 12 of them `Option`, 5 of them `Tagged*` opaque collections, and `Transaction.body` is a `KeepRaw[TransactionBody]`. Eagerly marshalling that tree on every decode is both a lot of generated code and a real runtime cost, and CBOR is the wire format anyway. Lazy accessors on a handle are cheaper and smaller. This is the one place where Scalus should look like CML rather than like solana-kit — and it is a taste call the owner can overrule.

**Primitive mapping rules (the ones the generator must be told):**

| Scala | TS | Rule |
|---|---|---|
| `Long` where the value can exceed 2^53 — `Coin`, `Value` quantities, `Mint`, `ExUnits`, fees, deposits | `bigint` | Forced, not aesthetic: max ADA supply is 4.5e16 lovelace; `Number.MAX_SAFE_INTEGER` is 9.007e15. `number` is *wrong* for lovelace. |
| `Long` for slots, epochs, indices, sizes, byte counts, timestamps-in-ms | `number` | All safely under 2^53; keeps arithmetic ergonomic. `SlotConfig` already does this. |
| `BigInt` (Plutus integers) | `bigint` | |
| `Option[T]` | `T \| undefined` (optional property / trailing optional param) | Facade uses `js.UndefOr`. **Pick `undefined`, never `null`.** Today's d.ts mixes both (`getStakeReward(): bigint \| null`); unify on `undefined` and treat `null` as a bug. Taste call, but consistency matters more than which. |
| `Seq`/`IndexedSeq`/`List`/`Set` | `readonly T[]` | facade `js.Array` |
| `Map[K, V]` with hex-able keys | `{ [k: string]: V }` | facade `js.Dictionary` |
| `SortedMap` where order is load-bearing (`MultiAsset`, `Withdrawals`, `VotingProcedures`) | `readonly Entry[]` (array of `{key, value}`) | Never a JS object — property order is not a contract, and CBOR canonical ordering is |
| `ByteString` / `Array[Byte]` payloads | `Uint8Array` | |
| Hash aliases (`TransactionHash`, `ScriptHash`, `PolicyId`, 16 of them) | **branded** `Uint8Array & { readonly __brand: "TransactionHash" }` + a `*Hex` string form for map keys | Without branding all 16 collapse to one mutually-assignable type. Via `@TsType` today; §4.6 proposes `@TsBrand`. |
| `opaque type` (`Mint`, `Tagged*`, `CostingInteger`) | erased to the underlying TS type | never surfaced |
| `KeepRaw[A]`, `Sized[A]` | erased | facade unwraps |
| Tuples | named interfaces | e.g. `Redeemers.Map`'s `Map[(RedeemerTag, Int), (Data, ExUnits)]` becomes `readonly RedeemerEntry[]` |
| `Either[E, A]` | **throw** a typed error | facade classes should `extend js.Error` so `instanceof Error` works — today's `PlutusScriptEvaluationError` is *not* an `Error` subclass and the d.ts has to warn about it. Fix that as part of this. |
| `Future[T]` | `Promise<T>` | `js.Promise`; only for `TxBuilder.complete` |
| `java.time.Instant`, `Try`, `Lens`, `ToData`/`FromData` typeclasses | not exported | `@TsIgnore` |

**Naming rules:**

1. The TS name is whatever `@JSExportTopLevel("…")` says. Scala facade names are free to carry `J`/`Js` prefixes; the prefix must **never** appear in TS. (`JEmulator` → `"Emulator"` already.)
2. Chased (non-exported) `js.Object` traits get `@TsName`. Same rule: no `J`.
3. Classes and interfaces `PascalCase`; functions, methods and statics `camelCase`; type-only unions `PascalCase`.
4. ADT variants are flattened with a parent-derived prefix wherever the bare name is generic or shadows a global: `Redeemers.Array` → `RedeemerArray`, `Metadatum.Map` → `MetadatumMap`, `TransactionOutput.Babbage` → `BabbageOutput`, `Credential.ScriptHash` → `ScriptCredential`.
5. Factories are `static` members on the class (`Value.ada`, `SlotConfig.mainnet`, `Address.fromBech32`) when a `js.Object` facade class exists; free functions (`decodeTransaction`) otherwise. Two compiler rules constrain this: `@JSExportStatic` only works when the companion class is a **non-native JS class**, so statics require an `extends js.Object` facade; and *"You may not export a lazy val as static"*, so only cheap eager `val`s become `static readonly` properties (`SlotConfig.mainnet`) — anything lazy or expensive must be a static **method** (`CardanoInfo.mainnet()`, as in the §3.3 samples).
6. Keep the Scala name whenever it is already domain-specific and unambiguous (`TransactionWitnessSet`, `ProtocolParams`, `RedeemerBudget`). Rename only for collision or clarity.
7. Banned as top-level export names in any module: `Array, Map, Set, Error, Date, Number, String, Object, Function, Symbol, Promise, BigInt, Proxy, Reflect, JSON, Math, Boolean, RegExp, WeakMap, WeakSet`.

### 3.5 Build changes — the honest list

Nothing here is free. Concretely:

1. **Linker.** No config change needed: `ModuleKind.ESModule` is already set and the moduleIDs alone cause multiple public modules. Output goes from one `main.js` (7.8 MB) to `main.js`, `ledger.js`, `txbuilder.js`, … plus linker-generated internal shared modules. Optionally add `ModuleSplitStyle.SmallModulesFor(List("scalus"))` later for finer chunks — evaluate, don't assume.

2. **esbuild.** This is the sharp edge. Today `prepareNpmPackage` runs esbuild over the single `main.js` with `--bundle --minify` and inlines `@noble/*` so `scalus.js` is a self-contained file loadable from a `<script type="module">`. With N entries you can either:
   - **(a) `--splitting --outdir=… --format=esm`** — smallest output, but esbuild's own docs say *"Code splitting is still a work in progress"* with a known import-ordering issue (evanw/esbuild#399). Not a foundation to bet the package on today.
   - **(b) Publish the Scala.js linker output directly** as `dist/`, with `@noble/curves` and `@noble/hashes` as real npm `dependencies` instead of inlined. It is already valid ESM with correct relative imports; every modern bundler handles it; shipping unminified ESM is normal for libraries. Cost: 7.8 MB on disk vs 3.1 MB (1.00 MB vs 0.71 MB gzipped over the wire, and consumers minify anyway).
   - **Recommended: (b).**

   **Critical constraint on (b): everything reachable through the `exports` map must resolve into ONE module graph.** Do not serve the root `.` from the self-contained esbuild bundle while serving subpaths from the linker output — that ships two copies of the Scala.js runtime and of all static state, so a consumer importing both `scalus` and `scalus/eval` would get two distinct `EvaluationResult` classes, failing `instanceof` across the boundary and doubling memory. Since Stage 0's deprecated root re-exports invite exactly that import pair, this is not a theoretical hazard. Therefore the root `.` is a stub over the linker output like every other subpath, and the **self-contained single-file bundle survives as a side artifact outside `exports`** — `dist/scalus-bundle.js`, documented for `<script type="module">` / CDN use only and never importable as `scalus`. (This also settles taste call #8: the CDN story is preserved without constraining the package layout.)

3. **package.json.** Layout: the Scala.js linker writes into `dist/`; each root-level `<module>.js` is a one-line re-export stub (`export * from "./dist/ledger.js";`) and each root-level `<module>.d.ts` is the generated declaration file. Root-level stubs exist so `scalus/ledger` also resolves as a plain path for `node10` consumers. Add the `exports` map and keep the legacy fields:

   ```jsonc
   {
     "main": "./scalus.js", "module": "./scalus.js", "types": "./scalus.d.ts",
     "exports": {
       ".":            { "types": "./scalus.d.ts",    "import": "./scalus.js" },
       "./ledger":     { "types": "./ledger.d.ts",    "import": "./ledger.js" },
       "./txbuilder":  { "types": "./txbuilder.d.ts", "import": "./txbuilder.js" },
       "./eval":       { "types": "./eval.d.ts",      "import": "./eval.js" },
       "./emulator":   { "types": "./emulator.d.ts",  "import": "./emulator.js" },
       "./rules":      { "types": "./rules.d.ts",     "import": "./rules.js" },
       "./uplc":       { "types": "./uplc.d.ts",      "import": "./uplc.js" },
       "./package.json": "./package.json"
     },
     "typesVersions": { "*": { "*": ["./*.d.ts"] } },
     "files": ["*.js", "*.d.ts", "dist/**", "README.md"]
   }
   ```

   Note `scalus.js` here is the *stub* over `dist/main.js`, not the bundled artifact — the bundle is `dist/scalus-bundle.js` and is deliberately absent from `exports`.

   Three mechanical rules from the Node and TypeScript docs that the map above obeys:
   - *"Within the `"exports"` object, key order is significant. During condition matching, earlier entries have higher priority."* and *"`"types"` ... should always be included first."* — hence `types` before `import` in every entry.
   - Declaring `exports` **encapsulates the package**: undeclared subpaths start throwing `ERR_PACKAGE_PATH_NOT_EXPORTED`. See §0 — this is the one genuinely breaking part of the change for existing downstream consumers.
   - `"./package.json"` is exported deliberately; viem and evolution-sdk both do this, and some tooling requires it.

   **The auto-import payoff is documented, and it is the strongest DX argument for subpaths.** The TypeScript team's own explanation (andrewbranch, microsoft/TypeScript#53116): *"when we do that, we only look at the package's entry points (`main`/`types` and `exports`) and files transitively referenced by those entry points, so we can avoid arbitrarily deep FS hits... But I think an explicit list of `exports` is generally considered better practice, and I don't mind privileging packages that tell us up front what work we're going to do."* An explicit subpath list is what makes those subpaths auto-importable at all. Conversely, TypeScript shipped `autoImportFileExcludePatterns` in 4.8 precisely because *"These modules might have lots of exports that can pollute the auto-imports list and make it harder to navigate"* — the failure mode of a single flat 600-name surface.

   Why both: TypeScript reads `"exports"` only under `moduleResolution` `node16`/`nodenext`/`bundler`; under `node10`/`node` **subpath exports are invisible**. `"typesVersions"` is read in *all* modes but ignored when `"exports"` is read — so the two together cover both worlds. Keeping the entry files at the package root also makes `scalus/ledger` resolve as a plain path for `node10` consumers. This matters concretely: `scalus-examples/js/src/main/ts/tsconfig.json` is on `"moduleResolution": "node"` today.

4. **`generateDts`** emits N files into the npm dir instead of one. **`checkDtsUpToDate`** diffs the whole directory (`git diff --exit-code -- <npmDir>`), not one path.

5. **npm tests.** Add a per-subpath `tsc --noEmit` check, a `node --input-type=module` import smoke test per subpath, and one deliberate collision test that imports `@meshsdk/core` (already a devDependency) and `scalus/ledger` in the same file. Re-baseline `bundle-size.test.ts` per entry.

### 3.6 What this does and does not buy in bytes

Honest position: **splitting will not make today's 3.1 MB much smaller**, because the ledger and rules code is already linked in for the Emulator (§1.4). What it buys:

- `scalus/eval` becomes a genuinely small entry for consumers who only evaluate scripts (the most common integration today) — plausibly 1.5–2 MB instead of 3.1 MB, but **this is unmeasured**.
- Adding TxBuilder — the only area with zero current footprint — no longer taxes every consumer.
- Do **not** set `"sideEffects": false` on the package without testing. Scala.js emits module-level static initializers; declaring the package side-effect-free is a correctness claim, not a hint, and the payload win here comes from entry splitting rather than from consumer-side tree-shaking of a minified megabundle.

---

## 4. What the generator must gain

Seven changes, roughly in dependency order. None of them is architectural.

**4.1 Read the moduleID.** `ExportCollector.annotStringArg` (`ExportCollector.scala:51`) currently matches `Apply(_, Literal(StringConstant(s)) :: _)` and keeps only the head. Extend to return `(name, moduleId)` with `moduleId` defaulting to `"main"`. The annotation's secondary constructor discards the argument at runtime, but the annotation *tree* in TASTy retains both literals, so this is readable — **confirm with a two-line fixture before building on it.**

**4.2 Emit one file per module.** Replace `--output <file>` with `--output-dir <dir>`; write `<moduleID>.d.ts`. Sorting, dedup and the duplicate-name check all become per-module — matching the linker's own `(ModuleID, name)` key.

**4.3 Cross-module type references.** `TsType.Named` carries no origin and `knownNames` is `Map[fqn, String]`. Make it `Map[fqn, (moduleId, tsName)]`, and when a module references a name owned by another module, emit at the top of the file:

```ts
import type { Value, Address } from "./ledger.js";
```

The `.js` extension is required for `node16`/`nodenext` consumers. Imports must be deterministically ordered and deduped, like the rest of the emitter.

**4.4 Placement rule for chased types.** Non-exported `js.Object` traits (`SubmitResult`, `EmulatorInitialState`, …) have no moduleID of their own. Rule:

1. A chased type lands in the module of the exported member that references it.
2. If exactly one module references it, done.
3. If two or more modules reference it, **hard-error**, naming the type and the modules, and require disambiguation.

This is the *one* place a `@TsModule("ledger")` annotation is warranted — on chased types only, as an error-resolution escape hatch, never as the primary mechanism for exported declarations. (Rejected alternative: a shared `internal.d.ts` imported by every module — it needs its own `exports` subpath or a hidden path, and it silently re-couples the modules.)

**4.5 A name lint.** Hard-error when a top-level export name in any module shadows a JS/TS global (list in §3.4 rule 7), and when a chased `@TsName` does the same. This catches `Redeemers.Array` and friends at generation time instead of at consumer-confusion time.

**4.6 `@TsType` needs a resolution check, and probably a branding helper.** A verbatim `@TsType("Value")` in the `txbuilder` module currently emits a dangling reference — nothing imports `Value`. Simplest correct rule: **verbatim types may only name globals and same-module declarations; anything else is an error.** For the 16 hash aliases, add `@TsBrand("TransactionHash")` (or accept the verbose `@TsType`) so they emit distinct branded types.

**4.7 Strictness policy — keep the hard errors; add a report mode.**

The instinct at 50x scale is to relax. Resist it. The surface grows by *curated facades*, not by pointing the generator at `shared/`; every non-exportable type in a facade signature is an authoring mistake, and a silent `any` at 600 declarations is far worse than at 13. Concretely:

- **Keep**: hard error naming member FQN + offending type + hint. Unchanged.
- **Add** `--report <file.json>`: run the collector, emit an inventory (exports per module, chased types, rejected members with reasons) and **exit 0**. This is a planning and review tool — "what would exporting `scalus.cardano.ledger` cost?" — and a nice CI artifact for surface diffs. It is explicitly *not* a build mode.
- **Reject**: per-module allowlists and an `any` fallback. Allowlists are a second source of truth (same objection as `@TsModule` for exported types) and they rot.
- **Reject**: a "migration period" leniency window. There is no migration — nothing is exported today that would newly fail.

---

## 5. Staged rollout

**Stage 0 — the spike (do this first, ~1 day, no API change).**
Give today's 13 exports `moduleID = "main"` explicitly, then move *only* the four eval functions and their result classes to a second module `eval`, keeping deprecated re-exports at the root. This proves, end to end: moduleID survives into TASTy; the linker emits `main.js` + `eval.js`; the same name can live in two modules; `@JSExportStatic` members follow their class's module; internal shared-module filenames are stable across builds (unknown #5 — it decides whether `files` needs globs and whether the stubs can hard-code paths); the `exports` map resolves under `bundler`, `node16` **and** `node10`; and it produces the **first real bundle-size measurement** for a split entry. Everything below is contingent on that number.

**Stage 1 — `scalus/ledger`, read path.** Highest value per unit of work by a wide margin: today JS users hold opaque CBOR blobs and cannot inspect anything. Ship `decodeTransaction`/`encodeTransaction`, a `Transaction` handle with accessors, `Value`, `Address`, `Utxo`, `Input`/`Output`, `Coin`, the branded hashes, `CardanoInfo`, `ProtocolParams`, `SlotConfig` (moves here), and `Certificate`/`Script`/`Datum` as discriminated unions. ~30 types.

**Stage 2 — `scalus/txbuilder`.** `TxBuilder` with the ~12 methods people actually use (`payTo`, `spend`, `mint`, `collaterals`, `references`, `attach`, `metadata`, `validDuring`, `minFee`, `build`, `sign`, `transaction`/`draft`), **not** all 80 defs and **not** the 19-case step DSL. `complete()` returns a `Promise`.

**Stage 3 — `scalus/emulator`.** Mechanical: `Emulator` moves out of the root, root keeps a deprecated re-export.

**Stage 4 — `scalus/rules`.** One or two free functions (`validateTransaction`, `applyTransaction`) plus a `RuleError` interface carrying `{ rule, message }`. Deliberately *not* the 28 validator objects.

**Stage 5 — `scalus/uplc`.** `Program` (from/to CBOR/flat/text), `Data` as a discriminated union, `PlutusVM`. Lowest urgency: `scalus/eval` already covers the common case.

**Stage 6 — `scalus/wallet`.** `HdAccount`, signing, `Bech32`.

**Never export:** the STS class zoo (`STS`, `Validator`, `Mutator`, 28 validators — path-dependent `Validator { type Error = E }` has no TS analogue at all); `TransactionBuilderStep` (19) and `StepError` (28); `DefaultFun` (101 cases); `Term`, `Constant`, `DefaultUni`, `CekMachine`, `CekValue`; `KeepRaw`, `Sized`, the five `Tagged*` types, `Word64`; all 274 `given` codecs as givens; `Lens`/optics; `scalus.compiler.sir` and the SIR compiler; anything in a `*.internal` package; `MachineParams`/`PlutusParams` (the 1,441-`Long` cost-parameter record — expose cost models as `number[][]`, as `evalPlutusScripts` already does).

**On the interop style guide (CLAUDE.md Tier-0 / Tier-1):**

- **Tier-0 helps directly and should be extended to `txbuilder` and `ledger` now.** No default parameters (the exporter maps them to optional params, which silently reorders meaning across overload sets), `@varargs`, named aliases for symbolic operators (`Value.+` → `plus`), non-implicit codec entry points, and no `using` clauses on public entry points — every one of those removes facade work later. `Program.evaluate(using vm)` and `TxBuilder.spend[T: ToData]` are the two worst offenders today.
- **Tier-1 `<ClassName>Platform` traits do *not* help here, and expecting them to will cost time.** `@JSExportTopLevel` must sit on the `ClassDef`, which lives in `shared/` where the annotation does not exist; and the compiler flatly refuses to export traits at all. Platform traits remain the right tool for the JVM/Java surface and for adding JS-friendly *Scala* helpers, but the TS surface must be top-level facade classes and objects in `js/` sourcesets. Recommendation: put them in one file per TS module (e.g. `scalus-cardano-ledger/js/src/main/scala/scalus/interop/js/Ledger.scala`), so "one file = one subpath" and the moduleID is impossible to get wrong by accident.

---

## 6. Risks, unknowns, and owner taste calls

**Unverified — confirm before committing code:**

1. That the `moduleID` literal is reachable from TASTy annotation trees. Strongly expected (annotation trees keep call arguments even though the secondary constructor discards it), but it is the load-bearing assumption of the whole design. Two-line fixture.
2. That two modules may export the same name. Evidence is strong — the linker's analyzer keys top-level exports by `(ModuleID, String)` — but it was read out of compiled bytecode, not tested.
3. That `@JSExportStatic` members follow their companion class's moduleID rather than defaulting to `main`.
4. **Every bundle-size number in §3.6 is an estimate.** Stage 0 exists to replace them with measurements.
5. Whether Scala.js linker-generated *internal* module filenames are stable across builds — if they are hashed, `files`/`checkDtsUpToDate` need globs, and a CDN story needs care.
6. Whether making facade error classes `extend js.Error` interacts badly with Scala.js exception semantics.

**Risks:**

- **esbuild code splitting is explicitly "a work in progress."** The recommendation routes around it (publish linker ESM, keep the bundle as a non-`exports` CDN artifact), but that trades a 3.1 MB self-contained artifact for a 7.8 MB directory plus two real `@noble/*` dependencies. If "self-contained single file" is a hard product requirement for the npm entry too, this design gets harder.
- **Two copies of the runtime is the failure mode to guard against.** Any layout where some `exports` entries come from a bundled artifact and others from the linker output ships duplicated Scala.js state and breaks `instanceof` across entries. §3.5(2) forbids it; a CI test that imports two subpaths and asserts a shared identity (e.g. `Object.is` on a shared static) is worth writing at Stage 0.
- **Legacy consumers.** `moduleResolution: "node"` makes subpaths invisible to TypeScript; our own `scalus-examples/js/src/main/ts` is on `"node"` + CommonJS and already cannot `require()` an ESM package. Some consumer breakage is unavoidable; `typesVersions` plus root-level entry files minimizes it.
- **Facade drift.** The generator kills *declaration* drift, not *semantic* drift between the Scala type and its JS facade. Nothing proposed here detects that a facade's `Value.plus` diverges from `Value.+`. Mitigation: shared behavioural tests, or accept it.
- **Volume.** Stage 1 alone is ~30 hand-written facade types. This is weeks, not days, and it creates a second API surface that must be maintained forever. The alternative — staying CBOR-only — is cheaper and much worse to use. That trade is real and should be made deliberately.
- **`scalus/ledger` merges `cardano.ledger` × 2 modules + `cardano.address`.** Good for users, but it means the module boundary no longer matches any Scala package, so `--exclude` prefixes stop being a useful organizing tool.

**Taste calls that are the owner's, not the engineer's:**

1. **`bigint` vs `number` for quantities.** `bigint` is technically forced for lovelace (4.5e16 > 2^53), but it is contagious and slightly awkward (`10n`). The alternative — hex/decimal strings, as the current `initialStakeRewards` dictionary does — is uglier but interoperates better with JSON. Recommendation: `bigint`.
2. **`Uint8Array` vs hex `string` for hashes and keys.** Recommendation: `Uint8Array` in data positions, hex `string` for anything used as a map key, with `*Hex` accessors. Reasonable people prefer all-hex.
3. **Root barrel or no barrel.** Recommendation: no barrel.
4. **Module granularity.** Eight subpaths as proposed, or a coarser three (`scalus`, `scalus/ledger`, `scalus/uplc`). Finer is better for payload, worse for "where do I import this from?".
5. **`Transaction` as an opaque handle vs a plain structural object.** Recommendation: handle. A structural object is more idiomatic 2026 TS and would please a solana-kit-shaped audience; it costs much more code and runtime.
6. **Single package with subpaths vs scoped multi-package (`@scalus/ledger`, `@scalus/txbuilder`).** Recommendation: single package now. Multi-package is possible later but the shared Scala.js runtime chunk makes it awkward, and it multiplies release overhead.
7. **`undefined` vs `null` for absence.** Recommendation: `undefined` everywhere; treat existing `| null` returns as bugs to fix in the same window.
8. **Whether to keep a self-contained single-file bundle at all.** Recommendation: yes, but only as `dist/scalus-bundle.js` outside the `exports` map, for CDN/`<script type="module">` use.

---

## Sources

**Verified firsthand in this session** (WebFetch of primary docs, plus reading published artifacts out of `scalus-cardano-ledger/js/src/main/npm/node_modules`, plus extracting error strings from the compiler and linker jars in the local Coursier cache).

*Scala.js / Scala 3*
- Module splitting and `@JSExportTopLevel(name, moduleID)`: https://www.scala-js.org/doc/project/module.html
- Exporting APIs to JavaScript: https://www.scala-js.org/doc/export-to-javascript.html
- "generate .d.ts" issue: https://github.com/scala-js/scala-js/issues/3836
- Export restrictions extracted from `dotty.tools.dotc.transform.sjs.PrepJSExports` in `scala3-compiler_3-3.3.8.jar`; per-module export keying from `org.scalajs.linker.analyzer.AnalyzerRun` and `OutputPatterns` in `scalajs-linker_2.12-1.22.0.jar` / `scalajs-linker-interface_2.12-1.22.0.jar`; `JSExportTopLevel` secondary constructor from `scalajs-library_2.13-1.22.0-sources.jar`

*Node / TypeScript / bundlers / editors*
- Node package entry points, subpath exports, conditional exports, `ERR_PACKAGE_PATH_NOT_EXPORTED`, key ordering: https://nodejs.org/api/packages.html
- TS modules reference (`exports`, `types` condition, `typesVersions`, node10 feature matrix): https://www.typescriptlang.org/docs/handbook/modules/reference.html
- TS handbook, Modules ("While not deprecated..."): https://www.typescriptlang.org/docs/handbook/2/modules.html
- TS handbook, Namespaces and Modules ("Needless Namespacing"): https://www.typescriptlang.org/docs/handbook/namespaces-and-modules.html
- `moduleResolution` reference: https://www.typescriptlang.org/tsconfig/#moduleResolution
- TS release notes 4.7 (types-condition ordering, separate CJS/ESM declarations), 4.8 (`autoImportFileExcludePatterns`), 5.0 (library-author guidance), 5.6 (`autoImportSpecifierExcludeRegexes`): https://www.typescriptlang.org/docs/handbook/release-notes/
- Auto-import from `exports` subpaths (TS team comment + fix): https://github.com/microsoft/TypeScript/issues/53116 and https://github.com/microsoft/TypeScript/pull/54831
- typescript-eslint `no-namespace` (note `allowDefinitionFiles: true` default): https://typescript-eslint.io/rules/no-namespace/
- esbuild code splitting ("still a work in progress"): https://esbuild.github.io/api/#splitting and https://github.com/evanw/esbuild/issues/399
- esbuild tree shaking, and the re-exported-namespace limitation: https://esbuild.github.io/api/#tree-shaking and https://github.com/evanw/esbuild/issues/1420
- webpack tree shaking / `sideEffects`: https://webpack.js.org/guides/tree-shaking/ and https://github.com/webpack/webpack/issues/9607
- Rollup `output.freeze`: https://rollupjs.org/configuration-options/ and https://github.com/rollup/rollup/issues/2225
- Vite "Avoid barrel files": https://vite.dev/guide/performance#avoid-barrel-files
- VS Code TypeScript editing / auto-imports: https://code.visualstudio.com/docs/typescript/typescript-editing

*viem*
- https://viem.sh/docs/introduction · https://viem.sh/docs/clients/custom · https://viem.sh/bench-bundlesize.svg
- https://github.com/wevm/viem (`src/index.ts`, `src/chains/index.ts`) · https://unpkg.com/viem/package.json

*ethers v6*
- https://docs.ethers.org/v6/migrating/ · https://docs.ethers.org/v6/getting-started/
- https://github.com/ethers-io/ethers.js (`src.ts/ethers.ts`) · https://unpkg.com/ethers/package.json · https://github.com/ethers-io/ethers.js/issues/1009

*Solana*
- https://github.com/anza-xyz/kit (README "Statistics" table and tree-shaking rationale) · https://www.solanakit.com/docs
- https://www.anza.xyz/blog/solana-web3-js-2-release-candidate · https://www.anza.xyz/blog/solana-web3-js-2-release · https://www.anza.xyz/blog/meet-kit-the-new-solana-javascript-sdk
- https://registry.npmjs.org/@solana/kit · https://registry.npmjs.org/@solana/web3.js

*Cardano*
- Lucid Evolution: https://github.com/Anastasia-Labs/lucid-evolution · https://anastasia-labs.github.io/lucid-evolution/install · https://unpkg.com/@lucid-evolution/lucid/package.json · https://registry.npmjs.org/@lucid-evolution/scalus-uplc
- Evolution SDK (IntersectMBO): https://github.com/IntersectMBO/evolution-sdk · `docs/content/docs/introduction/imports.mdx` · https://unpkg.com/@evolution-sdk/evolution/package.json
- Mesh: https://github.com/MeshJS/mesh (`packages/mesh-core/src/index.ts`, `packages/mesh-scalus-emulator`) · https://meshjs.dev/ · packaging and the `scalus` dependency read firsthand from `@meshsdk/core@1.9.1` (registry) and `@meshsdk/core-cst@1.9.0-beta.101` (installed locally)
- CSL: https://github.com/Emurgo/cardano-serialization-lib · https://unpkg.com/@emurgo/cardano-serialization-lib-browser@17.0.0/cardano_serialization_lib.d.ts
- CML: https://github.com/dcSpark/cardano-multiplatform-lib · https://unpkg.com/@dcspark/cardano-multiplatform-lib-browser@6.2.0/cardano_multiplatform_lib.d.ts
- wasm-bindgen (note: `https://rustwasm.github.io/wasm-bindgen/` 404s; use) https://wasm-bindgen.github.io/wasm-bindgen/ · .../reference/weak-references.html · .../reference/types/exported-rust-types.html · https://rustwasm.github.io/docs/wasm-pack/commands/build.html

*Kotlin/JS*
- https://kotlinlang.org/docs/js-to-kotlin-interop.html · https://kotlinlang.org/docs/js-ir-compiler.html · https://kotlinlang.org/docs/js-modules.html
- Golden `.d.ts` fixtures at `JetBrains/kotlin` tag v2.4.10: `js/js.translator/testData/typescript-export/js/module-systems/{esm,commonjs,umd,plain}.d.ts` and `.../namespaces/namespaces.d.ts`
- `PackageJson.kt` (no `exports` field): `libraries/tools/kotlin-gradle-plugin/.../targets/js/npm/PackageJson.kt`
- YouTrack: KT-66524 (ESM name clash), KT-81864 (per-file `types` pointer)
