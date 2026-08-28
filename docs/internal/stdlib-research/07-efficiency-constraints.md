# 07 – Efficiency constraints: what compiles to cheap UPLC in Scalus today

Research input for the new high-level on-chain standard library API.
Every claim below cites `file:line` in this repository or a number measured by a
test/doc in this repository.

**Provenance conventions**

| Tag | Meaning |
|---|---|
| (worktree) | file exists in `/Users/nau/projects/lantr/scalus/.claude/worktrees/stdlib-api-research` (this worktree, branched from `origin/master`) |
| (primary) | file exists **only** in the primary checkout `/Users/nau/projects/lantr/scalus`, i.e. uncommitted / not yet on `origin/master` |
| (unverified) | prior-session knowledge with no repo artifact – do not treat as a budget |

Unless tagged, paths are relative to the worktree root and the file is present in both checkouts.

**Baseline assumptions used throughout**

- Default compiler options: `SirToUplcV3Lowering` backend, `targetLanguage = PlutusV3`,
  `targetProtocolVersion = vanRossemPV` (PV11), `generateErrorTraces = true`,
  `removeTraces = false`, `optimizeUplc = false`, `valueBuiltins = true`
  (`scalus-core/shared/src/main/scala/scalus/compiler/compiler.scala:8-45`,
  `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIRDefaultOptions.scala:11-21`).
- **`optimizeUplc` is OFF by default.** Plain `sir.toUplc()` gets *no* UPLC-level optimizer.
  Only `Options.release` turns it on (`compiler.scala:63-70`). Any design rule that depends on
  the optimizer is therefore conditional; rules below say so explicitly.

> **Note on the task premise.** `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/prelude/DataOps.scala`
> is a 9-line stub whose entire body is commented out; the only live line is
> `type BuiltinData = scalus.uplc.builtin.Data` (`DataOps.scala:1-9`). The real `Data` API lives in
> `scalus-core/shared/src/main/scala/scalus/uplc/builtin/DataApi.scala` (325 lines) and in the
> `FromData`/`ToData` given instances.

---

## 0. The unit of account: what one CEK step costs

| Quantity | Value | Source |
|---|---:|---|
| 1 CEK machine step (var/const/lam/delay/force/apply/builtin/constr/case) | **16 000 cpu, 100 mem** | `scalus-core/shared/src/main/scala/scalus/uplc/eval/Cek.scala:52-63` (`defaultMachineCostsB`, aliased as `defaultMachineCostsC`/`defaultMachineCosts` at `:65-66`) |
| Machine startup | 100 cpu, 100 mem | `Cek.scala:53` |
| mainnet `price_step` | 7.21e-05 lovelace / cpu unit | `scalus-core/shared/src/main/resources/blockfrost-params-epoch-645.json:1045` |
| mainnet `price_mem` | 0.0577 lovelace / mem unit | `blockfrost-params-epoch-645.json:1044` |
| fee formula | `ceil(priceMemory*memory + priceSteps*steps)` | `scalus-core/shared/src/main/scala/scalus/cardano/ledger/Types.scala:521` |
| `max_tx_ex_mem` / `max_tx_ex_steps` | 16 500 000 / 10 000 000 000 | `blockfrost-params-epoch-645.json:1046-1047` |
| `min_fee_a` (per tx byte) / `min_fee_b` | 44 / 155 381 | `blockfrost-params-epoch-645.json` |

`CardanoInfo.mainnet` loads **`blockfrost-params-epoch-645.json`** (mainnet epoch 645, PV11 van
Rossem) – `scalus-core/shared/src/main/scala/scalus/cardano/ledger/CardanoInfo.scala:22-27`.
Do **not** cite `scalus-core/shared/src/main/resources/protocol-params.json`: same price numbers, but
it is a **PV10** snapshot (`protocolVersion.major = 10` at `:1073`) and `CardanoInfo.mainnet` never
loads it.

Derived design budgets (arithmetic over the constants above):

- **1 CEK step ≈ 6.92 lovelace** at mainnet prices (`16000*7.21e-5 + 100*0.0577 = 1.154 + 5.77`).
  **Memory, not CPU, is the dominant fee term for pure machine steps** (83 % of the cost).
- **Memory is the binding budget limit**: `16.5e6 / 100 = 165 000` machine steps per transaction,
  versus `10e9 / 16000 = 625 000` steps on the CPU limit. A validator that is "CPU-cheap" but
  allocation-heavy hits the wall ~3.8x sooner.
- 2 saved machine steps ≈ 13.85 lovelace; 6 saved steps ≈ 41.5 lovelace.

Cross-checks in the repo confirm the arithmetic:

- `scalus-core/jvm/src/test/scala/scalus/uplc/eval/ExprSizeAndBudgetTest.scala:82-106` –
  **one recursive call = 32 256 cpu, 102 mem, `ceil` 9 lovelace** (asserted at `:101-103`),
  and `0.5419298016` USD-millicents at 0.66 USD/ADA (`:105`).
- `ExprSizeAndBudgetTest.scala:293-312` (T2 proof) – self-application beats the Z combinator by
  exactly **6 machine steps per call = 96 000 cpu / 600 mem** (asserted `:310-311`); the same
  delta is re-asserted for a 2-arg loop at `:314-323`.
- `ExprSizeAndBudgetTest.scala:381-409` (T1 proof) – lifting 2 loop-invariant arguments out of a
  recursive call saves the same **96 000 cpu / 600 mem per call** (`:408-409`).

> Caveat: `ExprSizeAndBudgetTest` declares `given Options = Options(targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering)`
> at `:31-32` "to have stable sizes in terms, not in data representation". Its absolute numbers are
> **Scott-backend** numbers. The *per-machine-step* deltas (96 000/600, 32 256/102) are backend
> independent because they are pure CEK-step arithmetic; the flat sizes are not.

---

## 1. Data-level vs ADT-level representation

### 1.1 The three lowering backends

`TargetLoweringBackend` has exactly three cases
(`scalus-core/shared/src/main/scala/scalus/compiler/sir/SIRDefaultOptions.scala:3-6`), dispatched in
`scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/UplcPipeline.scala:43-69`:

| Backend | Entry | ADT encoding | Status |
|---|---|---|---|
| `ScottEncodingLowering` | `.../lowering/simple/ScottEncodingLowering.scala` (133 lines) | Scott-encoded lambdas | legacy / for stable term sizes |
| `SumOfProductsLowering` | `.../lowering/simple/SumOfProductsLowering.scala` (163 lines) | UPLC `Constr`/`Case` | legacy |
| **`SirToUplcV3Lowering`** | `.../lowering/SirToUplcV3Lowering.scala` (168 lines) | **representation-directed**; case classes are `Data` by default | **default** (`SIRDefaultOptions.scala:13`) |

Only `SirToUplcV3Lowering` receives `intrinsicModules` (`UplcPipeline.scala:67`); the two `simple`
backends are passed none (`UplcPipeline.scala:44-57`), so **every intrinsic in
`scalus/compiler/intrinsics/**` is inert on the Scott / SumOfProducts backends**.

The full pipeline is `removeTraces? → MutualRecursionElimination → StaticArgumentTransformation? →
lower(backend) → optimize? → fill positions` (`UplcPipeline.scala:10-23`, code at `:38-82`).

### 1.2 What decides a case class's runtime representation

`SirTypeUplcGenerator.apply(tp)` picks a *typegen* per SIR type
(`scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/typegens/SirTypeUplcGenerator.scala:313-429`).
For a `SIRType.CaseClass` the order is (`:383-397`):

1. non-trivial structural constraints (`resolveWithConstraints`, `:453-...`) – List/BuiltinList/PairList;
2. **`@UplcRepr` annotation** on the constructor decl (`:386-392`);
3. basic structural inference (`resolveBasicStructural`, `:491-496`).

`resolveBasicStructural` is two lines of policy:

```scala
val hasFun = containsFun(constrDecl, new IdentityHashMap[SIRType, SIRType]())
if hasFun then ProductCaseUplcConstrOnlyEmitter
else ProductCaseEmitter
```
(`SirTypeUplcGenerator.scala:494-496`)

- **A plain case class → `ProductCaseEmitter` → `ProductCaseClassRepresentation.ProdDataList`**
  (`typegens/ProductCaseEmitter.scala:41`), with `defaultDataRepresentation = ProdDataConstr`
  (`ProductCaseEmitter.scala:48-51`). I.e. **Data-backed by default**.
- **A case class that transitively contains a function type → `ProductCaseUplcConstrOnlyEmitter`**,
  i.e. a UPLC `Constr` that can never be Data. Same rule on the sum side
  (`SirTypeUplcGenerator.scala:369-370`: `else if !containsFun(tp, trace) then DataConstrEmitter else SumCaseUplcConstrOnlyEmitter`).
  **Embedding a closure in a stdlib type silently forces it out of the Data world** and blocks
  `equalsData`, `Data` field access, and every Data-shaped intrinsic.
- Explicit control is `@UplcRepr(UplcRepresentation.X)`
  (`scalus-core/shared/src/main/scala/scalus/compiler/annotations.scala:62-90`), with the case
  set enumerated at `scalus-core/shared/src/main/scala/scalus/compiler/UplcRepresentation.scala:12-29`
  (`DataData, Constant, PackedData, DataConstr, PackedSumDataList, SumBuiltinList(_), ProdBuiltinPair(_,_),
  ProductCase, SumCase, PackedDataMap, Data, BuiltinArray, ProductCaseOneElement, SumPairDataList, UplcConstr, TypeVar(_)`).

Representative annotations already in the stdlib:

| Type | Annotation | Line |
|---|---|---|
| `plutus.v1.Value` | `ProductCaseOneElement` | `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Value.scala:15` |
| `prelude.SortedMap` | `PackedDataMap` | `.../prelude/SortedMap.scala:16` |
| `prelude.AssocMap` | `PackedDataMap` | `.../prelude/AssocMap.scala:10` |
| `prelude.Varargs` | `ProductCaseOneElement` | `.../prelude/Varargs.scala:9` |
| `plutus.v1.PubKeyHash` | `ProductCaseOneElement` | `.../plutus/v1/Contexts.scala:448` |
| `plutus.v3.TxId` | `ProductCaseOneElement` | `.../plutus/v3/Contexts.scala:45` |

Six types are on a hard-coded "must be annotated" watchlist that prints a stderr warning if the
annotation is lost during lowering (`SirTypeUplcGenerator.scala:434-448`).

### 1.3 Field access cost: Data-backed product

`ProdDataListOps.genSelect` (`.../lowering/typegens/ProdDataListOps.scala:99-195`) is the whole story:

1. convert scrutinee to `ProdDataList` (`:106`) – a `unConstrData … sndPair` if it came in as `Data`;
2. walk to the field:
   - **PV11 and `fieldIndex >= 2`: one `dropList(fieldIndex, list)` builtin** (`:135-159`);
   - otherwise: a chain of `fieldIndex` × `tailList` (`:161-186`);
3. `headList` (`:187-193`);
4. then an `un*Data` if the consumer wants a primitive.

So a field read on a Data-backed case class is:

| Protocol version | Field index | Builtin calls |
|---|---|---|
| PV11 | 0 | `headList` (1) |
| PV11 | 1 | `tailList`, `headList` (2) |
| PV11 | n ≥ 2 | `dropList`, `headList` (**2, constant**) |
| PV10 / V1-V3 | n | n × `tailList` + `headList` (**n+1, linear**) |

plus one `unConstrData`/`sndPair` if the value arrives as raw `Data`.

**Reuse.** Both the tail chain and the `dropList` result are memoised as scope-keyed lazy vars –
`list0id + "_tail_${idx+1}"` (`:164`) and `list0id + "_drop_$fieldIndex"` (`:138`), looked up with
`lctx.scope.getById` (`:139`, `:165`) and emitted through `ScopeBracketsLoweredValue` (`:194`).
Scope is a plain id→var map (`.../lowering/LocalScope.scala:3-34`). Consequence: **repeated field
reads of the same scrutinee inside the same lowering scope share the spine**, so `tx.inputs` used
five times pays the walk once. This is reuse *within the lowering scope*, not a general CSE pass.

### 1.4 Field access cost: `@UplcRepr(UplcConstr)` product

Field projection becomes a UPLC `Case` on a `Constr` – one machine step, no builtin. The trade-off
is that such a value can never be `Data`, so it cannot be compared with `equalsData`, cannot be a
datum/redeemer without an explicit conversion, and is excluded from Data-shaped intrinsics.

### 1.5 Match / sum dispatch cost

`ProdDispatch.genMatch` routes by representation (`.../lowering/ProdDispatch.scala:76-108`):
`Prod|SumUplcConstr → SumUplcConstrOps.genMatchUplcConstr` (native `Case`), Data shapes →
`ProdDataListOps.genMatch`.

For a Data-backed sum, `DataConstrEmitter.genMatchDataConstr`
(`.../typegens/DataConstrEmitter.scala:262-...`) emits:

- `unConstrData` (via the `PairIntDataList` representation, `:271-277`),
- `fstPair` → the constructor index (`:279-292`),
- `sndPair` → the field list (`:294-310`),
- then the dispatch:
  - **PV11+: a single `Case` on the integer tag** – O(1) in the number of variants (`:315-319`, `lvCaseInteger`);
  - **pre-PV11: a chain of `equalsInteger` + `ifThenElse`** – O(#variants) (`:320-...`).

Same PV11 "case on builtin" switch appears for `Data` (`typegens/DataSirTypeGenerator.scala:351-355`)
and for lists (`typegens/SumListEmitterCommon.scala:417`).

### 1.6 **`FromData` deriving is FREE – the decode is erased, not lazy**

This is the single most important fact for the new API.

`FromData.derived` is a macro (`scalus-core/shared/src/main/scala/scalus/uplc/builtin/FromData.scala:21-23`)
that expands, for a case class, to an **eager** decoder
(`scalus-core/shared/src/main/scala/scalus/uplc/builtin/FromDataMacros.scala:54-124`):

```scala
(d: Data) =>
  val args = Builtins.unConstrData(d).snd
  new TxInfo(fromA(args.head), fromB(args.tail.head), fromC(args.tail.tail.head), …)
```

`genGetter` (`FromDataMacros.scala:91-100`) literally builds `args.tail.tail.…head` per field, so on
the JVM (and under the Scott backend) decoding `TxInfo` means 16 field decodes plus an O(n²) tail
walk. `TxInfo` has **16 fields** (`.../plutus/v3/Contexts.scala:854-871`) and derives its instance at
`.../plutus/v3/Contexts.scala:913`; `ScriptContext` at `.../plutus/v3/Contexts.scala:1108`.

**But under the default backend that code is never emitted.** Two steps erase it:

1. **Link time.** `SIRLinkerOptions.useUniversalDataConversion = (backend == SirToUplcV3Lowering)`
   (`.../compiler/sir/linking/SIRLinker.scala:20-21`). When true, the linker rewrites every
   `fromData`/`toData`-annotated `SIR.Apply` head into
   `UniversalDataConversion.fromData` / `.toData` (`SIRLinker.scala:300-318`:
   `if options.useUniversalDataConversion then anns.data.get("fromData") … anns.data.get("toData")`)
   instead of linking the real given instance. `UniversalDataConversion` itself is a marker object whose methods throw if ever
   actually called (`scalus-core/shared/src/main/scala/scalus/uplc/builtin/internal/UniversalDataConversion.scala:6-51`).
2. **Lowering.** `Lowering.lowerFromData` (`.../lowering/Lowering.scala:1089-1112`) returns a
   `ProxyLoweredValue(data)` whose `termInternal` is verbatim `data.termInternal(gctx)` (`:1096-1097`)
   – **the identity function**. Only the *type* and *representation* change (`:1092-1095`).

So `fromData[TxInfo](d)` costs **zero UPLC**. It is not "lazy decoding": there is no decoder at all.
The value stays as `Data` bytes and every subsequent `tx.field` pays the `dropList`/`headList` walk
of §1.3 on demand.

This is confirmed by a shipped budget test's own scaladoc:
`scalus-core/jvm/src/test/scala/scalus/uplc/eval/ValueBuiltinsBudgetTest.scala:48-49` –
*"`fromData[Value]`/`toData` are free (identity), so the SortedMap strategy has near-zero fixed cost
(884K-1.27M cpu)"* – and by the design doc
`docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md:26-28`.

**The trap.** The linking decision is made at *Scala compile time* from an implicit search for
`scalus.compiler.Options` at the `compile { … }` call site
(`scalus-plugin/src/main/scala/scalus/compiler/plugin/Plugin.scala:52-82`, `retrieveCompilerOptions`
→ `typer.inferImplicit`; used at `:136` and `:145`). If a file/class-level
`given Options = Options(ScottEncodingLowering)` is in scope, the **real** eager decoders are linked
into the SIR, and a later `sir.toUplc(backend = SirToUplcV3Lowering)` cannot undo it.

- Repo evidence of the magnitude: `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md` and the T7 design doc
  record `fromData[Value]`/`toData` as *free (identity)* under V3.
- The counter-measurement (66.5M cpu for a Scott-linked `fromData[Value](d).toData` on a 5×2 `Value`
  vs 112K identity) is **(unverified)** – I did not find it asserted in a repo test; treat the
  direction as established and the exact figure as anecdotal.

### 1.7 Summary of the representation trade-off

| | Data-backed (`ProdDataList`/`ProdDataConstr`, default) | `@UplcRepr(UplcConstr)` |
|---|---|---|
| `fromData` / `toData` | **free (identity)** | real conversion code |
| field read | 1-2 builtins (PV11), n+1 (PV10) | 1 `Case` machine step |
| `==` | 1 `equalsData` builtin | field-by-field `Case` + AND chain |
| usable as datum/redeemer | yes, zero-cost | needs explicit conversion |
| may contain closures | no | yes |
| PV11 sum dispatch | `unConstrData`+`fstPair`+`sndPair`+`Case` | `Case` |

---

## 2. Builtin-backed `Value` operations (CIP-153)

### 2.1 The builtins

All seven CIP-153 `MaryEraValue` builtins are wired end to end (enum case → SIR builtin → CEK
runtime → cost model → flat tag). Section header at
`scalus-core/shared/src/main/scala/scalus/uplc/DefaultFun.scala:1324-1329`.

| Builtin | `DefaultFun` | Signature | flat tag | `SIRBuiltins` | CEK |
|---|---|---|---:|---|---|
| `insertCoin` | `DefaultFun.scala:1341` | `ByteString → ByteString → Integer → Value → Value` | 94 (`:1616`) | `compiler/sir/SIRBuiltins.scala:597-601` | `uplc/Builtin.scala:1276` |
| `lookupCoin` | `:1352` | `ByteString → ByteString → Value → Integer` | 95 (`:1617`) | `:604-608` | `:1295` |
| `unionValue` | `:1361` | `Value → Value → Value` | 96 (`:1618`) | `:611-615` | `:1311` |
| `valueContains` | `:1372` | `Value → Value → Bool` | 97 (`:1619`) | `:618-622` | `:1329` |
| `valueData` | `:1381` | `Value → Data` | 98 (`:1620`) | `:625-629` | `:1347` |
| `unValueData` | `:1394` | `Data → Value` | 99 (`:1621`) | `:632-636` | `:1361` |
| `scaleValue` | `:1403` | `Integer → Value → Value` | 100 (`:1622`) | `:639-643` | `:1374` |

All are partial (can fail) except `lookupCoin` (`DefaultFun.scala:1491-1497`). All are in `batch6`,
available from `vanRossemPV` for PlutusV1/V2/V3 (`scalus/cardano/ledger/Builtins.scala:170-186`,
`:195-198`, `:205`, `:213`).

### 2.2 The gate

Three conditions must all hold:

1. `Options.valueBuiltins == true` – **default `true`**
   (`scalus-core/shared/src/main/scala/scalus/compiler/compiler.scala:45`, scaladoc `:36-44`).
   When false, `IntrinsicResolver.intrinsicModulesFor` removes the module outright
   (`.../lowering/IntrinsicResolver.scala:101-103`).
2. `targetProtocolVersion >= PV11` – registry entry
   `ValueModule -> List((WildcardRepr, 11, ValueIntrinsicsV11Module, ValueReprRules.rules, NoArgConvert))`
   (`IntrinsicResolver.scala:201-203`); the version test is at `:266` and `:284`.
3. backend is `SirToUplcV3Lowering` – only that arm receives `intrinsicModules`
   (`UplcPipeline.scala:67` vs `:44-57`).

**Failure mode is silent.** At PV10 or with the flag off, the linked SIR body (the portable
nested-map walk) is lowered unchanged, byte-identical to pre-T7 output
(`docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:419-420`; `compiler/intrinsics/ValueIntrinsics.scala:15-17`).
There is no warning.

Dispatch is by **method simple name** against a 7-entry map `ValueReprRules.rules`
(`compiler/intrinsics/ValueIntrinsics.scala:99-107`, matched at `IntrinsicResolver.scala:279-280`),
and the arity must match exactly (`IntrinsicResolver.scala:297-298`) – **partially applied `Value`
operations do not dispatch to builtins.**

### 2.3 Which `Value` operations are builtin-backed

Source: `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Value.scala`.

| Operation | def | PV11 + flag | Builtin | Provider |
|---|---|---|---|---|
| `quantityOf(cs, tn)` | `:795-800` | **builtin** | `lookupCoin` | `ValueIntrinsics.scala:32` |
| `getLovelace` | `:701` | **builtin (transitive)** | `lookupCoin` | via `:32` |
| `+` / `plus` | `:668`, `:439` | **builtin** | `unionValue` | `:47` |
| `-` / `minus` | `:671`, `:464` | **builtin** | `unionValue`+`scaleValue(-1)` | `:54` |
| `*` / `multiply` | `:674`, `:507` | **builtin** | `scaleValue` | `:64` |
| `unary_-` / `negate` | `:665`, `:413` | **builtin** | `scaleValue(-1)` | `:69` |
| `containsAtLeast(other)` | `:824-836` | **builtin** | `valueContains` | `:74` |
| `insertCoin(cs, tn, amt)` | `:863-873` | **builtin** | `insertCoin` | `:79` |
| `withoutLovelace` | `:984` | **builtin (transitive)** – body is `insertCoin(ada, ada, 0)` | `insertCoin` | via `:79` |
| `lovelaceAmount` | `:719` | loop | – | – |
| `isZero` / `nonZero` | `:734`, `:750` | loop | – | – |
| `isPositive` | `:758-759` | loop (nested `forall`) | – | – |
| `tokens(cs)` | `:901-902` | loop (`SortedMap.get`) | – | – |
| `hasOnly(cs, tn, amt)` | `:942-950` | loop + one `equalsData` | – | – |
| **`flatten`** | `:1015-1020` | **loop (nested `foldRight`)** | – | – |
| `policyIds` | `:1049` | loop (`SortedMap.keys`) | – | – |
| `===` / `eq` / `nonEq` | `:351`, `:374`, `:576` | loop (`SortedMap ===`) | – | – |
| `equalsAssets` | `:309-350` | loop | – | – |
| `valueOrd` / `<=>` | `:595` | loop | – | – |
| `toData` / `fromData` | `:613`, `:626` | **identity** (repr *is* Data) | – | – |
| factories (`zero`, `apply`, `lovelace`, `fromList`, `unsafeFromList`, …) | `:46`, `:74`, `:94`, `:188`, `:121` | loop | – | – |

There is **no** `unionWith`, `filter`, `toList` or `<=` on `Value`; the containment operator is
`containsAtLeast` (`:824`).

### 2.4 Representation and the phase-2 gap

`Value` is `@UplcRepr(ProductCaseOneElement) case class Value private (toSortedMap: SortedMap[PolicyId, SortedMap[TokenName, BigInt]])`
(`Value.scala:15-16`), and `SortedMap` is `@UplcRepr(PackedDataMap)` (`prelude/SortedMap.scala:16`).
So the runtime representation is literally the bytes of a `Data` map – which is why the intrinsics
can `typeProxy[Data](v)` for free (`ValueIntrinsics.scala:26-31`, asserted at `:94-97`).

The builtin opaque type `scalus.uplc.builtin.BuiltinValue` (`uplc/builtin/BuiltinValue.scala:25-27`)
is a **separate** type. A `BuiltinValueBacked` *representation* is **planned, not landed** – zero
occurrences in any `.scala` file; only prose in `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:432-436`
and `docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md:36-37, :177`.

**Consequence for the new API:** every intrinsic body currently does a full
`unValueData … valueData` round trip (`ValueIntrinsics.scala:47-83`), so a chain like
`(a + b + c).quantityOf(cs, tn)` re-parses the value between each step. Chained `Value` arithmetic
does not currently stay native.

### 2.5 Measured ratios

`docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md:13-28` –
*"measured 2026-08-18, 5 policies x 2 tokens, PV11 mainnet costs"*:

| Operation | portable prelude (cpu) | builtin (cpu) | ratio |
|---|---:|---:|---:|
| `quantityOf` / `lookupCoin` | 12.7 M | 0.99 M | **13x** |
| `plus` / `unionValue` | 125.9 M | 4.1 M | **31x** |
| `multiply` / `scaleValue` | 58.0 M | 3.3 M | **18x** |
| containment / `valueContains` | 110.3 M | 1.5 M | **75x** |

Fixed costs (`ValueBuiltinsBudgetTest.scala:48-52`): the SortedMap strategy has near-zero fixed cost
(884 K – 1.27 M cpu, because `fromData`/`toData` are identity), whereas `unValueData` pays real
parsing up front (3.7 M – 7.1 M cpu). **Break-even is 1 call on cpu and 0 on memory for every
operation** (asserted as `beCpu == Some(1L) && beMem == Some(0L)` at
`ValueBuiltinsBudgetTest.scala:180, :204, :225, :260`).

`CHANGELOG.md:64-71` states the same "~13-75x cheaper per operation" plus the behaviour change
(strict CIP-153 canonical-form validation; `unionValue`/`scaleValue` fail on 128-bit overflow).
`CHANGELOG.md:23` records `hasOnly` as *"~35% cheaper in fee"* than the `SortedMap` comparison it
replaced (that one is an `equalsData` win, not CIP-153).

Absolute pins that can be reused as budgets live in
`scalus-core/shared/src/test/scala/scalus/ledger/api/v1/ValueTest.scala` (~40 exact `ExUnits`
assertions under the default PV11 + `valueBuiltins=true` options), e.g.
`ValueTest.scala:884` `ExUnits(memory = 1413, steps = 990526)` for `* token by 2`, and
`ValueTest.scala:1274` `ExUnits(memory = 1689, steps = 1_175858)` for `insertCoin`.

---

## 3. `List` / `SortedMap` / `AssocMap` costs

### 3.1 Runtime representation

Under the default `SirToUplcV3Lowering`, `List[A]`'s runtime form is chosen per element type at
`typegens/SirTypeUplcGenerator.scala:355-365`:

| Condition on `A` | Emitter | Runtime form |
|---|---|---|
| `A` transitively contains a function type | `SumCaseUplcConstrOnlyEmitter` (`:365`) | UPLC `Constr(0,[h,t])` / `Constr(1,[])` |
| `elementReprFor(A)` is `Prod/SumUplcConstr` | `SumCaseUplcConstrEmitter` (`:359-361`) | UPLC `Constr` chain |
| `A` is a `BuiltinPair` | `SumPairBuiltinListEmitter` (`:362-363`) | builtin `list<pair<data,data>>` |
| otherwise (the normal case) | `SumBuiltinListEmitter(elemRepr)` (`:364`) | **builtin `list<elemRepr>`** |

So `Cons` → `mkCons`, `head`/`tail`/`isEmpty` → `headList`/`tailList`/`nullList`
(`typegens/SumListEmitterCommon.scala:150-157`, `:208-231`), and `match` on a list → a `Case` on the
builtin list at PV11, `ChooseList` below (`SumListEmitterCommon.scala:417`, `:564-583`).

`SortedMap` and `AssocMap` are both `@UplcRepr(PackedDataMap)`
(`prelude/SortedMap.scala:16-17`, `prelude/AssocMap.scala:10-11`): at runtime a single `Data` map
value. Construction is `mapData(...)` (`typegens/PackedDataMapEmitter.scala:93`), `.toList` is
`unMapData(...)` yielding a `SumPairBuiltinList` (`:106-116`), `match` is `unMapData(...)` (`:155-180`).
`PairList` has the same representation, so `SortedMap.toPairList` / `PairList.toList` are
**zero-cost relabels** (`prelude/PairList.scala:69-73`, `:191-195`; `prelude/SortedMap.scala:356-359`).

### 3.2 The dispatch cliff: Data-decoded lists get the smallest intrinsic set

`IntrinsicResolver.representationNames` (`.../lowering/IntrinsicResolver.scala:639-657`) maps a
representation to the provider names it may use:

| Representation | Provider names |
|---|---|
| `SumUplcConstr` | `["UplcConstrList"]` (`:644-645`) |
| `SumBuiltinList(er)` with `!er.isPackedData` | `["NativeBuiltinList", "BuiltinList"]` (`:646-647`) |
| `SumBuiltinList(er)` **packed** | `["BuiltinList"]` (`:648-649`) |
| `SumPairBuiltinList(_,_)` | `["PairList"]` (`:650-651`) |

`PackedData.isPackedData = true` and `ProdDataConstr.isPackedData = true`
(`LoweredValueRepresentation.scala:1595`, `:788`); `Constant.isPackedData = false` (`:1612`).

**Therefore a list that came out of the `ScriptContext` – `tx.inputs`, `tx.outputs`,
`tx.signatories` – has packed-Data elements and matches only `"BuiltinList"`.** That provider is
`BuiltinListOperations` (`compiler/intrinsics/ListIntrinsics.scala:241-287`) and its repr-rule table
`ListReprRules.listRules` is exactly five entries: `isEmpty, head, tail, drop, at`
(`ListIntrinsics.scala:90-94`). It has **no `map`, `filter`, `foldLeft`, `foldRight`, `find`,
`length`**. All of those run the plain prelude bodies compiled as UPLC recursion.

Native constant lists (built with `List.unboxedNil`, `prelude/List.scala:45`) match
`"NativeBuiltinList"` first and get the larger set
`isEmpty, head, tail, map, filter, foldLeft, foldRight, find, deleteFirst, distinct, diff`
(`ListIntrinsics.scala:146-159`). The `UplcConstrList` set is larger again (15 entries,
`ListIntrinsics.scala:212-234`) but implemented with `Constr`/`Case`, no list builtins at all
(`compiler/intrinsics/UplcConstrListOperations.scala:26-336`).

### 3.3 Builtin mapping and PV gating

| Intrinsic | Line | Builtins emitted | minPV |
|---|---|---|---:|
| `BuiltinListOperations.isEmpty` | `ListIntrinsics.scala:244-245` | `nullList` | 0 |
| `.head` | `:247-248` | `headList` | 0 |
| `.tail` | `:250-253` | `tailList` | 0 |
| `.contains` | `:257-258` | `equalsData` scan (`BuiltinListSupport.scala:28-36`) | 0 |
| `.indexOf` | `:260-261` | `equalsData` scan (`BuiltinListSupport.scala:38-46`) | 0 |
| `.deleteFirst` / `.distinct` / `.diff` | `:269-285` | `equalsData` scans | 0 |
| `BuiltinListOperationsV11.drop` | `:292-295` | **`dropList`** | **11** |
| `BuiltinListOperationsV11.at` | `:301-307` | **`dropList` + `nullList` + `headList`** | **11** |
| `BuiltinPairListOperationsV11.drop` | `:360-363` | **`dropList`** | **11** |
| `SortedMapIntrinsics.singleton` | `MapIntrinsics.scala:20-32` | `mapData(mkCons(mkPairData(…), mkNilPairData()))` | 0 |
| `SortedMapIntrinsics.empty` / `AssocMapIntrinsics.empty` | `MapIntrinsics.scala:35-36`, `:58-59` | `mapData(mkNilPairData())` | 0 |

`DropList` is CIP-158 and PV11-only (`scalus/uplc/DefaultFun.scala:1279-1281`; registry gate
`minPV = 11` at `IntrinsicResolver.scala:171`, `:181`). `HeadList`/`TailList`/`NullList`/`MkCons`/`ChooseList`
are core Plutus V1+. **There is no `IndexList` or `LengthOfList` builtin in this codebase** – only the
array analogues `LengthOfArray` (`DefaultFun.scala:1298`), `ListToArray` (`:1309`), `IndexArray`
(`:1322`), none of which the List intrinsics use.

**Only `empty` and `singleton` are intrinsified for maps** (`IntrinsicResolver.scala:183-200`,
`MapIntrinsics.scala:93-96`). Every other `SortedMap`/`AssocMap` operation runs prelude code.

### 3.4 `List` operation classification

(a) direct builtin · (b) intrinsic-replaced · (c) plain UPLC recursion · (d) allocates a new
list/tuple/Option. Line numbers are in `prelude/List.scala` unless noted.

| Method | Class | Intrinsic on | Tail-rec | Short-circuits | Allocates |
|---|---|---|---|---|---|
| `isEmpty` `:405` | a | all reprs | – | – | no |
| `head` `:1121` / `headOption` `:1136` | d | no | – | – | **1 Option** |
| `tail` `:1156` | a `tailList` | all | – | – | no |
| `at` `:457` / `!!` `:393` | b at PV11 (`dropList`) else c | **`BuiltinList` @PV11 only** | yes | yes | no |
| `get` `:487` / `isDefinedAt` `:437` | c+d | no | yes | yes | 1 Option |
| `drop` `:1178` | b at PV11 (`dropList`) | `UplcConstr`, `BuiltinList`@11 | yes | – | no |
| `length` `:1103` / `size` `:1106` | c `foldLeft` counter | **`UplcConstr` only** | yes | no | no |
| `count` `:1004` | c `foldLeft` | no | yes | no | **no** |
| `map` `:764` | prelude = **`foldRight`** | `UplcConstr`, `Native` | **no** | no | n `Cons` |
| `filter` `:805` | prelude = **`foldRight`** | `UplcConstr`, `Native` | **no** | no | k `Cons` |
| `filterMap` `:850` | `foldRight` | `UplcConstr` only | no | no | k `Cons` |
| `flatMap` `:786` | `foldRight` + `++` | no | no | no | **O(n·m)** `Cons` |
| `foldLeft` `:924` | b/c | `UplcConstr`, `Native` | **yes** | no | no |
| `foldRight` `:947` | b/c | `UplcConstr`, `Native` | no | no | no |
| `find` `:875` | b/d | `UplcConstr`, `Native` | yes | **yes** | 1 Option |
| `exists` `:967` | d = `find(p).isDefined` | via `find` | yes | **yes** | 1 Option |
| `forall` `:986` | c | no | **yes** | **yes** | no |
| `contains` `:518` | **b → `equalsData` scan** | all reprs | yes | **yes** | none when intrinsified |
| `indexOf` `:1027` | **b → `equalsData` scan** | all reprs | yes | **yes** | none when intrinsified |
| `takeWhile` `:1331` / `dropWhile` `:1228` | d / c | no | no | **yes** | k `Cons` / no |
| `takeRight` `:1305` / `dropRight` `:1202` / `init` `:1404` | d, `foldRight` over a **tuple accumulator** | `UplcConstr` | no | no | **n tuples** |
| `reverse` `:1419` | c/d | `UplcConstr` only | yes | no | n `Cons` |
| `appended` `:714` / `appendedAll` `:737` / `++` `:746` | d | `UplcConstr` | no | no | n `Cons` |
| `distinct` `:1354` | b/d, `foldLeft`+`exists`+`reverse` | all | outer yes | no | **2n `Cons`, O(n²) compares** |
| `diff` `:1382` | b/d, repeated `deleteFirst` | all | yes | yes | **O(n·m)** |
| `sort` `:341` / `quicksort` `:367` | b/d quicksort | `UplcConstr` only | no | – | O(n log n) `Cons` + tuples |
| `flatten` `:385` | d `foldRight` + `++` | no | no | no | O(total) `Cons` |
| `groupBy` `:536` / `groupMap` `:560` / `groupMapReduce` `:606` | d, `SortedMap.insert` per element | no | outer yes | no | **O(n·m)** – rebuilds the map spine and re-emits `mapData` on every element (`:572`, `:576`, `:618`, `:622`) |
| `zip` `:645` | d | no | no | yes | n `Cons` + n tuples |

**The most consequential line in the prelude:** `filter` is
`foldRight(List.empty[A])((h, t) => if predicate(h) then Cons(h, t) else t)`
(`prelude/List.scala:805-808`), and `foldRight` is not tail-recursive (`:947-949`). On the
Data-packed `BuiltinList` path – i.e. on `tx.inputs` / `tx.outputs` – there is no `filter`, `map` or
`foldRight` intrinsic, so `map`/`filter`/`filterMap` compile to a full non-tail UPLC recursion.

### 3.5 `SortedMap` / `AssocMap`

| Op | `SortedMap` | `AssocMap` | Cost shape |
|---|---|---|---|
| `get` | `:625` | `:111` | loop over the `unMapData` pair list + 1 Option. **SortedMap stops early on `Order.Less`** (`SortedMap.scala:633`); AssocMap always scans to hit-or-end |
| `at` / `getOrFail` | `:680` / `:659` | – | `get(...).getOrFail` → 1 Option |
| `contains` | `:696` | `:122` | `get(key).isDefined` → 1 Option |
| `insert` | `:711` | `:124` | rebuilds the spine: `PairCons` per prefix element, then `mapData` |
| `delete` | `:738` | `:137` | rebuilds the prefix |
| `union` | `:177` | `:148` | **SortedMap: one linear merge** (`:181-211`). **AssocMap: O(n·m)** – `rhs.get(k)` per left key (`AssocMap.scala:157`) plus `lhs.toList.exists` per right key (`:166`) |
| `toList` | field `:17` | field `:11` | one `unMapData` |
| `toPairList` | `:360` | `:66` | **free** (relabel) |
| `keys` / `values` | `:421` / `:436` | `:71` / `:74` | `foldRight` + `Cons`: n `Cons`, non-tail |
| `isEmpty` | `:372` → `PairList.isEmpty` `:88` | `:67` | `unMapData` + `nullList` – **O(1)** |
| `size` / `length` | `:396` / `:408` → `PairList.length` `:94` | `:69` / `:70` | **O(n)** despite being `inline` |
| `mapValues` | `:486` → `PairList.mapValues` `:103` | `:77` | `fstPair`/`sndPair` directly – **~4 ops/elem vs ~12** for `List.map` on tuples (`PairList.scala:98-102`) |
| `filter` / `filterKeys` / `filterNot` | `:501` `:517` `:532` | `:82` `:87` `:90` | rebuild + `mapData` |
| `find` / `findMap` | `:548` / `:573` | `:93` | short-circuits, 1 Option |
| `foldLeft` / `foldRight` | `:590` / `:606` | `:96` / `:99` | `foldLeft` tail-rec (`PairList.scala:120`), `foldRight` not (`:124`) |

`AssocMap` deliberately has **no `Eq`** – `assocMapNoEq` is a `compiletime.error`
(`prelude/AssocMap.scala:178-182`); the explicit `AssocMapEq.equals` (`:193`) is itself O(n·m).

### 3.6 Fusion: there is none

Verified by exhaustion, not by absence of evidence:

- The full UPLC pass list is `uplc/transform/OptimizerPipelines.scala:7-25` (V1V2) and `:27-61` (V3):
  `EtaReduce`, `Inliner`, `StrictIf`, `ForcedBuiltinsExtractor`, `CommonSubexpressionElimination`,
  `CommonContextExtraction`, `CaseConstrApply`. None rewrites a traversal.
- SIR-level passes are `RemoveRecursivity`, `MutualRecursionElimination`,
  `StaticArgumentTransformation`, `RemoveTraces` (plus two dead ones – see §5). None fuses.
- `IntrinsicResolver.tryResolveFull` extracts **one** `module.method` from the head of a single
  apply chain (`IntrinsicResolver.scala:225-260`) – a per-call substitution, never a rewrite over
  two composed calls.
- `CaseOfCase` / `CaseReduce` / `KnownCon` / `FloatDelay` / `ForceCaseDelay` are recorded as
  *confirmed absent, zero hits in any `.scala` file* (`docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:239-253`).
- Repo-wide grep for `fusion` / `deforest` / `fuse` in `scalus-core/shared/src/main/scala` and
  `scalus-plugin/src/main/scala` returns only unrelated substring hits.

**`xs.filter(p).length` therefore costs 2 full traversals and k `mkCons` allocations**: `filter`
walks all n (`List.scala:805-808`), materialises the survivor list, then `length` walks the k
survivors (`:1103` → `foldLeft` `:924`) – and `length` is itself a closure call per element, not a
builtin. Nothing removes the intermediate list. The single-pass replacement already exists:
`count` (`:1004`), a `foldLeft` that allocates nothing. `map(f).filter(g)` is likewise 2 traversals
and n + k `mkCons`; the library's hand-written answer is `filterMap` (`:850`).

CSE (`OptimizerPipelines.scala:48-50`) will share two *syntactically identical* `xs.filter(p)`
subterms – but only when `optimizeUplc` is on, and its default is **`false`**
(`SIRDefaultOptions.scala:16`).

### 3.7 `Option` allocates

`Option` carries no `@UplcRepr` (`prelude/Option.scala:15-18`), so it falls through the structural
branch at `SirTypeUplcGenerator.scala:369-370`:

| Context | Representation | `Some(x)` at runtime |
|---|---|---|
| default | `DataConstr` (`typegens/DataConstrEmitter.scala:34-46`) | `constrData(0, mkCons(x, mkNilData()))`; `None` = `constrData(1, mkNilData())`; destructuring = `unConstrData` (`:504`) |
| `A` contains a function | `SumUplcConstr` (`SirTypeUplcGenerator.scala:370`) | `Constr(0,[x])` / `Constr(1,[])` |
| inside a UplcConstr-list intrinsic | `SumUplcConstr`, forced by `lctx.inUplcConstrListScope` (`IntrinsicResolver.scala:329-338`, `:493-502`) | native `Constr` |

`OptionIntrinsics.scala` holds **only repr rules** (`isDefined, isEmpty, nonEmpty, get, getOrElse`,
`:55-63`), and the registry entry is keyed on `UplcConstrListRepr`
(`IntrinsicResolver.scala:173-178`). **A `DataConstr` Option – the default – gets no intrinsic and
runs the prelude bodies** (`prelude/Option.scala:123-394`).

**Does the allocation survive optimization? Yes.** No pass reduces `Case`/`unConstrData` applied to a
literal `Constr`: `CaseConstrApply` only *creates* `Case(Constr(...))`
(`uplc/transform/CaseConstrApply.scala:28-32`); `Inliner.go` handles identity, dead code, beta,
force/delay and partial evaluation only (`uplc/transform/Inliner.scala:203-212`); `PartialEvaluator`
folds only **closed** terms (`uplc/transform/PartialEvaluator.scala:41`), and an `Option` built from
a runtime value is not closed.

The only place the allocation genuinely disappears is where an intrinsic replaces the whole chain:
`List.contains` and `List.indexOf` become `Boolean`/`BigInt` scans
(`ListIntrinsics.scala:257-261`, `IntrinsicsNativeList.scala:68-79`) instead of the prelude's
`find(...).isDefined` / `indexOfOption(...).getOrElse(-1)` (`List.scala:518`, `:1027`) – and the
trailing implicit `Eq` argument is dropped too, so no `Eq` closure is built
(`IntrinsicResolver.scala:36-50`, `:257-260`).

Everything else that returns an `Option` allocates: `head`/`headOption` (`List.scala:1121`, `:1136`),
`get` (`:487`), `lastOption` (`:1087`), `find` (`:875`), `exists` (`:967`),
`SortedMap.get`/`at`/`contains` (`SortedMap.scala:625`, `:680`, `:696`),
`AssocMap.get`/`contains` (`AssocMap.scala:111`, `:122`).

### 3.8 In-repo cost warnings worth honouring

| Location | Note |
|---|---|
| `prelude/PairList.scala:8-18` | `PairList` "stays in the UPLC BuiltinPair representation, avoiding costly per-element conversions"; `toList`/`toPairList` are "zero-cost (no UPLC code generated)" |
| `prelude/PairList.scala:98-102` | `mapValues` uses `fstPair`/`sndPair` directly – "**~4 ops/element vs ~12**" |
| `prelude/SortedMap.scala:472-475` | `mapValues` "~3x cheaper than `List.map` on tuples" |
| `prelude/List.scala:321-331` | `sort`: single-pass partition + accumulator concat "halves the per-recursion traversal and avoids the `O(n)` `++` at every join"; a Data-path `sort` intrinsic is still a follow-up |
| `prelude/List.scala:341-344` | **open lowering bug**: a `Tuple2`-returning partition triggers a runtime `Case index 2 out of bounds for 1 branches`; the current shape must be kept until it is found |
| `compiler/intrinsics/ListIntrinsics.scala:296-300` | PV11 correctness guard: bare `headList(dropList(i, xs))` succeeds for negative `i` – a JVM/UPLC semantics divergence |
| `compiler/intrinsics/BuiltinListSupport.scala:9-24` | Data-repr elements compare with "just `equalsData`", so there is "no per-type code and no `eq` function to thread through"; the support is lowered **once** with `A` abstract |
| `compiler/intrinsics/UplcConstrListOperations.scala:192-198` | known divergence: `deleteFirst` on the UplcConstr path leaves elements in `DataData` repr, breaking the generic `listEq`; tracked as a follow-up |
| `uplc/transform/CaseConstrApply.scala:8-17` | cost note: `Apply` is 100 mem / 16 000 cpu, so `case(constr 0 [a,b,c]) f` (200/32 000) beats three nested applies (300/48 000) |
| `typegens/SirTypeUplcGenerator.scala:285-292` | **FIXME(X2)**: `SumDataList` hardcodes `PackedData` elements, blocking on-chain `bls12_381_G{1,2}_multiScalarMul` |

---

## 4. Existing measurements

**Checkout note.** `docs/internal/` is byte-identical between the two checkouts. The primary checkout
(`/Users/nau/projects/lantr/scalus`) has two extra files not on `origin/master`:
**`docs/internal/LIST_CONTAINS_IMPL_COST.md` (primary)** and
**`docs/internal/UPLC_CORRECTNESS_AUDIT.md` (primary)**. `scalus-core/jvm/src/test/scala/scalus/bench/`
also exists **only in the primary checkout** and contains exactly one file,
**`ContainsImplBenchTest.scala` (primary)**. Everything else cited here is present in the worktree.

**`UPLC_CORRECTNESS_AUDIT.md` (primary) contains no budget numbers at all** – it is a correctness
audit (pattern-match compilation, flat encoding tags, `Eq` soundness, newtype lowering). Its only
cost-adjacent statement is qualitative (`:405`, the lenient-else strictness gap is WONTFIX for Aiken
parity). Nothing to reuse as a budget.

### 4.0 Reality check: ExUnits are a minority of the fee

`docs/internal/articles/MPF/authorized_collections.md:395-403` decomposes a real MPF-16 withdraw
transaction, total fee 287 027 lovelace:

| Category | lovelace | share |
|---|---:|---:|
| base fee (`min_fee_b`) | 155 381 | **54.1 %** |
| tx size (1 132 B × 44) | 49 808 | 17.4 % |
| reference script | ~49 251 | 17.2 % |
| memory (415 719 × 0.0577) | 23 987 | 8.4 % |
| CPU (119.3 M × 0.0000721) | 8 600 | **3.0 %** |

**Execution units are ~11.4 % of a real transaction fee, and CPU alone is 3 %.** The same doc (`:595`)
notes that a 7-8 % ExUnits saving becomes < 1 % of the real fee, and even a 2x UPLC difference
shrinks to 10-15 % of total fee. Design implication: **script size and reference-script bytes are
worth as much as execution units** – a stdlib that adds 1 KB of code to save 10 % CPU is a net loss.

### 4.1 List: `find(...).isDefined` vs direct recursion – the Option tax, measured

`docs/internal/LIST_CONTAINS_IMPL_COST.md` **(primary)**, measured 2026-08-25, harness
`scalus-core/jvm/src/test/scala/scalus/bench/ContainsImplBenchTest.scala` **(primary)**, compiled with
`Options.releaseUntagged` (**PV11, traces OFF, UPLC optimizer ON**).

Per-call saving of direct recursion over `find(...).isDefined` (`LIST_CONTAINS_IMPL_COST.md:33-40`):

| backend | outcome | Δcpu | Δmem | Δlovelace | Δscript bytes |
|---|---|---:|---:|---:|---:|
| **V3 (default)** | miss | 326 483 | 1 064 | **84.93** | 29 |
| **V3 (default)** | hit | 564 996 | 2 028 | **157.75** | 29 |
| Scott | miss / hit | 128 000 / 144 000 | 800 / 900 | 55.39 / 62.31 | 14 |
| SoP | miss / hit | 64 000 / 96 000 | 400 / 600 | 27.69 / 41.54 | 9-10 |

Absolutes on `List[Data]`, V3 (`:51-62`) – `List[Data]` keeps the on-chain representation, so these
are the `contains` cost itself with no `FromData` inflation:

| len | case | `find` cpu / mem / lovelace | direct rec | Δ |
|---:|---|---|---|---:|
| 0 | miss | 800 516 / 3 996 / 288.29 | 474 033 / 2 932 / 203.35 | −29.5 % |
| 1 | hit | 2 201 572 / 5 761 / 491.14 | 1 636 576 / 3 733 / 333.39 | **−32.1 %** |
| 1 | miss | 2 091 059 / 5 597 / 473.71 | 1 764 576 / 4 533 / 388.78 | −17.9 % |
| 5 | hit@tail | 7 363 744 / 12 165 / 1 232.85 | 6 798 748 / 10 137 / 1 075.09 | −12.8 % |
| 20 | miss | 26 611 376 / 36 016 / 3 996.80 | 26 284 893 / 34 952 / 3 911.87 | −2.1 % |

**Derived constant: `equalsData` costs ~1 034 543 cpu per list element** (`:64`). That is why the
Option tax fades from 32 % at length 1 to 2 % at length 20 – it is a *fixed* per-call cost, not a
per-element one.

Findings (`:88-103`): on V3, `List.contains` already goes through `IntrinsicResolver` and is
**byte-identical to the hand-written direct recursion** (asserted at
`ContainsImplBenchTest.scala:213-224` **(primary)**). **`List.exists` is NOT intrinsic** and measures
exactly equal to the `find` form in every configuration – so **every `exists` call on V3 pays the
full 326 K (miss) / 565 K (hit) cpu Option tax**. The same shape appears in `List.isDefinedAt`,
`SortedMap.contains` and `AssocMap.contains`.

> Note: `LIST_CONTAINS_IMPL_COST.md:43` labels the prices "mainnet epoch 544". The price values match
> epoch 645; the epoch label is stale.

### 4.2 Per-op prelude budget pins (the largest reusable corpus)

**Options context for §4.2-§4.4:** `EvalTestKit.compilerOptions` =
`SirToUplcV3Lowering`, **`generateErrorTraces = true`**, `optimizeUplc = true`, PV11, VM
`PlutusVM.makePlutusV3VM()` on `CardanoInfo.mainnet` epoch-645 costs
(`scalus-core/shared/src/test/scala/scalus/testing/kit/EvalTestKit.scala:39-58`). **Traces are ON, so
these are upper bounds relative to `Options.release`.** `assertEvalWithBudgets(f, arg, …)` passes the
argument as `Data` at runtime (pin ≈ op cost); `assertEvalWithBudget(expr, …)` compiles the whole
expression including data construction (much larger).

Pin counts by file:

| File | `ExUnits(memory` pins |
|---|---:|
| `scalus-core/shared/src/test/scala/scalus/prelude/ListTest.scala` | **220** |
| `scalus-core/shared/src/test/scala/scalus/prelude/SortedMapTest.scala` | 71 |
| `scalus-core/shared/src/test/scala/scalus/ledger/api/v1/ValueTest.scala` | 69 |
| `scalus-core/shared/src/test/scala/scalus/prelude/MathTest.scala` | 47 |
| `scalus-core/shared/src/test/scala/scalus/builtin/ByteStringTest.scala` | 6 |
| `scalus-core/shared/src/test/scala/scalus/prelude/PairListTest.scala` | 4 |

**No test asserts a prelude-op budget via `ExBudget`** – `ExBudget` appears in exactly one test
(`scalus-examples/jvm/src/test/scala/scalus/examples/setbench/MpfCostAnalysisTest.scala`, only as
`ExBudgetCategory` for a print-only breakdown) plus implementation files. All real pins use `ExUnits`.

`ListTest.scala`, element type `BigInt`, lengths 0 / 1 / 2 (mem / cpu):

| Op | len 0 | len 1 | len 2 | lines |
|---|---|---|---|---|
| `map(_ + 1)` | 2 664 / 451 966 | 5 826 / 1 201 364 | 8 988 / 1 950 762 | `:1448, :1457, :1466` |
| `filter` | 2 664 / 451 966 | 5 561 / 1 025 785 | 8 990 / 1 751 966 | `:1548, :1557, :1566` |
| `filterMap` | 3 564 / 595 966 | 7 925 / 1 560 268 | 14 780 / 3 401 094 | `:1622, :1631, :1640` |
| `find` | 2 432 / 394 033 | 4 565 / 796 067 | 6 862 / 1 308 614 | `:1659, :1668, :1677` |
| `foldLeft` | 3 064 / 510 777 | 6 262 / 1 164 772 | 9 460 / 1 818 767 | `:1731, :1740, :1749` |
| `foldRight` | 2 864 / 478 777 | 5 862 / 1 100 772 | 8 860 / 1 722 767 | `:1767, :1776, :1785` |
| `length` / `size` | 3 064 / 510 777 | 6 030 / 1 112 028 | 8 996 / 1 713 279 | `:2155, :2164, :2173` |
| `count(_ > 0)` | 2 864 / 478 777 | 6 663 / 1 274 062 | 10 060 / 1 904 139 | `:1835, :1846, :1868` |
| `head` (on `Nil`) | **200 / 16 100** | 4 256 / 1 156 915 | – | `:2128, :2137` |
| `tail` (on `Nil`) | **200 / 16 100** | 1 132 / 186 033 | – | `:2227, :2235` |
| `headOption` | 932 / 154 033 | 2 096 / 424 546 | 2 096 / 424 546 | `:2091, :2100, :2109` |
| `prepended` / `+:` | 1 264 / 274 395 (flat, all lengths) | | | `:920-974` |
| `appended` / `:+` | 3 264 / 594 395 | 4 696 / 890 757 | 6 128 / 1 187 119 | `:1120, :1129, :1138` |
| `flatMap` | 3 464 / 579 966 | 8 658 / 1 723 797 | 15 784 / 3 243 990 | `:1486, :1497, :1508` |
| `flatten` | 2 864 / 483 966 | 7 392 / 1 348 117 | 15 484 / 2 916 992 | `:447, :456, :465` |
| `quicksort` | 3 732 / 602 033 | 10 952 / 2 290 652 | **43 371 / 9 530 315** | `:404, :413, :422` |
| `indexOf` | 4 528 / 901 260 | 7 289 / 2 686 757 | 9 492 / 4 174 508 | `:1966, :1977, :1999` |
| `deleteFirst` | 2 632 / 426 033 | 3 433 / 1 588 576 | 6 898 / 3 311 843 | `:2475, :2486, :2508` |
| `drop 1` | 3 033 / 533 870 | 4 139 / 927 789 | 4 139 / 927 789 | `:2256, :2267, :2278` |
| `dropRight 1` | 8 421 / 1 841 559 | 14 508 / 3 619 925 | 21 057 / 5 540 337 | `:2332, :2343, :2354` |
| `at` / `!!` | 1 664 / 276 143 | 2 860 / 630 470 | 1 664 / 276 143 | `:626, :635, :642`; `:663, :672, :679` |
| `zip` | 2 832 / 458 033 | 6 160 / 1 209 270 | 9 488 / 1 960 507 | `:847, :880, :891` |
| `===` on lists | 901 / 1 135 364 | 901 / 1 271 759 | 901 / 1 544 549 | `:308, :317, :326` |
| `ToData` ↔ `FromData` | **432 / 74 033** at every length | | | `:270, :279, :288` |
| **`groupBy`** | 7 796 / 1 334 902 | 28 761 / 6 220 745 | **56 784 / 12 701 110** | `:720, :729, :742` |
| **`groupMap`** | 7 996 / 1 366 902 | 30 025 / 6 448 788 | **59 112 / 13 125 196** | `:765, :774, :787` |
| **`groupMapReduce`** | 5 432 / 874 033 | 16 745 / 3 567 030 | **92 370 / 21 532 235** | `:809, :818, :828` |

Reusable design signals from this table:

- `prepended` is **flat** and 2.2x cheaper than `appended`.
- `dropRight` is ~4x `drop`.
- `groupBy`/`groupMap`/`groupMapReduce` blow up superlinearly (2 elements already costs 12-21 M cpu).
- `head`/`tail` on `Nil` = 200 mem / 16 100 cpu = **exactly one machine step** – the cheapest
  observable prelude call, a useful floor for "what a builtin-backed accessor costs".
- `ToData`/`FromData` is **length-independent at 432 / 74 033** – direct confirmation of §1.6.

### 4.3 `SortedMap`

`SortedMapTest.scala` runtime-arg pins, `SortedMap[BigInt, BigInt]`:

| Op | empty | 1 entry | lines |
|---|---|---|---|
| `get(k)` hit | 1 832 / 296 723 | **5 994 / 1 309 043** | `:1398, :1407` |
| `get(k)` miss | – | 4 429 / 929 240 | `:1416` |
| `length` / `size` | 2 132 / 344 723 | 3 534 / 669 931 | `:774, :783` / `:814, :823` |
| `keys` | – | 12 176 / 2 891 880 | `:861` |
| `values` | – | 12 408 / 3 005 543 | `:899` |
| `filter` | 3 064 / 556 969 | 7 225 / 1 563 239 | `:1064, :1073` |
| `find` | 2 832 / 456 723 | 7 357 / 1 562 338 (hit) · 6 861 / 1 381 674 (miss) | `:1178, :1187, :1196` |
| `foldLeft` | 4 564 / 749 467 | 14 476 / 3 527 562 | `:1313, :1323` |
| `insert` | 2 464 / 460 969 | – | `:1612` |
| `singleton` | 432 / 72 723 | 1 928 / 435 437 | `:46, :84` |
| `ToData` ↔ `FromData` | 500 / 64 100 | – | `:639, :648` |

Whole-expression pins (include map construction, 3 entries): `get(2)` = **41 302 / 10 105 379**
(`:1430`); `get(4)` miss = 46 468 / 11 125 417 (`:1443`); `fromList` = 43 351 / 10 853 342 (`:138`);
`union` = 54 386 / 14 318 018 (`:256`).

**`SORTEDMAP_LOOKUP_COST_FINDINGS.md`** (2026-08-19). ⚠ **`:3` marks the change REVERTED** – the
"new Eq get" rows describe code that is *not* in the tree. Only the `Ord` baseline is live, and it is
cross-validated: the `1 309 043 / 5 994` row matches the shipped pin at `SortedMapTest.scala:1407`
exactly.

Singleton `BigInt`-keyed map, hit, whole-program (`:60-64`):

| implementation | cpu | mem |
|---|---:|---:|
| current `Ord` get (`unIData` + 2× `lessThan` + `Order`) | **1 309 043** | 5 994 |
| `Eq` get via `equalsData` on packed keys (reverted) | 1 761 779 | 3 997 |
| **ideal: concrete-typed clone using `equalsInteger`** | **832 313** | 4 229 |

Value-lookup strategy comparison, n policies, fee in lovelace (`:25-31`): per-visited-entry fee is
**A (`SortedMap.get` w/ `Ord`) ≈ 354, C (ideal early-exit scan) ≈ 283, B (`hasOnly` plain
`equalsByteString` scan) ≈ 220 lovelace**. Conclusions (`:33-43`): sortedness never helps on a
present key; early exit only pays on misses sorting within ~the first 60 % of the map; B's memory is
2-4x lower than A's, and memory dominates the fee.

5-entry `ByteString`-keyed map, Eq vs Ord (reverted branch, `:71-76`): hit-first fee 443 vs 459;
hit-last 1 319 vs 1 632; **absent-early 1 302 vs 343 (~8x worse)**; absent-late 1 302 vs 1 615.
Whole-validator effect (`:78-79`): memory −4..6 %, cpu +1.5..3.5 %.

Root cause (`:53-57`): **generic `===` inside polymorphic code lowers to `equalsData` on packed
`Data` keys, not `equalsInteger`/`equalsByteString`.**

### 4.4 `Value`

`ValueTest.scala` (PV11 default, `valueBuiltins` active, traces ON):

| Op | Value shape | mem | cpu | line |
|---|---|---:|---:|---|
| `quantityOf` (ada in `zero`) | zero | 1 213 | 492 985 | `:1125` |
| `quantityOf` (matching token) | 1 policy × 1 token | 1 257 | 895 629 | `:1170` |
| `getLovelace` zero / lovelace / token | – | 1 213 / 1 257 / 1 257 | 492 985 / 895 629 / 895 629 | `:987, :996, :1005` |
| **`lovelaceAmount`** | lovelace-only, and lovelace+asset | **200** | **16 100** | `:1022, :1040` |
| `negate` | token / lovelace | 1 413 | 990 526 | `:445, :454` |
| `+` token+token | | 1 446 | 1 068 221 | `:493` |
| `*` by 2 / by 0 | | 1 413 / 1 391 | 990 526 / 952 367 | `:884, :893` |
| `insertCoin` | | 1 689 | 1 175 858 | `:1274` |
| `hasOnly` | 3 policies × 1 token | 8 687 | 3 620 482 | `:1367` |
| `withoutLovelace` zero → multi-asset | | 1 559 → 1 732 | 678 958 → 1 539 900 | `:1401-1436` |
| **`flatten`** zero / lovelace / token / multi | | 5 764 / 26 680 / 26 680 / **47 596** | 946 656 / 6 562 443 / 6 562 443 / **12 178 230** | `:1454, :1463, :1478, :1502` |
| `toSortedMap` | | 500 | 64 100 | `:52, :64` |
| `-` multi-asset partial cancel | | 56 569 | 15 524 929 | `:816, :838` |

**`===` vs `toData` comparison cost identically and scale with tree size** (`:1619-1799`):

| case | mem | cpu | lines |
|---|---:|---:|---|
| lovelace equal (via `Eq` and via `toData` – identical) | 901 | 1 653 665 | `:1619, :1628` |
| single token equal | 901 | 1 735 502 | `:1646, :1664` |
| single token **not** equal | 1 101 | 1 767 502 | `:1817, :1835` |
| two policies equal | 104 168 | 27 814 323 | `:1690, :1718` |
| three policies equal | 176 653 | 47 288 732 | `:1799` |

This is the strongest single confirmation of §1.6 / I7: **`===` and `toData`-comparison produce the
same term**, and both are whole-tree `equalsData` whose cost is proportional to the Data tree size.
Compare `flatten` on a multi-asset value (12.2 M cpu) with `quantityOf` on the same shape (0.9 M):
**~13x**, consistent with the T7 ratio table in §2.5.

### 4.5 Whole-validator budgets (reusable "what a real script costs")

All from `scalus-examples/jvm/src/test/scala/scalus/`, exact-equality pins, PV11 default:

| Validator | mem | cpu | file:line |
|---|---:|---:|---|
| PubKeyValidator | 10 345 | 2 855 689 | `examples/PubKeyValidatorTest.scala:61` |
| HelloCardano | 16 286 | 6 028 388 | `examples/HelloCardanoTest.scala:36` |
| **HTLC** reveal-preimage | 29 355 | 12 138 878 | `examples/htlc/HtlcTest.scala:87, :107` – exUnit fee **Coin(2 569)** at `:88` |
| HTLC timeout (tx `totalExUnits`) | 25 922 | 9 249 707 | `examples/htlc/HtlcTest.scala:188` |
| Pricebet join / win / timeout | 81 568 / 79 915 / 41 039 | 32 653 670 / 28 106 800 / 14 268 074 | `examples/pricebet/PricebetValidatorTest.scala:59, :135, :240` |
| Vault (4 paths) | 96 841 – 102 413 | 34 363 973 – 38 755 014 | `examples/vault/VaultTransactionTest.scala:153, :250, :406, :453` |
| Escrow (3 paths) | 116 600 – 134 154 | 41 533 374 – 48 867 522 | `examples/escrow/EscrowTest.scala:112, :217, :302` |
| Vesting | 154 500 / 185 197 | 57 255 916 / 74 033 910 | `examples/vesting/VestingTransactionTest.scala:142, :175` |
| AMM | 147 306 / 135 327 | 60 533 898 / 56 614 863 | `examples/amm/AmmTest.scala:173-174, :194-195` |
| **PaymentSplitter Optimized**, 5 payees | 265 511 / 260 595 (pre38/since38) | 109 405 371 / 107 662 822 | `examples/paymentsplitter/OptimizedPaymentSplitterValidatorTest.scala:53, :81` |
| **PaymentSplitter Naive**, 5 payees | 630 020 / 613 944 | 232 744 548 / 227 013 877 | `examples/paymentsplitter/NaivePaymentSplitterValidatorTest.scala:54, :82` |

**Naive vs Optimized PaymentSplitter is the cleanest in-repo "same semantics, 2.1x budget" data
point.** Script sizes: HTLC = **366 B** (`examples/htlc/HtlcTest.scala:64`), HelloCardano = **316 B**
(`examples/HelloCardanoTest.scala:23`).

A typical single-purpose validator therefore lands at **10 K – 200 K memory units** against a
16 500 000 limit – i.e. **1-2 % of the transaction memory budget**, leaving generous room but not for
a stdlib that multiplies traversals.

### 4.6 Data-access micro-costs and script-size primitives

`scalus-core/jvm/src/test/scala/scalus/uplc/eval/ExprSizeAndBudgetTest.scala`. These tests build raw
builtin expressions, so the backend choice does not distort them; but note the file-level
`given Options = Options(ScottEncodingLowering)` at `:31-32`.

| What | mem | cpu | fee (lovelace) | line |
|---|---:|---:|---:|---|
| 2nd `ByteString` from a `list data` (`unListData`+`tailList`+`headList`+`unBData`) | 1 628 | 434 988 | 126 | `:123-125` |
| 2nd `ByteString` from a **packed** `ByteString` (`sliceByteString`) | 1 336 | 232 710 | **94** | `:143-145` |
| 5th `ByteString` from a `list data` | 2 624 | 823 977 | 211 | `:163-165` |
| 5th `ByteString` from a packed `ByteString` | 1 336 | 232 710 | **94** | `:183-185` |
| 2nd Int from a `list data` | 1 628 | 435 590 | 126 | `:200-202` |
| 2nd Int from a 64-bit-packed-int `ByteString` | 1 737 | 1 346 626 | 198 | `:218-220` |

**List traversal costs ~390 K cpu per extra element hop; packed-`ByteString` slicing is flat.**

Flat bit sizes (same file): `unit` = 10 bits (`:35`), `bool` = 11 (`:39`), `BigInt(123)` = 26 (`:43`),
`Var` = 12 (`:47`), 1-arg lambda = 16 (`:51`), `let` = 8 (`:56`),
`List.single(true)` = 63 bits (`:65`, `:73`, `:79`).

Optimizer effect, pinned: `scalus-core/jvm/src/test/scala/scalus/uplc/transform/CaseConstrApplyTest.scala:51-52`
– 1 100 mem / 160 100 cpu → 1 000 / 144 100, i.e. exactly one 16 000-cpu machine step removed.

### 4.7 Codegen comparison numbers (context, point-in-time)

`docs/internal/CODEGEN_IMPROVEMENT_PLAN.md`. ⚠ **§2 tables are frozen at master `f05f9973e`,
2026-08-03, before T1 (SAT) and T7 (Value builtins) landed.** Source project Binocular, Aiken v1.1.23,
both sides trace-free, mainnet params.

Script sizes, CBOR with parameters applied (`:36-39`):

| Script | Scalus @PV11 | Scalus @PV10 | Aiken v1.1.23 |
|---|---:|---:|---:|
| Oracle | **7 387 B** | 8 749 B | 9 497 B |
| TM | **3 700 B** | 4 363 B | 6 251 B |

Real tx fee, oracle update (`:43-45`): Scalus @PV11 **829 441** vs Scalus @PV10 930 660 vs Aiken
929 489 – a dead heat at PV10 parity; the PV11 lowering wins 12.1 %.

Root causes (`:62-85`): on the TM mint path builtin CPU is identical (27.6 M vs 27.7 M) while Aiken
uses **15 % fewer machine steps** (26.8 M vs 31.5 M). On the oracle SetState path Aiken executes ~2x
the Data traversal (**137 vs 75 `tailList`, 76 vs 40 `unConstrData`**). PV11 `case`/`dropList` is
worth **~15 % cpu and ~1.4 KB** on the oracle. On the TM GC path Scalus executed **2× `equalsData`
(2.5 M cpu)**, 29-vs-19 `ifThenElse`, 18-vs-10 `equalsInteger` against Aiken's direct walks. Aiken's
single-use let sinking cut a failure path by **−61 % cpu** (`:401-404`).

T1 corpus effects (`:293-302`): 96 000 cpu / 600 mem per recursive call for 2 lifted args; Knights
8×8 (Data) −10.4 % mem / −3.7 % cpu; G2Accumulator −13.7 % / −10.3 %; **prelude `List` operations
commonly −16 % to −29 %**. Mutual-recursion lifting measured **zero** win on today's corpus (`:314-319`).

Plutus reference figures (`:580-593`): SOP encoding gains 11-27 % on nofib; Plutus' constant folding
+ builtin hoisting + inliner tuning gave "> 10 % average execution-cost savings"; **Data
representation is documented ~3x slower than Scott for compute-heavy manipulation** – the trade Scalus
takes in exchange for free `fromData`.

### 4.8 JMH benchmarks under `bench/`

| File (`bench/src/main/scala/`) | Measures |
|---|---|
| `scalus/uplc/eval/CekJVMBenchmark.scala` | CEK **wall-clock** (µs/op) on pre-deserialized `.flat` UPLC over `auction_1-{1..4}` |
| `scalus/uplc/eval/JITBenchmark.scala` | three JIT strategies on the same scripts |
| `scalus/cardano/ledger/PlutusScriptEvaluatorBenchmark.scala` | real mainnet blocks – TxInfo construction + evaluation overhead |
| `scalus/crypto/accumulator/PolyBenchmark.scala` | off-chain `Poly.product` at N = 1 K … 32 K |

**`bench/last-bench-result.txt` is wall-clock µs/op, not ExUnits, and is stale (2025-12-10).** For the
record: CekJVM 227-934 µs/op vs JIT_Hybrid 37-197 µs/op on the four auction scripts (lines 2-17), i.e.
JIT is ~5-6x faster than the interpreter. **None of this is a UPLC budget** and JMH never runs in CI
(`CODEGEN_IMPROVEMENT_PLAN.md:146-153`).

### 4.9 Staleness flags – read before reusing any number

1. **`SORTEDMAP_LOOKUP_COST_FINDINGS.md`: the "new Eq get" rows are REVERTED code** (`:3`, `:81-89`).
   Only the `Ord` baseline is live (cross-validated against `SortedMapTest.scala:1407`).
2. **`CODEGEN_IMPROVEMENT_PLAN.md` §2 is frozen at 2026-08-03**, before T1 and T7 landed.
3. **`docs/internal/EXAMPLES_REVIEW.md` (2026-07-08) budgets are historical and contradicted by live
   pins.** E.g. `:168-169` gives HTLC timeout `246934/83506481`; the current pin is
   `25 922 / 9 249 707` (`HtlcTest.scala:188`). **Rule: test pins are current truth, doc prose is
   historical.**
4. **`UPLC_CORRECTNESS_AUDIT.md` (primary) has no budget numbers.**
5. **`bench/last-bench-result.txt` is 2025-12-10 wall-clock, not ExUnits.**
6. **`authorized_collections.md:496, :585` mis-state the tx limits** ("14B steps, 10M mem"); the real
   epoch-645 values are 10 000 000 000 steps / 16 500 000 mem. The measured tables in that doc are
   unaffected.
7. **EvalTestKit pins (§4.2-§4.4) run with `generateErrorTraces = true`** – upper bounds relative to
   `Options.release`. The trace-free measurements are `ContainsImplBenchTest` **(primary)** and
   `ExprSizeAndBudgetTest`.
8. `KnightsTest` uses a **5 % tolerance** (`assertBudgetClose`, `KnightsTest.scala:43-54`) because CSE
   tie-breaking depends on scalac symbol IDs. Its `refBudget` provenance is undocumented, and
   `compareBudgetWithReferenceValue`
   (`scalus-testkit/shared/src/main/scala/scalus/testing/kit/ScalusTest.scala:267-289`) **only prints,
   never asserts.**
9. Dual baselines `ScalaCompilerVersion.baseline(pre38, since38)` = Scala 3.3.7 vs 3.8.4 codegen; the
   compiler generation alone moves budgets by 1-13 %.
10. `LIST_CONTAINS_IMPL_COST.md:43` labels the prices "epoch 544"; the values are epoch-645 values.

---

## 5. What the SIR/UPLC optimizer does

### 5.1 The ordered pass list

**Always, at scalac link time (not configurable):**

| # | Pass | Cite |
|---|---|---|
| 1 | every linked top-level def is emitted as a `Let` with `LetFlags.Recursivity` | `compiler/sir/linking/SIRLinker.scala:385` |
| 2 | `RemoveRecursivity` – strips `isRec` from single-binding lets whose rhs does not reference itself | `SIRLinker.scala:480`; impl `compiler/sir/RemoveRecursivity.scala:19-30` |

**Every `UplcPipeline.run`** (`.../lowering/UplcPipeline.scala:38-82`):

| # | Pass | Gate | Cite |
|---|---|---|---|
| 1 | `RemoveTraces` | `options.removeTraces` (default **false**) | `UplcPipeline.scala:39` |
| 2 | `MutualRecursionElimination` | **unconditional** | `:40` |
| 3 | `StaticArgumentTransformation` (SAT) | `options.optimizeUplc` | `:41-42` |
| 4 | backend lowering (+ **unconditional** `IntrinsicResolver` substitution, `IntrinsicResolver.scala:166-204`) | – | `:43-69` |
| 5 | UPLC optimizer | `uplcOptimizers` (replaces) else `optimizeUplc` | `:70-74` |
| 6 | source-position back-fill (no cost effect) | – | `:75-81` |

**`optimizeUplc = true`, PlutusV3 → `V3Optimizer`** (`uplc/transform/OptimizerPipelines.scala:27-61`):

```
Phase 1 (:41-45):  EtaReduce → Inliner  ×3  →  StrictIf  →  ForcedBuiltinsExtractor
Phase 2 (:48-50):  (CSE → Inliner) × cseIterations           [default 2]
Phase 3 (:53-55):  CCE → Inliner                             [cceEnabled, default FALSE]
Phase 4 (:58):     CaseConstrApply
```

Defaults `cseIterations = 2`, `cceEnabled = false` (`compiler/compiler.scala:16-17`).
V1/V2 get Phase 1 only, no CSE/CCE/CaseConstrApply (`OptimizerPipelines.scala:7-25`), selected by
language at `UplcPipeline.scala:88-91`.

**The default is NO optimization.** `SIRDefaultOptions.optimizeUplc = false`
(`SIRDefaultOptions.scala:16`); `Options.debug` explicitly sets it false (`compiler.scala:56-61`);
only `Options.release` / `releaseUntagged` set it true (`compiler.scala:63-76`). There is no
library-wide `given Options` – `PlutusV3.compile[A](code)(using opts: Options)`
(`scalus-core/shared/src/main/scala/scalus/uplc/Compiled.scala:425`) makes the user supply one.
Under the default, none of the V3Optimizer phases and no SAT runs; link-time passes,
`MutualRecursionElimination`, lowering and `IntrinsicResolver` still do.

**Passes that are NOT in any pipeline** (do not design around them):
`LetFloating` – only reachable from the legacy `simple` backends
(`.../lowering/simple/BaseSimpleLowering.scala:50-53`); the default backend calls
`Lowering.lowerSIR(MutualRecursionElimination(sir))` with no let-floating
(`SirToUplcV3Lowering.scala:28`).
`BooleanOptimizer` (`compiler/sir/BooleanOptimizer.scala:10`) and `AbbreviateErrorTraces`
(`compiler/sir/AbbreviateErrorTraces.scala:25`) are **dead** – referenced only from their own tests
/ spec. `OptimizingSirToUplcLowering` **no longer exists** (only historic `CHANGELOG.md:982, 1045, 1054`
mentions).

### 5.2 Per pass: what it recognises, what defeats it

| Pass | Recognises | Defeated by |
|---|---|---|
| `RemoveRecursivity` (`RemoveRecursivity.scala:19-30`) | single-binding rec let with no self-reference → plain let (which makes it an inlinable beta-redex) | multi-binding lets (`:31-39`), any syntactic self-reference (`isRecursive`, `:81-111`) |
| `MutualRecursionElimination` (`compiler/sir/MutualRecursionElimination.scala:7-40`) | N-member mutual group → nested single-binding lets, peers-as-params, eta-lets for far refs (O(N²)) | a cyclic group whose members are not all lambdas |
| `StaticArgumentTransformation` (`compiler/sir/StaticArgumentTransformation.scala:7-48`) | a param every self-call passes unchanged → bound once by a wrapper; saves one `Apply` per lifted arg per iteration | multi-binding rec lets, lazy lets, non-lambda rhs, duplicate param names, **any self-reference that is not the head of a fully saturated self-call**, or no static param. Pinned in `StaticArgumentTransformationTest.scala:353` (under-saturated), `:370` (bare self-ref), `:386` (shadowed param), `:466` (mutual group), `:576` (release-only) |
| `Inliner.shouldInline` (`uplc/transform/Inliner.scala:142-153`) | `OnceDirect` → inline anything; `OnceGuarded` (occurrence under `LamAbs`/`Delay`/`Case` branch, `:108-131`) → inline **only if `isValueForm`**; `Many` → only `Var`, `Builtin`, or `Const` of flat size ≤ 64 bits (`:150`); `Zero` + pure arg → DCE (`:227-229`) | a non-value-form rhs in guarded position; **any** helper used ≥ 2 times; impure args – saturated *partial* builtins and `Trace` count as impure (`uplc/transform/TermAnalysis.scala:211-223`) |
| `PartialEvaluator.tryEval` (`uplc/transform/PartialEvaluator.scala:36-57`) | **no builtin whitelist** – runs the real CEK machine on any subterm that is not already a value form, is **closed**, contains a reducible `Apply`/`Force`/`Case`, and contains no `Trace`; keeps the result only if it is a flat-encodable `Const`. `Data` constants **do** fold (`isFlatEncodable`, `:64-68`, excludes only BLS12-381 G1/G2/MlResult) | free variables (`:41`); **any `Trace` anywhere in the subterm** (`:46-47`) – and `generateErrorTraces = true` is the **default**, so error paths block folding; partial applications; budget cap `ExUnits(mem 1e6, steps 1e9)` (`:27`) |
| `EtaReduce` (`uplc/transform/EtaReduce.scala:44-47`) | `λx. f x → f` when `x ∉ freeVars(f)` and `f.isPure` | an impure head (e.g. a saturated partial builtin) |
| `StrictIf` (`uplc/transform/StrictIf.scala:178-193`) | removes the `Delay/Delay/Force` around `ifThenElse` when **both** branches are `Var \| Const \| LamAbs \| Delay \| Builtin \| Constr(_, Nil)`; saves 3 machine steps | any branch that is an `Apply`, `Force`, `Case`, non-empty `Constr` or `Error` – i.e. almost every branch that computes something |
| `ForcedBuiltinsExtractor` (`uplc/transform/ForcedBuiltinsExtractor.scala:32-37, 59, 77`) | hoists `force (builtin f)` used ≥ 2× to a top-level binding; occurrences inside a lambda count double | – |
| `CSE` (`uplc/transform/CommonSubexpressionElimination.scala`) | 3-pass path-based; candidate needs total count ≥ 2 (`:145`) | work-free/value-form terms and anything containing `Error` (`:459-471`, `:480-488`); free vars not in scope or shadowed (`:119-131`); **crossing a `Delay` or `Case` branch while referencing a shape-partial builtin** (`unConstrData`/`unMapData`/`unListData`/`unIData`/`unBData`/`headList`/`tailList`, plus any `Apply(Var(userHelper), _)` conservatively – `:406-416`, `:441-456`, `:157-164`) |
| `CCE` (`uplc/transform/CommonContextExtraction.scala:403`) | extracts one-hole contexts like `λa. headList(tailList(sndPair(unConstrData(a))))`; needs template size ≥ 6, ≥ 2 distinct leaves, `(N-1)*size > N+3` | **off by default** (`compiler.scala:17`) |
| `CaseConstrApply` (`uplc/transform/CaseConstrApply.scala:28-32`) | apply chains of **> 2 args** → `case (constr 0 [args]) [f]` | 1- and 2-arg chains are left as `Apply` |

### 5.3 Which combinator styles survive

**Survive well**

1. **Non-recursive helpers used exactly once.** `RemoveRecursivity` turns the linked rec-let into a
   plain let, whose rhs is a `LamAbs` – a value form (`TermAnalysis.scala:133`) – so both
   `OnceDirect` and `OnceGuarded` inline it. `InlinerTest.scala:303-309` shows
   `(λf. f 1) (λy. y+2)` collapsing to `3`.
2. **Fully closed computation, including higher-order and recursive.** `PartialEvaluator`
   CEK-evaluates it to a constant (`PartialEvaluatorTest.scala:130`, `:149`, `:156`). Requires
   `optimizeUplc = true` **and** no `Trace` on the path.
3. **Combinators with an intrinsic provider for the relevant representation** (§3.2/§3.3, §2.3) –
   substitution is unconditional and happens even with `optimizeUplc = false`.
4. **Recursive loops with an invariant parameter, under `Options.release`** – SAT lifts a
   predicate/comparator argument out of the loop: one `Apply` at entry instead of one per iteration.
5. **Repeated identical `force builtin` chains** and **repeated identical whole subexpressions in
   unconditional position** (ForcedBuiltinsExtractor, CSE) – release only.

**Do not survive**

1. **The lambda passed to a recursive higher-order combinator is never inlined into the loop body.**
   Chain of evidence: a recursive linked def lowers to
   `Apply(LamAbs(f, body), Apply(LamAbs(f, f f), LamAbs(f, rhs')))`
   (`.../lowering/LoweredValue.scala:1019-1042`); that fixpoint is an `Apply` over a non-builtin, so
   `isValueForm = false` (`TermAnalysis.scala:135-148`); call sites live inside the validator's
   lambda, so the occurrence is `OnceGuarded`, which requires a value form → **false**
   (`Inliner.scala:146`). With ≥ 2 call sites, `Many` also rejects a `LamAbs` (`:147-152`).
   **So `p` in `xs.filter(p)` is entered by a runtime `Apply` per element, always.**
2. **No fusion of any kind** – see §3.6.
3. **A helper used 2+ times is never inlined, at any size.** `Many` admits only `Var`, `Builtin`,
   `Const ≤ 64 bits` (`Inliner.scala:147-152`). There is no size-budgeted call-site inlining and no
   user inline hint; open as T15 (`docs/internal/CODEGEN_IMPROVEMENT_PLAN.md:621-634`).
4. **Field projections behind a branch are not hoisted.** CSE refuses to move anything referencing
   `unConstrData`/`headList`/`tailList` (or an unknown user helper) across a `Case` branch or `Delay`
   (`CommonSubexpressionElimination.scala:157-164`, `:406-416`, `:441-456`; test `:580`).
   Repeating `ctx.txInfo.outputs` in two match arms costs twice.
5. **`if`/`match` with computed branches keeps its delay/force** – `StrictIf.canBeStrict` rejects any
   `Apply` (`StrictIf.scala:180`), so 3 extra machine steps per conditional stay.
6. **Generic `===` on a type-variable-represented value lowers to `equalsData`, not the cheap
   primitive builtin.** Measured: generic `SortedMap.get` via `equalsData` = 1 761 779 cpu vs
   832 313 cpu for a concrete-typed clone using `equalsInteger` – a **2.1x** gap no optimizer pass
   closes (`docs/internal/SORTEDMAP_LOOKUP_COST_FINDINGS.md:60-72`; the fix is a lowering change,
   `:96-104`).
7. **No effect reordering / single-use sinking.** Scalus preserves source evaluation order by design;
   the plan records Aiken's inliner sinking single-use bindings past checks for −61 % CPU on failure
   paths (`CODEGEN_IMPROVEMENT_PLAN.md:76-79`, `:99`), and no `relaxedEvaluationOrder` option exists
   (`:249-250`). **Order your guards yourself.**
8. **2-argument calls do not get the cheap V3 application encoding** (`CaseConstrApply.scala:28`).

### 5.4 The fixpoint constraint, in the code

`.../lowering/LoweredValue.scala:1008-1018`, verbatim:

> ```
> Self-application encoding (no fixpoint combinator):
> (\f -> f 0) ((\f -> f f) (\f. \x. (f f) (x + 1)))
> Each recursive call costs one Apply instead of the Z-combinator's
> eta-wrapper dispatch (6 machine steps cheaper per call).
> The fixpoint `(\f -> f f) rhs'` is kept as a closed subterm in argument
> position (same top-level shape as the old `(\f -> body) (Z rhs)`) so
> Inliner + PartialEvaluator can still constant-fold closed recursive
> computations at compile time.
> ```

The same paragraph is repeated for the legacy backends at
`.../lowering/simple/BaseSimpleLowering.scala:604-611`.

Mechanically, "closed argument-position subterm" is exactly what lets
`PartialEvaluator.tryEval`'s `freeVars.nonEmpty` guard (`PartialEvaluator.scala:41`) pass on the
enclosing `Apply`. Float the fixpoint anywhere it captures a variable and the whole recursive call
stops folding. The 6-machine-step figure is measured, not asserted: `ExprSizeAndBudgetTest.scala:310-311`
and `:322-323`.

The fixpoint is also self-protecting: in `Apply(LamAbs(f, f f), rhs)` the binder occurs twice →
`Many` → `shouldInline(LamAbs, Many) = false` (`Inliner.scala:147-152`), so it is never beta-reduced
into itself. The Z combinator survives only as a `@deprecated` binary-compat stub
(`.../lowering/LoweringContext.scala:11-14`, `BaseSimpleLowering.scala:42-46`); multi-binding
recursive lets reaching a backend are a `sys.error` (`BaseSimpleLowering.scala:622-627`).

### 5.5 Documented caveats and open work

- **Fixed iteration counts, no fixpoint driver** – Aiken loops until node count stabilises; Scalus
  does 3× inline, 2× CSE, so "missed late-exposed redexes remain" (T3,
  `CODEGEN_IMPROVEMENT_PLAN.md:353-366`).
- **CSE output is not byte-stable**: the tie-break is `key.toString`, which embeds scalac symbol IDs
  (`CommonSubexpressionElimination.scala:169`), shifting output ~0.05 % across incremental
  recompiles (`CODEGEN_IMPROVEMENT_PLAN.md:185-191`) – this is why `KnightsTest` uses a 5 % tolerance.
- **No curried-prefix builtin hoisting**, no commutative-argument canonicalisation (T4, `:368-382`).
- `LetFloating` carries its own TODO about a missing post-pass beta reduction
  (`.../lowering/simple/LetFloating.scala:26-27`) – moot, since it is not in the default backend.
- `InlinerTest.scala:359-360` contains a stale "BUG:" comment claiming `tryPartialEval` is skipped;
  `Inliner.scala:233` does call it and `:361` asserts the fold succeeds. **Do not cite it.**

---

## 6. Idiom cost table

Relative cost is expressed as traversals / allocations / builtin counts where no measurement exists –
per the constraints above, those are structural facts, not estimates. "n" = list length,
"k" = survivors.

| # | Idiom (avoid) | Cost mechanism | Prefer | Why it is cheaper | Evidence |
|---|---|---|---|---|---|
| I1 | `tx.inputs.filter(p).length == 1` | 2 traversals + k `mkCons`; `filter` is a non-tail `foldRight`; on `BuiltinList` repr **neither** `filter` nor `length` has an intrinsic | `tx.inputs.count(p) == 1` – or better, a `foldLeft` that returns the *element*, not a count | 1 traversal, tail-recursive, 0 allocations | `List.scala:805-808`, `:947-949`, `:1004`, `:1103`; `ListIntrinsics.scala:90-94`; §3.6 |
| I2 | `xs.exists(p)` | `find(p).isDefined` → allocates 1 `Option` even though only a `Boolean` is wanted | `xs.contains(x)` when comparing for equality (intrinsic → `equalsData` scan, no Option, no `Eq` closure); otherwise `forall`/hand-rolled fold | intrinsic drops both the `Option` and the implicit `Eq` argument | `List.scala:967`, `:518`; `ListIntrinsics.scala:257-258`; `IntrinsicResolver.scala:36-50` |
| I3 | `tx.outputs.find(o => …)` then re-scan for a second property | 1 traversal per property, 1 `Option` each; `find`'s lambda is never inlined | one `foldLeft`/recursion returning the element (or the tuple of what you need) – or index by a redeemer-supplied index | eliminates n `Apply`s per extra pass | `List.scala:875`; `Inliner.scala:142-153`; §5.3(1) |
| I4 | `tx.outputs.find(…)` for a *known* position | O(n) scan | `tx.outputs.at(i)` / `!!` with `i` from the redeemer | at PV11 `at` = `dropList` + `nullList` + `headList` (constant builtins) | `ListIntrinsics.scala:301-307`; `List.scala:457`, `:393` |
| I5 | `value.flatten` / `value.toSortedMap.toList` walk | nested `foldRight` over policy × token; no intrinsic | `value.quantityOf(cs, tn)` / `getLovelace` / `containsAtLeast` | at PV11 these are single CIP-153 builtins: 13x / 31x / 75x cheaper per call | `Value.scala:1015-1020`, `:795-800`, `:824-836`; `ValueIntrinsics.scala:32`, `:74`; `…t7-value-builtins-lowering-design.md:19-23` |
| I6 | building a `Value` with `+` in a loop | each op pays a full `unValueData … valueData` round trip (phase-2 gap) | accumulate the scalar you need, or use `insertCoin` once | avoids the per-step re-parse | `ValueIntrinsics.scala:47-83`; `CODEGEN_IMPROVEMENT_PLAN.md:432-436` |
| I7 | `a.toData == b.toData` written by hand, believing `===` is field-by-field | **already redundant** – for any Data-backed type `a === b` *is* `equalsData(toData a, toData b)`. Measured identical: both spellings pin to `901 / 1 653 665` on `Value.lovelace` | just write `a === b` | `LoweringEq.generateEqualsForRepr` dispatches everything that is not a primitive or `@UplcRepr(UplcConstr)` to `generateDataEquals` | `LoweringEq.scala:139-146`, `:358-375`; `Eq` interception at `:56-60`, `:74-87`; pins `ValueTest.scala:1619` vs `:1628` |
| I7b | `a == b` (Scala `==`) on a case class in on-chain code | **compile error** – `compileEquality` supports only `BigInt`, `Boolean`, `ByteString`, `String`, `Data`; everything else falls to the error branch, and `Rational`/`AssocMap` get dedicated refusals | `a === b`; `Data == Data` is fine and is already `equalsData` | the plugin refuses rather than silently emitting a slow comparison – the error message itself points at `===` and `toData(x) == toData(y)` | `scalus-plugin/src/main/scala/scalus/compiler/plugin/SIRCompiler.scala:2782` (entry), `:3006` (dispatch), `:2889-2898` (`Data`), `:2904`, `:2915` (Rational/AssocMap), `:2925-2945` (general error) |
| I8 | `a === b` on `@UplcRepr(UplcConstr)` values | field-by-field `genSelect` + AND chain | `equalsData` is not available on that repr – either accept the chain or drop the annotation | the annotation is the thing that costs you here | `LoweringEq.scala:131-134`, `:384-417` |
| I9 | generic `===` on a `BigInt`/`ByteString` behind a type variable | lowers to `equalsData`, not `equalsInteger`/`equalsByteString`: **2.1x** slower (1 761 779 vs 832 313 cpu) | make the key type concrete at the comparison site | primitive-typed comparison hits `generatePrimitiveEquals` | `SORTEDMAP_LOOKUP_COST_FINDINGS.md:60-72`; `LoweringEq.scala:105-110`, `:346-355` |
| I10 | `SortedMap.contains(k)` to check **absence** | full `equalsData` scan, no early exit – regresses **4-8x in fee** vs the `Ord` form | keep the `Ord`-based lookup, or use `hasOnly` / a concrete-key scan | `hasOnly` is "~35 % cheaper in fee" | `SORTEDMAP_LOOKUP_COST_FINDINGS.md:76-84`, `:105-108`; `CHANGELOG.md:23` |
| I11 | `AssocMap.union` / `AssocMap` for anything with > ~5 entries | **O(n·m)**: `rhs.get(k)` per left key + `lhs.toList.exists` per right key | `SortedMap.union` | one linear merge, and `SortedMap.get` short-circuits on `Order.Less` | `AssocMap.scala:157`, `:166`; `SortedMap.scala:181-211`, `:633` |
| I12 | `xs.groupBy(f)` / `groupMap` / `groupMapReduce` | **O(n·m)** – `SortedMap.insert` per element, each rebuilding the spine and re-emitting `mapData` | fold into an accumulator, or pre-group off-chain | avoids n map rebuilds | `List.scala:572`, `:576`, `:618`, `:622` |
| I13 | `map.toList.map { case (k,v) => (k, f(v)) }` | `List[(A,B)]` pattern-matching: **~12 builtins/element** | `map.toPairList.mapValues(f)` (or `SortedMap.mapValues`) | `fstPair`/`sndPair` directly: **~4 builtins/element**; `toPairList` is a free relabel | `PairList.scala:98-102`, `:69-73`; `SortedMap.scala:472-475` |
| I14 | `xs.length == 0` / `map.size == 0` | O(n) walk (`SortedMap.size` is O(n) despite being `inline`) | `xs.isEmpty` / `map.isEmpty` | `nullList` – O(1) | `List.scala:405`, `:1103`; `SortedMap.scala:396` → `PairList.scala:94`, `SortedMap.scala:372` |
| I15 | `xs :+ elem` in a loop | O(n) per append → O(n²) | `elem +: xs` then one `reverse` | `mkCons` is O(1) | `List.scala:714`, `:669`, `:1419` |
| I16 | `xs.distinct`, `xs.diff(ys)`, `xs.sort` on-chain | O(n²) / O(n·m) / O(n log n) with tuple allocations; `sort` has an intrinsic **only** on the `UplcConstr` repr | sort/dedupe off-chain, verify the ordering on-chain in one pass | one O(n) check replaces the work | `List.scala:1354`, `:1382`, `:341`; `ListIntrinsics.scala:212-234` |
| I17 | a helper that returns `Option[A]` in a hot path | **`Option` always allocates** (`constrData(0, mkCons(x, mkNilData()))`) and nothing folds `Case`-over-literal-`Constr` | a `…OrFail(msg)` helper that returns `A` and errors, or a fold with an accumulator | no `Constr` build, no `unConstrData` on the consumer side | `Option.scala:15-18`; `DataConstrEmitter.scala:34-46`, `:504`; `CaseConstrApply.scala:28-32`; `Inliner.scala:203-212` |
| I18 | returning a tuple to carry 2 results out of a fold | `Tuple2` gets `ProdDataList` (`ProductCaseEmitter.scala:36-41`), so each step builds and destructures a Data list – `takeRight`/`dropRight` already pay **n tuples** | continuation-passing, or two folds only if one of them is cheap | avoids n Constr/list builds | `ProductCaseEmitter.scala:36-41`; `List.scala:1305`, `:1202` |
| I19 | `require(cond, "long message")` in a release build | with `generateErrorTraces = true` (**the default**), an error becomes `force(trace(msg, delay(Error)))` – a string constant + a builtin call + force/delay, and the `Trace` **blocks `PartialEvaluator` folding on that whole subterm** | `Options.release` (`generateErrorTraces = false`, `removeTraces = true`) | `genError` emits a bare `Term.Error()` | `Lowering.scala:19-26`; `SIRDefaultOptions.scala:14`; `PartialEvaluator.scala:46-47`; `compiler.scala:63-70` |
| I20 | `cond.?` (the trace-on-false operator) | expands to `if x then true else trace("<src> ? False")(false)` – a source-text string constant per use | keep it; set `removeTraces = true` for release | `RemoveTraces` rewrites `Apply(Apply(Trace, msg), value) → value`, dropping the string entirely | `utils/Macros.scala:583-586`; `compiler/sir/RemoveTraces.scala:6-14`, `:30-37` |
| I21 | expensive check before a cheap one | Scalus preserves source evaluation order; **no pass sinks or reorders** | order guards cheapest-and-most-likely-to-fail first, by hand | `&&`/`\|\|` do short-circuit; the optimizer will not do this for you | `CODEGEN_IMPROVEMENT_PLAN.md:76-79`, `:249-250` |
| I22 | reading `ctx.txInfo.outputs` inside two `match` arms | CSE will not hoist a `headList`/`unConstrData` chain across a `Case` branch | bind it once **above** the match | the `genSelect` scope cache only shares within one lowering scope | `CommonSubexpressionElimination.scala:157-164`, `:406-416`; `ProdDataListOps.scala:138-139`, `:164-165` |
| I23 | a stdlib type that stores a closure (comparator, callback, "lazy" thunk) | `containsFun` forces `ProductCaseUplcConstrOnly` / `SumCaseUplcConstrOnly` – the type can never be `Data`, loses `equalsData`, loses free `fromData`/`toData`, loses every Data-shaped intrinsic | keep closures out of stored types; pass them as arguments | representation is decided structurally, silently | `SirTypeUplcGenerator.scala:369-370`, `:494-496` |
| I24 | compiling with a `given Options(ScottEncodingLowering)` anywhere in the file | `useUniversalDataConversion` is decided at that call site; the **real** eager `FromData` decoders get linked, and `toUplc(backend = V3)` cannot undo it | never put a non-V3 `given Options` at class/file level in code you will measure | link decision is baked into the SIR | `SIRLinker.scala:20-21`; `Plugin.scala:52-82`, `:136`, `:145` |

**Verdict on the `optimize-contract` skill.** Source read: `scalus-skills/skills/optimize-contract/SKILL.md`
and `scalus-skills/skills/optimize-contract/references/patterns.md` (worktree copy, the canonical one).

| Skill claim | Verified? |
|---|---|
| O001 multiple traversals → single `foldLeft` | **Confirmed** – no fusion exists (§3.6) |
| O002 `foldRight` builds thunks, prefer `foldLeft` | **Confirmed** – `foldRight` is non-tail (`List.scala:947-949`) and `map`/`filter`/`filterMap` are built on it (`:765`, `:806`, `:851`) |
| O003 `flatten` is O(n·m) | **Confirmed** (`List.scala:385`) |
| O004 `distinct` is O(n²) | **Confirmed** (`List.scala:1354`) |
| O005 `:+` is O(n) | **Confirmed** (`List.scala:714`) |
| O007 `AssocMap` lookup has no early termination | **Confirmed** (`AssocMap.scala:111` vs `SortedMap.scala:633`) |
| O009 `length` to test emptiness is O(n) | **Confirmed**, and also true for `SortedMap.size` (`SortedMap.scala:396`) |
| O010 `AssocMap.fromList` is O(n²) | **Plausible, not verified here** |
| O014 "Scalus `&&`/`\|\|` DO short-circuit" | **Confirmed** by the absence of any reordering pass, and `SIR.And`/`SIR.Or` are preserved through linking (`SIRLinker.scala:363-370` region) |
| O016 / O020 "use `equalsData` instead of typed `===`" | **Stale.** For Data-backed types `===` **already lowers to `equalsData`** (`LoweringEq.scala:139-146`, `:358-375`). Writing `equalsData(a.toData, b.toData)` by hand buys nothing. The real advice is I9: make key types concrete so you get `equalsInteger`, not `equalsData` |
| O018 `PairList` ~4 builtins/element vs ~12 | **Confirmed** – the numbers come from the scaladoc itself (`PairList.scala:98-102`) |
| O023 "V3 optimizer has CSE but don't rely on it" | **Confirmed and understated** – CSE is off entirely unless `optimizeUplc = true`, which is **not** the default (`SIRDefaultOptions.scala:16`), and even then it refuses to cross `Case`/`Delay` for shape-partial builtins |
| O024 `generateErrorTraces = false` for production | **Confirmed**, and there is a second effect the skill misses: `Trace` blocks `PartialEvaluator` (`PartialEvaluator.scala:46-47`) |
| O028 "keep data in `Data` form between operations" | **Confirmed, and stronger than stated** – under V3 there is no other form: `fromData`/`toData` are identity (`Lowering.scala:1089-1112`) |
| "Estimated savings ~40-60 %", "~10-40 %", "~100-500 steps per `require`" | **Unverified** – no repo artifact backs these percentage ranges. Use the structural counts and the CEK-step arithmetic in §0 instead |
| O021/O022 `exp2`/`log2` are single builtins | **Not checked in this pass** |

---

## 7. Design rules for the new stdlib API

Hard constraints. Each rule names the fact that forces it.

**A. Representation**

1. **Stay Data-backed.** Under the default backend a case class is `ProdDataList`/`ProdDataConstr`
   (`ProductCaseEmitter.scala:41`, `:48-51`), which makes `fromData`/`toData` **literally identity**
   (`Lowering.scala:1089-1112`) – measured length-independent at 432 mem / 74 033 cpu
   (`ListTest.scala:270-288`). Any stdlib type that leaves the Data world gives that up. Only reach
   for `@UplcRepr(UplcConstr)` when a type is provably never serialised and is field-accessed in a
   hot loop.
2. **Never store a closure in a public stdlib type.** `containsFun` silently forces
   `Prod/SumCaseUplcConstrOnly` (`SirTypeUplcGenerator.scala:369-370`, `:494-496`), which loses
   `equalsData`, loses free `fromData`/`toData`, and loses every Data-shaped intrinsic. Pass
   comparators, predicates and validators as **arguments**, never as fields.
3. **Order case-class fields by access frequency for PV10 compatibility, not for PV11.** At PV11 a
   field read is `dropList + headList` regardless of index; at PV10 it is `index + 1` `tailList`
   calls (`ProdDataListOps.scala:135-186`). If the API must compile at PV10, field 0/1 are free and
   field 15 is not.
4. **A stdlib type must be usable as its own datum/redeemer without a conversion step.** That is the
   payoff of rule 1 and the reason `FromData` deriving costs nothing for a 16-field `TxInfo`
   (`FromDataMacros.scala:54-124` generates an eager decoder that the V3 pipeline never emits).

**B. API shape**

5. **Return the found element, never a Boolean, so callers do not traverse twice.** `exists` is
   `find(p).isDefined` (`List.scala:967`) and is **not** intrinsic – measured to cost the full Option
   tax of **326 483 cpu (miss) / 564 996 cpu (hit) ≈ 85 / 158 lovelace per call**
   (`LIST_CONTAINS_IMPL_COST.md:33-40` **(primary)**). Every predicate-shaped query in the new API
   must have an element-returning form.
6. **Do not return `Option` in hot paths.** `Option` allocates a real `constrData(0, mkCons(x, mkNilData()))`
   (`DataConstrEmitter.scala:34-46`) and **nothing folds `Case` over a literal `Constr`**
   (`CaseConstrApply.scala:28-32`, `Inliner.scala:203-212`, `PartialEvaluator.scala:41`). Ship a
   `…OrFail(message)` variant that returns `A` and errors, and make it the documented default.
7. **Do not return tuples from folds.** `Tuple2` lowers to `ProdDataList`
   (`ProductCaseEmitter.scala:36-41`), so an accumulator tuple builds and tears down a Data list per
   step – the reason `dropRight` costs ~4x `drop` (`ListTest.scala:2256` vs `:2332`). Prefer
   continuation-passing or a purpose-built accumulator type.
8. **Expose fold-based combinators that visit `inputs`/`outputs` once.** There is **no fusion of any
   kind** in Scalus (§3.6, §5.3): `xs.filter(p).length` is 2 traversals plus k `mkCons`. The API must
   ship the fused primitives itself – `count`, `filterMap`, `findMap` are the existing precedents.
   Anything a user would naturally write as a 2-stage pipeline needs a 1-stage name.
9. **Every combinator must be tail-recursive.** `foldRight` is not (`List.scala:947-949`) and
   `map`/`filter`/`filterMap` are all built on it (`:765`, `:806`, `:851`). Build results with an
   accumulator + one `reverse`, never with `foldRight`.
10. **Prepend, never append.** `prepended` is flat at 1 264 / 274 395 for every length;
    `appended` is 3 264 / 594 395 and grows (`ListTest.scala:920-974` vs `:1120-1138`).
11. **Never expose an O(n·m) or O(n²) combinator without a loud warning in its scaladoc.** Today's
    offenders: `AssocMap.union` (`AssocMap.scala:157`, `:166`), `List.distinct` (`:1354`),
    `List.diff` (`:1382`), `groupBy`/`groupMap`/`groupMapReduce` (`:536`, `:560`, `:606` – measured
    12-21 M cpu on **two** elements, `ListTest.scala:742`, `:787`, `:828`).
12. **Provide an index-addressed escape hatch for every search.** `List.at(i)` at PV11 is
    `dropList + nullList + headList` (`ListIntrinsics.scala:301-307`) – measured 1 664 / 276 143
    (`ListTest.scala:626`) versus a `find` that scans. Redeemer-supplied indices are the standard
    Cardano idiom and the API must make them first-class, not a workaround.

**C. Comparison and equality**

13. **Prefer Data-level comparison – and stop hand-writing it.** For any Data-backed type
    `a === b` **already lowers to** `equalsData(toData a, toData b)`
    (`LoweringEq.scala:139-146`, `:358-375`), proven by identical pins for the two spellings
    (`ValueTest.scala:1619` vs `:1628`, both 901 / 1 653 665). Document `===` as the cheap form; do
    not add `equalsData` wrappers to the API.
14. **Keep key types concrete at comparison sites.** Generic `===` behind a type variable emits
    `equalsData`, not `equalsInteger` – measured **1 761 779 vs 832 313 cpu, a 2.1x penalty**
    (`SORTEDMAP_LOOKUP_COST_FINDINGS.md:60-64`). A polymorphic `Map[K, V]` API pays this on every
    lookup; specialised `ByteString`-keyed and `BigInt`-keyed entry points do not.
15. **Budget `equalsData` at ~1 034 543 cpu per compared list element**
    (`LIST_CONTAINS_IMPL_COST.md:64` **(primary)**), and remember it is **whole-tree**: `Value`
    equality costs 1.65 M cpu for lovelace-only and 47.3 M for three policies
    (`ValueTest.scala:1619`, `:1799`). Never compare whole `TxInfo`-scale structures.

**D. `Value`**

16. **Route every `Value` query through the CIP-153 builtin surface**: `quantityOf`, `getLovelace`,
    `containsAtLeast`, `insertCoin`, `+`, `-`, `*`, `negate` (`ValueIntrinsics.scala:99-107`).
    These are **13x / 31x / 18x / 75x** cheaper than the portable walk and break even at **one call**
    (`…t7-value-builtins-lowering-design.md:19-28`; `ValueBuiltinsBudgetTest.scala:48-52`).
17. **Never expose `flatten`/`toList` as the idiomatic way to inspect a `Value`.** Measured
    12 178 230 cpu on a multi-asset value versus 895 629 for `quantityOf`
    (`ValueTest.scala:1502` vs `:1170`) – ~13x. Provide targeted accessors instead.
18. **Do not chain `Value` arithmetic in the API's own implementations.** Each intrinsic pays a full
    `unValueData … valueData` round trip (`ValueIntrinsics.scala:47-83`); `BuiltinValueBacked` is
    planned but **not landed** (zero `.scala` occurrences;
    `CODEGEN_IMPROVEMENT_PLAN.md:432-436`).
19. **Match the intrinsic's method name and arity exactly.** Dispatch is by simple name against a
    7-entry map and requires exact arity (`ValueIntrinsics.scala:99-107`,
    `IntrinsicResolver.scala:279-280`, `:297-298`); a wrapper with a different name or a partially
    applied call **silently** falls back to the loop.

**E. Collections**

20. **Default to `SortedMap`, never `AssocMap`.** `SortedMap.get` short-circuits on `Order.Less`
    (`SortedMap.scala:633`) and `SortedMap.union` is one linear merge (`:181-211`); `AssocMap` has
    neither, and `AssocMap` has deliberately no `Eq` (`AssocMap.scala:178-182`).
21. **Use `PairList` for anything key-value shaped.** `toPairList`/`toList` are **zero-cost relabels**
    (`PairList.scala:69-73`, `SortedMap.scala:356-359`), and `mapValues` via `fstPair`/`sndPair` is
    **~4 builtins/element vs ~12** for `List.map` on tuples (`PairList.scala:98-102`).
22. **`isEmpty`, never `size == 0`.** `SortedMap.size` is O(n) despite being `inline`
    (`SortedMap.scala:396` → `PairList.scala:94`); `isEmpty` is one `nullList` (`:372`, `:88`).
23. **Assume the caller's list is Data-packed, because it is.** `tx.inputs`/`tx.outputs` match only
    the `"BuiltinList"` provider (`IntrinsicResolver.scala:648-649`), whose intrinsic set is
    `isEmpty, head, tail, drop, at` (`ListIntrinsics.scala:90-94`) – **no `map`, `filter`, `foldLeft`,
    `length`**. Any combinator the new API builds on those four is prelude recursion, so the API must
    keep the per-element body minimal rather than assume the compiler will help.

**F. Errors, traces and the optimizer**

24. **Design for `optimizeUplc = false`, which is the default** (`SIRDefaultOptions.scala:16`;
    only `Options.release` turns it on, `compiler.scala:63-70`). Nothing may depend on CSE, inlining,
    partial evaluation or SAT for its cost story. `IntrinsicResolver` substitution is the one
    optimisation that runs unconditionally – build on that.
25. **Assume no helper you call twice is ever inlined.** `Many` occurrences admit only `Var`,
    `Builtin`, and `Const ≤ 64 bits` (`Inliner.scala:147-152`). Keep the number of *distinct*
    helper calls low, not the number of lines.
26. **Assume the lambda you pass to a combinator is entered by a runtime `Apply` per element**
    (§5.3(1)). Where the per-element body is one builtin, prefer an intrinsic-backed whole-operation
    call (`contains`, `indexOf`) over a HOF.
27. **Keep `require`/`fail` messages short, and document `Options.release`.** With
    `generateErrorTraces = true` (**the default**, `SIRDefaultOptions.scala:14`) each error becomes
    `force(trace(msg, delay(Error)))` (`Lowering.scala:19-26`) – a string constant plus a builtin plus
    force/delay – **and the `Trace` blocks `PartialEvaluator` folding on that whole subterm**
    (`PartialEvaluator.scala:46-47`). The `?` operator is fine: it is stripped whole by `RemoveTraces`
    (`RemoveTraces.scala:30-37`) when `removeTraces = true`.
28. **Order guards yourself, cheapest and most-likely-to-fail first.** No pass reorders or sinks
    effects (`CODEGEN_IMPROVEMENT_PLAN.md:76-79`, `:249-250`); Aiken's single-use sinking is worth
    −61 % cpu on failure paths and Scalus does not have it.
29. **Bind a field once above a `match`, never inside two arms.** CSE will not hoist an
    `unConstrData`/`headList` chain across a `Case` branch or `Delay`
    (`CommonSubexpressionElimination.scala:157-164`, `:406-416`); the `genSelect` scope cache
    (`ProdDataListOps.scala:138-139`, `:164-165`) only shares within a single lowering scope.
30. **Never introduce a recursive combinator whose fixpoint could be floated.** The self-application
    fixpoint must stay a **closed, argument-position subterm**
    (`LoweredValue.scala:1008-1018`, restated at `BaseSimpleLowering.scala:604-611`) or
    `PartialEvaluator`'s `freeVars.nonEmpty` guard (`PartialEvaluator.scala:41`) rejects the whole
    enclosing `Apply` and closed recursive computations stop constant-folding. The encoding is worth
    a measured **6 machine steps = 96 000 cpu / 600 mem per call** over the Z combinator
    (`ExprSizeAndBudgetTest.scala:310-311`).

**G. Measurement discipline**

31. **Budget in machine steps, not vibes.** 1 CEK step = 16 000 cpu + 100 mem ≈ **6.92 lovelace**;
    one recursive call ≈ 32 256 cpu / 102 mem ≈ **9 lovelace**
    (`Cek.scala:52-63`; `ExprSizeAndBudgetTest.scala:101-103`). The binding limit is **memory**:
    16 500 000 / 100 ≈ **165 000 machine steps per transaction**, 3.8x tighter than the CPU limit.
32. **Optimise script size as hard as execution units.** ExUnits are ~11.4 % of a real transaction
    fee and CPU alone is 3 % (`authorized_collections.md:395-403`); tx size is 17.4 % and reference
    script another 17.2 %. A stdlib abstraction that adds a kilobyte to save 10 % CPU loses.
33. **Never measure with a non-V3 `given Options` in scope.** `useUniversalDataConversion` is decided
    by an implicit search at the `compile { … }` call site at scalac time
    (`Plugin.scala:52-82`; `SIRLinker.scala:20-21`); a file-level Scott `given` links the real eager
    `FromData` decoders and `toUplc(backend = V3)` cannot undo it. `ExprSizeAndBudgetTest.scala:31-32`
    is such a file – its absolute numbers are Scott-backend numbers.
34. **Pin every new combinator's budget in `ListTest`/`SortedMapTest`-style `ExUnits` assertions.**
    That corpus (220 + 71 + 69 pins) is the project's regression net; a combinator with no pin has no
    cost contract. Note the pins run with traces ON, so they are upper bounds relative to release.
