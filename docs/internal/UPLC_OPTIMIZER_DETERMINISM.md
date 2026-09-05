# UPLC optimizer determinism – the `DefaultFun.hashCode` hazard, confirmed and fixed

**Status:** FIXED. 2026-09-05. Three-line change in `CommonSubexpressionElimination.scala` and
`CommonContextExtraction.scala`, guarded by five tests. No example validator's script hash moved.

**Origin:** `docs/internal/PGO_SGD_OPTIMIZATION_RESEARCH.md` §5 (branch
`worktree-pgo-sgd-research`) listed this as an unverified "reproducibility hazard" and a
precondition for any pinned-configuration workflow.

**Guards:** `scalus-core/shared/src/test/scala/scalus/uplc/transform/CseDeterminismTest.scala`,
`scalus-core/jvm/src/test/scala/scalus/uplc/transform/CseDeterminismCrossJvmTest.scala`,
`scalus-examples/jvm/src/test/scala/scalus/examples/CseTieOrderDeterminismTest.scala`,
`scalus-examples/jvm/src/test/scala/scalus/examples/PipelineDeterminismCrossJvmTest.scala` (§8).

---

## 1. Summary

**The hazard is real.** The same Scalus source compiled through the production pipeline
(`PlutusV3.compile`, `Options.releaseUntagged`) produced two different CBOR encodings – hence two
different script hashes – in different JVM runs. It was reproduced with a 12-line on-chain
function, and separately with a synthetic UPLC term pushed straight through the CSE pass.

**The 22 example validators were never affected.** Across 24 pre-fix JVM runs under six different
identity-hash algorithms, all 22 validators (two optimizer configurations each, 44 programs) were
byte-identical every time. They are immune by luck of shape, not by design: their tied CSE
candidates never share a bind point (see §4.3).

**Root cause, in one sentence.** `DefaultFun` extends `java.lang.Enum`, whose `hashCode` is
final and identity-based; `TermKey.structuralHash` folded it in, so the two candidate hash maps
iterated in identity-hash order, and the stable `sortBy((-size, key.toString))` kept that order for
candidates whose 60-character `showShort` prefixes coincide.

**Fix.** `mutable.HashMap` → `mutable.LinkedHashMap` for `counts` (CSE) and
`templateOccurrences` (CCE), so candidate order is first-occurrence order; and `bn.hashCode` →
`bn.ordinal` in `structuralHash`, so the key hash is a pure function of the term. The third
proposed fix – a total tie-break instead of the 60-char prefix – was not needed and not applied
(§5.2).

**Effect on outputs.** None for the 22 example validators (44 programs byte-identical before and
after). The reproduction now always yields the variant in which the first-occurring chain is
bound outermost.

---

## 2. The hypothesised chain, and what each link did in practice

| # | Claim | Verdict |
|---|---|---|
| 1 | `DefaultFun.hashCode` is an identity hash that differs between JVM runs | **True, with a nuance.** It is identity-based, but HotSpot's default algorithm (mode 5, thread-local xorshift with fixed seeds) is *deterministic for a fixed program in a fixed environment*. The value changes when anything before it changes: adding one class to the probe moved `AddInteger.hashCode` from 423031029 to 3447021; sbt's forked test JVM gave 1788274004; the address-based algorithms (modes 1 and 4) differ on every run. See §4.1. |
| 2 | `TermKey.structuralHash` folds it in, so every builtin-containing key has a run-dependent hash | **True.** `case Builtin(bn, _) => bn.hashCode * 31 + 7`. The other leaves (`NamedDeBruijn`, `Constant`, `DefaultUni` case objects, `ByteString`, `Data`) hash structurally. |
| 3 | `counts` and `templateOccurrences` therefore iterate in run-dependent order | **True.** The raw key order in the instrumentation dump differed between identity-hash modes. |
| 4 | `sortBy((-size, key.toString))` is stable, so ties keep that order | **True.** |
| 5 | `key.toString` is `showShort`, truncated to 60 chars, so structurally different candidates tie | **True, and common.** 68–74 tie groups per probe run across the example corpus. The CSE ties are all field-access chains of the shape `[__HeadList [__TailList [__SndPair [(builtin unConstrData) v]]]]` for two different `v`; the CCE ties also include `mkCons`/`constrData` and `equalsData` templates. See §4.3. |
| – | Tied order changes the output | **Only when the tied candidates share a bind point.** Then the two `let`s nest in extraction order and the de Bruijn encoding differs. With different bind points the insertions are independent and the result is order-invariant. |

---

## 3. Method

Everything below was run on Zulu 25 (the nix default devshell) in the `cse-determinism`
worktree, base `origin/master` at `ab5a99f6a`.

**Probe.** A temporary main (not committed) compiled the 22 example validators from
`BooleanOptimizerImpactTest` under `Options.releaseUntagged` ("default", matches the pinned
tests) and `Options.releaseUntagged.copy(cseIterations = 4, cceEnabled = true)` ("stress" –
CCE is off by default, so the default config never exercises `templateOccurrences`). It printed
size and SHA-256 of `Program.cborEncoded` per program, the identity hashes of five builtins, a
synthetic control term through `CommonSubexpressionElimination` alone, and – from the second
sweep – a 12-line `@Compile` reproduction (now `SameScopeFieldChains` in
`CseTieOrderDeterminismTest`).

**Cross-JVM lever.** HotSpot's experimental `-XX:hashCode=N` selects the identity-hash algorithm:

| N | algorithm |
|---|---|
| 0 | Park-Miller RNG (fixed seed) |
| 1 | address-derived function |
| 2 | constant 1 |
| 3 | global counter |
| 4 | object address |
| 5 | Marsaglia xorshift, thread-local state (default) |

Plain repetition is the wrong experiment: in mode 5 a fixed program gets the same identity hashes
every run (three bare runs, three sbt-forked runs – identical values within each environment).
Sweeping the mode is what perturbs bucket order. Modes 2 and 3 are what the permanent
cross-JVM test uses.

**Instrumentation.** A temporary `-Dscalus.cse.debug` dump (not committed) wrote, per CSE/CCE
invocation, the raw hash-map key order, the sorted candidate list with bind paths, and every
group of distinct keys tied on the sort key, flagged `sameBind` when members shared a bind path.
This separates link 3 from link 5 and tells harmless ties from harmful ones.

**Runs.**

| set | JVMs | modes | purpose |
|---|---|---|---|
| default-1 | 1 | 5 | harness check |
| sweep1 | 12 | 0,1,2,3,4,5 ×2 | mode sweep, 22 validators + control |
| sweep2 | 8 | 1,4,2,5 ×2 | + `SameScopeFieldChains`, + bind paths |
| sbt-forked | 3 | 5 | realistic environment (`testOnly`) |
| sweep3 (post-fix) | 8 | 1,4,2,5 ×2 | verification |

---

## 4. Results

### 4.1 Identity hashes

| environment | `AddInteger.hashCode` | stable across reruns? |
|---|---|---|
| bare `java -cp`, mode 5, probe v1 | 423031029 | yes (3 runs) |
| bare `java -cp`, mode 5, probe v2 (one more class) | 3447021 | yes (2 runs) |
| sbt forked test JVM, mode 5 | 1788274004 | yes (3 runs) |
| mode 1 | 73144765 / 73144775 / 73184779 / 1799140499 | **no** |
| mode 4 | 49771952 / 1660389744 / 49422672 / 1660035408 | **no** |
| mode 2 | 1 | constant |
| mode 3 | 886 | yes |

So "differs between JVM runs" is imprecise. It differs between *environments* – any change in
the code that runs before the first builtin is hashed – and, under two of the six HotSpot
algorithms, between runs.

### 4.2 Compiled bytes, pre-fix

| program | runs | distinct outputs |
|---|---|---|
| 22 example validators × 2 configs (44 programs) | 24 (21 bare + 3 sbt) | **1 each** |
| control term through CSE alone (40 bytes) | 24 | **2** – `9f4d8893…` and `6ac7188e…`, flipping between modes and between two runs of the same address-based mode |
| `SameScopeFieldChains`, default config (59 bytes) | 11 (8 bare + 3 sbt) + 2 test JVMs | **2** – `0f665807…` (d2 chain bound outermost) in 10 runs, `136272b8…` (d1 outermost) in sweep2 mode-1 run 2 and in the `CseTieOrderDeterminismTest` JVM |
| `SameScopeFieldChains`, stress config (48 bytes) | 11 | **2** |

The last row deserves emphasis: the two sbt environments – the probe suite and the pinned test's
own suite – are the same JDK, the same mode, the same machine, and the same source, and they
compiled it to different scripts, because a different test class ran first.

The two `SameScopeFieldChains` outputs differ exactly as predicted: the `let`-nesting of the two
extracted chains is swapped (`__cse_..._d2` outermost vs `__cse_..._d1` outermost).

### 4.3 Ties

Per sweep2 run: **74 tie groups** (CSE 37, CCE 37), of which **21 share a bind path**:

| pass | same-bind tie groups | belongs to |
|---|---|---|
| CSE | 7 | 1 control term, 6 `SameScopeFieldChains` (3 groups × 2 configs) |
| CSE | 0 | any example validator |
| CCE | 14 | example validators under the **stress** config (`Value.apply`, `lookupCoin`, `equalsData` field chains, `mkCons`/`constrData` templates) |

That is the whole explanation of the example validators' immunity in the default config: every
one of their CSE ties is between chains over variables bound in different scopes (generated names
like `input_215927'1221` vs `input_215956'1329`), so the bind paths differ and extraction order
cannot matter.

The 14 CCE same-bind groups are not fully explained. Their member order *did* vary between modes
(5 of 30 member lines differ between mode 2 and mode 5) and the bytes did not change. Since
`cceEnabled` is off by default and the fix removes the variation, this was not pursued.

### 4.4 Post-fix

sweep3, 8 JVMs, modes 1/4/2/5: **one digest for all 46 programs in every run**; control term
identical in every run. The 44 example-validator lines are byte-for-byte the same as pre-fix
(`diff` of the sorted RESULT lines: 0 lines). `SameScopeFieldChains` is now always `136272b8…`
(d1 outermost) – the first-occurrence order.

### 4.5 Guards, before and after

| test | pre-fix | post-fix |
|---|---|---|
| `CseDeterminismCrossJvmTest` – six tied terms through CSE and `V3Optimizer`, in-process vs child JVMs with `-XX:hashCode=2` and `3`, flat bytes compared | **FAILED** (child bytes differ) | passes, 1.5 s incl. two JVM spawns |
| `CseDeterminismTest` – `TermKey(Force(Builtin(HeadList))).hashCode == 33856` | **FAILED** (identity value) | passes |
| `CseDeterminismTest` – tied x/y chains bound in first-occurrence order | passed (coin flip in that JVM) | passes by construction |
| `CseTieOrderDeterminismTest` – `SameScopeFieldChains` CBOR pinned | passed in its own JVM, but the probe's sbt JVM produced the other variant | passes by construction |

The cross-JVM test is the honest guard: it fails on the pre-fix code regardless of which way the
coin fell in the parent JVM. The pin is the cheap stand-in the plan asked for. The two
order-pinning tests document the behaviour and would have been 50/50 before.

---

## 5. The fix

### 5.1 What changed

```scala
// CommonSubexpressionElimination.scala
val counts = mutable.LinkedHashMap.empty[TermKey, mutable.ArrayBuffer[(Path, Int)]]
case Builtin(bn, _) => bn.ordinal * 31 + 7

// CommonContextExtraction.scala
val templateOccurrences = mutable.LinkedHashMap.empty[TermKey, mutable.ArrayBuffer[(Path, Term)]]
```

`LinkedHashMap` iterates in insertion order, and insertion happens in the count/collect pass,
which is a pre-order, left-to-right traversal. Candidate order is therefore a function of the
term alone. `ordinal` is a pure function of the enum constant, so `TermKey.hashCode` no longer
depends on anything outside the term either. The first change is sufficient for determinism; the
second removes the cause and makes the hash pinnable.

### 5.2 What did not change, and why

- **The 60-char tie-break.** With insertion-ordered maps, ties resolve to first-occurrence order,
  which is deterministic. Replacing the truncated key with a total order (full `show`, or an
  `Ordering[Term]`) would buy no determinism and would change the tie order for every same-scope
  tie, i.e. change compiled outputs for no benefit. Left as is.
- **`sortBy` recomputes `key.toString` per comparison** (Scala's `sortBy` wraps the key function
  in the `Ordering`), and `showShort` renders the *whole* term before truncating. A performance
  nit, orthogonal, not touched.
- **`Term`'s derived `hashCode`** still goes through `Builtin(bn, _)` → `bn.hashCode`, so a raw
  `Term` remains an identity-dependent hash key. Nothing in `scalus-core` main code uses `Term`
  or `SIR` as a hash-map key (`grep` for `Map[Term`, `Set[Term`, `Map[SIR`, `Set[SIR`,
  `HashMap[Term`… finds none); `TermKey` is the wrapper to use. Worth remembering, not worth a
  wrapper-level `hashCode` override on an enum.

### 5.3 Other `DefaultFun` hash and ordering sites, checked

| site | verdict |
|---|---|
| `ForcedBuiltinsExtractor`: `counts`, `extracted` are `mutable.Map[DefaultFun, …]` | safe – both are only looked up by key; the final `foldRight`/`foldLeft` runs over `extracted.toArray.sortBy(_._2._2)`, i.e. sorted by the unique generated name `__<Builtin>` |
| `Meaning.allBuiltins.forcedBuiltins: HashMap[DefaultFun, Term]` | safe – only `apply` lookups (`builtinTerms(bn)`) in the lowerings |
| `Term.collectBuiltins: Set[DefaultFun]` | safe – only `subsetOf` in `Script.isValid…`; never iterated into output |
| `cardano.ledger.Builtins` batch sets | safe – set algebra only |
| `LoweredValue`: `groupBy(_.representation)` … `minBy(_.stableKey)` | already deterministic by design |
| SIR level (`linking`, `sir.transform`, `MutualRecursionElimination`) | no `SIR`-keyed hash containers; `SIRLinker` already uses `LinkedHashMap` |

The end-to-end sweep is the stronger evidence: 44 programs identical under six identity-hash
algorithms means nothing else in the pipeline leaks identity hashes into the output for this
corpus.

---

## 6. Re-running the experiment

The permanent harness is `CseDeterminismCrossJvmTest`. To sweep a whole validator suite the way
the investigation did, compile any main that prints `Program.cborEncoded` digests, export the
classpath, and run it under each mode:

```bash
sbtn "export scalusExamplesJVM/Test/fullClasspath" > cp.txt
for m in 0 1 2 3 4 5; do
  java -XX:+UnlockExperimentalVMOptions -XX:hashCode=$m -cp "$(tail -1 cp.txt)" your.Probe > out-$m.txt
done
sha256sum out-*.txt
```

`-XX:hashCode` exists on HotSpot from JDK 8 through 25 (verified on Zulu 21.0.8, the CI JDK, and Zulu 25); it is experimental, so it needs the
unlock flag. Modes 1 and 4 also vary between runs of the same mode.

---

## 7. Follow-ups

- The PGO/SGD research doc (branch `worktree-pgo-sgd-research`) §5 item 1 should point here and
  drop the "not observed; worth verifying" wording; the pinned-config precondition is met.
- If `DefaultFun` cases are ever reordered, `CseDeterminismTest`'s pin (`33856`) must be
  re-pinned; the comment in the test gives the formula.
- If CCE is ever enabled by default, re-measure the example suite: 14 of its same-bind tie
  groups are in CCE, and with the fix their order is first-occurrence, which is not necessarily
  the order the current pinned ExUnits were measured under (they were measured with CCE off).

---

## 8. Whole-pipeline coverage and the audit for everything else

Added the same day, after the question "could we check determinism for all transformations?".

### 8.1 The corpus test

`scalus-examples/jvm/src/test/scala/scalus/examples/PipelineDeterminismCrossJvmTest.scala`
compiles 24 programs – the 22 example validators, `SameScopeFieldChains`, and a pure-ADT
`LegacyBackendSample` – in-process and in one child JVM under `-XX:hashCode=2`, and compares the
CBOR hex of every program. Because the flag perturbs every identity-keyed container at once, one
comparison covers every pass without knowing where the containers are.

| configuration | programs | what it exercises |
|---|---|---|
| `release` (`Options.releaseUntagged`) | 24 | linker, MutualRecursionElimination, BooleanOptimizer, StaticArgumentTransformation, `SirToUplcV3Lowering`, `V3Optimizer` |
| `cce` (`cseIterations = 4, cceEnabled = true`) | 24 | CCE and the extra CSE rounds production leaves off |
| `v2` (V3 lowering targeting `Language.PlutusV2`) | 5 | `V1V2Optimizer` |
| `scott`, `sop` | 2 each | `ScottEncodingLowering`, `SumOfProductsLowering` |

57 lines, identical in both JVMs. The legacy backends cannot lower the example validators at all
– they leave `UniversalDataConversion.fromData`/`toData` as free variables, because that
intrinsic module is resolved only by the V3 lowering – so they get the two samples that use raw
builtins and pure ADTs (tracked as scalus3/scalus#366). The whole test takes about 15 s: the in-process
compile of the corpus plus
one child JVM doing the same.

### 8.2 What the flag cannot catch, and the grep for it

Identity hashes are the only realistic drift source in a single-threaded, non-random compiler,
but not the only conceivable one. `grep` over `scalus-core` compiler/uplc main code and
`scalus-plugin` for `Random`, `nanoTime`, `currentTimeMillis`, `UUID`, `listFiles`,
`Files.list`/`walk`, `Thread`, `.par`, `Future`, `identityHashCode`, `getenv`, `getProperty`:

| hit | verdict |
|---|---|
| `Plugin.scala:291`, `SIRCompiler.scala:366` `currentTimeMillis` | timing log lines only |
| `LoweringContext` `scalus.disable.helper.cache`, `SCALUS_TRACE_LETREC` | diagnostic knobs, off by default |
| `LoweringContext:291` `ThreadLocal` | diagnostic current-function name |
| `Cek.scala` `getProperty` | evaluator diagnostics, not compilation |
| `SIRType.scala:465` `identityHashCode` | `showDebug` only |
| `SIRType.scala:454` `System.identityHashCode(ref)` in `TypeProxy.hashCode` | **the same class as the `DefaultFun` hazard**: any recursive `SIRType` has an identity-dependent hash. Every `SIRType`-keyed container was checked: `SIRUnify.parentTypes` (`get`/`updated` only), `SIRUnify.topLevelTypes` (`exists`, and a `toList` that only feeds a maximum), the proxy `Set`s in `IntrinsicResolver` and `SumUplcConstrOps` (cycle detection, `contains`). All `Map[SIRType.TypeVar, …]` keys are structural. The grep found no `SIRType`-keyed container iterated into output (it would miss a `groupBy` on an accessor with another name, or a `LoweredValueRepresentation` key wrapping a type); the corpus test watches this empirically. |

No randomness, time, or threading reaches the compile path.

### 8.3 The compiler plugin

SIR is generated at scalac time, one stage before everything above, and dotty `Symbol`s are a
natural identity-hashed key. Audit of `scalus-plugin`:

| site | verdict |
|---|---|
| `SIRTypeEnv.vars`, `forwardRefs`; `typeVars`, `resolvedClasses`; `Macros.generateBuiltinsMap`; `EqNonStructuralMethods` | `Symbol`-keyed, lookups only, never iterated |
| `Plugin.scala:245` blueprint-modules manifest | `Set[String]`, `.toSeq.sorted` before writing |
| `CompilationError.scala:171` missing constructors | `Set[Symbol]`, sorted by name before rendering |
| `SIRTyper.binderIdBases` | `IdentityHashMap` used as a lookup for counter-assigned ids |
| `groupBy` sites | keyed by `String` |
| `SIRPreprocessor:136`, `PatternMatchingCompiler:1999` | `Set`s used for `contains` |

The grep found no `Symbol`-keyed container reaching the emitted SIR in iteration order (it checked
the iteration methods on the named maps above, not every `Map[Symbol` in the plugin), so a plugin-level
cross-JVM compile test is not warranted on this evidence. It would cost a scalac start per
child JVM; revisit if the plugin ever iterates over symbols to emit bindings.
