# Scalus codegen improvement plan (from the Aiken/Plutus comparison)

**Audience note:** this document is written to be handed to an AI coding agent
(or engineer) working *in the Scalus repository* with no other context. It is
self-contained: benchmark provenance, measured results, root-cause analysis,
and an implementation-ready task list. Scalus source references were verified
against `master` commit `f05f9973e` (2026-08-03); section 4 records what
already exists so tasks target real gaps. Aiken references are to
`aiken-lang/aiken` tag `v1.1.23`.

## 1. Provenance: how these numbers were produced

The Binocular project (a Bitcoin oracle for Cardano, `lantr-io/binocular`,
branch `aiken-rewrite`) contains two production Plutus V3 validators written
in Scalus, and a line-by-line Aiken port of both:

- **Oracle** (`BitcoinValidator`): large validator - Bitcoin consensus
  validation (PoW, difficulty retarget, MTP), a recursive fork-tree datum
  (`ChainState`/`ForkTree`), Merkle Patricia Forestry (MPF) root updates.
  Its datum is big and recursive.
- **TM** (`TreasuryMovementValidator`): small validator - raw Bitcoin tx
  parsing (bytestring slicing), MPF membership proof, NFT/datum continuity
  checks. Its datum is a few flat fields.

Both implementations pass the identical ScriptContext fixtures (66 shared
UPLC tests). ExUnits were recorded per test; per-builtin CEK profiles were
captured with Scalus's profiling CEK. Both sides measured trace-free
(`aiken build` default; Scalus `Options.release`). Scalus was measured at two
lowering targets: vanRossem (PV11, its default) and Plomin (PV10, matching
what Aiken can emit). Fees use mainnet protocol parameters.

## 2. Comparison results

### Script sizes (CBOR, params applied)

| Script | Scalus @PV11 | Scalus @PV10 | Aiken v1.1.23 |
|---|---:|---:|---:|
| Oracle | **7,387 B** | 8,749 B | 9,497 B |
| TM | **3,700 B** | 4,363 B | 6,251 B |

### Real transaction fee (oracle update scenario, mainnet params)

| Scalus @PV11 | Scalus @PV10 | Aiken |
|---:|---:|---:|
| **829,441** | 930,660 | 929,489 |

At PV10 parity the fee is a dead heat (-0.1%); Scalus's PV11 lowering wins it
by 12.1%.

### ExUnits (CPU) by workload, PV10 parity (like-for-like compilers)

| Workload | Winner | Margin |
|---|---|---|
| TM validator paths (small flat datum) | **Aiken** | -2% to -38% CPU |
| TM Confirm path (bytestring-builtin dominated) | tie | within 2% |
| Oracle paths (large recursive datum) | **Scalus** | Aiken +15% to +53% CPU |

At PV11, Scalus wins essentially every row on both validators.

### Root causes (from per-builtin CEK profiles)

1. **Aiken's TM win is machine-step economy, not builtins.** On the TM mint
   path, builtin CPU is identical (27.6M vs 27.7M) while Aiken uses 15% fewer
   machine steps (26.8M vs 31.5M). Its shrinker produces leaner term
   structure, and its recursion avoids fixpoint-combinator overhead.
2. **Scalus's oracle win is representation strategy.** Aiken's `expect`
   deep-validates the entire nested datum upfront, then re-walks fields via
   exposer functions: on the oracle SetState path Aiken executes ~2x the Data
   traversal (137 vs 75 `tailList`, 76 vs 40 `unConstrData`). Scalus's
   `fromData` is a no-op retag; fields are projected on demand with cached
   `tailList`-chain bindings.
3. **Scalus prelude idioms cost extra builtins.** `===` on ledger values
   compares whole Data trees (`equalsData` is size-proportional);
   `SortedMap`/`Value` combinators executed 2x `equalsData` and ~50% more
   `ifThenElse`/`equalsInteger` than Aiken's direct walks on the TM GC path.
4. **Aiken's inliner reorders effects.** Single-use bindings are sunk to
   their use site even past other checks: on failure paths that skipped an
   expensive state computation entirely (-61% CPU). Scalus preserves source
   evaluation order.
5. **PV targeting is Scalus's structural advantage.** Aiken emits one fixed
   PV9/10-era dialect (booleans via `force ifThenElse`; no flexible `case`,
   no `dropList` - its own `uplc decode` rejects builtin 88). Scalus
   parameterizes lowering by target PV and already exploits PV11 flexible
   `case` (booleans/integers/lists/pairs/Data) and `dropList`, worth ~15% CPU
   and ~1.4 KB on the oracle.

## 3. Technique matrix

| Technique | Aiken v1.1.23 | Scalus 1.0.0 | Plutus (PIR/UPLC) |
|---|---|---|---|
| User data representation | PlutusData Constr + exposers, primitives unwrapped | PlutusData-backed, representation-polymorphic lowering, no-op to/fromData | Scott or SOPs (CIP-85) |
| Datum/redeemer boundary | strict deep `expect` validation | lazy trust + on-demand projection | strict `unsafeFromBuiltinData` decode |
| Field access caching | per-clause field binds | deterministic cached tail/drop chains | n/a (decoded values) |
| Recursion | self-application + static-argument transformation | self-application + static-argument transformation (T2, T1) | fixpoint combinators / SOP-era letrec |
| Optimizer driver | fixpoint loop until node count stable | fixed iterations (3x inline, 2x CSE) | fixed pass pipeline, 12 iterations of simplifier |
| Multi-arg application | `case (constr 0 [...])` via split_body_lambda | `CaseConstrApply` | SOPs / apply chains |
| Repeated builtin partial apps | `builtin_curry_reducer` hoisting | generic CSE only | CSE |
| Force hoisting of builtins | yes (`builtin_force_reducer`) | yes (`ForcedBuiltinsExtractor`) | yes (in code gen) |
| Effect reordering for perf | yes (single-use inlining) | no (order-preserving) | no (conservative; purity-aware floating) |
| PV-targeted lowering | no (single dialect) | yes (PV10/PV11 gates) | partial (SOPs gated on PV9+) |
| Compile-time evaluation | `builtin_eval_reducer` (CEK, constants) | `PartialEvaluator` (CEK, budget-capped) | `EvaluateBuiltins` |
| Match compilation | Maranget decision trees | per-typegen match lowering | `CaseReduce`/`KnownCon` + UPLC case-of-case |

Plutus column notes: boundary strategy is strict decode by default but the
`AsData` library keeps values Data-represented (Scalus's default is
effectively "AsData everywhere"); optimizer runs up to 12 capped simplifier
iterations at PIR and UPLC levels, then one-shot final passes.

## 4. What already exists in Scalus (verified 2026-08-03)

The original draft assumed several gaps that turned out not to exist.
Everything below was verified against `master` (`f05f9973e`). Tasks in
section 5 build on this baseline.

### 4.1 Benchmark suites

**nofib ports** (budget-pinned, run in every normal `jvm/test` / CI run):

- `scalus-examples/jvm/src/test/scala/scalus/benchmarks/KnightsTest.scala` -
  Knights tour with `@UplcRepr(UplcConstr)` representation; 4x4/6x6/8x8;
  budgets asserted via `assertBudgetClose` with 5% tolerance (see 4.4).
- `scalus-examples/jvm/src/test/scala/scalus/benchmarks/knightsdata/KnightsDataTest.scala` -
  same algorithm in default Data representation; exact-equality pins via
  `ScalaCompilerVersion.baseline(pre38, since38)`.
- `scalus-examples/jvm/src/test/scala/scalus/benchmarks/ClausifyTest.scala` -
  Clausify F1-F5; exact-equality pins.
- Every suite also calls `compareBudgetWithReferenceValue`
  (`scalus-testkit/.../scalus/testing/kit/ScalusTest.scala:249-272`) against
  a hardcoded `refBudget` - print-only, never asserts. **The provenance of
  those reference numbers is documented nowhere** (presumably Plinth
  plutus-benchmark figures).
- Missing from the Aiken/plutus-benchmark nofib set: **primes, queens**.

**UPLC-CAPE** (submission pipeline already built and documented):

- Harnesses: `scalus-examples/jvm/src/test/scala/scalus/examples/cape/`
  `{factorial/FactorialCapeTest, fibonacci/FibonacciCapeTest,
  twopartyescrow/TwoPartyEscrowCapeTest}.scala`. They parse the upstream
  `cape-tests.json` fixtures (66 cases total) and pin script sizes, exact
  ExUnits, and mainnet fees.
- Submission generators + 9-step runbook:
  `scalus-examples/jvm/src/main/scala/scalus/examples/cape/CAPE-SUBMISSION.md`.
  Scenarios done: factorial, factorial_naive_recursion, fibonacci,
  fibonacci_naive_recursion, two_party_escrow. Not implemented: `ecd`.

**JMH** (`bench/` module; compiled but **never run in CI**):

- `CekJVMBenchmark` (CEK wall-clock over auction `.flat` scripts, mirrors
  Plutus `validation` benchmarks), `JITBenchmark` (3 JIT strategies),
  `PlutusScriptEvaluatorBenchmark` (real mainnet blocks), `PolyBenchmark`.
- sbt aliases `benchmark`, `benchmark-cek`, `benchmark-jit`,
  `benchmark-hybrid` (`build.sbt:1032-1044`). The only checked-in snapshot
  is `bench/last-bench-result.txt` (JMH wall-clock, not ExUnits).

**Other perf suites:** `scalus-examples/.../examples/setbench/` (MPF vs
accumulator budget comparisons, incl. per-builtin cost breakdown in
`MpfCostAnalysisTest`), `PlutusConformanceTest` (exact budget assertions
against the plutus-conformance corpus), `PlutusUseCasesBenchmarkTest`
(89-script CEK correctness corpus).

### 4.2 Budget maintenance machinery

- Budget pins live as `ExUnits(...)` literals in test sources, selected per
  Scala compiler generation via `ScalaCompilerVersion.baseline` (testkit).
- `scripts/update-budgets.py` re-runs `sbtn quick`, parses budget-mismatch
  failures, and rewrites the expected literals across 7 test directories
  (up to 25 iterations until green; `--dry` supported).
- A budget regression today = a red `jvm/test`. But there is **no time
  series, no CI artifact, no diffable per-commit snapshot**.

### 4.3 Profiling CEK (producer side complete)

- Entry: `Term.evaluateProfile` (`scalus-core/.../uplc/Term.scala:403`).
- `ProfilingData` carries per-source-location, **per-builtin**
  (`byFunction`), and per-location-per-builtin cost breakdowns.
- `ProfileFormatter`: `toText`/`summary`/`toCsv`/`toJson` (with
  `JsonSchemaVersion = 1`)/`toHtml` (self-contained report with hot-path
  tree and annotated source).
- `ProfileReportWriter` writes report sets and maintains
  `profile-manifest.json` (schema-versioned, keyed by script hash +
  redeemer). Env-driven config via `SCALUS_PROFILE`/`SCALUS_DUMP`
  (`EvaluatorReportConfig.scala`).
- What is missing is only the **CI job** that captures and diffs these.

### 4.4 Known instability affecting benchmarks

CSE tie-breaking depends on Scala-compiler symbol IDs embedded in `Term`
names (`CommonSubexpressionElimination.scala:169`). Incremental recompiles
shift IDs, producing structurally different UPLC with ~0.05% cost deltas -
which is why `KnightsTest` uses a 5% `assertBudgetClose` tolerance instead
of exact pins. Making the tie-break stable would let all pins be exact.

### 4.5 Optimizer pass inventory

`scalus-core/shared/src/main/scala/scalus/uplc/transform/`:

| Pass | Role |
|---|---|
| `Inliner.scala` | beta-reduction, small-value/identity inlining, dead-arg elimination, adjacent `Force(Delay(t)) -> t` (`:285-291`), calls `PartialEvaluator` |
| `EtaReduce.scala` | `λx. f x -> f` when pure and x not free in f |
| `CommonSubexpressionElimination.scala` | 3-pass path-based CSE (ported from Plutus) |
| `CommonContextExtraction.scala` | CCE - hoists common contexts (one-hole subtrees); **off by default** (`Options.cceEnabled = false`) |
| `CaseConstrApply.scala` | 3+-arg apply chains -> `case (constr 0 [args]) [f]`; final pass |
| `ForcedBuiltinsExtractor.scala` | hoists repeated `force (builtin f)` chains to top-level bindings |
| `StrictIf.scala` | strict `ifThenElse` when both branches safe (removes delay/force) |
| `PartialEvaluator.scala` | budget-capped CEK constant folding on closed, trace-free subterms |
| `TermAnalysis.scala` | purity/free-vars analysis (not a pass) |
| `PrepareForSerialization.scala` | BLS constant rewrite pre-flat-encoding (not in pipelines) |

Pipelines (`OptimizerPipelines.scala`): `V3Optimizer` (`:27-61`) = 3x
(EtaReduce |> Inliner), StrictIf, ForcedBuiltinsExtractor; then
`cseIterations` (default 2) x (CSE |> Inliner); then optional CCE |> Inliner;
then CaseConstrApply. `V1V2Optimizer` (`:7-25`) = the first phase only.
Custom passes can be appended via `Options.uplcOptimizers`.

### 4.6 PV11 features already wired

**CIP-153 Value builtins** are declared end-to-end: UPLC enum
(`uplc/DefaultFun.scala:1341-1403`), meanings (`uplc/Builtin.scala:1277-1379`),
implementations (`uplc/eval/BuiltinValueOps.scala`), SIR level
(`compiler/sir/SIRBuiltins.scala:597-643`), user API
(`uplc/builtin/Builtins.scala:1375-1451`), PV availability
(`cardano/ledger/Builtins.scala:170-187`, `Batches.batch6`).
`SIRType.BuiltinValue` exists, and `BuiltinValueSirTypeGenerator`
(`lowering/typegens/PrimitiveSirTypeGenerators.scala:913-945`) already uses
`valueData`/`unValueData` for representation conversion. **But
`insertCoin`/`lookupCoin`/`unionValue`/`valueContains`/`scaleValue` have zero
consumers** - the on-chain prelude `Value`
(`cardano/onchain/plutus/v1/Value.scala`) is still the AssocMap-of-AssocMap
encoding. That is the T7 gap.

**CIP-156 arrays** are wired end-to-end: `BuiltinArray[A]` user type +
`lengthOfArray`/`listToArray`/`indexArray` builtins, plugin support
(`SIRCompiler.scala:2512+`), `ProdBuiltinArrayEmitter` typegen,
`ScalusRuntime.arrayToList`, and the `@UplcRepr(BuiltinArray)` annotation
(`compiler/annotations.scala:90`). Selection is manual - the T12 gap is
measurement + auto-selection, not plumbing.

### 4.7 Confirmed absent (the actual work)

- ~~Static-argument transformation; self-application recursion encoding (all
  recursion goes through the shared Z combinator).~~ Both landed: T2
  (self-application, incl. mutual recursion) and T1
  (`compiler/sir/StaticArgumentTransformation.scala`, gated on
  `optimizeUplc`).
- `FloatDelay`, `ForceCaseDelay`, `CaseOfCase`, `CaseReduce`/`KnownCon`
  passes - zero hits in any `.scala` file; only the adjacent
  `Force(Delay(_))` cancellation in `Inliner` and `StrictIf` exist.
- Any `relaxedEvaluationOrder`-style option in `Options`
  (`compiler/compiler.scala:8-41`).
- Per-commit ExUnits/profile tracking in CI (no workflow touches
  bench/budget/profile).
- nofib primes + queens; CAPE `ecd` scenario.

## 5. Task list: Scalus codegen/optimizer improvements

Ordered by expected impact / effort ratio. Each task cites the measured
evidence, the Aiken/Plutus prior art, the Scalus code area, and a validation
method. Tasks T1-T12 come from the Aiken comparison and the measured
evidence; T13-T16 (end of section 6) come from the Plutus research.
Suggested implementation order: T11 (close the measurement gaps first),
then T2 (done), T1 (done), T3, T13, T4, T5, T7, T14, T15, T8, T6, T9, then the research
tracks T10, T12, T16.

### T1. Static-argument transformation for recursive functions (HIGH, DONE)

- **Evidence:** TM byte-parsing loops (varint walks, outpoint/output loops)
  re-pass the invariant `rawTx`/params on every recursive call through the Z
  combinator; Aiken lifts loop-invariant parameters out
  (`recursive_nonstatic_params`, `gen_uplc.rs:3243-3267`, applied at
  4461-4494) and this shows up as ~15% machine-step advantage on parsing
  paths with identical builtin work.
- **What landed:** a SIR-to-SIR pass
  `compiler/sir/StaticArgumentTransformation.scala` (sibling of
  `MutualRecursionElimination` / `RemoveRecursivity`). For a single-binding
  `let rec f = λp1..pn. body` it marks position `i` static when *every*
  self-call passes exactly `Var(pi)` there, then rewrites to
  `let f = λp1..pn. (let rec f$sat = λq1..qk. body' in f$sat q1..qk)`,
  where `q` are the changing params. When the static params form a prefix
  (the common `go f n acc` case) it emits the leaner
  `let f = λp1..pj. (let rec f$sat = λq1..qk. body' in f$sat)`, which also
  lets a partially applied `f static...` share one fixpoint across calls.
  Self-calls are detected through both `Var` and `ExternalVar`, shadow-aware.
  Skipped when unprovable: multi-binding (mutual) groups, lazy lets,
  non-lambda rhs, duplicate param names, any bare or under-saturated
  self-reference, or no static param. If all params are static the last is
  demoted (a nullary strict letrec diverges).
- **Gating:** applied only when `options.optimizeUplc` is set, in the single
  SIR->UPLC pipeline
  `compiler/sir/lowering/UplcPipeline.scala` `run`. Every entry point
  (`uplc/Compiled.scala` `toUplc`, `scalus/package.scala` `toUplc` /
  `toUplcOptimized` / `lowerToUplc`) delegates to it.
- **Measured:** the proof-record in
  `scalus-core/jvm/.../uplc/eval/ExprSizeAndBudgetTest.scala` pins
  **96,000 cpu / 600 mem saved per recursive call for 2 lifted arguments**
  (3 machine steps per argument no longer re-passed), obtained by
  differencing two loop lengths so the fixed entry cost cancels. Corpus
  effects: Knights 8x8 (Data repr) -10.4% mem / -3.7% cpu, Knights 6x6
  -8.1% / -3.2%, G2Accumulator -13.7% / -10.3%, prelude `List` operations
  commonly -16% to -29%, CAPE two-party-escrow -1.5% to -3.5%. A handful of
  validators regress slightly (functions entered often but iterating few
  times pay the wrapper) - the inherent SAT trade-off, same as Aiken's.
- **Mutual recursion - resolved 2026-08-16:** the unified `UplcPipeline` runs
  SAT after `MutualRecursionElimination`, so peers-as-params arguments are
  lifted. MRE's `selfChain` passes `fip f1 .. f(i-1)` (the peer's own params)
  on every self-call of a `$mutrec` peer, which is a textbook static
  argument; SAT now sees that shape and hoists it. `MutualRecursionElimination`
  is idempotent (it only rewrites `isRec && bindings.size >= 2`, and emits
  single-binding lets), so the backends' own second call is a no-op traversal
  and no backend constructor signature changed (no MiMa filters). Note the
  lift only fires for peers that are *also* self-recursive - a purely
  cross-recursive member's self-reference is a bare argument, which SAT
  refuses. Probed 2026-08-16: six prelude-heavy programs (List sort/fold,
  SortedMap, Value arithmetic, Data, nested flatten, quantityOf) produce
  **zero** multi-binding recursive Lets, so the measured win on today's
  corpus is nil; it matters for validators with top-level mutual recursion
  (recursive-descent parsers, mutually recursive tree walks). Guarded by
  `compiler/sir/lowering/UplcPipelineTest."optimized mutual recursion gets
  its peer parameters lifted"`.
- **Known limitation (still open):** runtime helpers built directly with
  `lvLetRec` (e.g. `ScalusRuntime.genArrayToList`, which re-passes an
  invariant `arr` and `n`) are out of scope - they are constructed at the
  lowered-value level, after SIR passes have run; `genMapList` is already
  hand-SAT'd and is the model for the shape this pass produces.
- **Tests:** `compiler/sir/StaticArgumentTransformationTest.scala` (17 cases:
  both shapes, edge cases, `ExternalVar`, polymorphism, optimizeUplc gating),
  plus the existing T2 guard
  `SelfApplicationRecursionTest."optimizer can still constant-fold closed
  recursive computations"`, which stays green.

### T2. Cheaper recursion encoding (HIGH, DONE)

- **Evidence:** every Scalus recursive call routes through the shared Z
  combinator `λff.(λxx.ff (λvv.xx xx vv)) (...)` (`uplc/Expr.scala:46-49`,
  bound at program root when `LoweringContext.zCombinatorNeeded` is set,
  `SirToUplcV3Lowering.scala:113`) - two extra applications + one lambda
  alloc per call versus Aiken's direct self-application (`f f args`,
  `gen_uplc.rs:4450-4459`). Note: the flag is set from `Lowering.scala:554`
  and from several `ScalusRuntime.scala` runtime bindings; the legacy simple
  backend has a parallel binding site
  (`lowering/simple/BaseSimpleLowering.scala:49`) that any rewrite must
  cover or explicitly exclude.
- **What:** lower single self-recursion as self-application: define
  `f' = λself.λargs. body[f := self self]` and call `f' f'`. No Z, no eta
  wrapper. Mutual recursion is supported since this change: `SIRLinker`
  groups SCCs into multi-binding rec Lets and `MutualRecursionElimination`
  rewrites them to nested single lets (peers-as-params, one fixpoint per
  group), lowered by the self-application encoding on all backends. Local
  (in-block) mutual recursion reports a clear error.
- **Validate:** microbenchmark a tight counting loop: per-iteration cost
  should drop by the Z-dispatch delta; all existing tests green.

### T3. Optimizer fixpoint driver + pass ordering study (MEDIUM, cheap)

- **Evidence:** Aiken repeats its multi-pass until the term node count stops
  shrinking (`optimize_repeatedly`, `optimize.rs:25-37`); Scalus runs fixed
  iteration counts (see 4.5 for the exact `V3Optimizer` phase list,
  `OptimizerPipelines.scala:27-61`). Missed late-exposed redexes remain.
- **What:** wrap `V3Optimizer` phases in a node-count fixpoint loop with an
  iteration cap; measure compile-time cost; keep flag to restore fixed mode.
  While in there: (a) evaluate enabling `CommonContextExtraction` by default
  (pass exists, `Options.cceEnabled = false`); (b) fix the CSE tie-break
  instability (4.4) so budget pins can be exact.
- **Validate:** compare optimized-term sizes and ExUnits across a corpus
  (Binocular validators are a good corpus) before/after; expect small wins,
  occasionally large ones.

### T4. Builtin curry/partial-application hoisting (MEDIUM)

- **Evidence:** Aiken's `builtin_curry_reducer` (`shrinker.rs:2557`) hoists
  repeated 2-arg builtin applications sharing a first argument into one
  binding, and canonicalizes commutative builtins constant-first
  (`convert_arithmetic_ops`/`flip_constants` rewrite `subtractInteger x c`
  to `addInteger x (-c)` to expose sharing). Scalus's generic CSE catches
  identical whole expressions but not the curried-prefix sharing pattern.
- **What:** add a UPLC pass in `scalus/uplc/transform/` that (a) normalizes
  commutative builtin argument order (constants first), (b) rewrites
  `subtractInteger x c` -> `addInteger x (-c)`, (c) hoists repeated
  `[(builtin f) X]` prefixes into shared bindings when applied 2+ times.
- **Validate:** term-size drop on validators using repeated
  `equalsInteger(tag, _)` dispatch and repeated `quantityOf`-style lookups.

### T5. Let-chain regrouping into case-constr headers (MEDIUM)

- **Evidence:** Aiken's `split_body_lambda` (`shrinker.rs:1520, 2773`)
  merges nested single-argument let-applications into one multi-parameter
  lambda applied via `case (constr 0 [defs...])` - the cheap V3 application
  encoding; the shrinker comment calls it "the ultimate function when used in
  conjunction with case_constr_apply". Scalus's `CaseConstrApply`
  (`uplc/transform/CaseConstrApply.scala`, the final `V3Optimizer` pass)
  only rewrites already-saturated 3+-arg application chains.
- **What:** extend `CaseConstrApply.scala` (or add a companion pass) to
  regroup chains of `[(lam x1 [(lam x2 body) e2]) e1]` into
  `case (constr 0 [e1, e2, ...]) [(lam x1 x2 ... body)]`, respecting
  dependency order and effect ordering between the bound terms.
- **Validate:** machine-step (Apply/Lam) count reduction on any validator;
  compare against Aiken's header structure in decompiled output.

### T6. Opt-in relaxed evaluation order (single-use let sinking) (MEDIUM)

- **Evidence:** Aiken's `inline_reducer` (`shrinker.rs:1877`) sinks
  single-use bindings to the use site even past other computations; measured
  effect: failure paths skipped an expensive unused computation entirely,
  -61% CPU on some Binocular negative-path tests. Scalus deliberately
  preserves source evaluation order.
- **What:** add `Options.relaxedEvaluationOrder` (default off; no such flag
  exists today - `compiler/compiler.scala:8-41`). When on, the Inliner may
  sink single-use, non-trivial bindings to their unique use site even when
  other effects (potential errors, traces) intervene. Document the
  semantics change (error identity/timing, cost of failing paths).
- **Validate:** failure-path ExUnits drop on validators computing state
  before cheap guards; success paths unchanged.

### T7. Lower prelude Value/SortedMap through PV11 batch-6 builtins (HIGH for PV11, PHASE 1 DONE)

- **Phase 1 landed 2026-08-18** (commits 95c6cf660..c92fd0181): `ValueIntrinsicsV11`
  lowers `Value.quantityOf`/`plus`/`minus`/`multiply`/`negate` and the new
  `Value.containsAtLeast` to `lookupCoin`/`unionValue`/`scaleValue`/`valueContains`
  at PV11, gated on `Options.valueBuiltins` (default true; registry `minPV = 11`,
  silent fallback to the linked SIR body at PV10 or flag-off). Measured ~13-75x
  cheaper per operation; conversions amortize at 1 call (cpu) / 0 (mem). Strict
  CIP-153 validation (canonical form, signed 128-bit range) is a documented
  behavior change. Design:
  `docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md`;
  measurements pinned in `scalus-core/jvm/.../uplc/eval/ValueBuiltinsBudgetTest.scala`.
- **Phase 2 (open):** a `BuiltinValueBacked` representation so chained ops stay
  native between calls (today each op pays a `valueData`/`unValueData` roundtrip),
  per the design doc's sketch. Also open: `interpretReprSIR` has no
  `ProductCaseOneElement` case (latent trap for future intrinsics; documented at
  the cast site in `compiler/intrinsics/ValueIntrinsics.scala`).

- **Evidence:** TM GC profile: Scalus executed 2x `equalsData` (2.5M CPU) and
  29-vs-19 `ifThenElse`, 18-vs-10 `equalsInteger` against Aiken's direct
  walks - prelude `SortedMap`/`Value` combinators and `===`-style Data
  equality are builtin-hungry. The CIP-153 builtins are fully declared and
  implemented (see 4.6) but only `valueData`/`unValueData` are consumed by
  lowering; `insertCoin`/`lookupCoin`/`unionValue`/`valueContains`/
  `scaleValue` have zero consumers, and the prelude `Value` is still the
  AssocMap-of-AssocMap encoding.
- **What:** when `targetProtocolVersion >= vanRossemPV`, lower
  `Value.quantityOf` -> `lookupCoin`, value add/subtract -> `unionValue`,
  containment -> `valueContains`, to/from Data -> `valueData`/`unValueData`,
  etc.; keep the portable lowering for PV10. Relevant machinery:
  `lowering/typegens/` (`BuiltinValueSirTypeGenerator` in
  `PrimitiveSirTypeGenerators.scala:913-945`, `PackedDataMapEmitter`,
  `SumPairBuiltinListEmitter`, type-name dispatch in
  `SirTypeUplcGenerator.scala:437-438`) and the SortedMap/AssocMap intrinsic
  wiring in `lowering/IntrinsicResolver.scala`.
- **Validate:** ExUnits on Value-heavy paths (NFT preservation checks,
  quantity lookups); expect large constant-factor wins and smaller code.

### T8. Cheap structural-equality specialization (MEDIUM)

- **Evidence:** `===` on ledger values lowers to whole-tree `equalsData`;
  on the GC path this was the single most expensive builtin call site.
  Often the compared values differ in one known field, or only a small
  projection needs comparing.
- **What:** (a) audit prelude/onchain stdlib for `===`-on-big-values
  idioms and provide targeted comparators (e.g. compare credential bytes,
  not whole Address Data); (b) consider a SIR-level rewrite: equality on a
  freshly-constructed value vs a decoded one can compare fields directly
  instead of serializing to Data.
- **Validate:** GC-path profile: `equalsData` count/cost drop.

### T9. Derived deep-validation combinator (datum safety parity) (LOW effort, LIB)

- **Evidence:** Aiken's `expect` gives free deep structural validation of
  datums (checked decode via `chooseData` dispatch,
  `builder.rs:523/583-670`); Scalus trusts the declared type, so
  malformed-but-well-typed datums must be rejected by hand-written checks
  (Binocular's SetState branch does this manually today).
- **What:** derive a `validate[T]: Data => Unit` (or `FromData` variant
  `strictFromData[T]`) that generates the same deep check Aiken produces,
  as an opt-in - keeping the lazy default. Document when to use it (datums
  crossing trust boundaries).
- **Validate:** unit tests with malformed Data; cost measured and documented.

### T10. Match-compilation upgrade: decision trees (RESEARCH then MEDIUM)

- **Evidence:** Aiken compiles `when` through Maranget-style decision trees
  (`decision_tree.rs`, since v1.1.6); Scalus lowers matches per-typegen with
  tag dispatch. For nested/overlapping patterns decision trees avoid repeated
  scrutinee decoding and repeated tag tests.
- **What:** prototype decision-tree compilation for SIR `Match` with nested
  patterns; measure on wide-enum and nested-pattern code. May be neutral for
  the flat matches typical of validators - measure before committing.
- **Validate:** synthetic nested-match benchmark + existing test corpus.

### T11. Benchmark and regression infrastructure - close the gaps (LOW effort, HIGH leverage)

Much of the originally planned work already exists (sections 4.1-4.3):
Clausify + Knights (both representations) are ported and budget-pinned,
three UPLC-CAPE scenarios are submitted with a documented runbook, and the
profiling CEK already emits schema-versioned per-builtin JSON. The
remaining gaps, in priority order:

- **(a) Per-commit ExUnits tracking - the real gap.** Budgets live as
  literals in test sources (refreshed by hand via
  `scripts/update-budgets.py`); a regression is only a red build, never a
  diff. Add a CI job (or a `Benchmark`-tagged suite + workflow) that runs
  the benchmark corpus, writes ExUnits + per-builtin profile JSON
  (producer exists, 4.3) to a snapshot file, and diffs it against a
  committed baseline so a pass regression shows as a per-builtin diff, not
  just a total. `MpfCostAnalysisTest` already demonstrates the
  per-builtin-breakdown pattern in-repo.
- **(b) Complete the nofib set:** port **primes** and **queens** (from
  `aiken/benchmarks/` / IntersectMBO plutus-benchmark), following the
  KnightsTest/KnightsDataTest two-representation pattern.
- **(c) Document `refBudget` provenance** for
  `compareBudgetWithReferenceValue` call sites (which Plutus/Plinth version
  produced them, how to regenerate) - currently stated nowhere.
- **(d) CAPE:** implement the `ecd` scenario; keep submissions current with
  each release (runbook: `CAPE-SUBMISSION.md`).
- **(e) Fix the CSE tie-break instability** (4.4) so `KnightsTest` can drop
  its 5% tolerance and all pins become exact.

**Why:** every Aiken shrinker improvement (10-20% claims in their
changelog) landed with this style of self-benchmarking; Scalus's optimizer
work (T1-T8, T13-T15) needs the same feedback loop before any pass lands.

### T12. PV11 arrays for fixed-shape data (RESEARCH)

- **Evidence:** PV11 adds `listToArray`/`indexArray`/`lengthOfArray`, and
  Scalus has the machinery wired end-to-end (see 4.6: `BuiltinArray[A]`,
  plugin support, `ProdBuiltinArrayEmitter`, `@UplcRepr(BuiltinArray)`) -
  but nothing selects it automatically and there are no measurements.
  Validators with fixed-size collections (Binocular's 11-timestamp MTP
  window; the sorted-insert median) do repeated positional access where
  arrays beat linked lists.
- **What:** measure `List` vs `BuiltinArray` for index-heavy access patterns
  at PV11; if it wins, document the `@UplcRepr(BuiltinArray)` recipe and
  consider auto-selecting arrays for fixed-arity product fields.

## 6. Ideas from the Plutus (PIR/UPLC) compiler

Verified against IntersectMBO/plutus (master, 2026-08). Pipeline: Haskell ->
GHC Core -> PIR (typed, up to 12 simplifier iterations) -> TPLC -> UPLC (up
to 12 simplifier iterations + one-shot final passes).

**PIR passes** (`plutus-ir/src/PlutusIR/Transform/`): Unwrap, CaseReduce
(case of literal constr), KnownCon (case of variable bound to a known
constructor), Beta (applied lambda -> strict let), StrictifyBindings (with a
real strictness analysis), EvaluateBuiltins, Inline (+ CallSiteInline),
RewriteRules (peepholes incl. CommuteFnWithConst - constant to canonical
side of commutative builtins; UnConstrConstrData; RemoveTrace), DeadCase,
CollapseCase (N consecutive list-casings with unused heads -> one
`case (drop N xs)`), LetFloatOut, RecSplit (minimal SCCs of letrec groups),
**RecInline** (inlines non-root, non-self-recursive helpers of a letrec
group - their 2026 addition), LetMerge, LetFloatIn (with relaxed mode),
DeadCode, ThunkRecursions, NonStrict.

**UPLC passes** (`untyped-plutus-core/src/UntypedPlutusCore/Transform/`), per
iteration: **FloatDelay** (moves `delay` from an argument into the body when
all uses are forced - strictly better than Plutonomy's Split Delay per its
header), **ForceCaseDelay** (`force (case s [delay t_i...])` -> strip both),
LetFloatOut, ForceDelay (cancels through intervening lambdas/apps),
**CaseOfCase** (pushes an outer case into `ifThenElse`/case branches that
produce known constrs), CaseReduce, Inline, EvaluateBuiltins. After the
loop, one-shot: CSE (up to 4 interleaved iterations, `ExcludeWorkFree`
subterm policy), **PolyBuiltin** (hoists fully-forced polymorphic builtins),
**ApplyToCase** (3+-arg applications -> `case (constr 0 [...]) [f]`,
deliberately run LAST because it destroys other opportunities - ordering
wisdom Scalus already follows: `CaseConstrApply` is the final `V3Optimizer`
pass).

**Inliner design**: modeled on "Secrets of the GHC Inliner"; unconditional
inlining (trivial/pure/single-occurrence, growth cap
`inline-unconditional-growth`=1) plus callsite inlining of fully-applied
functions gated on AST-size growth (`inline-callsite-growth`=5) and
effect-safety; GHC `INLINE` pragmas become always-inline hints; manual
`PlutusTx.Optimize.Inline.inline` at call sites; `peel`/`unroll` TH
combinators for bounded recursion unrolling.

**Encodings**: `DatatypeStyle = ScottEncoding | SumsOfProducts` (SOP default
since PV9/CIP-85; nofib gains 11-27%); the `AsData` library strategy keeps
values Data-encoded to avoid boundary decode - i.e. the strategy Scalus uses
by default, which Plutus documents as ~3x slower than Scott *for
compute-heavy manipulation* (CIP-85 motivation) - important nuance: lazy
Data wins at boundaries, loses in hot loops over decoded structure.

**Published impact**: constant folding + polymorphic-builtin hoisting +
inliner tuning: ">10% average execution-cost savings" on mainnet scripts
(dev updates 2026-04/05); SOP: 11-27% on nofib. **Roadmap**: PV11 builtin
`Value` (CIP-153: `insertCoin`/`lookupCoin`/`unionValue`/`scaleValue`/
`valueData`), arrays (CIP-138, `multiIndexArray` CIP-156 behind future PV),
flexible case on builtin Bool/Integer/Data, Agda-certified passes
(`certified-opts-only` flag), Plutus V4 script context.

### Additional tasks derived from Plutus

### T13. FloatDelay + ForceCaseDelay passes (MEDIUM)

- **Prior art:** UPLC `FloatDelay.hs`, `ForceCaseDelay.hs`. Neither exists
  in Scalus (verified, 4.7): the Inliner cancels only adjacent
  `Force(Delay(t))` (`Inliner.scala:285-291`); `StrictIf` handles the
  pure-if case. FloatDelay pushes a `delay` from an argument into the
  function body (all-uses-forced condition), unlocking further
  cancellations; ForceCaseDelay strips force/delay around case branches -
  directly relevant to Scalus's PV10 boolean/match lowering which is
  delay/force-heavy.
- **Where:** new passes in `scalus/uplc/transform/`, wired before Inliner in
  `OptimizerPipelines.scala`.
- **Validate:** delay/force node counts in optimized output; PV10 ExUnits.

### T14. Case-of-case + case-of-known-constructor at UPLC level (MEDIUM)

- **Prior art:** UPLC `CaseOfCase.hs` + `CaseReduce.hs`, PIR `KnownCon.hs`.
  None exist in Scalus (verified, 4.7). With Scalus emitting `Term.Case` at
  PV11 (booleans, tags, lists), an outer case over an `ifThenElse`/inner
  case of known constrs is reducible; Scalus currently relies on
  `PartialEvaluator` for closed terms only.
- **Where:** `scalus/uplc/transform/`; run inside the (new, T3) fixpoint loop.
- **Validate:** synthetic nested-match programs; PV11 validators' term size.

### T15. Size-budgeted callsite inlining + inline hints (MEDIUM)

- **Prior art:** Plutus `CallSiteInline.hs`: inline fully-applied functions
  when post-inline AST size <= pre-size + threshold (default 5), RHS cheap,
  args pure; AST-node count as the proxy for serialized size. Scalus's
  Inliner inlines small/beta-reducible terms but has no growth-budgeted
  callsite inlining and no user hint channel.
- **What:** add the size-budget heuristic to Scalus's Inliner; map a Scalus
  annotation (or Scala `inline` semantics at SIR level) to always-inline
  hints; consider `peel`/`unroll`-style bounded recursion unrolling helpers
  in the stdlib.
- **Validate:** corpus term sizes + ExUnits; no size regressions above the
  budget.

### T16. Usage-driven representation inference (RESEARCH, potentially large)

- **Prior art:** the Data-vs-Scott-vs-SOP tension: Plutus measures Data ~3x
  slower than Scott for heavy manipulation (CIP-85), yet Binocular measures
  Scalus's lazy-Data boundary beating Aiken's decode-everything by 15-53% on
  boundary-dominated code. Scalus already has both representations
  (`@UplcRepr(UplcConstr)` vs default Data-backed) - selection is manual,
  and the nofib Knights suite exists in both variants precisely because the
  choice matters (KnightsTest vs KnightsDataTest budgets differ 2-3x).
- **What:** infer per-type (or per-binding) representation from usage: values
  that cross the Data boundary once and are traversed repeatedly in hot
  recursive code should be decoded to SOP once; values touched sparsely stay
  lazy Data. Start with a heuristic (e.g. count of distinct field/match
  accesses per binding at SIR level), expose an override annotation.
- **Validate:** oracle-style (sparse access) AND fold-heavy (dense access)
  benchmarks must both stay at their current best or improve.

## 7. Further research recommendations

1. **Machine-step attribution.** The profiling CEK attributes builtin costs
   but not Var/Apply/Force/Case machine steps to source locations. Adding
   step attribution would turn "Aiken uses 15% fewer machine steps" into a
   per-function diff and make T2-T5 measurable individually.
2. **Neutral cross-compiler benchmarks.** No neutral, current Aiken-vs-Scalus
   numbers exist publicly (UPLC-CAPE has no Aiken submissions; MLabs
   uplc-benchmark published no numbers we could retrieve). Contributing both
   compilers' solutions to UPLC-CAPE fixtures would establish this.
3. **Boundary-strategy hybrid.** Measure a "validate once, then lazy" datum
   strategy (deep check + retag, keeping cached lazy projection) against both
   current strategies on large datums.
4. **Aiken PV11 trajectory.** Track whether Aiken adds a PV11 backend
   (flexible case, dropList); the current Scalus edge on deployables narrows
   when it does. Re-run this benchmark on each Aiken minor release.
5. **Evaluation-order semantics.** Before shipping T6, specify what Scalus
   guarantees about effect order (errors, traces, budget) - Aiken's silent
   reordering is a footgun worth not copying blindly.
6. **Certified passes.** Plutus is wiring an Agda certifier into its UPLC
   pipeline (per-pass traces, `certified-opts-only`). As Scalus's pass count
   grows (T3-T5, T13-T15), property-based pass equivalence testing (random
   UPLC terms, CEK-evaluate before/after with budget comparison) is the
   pragmatic version of the same idea and would catch pass bugs early.
   `TermAnalysisCekConformanceTest` (cross-checks static analysis against
   the CEK) is an existing seed for this style of testing.
7. **Track Plutus PV11+ builtin usage.** Plutus's own compiler will start
   emitting CIP-153 Value builtins and CIP-138 arrays; their pass/benchmark
   choices (published in the plutus-core dev updates) are a leading
   indicator for which Scalus lowerings (T7, T12) pay off most.

## 8. Raw evidence pointers (Binocular repo, branch `aiken-rewrite`)

- `docs/aiken-vs-scalus.md` - full per-test tables, Scalus @PV11 vs Aiken.
- `docs/aiken-vs-scalus-plomin.md` - same at PV10 parity.
- `docs/aiken-vs-scalus-codegen.md` - pipeline analysis + verdict.
- `src/test/scala/binocular/ValidatorBackend.scala` - swappable backends,
  PV/traces switches (`BINOCULAR_SCALUS_PV`, `BINOCULAR_SCALUS_TRACES`).
- `BINOCULAR_PROFILE=1 sbt test` - per-builtin CEK profiles to
  `target/perf/profiles/`.
- `sbt "Test/runMain binocular.DumpUplc"` + `aiken uplc decode <f> --cbor` -
  comparable decompiled UPLC for both compilers.
