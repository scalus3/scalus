# UPLC JIT simplification: design

Status: implemented, 2026-10-19 (steps 1-7); see "Step 7". Branch `worktree-jit-simplify`, worktree
`.claude/worktrees/jit-simplify`, based on `79ca316fb` (the end of the refactoring in
`docs/internal/JIT_REFACTORING_PLAN.md`).

## Goal

Find the simplest JIT that has exactly the semantics of the CEK machine at protocol version 11
and is still faster than the CEK machine. The JIT is experimental and is not used in production,
so simplicity and easy updates for new Cardano versions have priority over the last percent of
speed.

What it is: a staged JIT. Scalus compiles a UPLC script to JVM bytecode with Scala 3 staging when
the script is loaded, and the JVM's JIT compiles that bytecode to machine code.

The method: make the semantics equal by construction, put each optional optimization behind a
temporary flag, measure what each one gains, then delete the ones that do not pay for their code.

**Why:** the refactored JIT still diverges from the CEK machine (`mkCons` with a mismatched element
type succeeds), and it keeps two backends, about 50 hand-written builtins, a constant pool, let
inlining, an applied form and precompiled closures. No measurement shows which of these pay for
their complexity.

## Definitions

| Term | Meaning |
|---|---|
| CEK machine | `scalus.uplc.eval.CekMachine`, through `PlutusVM`: the reference semantics |
| JIT | the module `scalus-uplc-jit-compiler` |
| backend | one code generator of the JIT: nativestack (kept) or mincont (deleted) |
| `UplcValue` | the type of the value of a UPLC term in compiled code (section 1) |
| polymorphic builtin | a builtin whose `TypeScheme` has a type variable; there are 15 |
| monomorphic builtin | every other builtin |
| override | a hand-written implementation of a monomorphic builtin |
| bridge | `CekBridge`: runs a builtin through the CEK machine's own `BuiltinRuntime` |
| corpus | the scripts of section 4.1 |
| variant | one configuration of the JIT that the bench measures (in step 6, a combination of the since-deleted `JitOptions` flags) |
| CEK semantics | the result, the error or success, and the spent budget that the CEK machine gives |

## Scope

- The JIT MUST implement builtin semantics variant E. `[SC-1]`
- The JIT MUST implement the machines of protocol versions 11 (van Rossem) and 12 (Dijkstra). One
  compile MUST generate the code of one of them, the one chosen by its `protocolVersion`
  (`[EV-12]`). `[SC-2]`
- Code compiled for protocol version 11 MUST reject `case` on a `Data` scrutinee. Code compiled for
  12 MUST case on it as the CEK machine does at 12. `[SC-3]`
- The JIT MUST give CEK semantics for every term in scope. `[SC-4]`
- A difference from CEK semantics in result, error or budget is a defect. It MUST NOT be accepted as
  a known failure in a committed state. `[SC-5]`
- A failure MUST throw the exception class the CEK machine throws for it. A failing builtin MUST
  throw `BuiltinError` with the cause the CEK machine wraps. `[SC-6]`

**Why:** `[SC-3]` follows from `[SC-2]`: the CEK machine allows case on `Data` only from
Dijkstra (PV12) (`Cek.scala`, `CaseDataNotSupportedError`). The two machines differ only there. A
JIT generates code for one language version, so the protocol version is a compile-time input:
the generator emits the `case` dispatch of that version only (`JitRuntime.caseIndex` for 11,
`JitRuntime.caseIndexOnData` for 12), and the generated code never tests a flag. A run-time flag
would put a test of the protocol version in every `case` of every script, and keep code for both
machines in each. `[SC-5]` is the reason for this design: a JIT that accepts a script the chain
rejects is wrong, however fast it is.

*Example, illustrative.* `(case (con data (Constr 1 [I 7])) (con integer 0) (lam fs fs))`
compiled for 12 returns `(con (list data) [I 7])`; compiled for 11 it throws
`CaseDataNotSupportedError`, as the CEK machine of each version does.

Out of scope: renaming the module or package (`jit` to, for example, `staged`); PV12 features
other than case on `Data`; a shared term walk (`Term.foreachSubterm`).

## 1. Values

### 1.1 Representation

```scala
/** The value of a UPLC term in compiled code; mirrors `CekValue` case by case. */
type UplcValue = Constant | ConstrValue | ListValue | LamValue | DelayValue

final case class ConstrValue(tag: Long, args: List[UplcValue])   // CekValue.VConstr
final case class ListValue(elems: List[UplcValue])               // CekValue.VList

@FunctionalInterface trait LamValue   { def apply(arg: UplcValue): UplcValue }  // VLamAbs
@FunctionalInterface trait DelayValue { def apply(): UplcValue }                // VDelay
```

- Generated code MUST represent a constant as the `scalus.uplc.Constant` the CEK machine holds in
  `VCon`. `[VR-1]`
- A list constant MUST keep its element type (`Constant.List(elemType, elems)`), also when it is
  empty. `[VR-2]`
- Generated code MUST represent a `constr` value as `ConstrValue`. `[VR-3]`
- Generated code MUST represent a list of non-constants as `ListValue`, where the CEK machine has a
  `VList`. `[VR-4]`
- Generated code MUST represent a lambda as a `LamValue` and a delay as a `DelayValue`. `[VR-5]`
- A partly applied builtin MUST be a `LamValue` or a `DelayValue`. `[VR-6]`
- The JIT MUST NOT use any other runtime representation for a UPLC value. `[VR-7]`

**Why:** a raw `BigInt` or a plain Scala `List` drops the type the CEK machine checks. `mkCons`
compares the element type of the list with the type of the new element (`Builtin.scala:514-516`),
so an untyped empty list makes `mkCons-fail` succeed in the JIT. `ConstrValue` and `ListValue`
exist only because their fields can hold JIT functions, which no `CekValue` case can hold.
`LamValue` and `DelayValue` are interfaces, so generated lambdas still compile through
`LambdaMetafactory`, with no extra class and no wrapper per lambda.

*Example, illustrative.* `[(builtin mkNilData) (con unit ())]` gives
`Constant.List(DefaultUni.Data, Nil)`. `mkCons` of `(con integer 3)` onto it then fails, as in the
CEK machine.

### 1.2 Compiled term

```scala
@FunctionalInterface
trait CompiledTerm {
    def apply(logger: Logger, budget: BudgetSpender, params: MachineParams): UplcValue
}
```

- `jitUplc(term)` MUST return a `CompiledTerm`. `[CT-1]`
- One `CompiledTerm` MUST be safe to call from many threads at once. `[CT-2]`
- A `CompiledTerm` MUST throw `StackThresholdException` when the call depth reaches the limit. It
  MUST NOT fall back to the CEK machine itself. `[CT-3]`

## 2. Builtins

### 2.1 Three kinds

- The JIT MUST implement each of the 15 polymorphic builtins by hand. `[BI-1]`
- Each polymorphic builtin MUST match the same cases, throw the same errors and spend the same cost
  as its case in `Builtin.scala`. `[BI-2]`
- The JIT MAY implement a monomorphic builtin by hand as an override. `[BI-3]`
- An override MUST compute its cost with `MemoryUsage.memoryUsage(Constant)` and the costing
  function of the CEK machine. `[BI-4]`
- The JIT MUST NOT override a builtin whose CEK costing function, in variant E, is not a plain
  `DefaultCostingFun` or `ConstCostingFun`. `[BI-5]`
- The bridge MUST run every monomorphic builtin that has no override. `[BI-6]`
- The bridge MUST wrap each argument `Constant` in `CekValue.VCon` and call the runtime's
  `costFunction` and `f`. It MUST NOT convert values by type. `[BI-7]`

**Why:** the bridge calls the CEK machine's own code, so a bridged builtin has CEK semantics by
construction, and a new monomorphic Cardano builtin needs no JIT code. The bridge cannot run a
polymorphic builtin: an argument can be a `LamValue`, which has no `CekValue` form, and the CEK
code casts each argument to `CekValue`. `[BI-5]` excludes the costing wrappers
(`StringByByteLengthCostingFun`, `ValueMaxDepthCostingFun`, `DataNodeCountCostingFun`), which an
override would have to copy.

*Example, illustrative.* `mkCons` (`[BI-2]`):

```scala
(x, xs) match
    case (c: Constant, Constant.List(tp, l)) =>
        if c.tpe != tp then throw KnownTypeUnliftingError(tp, ...)
        else Constant.List(tp, c :: l)
    case (elem, ListValue(es))                              => ListValue(elem :: es)
    case (elem, Constant.List(DefaultUni.BuiltinValue, _))  => ListValue(elem :: Nil)
    case _                                                  => throw DeserializationError(...)
```

### 2.2 Candidate overrides

The corpus decides the overrides (section 4). The first candidates come from builtin call counts on
the 89 `plutus_use_cases` scripts with the CEK machine:

| Builtin | Calls | Kind |
|---|---|---|
| `UnConstrData` | 16 356 | candidate override |
| `HeadList`, `TailList`, `SndPair`, `IfThenElse`, `FstPair`, `ChooseList` | 5 112 - 13 415 each | polymorphic |
| `EqualsInteger` | 10 802 | candidate override |
| `UnBData`, `EqualsByteString`, `UnListData`, `UnIData`, `AddInteger` | 1 218 - 3 545 each | candidate override |

- Step 2 MUST start with exactly these 7 candidate overrides: `UnConstrData`, `EqualsInteger`,
  `UnBData`, `EqualsByteString`, `UnListData`, `UnIData`, `AddInteger`. `[BI-8]`
- A test MUST check `[BI-5]` for every override. `[BI-9]`
- A test MUST check that every polymorphic builtin has a hand-written implementation. `[BI-10]`

## 3. Backend and evaluation

### 3.1 One backend

- The JIT MUST have one backend, nativestack. `[BE-1]`
- The change MUST delete `mincont/`, `hybrid/`, `CaseHelper.genCaseDispatchCont` and the backend
  cases of `JITImplementation`. `[BE-2]`
- Generated code MUST count call depth and throw `StackThresholdException` at the limit. `[BE-3]`
- Generated code MUST NOT catch `StackOverflowError`. `[BE-4]`
- `StagingCompilerThread` MUST stay. `[BE-5]`

**Why:** every feature existed twice, once per backend. `[BE-4]`: a `StackOverflowError` thrown
during class initialization marks the class erroneous, and one thrown during `invokedynamic`
linking breaks its call site permanently. The counter throws while the stack still has room.

### 3.2 Compiler, evaluator and fallback

```scala
trait UplcEvaluator {
    def evaluate(budgetSpender: => BudgetSpender, logger: => Logger)(using vm: PlutusVM): Term
    final def evaluateScript(budgetSpender: => BudgetSpender, logger: => Logger)(using vm: PlutusVM): Unit
    def run(budgetSpender: => BudgetSpender, logger: => Logger)(using vm: PlutusVM): Unit
}
object UplcCompiler {
    def compile(
        term: Term,
        maxDepth: Int = 25_000,
        protocolVersion: MajorProtocolVersion = MajorProtocolVersion.vanRossemPV
    ): Compiled
    final class Compiled extends UplcEvaluator {   // the compiled code only
        val code: CompiledTerm
        val protocolVersion: MajorProtocolVersion   // the machine the code is for
        def withFallback(): UplcEvaluator           // the CEK machine on the call-depth limit
    }
}
```

*Example, illustrative.* `UplcCompiler.compile(term).withFallback().evaluateScript(spender, log)`
with a `PlutusVM` of PV11; for Dijkstra,
`UplcCompiler.compile(term, protocolVersion = MajorProtocolVersion.dijkstraPV)` with a `PlutusVM`
of PV12.

- `UplcCompiler.compile` MUST NOT need a `PlutusVM`. `[EV-11]`
- `UplcCompiler.compile` MUST take the call-depth limit `maxDepth` as a parameter, 25 000 by
  default. The compiled code MUST throw `StackThresholdException` at that depth. `[EV-10]`
- `UplcCompiler.compile` MUST take the protocol version as a parameter `protocolVersion` of type
  `MajorProtocolVersion`, 11 (van Rossem) by default. It MUST throw `IllegalArgumentException` for
  a version other than 11 or 12. The generated code MUST be the code of that version only, and
  MUST NOT read a protocol version when it runs. `[EV-12]`
- Every `evaluate` MUST reject a `PlutusVM` whose `semanticVariant` is not `E`. `[EV-1]`
- Every `evaluate` MUST reject a `PlutusVM` whose `protocolVersion` is not the `protocolVersion`
  the script was compiled for, with an `IllegalArgumentException` that names both versions.
  `[EV-2]`
- The check of `[EV-2]` MUST come before `budgetSpender` and `logger` are evaluated. The evaluator
  of `withFallback` MUST NOT run the CEK machine for a `PlutusVM` that `[EV-2]` rejects. `[EV-13]`
- `Compiled.evaluate` MUST run the `CompiledTerm` on the calling thread, with `budgetSpender` and
  `logger` evaluated once. `[EV-3]`
- On `StackThresholdException`, the evaluator of `withFallback` MUST drop that spender and that
  logger. `[EV-4]`
- It MUST then evaluate the term with `vm`, with `budgetSpender` and `logger` evaluated a second
  time. `[EV-5]`
- `evaluate` MUST return the term that `vm.evaluateDeBruijnedTerm` returns, or throw what it
  throws. `[EV-6]`
- A constant result MUST become `Term.Const`. A `ConstrValue` of constants MUST become
  `Term.Constr`. `[EV-7]`
- A result with no term (a function, or a list with a non-constant element) MUST throw
  `FunctionResultError`. It MUST NOT be evaluated again with the CEK machine. `[EV-8]`
- `evaluateScript` MUST throw `InvalidReturnValue` for a result that `PlutusVM.isResultValid`
  rejects, and for a result with no term. `[EV-9]`
- `run` MUST make the checks of `[EV-1]`, `[EV-2]` and `[EV-13]`, as `evaluate` does. `[EV-14]`
- `run` MUST spend the budget and write the logs that `evaluate` spends and writes. `[EV-15]`
- `run` MUST return normally for a result with no term. It MUST NOT convert the result to a
  term. `[EV-16]`
- The evaluator of `withFallback` MUST apply `[EV-4]` and `[EV-5]` to `run` too. `[EV-17]`

*Example, illustrative.* A benchmark times
`UplcCompiler.compile(term).withFallback().run(CountingBudgetSpender(), NoLogger)`. For
`(lam x x)`, `evaluate` throws `FunctionResultError`, and `run` returns. Both spend the same budget.

**Why `run`:** a caller that needs only the budget and the logs, such as a benchmark or a cost
estimate, must not fail on a script that returns a function. 78 of the 89 `plutus_use_cases`
scripts do. The CEK machine evaluates them without error, so before `run` such a caller called
`Compiled.code` directly and skipped the checks of `[EV-1]` and `[EV-2]`.

**Why:** an evaluator only evaluates; the fallback is a decorator, so a caller that wants the JIT
alone gets it, and the fallback rule lives in one place. A spent budget and written logs cannot be
undone, so the fallback needs a new spender and a new logger; by-name parameters let the caller
make them and keep the last one, which holds the budget and the logs of the result. The CEK
fallback of the old `HybridJIT` used the same spender, which counted the JIT's steps twice.
`[EV-2]` follows from `[SC-2]`: code for one version run with the machine of the other gives the
result of the wrong machine, for example a case on `Data` that the chain rejects. `[EV-13]`: the
fallback stands in for the compiled code, so it accepts only the machines the compiled code
accepts, and a rejected call spends no budget and writes no log. The default of `[EV-12]` is 11
because mainnet runs it and `PlutusVM` takes it as its own default; the other callers of the JIT
(tests, benchmarks) evaluate at 11. The depth limit is the caller's, because the caller owns the
threads and their stacks: about 25 000 fits a 64 MB stack. An evaluator that moved work to its own
large-stack threads cost a hand-off of 13-16 us per call. A function result has no term without the
environment the CEK machine keeps; scripts return unit, a bool or `Data`, and for a V3 script both
machines fail with `InvalidReturnValue`.

*Example, illustrative.* A script recurses deeper than `maxDepth`. The compiled code spends 10 000
steps and throws `StackThresholdException`. The `withFallback` evaluator evaluates `budgetSpender`
again and runs the CEK machine with the new spender. The caller keeps the last spender; its budget
is the CEK machine's alone.

## 4. Measurement

### 4.1 Corpus

- The corpus MUST contain the 89 scripts of `cardano-plutus-vm-benchmark/data/plutus_use_cases`.
  `[MS-1]`
- The corpus MUST contain 12 to 20 Scalus validators from `scalus-examples`, compiled for PlutusV3
  at PV11, with the datum, redeemer and script context of an existing test applied. `[MS-2]`
- The Scalus scripts MUST be stored as `.flat` files in
  `scalus-uplc-jit-compiler/src/test/resources/corpus/scalus/`. `[MS-3]`

**Why:** the 89 scripts are old PlutusTx code (heavy `fstPair`/`sndPair`/`unConstrData`, `case` in
11 scripts). Scalus PV11 code uses case on builtins, `dropList` and `lookupCoin`, so its hot set
differs.

### 4.2 Flags (historical: deleted in step 7)

The flags below existed only for the measurement of step 6; step 7 deleted `JitOptions` and every
flag (`[FL-5]`). The rules are kept as the record of how the features were measured.

```scala
final case class JitOptions(
    constPool: Boolean,
    letInlining: Boolean,
    appliedForm: Boolean,
    precompiledCurry: Boolean,
    overrides: Set[DefaultFun]
)
```

- `JitOptions` MUST select code generation at JIT-compile time only. `[FL-1]`
- The default `JitOptions` MUST turn every flag on. `[FL-2]`
- Every test suite MUST pass with all flags on and with all flags off. `[FL-3]`
- The budget helpers (`JitBudget`) and the depth counter MUST stay on. They MUST NOT be flags.
  `[FL-4]`
- Step 7 MUST delete `JitOptions` and every feature that you decide to remove. `[FL-5]`

| Flag | Off means |
|---|---|
| `constPool` | constants are built in place |
| `letInlining` | `[(lam x b) a]` compiles as a normal call |
| `appliedForm` | a saturated builtin goes through its curried closure |
| `precompiledCurry` | curried closures are generated lambdas |
| `overrides` | the builtins not in the set go through the bridge |

### 4.3 Variants and metrics

- The bench MUST measure these variants: the CEK machine; the branch `worktree-jit-refactor-plan`;
  all-on, three times; all-off; all-on without each flag. `[MS-4]`
- If all-on without overrides is at least 10 % slower than all-on, the bench MUST also measure
  all-on without each single override. `[MS-5]`
- For each variant, the bench MUST first check that every corpus script gives CEK semantics.
  `[MS-6]`
- A variant that fails `[MS-6]` MUST be reported as broken and MUST NOT be timed. `[MS-7]`
- The bench MUST run each variant in its own JVM. `[MS-8]`
- The bench MUST warm up all scripts round-robin with `-XX:CICompilerCount=12`. `[MS-9]`
- The warm-up MUST stop when two consecutive 5 s windows differ by less than 3 %. `[MS-10]`
- The bench MUST record per script: median eval time, compile time, time to steady state,
  class-file bytes, and whether it compiled at all. `[MS-11]`
- The bench MUST record the lines of code behind each flag. `[MS-12]`

**Quick mode.** The bench has a `--quick` option. It adds
`-XX:Tier3InvocationThreshold=100 -XX:Tier4InvocationThreshold=1000 -XX:Tier4CompileThreshold=2000`.

- The bench MUST run all-on in both modes once. `[MS-13]`
- If quick all-on is inside the noise band of the three default all-on runs, every variant MAY run
  in quick mode. `[MS-14]`
- Otherwise the final table MUST come from default mode. `[MS-15]`

**Why:** the generated code is thousands of small methods. HotSpot compiles a method with C2 only
after about 5 000 calls and needs a profile of call targets, type checks and branches. A shared
round-robin warm-up lets all scripts warm up in parallel. Each script runs one fixed path, so a
shorter profile is probably as good as a long one; `[MS-13]` checks that once.

### 4.4 Decision

- Step 6 MUST produce one table row per feature: eval-time change, compile-time change, size
  change, lines of code, and a keep or remove recommendation. `[DC-1]`
- The recommendation MUST keep a feature whose removal slows geo-mean eval time or compile time by
  at least 10 %. `[DC-2]`
- The recommendation MUST keep a feature without which a corpus script does not compile. `[DC-3]`
- The owner decides. The recommendation does not. `[DC-4]`

**Why:** run-to-run noise after warm-up is about 3 - 5 %. A gain under 10 % is hard to tell from
noise and does not pay for its maintenance in a proof of concept.

## 5. Steps

Each step is one commit on `worktree-jit-simplify`.

| # | Step | Gate |
|---|---|---|
| 1 | Corpus: export the Scalus validators (`[MS-2]`, `[MS-3]`); run the JIT-vs-CEK check on the whole corpus | the CEK machine evaluates every corpus script |
| 2 | Values and builtins: sections 1 and 2 | conformance known-failure list empty; CEK semantics on the corpus; `[BI-9]`, `[BI-10]` |
| 3 | One backend and `UplcCompiler`: section 3 | fallback test of `[EV-3]` to `[EV-5]`: result, budget and logs equal the CEK machine's, nothing counted twice |
| 4 | Flags: section 4.2 | `[FL-3]` |
| 5 | Bench and run script: section 4.3 | `[MS-6]` for all-on |
| 6 | Measure every variant; write the table of section 4.4 | the owner decides |
| 7 | Delete the removed features and `JitOptions`; final measurement; update `JitRunner` docs and `docs/internal/JIT_REFACTORING_PLAN.md` | all suites pass; final numbers against the CEK machine and `worktree-jit-refactor-plan` |

Rules for every step:

- Generators MUST take generators (`Quotes ?=> Expr`). They MUST NOT splice a finished `Expr` under
  a `val` or a lambda. `[RU-1]`
- Every generator MUST show its generated code for one concrete input in its doc comment.
  `[RU-2]`
- Commits MUST NOT carry attribution lines. `[RU-3]`
- `scalafmtAll` MUST run before each commit. `[RU-4]`
- Files MUST be added to git by name, never with `git add -A`. `[RU-5]`

**Why:** `[RU-1]` is the owner rule of commit `2eb9f3d61`: a finished `Expr` spliced under a `val`
or a lambda is copied at each nesting level, which took auction_1-1 from 1.9 s to 20 s to compile.
`[RU-2]` is section 2.5 of `docs/internal/JIT_REFACTORING_PLAN.md`.

## Implementation status

- Step 3 renamed `StackTresholdException` to `StackThresholdException`.
- Step 2 deleted `mincont/`, `hybrid/` and `JITImplementation` (`[BE-2]`, planned for step 3):
  porting them to the new values first would have been dead work. Step 3 keeps the rest of
  section 3.
- `[BI-7]` does not say what the bridge does with an argument that is not a constant (a function
  that an ill-typed script passes to a monomorphic builtin). It wraps a stand-in `CekValue`
  (`VLamAbs` or `VDelay` of `error`), with memory usage 1 and the same unlifting failure as the
  real value (`JitRuntime.toCekValue`).
- Every application that is not an inlined let now counts its call depth, also when the function
  and the argument are variables (`[f f]`): a recursion through such applications only had no
  counter before. The count is generated at each application (`stack.incr`/`decr` around the call),
  and `JitRuntime.applyFields` counts every call of a `case` field.
- All constants are read from the pool, including unit, booleans and strings, which were JVM
  literals: a constant is now a `Constant` object.
- The conformance suite has no known-failure list any more: every case must pass.
- Removed tools that ran the deleted backends: `JitCompileTimeBench` (the bench of step 5 measures
  compile time), `JITPlutusUseCasesBenchmarkTest` and `TestJITCompilationHang` (the corpus covers
  their scripts), `MincontCompareTest`, `SimpleDebugTest` (merged into `JITTest`),
  `JITDeepRecursionTest` (`UplcCompilerTest` tests deep recursion through the fallback). The JMH
  `JITBenchmark` in `bench/` has one class for the one backend.
- `Compiled.evaluate` takes the spender and the logger by name and returns the result term, as
  `PlutusVM.evaluateDeBruijnedTerm` does (`[EV-3]` to `[EV-6]`). `PlutusVM.isResultValid` is
  `private[uplc]` now, so `evaluateScript` shares its check (`[EV-9]`).
- `[EV-7]` names constants and `constr` values only. A `ListValue` result becomes a list constant of
  element type `BuiltinValue` if all its elements are constants. Otherwise it has no term, and
  `evaluate` throws `FunctionResultError` (`[EV-8]`); the CEK machine fails there too, in
  `dischargeCekValue`, with another class.
- The call-depth limit (25 000 counted applications) assumes a thread stack of 64 MB, which the
  caller provides (`[EV-10]`). Step 7 had run the compiled code on a pool of 256 MB-stack threads;
  the owner's review removed it: the hand-off cost 13-16 µs per evaluation (auction_1-1 186 µs
  against 170 µs on the calling thread), and threads are the caller's choice.
- `[FL-3]` cannot hold for the largest scripts: with every flag off, the five stablecoin scripts of
  `plutus_use_cases` exceed the class-size limit (65 535 constant-pool entries). With any one flag
  off and the others on, every corpus script compiles. So the all-off variant of the
  largest-script test checks that the class size is its only failure, and every other suite passes
  with all flags off. Which feature these scripts need is a `[DC-3]` input of step 6.
- `JitOptions.allOff` has no overrides; its constants are built in place by `ConstantExpr`: leaves
  from literals, `Data`, BLS12-381 points and `value` constants decoded from bytes, as built node
  by node a large `Data` would exceed the 64 KB limit of a JVM method.
- The bench (`JitAblationBench`, `JitAblationReport`, `scripts/jit-ablation.sh`) records the
  class-file bytes through `-Dscalus.jit.outDir`, which `StagingCompilerThread` passes to the
  staging compiler: every compile writes one class, `Generated$Code$From$Quoted`, so the bench
  clears the directory before each compile. It compiles the first 10 scripts once untimed, so the
  timed compiles do not include the warm-up of the staging compiler itself. A script that does not
  compile is recorded and left out of the timing; ratios are geometric means over the scripts both
  runs compiled. `[MS-12]` counts the code lines inside `// [flag: ...]` regions of the main
  sources; step 7 removes the markers with the flags.
- Step 6 did not use the protocol of `[MS-9]` and `[MS-10]`, and timed 14 scripts, not the corpus:
  - Round-robin over the whole corpus does not reach a steady state. The generated code of the 106
    scripts filled the default code cache (240 MB) after about 190 s and HotSpot disabled its
    compiler; with a 2 GB cache the round time still stayed at 110-150 ms after 600 s, against
    35 ms for the CEK machine. Each script gets 1/106 of the calls, so its methods reach the C2
    thresholds late, and the code warms up in steps: two 5 s windows on a plateau differ by less
    than 3 %, so `[MS-10]` stops too early (the reference branch stopped after 2 windows).
  - Owner's decision: time 14 scripts only (`JitCorpus.BenchSubset`: small, medium and large
    `plutus_use_cases` scripts, and Scalus validators). The CEK check of `[MS-6]` runs on them;
    `JitVsCekCheck` checked every variant on the whole corpus.
  - Round-robin over the 14 scripts, with at least 240 s of warm-up, still swung between 1.3 and
    3 ms per round after warm-up; three all-on runs gave 54, 71 and 85 µs geo-mean.
  - So each script is warmed up and timed alone (`-Djit.bench.isolated=true`, the default of
    `scripts/jit-ablation.sh`): 1 s windows until two differ by less than 3 %, at least 5 s, then
    the median of 3 s of runs. Three all-on runs gave 32.2, 31.2 and 33.2 µs (noise band 6.3 %).

## Step 6 measurements

2026-10-19, `scripts/jit-ablation.sh` on the 14 bench scripts, isolated mode, default JIT
thresholds, `-XX:CICompilerCount=12`, one JVM per variant, on the owner's laptop (14 cores; other
work running). Change when the feature is removed: the geometric mean over the scripts of the
per-script ratio of `no-<feature>` to all-on (mean of three all-on runs). Positive is slower or
larger. LOC: code lines (no blanks, no comments) that exist only for the feature / only for its
absence.

| Feature | Eval time | Compile time | Class bytes | LOC on / off | Recommendation |
|---|---|---|---|---|---|
| `constPool` | +194.8 % | +26.5 % | -3.5 % | 29 / 34 | keep (`[DC-2]`, eval) |
| `letInlining` | +25.1 % | +46.3 % | +110.5 % | 22 / 0 | keep (`[DC-2]`, eval and compile) |
| `appliedForm` | +69.7 % | +6.5 % | -1.3 % | 99 / 0 | keep (`[DC-2]`, eval) |
| `precompiledCurry` | -16.5 % | +5.9 % | +3.0 % | 62 / 54 | remove: generated curried lambdas are faster |
| `overrides` (all 7) | +19.5 % | +4.1 % | +0.5 % | 66 / 0 | keep some (`[DC-2]`, eval); see below |

All-on without one override (`[MS-5]`, as all-on without overrides was 19.5 % slower):

| Override | Eval time | Compile time | Recommendation |
|---|---|---|---|
| `UnBData` | +11.3 % | +48.7 % | keep (`[DC-2]`, eval) |
| `UnConstrData` | +7.3 % | +22.6 % | borderline: eval inside 10 %, just outside the noise band |
| `AddInteger` | -2.9 % | +4.9 % | remove |
| `EqualsByteString` | -7.2 % | -1.5 % | remove |
| `EqualsInteger` | -3.0 % | +2.5 % | remove |
| `UnIData` | -6.7 % | -6.0 % | remove |
| `UnListData` | -6.0 % | +0.6 % | remove |

An override changes the call target only, not the size of the generated code (class bytes within
0.3 %), so the compile-time changes of the single overrides are noise: the compile time of the
three all-on runs spread by 5.6 %, and the `UnBData` run warmed up for 184 s instead of 80-100 s,
a sign of other load. Read the override rows by eval time.

No single feature is needed to compile a script (`[DC-3]`): with any one flag off all 14 bench
scripts, and all 106 corpus scripts (`JitVsCekCheck`), compile. With every flag off,
stablecoin_1-5 does not (class too large).

Other runs, against all-on:

| Run | Eval time | Compile time | Class bytes |
|---|---|---|---|
| all-off | +246.4 % (13 of 14 scripts) | +95.8 % | +163.1 % |
| CEK machine | +182.9 %: all-on is 2.83x as fast | - | - |
| `worktree-jit-refactor-plan` (`79ca316fb`) | -13.0 %: the reference is 1.15x as fast | +110.9 %: all-on compiles 2.11x as fast | +20.2 % (9.27 MB against 7.71 MB) |
| all-on, quick mode | -7.8 %, outside the noise band: `[MS-15]`, the table is from default mode | +3.6 % | 0 |

Time to steady state, all-on: 6 s median per script (8 s max), 78-95 s for the 14 scripts.
Without `precompiledCurry` the JIT evaluates in 26.9 µs geo-mean, about as fast as the reference
(28.1 µs).

## Step 7

The owner's decisions (`[DC-4]`) on the table of step 6:

- Kept, now unconditional: the constant pool, let inlining, the applied form, and the overrides
  `UnConstrData` and `UnBData`.
- Removed: precompiled curried closures (`JitCurry`; an unapplied builtin is now generated
  lambdas, the faster path), and the overrides `AddInteger`, `EqualsByteString`, `EqualsInteger`,
  `UnIData` and `UnListData`, which now run through the bridge.
- Deleted with them (`[FL-5]`): `JitOptions` and every flag branch, `ConstantExpr` (in-place
  constants), the all-off test variants and the all-off class-size test, `JitVariants`, the flag
  markers and the per-flag LOC count of the bench.
- Dead code deleted: `UplcTermHelper`, `DefaultUniExpr`; `BuiltinEmitter.hasImpl` and
  `CekBridge.canRun` moved into `BuiltinImplsTest`, their only user.
- After review: `JitEvaluator` is replaced by `UplcCompiler.compile(term, maxDepth)`, the
  `UplcEvaluator` `Compiled` and its `withFallback()` decorator (section 3.2), which run on the
  calling thread; `CaseHelper` is merged into `nativestack/JIT.scala`; every failure uses the CEK
  machine's exception class.
- After the final review: the bench of steps 5 to 8 is renamed `JitBench`, `JitBenchReport` and
  `scripts/jit-bench.sh` (labels `jit-1..3` for `all-on-1..3`); the notes above keep the old names.
  UPLC calls of `Apply` and `Force` are generated at each site (a shared helper was megamorphic,
  uniswap-1 1.9x slower); the calls of `case` fields stay in `JitRuntime.applyFields`, now
  depth-counted.
- `scalus-uplc-jit-compiler/docs/` describes the current design (`DESIGN.md`); the mincont, hybrid
  and old benchmark documents are deleted. `docs/internal/JIT_REFACTORING_PLAN.md` is marked
  superseded.

### Final measurement

2026-10-19, the protocol of step 6 (`scripts/jit-ablation.sh`: 14 bench scripts, isolated mode,
`-XX:CICompilerCount=12`, one JVM per run), the final JIT three times, the CEK machine, and the
reference branch `worktree-jit-refactor-plan` (`79ca316fb`) with a copy of the bench. Results in
`target/jit-final/`. JIT values are per script the median of the three runs.

| | Final JIT | CEK machine | Reference |
|---|---|---|---|
| Eval, geo-mean | 27.6 µs | 84.9 µs: the JIT is 3.07x as fast | 29.9 µs: the JIT is 1.08x as fast |
| Compile, geo-mean | 0.33 s | - | 0.67 s: the JIT compiles 2.03x as fast |
| Class bytes, 14 scripts | 7.80 MB | - | 9.27 MB (+19 %) |

The three JIT runs gave 25.8, 27.1 and 34.1 µs geo-mean. The spread is in the two largest
scripts: multisig-sm-02 took 79, 187 and 517 µs, stablecoin_1-5 415, 302 and 2813 µs. A 5 s
minimum warm-up is too short for them; the other 12 scripts agree within 5 %. Against the
reference the JIT is faster on stablecoin_1-5, coop-6, guardrail and the Scalus validators, and
slower on pubkey-1, auction_1-1, auction_1-3, uniswap-1 and multisig-sm-02.

`JitVsCekCheck` on all 106 corpus scripts: no mismatch. Main sources of the JIT: 1974 lines
(1098 code lines) before step 7, 1635 (887) after.
