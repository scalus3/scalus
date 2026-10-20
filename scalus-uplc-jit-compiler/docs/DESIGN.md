# UPLC JIT: design

A staged JIT: Scalus compiles a UPLC script to JVM bytecode with Scala 3 staging when the script
is loaded, and the JVM's JIT compiles that bytecode to machine code. The compiled code evaluates
the script with exactly the semantics of the CEK machine: the same result, the same failures and the same
spent budget, per budget category. It is experimental and not used in production. The design and
its measurements are in `docs/superpowers/specs/2026-10-19-uplc-jit-simplification-design.md`.

## Scope

Builtin semantics variant E and the machine of protocol version 11 (van Rossem): PlutusV3 at PV11.
`case` on a constructor, bool, integer, unit, list or pair; `case` on `Data` fails, as on the CEK
machine before PV12. Evaluate other scripts with the CEK machine.

## Use

```scala
given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
val compiled = UplcCompiler.compile(program.term)  // a staging compile: 0.1-6 s; maxDepth 25 000
val evaluator = compiled.withFallback()                    // CEK on the call-depth limit
var spender: RestrictingBudgetSpender = null
val result: Term = evaluator.evaluate({ spender = RestrictingBudgetSpender(budget); spender }, Log())
evaluator.evaluateScript(RestrictingBudgetSpender(budget), Log())  // also checks the unit result
```

Every evaluator runs on the calling thread and returns or throws what
`vm.evaluateDeBruijnedTerm` does, with the CEK machine's exception classes. Its spender and logger
are by-name parameters. `compile` takes the call-depth limit: the compiled code throws
`StackThresholdException` at that depth; 25 000 fits a 64 MB thread stack, which the caller
provides. `Compiled` alone lets that exception through; `withFallback()` then evaluates the term
with the CEK machine from the start, with the spender and the logger evaluated a second time. The
last spender and logger hold the budget and the logs. A result with no term (a function) throws
`FunctionResultError`, and `evaluateScript` turns it into `InvalidReturnValue`, as the CEK machine
fails for a V3 script.

## Parts

| File | Role |
|---|---|
| `nativestack/JIT.scala` | the code generator: a UPLC lambda is a JVM lambda, an application a JVM call; also the code for `case` |
| `UplcValue.scala` | the values of compiled code, one case per `CekValue` case; `CompiledTerm` |
| `JitRuntime.scala` | shared runtime code: the failures of apply and force, `case` branch selection and the depth-counted application of `case` fields, with the checks of the CEK machine |
| `JitBudget.scala` | one static call per budget spend, with preallocated categories |
| `BuiltinEmitter.scala` | code for builtins: one static call when saturated, generated curried lambdas when not |
| `BuiltinImpls.scala` | the 15 polymorphic builtins and the `unConstrData` and `unBData` overrides, copies of `Builtin.scala` |
| `CekBridge.scala` | every other builtin, run by the CEK machine's own runtime |
| `ConstantPool.scala` | the constants of a script, passed to the compiled code once, never bytecode |
| `StagingCompilerThread.scala` | runs the staging compiles one at a time on a dedicated thread |
| `UplcCompiler.scala` | `UplcEvaluator`, `UplcCompiler.compile`, `Compiled` and its CEK fallback decorator |

Generators follow the owner rule: a generator takes the `Quotes` of the splice its result goes in
and is called inside it, so staging never re-owns a finished tree.

## Tests and tools

- `JitConformanceTest`: the Plutus conformance suite on the JIT; every case passes.
- `JitShapeTest`: term shapes the generators treat specially, also on failure, against the CEK
  machine: result, exception class and budget per category.
- `UplcCompilerTest`: `evaluate`, `evaluateScript` and `withFallback`, the depth limit (also for a
  loop through `case` alone), and one compiled term evaluated from 8 threads at once.
- `BuiltinImplsTest`, `CekBridgeTest`: the names of the hand-written builtins, the bridge cache.
- `JitVsCekCheck`: the JIT against the CEK machine on the corpus (89 `plutus_use_cases` scripts and
  17 Scalus validators, `JitCorpus`).
- `scripts/jit-bench.sh`: the bench (`JitBench`, `JitBenchReport`), the JIT against the CEK machine.
- `JitCodegenProbe`, `JitEvalProbe`: compile time and evaluation time of one script.
