Source: https://scalus.org/docs/smart-contracts/how-the-jit-works

# How the JIT Works

The JIT is an experimental module. Its API and its internals can change in any release, and it can
have bugs. Before you use it, read the warning on [JIT Evaluation](/docs/smart-contracts/jit-evaluation).

This page is for contributors, and for readers who want to know how the [JIT](/docs/smart-contracts/jit-evaluation)
is built to give exactly the CEK machine's answer. The code is in
[`scalus-uplc-jit-compiler`](https://github.com/scalus3/scalus/tree/master/scalus-uplc-jit-compiler).
Its design record is
[`DESIGN.md`](https://github.com/scalus3/scalus/blob/master/scalus-uplc-jit-compiler/docs/DESIGN.md),
and the rules it implements are in the
[JIT specification](https://github.com/scalus3/scalus/blob/master/docs/superpowers/specs/2026-10-19-uplc-jit-simplification-design.md).

## From UPLC to machine code

A compile has three stages:

1. `UplcCompiler.compile` walks the UPLC term once and generates Scala code for it, with Scala 3
   staging: quotes `'{ ... }` build code, and splices `${ ... }` put generated code inside it.
2. The staging compiler (`scala.quoted.staging`) compiles that code to JVM bytecode. The compiles
   run one at a time on one thread, `StagingCompilerThread`: a staging compiler can only be used
   from the thread that first ran it.
3. The JVM loads the classes. During warm-up, HotSpot's C2 compiler turns the hot methods into
   machine code.

The constants of a script never become bytecode. The `ConstantPool` collects them at compile time,
and the compiled function receives them once, as an array. So a large constant costs no compile
time and cannot reach the 64 KB size limit of a JVM method.

## One UPLC term, one piece of code

The JIT uses the JVM call stack where the CEK machine uses its continuation stack. A UPLC lambda
becomes a JVM lambda, and an application becomes a JVM call. The values mirror the CEK machine's
values, case by case:

| `UplcValue` | `CekValue` |
|---|---|
| `Constant` | `VCon` |
| `ConstrValue` | `VConstr` |
| `LamValue` | `VLamAbs`, or a builtin that takes a term argument next |
| `DelayValue` | `VDelay`, or a builtin that takes a force next |

Each term kind generates this code, where `spend(K)` is one call of `JitBudget`:

```text
x              ~> { spend(Var); x }
(lam x b)      ~> { spend(LamAbs); ((arg: UplcValue) => <b>): LamValue }
[(lam x b) a]  ~> { spend(Apply); spend(LamAbs); val x = <a>; <b> }        // an inlined let
[f a]          ~> { spend(Apply); val fun = <f>; val a = <a>
                    fun match { case l: LamValue => l.apply(a)            // a counted call
                                case other => JitRuntime.cannotApply(other) } }
(force t)      ~> { spend(Force); <t> match { case d: DelayValue => d.apply() ... } }
(delay t)      ~> { spend(Delay); (() => <t>): DelayValue }
(con c)        ~> { spend(Const); consts(i) }
(constr i a*)  ~> { spend(Constr); ConstrValue(i, List(<a>*)) }
(case s b*)    ~> { spend(Case); <dispatch on <s>> }
(error)        ~> throw EvaluationFailure(Nil)
```

This is the generator for an application, from `nativestack/JIT.scala`:

```scala showLineNumbers
case Term.Apply(f, arg, _) =>
    '{
        ${ GenCtx.spendStep(StepKind.Apply) }
        val fun = ${ gen(f) }
        val a = ${ gen(arg) }
        fun match
            case l: LamValue =>
                ${ nctx.stack }.incr()
                val r = l.apply(a)
                ${ nctx.stack }.decr()
                r
            case other => JitRuntime.cannotApply(other)
    }
```

The call `l.apply(a)` is generated at every application, not in a shared helper. HotSpot keeps a
profile of receiver types per bytecode, so each call site sees only the one or two lambda classes
it calls, and C2 inlines them. Behind one shared helper, every site saw thousands of classes, and
C2 inlined nothing: the `uniswap-1` contract ran 1.9× slower.

A let binding, `[(lam x b) a]`, becomes a local `val` instead of a closure and a call. Each closure
is a JVM class and method that HotSpot must compile, so fewer closures mean a shorter warm-up. The
generator inlines lets into a method only while the method has fewer than 20 terms: small methods
stay cheap for C2 to inline into their callers.

## Builtins

- A saturated builtin application is one static call. Its arguments are computed in the CEK
  machine's order.
- A partly applied builtin is a curried lambda, a `BuiltinLamValue` or a `BuiltinDelayValue`.
- The 15 polymorphic builtins, `unConstrData` and `unBData` have hand-written implementations in
  `BuiltinImpls`, copied from the CEK machine's `Builtin.scala`.
- Every other builtin runs on the CEK machine's own builtin runtime, through `CekBridge`. So its
  result, its failures and its cost are those of the CEK machine.

## Exactness

A different answer from the CEK machine is a consensus bug, so the JIT must match it in three
things:

- **The result.** The same term, or the same exception class. The checks happen where the CEK
  machine makes them: an application computes the function and the argument first, and only then
  checks that the function is one.
- **The budget, per cost category.** Each term spends the steps that the CEK machine's `computeCek`
  spends for it, in the same order relative to its subterms. So a `RestrictingBudgetSpender` stops
  at the same step as on the CEK machine. A spend is one static call, such as
  `JitBudget.spendVar(budget, params)`, with the categories allocated once.
- **The machine.** A compile generates code for one protocol version. Versions 11 and 12 differ only
  in `case` on `Data`: code for 11 rejects it, and code for 12 performs it. Every evaluation checks
  the language, the semantics variant and the protocol version of its `PlutusVM` before it spends
  anything.

The tests check this against the CEK machine: the Plutus conformance suite (`JitConformanceTest`,
every case passes), the term shapes that the generator treats in a special way, also when they
fail (`JitShapeTest`), and 89 `plutus_use_cases` scripts and 17 Scalus validators (`JitVsCekCheck`).

## Call depth and the CEK fallback

A UPLC loop is a recursion through applications, so a deep loop is a deep JVM stack. Every
application that is not an inlined let, and every call of a `case` branch with its fields, counts its call depth in a `NativeStackContext`. At the
`maxDepth` of the compile (25,000 by default), the code throws `StackThresholdException`.

`withFallback()` catches it and evaluates the script again, from the start, with the CEK machine.
The JIT cannot continue where it stopped: its state is on the JVM stack, and the CEK machine has no
way to take it over. The budget already spent and the logs already written cannot be undone, so
the fallback evaluates the by-name spender and logger a second time. The last spender and logger
hold the budget and the logs of the result.

## Arguments

`applyTo(a1, .., an)` evaluates `[[term (con a1)] .. (con an)]` with the code of `term`. The
CEK machine spends, on `[[term a1] a2]`: Startup, Apply, Apply, the steps of `term`, Const, the
steps of the call with `a1`, Const, the steps of the call with `a2`. The compiled code spends
Startup and the steps of `term` itself, so an applied evaluator can spend its Apply steps only
before the code or after it. It spends them before: then the budget per category is always the
CEK machine's, except for a budget that runs out in the first `n + 1` spends. For one argument
and a budget of 150 memory units:

| Machine | Spends, in order | Fails on |
|---|---|---|
| CEK machine on `[term (con a)]` | Startup (100 memory), Apply (100 memory) | Apply |
| JIT, `applyTo(a)` | Apply (100 memory), Startup (100 memory) | Startup |

Both spend the same total. Only the step that fails is different, and no transaction has such a
small budget.

The calls that apply the arguments happen outside the compiled code, so they do not count toward
`maxDepth`. An applied evaluator can reach `StackThresholdException` up to `n` calls deeper than
the same applied term compiled whole. The 64 MB stack has room for that.

## Compile time: create each `Expr` where it will live

The first JIT took a median of 22 s to compile a contract, and 197 s for the slowest. The time
went into copies: a finished `Expr` that is spliced under a `val` or a lambda is copied to change
its owner, and every level of nesting copied the levels below it again.

The fix is a rule for every generator: it takes the `Quotes` of the splice that its result goes
in, and it is called inside that splice. So each piece of code is created with its final owner,
and nothing is copied. The generated code is the same. The median compile went down to 2.3 s, and
the slowest to 7.7 s. The [blog post](https://lantr.io/blog/blazing-fast-lambda-calculus-scala-3-staging/)
tells the full story.

## Code map

| File | Role |
|---|---|
| `nativestack/JIT.scala` | The code generator, and the code for `case` |
| `UplcValue.scala` | The values of compiled code, one case per `CekValue` case; `CompiledTerm` |
| `JitRuntime.scala` | Shared runtime code: the failures of apply and force, `case` branch selection |
| `JitBudget.scala` | One static call per budget spend |
| `BuiltinEmitter.scala` | Code for builtins: a static call when saturated, curried lambdas when not |
| `BuiltinImpls.scala` | The hand-written builtins |
| `CekBridge.scala` | Every other builtin, on the CEK machine's runtime |
| `ConstantPool.scala` | The constants of a script, passed to the code as an array |
| `StagingCompilerThread.scala` | The thread that runs the staging compiles |
| `UplcCompiler.scala` | `UplcEvaluator`, `UplcCompiler.compile`, the applied evaluator and the CEK fallback |

To see the idea without the production details, read
[tiny-uplc-jit](https://github.com/lantr-io/tiny-uplc-jit): an interpreter, a closure compiler and
a staged compiler for a small UPLC, in a few hundred lines.
