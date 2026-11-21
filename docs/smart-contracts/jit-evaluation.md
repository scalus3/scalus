Source: https://scalus.org/docs/smart-contracts/jit-evaluation

# JIT Evaluation

**The JIT is an experimental module. Use it with caution.**

- **The API can change** in any release, also in a minor one. The module has no binary
  compatibility check.
- **It can have bugs.** It passes the Plutus conformance suite and its tests compare it with the
  CEK machine, but it is not used in production yet. A result, an error or a budget can be
  different from the CEK machine's.
- **Check the results that matter with the CEK machine.** For example, evaluate a transaction with
  the CEK machine before you submit it, and run your tests on both.

If the JIT and the CEK machine give different answers, please [open an
issue](https://github.com/scalus3/scalus/issues).

The Scalus JIT compiles a UPLC script to JVM bytecode. It is designed to evaluate the script with
the semantics of the CEK machine: the same result, the same failures and the same budget per cost
category. On the [public benchmark](https://plutus-vm-bench.lantr.io) of 89 real contracts it
takes 94 µs per contract, against 171 µs for the Scalus CEK machine and 276 µs for the Haskell VM.

## When to use it

A compile takes seconds: 2.3 s for the median of the 89 benchmark contracts, 7.7 s for the
slowest. The JVM then needs warm-up before the generated code runs at full speed. So the JIT pays
off only for a script that runs many times. The median break-even against the CEK machine is
about 16,000 evaluations.

Good uses:

- Emulators, layer-2 ledgers and test suites that run the same validators for every transaction.
- Property tests and fuzzers that run one validator thousands of times.
- Indexers and chain replays. A few scripts do most of the work: in four weeks of Cardano mainnet
  (early 2025), the 100 most used Plutus V2 scripts, 6 % of them, did 93 % of the V2 script runs.

For a script that runs a few times, use the CEK machine (see [Evaluating
script](/docs/smart-contracts/evaluating-script)).

## Scope

- Plutus V3 and V4 scripts, with builtin semantics variant E, at protocol version 11 (van Rossem)
  or 12 (Dijkstra). For other scripts, use the CEK machine.
- A compile generates code for one protocol version, 11 by default. Every evaluation checks that
  its `PlutusVM` is a Plutus V3 or V4 machine with semantics variant E and the protocol version of
  the compile. Any other machine throws `IllegalArgumentException` before the script runs.
- The JIT does not know the language of a script. Do not give it a Plutus V1 or V2 script: it would
  evaluate the script with the rules and costs of the V3 machine, with no error.
- JVM only, and only in a Scala 3.3 (LTS) project. The JIT runs the Scala 3.3 compiler at run
  time. In a project on a later Scala version, the first compile fails with `NoSuchMethodError`.

## Setup

Add the module to `build.sbt`:

```scala copy
libraryDependencies += "org.scalus" %% "scalus-uplc-jit-compiler" % scalusVersion
```

The compiled code makes a JVM call for each UPLC application, so a deep recursion needs a large
thread stack. The default call-depth limit, 25,000, fits a 64 MB stack. Give that stack to the
threads that evaluate scripts. For `sbt run` and the tests, fork the JVM:

```scala copy
// build.sbt
run / fork := true
run / javaOptions += "-Xss64m"
Test / fork := true
Test / javaOptions += "-Xss64m"
```

In a server, run the evaluations on a thread with its own stack size:

```scala copy
val jitThread = new Thread(null, () => evaluateTransactions(), "jit", 64L * 1024 * 1024)
jitThread.start()
```

If the stack is too small for the limit, the JVM throws `StackOverflowError`. No evaluator catches
it. For a smaller stack, give `UplcCompiler.compile` a smaller `maxDepth`.

## Evaluate a validator

`applyTo` needs Scalus 1.5.0 or newer. The rest of this page works with 1.4.1 or newer.

Compile the validator once, unapplied. Then apply it to the script context of each transaction
with `applyTo`. This takes no new compile. `HtlcContract` is the compiled validator from [Working
with a contract](/docs/dapp-development/working-with-contract):

```scala copy showLineNumbers
import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion}
import scalus.uplc.Constant
import scalus.uplc.builtin.Data
import scalus.uplc.eval.*
import scalus.uplc.jit.UplcCompiler

given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

// once per script: a staging compile, which takes seconds
val validator = UplcCompiler.compile(HtlcContract.program.term).withFallback()

// for each transaction
def validate(scriptContext: Data, budget: ExUnits): ExUnits = {
    var spender: RestrictingBudgetSpender = null
    validator
        .applyTo(Constant.Data(scriptContext))
        .evaluateScript({ spender = RestrictingBudgetSpender(budget); spender }, Log())
    spender.getSpentBudget
}
```

- `UplcCompiler.compile` takes a term with named variables: `Program.term`, not a De Bruijn term.
  For a script from the chain, use `script.program.term`.
- `withFallback()` evaluates the script again with the CEK machine if it reaches the call-depth
  limit. Call it before `applyTo`: `applyTo` returns a `UplcEvaluator`, which has no
  `withFallback`.
- `applyTo(a1, .., an)` evaluates `[[term (con a1)] .. (con an)]`. For a parameterized validator,
  pass the parameters first, then the script context. Or apply the parameters before the compile:
  then each parameter set is a script of its own, with its own hash.
- `evaluateScript` throws what the CEK machine throws: `OutOfExBudgetError`, `EvaluationFailure`
  for an `(error)` term, a `BuiltinError`, and so on. A Plutus V3 script that does not return unit
  fails with `InvalidReturnValue`.

### Budget and logs

The spender and the logger are by-name parameters. An evaluation that falls back to the CEK
machine evaluates them a second time: the compiled code has already spent part of the budget and
written logs, and those cannot be undone. So create the spender and the logger inside the
argument, as above, and read the last one. A `val` that you pass in counts the budget twice after
a fallback.

To measure the cost of a script without its result, use `run`:

```scala copy
val spender = CountingBudgetSpender()
validator.applyTo(Constant.Data(scriptContext)).run(spender, NoLogger)
```

The budget is that of the CEK machine on the applied term, per cost category. One detail is
different: the JIT spends the Apply steps of the arguments before the startup cost. Only a budget
below `startup + n × applyCost` sees the difference: 200 memory units or 16,100 CPU steps for one
argument. See [How the JIT works](/docs/smart-contracts/how-the-jit-works#arguments).

## Cache compiled scripts by hash

A script hash names one script forever. The compiled code is for one protocol version, so key the
cache by the script hash and the protocol version: after a hard fork, the old entries no longer
match the machine. One compiled script is safe to evaluate from many threads at once, and
`UplcCompiler.compile` is safe to call from any thread:

```scala copy showLineNumbers
import java.util.concurrent.ConcurrentHashMap
import scalus.cardano.ledger.{Language, MajorProtocolVersion, PlutusScript, ScriptHash}
import scalus.uplc.Term
import scalus.uplc.jit.{UplcCompiler, UplcEvaluator}

/** Compiles on the first read of `evaluator`, outside the lock of the map. */
final class LazyEvaluator(term: Term, pv: MajorProtocolVersion) {
    lazy val evaluator: UplcEvaluator =
        UplcCompiler.compile(term, protocolVersion = pv).withFallback()
}

val evaluators = ConcurrentHashMap[(ScriptHash, MajorProtocolVersion), LazyEvaluator]()

/** The JIT evaluator of a Plutus V3 or V4 script; None for the CEK machine to evaluate. */
def evaluatorFor(script: PlutusScript, pv: MajorProtocolVersion): Option[UplcEvaluator] =
    if script.language != Language.PlutusV3 && script.language != Language.PlutusV4 then None
    else
        val entry = evaluators.computeIfAbsent(
          (script.scriptHash, pv),
          _ => LazyEvaluator(script.program.term, pv)
        )
        Some(entry.evaluator)
```

The map creates the `LazyEvaluator` under its lock, but the compile runs outside it, so a compile
of several seconds does not block the other scripts. If a compile throws, the next call compiles
again. Evaluate a V4 script with a V4 `PlutusVM`.

Compile only the scripts that run often: in the mainnet sample above, 6 % of the scripts did 93 %
of the runs. The other scripts are faster on the CEK machine, because they never repay the compile.

## Performance

The [benchmark site](https://plutus-vm-bench.lantr.io/?timing=eval) compares 15 Plutus VMs on the
same 89 contracts. The JIT is about 1.8× faster than the Scalus CEK machine and about 3× faster
than the Haskell VM, and up to 9× on some contracts. On a few contracts the interpreters still
win: on some tiny ones, and on the largest ones, where the JVM needs more warm-up than the
benchmark gives.

## Learn more

- [How the JIT works](/docs/smart-contracts/how-the-jit-works): the code generator, the budget,
  the fallback.
- [Blazing-Fast Lambda Calculus via
  Metaprogramming](https://lantr.io/blog/blazing-fast-lambda-calculus-scala-3-staging/): the
  ScalaDays 2026 talk, written up.
- [tiny-uplc-jit](https://github.com/lantr-io/tiny-uplc-jit): the same idea in a few hundred lines.
- [`scalus-uplc-jit-compiler`](https://github.com/scalus3/scalus/tree/master/scalus-uplc-jit-compiler):
  the source.
