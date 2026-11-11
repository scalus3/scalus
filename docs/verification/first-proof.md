Source: https://scalus.org/docs/verification/first-proof

# Your First Proof

This page proves a rule of a real contract, the HTLC validator of the Scalus examples, for **every
script context the ledger can pass it**: any number of inputs, outputs and signatures, any validity
range, any datum. It assumes the [setup](/docs/verification/setup) is done. The suite it builds is
part of
[`HtlcVerificationTest`](https://github.com/scalus3/scalus/blob/master/scalus-examples/jvm/src/test/scala/scalus/examples/htlc/HtlcVerificationTest.scala)
in the Scalus repository.

## The Contract

A hashed timelock contract locks funds for a receiver, who can take them by revealing a secret
before a timeout. After the timeout, the committer can take them back. The datum names both
parties, the hash of the secret and the timeout. This is the refund branch of
[`HtlcValidator`](https://github.com/scalus3/scalus/blob/master/scalus-examples/jvm/src/main/scala/scalus/examples/htlc/HtlcValidator.scala):

```scala
case Action.Timeout =>
    val validFrom = tx.validFromOrFail(ValidRangeMustBeBound)
    // validFrom is inclusive, hence 10 <= 10 is correct
    require(config.timeout <= validFrom, InvalidCommitterTimePoint)
    require(tx.isSignedBy(config.committer), UnsignedCommitterTransaction)
```

The rule to prove: **a refund is rejected unless its validity range starts at the timeout or
later**, whoever signs it.

## The Suite

A suite of proofs is a ScalaTest suite with `LeanProofsSuite` mixed in. `import proofs.*` brings in
its verifier and `check`.

```scala filename="HtlcProofsTest.scala" copy showLineNumbers
package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v3.{IntervalBoundType, ScriptContext, ScriptInfo}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.LeanProofsSuite

class HtlcProofsTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*
    import HtlcProofsTest.*

    // The validator's entry point: the program the ledger runs, applied to a script context.
    private val validator =
        FunctionDef.named("htlc", (context: Data) => HtlcValidator.validate(context))
    verifier.addFunction(validator)

    private val refundRejectedBeforeTheTimeout = verifier.statement(
      "htlc.refund.rejected_before_the_timeout",
      forAllData[ScriptContext](ctx =>
          (isRefund(ctx) && !startsFromTheTimeout(ctx)) ==>: fails(validator, ctx.toData)
      )
    )

    /** A negative control, declared with `refute`: it must be refuted. */
    private val refundRejectedAtTheTimeout = verifier.refute(
      "htlc.refund.rejected_at_the_timeout",
      forAllData[ScriptContext](ctx =>
          (isRefund(ctx) && !startsAfterTheTimeout(ctx)) ==>: fails(validator, ctx.toData)
      )
    )

    test("a refund is rejected unless its validity range starts at the timeout or later") {
        check(refundRejectedBeforeTheTimeout, budget = 4000)
        // at the timeout itself, a refund is accepted
        check(refundRejectedAtTheTimeout, budget = 4000)
    }
}

/** The premises: they read the context as the validator does. */
object HtlcProofsTest {

    /** Whether `ctx` is of spending an output that has a datum. */
    inline def spendsWithDatum(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, prelude.Option.Some(_)) => true
        case _                                                    => false

    /** The datum of the output `ctx` spends. */
    inline def datumOf(inline ctx: ScriptContext): Data = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, datum) => datum.getOrFail("no datum")
        case _                                   => prelude.fail("not a spend")

    /** The datum of the output `ctx` spends, read as the validator reads it. */
    inline def configOf(inline ctx: ScriptContext): Config = datumOf(ctx).to[Config]

    /** A spend of an output with a datum, with the redeemer `Timeout`. */
    inline def isRefund(inline ctx: ScriptContext): Boolean =
        spendsWithDatum(ctx) && (ctx.redeemer.to[Action] match
            case Action.Timeout   => true
            case Action.Reveal(_) => false)

    /** The validity range starts at a time, the datum's timeout or later. */
    inline def startsFromTheTimeout(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.validRange.from.boundType match
            case IntervalBoundType.Finite(time) => configOf(ctx).timeout <= time
            case _                              => false

    /** The validity range starts at a time after the datum's timeout. */
    inline def startsAfterTheTimeout(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.validRange.from.boundType match
            case IntervalBoundType.Finite(time) => configOf(ctx).timeout < time
            case _                              => false
}
```

- **`validator`** is the entry point, `HtlcValidator.validate`, registered as a function of the
  verifier. A statement that calls it is about its compiled UPLC.
- **`forAllData[ScriptContext]`** ranges over every `Data` encoding of a `ScriptContext`, as the
  ledger passes it to the script. There is no limit on the number of inputs, outputs or signatures.
- **`premise ==>: claim`** is an implication. The premise picks the contexts the rule is about:
  refunds whose range does not start at the timeout or later. The claim, `fails(validator, ...)`,
  says that the validator rejects them.
- **The premises are `inline def`s** that read the context as the validator does. They live in an
  `object`: an `inline def` of the suite would leave a reference to the suite's `this` in the
  expression that the plugin compiles.
- **`verifier.statement`** declares a statement that must be proven, and **`verifier.refute`** one
  that must be refuted. `check` runs it, and fails the test when the result is not the expected
  one. The name is the statement's identity: its result is kept under that name.

## Why Add a Negative Control

A proof of a statement that says less than you think passes as well as a good one: a premise that
is never true, for instance, makes any claim provable. A negative control is a statement close to
the one you prove that must be refuted. It shows that the check sees the code.

Here, the control moves the boundary by one: "a refund is rejected unless its range starts *after*
the timeout". The validator accepts a refund that starts exactly at the timeout, so the solver finds
such a context, signed by the committer, and Scalus replays it on its CEK machine. The control is
refuted, and the suite passes.

## Run It

```bash copy
sbt "verify *HtlcProofsTest"
```

`verify` builds the Lean workspace, then runs the suite with Lean required: a statement that cannot
be checked fails the run. Without a test name, it runs every suite of proofs in the project. See
[sbt Commands](/docs/verification/sbt-commands).

`budget = 4000` is the number of steps Lean's machine may run each program of the statement.
`check(statement)` without a budget searches for one, and keeps the one it finds with the result.
[Tuning Your Proof](/docs/verification/tuning) explains the budget, and what to do when a check
does not finish.

## Why No Limit Is Needed Here

The validator checks the validity range before it searches the signatories. Every refund that the
premise picks fails at the time check, before the validator reads the signatories. So Lean decides
the statement for every context.

The rule "nobody but the committer can refund" is different: the validator searches the
signatories to decide it, and the search is as long as the list. Such a statement may need its data
limited, as [Tuning Your Proof](/docs/verification/tuning#limit-the-data) shows.

## A Function, Too

Any `@Compile` function can be the subject of a statement, not only a validator:

```scala
@Compile
object RoundedMagnitude {
    /** Half the magnitude, rounded up: -5 -> 3, 0 -> 0, 4 -> 2. */
    def half(x: BigInt): BigInt = {
        val magnitude = if x < 0 then -x else x
        (magnitude + 1) / 2
    }
}

private val neverNegative =
    verifier.statement("half.never_negative", forAll[BigInt](x => half(x) >= 0))
```

`half(x)` inside a statement is not a JVM call: the plugin compiles each Boolean test, with the
functions it calls, into a UPLC program. The complete suite, with a counterexample and an
inconclusive check, is
[`BigIntVerificationTest`](https://github.com/scalus3/scalus/blob/master/scalus-verification/src/test/examples/BigIntVerificationTest.scala).

## Writing Statements

A statement is built from quantifiers, connectives and Boolean tests. A Boolean test is ordinary
Scala, with `if`, `match` and local `val`s, as long as Scalus can compile it.

| Form                                                    | Meaning                                                        |
| ------------------------------------------------------- | -------------------------------------------------------------- |
| `forAll[A](x => ...)`, also with 2 and 3 variables      | for every `x` of type `A`                                      |
| `forAllData[A](x => ...)`                               | for every `Data` encoding of a value of `A`, as the ledger passes it |
| `exists[A](x => ...)`, `existsLet(witness)(x => ...)`   | there is an `x`; `existsLet` names it, and is cheaper          |
| `p ==>: q`                                              | `p` implies `q` (right-associative)                            |
| `p && q`, `p \|\| q`, `!p`, `p <=> q`                     | and, or, not, if and only if                                   |
| `succeeds(f, args)`, `fails(f, args)`                   | the function returns, or fails, on `args`                      |
| `call(f, args)(r => ...)`                               | the function returns, and its result `r` satisfies the body    |
| `whenReturns(f, args)(r => ...)`                        | where the function returns, its result satisfies the body      |
| `contract(f)(expects = ..., ensures = ...)`             | a precondition and a postcondition                             |

Variables can be `BigInt`, `Boolean`, `ByteString`, `String`, `Data`, lists and maps, and case
classes and enums of them. `f` is a `FunctionDef`, registered with `verifier.addFunction`:
`FunctionDef(Obj.method)` for a method of a `@Compile` object, or
`FunctionDef.named("name", (x: BigInt) => ...)` for any other function.

### Contracts in the Function's Body

A function can state its own precondition and postcondition with `Spec`, from the prelude. The
clauses are removed before lowering, so the script's bytes and hash do not change:

```scala
import scalus.cardano.onchain.plutus.prelude.Spec
import scalus.cardano.onchain.plutus.prelude.Spec.ensuring

@Compile
object Vault {
    def clamp(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        (if x < lo then lo else if x > hi then hi else x).ensuring(r => lo <= r && r <= hi)
    }
}
```

`FunctionContract.inSource(FunctionDef(Vault.clamp))` reads the clauses as a contract to prove.
For a validator, `Spec.ensures` at the head of a handler states what holds wherever it accepts,
and `verifier.guarantees(function)` declares one statement for each clause. A `LeanProofsSuite`
whose tests all passed fails if a stated clause is neither proved nor declared open
(`verifier.open(statement, reason)`).

Next: [tune a proof that does not finish](/docs/verification/tuning).
