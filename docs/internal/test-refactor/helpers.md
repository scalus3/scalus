# Helper catalog (P3, Agent H)

Status: proposed, 2026-10-27. Every helper in this file has status `proposed` until the owner
approves it (`[P3-11]`). Inputs: the P2 digests in `p3/findings-by-scope/`, `helper-mentions.jsonl`,
`cross-cluster.jsonl`, `budget-inventory.csv`, `shared-helpers.csv`, `repeated-defs.csv`, and the
helper sources cited below. Counts come from `grep` over `**/src/test/**/*.scala` (regex per count
in Appendix A) or from the evidence lines of the finding ("evidence sites").

The owner decisions D1 to D4 in the spec (`[P3-11a]`) supersede the names in this draft. Example:
the D3 revision (2026-11-01) renames `checkResult` to `assertSuccess` and `assertFailure`, and
`submitPinned` to `submitOk`.

## 1. Summary

| Test kind | Layer | Lives in | Platforms |
|---|---|---|---|
| Pure off-chain unit | `AnyFunSuite`, ScalaCheck, `ArbitraryInstances` | each module | all |
| On-chain prelude and stdlib eval | `EvalTestKit` (with `BudgetPins`) | scalus-core shared test, copied into testkit main | all |
| Validator unit (Program applied to a ScriptContext) | `ScalusTest` (with `BudgetPins`), `TestUtil` | testkit main | JVM, JS |
| Transaction and Emulator scenario | `ScalusTest`, `TestUtil.emulatorWith`, `Emulator.withAddresses`, `Scenario`, `ContractScalaCheckCommands` | testkit | JVM (Emulator also JS) |
| Ledger rules | `ValidatorRulesTestKit` with new builders, `CertsTestKit` | ledger shared test | JVM, JS |
| Property | `checkEval`, new `checkEvalAgrees`, `forAll` | `EvalTestKit` | all |
| Generators | the 6 `ArbitraryInstances` traits, `ArbitraryDerivation` | scalus-core shared test, copied | all |
| Budget pins | `BudgetPins`, `BudgetTable`, `Pin`, `Generation`, `<Suite>Budgets.scala`, `BudgetTableWriter`, `sbtn updateBudgets` | scalus-core shared test (writer in jvm), copied | all (writer JVM) |

- Stays: `EvalTestKit`, `ScalusTest`, `TestUtil`, `Party`, `ValidatorRulesTestKit`, the `ArbitraryInstances` chains, `Scenario`, `ContractScalaCheckCommands`, `Benchmark`.
- Merges: three on-chain assertion families become one (`EvalTestKit` for code, `ScalusTest.checkResult` for a `Result`); `assertEvalWithBudget*`, CAPE maps and `baseline` literals become budget tables.
- Goes: `EvalTestDsl`, `ResultAssertions`, `Evaluator`, `BaseValidatorTest(+Jvm)`, `update-budgets.py`, 4 private `Benchmark` copies, local `genesisHash`/`given`/`createProvider`/`runValidator`/`assertSubmit*` copies.

## 2. Budget-pin API

### 2.1 Decisions

**Storage: one generated, committed Scala table per suite**, `<Suite>Budgets.scala`, in the suite's
directory and package. This confirms the spec's recommendation (spec, "Budget pins", implementation
status). Reasons:

1. Scala source compiles on JVM, JS and Native. Resource files do not reach JS tests, and
   `scalus-core` shared suites hold 435 of the 455 exact pins (`budget-inventory.csv`:
   `scalus-core/shared` has 415 `exact` and 20 `exact+fee` rows).
2. A table is data keyed by string. `[BP-8a]` (update by key) and `[BP-8c]` (unused key fails) are
   set lookups. Inline literals need source rewriting by value; `update-budgets.py` already shows the
   cost (471 lines, and the shared-literal stomp described in the spec).
3. A budget commit changes only table files. The reviewer sees every drift of a suite in one diff.

Rejected: inline literals (status quo, 810 `ExUnits(` literals in 79 files, A-1); resource files
(not on JS); one repository-wide table (two active units would edit one file, `[P4-5a]`).

**Location: `scalus-core/shared/src/test/scala/scalus/testing/budget/`, copied into
`scalus-testkit` main.** The deciding constraint is not MiMa:

- `scalus-testkit` has no `mimaPreviousArtifacts`. The project block is `build.sbt:667-757`; MiMa is
  set only for scalus (`build.sbt:474`), the bloxbean library (`build.sbt:865`) and the ledger
  (`build.sbt:1043`). The task brief says testkit is MiMa-checked; the build says it is not. Testkit
  is still a published artifact (`publishOnlyLts`, `build.sbt:672`), so removed members get a
  `@deprecated("...", "1.5.0")` cycle, not a silent delete.
- `scalus-core` tests cannot depend on `scalus-testkit` (`findings.md:172-173`, the F-8 correction).
  So the pin core must sit in scalus-core test sources. The build already copies
  `scalus/testing/kit/EvalTestKit.scala` and the six `ArbitraryInstances` files into testkit main
  (`build.sbt:691-707`), and `KeyPairGenerator.scala` per platform (`build.sbt:732-739`, `749-756`).
  The pin core joins the shared list; the writer joins the JVM list. User projects and ejected
  examples then get the API from the published jar.
- The same constraint explains the 4 private `Benchmark` tags (H-56) and moves
  `ScalaCompilerVersion` into core test sources (H-04).

### 2.2 Data types

File `scalus-core/shared/src/test/scala/scalus/testing/budget/Pin.scala`:

```scala
package scalus.testing.budget

/** One pinned measurement. `fee` is lovelace. */
enum Pin:
    /** One script evaluation. `fee` = `ExUnits.fee(CardanoInfo.mainnet.protocolParams.executionUnitPrices).value`. */
    case Eval(memory: Long, steps: Long, fee: Long)
    /** One balanced transaction. `memory`/`steps` = sum of all redeemer ExUnits; `fee` = `tx.body.value.fee.value`. */
    case Tx(memory: Long, steps: Long, fee: Long)
    /** A script size in bytes (`[BP-5]`). */
    case Size(bytes: Long)

/** A compiler generation: the oldest compiler line that produces the same UPLC. */
enum Generation(val minMajor: Int, val minMinor: Int):
    case Scala33 extends Generation(3, 3)   // 3.3.x LTS
    case Scala38 extends Generation(3, 8)   // 3.8.x and 3.9.x (ScalaCompilerVersion.scala:33 verified them equal)

object Generation:
    /** The greatest case whose (major, minor) is at most the version's; parses the numbers, so `3.10.x` sorts after `3.8.x`. */
    def of(version: String): Generation
    /** The generation of the compiler that compiles the call site. Inline on purpose: it must expand
      * in the suite's own compilation unit, not once in the published jar (see 2.5). */
    transparent inline def current: Generation = of(ScalaCompilerVersion.version)
```

`Pin.Eval` and `Pin.Tx` carry the same three fields with a different fee meaning. Two cases keep the
failure message and the writer honest about what moved. `Option` fields were rejected: a tx-only pin
would carry three `None`s, and a Java caller of a test table does not exist.

File `BudgetTable.scala`:

```scala
abstract class BudgetTable:
    /** The generation this table was compiled with. The writer emits
      * `override val generation: Generation = Generation.current` as the first line of every table,
      * so the macro expands in the suite's project, with the suite's compiler. */
    def generation: Generation
    /** One value for every generation. */
    protected final def pin(key: String, value: Pin): Unit
    /** One value for one generation. A key has either one plain row or one row per generation. */
    protected final def pin(key: String, generation: Generation, value: Pin): Unit
    final def get(key: String, generation: Generation): Option[Pin]
    final def keys: Set[String]
    final def rows: Seq[BudgetTable.Row]              // for the writer and the report

object BudgetTable:
    final case class Row(key: String, generation: Option[Generation], value: Pin)
    /** Migration default: pins are recorded but not checked. Removed at the end of step 5 (2.11). */
    object unpinned extends BudgetTable:
        val generation: Generation = Generation.Scala33   // never consulted: unpinned skips every lookup
```

### 2.3 Test-side API

File `BudgetPins.scala`. Two traits, because `ScalusTest` cannot carry a ScalaTest self-type: 8
companion objects extend it (`object AmmTest extends ScalusTest`, `AmmTest.scala:373`; also
`PricebetValidatorTest.scala:644`, `DecentralizedIdentityTest.scala:533`,
`AuctionValidatorTest.scala:103`, `EditableNftValidatorTest.scala:587`, `LinkedListTest.scala:983`,
`CrowdfundingEmulatorTest.scala:71`, `LotteryValidatorTest.scala:930`; A-2). No object extends
`EvalTestKit`.

```scala
/** Pin primitives and the table. No self-type, so companion objects can keep extending ScalusTest. */
trait BudgetPins {
    /** The suite's table. Override with `<Suite>Budgets`. */
    protected def budgetTable: BudgetTable = BudgetTable.unpinned

    // Primitives. Key = current test name, or "<test name> / <label>" (2.4).
    protected final def pinEval(budget: ExUnits)(using source.Position): Unit
    protected final def pinEval(label: String, budget: ExUnits)(using source.Position): Unit
    protected final def pinTx(tx: Transaction)(using source.Position): Unit
    protected final def pinTx(label: String, tx: Transaction)(using source.Position): Unit
    protected final def pinSize(script: PlutusScript)(using source.Position): Unit
    protected final def pinSize(label: String, bytes: Long)(using source.Position): Unit
    /** Runs `body` with pinning off. For a fixed-input evaluation inside a property that cannot use `checkEval`. */
    protected final def unpinned[A](body: => A): A
    // the current test name, set by BudgetPinsSuite; a pin with no current test fails:
    // "no current test: mix BudgetPinsSuite into the suite class"
    // lookups use budgetTable.generation, never Generation.current (2.5)
}

/** ScalaTest hooks. Stackable; the suite class mixes it in next to ScalusTest or EvalTestKit. */
trait BudgetPinsSuite extends org.scalatest.TestSuiteMixin { this: org.scalatest.TestSuite & BudgetPins =>
    abstract override protected def withFixture(test: NoArgTest): Outcome   // sets the test name, counts tests
    abstract override def run(testName: Option[String], args: Args): Status // update mode, unused-key check, writer
}
```

`EvalTestKit extends BudgetPins, BudgetPinsSuite` (no companion objects, so the self-type is
free). `ScalusTest extends BudgetPins` only; a suite class that pins writes
`class AmmTest extends AnyFunSuite, ScalusTest, BudgetPinsSuite`. The `budget-pin` unit adds the
mixin when it sets `budgetTable`; until then `BudgetTable.unpinned` makes every primitive a no-op.
`EvalTestKit` (H-05) and `ScalusTest` (H-06) call the primitives; a test calls them directly only
when it already holds an `ExUnits` (CAPE harness, H-07) or a `Transaction` (tx tests).

`EvalTestKit` after the change (signatures only; bodies as today plus one `pinEval` call):

```scala
trait EvalTestKit extends Assertions, ScalaCheckPropertyChecks, ArbitraryInstances, BudgetPins, BudgetPinsSuite { this: TestSuite =>
    // every fixed-input assertion pins; the label overload is for a test with more than one evaluation
    protected final inline def assertEval(inline code: Boolean)(using PlutusVM, Options): Unit
    protected final inline def assertEval(label: String)(inline code: Boolean)(using PlutusVM, Options): Unit
    protected final inline def assertEvalEq[T: Eq](inline code: T, inline expected: T)(using PlutusVM, Options): Unit
    protected final inline def assertEvalEq[T: Eq](label: String)(inline code: T, inline expected: T)(using PlutusVM, Options): Unit
    protected final inline def assertEvalEq[A1, T: Eq](inline code: A1 => T, arg1: A1, inline expected: T)(using ...): Unit
    protected final inline def assertEvalEq[A1, T: Eq](label: String)(inline code: A1 => T, arg1: A1, inline expected: T)(using ...): Unit
    protected final inline def assertEvalNotEq[T: Eq](inline code: T, inline expected: T)(using ...): Unit            // + label overload
    protected final inline def assertEvalFails[E <: Throwable: ClassTag](inline code: Any)(using ...): Unit          // + label overload; pins the failing run ([BP-1])
    protected final inline def assertEvalFailsWithMessage[E <: Throwable: ClassTag](expectedMessage: String)(inline code: Any)(using ...): Unit
    protected final inline def assertEvalFailsWithMessage[E <: Throwable: ClassTag](label: String, expectedMessage: String)(inline code: Any)(using ...): Unit
    protected final inline def assertEvalSuccess(inline code: Any)(using ...): Unit                                 // + label overload
    // property: unchanged, never pins ([BP-1a])
    protected inline final def checkEval[A1](inline f: A1 => Boolean, ...): Assertion                                // EvalTestKit.scala:484-507, 509-539, 541-577
    // term level: unchanged (terms built by hand are not Scalus code; the suite pins with pinEval if needed)
    protected def assertTermEvalEq(a: Term, b: Term)(using PlutusVM): Unit                                          // EvalTestKit.scala:471-474
}
```

The one-argument forms keep the "argument passed as Data" semantics of today's
`assertEvalWithBudget[A1, T]` (`EvalTestKit.scala:153-187`), so constant folding cannot remove the
work (findings C22-8, C22-9 show why that matters).

`ScalusTest` after the change:

```scala
trait ScalusTest extends ArbitraryInstances, Assertions, BudgetPins {
    protected def checkResult(expected: (String | Unit, Option[ExUnits]), actual: Result): Unit       // keeps ScalusTest.scala:230-265; now also pinEval(actual.budget)
    protected def checkResult(label: String, expected: String | Unit, actual: Result): Unit          // new
    protected def submitOk(emulator: Emulator, tx: Transaction): TransactionHash                     // asserts Right; no pin (setup transactions)
    protected def submitPinned(emulator: Emulator, tx: Transaction): TransactionHash                 // submitOk + pinTx(tx)
    protected def submitPinned(label: String, emulator: Emulator, tx: Transaction): TransactionHash
    protected def assertSubmitFails(emulator: Emulator, expectedLog: String)(tx: Transaction): Unit  // H-17
}
```

`success(budget)` and `failure(message, budget)` (`ScalusTest.scala:224-228`) stay one release as
`@deprecated`; `checkResult` still compares a given budget, then pins.

### 2.4 Keys

- Key grammar: `key := testName | testName + " / " + label`. The test name comes from
  `withFixture` (`NoArgTest.name`). A label is required when a test pins more than once; the second
  unlabelled pin in one test fails with `key "<test>" pinned twice: add a label or use checkEval`.
  The same check catches `assertEval*` inside `forAll` (`[BP-1a]`): the key repeats with a different
  value.
- `[BP-6b]` (one value per `Options`): a test that evaluates under several `Options` sets
  `given Options` locally per block and names the options in the label, for example
  `assertEvalEq("sum @ noOptimize")(...)`. No `assertEvalWithBudgets` variant list is needed: all
  288 call sites today pass a one-element `Seq(compilerOptions -> ...)` (A-3; finding C23-9).
- Test names must not embed measured values. 15 suites do (`test(s"... ${size} bytes")`, A-4), for
  example `HtlcTest.scala:63` and `AmmTest.scala:34`. Those tests get a fixed name and a `pinSize`
  call; the `budget-pin` units carry this rename.
- Shared abstract suites (`SimpleLoweringTestBase`, H-32's trait) declare `budgetTable` abstract;
  each concrete suite has its own table. Two suites never share a table.

### 2.5 Generations (`[BP-6]`, `[BP-6a]`)

- Today `ScalaCompilerVersion.version` reads `-Dscalus.test.scalaVersion`
  (`ScalaCompilerVersion.scala:24`), and only `scalusExamples` sets it (`build.sbt:808-811`). JS and
  Native runs, and scalus-core JVM runs, see the default `"3.3.8"`.
- Proposal: `ScalaCompilerVersion.version` becomes a `transparent inline def` backed by a macro that
  returns the version of the compiler that compiles the call site. The macro reads
  `dotty.tools.dotc.config.Properties.versionNumberString` by reflection (the macro runs inside the
  compiler JVM on every platform), falls back to `sys.props("scalus.test.scalaVersion")`, then to
  `"3.3.8"`. No build change, works with published jars only (ejected examples), works on JS and
  Native because the result is a literal.
- Expansion site: an inline def expands where it is called. A `val` in the published jar would be
  expanded once, with 3.3.8 (`publishOnlyLts`, `build.sbt:672`), and every user would pin as
  `Scala33`. So `Generation.current` is itself `transparent inline`, and the only call sits in the
  generated `<Suite>Budgets` object (`override val generation = Generation.current`, 2.8), which
  the suite's project compiles with the suite's compiler. `BudgetPins` reads `budgetTable.generation`.
- Both fallbacks run at macro-expansion time inside the compiler JVM, so the `sys.props` fallback
  sees the sbt server's property, not the forked test JVM's. That is why the acceptance test below
  compares with the test JVM's `-D` value: it fails if the fallback fired.
- Acceptance test (unit 1): `ScalaCompilerVersionTest` in scalus-core shared test asserts
  `ScalaCompilerVersion.version == sys.props("scalus.test.scalaVersion")` on the JVM, with
  `-Dscalus.test.scalaVersion=${scalaVersion.value}` added to every forked JVM test config. On JS and
  Native the same suite asserts only that the value parses as `3.<minor>`. The pinned suites on
  CI-JS and CI-Native are the second proof: a wrong generation fails every table.
- Fallback if the reflection proves brittle (Q1): `sbt-buildinfo` (already in
  `project/plugins.sbt`) generates `scalus.testing.budget.BuildGeneration` into `Test / sourceManaged`
  of every project with pinned tests, and `Generation.current` reads it through a small
  platform-neutral `ServiceLoader`-free hook (`Generation.override`). More build wiring, no
  compiler-internal name.
- Table rows: a key has one plain row, or one row per generation. When an update run on generation G
  finds a value that differs from a plain row, the writer splits the row: the old value becomes the
  row of every other generation, the new value the row of G. When all generation rows become equal,
  the writer merges them into one plain row. A missing row for the current generation fails like a
  missing key.
- `ScalaCompilerVersion.baseline(pre38, since38)` (21 calls in 9 files, A-5) stays `@deprecated`
  until its last caller is migrated by a `budget-pin` unit.

### 2.6 Failure message (`[BP-7]`)

Format: the suite, the key, the generation, then one line per pinned quantity with the expected
value, the actual value, and the signed percent relative to the expected value, one decimal.

```
ListTest: budget pin mismatch, key "range / range(1, 3)", generation Scala33
  memory   9 934 -> 9 800 (-1.3 %)
  steps    2 298 799 -> 2 250 000 (-2.1 %)
  fee      739 -> 728 (-1.5 %)
Run `sbtn updateBudgets` to accept the new values.
```

The expected values are the real pin of `ListTest.scala:180-187` with the fee at the mainnet prices
of `blockfrost-params-epoch-645.json:1044-1045` (`price_mem` 0.0577, `price_step` 0.0000721,
ceiling: `9934 * 0.0577 + 2_298_799 * 0.0000721 = 738.9`, so 739; the same formula gives
`HtlcTest.scala:87-88`'s `Coin(2222)` from `ExUnits(24831, 10_940_152)`). The actual values are
illustrative.

Unchanged quantities print `(+0.0 %)`. A `Pin.Tx` prints the same three lines with `fee` meaning
the transaction fee. A `Pin.Size` prints one `bytes` line. A missing key prints
`ListTest: no budget pin for key "range / range(1, 3)". Run sbtn updateBudgets.`

### 2.7 Strict checks (`[BP-8c]`)

- No pin for a key: the test fails (message above), unless the suite uses `BudgetTable.unpinned`
  (migration only, 2.11) or update mode is on.
- A key pinned twice in one run: the test fails (2.4).
- Unused keys: `run` compares the keys used in the run with `budgetTable.keys` after `super.run`.
  The check runs only when every selectable test of the suite ran, that is when the count of tests
  seen by `withFixture` equals `expectedTestCount(args.filter)`; that count already excludes tests
  dropped by tag (`-l scalus.testing.Benchmark` on CI) and by `-z`/`-t`, and those runs skip the
  check. On unused keys the suite aborts with
  `ListTest: 2 unused budget pins: "old name", "removed / case 3"`; sbt reports the aborted suite
  as a failure.

### 2.8 Update tool (`[BP-8]`, `[BP-8a]`, `[BP-8b]`)

- Trigger: the ScalaTest config map, `-- -Dscalus.budgets.update=true`. `run` reads
  `args.configMap`. This reaches forked and non-forked test JVMs alike; the ledger JVM project does
  not fork (no `Test / fork` in `build.sbt:1038-1256`), so a JVM system property would not reach
  `EmulatorTest` (46 sites). No build setting is needed.
- sbt command, with the queued-steps helper at `build.sbt:1316-1327`:
  `commandSequence("updateBudgets", "jvm/testOnly -- -Dscalus.budgets.update=true", "scalafmtAll")`
  and `commandSequence("updateBudgetsAll", "++3.3.8", "updateBudgets", "++3.9.0", "updateBudgets", "++3.3.8")`.
  `sbtn "scalusJVM/testOnly *ListTest -- -Dscalus.budgets.update=true"` updates one suite.
- In update mode a mismatch or a missing key does not fail. Each pin call records
  `(key, generation, expected, actual)`. After `super.run`, `BudgetTableWriter.write` rewrites the
  table: current-generation values replaced, new keys appended in first-use order, unused keys
  dropped only when every test ran, other generation columns kept.
- Writer location: `scalus-core/jvm/src/test/scala/scalus/testing/budget/BudgetTableWriter.scala`,
  copied into testkit jvm main. JS and Native get an object of the same name whose `write` throws
  `budget update runs on the JVM only` (the platform-object idiom of `KeyPairGenerator`).
- File path: the suite source path comes from the `source.Position` of the first pin call
  (`Position.filePathname`); the table is `<dir>/<SuiteSimpleName>Budgets.scala` in the suite's
  package. A new suite starts with an empty `object FooTestBudgets extends BudgetTable {}` next to
  it and `override val budgetTable = FooTestBudgets`; the first update run fills it.
- Emitted code, one row per line:

```scala
package scalus.cardano.onchain.plutus.prelude

import scalus.testing.budget.{BudgetTable, Generation, Pin}

/** Budget pins of [[ListTest]]. Generated by `sbtn updateBudgets`. Do not edit by hand. */
object ListTestBudgets extends BudgetTable {
    override val generation: Generation = Generation.current // expands with the suite's compiler (2.5)
    pin("singleton", Pin.Eval(memory = 1_032, steps = 216_462, fee = 76))
    pin("range / range(1, 3)", Pin.Eval(memory = 9_934, steps = 2_298_799, fee = 739))
}
```

  (Values from `ListTest.scala:36-43` and `:180-187`, fees computed as in 2.6. A per-generation
  example is in 2.12 (b).)

  Numbers use groups of three with `_` (same grouping as the failure message). Long keys wrap under
  scalafmt; the command ends with `scalafmtAll`, so the writer does not need to be format-perfect,
  and `ci-jvm` keeps its `scalafmtCheckAll` gate.
- Method size: ListTest will hold about 290 keys after `[BP-1]` (289 inventory sites), about 12 KB
  of constructor bytecode, or 24 KB with two generations. The JVM limit is 64 KB per method. The
  writer splits tables above 500 rows into `private def rows1(): Unit` chunks called from the
  constructor, so no table can hit the limit.

### 2.9 Commit message (`[BP-9]`)

The writer prints a report and writes it to `target/budgets/report.md`:

```
budgets: 12 pins updated, 2 added, 1 removed in 3 suites (Scala33)
ListTest: 10 updated; memory -1.8 %, steps -2.1 %, fee -2.0 % over the updated pins
  range / range(1, 3): memory 9 934 -> 9 800 (-1.3 %), steps 2 298 799 -> 2 250 000 (-2.1 %), fee 118 -> 116 (-1.7 %)
  ...
ValueTest: 2 added (no previous value): "fromList / two policies", "apply / token"
```

The coordinator pastes the first block into the commit body. Added keys are listed separately so a
rename does not hide a regression (2.13).

### 2.10 Where each piece lives

| Piece | File | Copied into |
|---|---|---|
| `Pin`, `Generation`, `BudgetTable`, `BudgetPins` | `scalus-core/shared/src/test/scala/scalus/testing/budget/` | testkit shared main (`build.sbt:695-704` list) |
| `ScalaCompilerVersion` (macro) | `scalus-core/shared/src/test/scala/scalus/ScalaCompilerVersion.scala` | testkit shared main; the testkit copy at `scalus-testkit/shared/src/main/scala/scalus/ScalaCompilerVersion.scala` is deleted |
| `BudgetTableWriter` | `scalus-core/jvm/src/test/...`, stubs in `js/` and `native/` | testkit jvm and js main (`build.sbt:736`, `753` lists) |
| `EvalTestKit` changes | `scalus-core/shared/src/test/scala/scalus/testing/kit/EvalTestKit.scala` | already copied (`build.sbt:703`) |
| `ScalusTest` changes | `scalus-testkit/shared/src/main/scala/scalus/testing/kit/ScalusTest.scala` | published directly |
| `updateBudgets` commands | `build.sbt` | – |
| `<Suite>Budgets.scala` | next to each suite | – |

### 2.11 Migration

| Today | After | Sites |
|---|---|---|
| `assertEvalWithBudget(code, expected, ExUnits(...))` (`EvalTestKit.scala:126-151`) | `assertEvalEq(code, expected)` (+ label) | 113 |
| `assertEvalWithBudgets(code, [arg,] expected, Seq(opts -> ExUnits))` (`:197-270`) | `assertEvalEq([label])(code, [arg,] expected)` | 288 |
| `assertEvalWithBudgetAndFee(..., ExUnits, Coin)` (`:279-347`) | same | 20 |
| `assertEvalWithinBudget(...)` (`:98-123`), upper bound | `assertEvalEq`, exact (`[BP-2]`) | 12 |
| `assertEval` / `assertEvalEq` / `assertEvalFails*` with no pin | unchanged call, now pinned; labels where a test has several | 167 + 120 + 96 |
| `ResultAssertions.assertBudgetEquals`, `EvalTestDsl.assertBudgetEquals` | retired (0 users) | 0 |
| `checkResult(success(ExUnits(...)), r)` (`ScalusTest.scala:222-265`) | `checkResult(success, r)` | 13 |
| `assert(result.budget == ExUnits(...))` by hand | `checkResult(success, result)` or `pinEval(result.budget)` | 53 |
| `ScalaCompilerVersion.baseline(pre38 = ..., since38 = ...)` | two table rows | 21 |
| CAPE `expectedBaseBudgets: Map[String, ExUnits]` + fee maps (`FibonacciCapeTest.scala:68-125`) | `CapeHarness.registerCases` + table (H-07) | 8 maps in 6 suites |
| `assertScriptBudget(tx, ExUnits, Coin)` (`AmmTest.scala:26-33`), `scalusPins` map (`DaBondPoolEmulatorTest.scala:298-306`) | `pinTx(label, tx)` | 2 suites |
| `assert(tx.body.value.fee == ...)` forced or asserted by hand | `pinTx` (`[BP-4]`) | 20 `tx-fee` + 9 `forced-fee` rows |
| `scripts/update-budgets.py` | `sbtn updateBudgets` | – |

Order: unit 1 (section 6) ships the API with `budgetTable = BudgetTable.unpinned` as the default,
so no existing suite changes behavior. The old `assertEvalWithBudget*` methods stay as deprecated
shims that ignore their literal and call the pinned form. Step 5 `budget-pin` units then, file by
file: set `budgetTable`, add labels, delete the literals, and the coordinator runs
`sbtn "<project>/testOnly <Suite> -- -Dscalus.budgets.update=true"` once per file (edit agents never
run sbt, `[AG-2]`; this is step 3 of the unit loop, before `testOnly`). The last `budget-pin` unit
deletes `BudgetTable.unpinned`, the shims and `update-budgets.py`, and adds a gate:
`scripts/budget_inventory.py` must report 0 `none` rows for fixed-input sites (the script learns the
new API: a site is `exact` when its suite overrides `budgetTable`).

### 2.12 Before and after

(a) `ListTest.scala:163-196`, test `range`, three evaluations:

```scala
// before
assertEvalWithBudgets(
  (n: BigInt) => List.range(n, 3),
  BigInt(1),
  Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
  Seq(compilerOptions -> ExUnits(memory = 9934, steps = 2_298799))
)
// after (ListTest)
assertEvalEq("range(1, 3)")(
  (n: BigInt) => List.range(n, 3),
  BigInt(1),
  Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
)
// after (ListTestBudgets.scala, generated; fee = ceil(9934 * 0.0577 + 2_298_799 * 0.0000721))
pin("range / range(1, 3)", Pin.Eval(memory = 9_934, steps = 2_298_799, fee = 739))
```

(b) Validator unit test, `BettingValidatorTest.scala:44` (test name) and `:85-91`:

```scala
// before
assert(result.isSuccess, "Script execution should succeed for initial minting")
assert(result.budget == ScalaCompilerVersion.baseline(
  pre38 = ExUnits(memory = 67489, steps = 23_432519),
  since38 = ExUnits(memory = 65329, steps = 22_719832)))
// after
checkResult(success, result)
// BettingValidatorTestBudgets.scala (fees at mainnet prices: 5583.6 -> 5584, 5407.6 -> 5408)
pin("Verify that a bet can be properly initialized", Generation.Scala33, Pin.Eval(memory = 67_489, steps = 23_432_519, fee = 5_584))
pin("Verify that a bet can be properly initialized", Generation.Scala38, Pin.Eval(memory = 65_329, steps = 22_719_832, fee = 5_408))
```

(c) Emulator transaction test that must pin `tx.body.value.fee`, `AmmTest.scala:26-33` (the local
helper) and its caller in `test("budget: deposit")`, `AmmTest.scala:163-178`:

```scala
// before (AmmTest.scala:169-177)
assert(provider.submit(depositTx).await().isRight, "deposit should succeed")
assertScriptBudget(
  depositTx,
  ExUnits(memory = 119051, steps = 50_745_771),
  ScalaCompilerVersion.baseline(pre38 = Coin(443550L), since38 = Coin(441174L))
)
// after
submitPinned(provider, depositTx)
// AmmTestBudgets.scala (the ExUnits are equal on both generations, the tx fee is not)
pin("budget: deposit", Generation.Scala33, Pin.Tx(memory = 119_051, steps = 50_745_771, fee = 443_550))
pin("budget: deposit", Generation.Scala38, Pin.Tx(memory = 119_051, steps = 50_745_771, fee = 441_174))
```

### 2.13 Risks

| Risk | How the design handles it |
|---|---|
| Test rename | Old key becomes unused (suite fails), new key is missing (test fails). `updateBudgets` moves the value; the report lists the new key under "added", so the reviewer compares the removed and added rows. |
| Two suites share a table | Impossible by construction: the table name is the suite name; shared traits declare `budgetTable` abstract. |
| Shared tests on JS and Native | Tables are Scala source and compile everywhere. `Generation.current` is a compile-time literal. A platform that computes a different budget fails its CI job, which is a real finding, not noise. JS and Native never write (`[BP-8b]`). |
| Property tests (`[BP-1a]`) | `checkEval` never pins. `assertEval*` inside `forAll` fails on the second iteration with "pinned twice". A property test keeps one fixed-input case, which pins as usual. |
| Differential tests (`[BP-1b]`) | JIT = CEK and Aiken = Scalus comparisons assert equality of two budgets and call no pin primitive. `DaBondPoolDifferentialTest` stays as it is; `DaBondPoolEmulatorTest` keeps its Scalus pins via `pinTx`. |
| A unit pins a regression by accident | Update mode never runs on CI. The commit body carries the per-key delta; the reviewer sees `+x %` lines. |
| Fee snapshot change | Every fee pin moves; one `updateBudgetsAll` run and one commit, as the spec requires. |
| Table drift between generations | A value is updated only for the generation that ran. `updateBudgetsAll` covers both; `ci-jvm` runs 3.3.8 and 3.9.0, so a stale column fails there. |

## 3. Helper catalog

Entry shape: id, name, status (`keep` / `change` / `new` / `retire`, all `proposed`), location,
signature, findings resolved, call sites replaced and how counted, one before/after from a real
test, and the existing helper it replaces or extends (`[P3-12]`). Findings from several clusters
that ask for the same concept are merged into one entry (`[P3-3]`).

### A. Budget pins

**H-01 `Pin`, `Generation`, `BudgetTable`** – `new`. Location and signatures: 2.2. `BudgetTable.generation`
is the one place where `Generation.current` is expanded (2.5). Resolves `[BP-1]`
to `[BP-9]`; findings C23-9, C22-8, C22-9 (pins that measure a constant are visible once the
argument form is the only form), C06-17. Replaces: the 810 `ExUnits(` literals in 79 test files
(A-1; about 20 are inputs, not pins, for example `PlutusScriptsTransactionMutatorTest`). Extends
nothing; replaces the literal-by-value model of `scripts/update-budgets.py`.

**H-02 `BudgetPins`** – `new`. Location and signature: 2.3. Resolves `[BP-7]`, `[BP-8c]`, `[AG-9]`.
Replaces the 53 hand-written `assert(result.budget == ExUnits(...))` sites in 25 files (A-6), the
fee assert at `HtlcTest.scala:88`, and the six `result.budget.fee(prices).value == N` asserts in
`ExprSizeAndBudgetTest.scala:125-220`. Before/after: 2.12 (b), (c). `BudgetPins` is mixed into
`EvalTestKit` and `ScalusTest`; `BudgetPinsSuite` into `EvalTestKit` and each pinning suite class.

**H-03 `<Suite>Budgets.scala` tables, `BudgetTableWriter`, `updateBudgets`** – `new`. Location:
2.10. Signature: `BudgetTableWriter.write(file: Path, pkg: String, suite: String, rows: Seq[BudgetTable.Row]): Unit`
(JVM). The writer emits `override val generation: Generation = Generation.current` as the first
line of every table (2.5, 2.8). Resolves `[BP-8]`, `[BP-8a]`, `[BP-8b]`, `[BP-9]`. Replaces
`scripts/update-budgets.py` (471 lines). Before/after: 2.8.

**H-04 `ScalaCompilerVersion`** – `change`. Moves from `scalus-testkit/shared/src/main/scala/scalus/ScalaCompilerVersion.scala`
to `scalus-core/shared/src/test/scala/scalus/ScalaCompilerVersion.scala` with the same FQN, copied
into testkit main. `version` becomes the compile-time macro of 2.5; `hasLeanDesugaring` stays;
`baseline` is `@deprecated("use the suite budget table", "1.5.0")`. Resolves `[BP-6a]`. Call sites:
21 `baseline` calls in 9 files (A-5), migrated by `budget-pin` units. Before/after: 2.12 (b).

**H-05 `EvalTestKit` pinned assertions** – `change`. Location `scalus-core/shared/src/test/scala/scalus/testing/kit/EvalTestKit.scala`.
Signatures: 2.3. Resolves C23-9 (288 one-element `Seq`s), C22-8, C22-9, C12-23 (the seven copied
`evaluateDebug match` blocks collapse into one `protected final def checkEvalResult(result: Result, expected: => Term, label: String): ExUnits`,
non-inline, called by the inline methods), C12-24 (`assertEvalFailsWithMessage` also checks the
message in the no-logs branch, `EvalTestKit.scala:438-443`). Call sites: 113 + 288 + 20 + 12 pinned
forms become `assertEvalEq`; 383 unpinned `assertEval*` sites (A-7) gain pins; the ones in a test
with several evaluations need a label (step 5), for example the 8 calls of `findUniqueOrFail`
(`ListTest.scala:46-104`).
Before/after: 2.12 (a). Replaces `assertEvalWithBudget`, `assertEvalWithBudgets`,
`assertEvalWithBudgetAndFee`, `assertEvalWithinBudget`, `nativeOpts` (`EvalTestKit.scala:51-52`,
an alias of `compilerOptions`).

**H-06 `ScalusTest.checkResult` and `submitPinned`** – `change`. Location
`scalus-testkit/shared/src/main/scala/scalus/testing/kit/ScalusTest.scala:222-265`. Signatures:
2.3. Resolves C06-9 (8 hand-written failure tails in `CrowdfundingValidatorTest.scala:96-104` and
siblings), C25-15 (`assertCase` re-implements `checkResult`), `[BP-4]`. Call sites: 13 `checkResult`
calls (A-8) keep their shape; 62 `assert(x.isFailure ...)` plus 45 `logs.exists(_.contains(...))`
pairs (A-9) become `checkResult(failure("..."), result)`; 20 `tx-fee` and 9 `forced-fee` inventory
rows become `submitPinned`/`pinTx`. Before/after:

```scala
// CrowdfundingValidatorTest.scala:97-104, before
val result = program.evaluateDebug
assert(result.isFailure, "Should fail when recipient doesn't sign")
assert(result.logs.exists(_.contains("Recipient must sign")), s"...")
// after
checkResult(failure("Recipient must sign"), program.evaluateDebug)
```

Extends `checkResult`; replaces `success(budget)` and `failure(msg, budget)`.

**H-07 `CapeHarness.registerCases`** – `new`. Location `scalus-examples/jvm/src/test/scala/scalus/examples/cape/CapeTestSuite.scala`
(next to `CapeHarness.run`, `:239-250`). Signature:
`def registerCases(suite: AnyFunSuite & BudgetPins, prefix: String, program: Program, cases: Seq[CapeCase])(using PlutusVM): Unit`
and `def evalInteger(program: Program, x: BigInt, label: String)(using PlutusVM): BigInt`.
Each case becomes `test(s"$prefix: ${c.name}")` that runs the harness, asserts the expected term,
and calls `pinEval(budget)`; every measurement case therefore needs a table row, which satisfies
C06-17. Resolves C06-16, C06-17. Replaces 8 copies of the loop and 4 name-keyed maps per suite in
6 suites (evidence sites, `FibonacciCapeTest.scala:40-66, 68-125`), and 5 `evalX` copies
(`FibonacciCapeTest.scala:145, 181`). Before/after: the loop at `FibonacciCapeTest.scala:40-52`
becomes `CapeHarness.registerCases(this, "CAPE base", baseProgram, baseSuite.cases)`, and the row
`"fibonacci_5" -> ExUnits(memory = 26957, steps = 7_071023)` plus `"fibonacci_5" -> Coin(2066)`
(`:73`, `:87`) becomes `pin("CAPE base: fibonacci_5", Pin.Eval(memory = 26_957, steps = 7_071_023, fee = 2_066))`.

### B. On-chain evaluation (`EvalTestKit`)

**H-08 `EvalTestKit` as the one on-chain family** – `keep`. Location as H-05. Recommendation: of the
three families, keep `EvalTestKit` (31 suites, A-2). `ResultAssertions` and `EvalTestDsl` have no
user outside testkit (grep over the repository: only the three testkit files reference them;
C12-6 confirms 0 users and 0 % coverage). `BaseValidatorTest` has 2 users (H-58). Adoption
findings: C08-17 (9 suites with local `given Options`/`PlutusVM`), C39-9 (3 suites), C10-14 (4 Eq
lowering suites, 32 tests), C04-18, C01-13, C26-10, C27-20, C01-8, C01-9, C04-19, C04-23. Call
sites: 68 local `given Options = Options(` in 55 files and 142 `given PlutusVM` in 105 files (A-10);
82 test files spell out `SirToUplcV3Lowering` options equal to `EvalTestKit.compilerOptions`
(`EvalTestKit.scala:39-44`). Before/after (`CompilePatternMatchingTest.scala:17-24`): delete the
8-line `given Options`/`given PlutusVM` block, add `with EvalTestKit`, override `compilerOptions`
only where it differs (`optimizeUplc = false`).

**H-09 `checkEvalAgrees`** – `new`. Location `EvalTestKit`. Signature:
`protected inline final def checkEvalAgrees[A1: Arbitrary, T](inline code: A1 => T, configParams: PropertyCheckConfigParam*)(using PlutusVM, Options)(implicit inline FromData[A1], inline ToData[A1], inline ToData[T], ...): Assertion`.
Compiles `(d: Data) => code(d.to[A1]).toData` once and asserts, per sample, that the evaluated term
equals `Term.Const(code(sample).toData)`; overloads for 2 and 3 arguments as `checkEval` has
(`EvalTestKit.scala:509-577`). Resolves C23-3 (the ListTest intrinsics are checked at
2-6 hand-picked inputs). Call sites: 0 today; C23-3 adds about 16 properties in ListTest. Extends
`checkEval` (`EvalTestKit.scala:484-507`), which only accepts a Boolean law. Before/after: the
JVM-only property `deleteFirst - property` (`ListTest.scala:2611-2622`, a `check` against the
Scala `patch` oracle) gains one line, `checkEvalAgrees((list: List[BigInt], value: BigInt) => list.deleteFirst(value))`,
which runs `deleteFirst` on the CEK per sample and compares with the JVM result.

**H-10 `assertEvalLogs` / `evalLogs`** – `new`. Location `EvalTestKit`. Signature:
`protected final inline def evalLogs(inline code: Any)(using PlutusVM, Options): Seq[String]` and
`protected final inline def assertEvalLogs(inline code: Any, expected: Seq[String])(using ...): Unit`
(pins the run). Resolves C04-17. Call sites: 9 tests in `LogTest.scala:31-42, 151-167` and
`DeprecatedPackageObjectsTest.scala:75-77` (evidence sites, 10). Before/after: the 11-line
`compile + toUplc + evaluateDebug + match Success(_, _, _, logs)` block becomes
`assertEvalLogs(code, Seq("a", "b"))`. Extends `assertEvalSuccess` (`EvalTestKit.scala:453-466`).

**H-11 `expectFailure[E]`** – `new`. Location `EvalTestKit`. Signature:
`protected final def expectFailure[E <: Throwable: ClassTag](result: Result): E`, fails with the
result and the actual exception class. Resolves C24-16. Call sites: about 12 `.asInstanceOf[Result.Failure].exception.isInstanceOf[X]`
casts and 6 three-arm matches (evidence sites in `ConfiguredEvaluationTest.scala:61-127`,
`CekSourcePosTest.scala:120-142`, `SourceTraceTest.scala:23-35`). Before/after:
`result.asInstanceOf[Result.Failure].exception.isInstanceOf[BuiltinError]` becomes
`expectFailure[BuiltinError](result)`. Extends nothing; `Result` has `success` but no `failure`
accessor (`Cek.scala:608-612`).

**H-12 `assertTermEvalEq(a, b, clue)`** – `change`. Location `EvalTestKit.scala:471-474`. Adds an
overload `(a: Term, b: Term, clue: => String)(using PlutusVM)`. Resolves T-18. Call sites: about 15
local `a.evaluate α_== b.evaluate` sites in the optimizer suites (evidence sites,
`CommonSubexpressionEliminationTest.scala:191-214`, `InlinerTest.scala:595-603`); 198 existing
callers in 3 files stay (A-11).

**H-13 `assertEvaluatesOnAllBackends`** – `new`. Location
`scalus-core/shared/src/test/scala/scalus/compiler/sir/SirTestDsl.scala` (with H-28). Signature:
`def assertEvaluatesOnAllBackends(sir: SIR, expected: Term, pv: MajorProtocolVersion)(using PlutusVM): Unit`.
Resolves C13-18 (the loop), C27-19 (`evalInt`/`evalBool` in `MutualRecursionTest.scala:84-98`),
C13-19, C01-6. Call sites: 5 `TargetLoweringBackend.values` loops in 2 files plus 3 local
`evalInt`/`evalBool` defs (A-12). Replaces the 12-line per-backend loop. Before/after:
`MutualRecursionTest.scala:84-98` (`evalInt(sir, backend)` with a `for backend <- TargetLoweringBackend.values`
loop and a `Success(Const(Integer(n)))` match in every test) becomes
`assertEvaluatesOnAllBackends(sir, BigInt(42).asTerm, vanRossemPV)`.

### C. Validator and transaction tests (`ScalusTest`, `TestUtil`)

**H-14 `TestUtil.runValidator`** – `new`. Location
`scalus-cardano-ledger/shared/src/test/scala/scalus/testing/kit/TestUtil.scala` (copied into
testkit main, `build.sbt:715-725`), next to `getScriptContextV3` (`TestUtil.scala:190`). Signature:
`extension (tx: Transaction) def runValidator(utxos: Utxos, purpose: RedeemerPurpose)(using CardanoInfo, PlutusVM): Result`.
It resolves the script from the purpose's credential, builds the V3 context, and evaluates with
`evaluateDebug`; the caller passes `emulator.utxos` (`EmulatorBase.scala:62`). Resolves C11-18,
C31-13, C34-12. Call sites: 8 local `runValidator` defs in 8 files (A-13), for example
`BettingTransactionTest.scala:103-124`, `VestingTransactionTest.scala:41-59`. Before/after:
`runValidator(provider, withdrawTx, vaultUtxo.input)` becomes
`withdrawTx.runValidator(provider.utxos, ForSpend(vaultUtxo.input))`. Extends `getScriptContextV3`;
replaces `ScalusTest.runWithDebug` at those sites (`ScalusTest.scala:77-80`, 25 calls stay valid).

**H-15 `ScalusTest.submitOk`** – `new`. Location `ScalusTest`. Signature: 2.3 (uses `Emulator.submitSync`,
`EmulatorBase.scala:108`, so it works on JVM and JS). Resolves C33-17 (`submitOk`), C38-12 (the
IT `submit ... match` frame, via `YaciTestContext.submit`). Call sites: 103
`assert(... submit(...)...isRight)` sites in 21 files (A-14). Before/after:
`assert(provider.submit(lockTx).await().isRight)` (`HtlcTest.scala:56`) becomes
`submitOk(provider, lockTx)`. Extends nothing.

**H-16 `ScalusTest.assertSubmitFails`** – `new`. Location `ScalusTest`, next to `assertScriptFail`
(`:42-57`). Signature: `protected def assertSubmitFails(emulator: Emulator, expectedLog: String)(tx: Transaction): Unit`,
matching `Left(SubmitError.ScriptFailure)` whose logs contain `expectedLog`, body from
`VestingTransactionTest.scala:87-102`. Merged from C25-8 (`ScalusTest`), C16-4 (`ScalusTest`),
C14-15 (an examples trait "because MiMa"); MiMa does not apply (2.1), so `ScalusTest` it is. Call
sites: 4 local defs (A-15; `LinkedListTest.scala:1053`, `AmmTest.scala:454`,
`VestingTransactionTest.scala:87`, `UpgradeableProxyTest.scala:87`) and their callers (for example
`LinkedListTest.scala:72, 187, 281`). Before/after: the private copy is deleted; `assertSubmitFails(provider, deinitTx)`
becomes `assertSubmitFails(provider, "list is not empty")(deinitTx)`. Extends `assertScriptFail`.

**H-17 `TestUtil.genesisHash`** – `keep`, adopt. Location `TestUtil.scala:55-56`. Merged from
C41-13, C17-15, C12-11, C20-18, C09-24, C33-16. Call sites: 48 local `val genesisHash` in 35 files
(A-16). Before/after: delete the local val, `import scalus.testing.kit.TestUtil.genesisHash`.

**H-18 `Party`** – `keep`, adopt. Location `scalus-cardano-ledger/shared/src/test/scala/scalus/testing/kit/Party.scala:27-61`.
Merged from C02-17, C19-16, C37-16, C41-14, C29-4, C02-16. Call sites: 94 `generateKeyPair()`
calls in 12 files (A-17), of which 73 are in the ledger rules suites; the rest are
`TransactionSignerTest` and the generator definitions themselves. Most sites follow with
`platform.blake2b_224(publicKey)` to make a key hash. Before/after:
`val (sk, pk) = generateKeyPair(); AddrKeyHash(blake2b_224(pk))` becomes `Alice.addrKeyHash`, and
`signEd25519(...)` becomes `Alice.signer.sign(tx)`. Then `ValidatorRulesTestKit.generateKeyPair`
(`ValidatorRulesTestKit.scala:66-67`) loses its last callers (H-27).

**H-19 `scalus.utils.await`** – `keep`, adopt. Location `scalus-cardano-ledger/jvm/src/main/scala/scalus/utils/FutureOps.scala:32`.
Resolves C12-11, C37-16. Call sites: 33 `Await.result` in 9 files (A-18); 854 `.await()` sites
already use it. Before/after: `Await.result(f, Duration.Inf)` becomes `f.await()`.

**H-20 `TestUtil.scriptLockedUtxo`** – `new`. Location `TestUtil`. Signature:
`def scriptLockedUtxo(script: PlutusScript, datum: Data)(using CardanoInfo): Utxo` plus an overload
without datum (`42.toData`); no default parameter (Tier 0). Resolves C09-25. Call sites: 3 verbatim
`createScriptLockedUtxo` defs (A-19). Before/after: the private def at `TxBuilderTest.scala:37-45`
is deleted; `createScriptLockedUtxo(script)` becomes `TestUtil.scriptLockedUtxo(script)`.

**H-21 `TestUtil.input` and `adaOutput`** – `new`. Location `TestUtil`, next to `genesisHash`.
Signatures: `def input(i: Int): TransactionInput = Input(genesisHash, i)`,
`def adaOutput(address: Address, ada: Long): TransactionOutput`. Resolves C20-19 (and the dead
copies in `DiagnosticReplayTest.scala:23-25`, C09-24). Call sites: 10 `def input(` and 5
`def adaOutput` (A-20). Extends `genesisHash`. Before/after: `TxBuilderCompleteTest.scala:49-52`
(`def input(index: Int) = Input(genesisHash, index)`, `def adaOutput(address, ada: Int)`) is deleted
and the suite imports `TestUtil.{input, adaOutput}`; call sites do not change.

### D. Emulator setup

**H-22 `TestUtil.genesisUtxos` and `emulatorWith`** – `new`. Location `TestUtil`. Signatures:

```scala
@scala.annotation.varargs def genesisUtxos(outputs: (Address, Value)*): Utxos        // Input(genesisHash, i) -> Output(address, value)
@scala.annotation.varargs def emulatorWith(outputs: (Address, Value)*): Emulator     // Emulator(genesisUtxos(outputs*), Context.testMainnet())
```

Merged from C17-16 (`genesisUtxos`), C33-16, C20-19 (`wallet(ada*)`), C37-16 (`emulatorWith`),
C12-12 (`aliceUtxos`), C11-7 (`emulatorWith(owner)`), and the 13 `createProvider` rows of
`repeated-defs.csv`. Call sites: 13 `def createProvider`, 8 `def createEmulator`, 1 `def mkEmulator`,
and 74 inline `Emulator(initialUtxos = Map(...))` blocks in 40 files (A-21). Before/after
(`UpgradeableProxyTest.scala:76-83`):

```scala
// before
private def createProvider(): Emulator = Emulator(initialUtxos = Map(
    Input(TestUtil.genesisHash, 0) -> Output(Alice.address, Value.ada(5000)),
    Input(TestUtil.genesisHash, 1) -> Output(Alice.address, Value.ada(5000)),
    Input(TestUtil.genesisHash, 2) -> Output(Bob.address, Value.ada(5000))), initialContext = Context.testMainnet())
// after
private def createProvider(): Emulator =
    emulatorWith(Alice.address -> ada(5000), Alice.address -> ada(5000), Bob.address -> ada(5000))
```

Extends `Emulator.withAddresses(addresses, value)`
(`scalus-cardano-ledger/jvm/src/main/scala/scalus/cardano/node/Emulator.scala:75-93`; JS at
`scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/Emulator.scala:69-78`), which covers
only equal values per address. When all values are equal,
`Emulator.withAddresses` stays the call (47 sites in 14 files).

### E. Ledger rules kits

*Superseded in part (2026-11-24):* D6 (`d6-rules-kits.md`) amends H-23, H-24, H-25 and H-50, and
moves H-38 to D8; where the two disagree, the D6 note wins.

**H-23 `ValidatorRulesTestKit` builders** – `new` members. Location
`scalus-cardano-ledger/shared/src/test/scala/scalus/cardano/ledger/rules/ValidatorRulesTestKit.scala`.
Signatures: `def scriptAddress(hash: ScriptHash): Address`, `def scriptUtxo(hash: ScriptHash, datum: Option[DatumOption], scriptRef: Option[ScriptRef]): Utxo`,
`def txSpending(inputs: TransactionInput*)(witnessSet: TransactionWitnessSet): Transaction`,
`def validate(validator: STS.Validator, utxos: Utxos, tx: Transaction): Either[TransactionException, Unit]`
(runs `Context() + State(utxos)`). Resolves C02-18, and the fixed-fixture asks of C19-11, C02-16.
Call sites: 219 `Context()` in 31 files (A-22), 7 suites in the scripts cluster. Before/after:
`ExactSetOfRedeemersValidatorTest.scala:37-62` (26 lines of body, witness set and context) becomes
`validate(ExactSetOfRedeemersValidator, Map(scriptUtxo(h, None, None)), txSpending(input)(witnessWith(script, redeemer)))`.

**H-24 `CertsTestKit`** – `new`. Location `scalus-cardano-ledger/shared/src/test/scala/scalus/cardano/ledger/rules/CertsTestKit.scala`,
a trait next to `ValidatorRulesTestKit`. Signatures: `def context(certState: CertState, epoch: Int, protocolMajor: Int): Context`,
`def certTx(certs: Certificate*)(withdrawals: (RewardAccount, Coin)*): Transaction`,
`def poolRegistration(operator: PoolKeyHash, cost: Coin, rewardAccount: RewardAccount): PoolRegistration`,
`def accountState(balance: Coin, pool: Option[PoolKeyHash], drep: Option[DRep]): AccountState`,
`def certState(accounts: Map[Credential, AccountState], pools: ..., dreps: ...): CertState`,
plus `keyDeposit`, `params`. Resolves C29-10. Call sites: 5 `def mkContext` (A-23) plus 4 `context`
copies and 9 `tx`/`runValidator` copies in the 9 certificate and withdrawal suites (evidence sites,
`CertsValidatorTest.scala:25-35`, `WithdrawalsTest.scala:52-68`). Before/after: the 11-line
`private def mkContext(certState: CertState): Context` at `CertsValidatorTest.scala:25-35` is
deleted; `mkContext(state)` becomes `context(state, epoch = 0, protocolMajor = 10)` from the kit.

**H-25 `ValidatorRulesTestKit.withNetwork`, `randomTransactionWithIsValidField`, `generateKeyPair`**
– `retire`. Location `ValidatorRulesTestKit.scala:14-61, 63-64, 66-67`. Resolves C29-6
(`withNetwork` has 1 user), C19-11 (13 users of the random tx; each replaced by a fixed
`Transaction(TransactionBody(...))`), C02-17 (key pairs replaced by `Party`, H-18). Retired after
their units land, in the same unit as H-23.

### F. Compiler, SIR and UPLC test DSLs

**H-26 `SirTestDsl`** – `new`. Location `scalus-core/shared/src/test/scala/scalus/compiler/sir/SirTestDsl.scala`.
Signatures: `int(n: BigInt): SIR`, `bool(b: Boolean): SIR`, `str(s: String): SIR` (via
`SIR.Const.*`, `SIR.scala:404-420`), `v(name: String, tp: SIRType): SIR.Var`,
`lam(params: SIR.Var*)(body: SIR): SIR`, `let(name: String, tp: SIRType, rhs: SIR)(body: SIR): SIR`,
`letRec(bindings: (SIR.Var, SIR)*)(body: SIR): SIR`, `app(f: SIR, a: SIR): SIR` (via `SirDSL.$`),
plus H-13. Resolves C13-18, C27-19, C13-19, C13-21, C39-7. Call sites: 8 suites with their own
`ann`/`intConst`/`v`/`mkLam`/`let` builders (`repeated-defs.csv`: `intConst` 4 files) and about 25
`SIR.LamAbs(v(..), body, List.empty, ann)` in `StaticArgumentTransformationTest` (evidence sites).
Not `SirDSL.lam` (C13-28). Before/after: `SIR.LamAbs(v("y", intTp), selfCall, List.empty, ann)`
(`StaticArgumentTransformationTest.scala:436`) becomes `lam(v("y", intTp))(selfCall)`.

**H-27 `Term.subterms` and `alphaEq` test extension** – `new`. Location
`scalus-core/shared/src/test/scala/scalus/uplc/TermTestDsl.scala` next to `TermDSL`. Signatures:
`extension (t: Term) def subterms: Iterator[Term]` (pre-order, all 10 cases) and
`extension (a: Term) infix def alphaEq(b: Term): Boolean` (`DeBruijn.deBruijnTerm(a) α_== DeBruijn.deBruijnTerm(b)`).
Resolves C27-17, C10-6, C10-21. Call sites: 4 hand-written walkers in 3 files and 4 `alphaEq`
copies (A-24); 73 hand-written `case Term.Apply|LamAbs|Force|Delay(` arms in 12 files are
candidates (A-25). Before/after: `collectVars` (`UnboxedNilTest.scala:37-51`) becomes
`term.subterms.collect { case Term.Var(n) => n.name }`.

**H-28 `SirTestVocabulary`** – `new`. Location `scalus-core/jvm/src/test/scala/scalus/compiler/SirTestVocabulary.scala`,
a trait with `sirData`, `sirBool`, `sirInt`, `sirString`, `sirByteString`, `sirVoid`, `sirList`,
`sirBuiltinList`, `sirPair`, the six `sirConst` overloads, `sirConstUnit`, `AnE`, `deadbeef`.
Resolves C08-15. Call sites: 2 verbatim blocks (`CompilerPluginToSIRTest.scala:31-52`,
`CompilerPluginBuiltinsToSIRTest.scala:26-56`). Owner choice in C08-15: the trait, or `SirDSL`'s
`Conversion` instances where the type is unambiguous; the trait is recommended because it keeps the
names the 2 suites already use. Before/after: the six `sirConst` overloads at
`CompilerPluginToSIRTest.scala:43-48` and their twins in `CompilerPluginBuiltinsToSIRTest` are
deleted; both suites add `with SirTestVocabulary`; call sites do not change.

**H-29 `SnippetCompilation.loadObjectMember`** – `new`. Location
`scalus-core/jvm/src/test/scala/scalus/compiler/plugin/SnippetCompilation.scala`. Signature:
`def loadObjectMember[T](outDir: Path, objectName: String, member: String): T`, opens, uses and
closes the `URLClassLoader`; the temp directory is deleted after use. Resolves C08-16. Call sites: 2
(`LargeSirStringTest.scala:22-31`, `SecondaryParamListCaseClassTest.scala:21-24`). Before/after:
`val loader = new URLClassLoader(Array(outDir.toUri.toURL), getClass.getClassLoader)` plus the
`MODULE$` reflection (`LargeSirStringTest.scala:22-31`) becomes
`SnippetCompilation.loadObjectMember[SIR](outDir, "Snippet", "sir")`.

**H-30 `SimpleLoweringStructuralTests`** – `new`. Location
`scalus-core/shared/src/test/scala/scalus/compiler/sir/lowering/simple/SimpleLoweringStructuralTests.scala`,
a trait with abstract `lowersTo(sir: SIR, expected: Term): Unit` and the six structural tests,
mixed into the two PV10 suites. Resolves C10-13. Call sites: 6 copied tests, about 110 lines
(`ScottEncodingLoweringTest.scala:43-113`, `SumOfProductsLoweringTest.scala:37-151`). Before/after:
`test("lower constant")` and its five siblings at `ScottEncodingLoweringTest.scala:43-113` are
deleted from both suites; each suite adds `with SimpleLoweringStructuralTests` and keeps its own
`lowersTo`.

**H-31 `TermDSL`** – `keep`, adopt. Location `scalus-core/shared/src/main/scala/scalus/uplc/TermDSL.scala`.
Resolves T-28, C24-7, C27-13, C36-8. Call sites: 6 local `def add` (`repeated-defs.csv` row `add`),
3 `chain` builders. Before/after: `Apply(Apply(Builtin(AddInteger), a), b)` becomes `AddInteger $ a $ b`.

**H-32 `Term.collectBuiltins`, `Term.usesConstrOrCase`, `FlatCodec.encode/decode`** – `keep`, adopt
(main code). Locations `Term.scala:289`, `Term.scala:280-287`, `FlatCodec.scala:12-38`. Resolves
C10-7 (string search of `t.show`), C27-18, C07-21. Call sites: `hasBuiltin`/`cip153Names`
(`ValueIntrinsicsLoweringTest.scala:46-59`), `containsCaseOrConstr` (`UplcPipelineTest.scala:64-71`),
about 10 `EncoderState`/`DecoderState` sequences in `FlatTest.scala:263-418` and `BuiltinValueFlatTest.scala:22-25`.

**H-33 `PlutusConformanceTest.referenceBudget`** – `change`. Location
`scalus-core/shared/src/test/scala/scalus/uplc/eval/PlutusConformanceTest.scala:165, 261-270`.
`BudgetRegex` becomes `protected`; new `protected def referenceBudget(name: String): ExUnits`.
Resolves C40-7. Call sites: `JitConformanceTest.scala:103-113` (1 copy). Before/after: the copied
`BudgetRegex` and `expectedBudget` at `JitConformanceTest.scala:103-113` are deleted;
`expectedBudget(name)` becomes `referenceBudget(name)` from the base suite.

**H-34 `ExampleCorpus`** – `new`. Location `scalus-examples/jvm/src/test/scala/scalus/examples/ExampleCorpus.scala`.
Signatures: `def validators(using Options): Seq[(String, SIR)]`, `def countNodes(t: Term): Int`,
`def baselineUplc(c: PlutusV3[?]): Term`. Resolves C25-16. Call sites: 3 copies of the 22-contract
list, 2 `countNodes`, 2 baseline compiles (evidence sites in `BooleanOptimizerImpactTest`,
`PipelineDeterminismCrossJvmTest`, `CceImpactMeasurementTest`, `CcePipelineTest`). Before/after:
`private def countNodes(t: Term): Int = t match ...` (`BooleanOptimizerImpactTest.scala:24-34`) and
the 22-entry list at `:133-180` are deleted; the suite calls `ExampleCorpus.countNodes(term)` and
`ExampleCorpus.validators`.

### G. Generators

*Superseded in part (2026-11-21):* D5 (`d5-generators.md`) amends H-35 and H-36; where the two
disagree, the D5 note wins.

**H-35 `ArbitraryInstances` chains** – `keep` structure, `change` one thing. Locations:
`scalus-core/shared/src/test/scala/scalus/uplc/test/ArbitraryInstances.scala:28` (trait, 18
givens), `scalus-core/shared/src/test/scala/scalus/ledger/api/v1/ArbitraryInstances.scala:11-12`
(object + trait, v1 extends uplc.test), v2, v3,
`scalus-core/shared/src/test/scala/scalus/cardano/address/ArbitraryInstances.scala:7-8`,
`scalus-core/shared/src/test/scala/scalus/cardano/ledger/ArbitraryInstances.scala:16-17` (72 givens). All six are copied into testkit main (`build.sbt:697-702`), so they may not depend on
test-only helpers (F-29). The two chains stay as they are: a suite mixes in the deepest trait it
needs, and `scalus.testing.kit.ArbitraryInstances` (testkit, 5 lines) joins `v3` and `uplc.test`
for `ScalusTest`. The one change: add `object ArbitraryInstances extends ArbitraryInstances` to
`uplc.test` (F-8: it has no companion, so callers import a subclass). The v1/v2/v3 files declare
`scalus.cardano.onchain.plutus.v*` while living under `ledger/api/v*` (F-8); moving the files is a
`style` item, not a helper change. New members, all opt-in `def`s (not givens):
`boundedListArb[A: Arbitrary](max: Int): Arbitrary[List[A]]` (C22-18; replaces the local
`Gen.choose(0, 10)` arbitraries in `ListTest.scala:583-588` and
`scalus-core/shared/src/test/scala/scalus/ledger/api/v1/ValueTest.scala:23-28`; before/after: the
6-line `given [T: Arbitrary]: Arbitrary[List[T]] = Arbitrary { for size <- Gen.choose(0, 10) ... }`
becomes `given [T: Arbitrary]: Arbitrary[List[T]] = boundedListArb(10)`),
`genSmallInterval` in v1 (C22-3), `genConstantFull(withBls: Boolean)` in uplc.test (C07-13, C36-6),
a `(map, key)` generator with key hits half the time (C04-11, C23-8). Call sites: 45 files use the
chains (`shared-helpers.csv`).

**H-36 `ArbitraryDerivation`** – `keep`. Location `scalus-core/shared/src/test/scala/scalus/testing/ArbitraryDerivation.scala:13`.
No finding asks for a change.

### H. Module-local kits

Each entry is one `object` or `trait` in the test package it serves; none is published.

**H-37 `RawCbor`** – `new`, `scalus-core/shared/src/test/scala/scalus/cardano/ledger/RawCbor.scala`:
`Raw` encoder and given, `cbor`, `cborArray`, `cborMap`, `value`, `assertRejected(cause: String)(body: => Any)`,
`body(pv: Int)(fields: Raw*)`. Resolves C05-17. Replaces 3 copies, about 40 lines each
(`RepeatedElementsTest.scala:36-62`, `PointerAddressDecodingTest.scala:27-74`,
`HardForkInitiationDecodingTest.scala:26-35`). Before/after: the hand-written `Raw` encoder block at
`RepeatedElementsTest.scala:36-62` is deleted; the suite adds `with RawCbor` and keeps its
`cborMap(...)`/`assertRejected(...)` calls unchanged.

**H-38 `StreamTestKit`** – `new`, `scalus-cardano-ledger/shared/src/test/scala/scalus/cardano/node/stream/StreamTestKit.scala`:
`point(n: Long): ChainPoint` (big-endian hash so n stays distinct past 255), `block(n: Long)`,
`drain[A](source: ScalusAsyncSource[A]): Seq[A]`. Resolves C37-17. Replaces 4 `point` and 3 `drain`
defs (A-26). Adopt `StreamCapabilities.inProcess` (C37-18, 6 literals) in the same unit.
Before/after: `private def point(n: Long): ChainPoint` (`HubDriverTest.scala:33-37`) and
`private def drain[A](m: Mailbox[A]): List[A]` (`SubscriptionHubTest.scala:52-60`) are deleted;
both suites `import StreamTestKit.*`; the 9-line `StreamCapabilities(...)` literal at
`HubDriverTest.scala:23-31` becomes `StreamCapabilities.inProcess(SubscriptionKind.all, NoReplay, horizon)`.

**H-39 `JsAssertions`** – `new`, `scalus-core/js/src/test/scala/scalus/utils/scalajs/JsAssertions.scala`:
`typeErrorFrom(body: => Any): js.TypeError`, `jsErrorFrom[E <: js.Any: ClassTag](body: => Any): E`,
`mainnetSlotConfigRecord`. Resolves C28-12. Replaces 5 `typeError` defs (A-27) and 2 slot-config
records. The ledger JS tests see scalus-core test classes (`build.sbt`: `scalus % "compile->compile;test->test"`).
Before/after: `private def typeError(body: => Any): js.TypeError` (`JUplcTest.scala:22-25`) is
deleted; `typeError(...)` becomes `typeErrorFrom(...)` after `import JsAssertions.*`.

**H-40 `MpfBehaviors` and `Fruits`** – `new`, `scalus-core/shared/src/test/scala/scalus/crypto/trie/MpfBehaviors.scala`:
a trait parameterized by the off-chain and on-chain operations with an abstract `Proof` type, plus
the 30-entry fruit fixture. Resolves C26-26, C26-27. Replaces 19 of 22 tests duplicated between
`MerklePatriciaForestryTest` and `FusedMerklePatriciaForestryTest` (about 180 lines). Before/after:
`fruitEntries` (`MerklePatriciaForestryTest.scala:10-48`) and the 19 shared tests (`:50-225`) move
into the trait; `MerklePatriciaForestryTest` becomes a thin instance that supplies the off-chain
and on-chain operations.

**H-41 `LedgerEncoding`** – `new`, `scalus-examples/jvm/src/test/scala/scalus/examples/verify/LedgerEncoding.scala`:
`scriptContext`, `spendingInfo`, `noDatum`, `someDatum`, `certifying`, `rewardingInfo(credential)`,
with `LedgerEncodingTest`. Resolves C16-21, C31-14, C16-20. Replaces the inline defs in
`object VestingVerificationTest` (`VestingVerificationTest.scala:725`, defs at `:740-755`) that two
unrelated suites import (`HtlcVerificationTest.scala:9`, `PaymentSplitterVerificationTest.scala:10`).
Before/after: `import scalus.examples.vesting.VestingVerificationTest.{scriptContext, spendingInfo, noDatum, someDatum}`
becomes `import scalus.examples.verify.LedgerEncoding.*`; the defs move unchanged.

**H-42 `CrowdfundingTestKit`** – `new`, crowdfunding test package: lookup helpers and
`buildCreateTx`/`buildDonateTx`/`buildWithdrawTx`/`buildReclaimTx(reader, ...)`, a genesis
emulator factory. Resolves C06-4, C06-6. Replaces about 300 duplicated lines across
`CrowdfundingScalaCheckCommandTest`, `CrowdfundingScenarioTest`, `CrowdfundingEmulatorTest`
(evidence sites). Before/after: `private def extractPkh(address: ShelleyAddress): PubKeyHash`
(`CrowdfundingScenarioTest.scala:165`) is deleted and `extractPkh(party.address)` becomes
`PubKeyHash(party.addrKeyHash)`; `private def getDonationScript(campaignId)` (`:177`, copied three
times) becomes `CrowdfundingTestKit.donationScript(campaignId)`.

**H-43 `LotteryTestTxs`** – `new`, lottery test package: side-parameterized `reveal`,
`secondReveal`, `lose`, `timeout` builders taking a reader. Resolves C18-9, C18-4. Replaces about
200 lines mirrored between `LotteryScenarioTest.scala:250-448` and
`LotteryScalaCheckCommandTest.scala:348-477`; both local `genByteStringOfN` become the shared one.
Before/after: `private def buildRevealP1Tx(...)` (`LotteryScalaCheckCommandTest.scala:348`) and its
P2 mirror are deleted; both suites call `LotteryTestTxs.reveal(Side.P1, reader, utxo, info)`;
`private def genByteStringOfN(n: Int)` (`:134`) becomes the inherited `ScalusTest` generator.

**H-44 `PaymentSplitterTestCases.txInputs/txOutputs`** – `change`,
`scalus-examples/jvm/src/test/scala/scalus/examples/paymentsplitter/PaymentSplitterTestCases.scala`:
`def txInputs(tc, scriptHash, lockTxId, payeesTxId): SList[TxInInfo]`, `def txOutputs(tc): SList[TxOut]`.
Resolves C31-12, C31-4, C31-18. Replaces 35 lines copied in the naive and optimized suites.
Before/after: the `scriptInputs ... allInputs` construction at
`NaivePaymentSplitterValidatorTest.scala:156-182` becomes
`PaymentSplitterTestCases.txInputs(tc, scriptHash, lockTxId, payeesTxId)`.

**H-45 `SetBenchTestKit`** – `new`, setbench test package: `findOutputAt(tx, address)`,
`findRefScriptUtxo(tx)`, `extractMetrics(tx)`, `aikenScript` (stream closed), `elements(n)`.
Resolves C14-6, with C14-2 (use `SetBenchTransactions` overloads) and C14-5 (use derived
`toData`, delete 6 `mpf16o*ToData` defs, A-28). Replaces 3 + 2 + 3 + 6 local copies (evidence sites).
Before/after: `private def findContractUtxo(...)` and `private def extractMetrics(tx)`
(`SetBenchEmulatorTest.scala:648-674`) are deleted in favor of `SetBenchTestKit.findOutputAt` and
`extractMetrics`; `mpf16oProofToData(mpf16o.proveMembership(key))` (`FusingBenchmarkTest.scala:54-67`)
becomes `mpf16o.proveMembership(key).toData`.

**H-46 `WithdrawZeroFixtures`** – `new`, `scalus-design-patterns/src/test/scala/scalus/patterns/WithdrawZeroFixtures.scala`:
`withdrawZero(hashes*)`, `redeemers(spending, rewarding)`, `rewardCtx(tx, h, r)`,
`spendCtx(tx, ref, datum, r)`, `eval(program, ctx)`; outputs via `ScalusTest.makePubKeyHashOutput`
(`ScalusTest.scala:198-208`). Resolves C32-16, C32-15. Replaces the 4 `runWithBudget` copies (A-29)
and 3 hand-built withdraw-zero transactions. Before/after: the `withdrawals = SortedMap(...)`,
`redeemers = AssocMap(...)` and two `ScriptContext` constructions inside `runWithBudget()`
(`BatchAuctionTest.scala:338-360`) become `withdrawZero(h)`, `redeemers(spend, reward)`,
`rewardCtx(tx, h, r)` and `spendCtx(tx, ref, datum, r)`; `checkResult(expectedResult, spendResult)`
(`:317`) stays and now pins.

**H-47 `CellEmulatorSuite`** – `new`, `scalus-utxo-cell/shared/src/test/scala/scalus/utxocells/CellEmulatorSuite.scala`:
`given testEnv`, `signed(builder, party)`; `submitOk` comes from H-15. Resolves C33-17, C33-16.
Replaces about 25 `TxBuilder(...).complete(..).sign(..)` + `assert(submitSync(tx).isRight)` pairs.
Before/after: `assert(emulator.submitSync(initTx).isRight)` (`AuctionCellSpec.scala:172`) becomes
`submitOk(emulator, initTx)`, and the three-UTxO `Emulator(initialUtxos = Map(...))` at `:134-140`
becomes `Emulator.withAddresses(Seq(Alice.address, Bob.address, Charles.address), Value.ada(1000))`.

**H-48 `SortInputShapes`** – `new`, `scalus-core/jvm/src/test/scala/scalus/uplc/eval/SortInputShapes.scala`:
`val all: Seq[(String, Int => Seq[BigInt])]`. Resolves C21-15. Replaces 3 copies
(`SortBudgetTest.scala:63-83`, `SortedMapFromListBudgetTest.scala:51-58`, `UplcConstrSortTest.scala:62-69`).
Before/after: `private val patterns: Seq[(String, Int => Seq[BigInt])] = Seq(...)`
(`SortedMapFromListBudgetTest.scala:51-58`) becomes `private val patterns = SortInputShapes.all`.

**H-49 `TestkitFixtures`** – `new`, `scalus-testkit/jvm/src/test/scala/scalus/testing/TestkitFixtures.scala`:
`aliceUtxos: Utxos`, `aliceOnly: ImmutableEmulator`, `payBob(reader, withDatum: Boolean): Transaction`.
Resolves C12-12. Replaces 10 copies of the "Alice pays Bob 10 ADA" block in `ScenarioTest` and
`ImmutableEmulatorTest` (evidence sites). Before/after: the 9-line
`TxBuilder.payTo(Bob.address, Value.ada(10)).complete(reader, Alice).sign(Alice.signer)` block at
`ScenarioTest.scala:325-333` becomes `TestkitFixtures.payBob(reader, withDatum = false)`.

**H-50 `CardanoLedgerVectors.checkVector`** – `change`,
`scalus-cardano-ledger/jvm/src/test/scala/scalus/testing/conformance/CardanoLedgerVectors.scala`:
`def checkVector(name: String, mode: EvaluatorMode): List[String]` and a vectors-resource
parameter. Resolves C30-6, C30-7, C30-5. Replaces the inline loop in the jvm suite
(`scalus-cardano-ledger/jvm/src/test/scala/scalus/testing/conformance/CardanoLedgerConformanceTest.scala:32-54`)
and the weaker IT copy
(`scalus-cardano-ledger-it/src/test/scala/scalus/testing/conformance/CardanoLedgerConformanceTest.scala:24-31`).
Before/after: the jvm suite's `test("Conformance test vector: " + vector)` body (`:31-54`) becomes
`assert(checkVector(vector, mode).isEmpty, checkVector(vector, mode).mkString("\n"))`; the IT suite
calls the same with `EvaluatorMode.Validate`.

**H-51 UplcBlaster test helpers** – `new`, `scalus-verification/src/test/scala/scalus/verify/uplcblaster/BlasterTestKit.scala`:
`binder(prop)`, `falsified(goal, value)`, `artifactOf(result)`, `leanCounts(goal, values)`,
`holdsOnCek(term)`. Resolves C03-11. Replaces 7 + 2 + 7 + 2 + 4 copies in `UplcBlasterTest`
(evidence sites), shared with `PreludeProofsTest` and `SpecificationsTest`. Before/after: the
3-line `UplcBlaster.lower(prop, functions) match { case ... }` binder extraction (`UplcBlasterTest.scala:64-66`,
repeated at `:273-275` and six more sites) becomes `binder(prop)`.

### I. Platform and tags

**H-52 `Benchmark` tag** – `change`. Moves from `scalus-testkit/shared/src/main/scala/scalus/testing/Benchmark.scala:12`
to `scalus-core/shared/src/test/scala/scalus/testing/Benchmark.scala` (same FQN), added to the copy
list; the testkit file is deleted. Resolves F-8 (the tag is private in 4 core files because core
cannot see testkit). Replaces 4 `private object Benchmark` (A-30: `BilinearAccumulatorBenchmarkTest.scala:96`,
`GenStatsTest.scala:33`, `SortedMapFromListBudgetTest.scala:40`, `SortBudgetTest.scala:47`).

## 4. Retire list

All `proposed`. Published members get `@deprecated("...", "1.5.0")` (latest tag `v1.5.0`) for one
release, then a delete; test-only members are deleted in the unit that replaces them.

| Helper | Location | Users today | Replaced by | Also update |
|---|---|---|---|---|
| `EvalTestDsl`, `EvalTestBuilder`, `EvalSubject`, `EvalTestResult` | `scalus-testkit/shared/src/main/scala/scalus/testing/dsl/EvalTestDsl.scala:21-162` | 0 | H-05 | `scalus-skills/skills/contract-test/SKILL.md` ("Budget assertions from EvalTestDsl"), `optimize-contract/SKILL.md` and `references/patterns.md`; skills version bump per `CONTRIBUTING.md` "Agent artifacts" |
| `ResultAssertions`, `Expected`, `AssertionError`, `BudgetAssertionError`, `MultiResultAssertions` | `.../testing/assertions/ResultAssertions.scala:14-207` | 0 outside testkit; `Expected` used by the 2 `BaseValidatorTest` suites (10 calls) | H-05, H-06 | same skills |
| `Evaluator`, `EvalConfig`, `MultiEvalResult` | `.../testing/eval/Evaluator.scala:21-156` | 0 | `Program.evaluateDebug`, H-13 | – |
| `BaseValidatorTest`, `BaseValidatorTestJvm` (`assertEvalResult`, `assertSameResult`, `assertSameResultWithUplcCli`, `assertUplcEvalResult`, `hoskyMint*`) | `.../testing/kit/BaseValidatorTest.scala:19-84`, `BaseValidatorTestJvm.scala:14-60` | 2 suites: `PreimageExampleTest.scala:16`, `MintingPolicyExampleTest.scala:18` (10 `assertEvalResult` calls, 17 `hoskyMint` uses) | `ScalusTest` + `checkResult`; the two fixtures move into the suites | `contract-test/SKILL.md` ("assertEvalResult(expected)(program)") |
| `assertEvalWithBudget`, `assertEvalWithBudgets`, `assertEvalWithBudgetAndFee`, `assertEvalWithinBudget`, `nativeOpts` | `EvalTestKit.scala:51-52, 98-347` | 113 + 288 + 20 + 12 | H-05 | `contract-test/SKILL.md` pin convention |
| `ScalusTest.success(budget)`, `failure(msg, budget)` | `ScalusTest.scala:224-228` | 13 `checkResult` callers, few with a budget | H-06 | – |
| `ScalaCompilerVersion.baseline` | H-04 | 21 calls, 9 files | tables | `contract-test/SKILL.md` |
| `ValidatorRulesTestKit.withNetwork`, `randomTransactionWithIsValidField`, `generateKeyPair` | `ValidatorRulesTestKit.scala:14-67` | 1, 13, 94 call sites | H-23, H-18 | – |
| `scripts/update-budgets.py` | `scripts/` | manual | H-03 | `CONTRIBUTING.md` if it names the script |
| 4 private `Benchmark` objects | A-30 | – | H-52 | – |
| Local copies: 48 `genesisHash`, 68 `given Options`, 142 `given PlutusVM`, 13 `createProvider`, 8 `createEmulator`, 8 `runValidator`, 4 `assertSubmit*`, 4 `runWithBudget`, 3 `createScriptLockedUtxo`, 10 `input`, 5 `adaOutput`, 5 `mkContext`, 4 `point`, 3 `drain`, 5 `typeError`, 6 `mpf16o*ToData` | A-10 to A-30 | – | H-14 to H-48 | – |

`ScalusTest.compareBudgetWithReferenceValue` (`ScalusTest.scala:267-289`, 12 callers in the
benchmark suites) and `runWithProfileReport` (1 caller) stay; no finding retires them.

## 5. Open questions for the owner

1. **Generation reader (`[BP-6a]`): macro or build-generated source?** Recommended: the macro
   (2.5), because it needs no build change and works for ejected projects. Risk: it names a
   compiler-internal class; the unit-1 acceptance test catches a break on every compiler bump.
2. **Pin every fixed-input `assertEval*` by default (`[BP-1]` as written), or only the labelled
   ones?** Recommended: every one. ListTest alone has about 255 unpinned fixed-input sites; opt-in
   pinning would leave them unpinned for good. The cost is labels in tests with several evaluations.
3. **Pin setup transactions in Emulator tests?** Recommended: no. `submitOk` for setup, `submitPinned`
   for the transaction under test. Pinning `lock` in each of HtlcTest's 10 tests would add 10 equal rows.
4. **`Pin` as an enum with `Eval`/`Tx`/`Size` cases, or one record with optional fields?**
   Recommended: the enum (2.2). The failure message and the writer then know what each row means.
5. **Number format in tables: groups of three (`2_298_799`) or the current million-boundary style
   (`2_298799`)?** Recommended: groups of three, the same grouping as the failure message. The
   writer, not a human, writes these numbers.
6. **Fork the ledger JVM tests?** Not needed for the update trigger (config map, 2.8). Recommended
   anyway in unit 1, so `-Dscalus.test.scalaVersion` reaches the acceptance test there as well, and
   P1's `testForkedParallel` applies to the ledger suites.

## 6. Implementation units (`extract-helper`, step 1 of `[P3-8]`)

Each unit is one branch; sizes are estimates of changed lines (`[P4-3]` cap is about 800).

| # | Unit | Module | Files | Est. lines |
|---|---|---|---|---|
| 1 | Budget-pin API and update tool: H-01, H-02 (`BudgetPins` and `BudgetPinsSuite`), H-03, H-04, H-52, the `BudgetPins` mixin on `EvalTestKit`/`ScalusTest` only (no site migration), `updateBudgets` commands, copy lists in `build.sbt`, `ScalaCompilerVersion` move, `ScalaCompilerVersionTest`, `BudgetPinsTest` (mismatch message, twice-pinned, unused key, update round trip on a temp file), `-Dscalus.test.scalaVersion` for all forked JVM test configs | scalus-core (shared, jvm, js, native test), scalus-testkit, build.sbt | about 13 new or changed | 750 |
| 2 | `EvalTestKit` pinned assertions H-05 (incl. C12-23, C12-24), deprecated shims, H-10, H-11, H-12; H-06 `checkResult`/`submitOk`/`submitPinned`/`assertSubmitFails` (H-15, H-16); H-07 `CapeHarness.registerCases`; skills and `CONTRIBUTING.md` text for the new pin convention | scalus-core, scalus-testkit, scalus-examples | `EvalTestKit.scala`, `ScalusTest.scala`, `CapeTestSuite.scala`, 2 SKILL.md | 600 |
| 3 | `TestUtil` additions H-14, H-20, H-21, H-22, and `uplc.test.ArbitraryInstances` companion + `boundedListArb` (H-35) | scalus-cardano-ledger (shared test), scalus-core | `TestUtil.scala`, `ArbitraryInstances.scala` | 200 |
| 4 | Ledger rules kits H-23, H-24, H-25 retire, H-38 `StreamTestKit`, H-50 `checkVector` | scalus-cardano-ledger | `ValidatorRulesTestKit.scala`, `CertsTestKit.scala`, `StreamTestKit.scala`, `CardanoLedgerVectors.scala` + the 9 cert suites and 7 script suites that the kits serve are `adopt-helper` work, not this unit | 450 |
| 5 | Compiler test DSLs H-13, H-26, H-27, H-28, H-29, H-30, H-33 | scalus-core | `SirTestDsl.scala`, `TermTestDsl.scala`, `SirTestVocabulary.scala`, `SnippetCompilation.scala`, `SimpleLoweringStructuralTests.scala`, `PlutusConformanceTest.scala` | 500 |
| 6 | Core module-local kits H-37 `RawCbor`, H-39 `JsAssertions`, H-40 `MpfBehaviors`, H-48 `SortInputShapes` | scalus-core | 4 new files + the 2 MPF suites rewritten on the trait | 650 |
| 7 | Examples kits H-34 `ExampleCorpus`, H-41 `LedgerEncoding`, H-44, H-45 `SetBenchTestKit` | scalus-examples | 4 new or changed files | 450 |
| 8 | Examples kits H-42 `CrowdfundingTestKit`, H-43 `LotteryTestTxs` | scalus-examples | 2 new files + 5 suites | 800 (split by package if the rewrite of the 5 suites exceeds it) |
| 9 | Other modules H-46 `WithdrawZeroFixtures`, H-47 `CellEmulatorSuite`, H-49 `TestkitFixtures`, H-51 Blaster helpers | scalus-design-patterns, scalus-utxo-cell, scalus-testkit, scalus-verification | 4 new files | 400 |

Retirements of `EvalTestDsl`, `ResultAssertions`, `Evaluator`, `BaseValidatorTest(+Jvm)` and the
`assertEvalWithBudget*` shims are `adopt-helper`/`budget-pin` work after their last caller moves;
the final `budget-pin` unit deletes `BudgetTable.unpinned` and `update-budgets.py` (2.11).

## Appendix A. How the counts were made

All over `**/src/test/**/*.scala`, excluding `target/` and `.claude/`, with Python `re` on
2026-10-27:

| Id | Regex | Result |
|---|---|---|
| A-1 | `ExUnits\(` | 810 sites, 79 files |
| A-2 | files containing `EvalTestKit` minus the definition; each contains `AnyFunSuite` | 31 files; `ScalusTest` users 30, all `AnyFunSuite` |
| A-3 | `assertEvalWithBudgets\(` then count `->` inside its `Seq(` | 288 single-variant, 0 multi-variant |
| A-4 | `test\(s"[^"]*(size|bytes)` | 15 sites (149 interpolated test names in total) |
| A-5 | `ScalaCompilerVersion\.baseline` | 21 sites, 9 files |
| A-6 | `\.budget == ` | 53 sites, 25 files |
| A-7 | `\bassertEval\(` 167, `\bassertEvalEq\(` 120, `assertEvalFails\[` 50, `assertEvalFailsWithMessage\[` 46, `assertEvalNotEq\(` 4, `assertEvalSuccess\(` 1, `assertEvalWithinBudget\(` 12 | per pattern |
| A-8 | `\bcheckResult\(` outside `ScalusTest.scala` | 13 sites, 3 files |
| A-9 | `assert\([a-zA-Z]+\.isFailure` 62 in 28 files; `logs\.exists\(_\.contains` 45 in 18 files | – |
| A-10 | `given Options\s*=\s*Options\(` 68 in 55 files; `given PlutusVM\s*=` 142 in 105 files; files with `SirToUplcV3Lowering` 82 | – |
| A-11 | `assertTermEvalEq\(` | 198 sites, 3 files |
| A-12 | `TargetLoweringBackend\.values` 5 in 2 files; `def eval(Int|Bool|Integer|BigInt|X)\(` 3 in 2 files | – |
| A-13 | `def runValidator` | 8 sites, 8 files |
| A-14 | `assert\([^\n]*submit(Sync)?\([^\n]*isRight` | 103 sites, 21 files |
| A-15 | `def assertSubmit` | 4 sites, 4 files |
| A-16 | `val genesisHash\s*[:=]` | 48 sites, 35 files |
| A-17 | `generateKeyPair\(\)` | 94 sites, 12 files |
| A-18 | `Await\.result` 33 in 9 files; `\.await\(\)` 854 in 42 files | – |
| A-19 | `def createScriptLockedUtxo` | 3 |
| A-20 | `def input\(` 10 in 10 files; `def adaOutput` 5 in 4 files | – |
| A-21 | `def createProvider` 13; `def createEmulator` 8 in 7 files; `def mkEmulator` 1; `Emulator\(\s*\n?\s*initialUtxos` 74 in 40 files; `Emulator\.withAddresses` 47 in 14 files | – |
| A-22 | `Context\(\)` | 219 sites, 31 files |
| A-23 | `def mkContext` | 5 |
| A-24 | `def (collectFunctionNames|collectVars|listElemTypes|containsBuiltin)` 4 in 3 files; `def alphaEq` 4 in 4 files | – |
| A-25 | `case Term\.(Apply|LamAbs|Force|Delay)\(` | 73 sites, 12 files |
| A-26 | `def point\(` 4; `def drain` 3 | – |
| A-27 | `def typeError` | 5 |
| A-28 | `def mpf16o(Step|Proof)ToData` | 6 sites, 3 files |
| A-29 | `def runWithBudget` | 4 |
| A-30 | `private object Benchmark` | 4 |

Inventory numbers (`budget-inventory.csv`, 1293 rows): `none` 759, `exact` 435, `property` 35,
`tx-fee` 20, `exact+fee` 20, `upper-bound` 15, `forced-fee` 9; 105 files; ListTest 289, ValueTest
126, SortedMapTest 105, MathTest 63 sites. The `property` state is a heuristic over the whole test
block: the 8 `assertEval` rows at `ListTest.scala:659-666` sit after the `check` block, not in it.
