package scalus.testing.budget

import org.scalactic.source
import org.scalatest.exceptions.{StackDepthException, TestFailedException}
import org.scalatest.{Args, Failed, Outcome, Status, Succeeded, TestSuite, TestSuiteMixin}
import scalus.cardano.ledger.{ExUnits, Transaction}

import scala.collection.mutable

/** Budget pin primitives (spec "Budget pins").
  *
  * A pin compares one measured budget with the suite's [[BudgetTable]], by key. The key is the
  * current test name, or `"<test name> / <label>"` when a test pins more than once.
  *
  * This trait has no ScalaTest self-type, so a companion object can mix it in. The suite class also
  * mixes in [[BudgetPinsSuite]], which tells the pins the current test name and runs the checks at
  * the end of the suite.
  *
  * {{{
  * class MathTest extends AnyFunSuite, EvalTestKit, BudgetPinsSuite {
  *     override protected def budgetTable: BudgetTable = MathTestBudgets
  *     test("abs - 0 - budget") { assertEvalEq(BigInt(0).absolute, BigInt(0)) } // pins
  * }
  * }}}
  */
trait BudgetPins {

    /** The suite's table. Override it with the generated `<Suite>Budgets` object. */
    protected def budgetTable: BudgetTable = BudgetTable.unpinned

    private[budget] final val budgetPinState: BudgetPins.State = BudgetPins.State()

    /** Registers a check that runs only when `.at(...)` is called on it. A test that ends with an
      * unused one fails: without `.at`, nothing was checked.
      */
    protected final def registerPendingAt()(using pos: source.Position): BudgetPins.PendingAt =
        budgetPinState.registerPending(s"${pos.fileName}:${pos.lineNumber}")

    /** Pins one evaluation of a script of `size` bytes under the current test name. */
    protected final def pinEval(budget: ExUnits, size: Int)(using source.Position): Unit =
        pinValue(None, Pin.eval(budget, size))

    /** Runs `body` with every pin key labelled `label`, instead of the automatic label (the source
      * text of the evaluated code). For the rare case where the source text is not a good key.
      * {{{
      * pinLabel("sum"):
      *     assertEvalEq(BigInt(5))(BigInt(2) + BigInt(3))
      * }}}
      */
    protected final def pinLabel[A](label: String)(body: => A): A = {
        val state = budgetPinState
        val outer = state.labelOverride
        state.labelOverride = Some(label)
        try body
        finally state.labelOverride = outer
    }

    /** Pins one evaluation of a script of `size` bytes under `"<test name> / <label>"`. */
    protected final def pinEval(label: String, budget: ExUnits, size: Int)(using
        source.Position
    ): Unit =
        pinValue(Some(label), Pin.eval(budget, size))

    /** Pins a balanced transaction under the current test name: redeemer budgets, size and fee. */
    protected final def pinTx(tx: Transaction)(using source.Position): Unit =
        pinValue(None, Pin.tx(tx))

    /** Pins a script size in bytes under the current test name (`[BP-5]`). */
    protected final def pinSize(bytes: Int)(using source.Position): Unit =
        pinValue(None, Pin.Size(bytes))

    /** Fails at `pos`, the caller's position. ScalaTest's `fail` is an inline macro that would
      * report this file instead.
      */
    private def failAt(message: String)(using pos: source.Position): Nothing =
        throw TestFailedException((_: StackDepthException) => Some(message), None, pos)

    private def pinValue(label: Option[String], actual: Pin)(using pos: source.Position): Unit = {
        val table = budgetTable
        if !table.isUnpinned then
            val state = budgetPinState
            val suite = BudgetPins.suiteName(this)
            val test = state.currentTest.getOrElse(
              failAt(s"$suite: no current test: mix BudgetPinsSuite into the suite class")
            )
            val key = state.labelOverride.orElse(label).fold(test)(l => s"$test / $l")
            if !state.use(key, actual) then
                failAt(
                  s"$suite: budget pin key \"$key\" pinned twice: add a label, " +
                      "or use checkEval for a property test"
                )
            if !state.updateMode then
                table.get(key) match
                    case None =>
                        failAt(BudgetText.missing(suite, key, table.generation, actual))
                    case Some(expected) if expected != actual =>
                        failAt(
                          BudgetText.mismatch(suite, key, table.generation, expected, actual)
                        )
                    case Some(_) => ()
    }
}

object BudgetPins {

    /** The mutable state of one suite instance's pins. ScalaTest runs the tests of one suite one
      * after another, so plain fields are enough.
      */
    final class State {
        var currentTest: Option[String] = None
        var testsRun: Int = 0
        var updateMode: Boolean = false
        var anyTestFailed: Boolean = false
        var labelOverride: Option[String] = None
        private val used = mutable.LinkedHashMap.empty[String, Pin]

        /** Records the actual value of `key`. False when the key was already used in this run. */
        def use(key: String, actual: Pin): Boolean =
            if used.contains(key) then false
            else
                used(key) = actual
                true

        private val pending = mutable.LinkedHashMap.empty[Int, String]
        private var nextPending = 0

        def registerPending(where: String): PendingAt =
            nextPending += 1
            pending(nextPending) = where
            PendingAt(this, nextPending)

        private[budget] def applied(id: Int): Unit = { val _ = pending.remove(id) }

        /** The places of the checks still waiting for `.at`, and forgets them. */
        def takeUnapplied(): Seq[String] =
            val all = pending.values.toSeq
            pending.clear()
            all

        /** The keys used in this run with their actual values, in first-use order. */
        def actuals: Seq[(String, Pin)] = used.toSeq
    }

    /** A registered check; `.at(...)` marks it applied. */
    final class PendingAt private[budget] (state: State, id: Int) {
        def applied(): Unit = state.applied(id)
    }

    private[budget] def suiteName(suite: Any): String =
        suite.getClass.getSimpleName.stripSuffix("$")
}

/** The ScalaTest hooks of the budget pins. Mix it into the suite class, after the suite style:
  * `class MathTest extends AnyFunSuite, EvalTestKit, BudgetPinsSuite`.
  *
  *   - Sets the current test name for the pin keys.
  *   - Check mode (default): after a run in which every test ran and passed, a table key that no
  *     test used fails the suite (`[BP-8c]`).
  *   - Update mode (`-- -Dscalus.budgets.update=true`): pins never fail. After the run the table
  *     file is rewritten by key, and a report goes to `target/budgets/report.md` (`[BP-8]`,
  *     `[BP-9]`). The ScalaTest config map reaches forked and non-forked test JVMs alike.
  */
trait BudgetPinsSuite extends TestSuiteMixin { this: TestSuite & BudgetPins =>

    abstract override protected def withFixture(test: NoArgTest): Outcome = {
        val state = budgetPinState
        state.currentTest = Some(test.name)
        state.testsRun += 1
        try
            val outcome = super.withFixture(test) match
                case Succeeded =>
                    // A check created without `.at` never ran: fail rather than pass silently.
                    val unapplied = state.takeUnapplied()
                    if unapplied.isEmpty then Succeeded
                    else
                        Failed(
                          TestFailedException(
                            "assertEvalEq(expected)(f) without .at(...) at " +
                                unapplied.mkString(", "),
                            0
                          )
                        )
                case other =>
                    val _ = state.takeUnapplied()
                    other
            if !outcome.isSucceeded then state.anyTestFailed = true
            outcome
        finally state.currentTest = None
    }

    abstract override def run(testName: Option[String], args: Args): Status = {
        val state = budgetPinState
        state.updateMode =
            args.configMap.get(BudgetPinsSuite.UpdateKey).exists(_.toString == "true")
        val status = super.run(testName, args)
        // Unused keys are decided only after a full, green run: a filtered or failed run did not
        // reach every pin.
        // `withFixture` records failures, so no blocking `Status.succeeds()`, which Scala.js lacks.
        val complete = testName.isEmpty &&
            state.testsRun == expectedTestCount(args.filter) &&
            !state.anyTestFailed
        finishPins(complete)
        status
    }

    private def finishPins(complete: Boolean): Unit = {
        val table = budgetTable
        val state = budgetPinState
        val suite = BudgetPins.suiteName(this)
        if !table.isUnpinned then
            val usedKeys = state.actuals.map(_._1).toSet
            val unused = if complete then table.keys.filterNot(usedKeys) else Seq.empty
            if state.updateMode then
                val merged = BudgetText.merge(table.all, table.generation, state.actuals, complete)
                if merged != table.all then
                    // The table object's class name, e.g. "scalus.x.MathTestBudgets$".
                    val name = table.getClass.getName.stripSuffix("$")
                    val dot = name.lastIndexOf('.')
                    BudgetFiles.writeTable(
                      table.sourcePath,
                      BudgetText.source(name.take(dot), name.drop(dot + 1), suite, merged)
                    )
                    BudgetFiles.appendReport(
                      BudgetText.report(suite, table.generation, table, state.actuals, unused)
                    )
            else if unused.nonEmpty then
                throw IllegalStateException(
                  s"$suite: ${unused.size} unused budget pins: " +
                      unused.map(k => s"\"$k\"").mkString(", ") + ". Run `sbtn updateBudgets`."
                )
    }
}

object BudgetPinsSuite {

    /** The ScalaTest config map key of update mode: `testOnly -- -Dscalus.budgets.update=true`. */
    val UpdateKey: String = "scalus.budgets.update"
}
