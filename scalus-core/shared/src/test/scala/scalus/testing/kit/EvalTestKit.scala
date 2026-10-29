package scalus.testing.kit

import org.scalacheck.util.Pretty
import org.scalacheck.Arbitrary
import org.scalactic.{source, Prettifier}
import org.scalatest.Assertion
import org.scalatest.Assertions
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.cardano.ledger.{CardanoInfo, Coin, ExUnits, Script}
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.cardano.onchain.plutus.prelude.{Eq, Option as ScalusOption}
import scalus.uplc.*
import scalus.uplc.Constant.given
import scalus.uplc.Term.asTerm
import scalus.uplc.test.ArbitraryInstances
import scalus.uplc.eval.*
import scalus.testing.budget.{BudgetPins, BudgetPinsSuite}
import scalus.testing.internal.SourceText
import org.scalatest.TestSuite

import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/** ScalaTest integration trait for evaluation testing.
  *
  * Provides:
  *   - Inline compilation via `assertEval*` methods using `PlutusV3.compile`
  *   - Given-based configuration injection
  *   - Property-based testing support via `checkEval`
  *   - Configurable PlutusVM via `using` parameters
  *
  * This trait provides the core functionality for testing Scalus code evaluation.
  */
trait EvalTestKit
    extends Assertions
    with ScalaCheckPropertyChecks
    with ArbitraryInstances
    with BudgetPins
    with BudgetPinsSuite { this: TestSuite =>
    export org.scalatestplus.scalacheck.Checkers.*
    export org.scalacheck.{Arbitrary, Gen, Shrink}
    export scalus.cardano.onchain.plutus.prelude.{!==, <=>, ===}

    protected def compilerOptions: Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    protected given Options = compilerOptions

    /** Alias retained for compatibility; same as `compilerOptions` since the `nativeListElements`
      * flag was removed in favor of per-type `@UplcRepr` annotations.
      */
    protected lazy val nativeOpts: Options =
        compilerOptions

    /** Default PlutusVM - can be overridden by subclasses */
    protected def plutusVM: PlutusVM = PlutusVM.makePlutusV3VM()

    /** Given PlutusVM using plutusVM - methods can override via `using` parameter */
    protected given PlutusVM = plutusVM

    // ----- Utility -----

    protected final inline def liftThrowableToOption[A](inline code: A): ScalusOption[A] =
        try ScalusOption.Some(code)
        catch case NonFatal(_) => ScalusOption.None

    // ----- Inline Compilation Assertions -----

    /** Evaluates `term` on the PlutusVM and returns the result term and its budget. Fails the test
      * when the evaluation fails.
      */
    protected final def evalSuccess(term: Term)(using
        vm: PlutusVM,
        pos: source.Position
    ): (Term, ExUnits) =
        term.evaluateDebug match
            case Result.Success(result, budget, _, _) => (result, budget)
            case Result.Failure(exception, budget, costs, _) =>
                fail(s"""Expected success, but got failure: $exception;
                       |budget: $budget;
                       |costs: ${costs.toMap}""".stripMargin)

    protected final def assertTermEq(actual: Term, expected: Term)(using source.Position): Unit =
        assert(actual α_== expected, s"Expected term $expected, but got ${actual.show}")

    // ----- Correctness: closed expressions, never pinned -----
    //
    // A closed expression can fold to a constant at compile time (partial evaluation in the
    // Inliner runs the CEK machine on it). The result check still holds, but its budget is the cost
    // of a constant, so these forms never pin. Use the `.at` forms below for budgets.

    /** Assert that code evaluates to true on both JVM and PlutusVM. No budget pin. */
    protected final inline def assertEval(
        inline code: Boolean
    )(using vm: PlutusVM, options: Options, pos: source.Position): Unit =
        val compiled = PlutusV3.compile(code)
        assert(compiled.code)
        assertTermEq(evalSuccess(compiled.program.term)._1, trueTerm)

    /** Assert that `code` evaluates to `expected` on both the JVM and the PlutusVM. No budget pin:
      * for a budget, pass the inputs at runtime with `assertEvalEq(expected)(f).at(...)`.
      * {{{
      * assertEvalEq[BigInt](1)(min(1, 2))
      * assertEvalEq(Cons(BigInt(4), Nil)):
      *     List.range(BigInt(1), BigInt(3)).map(_ * 2).filter(_ > 2)
      * }}}
      */
    protected final inline def assertEvalEq[T: Eq](inline expected: T)(
        inline code: T
    )(using vm: PlutusVM, options: Options, pos: source.Position): Unit =
        val compiled = PlutusV3.compile(code)
        val compiledExpected = PlutusV3.compile(expected)
        assert(
          compiled.code === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${compiled.code}"
        )
        assertTermEq(
          evalSuccess(compiled.program.term)._1,
          compiledExpected.program.term.evaluate
        )

    /** Legacy order, kept for users of the published testkit: prefer
      * `assertEvalEq(expected)(code)`. No budget pin. The JVM name differs only because both forms
      * erase to the same signature.
      */
    @targetName("assertEvalEqLegacyOrder")
    protected final inline def assertEvalEq[T: Eq](
        inline code: T,
        inline expected: T
    )(using vm: PlutusVM, options: Options, pos: source.Position): Unit =
        assertEvalEq(expected)(code)

    // ----- Budget: a function applied to runtime arguments, pinned -----
    //
    // The arguments reach the script as Data at runtime, like a datum or a redeemer, so constant
    // folding cannot remove the work. The pin label is the source text of `f`, then `@` and the
    // arguments: "min @ (1, 2)". `.at` checks the result on the JVM and on the PlutusVM, then pins.
    // A test that creates an `assertEvalEq(expected)(f)` and never calls `.at` fails.

    /** `assertEvalEq(expected)(f).at(a1)`: `f(a1)` evaluates to `expected`; pins the budget.
      * {{{
      * assertEvalEq(BigInt(3))((l: List[BigInt]) => l.length).at(Cons(1, Cons(2, Cons(3, Nil))))
      * }}}
      */
    protected final inline def assertEvalEq[A1, T: Eq](inline expected: T)(
        inline f: A1 => T
    )(using
        vm: PlutusVM,
        options: Options,
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        pos: source.Position
    ): EvalTestKit.At1[A1] =
        val compiled = PlutusV3.compile { (d1: Data) => f(d1.to[A1]) }
        val compiledExpected = PlutusV3.compile(expected)
        val label = SourceText.of(f)
        // The ledger's size of the deployed script: the compiled function, without its arguments.
        val size = Script.PlutusV3(compiled.program).script.size.toLong
        val toData1 = (a: A1) => toData[A1](a)
        EvalTestKit.At1(
          registerPendingAt(),
          a1 =>
              assert(
                f(a1) === compiledExpected.code,
                s"Expected ${compiledExpected.code}, but got ${f(a1)}"
              )
              val (result, budget) = evalSuccess(compiled.program.term $ toData1(a1).asTerm)
              assertTermEq(result, compiledExpected.program.term.evaluate)
              pinEval(s"$label @ $a1", budget, size)
        )

    /** `assertEvalEq(expected)(f).at(a1, a2)`: `f(a1, a2)` evaluates to `expected`; pins the
      * budget.
      * {{{
      * assertEvalEq(BigInt(1))(min).at(1, 2)
      * }}}
      */
    protected final inline def assertEvalEq[A1, A2, T: Eq](inline expected: T)(
        inline f: (A1, A2) => T
    )(using
        vm: PlutusVM,
        options: Options,
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        pos: source.Position
    ): EvalTestKit.At2[A1, A2] =
        val compiled = PlutusV3.compile { (d1: Data, d2: Data) => f(d1.to[A1], d2.to[A2]) }
        val compiledExpected = PlutusV3.compile(expected)
        val label = SourceText.of(f)
        // The ledger's size of the deployed script: the compiled function, without its arguments.
        val size = Script.PlutusV3(compiled.program).script.size.toLong
        val toData1 = (a: A1) => toData[A1](a)
        val toData2 = (a: A2) => toData[A2](a)
        EvalTestKit.At2(
          registerPendingAt(),
          (a1, a2) =>
              assert(
                f(a1, a2) === compiledExpected.code,
                s"Expected ${compiledExpected.code}, but got ${f(a1, a2)}"
              )
              val applied = compiled.program.term $ toData1(a1).asTerm $ toData2(a2).asTerm
              val (result, budget) = evalSuccess(applied)
              assertTermEq(result, compiledExpected.program.term.evaluate)
              pinEval(s"$label @ ($a1, $a2)", budget, size)
        )

    /** `assertEvalEq(expected)(f).at(a1, a2, a3)`: `f(a1, a2, a3)` evaluates to `expected`; pins
      * the budget.
      */
    protected final inline def assertEvalEq[A1, A2, A3, T: Eq](inline expected: T)(
        inline f: (A1, A2, A3) => T
    )(using
        vm: PlutusVM,
        options: Options,
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        inline a3FromData: FromData[A3],
        inline a3ToData: ToData[A3],
        pos: source.Position
    ): EvalTestKit.At3[A1, A2, A3] =
        val compiled = PlutusV3.compile { (d1: Data, d2: Data, d3: Data) =>
            f(d1.to[A1], d2.to[A2], d3.to[A3])
        }
        val compiledExpected = PlutusV3.compile(expected)
        val label = SourceText.of(f)
        // The ledger's size of the deployed script: the compiled function, without its arguments.
        val size = Script.PlutusV3(compiled.program).script.size.toLong
        val toData1 = (a: A1) => toData[A1](a)
        val toData2 = (a: A2) => toData[A2](a)
        val toData3 = (a: A3) => toData[A3](a)
        EvalTestKit.At3(
          registerPendingAt(),
          (a1, a2, a3) =>
              assert(
                f(a1, a2, a3) === compiledExpected.code,
                s"Expected ${compiledExpected.code}, but got ${f(a1, a2, a3)}"
              )
              val applied = compiled.program.term $ toData1(a1).asTerm $ toData2(a2).asTerm $
                  toData3(a3).asTerm
              val (result, budget) = evalSuccess(applied)
              assertTermEq(result, compiledExpected.program.term.evaluate)
              pinEval(s"$label @ ($a1, $a2, $a3)", budget, size)
        )

    /** Assert that code evaluates to expected value with budget limit check (upper bound).
      *
      * Legacy: use `assertEvalEq`, which pins the budget in the suite's budget table. The
      * `budget-pin` units migrate the callers, then this method goes.
      */
    protected final inline def assertEvalWithinBudget[T: Eq](
        inline code: T,
        inline expected: T,
        budget: ExUnits
    )(using vm: PlutusVM, options: Options): Unit =
        val compiled = PlutusV3.compile(code)
        val compiledExpected = PlutusV3.compile(expected)

        assert(
          compiled.code === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${compiled.code}"
        )

        compiled.program.term.evaluateDebug match
            case Result.Success(term, exunits, costs, logs) =>
                assert(
                  exunits.memory <= budget.memory && exunits.steps <= budget.steps,
                  s"Budget exceeded: got $exunits, but expected at most $budget"
                )

                val expectedTerm = compiledExpected.program.term.evaluate
                assert(term α_== expectedTerm, s"Expected term $expectedTerm, but got ${term.show}")
            case Result.Failure(exception, budget, costs, logs) =>
                fail(s"""Expected success, but got failure: $exception;
                       |budget: $budget;
                       |costs: ${costs.toMap}""".stripMargin)

    /** Assert that code evaluates to expected value with exact budget match.
      *
      * Legacy: use `assertEvalEq`, which pins the budget in the suite's budget table. The
      * `budget-pin` units migrate the callers, then this method goes.
      */
    protected final inline def assertEvalWithBudget[T: Eq](
        inline code: T,
        inline expected: T,
        budget: ExUnits
    )(using vm: PlutusVM, options: Options): Unit =
        val compiled = PlutusV3.compile(code)
        val compiledExpected = PlutusV3.compile(expected)

        assert(
          compiled.code === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${compiled.code}"
        )

        compiled.program.term.evaluateDebug match
            case Result.Success(term, exunits, costs, logs) =>
                assert(
                  exunits == budget,
                  s"Budget mismatch: got $exunits, but expected $budget"
                )

                val expectedTerm = compiledExpected.program.term.evaluate
                assert(term α_== expectedTerm, s"Expected term $expectedTerm, but got ${term.show}")
            case Result.Failure(exception, budget, costs, logs) =>
                fail(s"""Expected success, but got failure: $exception;
                       |budget: $budget;
                       |costs: ${costs.toMap}""".stripMargin)

    /** Assert that a function evaluates to expected value with exact budget match. The argument is
      * passed as Data at runtime, preventing compile-time constant folding.
      */
    protected final inline def assertEvalWithBudget[A1, T: Eq](
        inline code: A1 => T,
        arg1: A1,
        inline expected: T,
        budget: ExUnits
    )(using
        vm: PlutusVM,
        options: Options,
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1]
    ): Unit =
        val compiled = PlutusV3.compile { (d: Data) => code(d.to[A1]) }
        val compiledExpected = PlutusV3.compile(expected)

        assert(
          code(arg1) === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${code(arg1)}"
        )

        val applied = compiled.program.term $ toData[A1](arg1).asTerm
        applied.evaluateDebug match
            case Result.Success(term, exunits, costs, logs) =>
                assert(
                  exunits == budget,
                  s"Budget mismatch: got $exunits, but expected $budget"
                )
                val expectedTerm = compiledExpected.program.term.evaluate
                assert(term α_== expectedTerm, s"Expected term $expectedTerm, but got ${term.show}")
            case Result.Failure(exception, budget, costs, logs) =>
                fail(s"""Expected success, but got failure: $exception;
                       |budget: $budget;
                       |costs: ${costs.toMap}""".stripMargin)

    /** Assert that code evaluates to expected value under multiple compiler option configurations.
      *
      * Compiles the code once, then re-lowers with each `Options` and asserts the corresponding
      * budget. Useful for testing the same code under different lowering configurations.
      *
      * @param budgets
      *   list of (Options, ExUnits) pairs to test
      */
    protected final inline def assertEvalWithBudgets[T: Eq](
        inline code: T,
        inline expected: T,
        budgets: Seq[(Options, ExUnits)]
    )(using vm: PlutusVM, options: Options): Unit =
        val compiled = PlutusV3.compile(code)
        val compiledExpected = PlutusV3.compile(expected)

        assert(
          compiled.code === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${compiled.code}"
        )

        for (opts, budget) <- budgets do
            val c = compiled.withOptions(opts)
            val ce = compiledExpected.withOptions(opts)
            c.program.term.evaluateDebug match
                case Result.Success(term, exunits, costs, logs) =>
                    assert(
                      exunits == budget,
                      s"Budget mismatch for $opts: got $exunits, but expected $budget"
                    )
                    val expectedTerm = ce.program.term.evaluate
                    assert(
                      term α_== expectedTerm,
                      s"Expected term $expectedTerm, but got ${term.show} (opts=$opts)"
                    )
                case Result.Failure(exception, budget, costs, logs) =>
                    fail(s"""Expected success for $opts, but got failure: $exception;
                           |budget: $budget;
                           |costs: ${costs.toMap}""".stripMargin)

    /** Assert that a function evaluates to expected value under multiple compiler option
      * configurations. The argument is passed as Data at runtime, preventing compile-time constant
      * folding.
      */
    protected final inline def assertEvalWithBudgets[A1, T: Eq](
        inline code: A1 => T,
        arg1: A1,
        inline expected: T,
        budgets: Seq[(Options, ExUnits)]
    )(using
        vm: PlutusVM,
        options: Options,
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1]
    ): Unit =
        val compiled = PlutusV3.compile { (d: Data) => code(d.to[A1]) }
        val compiledExpected = PlutusV3.compile(expected)

        assert(
          code(arg1) === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${code(arg1)}"
        )

        for (opts, budget) <- budgets do
            val c = compiled.withOptions(opts)
            val ce = compiledExpected.withOptions(opts)
            val applied = c.program.term $ toData[A1](arg1).asTerm
            applied.evaluateDebug match
                case Result.Success(term, exunits, costs, logs) =>
                    assert(
                      exunits == budget,
                      s"Budget mismatch for $opts: got $exunits, but expected $budget"
                    )
                    val expectedTerm = ce.program.term.evaluate
                    assert(
                      term α_== expectedTerm,
                      s"Expected term $expectedTerm, but got ${term.show} (opts=$opts)"
                    )
                case Result.Failure(exception, budget, costs, logs) =>
                    fail(s"""Expected success for $opts, but got failure: $exception;
                           |budget: $budget;
                           |costs: ${costs.toMap}""".stripMargin)

    /** Mainnet execution-unit prices, used by the `*AndFee` assertions to pin fees in lovelace. */
    protected final def mainnetPrices = CardanoInfo.mainnet.protocolParams.executionUnitPrices

    /** Assert that code evaluates to `expected` with exactly `budget` execution units and exactly
      * `fee` lovelace at mainnet prices, under the given `Options`. On a mismatch the message
      * reports both the actual budget and the actual fee, so a pin can be updated in one run.
      */
    protected final inline def assertEvalWithBudgetAndFee[T: Eq](
        inline code: T,
        inline expected: T,
        budget: ExUnits,
        fee: Coin
    )(using vm: PlutusVM, options: Options): Unit =
        val compiled = PlutusV3.compile(code)
        val compiledExpected = PlutusV3.compile(expected)
        assert(
          compiled.code === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${compiled.code}"
        )
        compiled.program.term.evaluateDebug match
            case Result.Success(term, exunits, costs, logs) =>
                val actualFee = exunits.fee(mainnetPrices)
                assert(
                  exunits == budget && actualFee == fee,
                  s"Budget/fee mismatch for $options: got $exunits fee=${actualFee.value}, " +
                      s"expected $budget fee=${fee.value}"
                )
                val expectedTerm = compiledExpected.program.term.evaluate
                assert(
                  term α_== expectedTerm,
                  s"Expected term $expectedTerm, but got ${term.show} (opts=$options)"
                )
            case Result.Failure(exception, budget, costs, logs) =>
                fail(s"""Expected success for $options, but got failure: $exception;
                       |budget: $budget;
                       |costs: ${costs.toMap}""".stripMargin)

    /** Same as the one-argument `assertEvalWithBudgetAndFee`, with the argument passed as Data at
      * runtime so constant folding cannot remove the work being measured.
      */
    protected final inline def assertEvalWithBudgetAndFee[A1, T: Eq](
        inline code: A1 => T,
        arg1: A1,
        inline expected: T,
        budget: ExUnits,
        fee: Coin
    )(using
        vm: PlutusVM,
        options: Options,
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1]
    ): Unit =
        val compiled = PlutusV3.compile { (d: Data) => code(d.to[A1]) }
        val compiledExpected = PlutusV3.compile(expected)
        assert(
          code(arg1) === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${code(arg1)}"
        )
        val applied = compiled.program.term $ toData[A1](arg1).asTerm
        applied.evaluateDebug match
            case Result.Success(term, exunits, costs, logs) =>
                val actualFee = exunits.fee(mainnetPrices)
                assert(
                  exunits == budget && actualFee == fee,
                  s"Budget/fee mismatch for $options: got $exunits fee=${actualFee.value}, " +
                      s"expected $budget fee=${fee.value}"
                )
                val expectedTerm = compiledExpected.program.term.evaluate
                assert(
                  term α_== expectedTerm,
                  s"Expected term $expectedTerm, but got ${term.show} (opts=$options)"
                )
            case Result.Failure(exception, budget, costs, logs) =>
                fail(s"""Expected success for $options, but got failure: $exception;
                       |budget: $budget;
                       |costs: ${costs.toMap}""".stripMargin)

    /** Assert that code evaluates to different value than expected. No budget pin. */
    protected final inline def assertEvalNotEq[T: Eq](
        inline code: T,
        inline expected: T
    )(using vm: PlutusVM, options: Options, pos: source.Position): Unit =
        val compiled = PlutusV3.compile(code)
        val compiledExpected = PlutusV3.compile(expected)

        assert(
          compiled.code !== compiledExpected.code,
          s"Expected not equal to ${compiledExpected.code}, but got ${compiled.code}"
        )

        val codeTerm = evalSuccess(compiled.program.term)._1
        val expectedTerm = compiledExpected.program.term.evaluate
        assert(
          !(codeTerm α_== expectedTerm),
          s"Expected term not equal to $expectedTerm, but got $codeTerm"
        )

    /** Assert that code evaluation fails with expected exception type. */
    protected final inline def assertEvalFails[E <: Throwable: ClassTag](
        inline code: Any
    )(using vm: PlutusVM, options: Options): Unit =
        var isExceptionThrown = false

        val _ =
            try code
            catch
                case NonFatal(exception) =>
                    assert(
                      summon[ClassTag[E]].runtimeClass.isAssignableFrom(exception.getClass),
                      s"Expected exception of type ${summon[ClassTag[E]]}, but got $exception"
                    )
                    val compiled = PlutusV3.compile(code)
                    val result = compiled.program.evaluateDebug
                    result match
                        case failure: Result.Failure =>
                            result.logs.lastOption match
                                case Some(message) =>
                                    assert(message.contains(exception.getMessage))
                                case None =>
                                    val failMsg = failure.exception.getMessage
                                    val exMsg = exception.getMessage
                                    if exMsg != null && exMsg.nonEmpty then
                                        assert(
                                          failMsg.contains(exMsg),
                                          s"Expected UPLC error to contain '$exMsg', but got: $failMsg"
                                        )
                                    else
                                        assert(
                                          failMsg.contains(exception.getClass.getName),
                                          s"Expected UPLC error to contain '${exception.getClass.getName}', but got: $failMsg"
                                        )
                        case _ =>
                            fail(s"Expected failure, but got success: $result")

                    isExceptionThrown = true

        if !isExceptionThrown then
            fail(s"Expected exception of type ${summon[ClassTag[E]]}, but got success: $code")

    /** Assert that code evaluation fails with specific error message. */
    protected final inline def assertEvalFailsWithMessage[E <: Throwable: ClassTag](
        expectedMessage: String
    )(inline code: Any)(using vm: PlutusVM, options: Options): Unit =
        var isExceptionThrown = false

        val _ =
            try code
            catch
                case NonFatal(exception) =>
                    assert(
                      summon[ClassTag[E]].runtimeClass.isAssignableFrom(exception.getClass),
                      s"Expected exception of type ${summon[ClassTag[E]]}, but got $exception"
                    )

                    assert(
                      exception.getMessage == expectedMessage,
                      s"Expected message '$expectedMessage', but got '${exception.getMessage}'"
                    )

                    val compiled = PlutusV3.compile(code)
                    val result = compiled.program.evaluateDebug
                    result match
                        case failure: Result.Failure =>
                            result.logs.lastOption match
                                case Some(message) =>
                                    assert(message.contains(exception.getMessage))
                                case None =>
                                    assert(
                                      failure.exception.getMessage.contains(
                                        exception.getClass.getName
                                      )
                                    )
                        case _ =>
                            fail(s"Expected failure, but got success: $result")

                    isExceptionThrown = true

        if !isExceptionThrown then
            fail(s"Expected exception of type ${summon[ClassTag[E]]}, but got success: $code")

    /** Assert code evaluates successfully (no exception). */
    protected final inline def assertEvalSuccess(
        inline code: Any
    )(using vm: PlutusVM, options: Options): Unit =
        val _ =
            try code
            catch
                case NonFatal(exception) => fail(s"Expected success, but got exception: $exception")

        val compiled = PlutusV3.compile(code)
        val result = compiled.program.evaluateDebug
        result match
            case failure: Result.Failure =>
                fail(s"Expected success, but got failure: $failure")
            case _ =>

    // ----- Term-Level Assertions -----

    /** Assert two terms evaluate to the same result */
    protected def assertTermEvalEq(a: Term, b: Term)(using vm: PlutusVM): Unit =
        val aResult = a.evaluate
        val bResult = b.evaluate
        assert(aResult α_== bResult, s"Terms not equal: $aResult vs $bResult")

    /** Assert term evaluation throws expected exception */
    protected def assertTermEvalThrows[E <: Throwable: ClassTag](term: Term)(using
        vm: PlutusVM
    ): Unit =
        assertThrows[E](term.evaluate)

    // ----- Property-based Testing -----

    protected inline final def checkEval[A1](
        inline f: A1 => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(using
        vm: PlutusVM,
        options: Options
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        config: PropertyCheckConfiguration,
        a1: Arbitrary[A1],
        s1: Shrink[A1],
        pp1: A1 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion =
        val compiled = PlutusV3.compile { (data: Data) => f(data.to[A1]) }

        def handler(payload: A1): Boolean =
            val applied = compiled.program.term $ toData[A1](payload).asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload)

        check(handler, configParams*)

    protected inline final def checkEval[A1, A2](
        inline f: (A1, A2) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(using
        vm: PlutusVM,
        options: Options
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        config: PropertyCheckConfiguration,
        a1: Arbitrary[A1],
        s1: Shrink[A1],
        pp1: A1 => Pretty,
        a2: Arbitrary[A2],
        s2: Shrink[A2],
        pp2: A2 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion =
        val compiled = PlutusV3.compile { (d1: Data, d2: Data) =>
            f(d1.to[A1], d2.to[A2])
        }

        def handler(payload1: A1, payload2: A2): Boolean =
            val applied = compiled.program.term $ payload1.toData.asTerm $ payload2.toData.asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload1, payload2)

        check(handler, configParams*)

    protected inline final def checkEval[A1, A2, A3](
        inline f: (A1, A2, A3) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(using
        vm: PlutusVM,
        options: Options
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        inline a3FromData: FromData[A3],
        inline a3ToData: ToData[A3],
        config: PropertyCheckConfiguration,
        a1: Arbitrary[A1],
        s1: Shrink[A1],
        pp1: A1 => Pretty,
        a2: Arbitrary[A2],
        s2: Shrink[A2],
        pp2: A2 => Pretty,
        a3: Arbitrary[A3],
        s3: Shrink[A3],
        pp3: A3 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion =
        val compiled = PlutusV3.compile { (d1: Data, d2: Data, d3: Data) =>
            f(d1.to[A1], d2.to[A2], d3.to[A3])
        }

        def handler(payload1: A1, payload2: A2, payload3: A3): Boolean =
            val applied =
                compiled.program.term $ payload1.toData.asTerm $ payload2.toData.asTerm $ payload3.toData.asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload1, payload2, payload3)

        check(handler, configParams*)

    private val trueTerm = true.asTerm
}

object EvalTestKit {

    /** The pending check of `assertEvalEq(expected)(f)`: `.at(a1)` runs it. */
    final class At1[A1](pending: BudgetPins.PendingAt, check: A1 => Unit) {
        def at(a1: A1): Unit = { pending.applied(); check(a1) }
    }

    /** The pending check of `assertEvalEq(expected)(f)`: `.at(a1, a2)` runs it. */
    final class At2[A1, A2](pending: BudgetPins.PendingAt, check: (A1, A2) => Unit) {
        def at(a1: A1, a2: A2): Unit = { pending.applied(); check(a1, a2) }
    }

    /** The pending check of `assertEvalEq(expected)(f)`: `.at(a1, a2, a3)` runs it. */
    final class At3[A1, A2, A3](
        pending: BudgetPins.PendingAt,
        check: (A1, A2, A3) => Unit
    ) {
        def at(a1: A1, a2: A2, a3: A3): Unit = { pending.applied(); check(a1, a2, a3) }
    }
}
