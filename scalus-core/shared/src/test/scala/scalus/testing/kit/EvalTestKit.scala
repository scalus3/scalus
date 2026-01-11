package scalus.testing.kit

import org.scalacheck.util.Pretty
import org.scalacheck.Arbitrary
import org.scalactic.{source, Prettifier}
import org.scalatest.Assertion
import org.scalatest.Assertions
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.Data
import scalus.builtin.Data.{fromData, toData, FromData, ToData}
import scalus.cardano.ledger.ExUnits
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.prelude.{Eq, Option as ScalusOption}
import scalus.uplc.{DeBruijn, PlutusV3, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.test.ArbitraryInstances
import scalus.uplc.eval.*

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
trait EvalTestKit extends Assertions with ScalaCheckPropertyChecks with ArbitraryInstances {
    export org.scalatestplus.scalacheck.Checkers.*
    export org.scalacheck.{Arbitrary, Gen, Shrink}
    export scalus.prelude.{!==, <=>, ===}

    given compilerOptions: Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    /** Default PlutusVM - can be overridden by subclasses */
    protected def plutusVM: PlutusVM = PlutusVM.makePlutusV3VM()

    /** Given PlutusVM using plutusVM - methods can override via `using` parameter */
    protected given PlutusVM = plutusVM

    // ----- Utility -----

    protected final inline def liftThrowableToOption[A](inline code: A): ScalusOption[A] =
        try ScalusOption.Some(code)
        catch case NonFatal(_) => ScalusOption.None

    // ----- Inline Compilation Assertions -----

    /** Assert that code evaluates to true on both JVM and PlutusVM. */
    protected final inline def assertEval(inline code: Boolean)(using vm: PlutusVM): Unit =
        assert(code)
        val compiled = PlutusV3.compile(code)
        val codeTerm = vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(compiled.program.term))
        assert(codeTerm α_== trueTerm)

    /** Assert that code evaluates to expected value on both JVM and PlutusVM. */
    protected final inline def assertEvalEq[T: Eq](
        inline code: T,
        inline expected: T
    )(using vm: PlutusVM): Unit =
        val compiled = PlutusV3.compile(code)
        val compiledExpected = PlutusV3.compile(expected)

        assert(
          compiled.code === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${compiled.code}"
        )

        val codeTerm = vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(compiled.program.term))
        val expectedTerm =
            vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(compiledExpected.program.term))
        assert(
          codeTerm α_== expectedTerm,
          s"Expected term $expectedTerm, but got $codeTerm"
        )

    /** Assert that code evaluates to expected value with budget limit check. */
    protected final inline def assertEvalWithinBudget[T: Eq](
        inline code: T,
        inline expected: T,
        budget: ExUnits
    )(using vm: PlutusVM): Unit =
        val compiled = PlutusV3.compile(code)
        val compiledExpected = PlutusV3.compile(expected)

        assert(
          compiled.code === compiledExpected.code,
          s"Expected ${compiledExpected.code}, but got ${compiled.code}"
        )

        val spender = TallyingBudgetSpenderLogger(CountingBudgetSpender())

        val codeTerm = vm.evaluateDeBruijnedTerm(
          DeBruijn.deBruijnTerm(compiled.program.term),
          budgetSpender = spender
        )

        if spender.getSpentBudget.steps > budget.steps ||
            spender.getSpentBudget.memory > budget.memory
        then
            fail:
                s"""Performance regression,
                |expected: $budget,
                |but got: ${spender.getSpentBudget};
                |costs: ${spender.costs.toMap}""".stripMargin

        val expectedTerm =
            vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(compiledExpected.program.term))
        assert(
          codeTerm α_== expectedTerm,
          s"Expected term $expectedTerm, but got $codeTerm"
        )

    /** Assert that code evaluates to different value than expected. */
    protected final inline def assertEvalNotEq[T: Eq](
        inline code: T,
        inline expected: T
    )(using vm: PlutusVM): Unit =
        val compiled = PlutusV3.compile(code)
        val compiledExpected = PlutusV3.compile(expected)

        assert(
          compiled.code !== compiledExpected.code,
          s"Expected not equal to ${compiledExpected.code}, but got ${compiled.code}"
        )

        val codeTerm = vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(compiled.program.term))
        val expectedTerm =
            vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(compiledExpected.program.term))
        assert(
          !(codeTerm α_== expectedTerm),
          s"Expected term not equal to $expectedTerm, but got $codeTerm"
        )

    /** Assert that code evaluation fails with expected exception type. */
    protected final inline def assertEvalFails[E <: Throwable: ClassTag](
        inline code: Any
    )(using vm: PlutusVM): Unit =
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

    /** Assert that code evaluation fails with expected exception type and budget limit check. */
    protected final inline def assertEvalFailsWithinBudget[E <: Throwable: ClassTag](
        inline code: Any,
        budget: ExUnits
    )(using vm: PlutusVM): Unit =
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
                                    assert(
                                      failure.exception.getMessage.contains(
                                        exception.getClass.getName
                                      )
                                    )
                                    if result.budget.steps > budget.steps ||
                                        result.budget.memory > budget.memory
                                    then
                                        fail:
                                            s"""Performance regression,
                                            |expected: $budget,
                                            |but got: ${result.budget};
                                            |costs: ${result.costs}""".stripMargin
                        case _ =>
                            fail(s"Expected failure, but got success: $result")

                    isExceptionThrown = true

        if !isExceptionThrown then
            fail(s"Expected exception of type ${summon[ClassTag[E]]}, but got success: $code")

    /** Assert that code evaluation fails with specific error message. */
    protected final inline def assertEvalFailsWithMessage[E <: Throwable: ClassTag](
        expectedMessage: String
    )(inline code: Any)(using vm: PlutusVM): Unit =
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
    protected final inline def assertEvalSuccess(inline code: Any)(using vm: PlutusVM): Unit =
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

    /** Assert code compiles and evaluates without checking result. */
    protected final inline def assertEvalCompile(inline code: Any)(using vm: PlutusVM): Unit =
        val compiled = PlutusV3.compile(code)
        compiled.program.term.evaluate

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
        vm: PlutusVM
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
        val compiled = PlutusV3.compile { (data: Data) => f(fromData[A1](data)) }

        def handler(payload: A1): Boolean =
            val applied = compiled.program.term $ toData[A1](payload).asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload)

        check(handler, configParams*)

    protected inline final def checkEval[A1, A2](
        inline f: (A1, A2) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(using
        vm: PlutusVM
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
            f(fromData[A1](d1), fromData[A2](d2))
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
        vm: PlutusVM
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
            f(fromData[A1](d1), fromData[A2](d2), fromData[A3](d3))
        }

        def handler(payload1: A1, payload2: A2, payload3: A3): Boolean =
            val applied =
                compiled.program.term $ payload1.toData.asTerm $ payload2.toData.asTerm $ payload3.toData.asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload1, payload2, payload3)

        check(handler, configParams*)

    private val trueTerm = PlutusV3.compile(true).program.term.evaluate
}
