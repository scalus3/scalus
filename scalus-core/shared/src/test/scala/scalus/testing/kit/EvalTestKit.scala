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
import scalus.compiler.{compileInline, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.prelude.{Eq, Option as ScalusOption}
import scalus.toUplc
import scalus.uplc.{DeBruijn, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.test.ArbitraryInstances
import scalus.uplc.eval.*

import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/** ScalaTest integration trait for evaluation testing.
  *
  * Provides:
  *   - Inline compilation via `assertEval*` methods
  *   - Given-based configuration injection
  *   - Property-based testing support via `checkEval`
  *   - Infix syntax: `code evalEq expected`
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

    protected given PlutusVM = PlutusVM.makePlutusV3VM()

    // ----- Utility -----

    protected final inline def liftThrowableToOption[A](inline code: A): ScalusOption[A] =
        try ScalusOption.Some(code)
        catch case NonFatal(_) => ScalusOption.None

    // ----- Inline Compilation Assertions -----

    /** Assert that code evaluates to true on both JVM and PlutusVM. */
    protected final inline def assertEval(inline code: Boolean): Unit =
        assert(code)
        val codeTerm = compileInline(code).toUplc(true).evaluate
        assert(codeTerm α_== trueTerm)

    /** Assert that code evaluates to expected value on both JVM and PlutusVM. */
    protected final inline def assertEvalEq[T: Eq](inline code: T, inline expected: T): Unit =
        assertEvalEqBudget(code, expected)

    /** Assert that code evaluates to expected value with budget check. */
    protected final inline def assertEvalEqBudget[T: Eq](
        inline code: T,
        inline expected: T,
        budget: scala.Option[ExUnits] = None
    ): Unit =
        assert(code === expected, s"Expected $expected, but got $code")

        val vm = summon[PlutusVM]
        val spender =
            if budget.nonEmpty
            then TallyingBudgetSpenderLogger(CountingBudgetSpender())
            else NoBudgetSpender

        val codeTerm = vm.evaluateDeBruijnedTerm(
          DeBruijn.deBruijnTerm(compileInline(code).toUplc(true)),
          budgetSpender = spender
        )

        if budget.exists: budget =>
                spender.getSpentBudget.steps > budget.steps ||
                    spender.getSpentBudget.memory > budget.memory
        then
            fail:
                s"""Performance regression,
                |expected: ${budget.get},
                |but got: ${spender.getSpentBudget};
                |costs: ${spender
                      .asInstanceOf[TallyingBudgetSpenderLogger]
                      .costs
                      .toMap}""".stripMargin

        val expectedTerm = compileInline(expected).toUplc(true).evaluate
        assert(
          codeTerm α_== expectedTerm,
          s"Expected term $expectedTerm, but got $codeTerm"
        )

    /** Assert that code evaluates to different value than expected. */
    protected final inline def assertEvalNotEq[T: Eq](inline code: T, inline expected: T): Unit =
        assert(code !== expected, s"Expected not equal to $expected, but got $code")

        val codeTerm = compileInline(code).toUplc(true).evaluate
        val expectedTerm = compileInline(expected).toUplc(true).evaluate
        assert(
          !(codeTerm α_== expectedTerm),
          s"Expected term not equal to $expectedTerm, but got $codeTerm"
        )

    /** Assert that code evaluation fails with expected exception type. */
    protected final inline def assertEvalFails[E <: Throwable: ClassTag](inline code: Any): Unit =
        assertEvalBudgetFails[E](code)

    /** Assert that code evaluation fails with expected exception type and budget check. */
    protected final inline def assertEvalFails[E <: Throwable: ClassTag](cpu: Long, memory: Long)(
        inline code: Any
    ): Unit = assertEvalBudgetFails[E](code, Some(ExUnits(memory = memory, steps = cpu)))

    /** Assert that code evaluation fails with budget verification. */
    protected final inline def assertEvalBudgetFails[E <: Throwable: ClassTag](
        inline code: Any,
        budget: scala.Option[ExUnits] = None
    ): Unit =
        var isExceptionThrown = false

        val _ =
            try code
            catch
                case NonFatal(exception) =>
                    assert(
                      summon[ClassTag[E]].runtimeClass.isAssignableFrom(exception.getClass),
                      s"Expected exception of type ${summon[ClassTag[E]]}, but got $exception"
                    )
                    val result = compileInline(code).toUplc(true).evaluateDebug
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
                                    if budget.exists: budget =>
                                            result.budget.steps > budget.steps ||
                                                result.budget.memory > budget.memory
                                    then
                                        fail:
                                            s"""Performance regression,
                                            |expected: ${budget.get},
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
    )(inline code: Any): Unit =
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

                    val result = compileInline(code).toUplc(true).evaluateDebug
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
    protected final inline def assertEvalSuccess(inline code: Any): Unit =
        val _ =
            try code
            catch
                case NonFatal(exception) => fail(s"Expected success, but got exception: $exception")

        val result = compileInline(code).toUplc(true).evaluateDebug
        result match
            case failure: Result.Failure =>
                fail(s"Expected success, but got failure: $failure")
            case _ =>

    /** Assert code compiles and evaluates without checking result. */
    protected final inline def assertEvalCompile(inline code: Any): Unit =
        compileInline(code).toUplc(true).evaluate

    // ----- Infix Extension Methods -----

    extension [T: Eq](inline code: T)
        /** Infix syntax with budget: `code evalEq (cpu, mem) (expected)` */
        @targetName("assertEvalEqBudgetTo")
        protected final inline infix def evalEq(cpu: Long, memory: Long)(inline expected: T): Unit =
            assertEvalEqBudget(code, expected, Some(ExUnits(memory = memory, steps = cpu)))

        /** Infix syntax: `code evalEq expected` */
        @targetName("assertEvalEqTo")
        protected final inline infix def evalEq(inline expected: T): Unit =
            assertEvalEqBudget(code, expected)

    // ----- Term-Level Assertions -----

    /** Assert two terms evaluate to the same result */
    protected def assertTermEvalEq(a: Term, b: Term): Unit =
        val aResult = a.evaluate
        val bResult = b.evaluate
        assert(aResult α_== bResult, s"Terms not equal: $aResult vs $bResult")

    /** Assert term evaluation throws expected exception */
    protected def assertTermEvalThrows[E <: Throwable: ClassTag](term: Term): Unit =
        assertThrows[E](term.evaluate)

    // ----- Property-based Testing -----

    protected inline final def checkEval[A1](
        inline f: A1 => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
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
        val sir = compileInline { (data: Data) => f(fromData[A1](data)) }
        val uplc = sir.toUplc(true)

        def handler(payload: A1): Boolean =
            val applied = uplc $ toData[A1](payload).asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload)

        check(handler, configParams*)

    protected inline final def checkEval[A1, A2](
        inline f: (A1, A2) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
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
        val sir = compileInline { (d1: Data, d2: Data) =>
            f(fromData[A1](d1), fromData[A2](d2))
        }

        val uplc = sir.toUplc(true)

        def handler(payload1: A1, payload2: A2): Boolean =
            val applied = uplc $ payload1.toData.asTerm $ payload2.toData.asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload1, payload2)

        check(handler, configParams*)

    protected inline final def checkEval[A1, A2, A3](
        inline f: (A1, A2, A3) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
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
        val sir = compileInline { (d1: Data, d2: Data, d3: Data) =>
            f(fromData[A1](d1), fromData[A2](d2), fromData[A3](d3))
        }

        val uplc = sir.toUplc(true)

        def handler(payload1: A1, payload2: A2, payload3: A3): Boolean =
            val applied =
                uplc $ payload1.toData.asTerm $ payload2.toData.asTerm $ payload3.toData.asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload1, payload2, payload3)

        check(handler, configParams*)

    protected inline final def checkEval[A1, A2, A3, A4](
        inline f: (A1, A2, A3, A4) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        inline a3FromData: FromData[A3],
        inline a3ToData: ToData[A3],
        inline a4FromData: FromData[A4],
        inline a4ToData: ToData[A4],
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
        a4: Arbitrary[A4],
        s4: Shrink[A4],
        pp4: A4 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion =
        val sir = compileInline { (d1: Data, d2: Data, d3: Data, d4: Data) =>
            f(fromData[A1](d1), fromData[A2](d2), fromData[A3](d3), fromData[A4](d4))
        }

        val uplc = sir.toUplc(true)

        def handler(payload1: A1, payload2: A2, payload3: A3, payload4: A4): Boolean =
            val applied =
                uplc $ payload1.toData.asTerm $ payload2.toData.asTerm $ payload3.toData.asTerm $ payload4.toData.asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload1, payload2, payload3, payload4)

        check(handler, configParams*)

    protected inline final def checkEval[A1, A2, A3, A4, A5](
        inline f: (A1, A2, A3, A4, A5) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        inline a3FromData: FromData[A3],
        inline a3ToData: ToData[A3],
        inline a4FromData: FromData[A4],
        inline a4ToData: ToData[A4],
        inline a5FromData: FromData[A5],
        inline a5ToData: ToData[A5],
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
        a4: Arbitrary[A4],
        s4: Shrink[A4],
        pp4: A4 => Pretty,
        a5: Arbitrary[A5],
        s5: Shrink[A5],
        pp5: A5 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion =
        val sir = compileInline { (d1: Data, d2: Data, d3: Data, d4: Data, d5: Data) =>
            f(
              fromData[A1](d1),
              fromData[A2](d2),
              fromData[A3](d3),
              fromData[A4](d4),
              fromData[A5](d5)
            )
        }

        val uplc = sir.toUplc(true)

        def handler(
            payload1: A1,
            payload2: A2,
            payload3: A3,
            payload4: A4,
            payload5: A5
        ): Boolean =
            val applied =
                uplc $ payload1.toData.asTerm $ payload2.toData.asTerm $ payload3.toData.asTerm $ payload4.toData.asTerm $ payload5.toData.asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(payload1, payload2, payload3, payload4, payload5)

        check(handler, configParams*)

    protected inline final def checkEval[A1, A2, A3, A4, A5, A6](
        inline f: (A1, A2, A3, A4, A5, A6) => Boolean,
        configParams: org.scalatestplus.scalacheck.Checkers.PropertyCheckConfigParam*
    )(implicit
        inline a1FromData: FromData[A1],
        inline a1ToData: ToData[A1],
        inline a2FromData: FromData[A2],
        inline a2ToData: ToData[A2],
        inline a3FromData: FromData[A3],
        inline a3ToData: ToData[A3],
        inline a4FromData: FromData[A4],
        inline a4ToData: ToData[A4],
        inline a5FromData: FromData[A5],
        inline a5ToData: ToData[A5],
        inline a6FromData: FromData[A6],
        inline a6ToData: ToData[A6],
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
        a4: Arbitrary[A4],
        s4: Shrink[A4],
        pp4: A4 => Pretty,
        a5: Arbitrary[A5],
        s5: Shrink[A5],
        pp5: A5 => Pretty,
        a6: Arbitrary[A6],
        s6: Shrink[A6],
        pp6: A6 => Pretty,
        prettifier: Prettifier,
        pos: source.Position
    ): Assertion =
        val sir =
            compileInline { (d1: Data, d2: Data, d3: Data, d4: Data, d5: Data, d6: Data) =>
                f(
                  fromData[A1](d1),
                  fromData[A2](d2),
                  fromData[A3](d3),
                  fromData[A4](d4),
                  fromData[A5](d5),
                  fromData[A6](d6)
                )
            }

        val uplc = sir.toUplc(true)

        def handler(
            payload1: A1,
            payload2: A2,
            payload3: A3,
            payload4: A4,
            payload5: A5,
            payload6: A6
        ): Boolean =
            val applied =
                uplc $ payload1.toData.asTerm $ payload2.toData.asTerm $ payload3.toData.asTerm $ payload4.toData.asTerm $ payload5.toData.asTerm $ payload6.toData.asTerm
            val resultTerm = applied.evaluate
            (resultTerm α_== trueTerm) && f(
              payload1,
              payload2,
              payload3,
              payload4,
              payload5,
              payload6
            )

        check(handler, configParams*)

    private val trueTerm = compileInline(true).toUplc(true).evaluate
}
