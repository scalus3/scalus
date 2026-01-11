package scalus.testing.dsl

import scalus.cardano.ledger.ExUnits
import scalus.compiler.sir.{SIR, TargetLoweringBackend}
import scalus.testing.assertions.{MultiResultAssertions, ResultAssertions}
import scalus.testing.eval.{EvalConfig, Evaluator, MultiEvalResult}
import scalus.uplc.{Compiled, Constant, Program, Term}
import scalus.uplc.eval.{PlutusVM, Result}

/** Fluent builder for evaluation tests.
  *
  * Usage:
  * {{{
  * eval(sir)
  *     .onAllBackends
  *     .withUplcCli
  *     .expectSuccess()
  *     .assertBudgetWithin(1000, 10000)
  * }}}
  */
class EvalTestBuilder[A](
    private val subject: EvalSubject[A],
    private val config: EvalConfig = EvalConfig.default
):
    /** Configure with custom EvalConfig */
    def withConfig(newConfig: EvalConfig): EvalTestBuilder[A] =
        new EvalTestBuilder(subject, newConfig)

    /** Use all backends for testing */
    def onAllBackends: EvalTestBuilder[A] =
        new EvalTestBuilder(subject, config.copy(backends = TargetLoweringBackend.values.toSeq))

    /** Include UplcCli in testing (JVM-only) */
    def withUplcCli: EvalTestBuilder[A] =
        new EvalTestBuilder(subject, config.copy(useUplcCli = true))

    /** Use specific PlutusVM */
    def onVM(vm: PlutusVM): EvalTestBuilder[A] =
        new EvalTestBuilder(subject, config.copy(vms = Seq(vm)))

    /** Assert the evaluation succeeds */
    def expectSuccess(): EvalTestResult =
        evaluate().assertSuccess()

    /** Assert the evaluation succeeds with a specific term */
    def expectTerm(term: Term): EvalTestResult =
        evaluate().assertTermEquals(term)

    /** Assert the evaluation succeeds with a specific value */
    def expectValue[V: Constant.LiftValue](value: V): EvalTestResult =
        evaluate().assertValueEquals(value)

    /** Assert the evaluation fails */
    def expectFailure(): EvalTestResult =
        evaluate().assertFailure()

    /** Assert the evaluation fails with a message containing the substring */
    def expectFailureWith(message: String): EvalTestResult =
        evaluate().assertFailureWith(message)

    /** Assert results are consistent across all backends */
    def expectSameOnAllBackends(): EvalTestResult =
        val multiResult = evaluateMulti()
        MultiResultAssertions.assertAllSame(multiResult)
        EvalTestResult(multiResult.results.values.head, config)

    /** Assert budget is within limits */
    def expectBudgetWithin(memory: Long, steps: Long): EvalTestResult =
        val result = evaluate()
        ResultAssertions.assertBudgetWithin(ExUnits(memory, steps), result.result)
        result

    private def evaluate(): EvalTestResult =
        given PlutusVM = config.vms.head
        val result = subject match
            case EvalSubject.FromSIR(sir) =>
                Evaluator.evalSIR(
                  sir,
                  config.backends.head,
                  config.compilerOptions.generateErrorTraces
                )
            case EvalSubject.FromCompiled(compiled) =>
                Evaluator.evalCompiled(compiled)
            case EvalSubject.FromTerm(term) =>
                Evaluator.evalTerm(term)
            case EvalSubject.FromProgram(program) =>
                Evaluator.evalProgram(program)
        EvalTestResult(result, config)

    private def evaluateMulti(): MultiEvalResult =
        subject match
            case EvalSubject.FromSIR(sir) => Evaluator.evalMulti(sir, config)
            case _ =>
                throw new UnsupportedOperationException(
                  "Multi-backend evaluation only supported for SIR"
                )

/** Subject of evaluation - what we're testing */
enum EvalSubject[A]:
    case FromSIR(sir: SIR)
    case FromCompiled[B](compiled: Compiled[B]) extends EvalSubject[B]
    case FromTerm(term: Term) extends EvalSubject[Term]
    case FromProgram(program: Program) extends EvalSubject[Program]

/** Result of an evaluation test with additional assertion methods */
case class EvalTestResult(result: Result, config: EvalConfig):

    def assertSuccess(): EvalTestResult =
        ResultAssertions.assertSuccess(result)
        this

    def assertFailure(): EvalTestResult =
        ResultAssertions.assertFailure(result)
        this

    def assertFailureWith(message: String): EvalTestResult =
        ResultAssertions.assertFailureWith(message, result)
        this

    def assertTermEquals(expected: Term): EvalTestResult =
        ResultAssertions.assertTermEquals(expected, result)
        this

    def assertValueEquals[V: Constant.LiftValue](expected: V): EvalTestResult =
        val expectedTerm = Term.Const(Constant.asConstant(expected))
        ResultAssertions.assertTermEquals(expectedTerm, result)
        this

    def assertBudgetWithin(memory: Long, steps: Long): EvalTestResult =
        ResultAssertions.assertBudgetWithin(ExUnits(memory, steps), result)
        this

    def assertBudgetEquals(memory: Long, steps: Long): EvalTestResult =
        ResultAssertions.assertBudgetEquals(ExUnits(memory, steps), result)
        this

    /** Get the budget from a successful result */
    def budget: ExUnits = result match
        case Result.Success(_, b, _, _) => b
        case _ => throw new IllegalStateException("Cannot get budget from failed result")

    /** Get logs from the result */
    def logs: Seq[String] = result.logs

/** DSL entry points */
object EvalTestDsl:

    /** Start testing from SIR */
    def eval(sir: SIR): EvalTestBuilder[SIR] =
        new EvalTestBuilder(EvalSubject.FromSIR(sir))

    /** Start testing from a Compiled script */
    def eval[A](compiled: Compiled[A]): EvalTestBuilder[A] =
        new EvalTestBuilder(EvalSubject.FromCompiled(compiled))

    /** Start testing from a Term */
    def eval(term: Term): EvalTestBuilder[Term] =
        new EvalTestBuilder(EvalSubject.FromTerm(term))

    /** Start testing from a Program */
    def eval(program: Program): EvalTestBuilder[Program] =
        new EvalTestBuilder(EvalSubject.FromProgram(program))
