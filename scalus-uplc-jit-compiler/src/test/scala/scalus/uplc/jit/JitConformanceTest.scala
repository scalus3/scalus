package scalus.uplc.jit

import scalus.cardano.ledger.ExUnits
import scalus.uplc.eval.PlutusConformanceTest
import scalus.uplc.{Program, Term}

import scala.util.control.NonFatal

/** The Plutus conformance corpus, evaluated with the JIT instead of the CEK machine.
  *
  * The corpus is generated under builtin semantics variant E (PlutusV3 at PV11), which is the scope
  * of the JIT, and [[PlutusConformanceTest]] builds its machine parameters from the variant-E
  * reference cost models. This class reuses its case discovery, file reading and machine
  * parameters, and replaces only [[check]]: the JIT compiles the term, runs it with those
  * parameters, and its result and budget are compared with the expectation files. Every case must
  * pass: a difference from the CEK machine is a defect (spec [SC-5]).
  *
  * The JIT returns a [[UplcValue]], not a term. A `con` or `constr` result is compared by value
  * (see [[JitCekDiff.matches]]); any other expected term (a `lam`, a `delay`, a partly applied
  * builtin) can only be checked to be a function, plus the budget.
  */
class JitConformanceTest extends PlutusConformanceTest {

    /** What compiling and running one corpus program produced. */
    private enum Outcome:
        case ParseError
        case CompileError(e: Throwable)
        case EvaluationFailure(e: Throwable)
        case Success(value: UplcValue, budget: ExUnits)

    /** The corpus cases that need Dijkstra (PV12): they case on `Data.Constr` (Plutus 1.69). The
      * JIT targets PV11 and rejects every Data scrutinee (`JitRuntime.caseIndex`), so these cases
      * cannot agree with the suite's PV12 CEK machine.
      */
    override protected def ignoredCases: Map[String, String] =
        discoveredCases.filter(needsDijkstra).map(_ -> "the JIT has no case on Data (PV12)").toMap

    override protected def check(name: String): Unit = test(name) {
        val outcome = parseScoped(readFile(s"$path/$name.uplc")) match
            case None          => Outcome.ParseError
            case Some(program) => run(program.term)
        assert(matches(name, outcome), s"$name: $outcome")
    }

    test("count of cases whose expected result is compared only as a function") {
        val functionLike = discoveredCases.count { name =>
            expectationOf(name) == CaseExpectation.TermAndBudget &&
            JitCekDiff.isFunctionLike(expectedTerm(name))
        }
        info(s"$functionLike of ${discoveredCases.size} cases expect a function result")
    }

    private def run(term: Term): Outcome = {
        // A compile failure is never an evaluation failure: a builtin the JIT cannot compile must
        // not pass the `evaluation failure` cases by accident.
        val compiled =
            try nativestack.JIT.jitUplc(term)
            catch case NonFatal(e) => return Outcome.CompileError(e)
        val result = JitCekDiff.runJit(compiled, plutusVM.machineParams)
        result.result match
            case scala.util.Success(value) => Outcome.Success(value, result.total)
            case scala.util.Failure(e)     => Outcome.EvaluationFailure(e)
    }

    private def matches(name: String, outcome: Outcome): Boolean =
        expectationOf(name) match
            case CaseExpectation.ParseError        => outcome == Outcome.ParseError
            case CaseExpectation.EvaluationFailure =>
                // the JIT must fail as the CEK machine fails, with the same exception class
                outcome match
                    case Outcome.EvaluationFailure(e) => JitCekDiff.sameFailure(cekFailure(name), e)
                    case _                            => false
            case CaseExpectation.TermAndBudget =>
                outcome match
                    case Outcome.Success(value, budget) =>
                        budget == expectedBudget(name) &&
                        JitCekDiff.matches(expectedTerm(name), value)
                    case _ => false

    /** What the CEK machine of the suite throws for the program of `name`. */
    private def cekFailure(name: String): Throwable =
        Program.parseUplc(readFile(s"$path/$name.uplc")) match
            case Right(program) =>
                JitCekDiff
                    .runCek(plutusVM, program.term)
                    .result
                    .failed
                    .getOrElse(
                      fail(s"$name: the CEK machine does not fail")
                    )
            case Left(err) => fail(s"$name: cannot parse the program: $err")

    private def expectedTerm(name: String): Term =
        Program.parseUplc(readFile(s"$path/$name.uplc.expected")) match
            case Right(program) => program.term
            case Left(err)      => fail(s"$name: cannot parse the expected result: $err")

    private val BudgetRegex = """\(\{cpu:\s(\d+)\n\|\smem:\s(\d+)\}\)""".r

    private def expectedBudget(name: String): ExUnits =
        readFile(s"$path/$name.budget.expected") match
            case BudgetRegex(cpu, mem) => ExUnits(memory = mem.toLong, steps = cpu.toLong)
            case other                 => fail(s"$name: not a budget: $other")
}
