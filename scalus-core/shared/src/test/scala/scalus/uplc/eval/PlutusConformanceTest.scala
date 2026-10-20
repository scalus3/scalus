package scalus
package uplc
package eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.platform
import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion}

import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** Tests for the Plutus Conformance Test Suite. */
abstract class PlutusConformanceTest extends AnyFunSuite:
    /** Plutus's reference variant-E machine parameters.
      *
      * The Plutus conformance corpus (>= 1.63 / van Rossem) is generated under semantics variant E:
      * PV11 features (case-on-builtins), UTF-8-byte-length text costing, consByteString range
      * checks, and Int64-bounded shift/rotate amounts. Build the VM from Plutus's reference
      * variant-E builtin cost model AND reference variant-E CEK machine costs so produced budgets
      * match the corpus exactly.
      */
    private lazy val referenceParams: MachineParams = {
        def reference(name: String): String =
            new String(platform.readFile(s"scalus-core/shared/src/main/resources/$name"), "UTF-8")
        val builtinCostModel = BuiltinCostModel.fromJsonString(reference("builtinCostModelE.json"))
        val machineCosts = CekMachineCosts.fromMap(
          ujson
              .read(reference("cekMachineCostsE.json"))
              .obj
              .iterator
              .flatMap { (key, value) =>
                  Seq(
                    s"$key-exBudgetCPU" -> value("exBudgetCPU").num.toLong,
                    s"$key-exBudgetMemory" -> value("exBudgetMemory").num.toLong
                  )
              }
              .toMap
        )
        MachineParams(machineCosts, builtinCostModel)
    }

    /** The VM at van Rossem (PV11), the protocol version mainnet runs. Every case uses it, except
      * the ones that need Dijkstra (see [[needsDijkstra]]).
      */
    protected lazy val plutusVM: PlutusVM =
        PlutusVM.makePlutusV3VM(referenceParams, MajorProtocolVersion.vanRossemPV)

    /** The VM at Dijkstra (PV12). makePlutusV3VM derives the same variant E from it. */
    protected lazy val dijkstraVM: PlutusVM =
        PlutusVM.makePlutusV3VM(referenceParams, MajorProtocolVersion.dijkstraPV)

    /** Since 1.69 the corpus cases on `Data.Constr` (term/constant-case/data), which Plutus enables
      * only from Dijkstra (PV12). Only those cases run on [[dijkstraVM]], so the rest of the corpus
      * checks the protocol version mainnet runs.
      */
    protected def needsDijkstra(name: String): Boolean =
        name.startsWith("term/constant-case/data/")

    protected given PlutusVM = plutusVM

    // Every UPLC evaluation case in the linked plutus-conformance corpus is discovered at compile
    // time (see Macros.conformanceEvaluationCasesImpl) and run by default; only cases listed in
    // `ignoredCases` are skipped. Adding a new corpus case requires no change here.
    private inline def discoveredCasesInline: List[String] =
        ${ scalus.utils.Macros.conformanceEvaluationCasesImpl }
    protected val discoveredCases: List[String] = discoveredCasesInline

    /** Conformance cases this platform deliberately skips, each with the reason. Keys are corpus
      * case paths relative to `uplc/evaluation`, without the `.uplc` suffix.
      *
      * Empty by default. A skip is a statement about one platform's dependencies, so it belongs in
      * that platform's subclass — keeping the blst cases here made the JS build, which does not use
      * blst at all, claim a JVM defect as its own.
      */
    protected def ignoredCases: Map[String, String] = Map.empty

    discoveredCases.foreach { name =>
        ignoredCases.get(name) match
            case Some(reason) => ignore(s"$name [ignored: $reason]")(())
            case None         => check(name)
    }

    test("the cases that need Dijkstra fail at van Rossem (PV11)") {
        val dijkstraCases = discoveredCases.filter(needsDijkstra)
        assert(dijkstraCases.nonEmpty, "no corpus case needs Dijkstra; remove needsDijkstra")
        for name <- dijkstraCases do
            eval(readFile(s"$path/$name.uplc"), plutusVM) match
                case Right(_: Result.Failure) =>
                case other => fail(s"$name does not need Dijkstra: at PV11 it gives $other")
    }

    test("ignoredCases has no stale entries (all present in the corpus)") {
        val stale = ignoredCases.keySet -- discoveredCases.toSet
        assert(stale.isEmpty, s"ignoredCases lists cases not in the corpus: ${stale.toList.sorted}")
    }

    private type EvalFailure = "evaluation failure"
    private type ParseError = "parse/decode error"
    private type Error = EvalFailure | ParseError
    private def parseExpected(code: String): Either[Error, Term] = {
        code match
            case "evaluation failure" => Left("evaluation failure")
            case "parse/decode error" => Left("parse/decode error")
            case _ =>
                Program.parseUplc(code) match
                    case Left(value) => fail(s"Unexpected parse error: $value")
                    case Right(program) =>
                        Right(DeBruijn.deBruijnTerm(program.term))

    }

    /** Parses a corpus program the way the Plutus conformance runner does.
      *
      * The runner reports a free variable as `parse/decode error`, not as an evaluation failure: a
      * `.flat` file cannot decode with one, and the textual case must agree with it (see
      * `getInputProg` in plutus-conformance `PlutusConformance/Common.hs`). So a program that
      * parses but does not convert to De Bruijn indices is a parse error here too.
      */
    protected def parseScoped(code: String): Option[Program] =
        Program.parseUplc(code).toOption.filter { program =>
            Try(DeBruijn.deBruijnTerm(program.term, throwOnFreeVariable = true)).isSuccess
        }

    private def eval(code: String, vm: PlutusVM): Either[Error, Result] = {
        parseScoped(code) match
            case Some(program) => Right(program.term.evaluateDebug(using vm))
            case None          => Left("parse/decode error")
    }

    protected def readFile(path: String): String = {
        new String(platform.readFile(path), "UTF-8")
    }

    protected def path = s"plutus-conformance/test-cases/uplc/evaluation"

    private val BudgetRegex = """\(\{cpu:\s(\d+)\n\|\smem:\s(\d+)\}\)""".r

    protected def check(name: String): Unit =
        test(name) {
            val code = readFile(s"$path/$name.uplc")
            val expected = readFile(s"$path/$name.uplc.expected")
            val expectedBudget =
                val budget = Try(readFile(s"$path/$name.budget.expected"))
                budget match
                    case Success(BudgetRegex(cpu, mem)) =>
                        Right(ExUnits(memory = mem.toLong, steps = cpu.toLong))
                    case Success("parse/decode error") => Left("parse/decode error")
                    case Success("evaluation failure") => Left("evaluation failure")
                    case Failure(_)                    => Left("no file")
                    case _ => fail(s"Unexpected budget format:\n$budget")

            // println(eval(code).show)
            (
              eval(code, if needsDijkstra(name) then dijkstraVM else plutusVM),
              parseExpected(expected),
              expectedBudget
            ) match
                case (
                      Right(Result.Success(actualTerm, budget, _, _)),
                      Right(expectedTerm),
                      expectedBudget
                    ) =>
                    assert(
                      actualTerm α_== expectedTerm,
                      s"Expected $expectedTerm but got $actualTerm"
                    )
                    expectedBudget match
                        case Right(expectedBudget) =>
                            assert(
                              budget == expectedBudget,
                              s"Expected $expectedBudget but got $budget"
                            )
                        case Left("no file") => // ignore
                        case Left(e)         => fail(s"Expected $e but got $budget")
                case (
                      Right(Result.Failure(actualTerm, _, _, _)),
                      Left("evaluation failure"),
                      Left("evaluation failure")
                    ) =>
                    assert(true)
                case (Left(e1), Left(e2), Left(_)) => assert(e1 == e2)
                case (a, b, c)                     => fail(s"Expected $b but got $a and budget $c")
        }

    /** Which of [[check]]'s three outcomes the corpus asks of one case.
      *
      * The conformance numbers published in the npm README and on scalus.org are a count of these,
      * so the guard that pins them classifies cases through the same expectation files [[check]]
      * reads, rather than through a second copy of the rules that could drift away from it.
      */
    protected enum CaseExpectation:
        /** The expectation file holds a term and the budget file holds `({cpu: … | mem: …})`, so
          * [[check]] asserts both: the term is α-equivalent to the expected one, and the CPU and
          * memory spent are exactly the reference's.
          */
        case TermAndBudget

        /** Both expectation files say `evaluation failure`. There is no term or budget to compare,
          * so [[check]] asserts only that evaluation fails here too.
          */
        case EvaluationFailure

        /** Both expectation files say `parse/decode error`. [[check]] asserts only that the program
          * is rejected at parse time here too.
          */
        case ParseError

    /** Classifies one corpus case by what [[check]] asserts about it.
      *
      * Fails the calling test rather than guessing when the two expectation files disagree, so a
      * corpus that grew a term-bearing case without a budget file cannot be counted as one whose
      * budget is checked.
      */
    protected def expectationOf(name: String): CaseExpectation = {
        val expected = readFile(s"$path/$name.uplc.expected")
        val budget = Try(readFile(s"$path/$name.budget.expected"))
        (expected, budget) match
            case ("evaluation failure", Success("evaluation failure")) =>
                CaseExpectation.EvaluationFailure
            case ("parse/decode error", Success("parse/decode error")) => CaseExpectation.ParseError
            case ("evaluation failure" | "parse/decode error", other) =>
                fail(s"$name expects '$expected' but its budget file says $other")
            case (_, Success(BudgetRegex(_, _))) => CaseExpectation.TermAndBudget
            case (_, other) =>
                fail(s"$name expects a term but its budget file is not a budget: $other")
    }
