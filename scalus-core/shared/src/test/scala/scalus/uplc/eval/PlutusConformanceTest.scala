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

    // Since Plutus 1.67 most cases also have a `.flat` file: the same program, flat-encoded. They
    // are discovered the same way (see Macros.conformanceFlatCasesImpl).
    private inline def discoveredFlatCasesInline: List[String] =
        ${ scalus.utils.Macros.conformanceFlatCasesImpl }
    protected val discoveredFlatCases: List[String] = discoveredFlatCasesInline

    /** Whether this suite also runs the `.flat` cases (see [[checkFlat]]). */
    protected def runsFlatCases: Boolean = true

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

    if runsFlatCases then
        discoveredFlatCases.foreach { name =>
            ignoredCases.get(name) match
                case Some(reason) => ignore(s"$name.flat [ignored: $reason]")(())
                case None         => checkFlat(name)
        }

        test("every case has a .flat file, except the cases of the textual parser") {
            // Plutus's runner has the same rule (`dirsWithNoFlatFiles` in
            // PlutusConformance/Common.hs).
            def testsTextualParser(name: String) =
                name.startsWith("builtin/parser/") || name.startsWith("term/parser/")
            val expected = discoveredCases.filterNot(testsTextualParser)
            val missing = expected.diff(discoveredFlatCases)
            val unexpected = discoveredFlatCases.diff(expected)
            assert(missing.isEmpty, s"cases without a .flat file: $missing")
            assert(unexpected.isEmpty, s".flat files that are not expected: $unexpected")
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

    /** The VM a case runs on: [[dijkstraVM]] if it [[needsDijkstra]], else [[plutusVM]]. */
    protected def vmFor(name: String): PlutusVM =
        if needsDijkstra(name) then dijkstraVM else plutusVM

    protected def check(name: String): Unit =
        test(name) {
            val code = readFile(s"$path/$name.uplc")
            val expected = readFile(s"$path/$name.uplc.expected")
            assertOutcome(eval(code, vmFor(name)), parseExpected(expected), expectedBudget(name))
        }

    /** Decodes a flat-encoded program the way the Plutus conformance runner does (`getInputProg`
      * and `decodeFlatProg` in plutus-conformance `PlutusConformance/Common.hs`).
      *
      * An empty file is a decode error without decoding. Otherwise Plutus's `unflat` must consume
      * the whole input: the program, then the filler that pads it to a byte boundary, and nothing
      * after it. A decoded program with a free variable is a decode error too, because the runner
      * converts it to named variables, which fails on an index out of scope (also inside `constr`
      * and `case`, unlike the ledger's scope check).
      */
    protected def decodeFlat(bytes: Array[Byte]): Option[DeBruijnedProgram] =
        if bytes.isEmpty then None
        else
            Try {
                val program = ProgramFlatCodec.decodeFlat(bytes)
                require(isClosed(program.term), "a variable is out of scope")
                program
            }.toOption

    private def isClosed(term: Term): Boolean = {
        import Term.*
        def go(term: Term, level: Int): Boolean = term match
            case Var(name, _)       => name.index > 0 && name.index <= level
            case LamAbs(_, body, _) => go(body, level + 1)
            case Apply(f, arg, _)   => go(f, level) && go(arg, level)
            case Force(t, _)        => go(t, level)
            case Delay(t, _)        => go(t, level)
            case Constr(_, args, _) => args.forall(go(_, level))
            case Case(arg, cases, _) =>
                go(arg, level) && cases.forall(go(_, level))
            case _: Const | _: Builtin | _: Term.Error => true
        go(term, 0)
    }

    /** Runs the `.flat` input of a corpus case.
      *
      * Like Plutus's runner in its `--format=flat` mode, the result is compared with the
      * `.flat.expected` file: a flat-encoded result program, or the text of a failure marker. The
      * budget file is shared with the textual case. On top of that, the decoded program must be the
      * program the `.uplc` file parses to, so a decoder bug that changes the program but not its
      * result, or that the expectation decodes the same wrong way, still fails here.
      */
    protected def checkFlat(name: String): Unit =
        test(s"$name.flat") {
            val decoded = decodeFlat(platform.readFile(s"$path/$name.flat"))
            (decoded, parseScoped(readFile(s"$path/$name.uplc"))) match
                case (Some(fromFlat), Some(fromText)) =>
                    assert(
                      fromFlat.version == fromText.version,
                      s"the .flat program has version ${fromFlat.version}, " +
                          s"the .uplc one ${fromText.version}"
                    )
                    assert(
                      fromFlat.term α_== DeBruijn.deBruijnTerm(fromText.term),
                      s"the .flat program decodes to\n${fromFlat.show}\n" +
                          s"but the .uplc program is\n${fromText.show}"
                    )
                case (None, None) =>
                case (fromFlat, fromText) =>
                    fail(
                      s"decoded .flat: ${fromFlat.map(_.show)}, parsed .uplc: ${fromText.map(_.show)}"
                    )
            val actual: Either[Error, Result] = decoded match
                case Some(program) => Right(program.toProgram.term.evaluateDebug(using vmFor(name)))
                case None          => Left("parse/decode error")
            assertOutcome(actual, flatExpected(name), expectedBudget(name))
        }

    /** Reads a `.flat.expected` file as Plutus's `decodeFlatExpected` does: the text of a failure
      * marker, or else a flat-encoded program.
      */
    private def flatExpected(name: String): Either[Error, Term] = {
        val bytes = platform.readFile(s"$path/$name.flat.expected")
        new String(bytes, "UTF-8") match
            case "evaluation failure" => Left("evaluation failure")
            case "parse/decode error" => Left("parse/decode error")
            case _ =>
                decodeFlat(bytes) match
                    case Some(program) => Right(program.term)
                    case None          => fail(s"$name.flat.expected does not decode")
    }

    private def expectedBudget(name: String): Either[String, ExUnits] = {
        val budget = Try(readFile(s"$path/$name.budget.expected"))
        budget match
            case Success(BudgetRegex(cpu, mem)) =>
                Right(ExUnits(memory = mem.toLong, steps = cpu.toLong))
            case Success("parse/decode error") => Left("parse/decode error")
            case Success("evaluation failure") => Left("evaluation failure")
            case Failure(_)                    => Left("no file")
            case _                             => fail(s"Unexpected budget format:\n$budget")
    }

    private def assertOutcome(
        actual: Either[Error, Result],
        expected: Either[Error, Term],
        expectedBudget: Either[String, ExUnits]
    ): Unit =
        (actual, expected, expectedBudget) match
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

    /** Which of [[check]]'s three outcomes the corpus asks of one case.
      *
      * The JS guard of the published conformance claims and the JIT suite classify cases through
      * the same expectation files [[check]] reads, rather than through a second copy of the rules
      * that could drift away from it.
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
