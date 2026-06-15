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
    protected lazy val plutusVM: PlutusVM = {
        // The Plutus conformance corpus (>= 1.63 / van Rossem) is generated under semantics
        // variant E: PV11 features (case-on-builtins), UTF-8-byte-length text costing,
        // consByteString range checks, and Int64-bounded shift/rotate amounts. Build the VM from
        // Plutus's reference variant-E builtin cost model AND reference variant-E CEK machine costs
        // so produced budgets match the corpus exactly; makePlutusV3VM derives variant E from
        // vanRossemPV.
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
        val params = MachineParams(machineCosts, builtinCostModel)
        PlutusVM.makePlutusV3VM(params, MajorProtocolVersion.vanRossemPV)
    }

    protected given PlutusVM = plutusVM

    // Every UPLC evaluation case in the linked plutus-conformance corpus is discovered at compile
    // time (see Macros.conformanceEvaluationCasesImpl) and run by default; only cases listed in
    // `ignoredCases` are skipped. Adding a new corpus case requires no change here.
    private inline def discoveredCasesInline: List[String] =
        ${ scalus.utils.Macros.conformanceEvaluationCasesImpl }
    private val discoveredCases: List[String] = discoveredCasesInline

    /** Conformance cases we deliberately skip, each with the reason. Keys are corpus case paths
      * relative to `uplc/evaluation`, without the `.uplc` suffix.
      */
    protected def ignoredCases: Map[String, String] = Map(
      "builtin/constant/string/string-07/string-07" ->
          "Plutus 1.53 new string constant cases not yet supported by the Scalus UPLC parser",
      "builtin/constant/string/string-08/string-08" ->
          "Plutus 1.53 new string constant cases not yet supported by the Scalus UPLC parser",
      "builtin/semantics/bls12_381-cardano-crypto-tests/signature/large-dst/large-dst" ->
          "blst Java binding bug for DST longer than 255 bytes (supranational/blst#232)",
      "builtin/semantics/bls12_381_G1_hashToGroup/hash-dst-len-255/hash-dst-len-255" ->
          "blst Java binding bug for DST longer than 255 bytes (supranational/blst#232)",
      "builtin/semantics/bls12_381_G2_hashToGroup/hash-dst-len-255/hash-dst-len-255" ->
          "blst Java binding bug for DST longer than 255 bytes (supranational/blst#232)"
    )

    discoveredCases.foreach { name =>
        ignoredCases.get(name) match
            case Some(reason) => ignore(s"$name [ignored: $reason]")(())
            case None         => check(name)
    }

    test("ignoredCases has no stale entries (all present in the corpus)") {
        val stale = ignoredCases.keySet -- discoveredCases.toSet
        assert(stale.isEmpty, s"ignoredCases lists cases not in the corpus: ${stale.toList.sorted}")
    }

    private type EvalFailure = "evaluation failure"
    private type ParseError = "parse error"
    private type Error = EvalFailure | ParseError
    private def parseExpected(code: String): Either[Error, Term] = {
        code match
            case "evaluation failure" => Left("evaluation failure")
            case "parse error"        => Left("parse error")
            case _ =>
                Program.parseUplc(code) match
                    case Left(value) => fail(s"Unexpected parse error: $value")
                    case Right(program) =>
                        Right(DeBruijn.deBruijnTerm(program.term))

    }

    private def eval(code: String): Either[Error, Result] = {
        Program.parseUplc(code) match
            case Right(program) =>
                Right(program.term.evaluateDebug)
            case Left(_) =>
                Left("parse error")
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
                val budget = Try(readFile(s"$path/$name.uplc.budget.expected"))
                budget match
                    case Success(BudgetRegex(cpu, mem)) =>
                        Right(ExUnits(memory = mem.toLong, steps = cpu.toLong))
                    case Success("parse error")        => Left("parse error")
                    case Success("evaluation failure") => Left("evaluation failure")
                    case Failure(_)                    => Left("no file")
                    case _ => fail(s"Unexpected budget format:\n$budget")

            // println(eval(code).show)
            (eval(code), parseExpected(expected), expectedBudget) match
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
