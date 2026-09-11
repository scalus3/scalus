package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.transform.{CommonContextExtraction, CommonSubexpressionElimination, Inliner}
import scalus.uplc.{PlutusV3, Term}
import scalus.examples.auction.AuctionValidator

/** CCE must not grow the encoded script on these compiled contracts.
  *
  * This measures the actual output independently of the profitability formula. A whole-pass
  * decrease does not prove each individual extraction pays; isolated core tests cover rejection of
  * growing extractions.
  */
class CceProfitabilityTest extends AnyFunSuite {

    private def rawUplc(compiled: PlutusV3[?]): Term = {
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = false,
          removeTraces = true,
          optimizeUplc = true,
          cseIterations = 0,
          cceEnabled = false
        )
        compiled.sir.toUplc(optimizeUplc = false)
    }

    /** Mirrors V3Optimizer phases 1-2 so CCE sees the term it sees in the real pipeline. */
    private def afterCse(term: Term): Term = {
        val cse = new CommonSubexpressionElimination()
        val inliner = new Inliner()
        (0 until 2).foldLeft(term)((t, _) => inliner(cse(t)))
    }

    private def check(name: String, compiled: PlutusV3[?]): Unit = {
        val cce = new CommonContextExtraction()
        val before = afterCse(rawUplc(compiled))
        val after = cce(before)
        val beforeBytes = before.plutusV3.cborByteString.size
        val afterBytes = after.plutusV3.cborByteString.size
        assert(
          afterBytes <= beforeBytes,
          s"$name: CCE grew the script from $beforeBytes to $afterBytes bytes"
        )
    }

    test("CCE does not grow the encoded script: cape linear_vesting") {
        given Options = Options.releaseUntagged
        check(
          "linear_vesting",
          PlutusV3.compile(scalus.examples.cape.linearvesting.LinearVestingValidator.validate)
        )
    }

    test("CCE does not grow the encoded script: cape htlc") {
        given Options = Options.releaseUntagged
        check("htlc", PlutusV3.compile(scalus.examples.cape.htlc.HtlcValidator.validate))
    }

    test("CCE does not grow the encoded script: AuctionValidator") {
        given Options = Options.release
        check("AuctionValidator", PlutusV3.compile(AuctionValidator.validate))
    }
}
