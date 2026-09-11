package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.transform.{CommonContextExtraction, CommonSubexpressionElimination, Inliner}
import scalus.uplc.{PlutusV3, Term}
import scalus.examples.auction.AuctionValidator

/** Every extraction CCE applies must still pay at the occurrence count it is applied with.
  *
  * Extractions are applied in sequence, largest first, and each rewrites the term, so a large
  * template can swallow occurrences of a smaller one and drop it below the count it was scored at.
  * Real validators produce chains of nested accessors where this happens; synthetic terms are
  * surprisingly hard to make trigger it, so this runs against compiled contracts.
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
        cce(afterCse(rawUplc(compiled)))
        val savings = cce.logs.flatMap { l =>
            raw"saved=(-?[0-9.]+) bits".r.findFirstMatchIn(l).map(_.group(1).toDouble)
        }
        val losers = savings.filter(_ <= 0)
        assert(
          losers.isEmpty,
          s"$name: ${losers.size} of ${savings.size} extractions do not pay ($losers):\n" +
              cce.logs.mkString("\n")
        )
    }

    test("CCE applies no unprofitable extraction: cape linear_vesting") {
        given Options = Options.releaseUntagged
        check(
          "linear_vesting",
          PlutusV3.compile(scalus.examples.cape.linearvesting.LinearVestingValidator.validate)
        )
    }

    test("CCE applies no unprofitable extraction: cape htlc") {
        given Options = Options.releaseUntagged
        check("htlc", PlutusV3.compile(scalus.examples.cape.htlc.HtlcValidator.validate))
    }

    test("CCE applies no unprofitable extraction: AuctionValidator") {
        given Options = Options.release
        check("AuctionValidator", PlutusV3.compile(AuctionValidator.validate))
    }
}
