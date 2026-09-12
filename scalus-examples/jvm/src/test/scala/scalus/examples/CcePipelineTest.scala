package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.transform.{CommonContextExtraction, CommonSubexpressionElimination, Inliner, V3Optimizer}
import scalus.uplc.{PlutusV3, Term}
import scalus.examples.auction.AuctionValidator

/** CCE rewrites every occurrence site into `[f leaf]`. Sites that shared a leaf become identical
  * subterms, which is a plain CSE opportunity that did not exist before CCE ran. The pipeline must
  * therefore run CSE again after CCE, or those duplicates ship.
  */
class CcePipelineTest extends AnyFunSuite {

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

    private def checkPipeline(name: String, raw: Term): Unit = {
        val optimized = new V3Optimizer(2, cceEnabled = true)(raw)
        val cse = new CommonSubexpressionElimination()
        cse(optimized)
        assert(
          cse.logs.isEmpty,
          s"$name: CSE still found ${cse.logs.size} shareable subexpressions after the " +
              s"pipeline finished:\n${cse.logs.take(8).mkString("\n")}"
        )
    }

    // Prepare the same two CSE/Inliner rounds as the original profitability fixtures.
    // This deliberately stops before CCE and does not run the full V3 pipeline's other phases.
    private def checkProfitability(name: String, raw: Term): Unit = {
        val cse = new CommonSubexpressionElimination()
        val inliner = new Inliner()
        val before = (0 until 2).foldLeft(raw)((t, _) => inliner(cse(t)))
        val after = new CommonContextExtraction()(before)
        val beforeBytes = before.plutusV3.cborByteString.size
        val afterBytes = after.plutusV3.cborByteString.size
        // Actual encoded sizes are independent of the pricing formula. Whole-pass savings
        // do not establish profitability of each extraction; isolated core tests cover that.
        assert(
          afterBytes <= beforeBytes,
          s"$name: CCE grew the script from $beforeBytes to $afterBytes bytes"
        )
    }

    private lazy val linearVesting: Term = {
        given Options = Options.releaseUntagged
        rawUplc(
          PlutusV3.compile(scalus.examples.cape.linearvesting.LinearVestingValidator.validate)
        )
    }

    private lazy val htlc: Term = {
        given Options = Options.releaseUntagged
        rawUplc(PlutusV3.compile(scalus.examples.cape.htlc.HtlcValidator.validate))
    }

    private lazy val auction: Term = {
        given Options = Options.release
        rawUplc(PlutusV3.compile(AuctionValidator.validate))
    }

    for (name, fixture) <- List(
          "cape linear_vesting" -> (() => linearVesting),
          "cape htlc" -> (() => htlc),
          "AuctionValidator" -> (() => auction)
        )
    do
        test(s"no CSE opportunity survives the CCE phase: $name") {
            checkPipeline(name, fixture())
        }
        test(s"CCE does not grow the encoded script: $name") {
            checkProfitability(name, fixture())
        }
}
