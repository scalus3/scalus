package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.transform.{CommonSubexpressionElimination, V3Optimizer}
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

    private def check(name: String, compiled: PlutusV3[?]): Unit = {
        val optimized = new V3Optimizer(2, cceEnabled = true)(rawUplc(compiled))
        val cse = new CommonSubexpressionElimination()
        cse(optimized)
        assert(
          cse.logs.isEmpty,
          s"$name: CSE still found ${cse.logs.size} shareable subexpressions after the " +
              s"pipeline finished:\n${cse.logs.take(8).mkString("\n")}"
        )
    }

    test("no CSE opportunity survives the CCE phase: cape linear_vesting") {
        given Options = Options.releaseUntagged
        check(
          "linear_vesting",
          PlutusV3.compile(scalus.examples.cape.linearvesting.LinearVestingValidator.validate)
        )
    }

    test("no CSE opportunity survives the CCE phase: cape htlc") {
        given Options = Options.releaseUntagged
        check("htlc", PlutusV3.compile(scalus.examples.cape.htlc.HtlcValidator.validate))
    }

    test("no CSE opportunity survives the CCE phase: AuctionValidator") {
        given Options = Options.release
        check("AuctionValidator", PlutusV3.compile(AuctionValidator.validate))
    }
}
