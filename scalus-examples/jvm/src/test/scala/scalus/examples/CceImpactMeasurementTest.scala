package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.{PlutusV3, Program, Term}
import scalus.uplc.transform.V3Optimizer
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}
import scalus.examples.auction.AuctionValidator
import scalus.examples.paymentsplitter.{NaivePaymentSplitterValidator, OptimizedPaymentSplitterValidator}

/** Measures the impact of Common Context Extraction (CCE) and Common Subexpression Elimination
  * (CSE) on code size and execution budget for representative smart contracts.
  *
  * For each contract, we:
  *   1. Compile to raw UPLC (no CSE/CCE, but with standard eta-reduce/inline/builtins passes)
  *   1. Apply 4 optimizer configurations: baseline, CSE only, CCE only, CSE+CCE
  *   1. Report flat-encoded size (bytes) for each
  *
  * CCE's primary goal is reducing code **size** (CBOR bytes), not execution budget.
  */
class CceImpactMeasurementTest extends AnyFunSuite {

    // Mainnet execution unit prices
    private val exPrices = ExUnitPrices(
      priceMemory = NonNegativeInterval(0.0577, precision = 15),
      priceSteps = NonNegativeInterval(0.0000721, precision = 15)
    )

    case class OptimizerConfig(
        name: String,
        cseIterations: Int,
        cceEnabled: Boolean
    )

    private val configs = Seq(
      OptimizerConfig("Baseline (no CSE/CCE)", cseIterations = 0, cceEnabled = false),
      OptimizerConfig("CSE only (2 iters)", cseIterations = 2, cceEnabled = false),
      OptimizerConfig("CCE only", cseIterations = 0, cceEnabled = true),
      OptimizerConfig("CSE + CCE", cseIterations = 2, cceEnabled = true)
    )

    private def measureAndReport(contractName: String, rawUplc: Term): Unit = {
        info(s"\n$contractName:")
        info(
          f"${"Config"}%-25s ${"Flat(bytes)"}%12s ${"delta"}%8s ${"UPLC nodes"}%12s ${"delta"}%8s"
        )
        info("-" * 75)

        val results = configs.map { cfg =>
            val optimizer = new V3Optimizer(cfg.cseIterations, cfg.cceEnabled)
            val optimized = optimizer(rawUplc)
            val program = Program.plutusV3(optimized)
            val flatSize = program.flatEncoded.length
            val nodeCount = countNodes(optimized)
            (cfg, flatSize, nodeCount)
        }

        val (_, baselineFlat, baselineNodes) = results.head

        results.foreach { case (cfg, flatSize, nodeCount) =>
            val flatDelta = flatSize - baselineFlat
            val flatPct =
                if baselineFlat > 0 then f"${flatDelta.toDouble / baselineFlat * 100}%+.1f%%"
                else "N/A"
            val nodeDelta = nodeCount - baselineNodes
            val nodePct =
                if baselineNodes > 0 then f"${nodeDelta.toDouble / baselineNodes * 100}%+.1f%%"
                else "N/A"
            info(f"${cfg.name}%-25s $flatSize%12d $flatPct%8s $nodeCount%12d $nodePct%8s")
        }
    }

    private def countNodes(t: Term): Int = t match
        case Term.Var(_, _)          => 1
        case Term.Const(_, _)        => 1
        case Term.Builtin(_, _)      => 1
        case Term.Error(_)           => 1
        case Term.LamAbs(_, body, _) => 1 + countNodes(body)
        case Term.Apply(f, arg, _)   => 1 + countNodes(f) + countNodes(arg)
        case Term.Force(inner, _)    => 1 + countNodes(inner)
        case Term.Delay(inner, _)    => 1 + countNodes(inner)
        case Term.Constr(_, args, _) => 1 + args.map(countNodes).sum
        case Term.Case(s, cases, _)  => 1 + countNodes(s) + cases.map(countNodes).sum

    /** Compiles a contract to raw UPLC (with standard passes but no CSE/CCE). */
    private def compileRaw(compiled: PlutusV3[?]): Term = {
        val sir = compiled.sir
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = false,
          removeTraces = true,
          optimizeUplc = false
        )
        sir.toUplc(optimizeUplc = false)
    }

    /** Compiles a contract to baseline-optimized UPLC (standard passes, no CSE/CCE). */
    private def compileBaseline(compiled: PlutusV3[?]): Term = {
        val sir = compiled.sir
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          generateErrorTraces = false,
          removeTraces = true,
          optimizeUplc = true,
          cseIterations = 0,
          cceEnabled = false
        )
        sir.toUplc(optimizeUplc = false)
    }

    test("CCE impact: HelloCardano") {
        given Options = Options.release
        val compiled = PlutusV3.compile(HelloCardano.validate)
        val rawUplc = compileBaseline(compiled)
        measureAndReport("HelloCardano", rawUplc)
    }

    test("CCE impact: AuctionValidator") {
        given Options = Options.release
        val compiled = PlutusV3.compile(AuctionValidator.validate)
        val rawUplc = compileBaseline(compiled)
        measureAndReport("AuctionValidator", rawUplc)
    }

    test("CCE impact: NaivePaymentSplitter") {
        given Options = Options.release
        val compiled = PlutusV3.compile(NaivePaymentSplitterValidator.validate)
        val rawUplc = compileBaseline(compiled)
        measureAndReport("NaivePaymentSplitter", rawUplc)
    }

    test("CCE impact: OptimizedPaymentSplitter") {
        given Options = Options.release
        val compiled = PlutusV3.compile(OptimizedPaymentSplitterValidator.validate)
        val rawUplc = compileBaseline(compiled)
        measureAndReport("OptimizedPaymentSplitter", rawUplc)
    }
}
