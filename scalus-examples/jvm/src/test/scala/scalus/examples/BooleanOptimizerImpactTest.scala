package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.compiler.sir.SIR.*
import scalus.compiler.sir.lowering.UplcPipeline
import scalus.compiler.sir.transform.BooleanOptimizer
import scalus.compiler.sir.{AnnotatedSIR, SIR}
import scalus.uplc.{PlutusV3, Program, Term}

/** Measures what [[BooleanOptimizer]] buys on the example contract suite.
  *
  * For each contract we take the plugin-produced SIR and lower it four ways - with and without the
  * boolean pass, each with and without the UPLC optimizer - and report the flat-encoded script size
  * and the UPLC node count. We also report which boolean rules fired, so a zero delta can be told
  * apart from "no rule matched".
  *
  * Size only; execution budget deltas come from the pinned `ExUnits` assertions in the per-example
  * tests.
  */
class BooleanOptimizerImpactTest extends AnyFunSuite {

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

    /** Lowers through the real pipeline with the boolean pass forced on or off, so both arms differ
      * in exactly one step. In production the pass follows `optimizeUplc`.
      */
    private def lower(sir: SIR, optimizeUplc: Boolean, optimizeBooleans: Boolean): Term = {
        val opts = Options.releaseUntagged.copy(optimizeUplc = optimizeUplc)
        UplcPipeline.run(
          sir,
          opts,
          opts.targetLanguage,
          UplcPipeline.defaultOptimizer(opts.targetLanguage, opts),
          optimizeBooleans = optimizeBooleans
        )
    }

    /** Counts `if !c then t else f` sites reachable by the ORIGINAL BooleanOptimizer traversal,
      * which descended into Apply/Select/Match/LamAbs and a Let's *body* only - never into a Let
      * binding's value, a Constr argument, a Cast, or an Error message. Linked SIR puts almost the
      * whole program in Let binding values, so this is the measure of how much the original pass
      * could have seen at all.
      */
    private def legacyReachableSwaps(sir: SIR): Int = {
        def go(s: SIR): Int = s match
            case Decl(_, term)   => go(term)
            case a: AnnotatedSIR => goE(a)
        def goE(s: AnnotatedSIR): Int = s match
            case Not(Not(a, _), _)                    => goE(a)
            case IfThenElse(Not(cond, _), t, f, _, _) => 1 + goE(cond) + goE(t) + goE(f)
            case Or(Not(a, _), Not(b, _), _)          => goE(a) + goE(b)
            case And(Not(a, _), Not(b, _), _)         => goE(a) + goE(b)
            case And(a, b, _)                         => goE(a) + goE(b)
            case Or(a, b, _)                          => goE(a) + goE(b)
            case Not(a, _)                            => goE(a)
            case IfThenElse(cond, t, f, _, _)         => goE(cond) + goE(t) + goE(f)
            case Apply(f, arg, _, _)                  => goE(f) + goE(arg)
            case Select(scrutinee, _, _, _)           => go(scrutinee)
            case Match(scrutinee, cases, _, _) => goE(scrutinee) + cases.map(c => go(c.body)).sum
            case Let(_, body, _, _)            => go(body) // bindings NOT visited
            case LamAbs(_, term, _, _)         => go(term)
            case _                             => 0 // Constr/Cast/Error NOT visited
        go(sir)
    }

    private case class Row(
        name: String,
        rawOff: Int,
        rawOn: Int,
        optOff: Int,
        optOn: Int,
        nodesOptOff: Int,
        nodesOptOn: Int,
        rules: Map[String, Int],
        legacySwaps: Int
    )

    private val rows = scala.collection.mutable.ListBuffer.empty[Row]

    private def measure(name: String, sir: SIR): Unit = {
        // RemoveTraces runs before the boolean pass in the pipeline, so count rule hits on the
        // same input the pipeline sees.
        val detraced = scalus.compiler.sir.RemoveTraces.transform(sir)
        val (_, stats) = BooleanOptimizer.optimizeCounting(detraced)

        def flat(t: Term): Int = Program.plutusV3(t).flatEncoded.length
        val rawOff = lower(sir, optimizeUplc = false, optimizeBooleans = false)
        val rawOn = lower(sir, optimizeUplc = false, optimizeBooleans = true)
        val optOff = lower(sir, optimizeUplc = true, optimizeBooleans = false)
        val optOn = lower(sir, optimizeUplc = true, optimizeBooleans = true)

        val row = Row(
          name,
          flat(rawOff),
          flat(rawOn),
          flat(optOff),
          flat(optOn),
          countNodes(optOff),
          countNodes(optOn),
          stats.hits,
          legacyReachableSwaps(detraced)
        )
        rows += row
        info(
          f"${row.name}%-32s raw ${row.rawOff}%6d -> ${row.rawOn}%6d (${row.rawOn - row.rawOff}%+4d)" +
              f"   opt ${row.optOff}%6d -> ${row.optOn}%6d (${row.optOn - row.optOff}%+4d)" +
              f"   nodes ${row.nodesOptOff}%6d -> ${row.nodesOptOn}%6d (${row.nodesOptOn - row.nodesOptOff}%+4d)" +
              s"   rules ${
                      if row.rules.isEmpty then "none"
                      else row.rules.toSeq.sorted.map((k, v) => s"$k=$v").mkString(",")
                  }" +
              s"   legacy-reachable-swaps ${row.legacySwaps}"
        )
    }

    private def contract(name: String)(sir: => SIR): Unit =
        test(s"BooleanOptimizer impact: $name") { measure(name, sir) }

    given Options = Options.releaseUntagged

    contract("HelloCardano")(PlutusV3.compile(HelloCardano.validate).sir)
    contract("MembershipToken")(PlutusV3.compile(MembershipTokenValidator.validate).sir)
    contract("Auction")(PlutusV3.compile(scalus.examples.auction.AuctionValidator.validate).sir)
    contract("Allowlist")(
      PlutusV3.compile(scalus.examples.bilinearAccumulator.AllowlistValidator.validate).sir
    )
    contract("Amm")(PlutusV3.compile(scalus.examples.amm.AmmValidator.validate).sir)
    contract("Betting")(PlutusV3.compile(scalus.examples.betting.BettingValidator.validate).sir)
    contract("Crowdfunding")(
      PlutusV3.compile(scalus.examples.crowdfunding.CrowdfundingValidator.validate).sir
    )
    contract("DecentralizedIdentity")(
      PlutusV3
          .compile(scalus.examples.decentralizedidentity.DecentralizedIdentityValidator.validate)
          .sir
    )
    contract("EditableNft")(
      PlutusV3.compile(scalus.examples.editablenft.EditableNftValidator.validate).sir
    )
    contract("Escrow")(PlutusV3.compile(scalus.examples.escrow.EscrowValidator.validate).sir)
    contract("Htlc")(PlutusV3.compile(scalus.examples.htlc.HtlcValidator.validate).sir)
    contract("LinearVesting")(
      PlutusV3.compile(scalus.examples.cape.linearvesting.LinearVestingValidator.validate).sir
    )
    contract("LinkedList")(
      PlutusV3.compile(scalus.examples.linkedlist.LinkedListValidator.validate).sir
    )
    contract("Lottery")(PlutusV3.compile(scalus.examples.lottery.LotteryValidator.validate).sir)
    contract("NaivePaymentSplitter")(
      PlutusV3.compile(scalus.examples.paymentsplitter.NaivePaymentSplitterValidator.validate).sir
    )
    contract("OptimizedPaymentSplitter")(
      PlutusV3
          .compile(scalus.examples.paymentsplitter.OptimizedPaymentSplitterValidator.validate)
          .sir
    )
    contract("Pricebet")(PlutusV3.compile(scalus.examples.pricebet.PricebetValidator.validate).sir)
    contract("SimpleTransfer")(
      PlutusV3.compile(scalus.examples.simpletransfer.SimpleTransferValidator.validate).sir
    )
    contract("TwoPartyEscrow")(
      PlutusV3.compile(scalus.examples.cape.twopartyescrow.TwoPartyEscrowValidator.validate).sir
    )
    contract("Vault")(PlutusV3.compile(scalus.examples.vault.VaultValidator.validate).sir)
    contract("Vesting")(PlutusV3.compile(scalus.examples.vesting.VestingValidator.validate).sir)
    contract("UpgradeableProxy")(
      PlutusV3.compile(scalus.examples.upgradeableproxy.ProxyValidator.validate).sir
    )

    test("the pass changes a real contract's compiled program") {
        val sir = scalus.examples.betting.BettingContract.compiled.sir
        val on = Program.plutusV3(lower(sir, optimizeUplc = true, optimizeBooleans = true))
        val off = Program.plutusV3(lower(sir, optimizeUplc = true, optimizeBooleans = false))
        info(s"Betting flat bytes: on=${on.flatEncoded.length} off=${off.flatEncoded.length}")
        assert(on.flatEncoded.length != off.flatEncoded.length)
    }

    test("`optimizeUplc = true` is what switches the pass on in the production pipeline") {
        // Guards the whole measurement: the forced-on arm above must equal what a plain
        // `Options.release` compile now produces, and the forced-off arm must equal a
        // `optimizeUplc = false` pipeline's boolean handling.
        val sir = scalus.examples.betting.BettingContract.compiled.sir
        val opts = Options.releaseUntagged
        val production = UplcPipeline.run(
          sir,
          opts,
          opts.targetLanguage,
          UplcPipeline.defaultOptimizer(opts.targetLanguage, opts)
        )
        val forcedOn = lower(sir, optimizeUplc = true, optimizeBooleans = true)
        assert(production == forcedOn, "optimizeUplc = true must enable the boolean pass")
    }

    test("the forced-off arm reproduces the pipeline as it was before the pass existed") {
        // For contracts where no rule fires, forcing the pass on must be a no-op. If the two arms
        // ever differ there, the measurement is comparing something other than this pass.
        val unaffected = Seq(
          "HelloCardano" -> PlutusV3.compile(HelloCardano.validate).sir,
          "Htlc" -> PlutusV3.compile(scalus.examples.htlc.HtlcValidator.validate).sir
        )
        unaffected.foreach { (name, sir) =>
            val (_, stats) =
                BooleanOptimizer.optimizeCounting(scalus.compiler.sir.RemoveTraces.transform(sir))
            assert(stats.total == 0, s"$name unexpectedly has rewrite sites")
            assert(
              lower(sir, optimizeUplc = true, optimizeBooleans = true) ==
                  lower(sir, optimizeUplc = true, optimizeBooleans = false),
              s"$name: the pass changed a contract it has no rewrite for"
            )
        }
    }

    test("report where the rewrites happen") {
        // Turns "budget unchanged" from an inference into an explained result: it says which
        // source lines the rewritten conditionals are on.
        val sirs = Seq(
          "Betting" -> scalus.examples.betting.BettingContract.compiled.sir,
          "Escrow" -> scalus.examples.escrow.EscrowContract.compiled.sir,
          "Auction" -> PlutusV3.compile(scalus.examples.auction.AuctionValidator.validate).sir
        )
        sirs.foreach { (name, sir) =>
            val (_, stats) =
                BooleanOptimizer.optimizeCounting(scalus.compiler.sir.RemoveTraces.transform(sir))
            info(s"$name rewrite sites:")
            stats.sites.foreach((rule, pos) => info(s"    $rule at $pos"))
        }
    }

    test("ZZ summary") {
        val all = rows.toList
        info("")
        info("=" * 120)
        info(
          f"${"contract"}%-32s ${"raw off"}%8s ${"raw on"}%8s ${"delta"}%7s ${"opt off"}%8s ${"opt on"}%8s ${"delta"}%7s ${"%"}%7s"
        )
        all.foreach { r =>
            val pct =
                if r.optOff > 0 then f"${(r.optOn - r.optOff).toDouble / r.optOff * 100}%+.2f%%"
                else "n/a"
            info(
              f"${r.name}%-32s ${r.rawOff}%8d ${r.rawOn}%8d ${r.rawOn - r.rawOff}%+7d ${r.optOff}%8d ${r.optOn}%8d ${r.optOn - r.optOff}%+7d $pct%7s"
            )
        }
        val totalRawOff = all.map(_.rawOff).sum
        val totalRawOn = all.map(_.rawOn).sum
        val totalOptOff = all.map(_.optOff).sum
        val totalOptOn = all.map(_.optOn).sum
        info("-" * 120)
        info(
          f"${"TOTAL"}%-32s $totalRawOff%8d $totalRawOn%8d ${totalRawOn - totalRawOff}%+7d $totalOptOff%8d $totalOptOn%8d ${totalOptOn - totalOptOff}%+7d"
        )
        info("")
        info(
          f"if-not-swap sites: full traversal ${all.map(_.rules.getOrElse("if-not-swap", 0)).sum}%d," +
              f" reachable by the ORIGINAL body-only traversal ${all.map(_.legacySwaps).sum}%d"
        )
        val allRules = all.flatMap(_.rules).groupMapReduce(_._1)(_._2)(_ + _)
        info("")
        info("rule hits across all contracts:")
        if allRules.isEmpty then info("  (none)")
        else allRules.toSeq.sortBy(-_._2).foreach((k, v) => info(f"  $k%-28s $v%6d"))
    }
}
