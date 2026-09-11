package scalus.uplc.transform

import scalus.*
import scalus.uplc.Term
import scalus.uplc.eval.Log

class V1V2Optimizer extends Optimizer {
    private val logger = Log()
    def apply(term: Term): Term = {
        logger.clear()

        val builtinsExtractor = new ForcedBuiltinsExtractor(logger)
        val inliner = new Inliner(logger)
        val etaReduce = new EtaReduce(logger)
        val strictIf = new StrictIf(logger)

        // Run eta-reduce/inline passes 3 times to handle patterns created by inlining
        term |> etaReduce.apply |> inliner.apply
            |> etaReduce.apply |> inliner.apply
            |> etaReduce.apply |> inliner.apply
            |> strictIf.apply // convert eligible ifs to strict ifs
            |> builtinsExtractor.apply // extract forced builtins
    }
    def logs: Seq[String] = logger.getLogs.toVector
}

class V3Optimizer(
    cseIterations: Int = 2,
    cceEnabled: Boolean = false,
    letChainRegroup: Boolean = false
) extends Optimizer {

    /** Kept so the pre-`letChainRegroup` two-argument signature stays binary-compatible. */
    def this(cseIterations: Int, cceEnabled: Boolean) = this(cseIterations, cceEnabled, false)

    private val logger = Log()
    def apply(term: Term): Term = {
        logger.clear()

        val caseConstr = new CaseConstrApply(logger)
        val regrouper = new LetChainRegroup(logger)
        val builtinsExtractor = new ForcedBuiltinsExtractor(logger)
        val inliner = new Inliner(logger)
        val etaReduce = new EtaReduce(logger)
        val strictIf = new StrictIf(logger)
        val cse = new CommonSubexpressionElimination(logger)
        val cce = new CommonContextExtraction(logger)

        // Phase 1: Run eta-reduce/inline passes 3 times to handle patterns created by inlining
        val simplified = term |> etaReduce.apply |> inliner.apply
            |> etaReduce.apply |> inliner.apply
            |> etaReduce.apply |> inliner.apply
            |> strictIf.apply // convert eligible ifs to strict ifs
            |> builtinsExtractor.apply // extract forced builtins

        // Phase 2: CSE interleaved with inliner (configurable iterations)
        val withCse = (0 until cseIterations).foldLeft(simplified) { (t, _) =>
            t |> cse.apply |> inliner.apply
        }

        // Phase 3: CCE, then inliner to clean up single-use lambdas, then CSE again. CCE
        // rewrites every occurrence site into `[f leaf]`, so sites that shared a leaf become
        // identical subterms: a plain CSE opportunity that did not exist before CCE ran. On
        // AuctionValidator one such group has eleven occurrences.
        val withCce =
            if cceEnabled then withCse |> cce.apply |> inliner.apply |> cse.apply |> inliner.apply
            else withCse

        // Phase 4: regroup independent let chains into multi-argument applications, so the
        // case/constr encoding below can cover a whole run of bindings with one Case + Constr
        // instead of one Apply + LamAbs each. Must stay immediately before CaseConstrApply:
        // re-association on its own can cost extra steps when a bound expression fails.
        val regrouped =
            if letChainRegroup then withCce |> regrouper.apply
            else withCce

        // Phase 5: Final passes
        regrouped |> caseConstr.apply // optimize multiple applys to more optimal case/constr nodes
    }
    def logs: Seq[String] = logger.getLogs.toVector
}
