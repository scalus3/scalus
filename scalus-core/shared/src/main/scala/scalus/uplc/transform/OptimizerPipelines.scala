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

class V3Optimizer(cseIterations: Int = 2, cceEnabled: Boolean = false) extends Optimizer {
    private val logger = Log()
    def apply(term: Term): Term = {
        logger.clear()

        val caseConstr = new CaseConstrApply(logger)
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

        // Phase 3: CCE followed by inliner to clean up single-use lambdas
        val withCce =
            if cceEnabled then withCse |> cce.apply |> inliner.apply
            else withCse

        // Phase 4: Final passes
        withCce |> caseConstr.apply // optimize multiple applys to more optimal case/constr nodes
    }
    def logs: Seq[String] = logger.getLogs.toVector
}
