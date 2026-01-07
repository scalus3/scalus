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

class V3Optimizer extends Optimizer {
    private val logger = Log()
    def apply(term: Term): Term = {
        logger.clear()

        val caseConstr = new CaseConstrApply(logger)
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
            |> caseConstr.apply // optimize multiple applys to more optimal case/constr nodes
    }
    def logs: Seq[String] = logger.getLogs.toVector
}
