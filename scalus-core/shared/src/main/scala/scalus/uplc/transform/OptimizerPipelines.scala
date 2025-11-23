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

        term |> etaReduce.apply // first eta reduce to expose more inlining opportunities
            |> inliner.apply // inline functions and eliminate dead code
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

        term |> etaReduce.apply // first eta reduce to expose more inlining opportunities
            |> inliner.apply // inline functions and eliminate dead code
            |> strictIf.apply // convert eligible ifs to strict ifs
            |> builtinsExtractor.apply // extract forced builtins
            |> caseConstr.apply // optimize multiple applys to more optimal case/constr nodes
    }
    def logs: Seq[String] = logger.getLogs.toVector
}
