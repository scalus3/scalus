package scalus.uplc.transform

import scalus.cardano.ledger.Word64
import scalus.uplc.Term
import scalus.uplc.Term.{Apply, Case, Constr}
import scalus.uplc.eval.{Log, Logger}
import scalus.uplc.internal.TermTraversal.mapChildren

/** Replace nested Apply with Case/Constr
  *
  * For example, replace `(apply (apply (apply f a) b) c)` with `(case (constr 0 [a, b, c]) f)`.
  * This is more memory/cpu efficient than nested Apply at least in Plutus V3 Plomin HF, protocol
  * version 10.
  *
  * With current machine costs, Apply costs 100 memory and 16000 cpu, same for Case/Constr. Hence
  * (case (constr 0 [a, b, c]) f) costs 200 memory and 32000 cpu, while `(apply (apply (apply f a)
  * b) c)` costs 300 memory and 48000 cpu.
  */
class CaseConstrApply(logger: Logger = new Log()) extends Optimizer {
    def apply(term: Term): Term = extractPass(term)

    def logs: Seq[String] = logger.getLogs.toSeq

    /** Replaces each application spine of more than two arguments, after rewriting its arguments,
      * then its function. A shorter spine stays as it is, with its annotations, and its children
      * are walked.
      *
      * Own recursion, not transformDown: transformDown would walk the new `Case` and `Constr` and
      * build both a second time once their children change (JMH: 5 to 8% slower).
      */
    private def extractPass(term: Term): Term = {
        def go(t: Term): Term = t match
            // Three nested Applys: a spine of three or more arguments, `[[[f a] b] c]`.
            case app @ Apply(Apply(Apply(_, _, _), _, _), _, _) =>
                val (f, args) = app.applyToList
                logger.log(s"Replacing ${args.size} Apply with Case/Constr")
                Case(Constr(Word64.Zero, args.map(go)), go(f) :: Nil)
            case _ => t.mapChildren(go)
        go(term)
    }
}

object CaseConstrApply:
    def apply(term: Term): Term = new CaseConstrApply().apply(term)
