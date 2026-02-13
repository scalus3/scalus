package scalus.uplc.transform

import scalus.*
import scalus.uplc.Term.*
import scalus.uplc.transform.TermAnalysis.isPure
import scalus.uplc.{NamedDeBruijn, Term}
import scalus.uplc.eval.{Log, Logger}

/** Performs eta-reduction on a term.
  *
  * Eta-reduction is the process of removing redundant lambda abstractions from a term. For example,
  * the term `λx. f x` can be eta-reduced to `f` but only if
  *   - `x` is not free in `f`
  *   - `f` is a pure expression
  *
  * Purity checking is handled by [[TermAnalysis.isPure]]. A term is pure if it does not contain any
  * side effects, such as `Error`, `Force` of non-delayed terms, or saturated builtin applications.
  * See [[TermAnalysis.isPure]] for comprehensive documentation on purity semantics.
  *
  * @see
  *   [[https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B7-reduction Eta reduction]]
  * @see
  *   [[TermAnalysis.isPure]] for purity semantics
  */
class EtaReduce(logger: Logger = new Log()) extends Optimizer:
    /** Applies eta-reduction optimization to a term.
      *
      * @param term
      *   The UPLC term to optimize
      * @return
      *   The optimized term
      */
    def apply(term: Term): Term = etaReduce(term)

    /** Returns the accumulated logs from optimization operations.
      *
      * @return
      *   Sequence of log messages
      */
    def logs: Seq[String] = logger.getLogs.toSeq

    /** Performs eta-reduction on a term. */
    private def etaReduce(term: Term): Term = term match
        case LamAbs(name1, Term.Apply(f, Term.Var(name2, _), _), pos)
            if name1 == name2.name && !freeNames(f).contains(name1) && f.isPure =>
            logger.log(s"Eta-reducing term: ${f.show}")
            etaReduce(f)
        case LamAbs(name, body, pos) =>
            val body1 = etaReduce(body)
            if body ~!=~ body1 then etaReduce(LamAbs(name, body1, pos)) else term
        case Apply(f, arg, pos) => Apply(etaReduce(f), etaReduce(arg), pos)
        case Force(term, pos)   => Force(etaReduce(term), pos)
        case Delay(term, pos)   => Delay(etaReduce(term), pos)
        case _                  => term

    /** Returns the set of free names in a term */
    private def freeNames(term: Term): Set[String] = term match
        case Var(NamedDeBruijn(name, _), _) => Set(name)
        case LamAbs(name, body, _)          => freeNames(body) - name
        case Apply(f, arg, _)               => freeNames(f) ++ freeNames(arg)
        case Force(term, _)                 => freeNames(term)
        case Delay(term, _)                 => freeNames(term)
        case _                              => Set.empty

object EtaReduce:
    def apply(term: Term): Term = new EtaReduce().apply(term)
    def apply(term: Term, logger: String => Unit): Term = {
        val log = new Log()
        val result = new EtaReduce(log).apply(term)
        log.getLogs.foreach(logger)
        result
    }
