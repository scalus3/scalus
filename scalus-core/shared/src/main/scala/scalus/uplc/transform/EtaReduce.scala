package scalus.uplc.transform

import scalus.*
import scalus.uplc.Term.*
import scalus.uplc.transform.TermAnalysis.isPure
import scalus.uplc.{NamedDeBruijn, Term}

/** Performs eta-reduction on a term.
  *
  * @see
  *   [[etaReduce]]
  */
object EtaReduce:
    /** Eta-reduces a term
      * @see
      *   [[etaReduce]]
      */
    def apply(term: Term): Term = etaReduce(term, _ => ())
    def apply(term: Term, logger: String => Unit): Term = etaReduce(term, logger)

    /** Performs eta-reduction on a term.
      *
      * Eta-reduction is the process of removing redundant lambda abstractions from a term. For
      * example, the term `λx. f x` can be eta-reduced to `f` but only if
      *   - `x` is not free in `f`
      *   - `f` is a pure expression
      *
      * Purity checking is handled by [[TermAnalysis.isPure]]. A term is pure if it does not contain
      * any side effects, such as `Error`, `Force` of non-delayed terms, or saturated builtin
      * applications. See [[TermAnalysis.isPure]] for comprehensive documentation on purity
      * semantics.
      *
      * @see
      *   [[https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B7-reduction Eta reduction]]
      * @see
      *   [[TermAnalysis.isPure]] for purity semantics
      */
    def etaReduce(term: Term): Term = etaReduce(term, _ => ())
    def etaReduce(term: Term, logger: String => Unit): Term = term match
        case LamAbs(name1, Term.Apply(f, Term.Var(name2)))
            if name1 == name2.name && !freeNames(f).contains(name1) && f.isPure =>
            logger(s"Eta-reducing term: ${f.show}")
            etaReduce(f, logger)
        case LamAbs(name, body) =>
            val body1 = etaReduce(body, logger)
            if body != body1 then etaReduce(LamAbs(name, body1), logger) else term
        case Apply(f, arg) => Apply(etaReduce(f, logger), etaReduce(arg, logger))
        case Force(term)   => Force(etaReduce(term, logger))
        case Delay(term)   => Delay(etaReduce(term, logger))
        case _             => term

    /** Returns the set of free names in a term */
    def freeNames(term: Term): Set[String] = term match
        case Var(NamedDeBruijn(name, _)) => Set(name)
        case LamAbs(name, body)          => freeNames(body) - name
        case Apply(f, arg)               => freeNames(f) ++ freeNames(arg)
        case Force(term)                 => freeNames(term)
        case Delay(term)                 => freeNames(term)
        case _                           => Set.empty
