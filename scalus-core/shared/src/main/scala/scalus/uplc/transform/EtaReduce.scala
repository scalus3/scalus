package scalus.uplc.transform

import scalus.*
import scalus.cardano.ledger.Word64
import scalus.uplc.Term.*
import scalus.uplc.transform.TermAnalysis.{freeVars, isPure}
import scalus.uplc.Term
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
  * On top of the syntactic purity check, the pass tracks a value-arity environment for
  * `[(lam x body) rhs]` let-bindings: when `rhs` provably evaluates to a lambda of arity `n`
  * (counting the self-application fixpoint encoding `[(lam f [f f]) (lam f (lam a ... body))]`
  * produced for recursive functions), a partial application `x a1 ... ak` with `k < n` pure
  * arguments is itself a pure expression, so multi-argument eta-wrappers like `λa. λb. f a b`
  * reduce to `f`. This removes the wrapper the compiler emits around a multi-parameter recursive
  * entry point (`[(lam f (lam a (lam b [f a b]))) fix]` becomes `[(lam f f) fix]`, which the
  * Inliner then collapses to `fix`). The same wrapper in the case-constr application encoding —
  * `(lam a (lam b (case (constr 0 a b) f)))` as produced by [[CaseConstrApply]] — reduces too, see
  * `caseConstrEtaRedex`.
  *
  * '''Precondition: named terms only.''' Every analysis here — the `arities` environment, the
  * capture checks via [[TermAnalysis.freeVars]], and the field matching in `caseConstrEtaRedex` —
  * identifies variables by NAME and ignores `NamedDeBruijn.index`, so scope is whatever the binder
  * nesting says it is. That is the representation the compiler pipeline produces: `UplcPipeline`
  * hands optimizers a named term and de Bruijn indices are only assigned later, at
  * `Program.deBruijnedProgram`. Do not run this pass on an already-de-Bruijned term (one whose
  * `Var` indices are meaningful — the CEK requires that form, see `lookupVarName`, which rejects
  * index 0): there, two distinct binders may share a name, and name-based scoping would conflate
  * them.
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
    private def etaReduce(term: Term): Term = etaReduce(term, Map.empty)

    /** Performs eta-reduction on a term.
      *
      * @param arities
      *   value arity of let-bound variables in scope, see [[valueArity]]
      */
    private def etaReduce(term: Term, arities: Map[String, Int]): Term = term match
        case LamAbs(name1, Term.Apply(f, Term.Var(name2, _), _), ann)
            if name1 == name2.name && !f.freeVars.contains(name1) &&
                (f.isPure || valueArity(f, arities - name1) >= 1) =>
            logger.log(s"Eta-reducing term: ${f.show}")
            etaReduce(f, arities - name1)
        case lam @ LamAbs(name, body, ann) =>
            caseConstrEtaRedex(lam, arities) match
                case Some(f) =>
                    logger.log(s"Eta-reducing case-constr wrapper to: ${f.show}")
                    etaReduce(f, arities)
                case None =>
                    val body1 = etaReduce(body, arities - name)
                    if body ~!=~ body1 then etaReduce(LamAbs(name, body1, ann), arities) else term
        case Apply(f, arg, ann) =>
            val arg1 = etaReduce(arg, arities)
            val f1 = f match
                case LamAbs(x, body, lann) =>
                    // A let-binding: while reducing the body, `x` is known to be bound to the
                    // value of `arg` (the body only evaluates after `arg` succeeded, so
                    // recording its value arity is sound even for an impure `arg`).
                    val body1 = etaReduce(body, arities.updated(x, valueArity(arg1, arities)))
                    // The redex lambda may itself be an eta-redex; check its (already fully
                    // reduced) body once - re-walking it here would be exponential on nested
                    // let-chains.
                    LamAbs(x, body1, lann) match
                        case LamAbs(n1, Term.Apply(g, Term.Var(n2, _), _), _)
                            if n1 == n2.name && !g.freeVars.contains(n1) &&
                                (g.isPure || valueArity(g, arities) >= 1) =>
                            logger.log(s"Eta-reducing term: ${g.show}")
                            g
                        case reduced =>
                            caseConstrEtaRedex(reduced, arities) match
                                case Some(g) =>
                                    logger.log(s"Eta-reducing case-constr wrapper to: ${g.show}")
                                    g
                                case None => reduced
                case other => etaReduce(other, arities)
            Apply(f1, arg1, ann)
        case Force(term, ann) => Force(etaReduce(term, arities), ann)
        case Delay(term, ann) => Delay(etaReduce(term, arities), ann)
        // Constr/Case carry subterms like every other node and must be traversed. PV11 lowering
        // emits Case throughout, so a wrapper nested under one is the common shape, not a corner
        // case. Neither node binds anything, so `arities` carries through unchanged.
        case Constr(tag, args, ann) => Constr(tag, args.map(etaReduce(_, arities)), ann)
        case Case(scrut, cases, ann) =>
            Case(etaReduce(scrut, arities), cases.map(etaReduce(_, arities)), ann)
        case _ => term

    /** Matches the case-constr encoding of a multi-argument eta-wrapper:
      *
      * {{{(lam a1 .. (lam an (case (constr 0 a1 .. an) f)))   =>   f}}}
      *
      * [[CaseConstrApply]] rewrites `[f a1 .. an]` (n >= 3) into `(case (constr 0 a1 .. an) f)`, so
      * an eta-wrapper over a multi-argument function can appear in this encoding as well as as a
      * plain apply chain (custom `uplcOptimizers` pipelines and hand-written UPLC; the default
      * pipeline runs [[CaseConstrApply]] after [[EtaReduce]], so it only ever sees plain applies).
      *
      * Soundness conditions, mirroring the plain rule:
      *   - exactly one case branch and constructor tag 0 (any other shape is not an application
      *     encoding: it errors or selects differently at runtime)
      *   - the constr fields are exactly the bound variables in binder order — all of them and
      *     nothing else — and the binder names are pairwise distinct (a duplicated name would make
      *     two fields refer to the same innermost binder)
      *   - `f` does not capture any of the bound variables
      *   - `f` provably evaluates to a lambda accepting at least `n` arguments:
      *     `valueArity(f) >= n`. For `n == 1` a syntactically pure `f` is also accepted, exactly
      *     like the plain `λx. f x` rule (`(case (constr 0 x) f)` and `[f x]` are the same
      *     computation). For `n >= 2` purity alone is NOT enough: the wrapper partially applied to
      *     `k < n` arguments is still a value, while `f` partially applied runs `f`'s value — for
      *     example `(lam a (lam b (case (constr 0 a b) (con integer 5))))` applied to one argument
      *     is a lambda value, but `(con integer 5)` applied to one argument is an error. The plain
      *     rule cannot over-reduce this way because it peels one lambda at a time and `[f a]` is
      *     never syntactically pure; this rule drops `n` binders at once, so it needs the full
      *     arity guarantee.
      *
      * The returned `f` contains no occurrence of the dropped binder names (free occurrences are
      * rejected, bound ones belong to inner binders that shadow them), so the caller's arity
      * environment remains valid for it unchanged.
      */
    private def caseConstrEtaRedex(term: Term, arities: Map[String, Int]): Option[Term] = {
        @annotation.tailrec
        def peel(t: Term, revNames: List[String]): (List[String], Term) = t match
            case LamAbs(n, body, _) => peel(body, n :: revNames)
            case other              => (revNames, other)
        val (revNames, inner) = peel(term, Nil)
        inner match
            case Case(Constr(tag, fields, _), f :: Nil, _) if tag == Word64.Zero =>
                val names = revNames.reverse
                val n = names.size
                val fieldsMatch =
                    n > 0 && fields.sizeCompare(n) == 0 &&
                        fields.lazyZip(names).forall {
                            case (Var(nd, _), name) => nd.name == name
                            case _                  => false
                        }
                def distinctNames = names.toSet.size == n
                def fDoesNotCapture = { val fv = f.freeVars; !names.exists(fv.contains) }
                def aritySufficient =
                    valueArity(f, arities -- names) >= n || (n == 1 && f.isPure)
                if fieldsMatch && distinctNames && fDoesNotCapture && aritySufficient
                then Some(f)
                else None
            case _ => None
    }

    /** Lower bound on the ''value arity'' of a term.
      *
      * `valueArity(t) == n` with `n >= 1` guarantees that `t` evaluates without error or
      * divergence, and that applying its value to up to `n - 1` pure arguments is again a pure
      * expression (each such partial application immediately yields a lambda value without running
      * any computation). `0` means "no information".
      *
      * This makes `x a1 ... ak` provably pure when `x` is a let-bound variable with known value
      * arity `n > k` and the arguments are pure — the case [[TermAnalysis.isPure]] cannot see
      * because purity of an `Apply` of a variable depends on what the variable is bound to.
      */
    private def valueArity(term: Term, arities: Map[String, Int]): Int = term match
        // let-binding: the value arity of the body with x bound to the value of rhs.
        // Requires rhs to be pure: the let itself must evaluate without effects.
        case Apply(LamAbs(x, body, _), rhs, _) if rhs.isPure =>
            valueArity(body, arities.updated(x, valueArity(rhs, arities)))
        // applying one pure argument to a term of arity n >= 2 yields a value of arity n - 1
        case Apply(f, arg, _) if arg.isPure =>
            val n = valueArity(f, arities)
            if n >= 2 then n - 1 else 0
        // a lambda is a value; applying one argument evaluates its body, which is safe for
        // valueArity(body) further arguments (0 when the body is an unknown computation)
        case LamAbs(x, body, _) => 1 + valueArity(body, arities - x)
        case Var(name, _)       => arities.getOrElse(name.name, 0)
        case _                  => 0

object EtaReduce:
    def apply(term: Term): Term = new EtaReduce().apply(term)
    def apply(term: Term, logger: String => Unit): Term = {
        val log = new Log()
        val result = new EtaReduce(log).apply(term)
        log.getLogs.foreach(logger)
        result
    }
