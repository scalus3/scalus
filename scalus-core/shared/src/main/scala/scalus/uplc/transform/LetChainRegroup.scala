package scalus.uplc.transform

import scalus.uplc.{Term, UplcAnnotation}
import scalus.uplc.Term.*
import scalus.uplc.eval.{Log, Logger}
import scalus.uplc.transform.TermAnalysis.freeVars

/** Re-associates a chain of nested lets into multi-argument applications.
  *
  * A let is `[(lam x body) rhs]`, so a chain of them nests to the right. Bindings that do not
  * depend on each other can be applied as one argument list instead:
  *
  * {{{
  * // before: three nested lets — 3 Apply + 3 LamAbs = 6 machine steps
  * [(lam a [(lam b [(lam c body) ec]) eb]) ea]
  *
  * // after: one three-argument application — same step count on its own...
  * [[[(lam a (lam b (lam c body))) ea] eb] ec]
  *
  * // ...but [[CaseConstrApply]] then encodes it as Case + Constr + 3 LamAbs = 5 steps
  * (case (constr 0 [ea, eb, ec]) [(lam a (lam b (lam c body)))])
  * }}}
  *
  * A group of `N` bindings therefore saves `N - 2` machine steps per execution of the chain. This
  * pass only re-associates — the `case`/`constr` encoding is left to [[CaseConstrApply]], which
  * already fires on any application chain with 3+ arguments and must stay the single place that
  * decision is made (`Case`/`Constr` are illegal before Plutus V3).
  *
  * ==Why the threshold is five, not three==
  *
  * Steps are not the whole fee. The `case (constr 0 [...])` encoding is about '''one byte larger'''
  * than the applies it replaces (measured across the example corpus: 74 groups cost 65 bytes), and
  * on mainnet a script byte costs 15 lovelace of reference-script fee in every transaction that
  * uses the script, while a machine step costs 6.92 lovelace (`100 mem * 0.0577 + 16,000 cpu *
  * 0.0000721`).
  *
  * So a group pays for itself only when `(N - 2) * 6.92 > 15`, i.e. `N >= 5`. At `N = 3` the
  * grouping saves 6.92 lovelace of execution and costs 15 lovelace of script size — a net loss on
  * any validator executed once per transaction. [[MinRunSize]] is therefore 5.
  *
  * This is conservative for chains that run more than once per transaction (inside a loop, or a
  * script spending several inputs), where the step saving multiplies but the byte is paid once.
  *
  * ==Grouping rule==
  *
  * The chain is split into '''maximal contiguous runs'''. A binding joins the current run when
  *   - its right-hand side does not mention any binder already in the run, and
  *   - its name does not repeat a binder already in the run.
  *
  * Bindings are never reordered. Aiken's `split_body_lambda` moves bindings between groups to grow
  * them, which changes the order effects happen in; Scalus preserves source evaluation order.
  *
  * ==Why no purity guard is needed==
  *
  * The CEK machine evaluates `[f a]` function-first, then argument. So `[[[F ea] eb] ec]` evaluates
  * `F` (a lambda — effect-free), then `ea`, then the beta-reduction yields the next lambda
  * (effect-free), then `eb`, then `ec` — exactly the order of the nested form. Under the subsequent
  * `case (constr 0 [...])` encoding the fields are also evaluated left to right. A right-hand side
  * that errors, traces or diverges therefore does so at the same point relative to the others in
  * both forms, so this pass applies to effectful bindings (a `require(...)` lowers to a binding
  * with no uses) as readily as to values.
  *
  * Moving a right-hand side out of the scope of the earlier binders cannot capture or free a name
  * either: in the input it sits under those binders, so a free occurrence of one of them refers to
  * the let, which is exactly what the dependency test rejects.
  *
  * @param logger
  *   Logger for tracking regrouping operations
  * @param minRunSize
  *   smallest run worth grouping; see the threshold discussion above
  * @see
  *   [[CaseConstrApply]] for the pass that turns the resulting chains into `case`/`constr`
  */
class LetChainRegroup(
    logger: Logger = new Log(),
    minRunSize: Int = LetChainRegroup.MinRunSize
) extends Optimizer {

    def apply(term: Term): Term = go(term)

    def logs: Seq[String] = logger.getLogs.toSeq

    /** One `[(lam name body) rhs]` layer, keeping both nodes' annotations. */
    private case class Binding(
        name: String,
        rhs: Term,
        lamAnn: UplcAnnotation,
        applyAnn: UplcAnnotation
    )

    private def go(term: Term): Term = term match
        case Apply(LamAbs(_, _, _), _, _) =>
            val (chain, body) = collectChain(term)
            rebuild(runs(chain.map(b => b.copy(rhs = go(b.rhs)))), go(body))
        case Apply(f, arg, ann)                        => Apply(go(f), go(arg), ann)
        case LamAbs(name, body, ann)                   => LamAbs(name, go(body), ann)
        case Force(t, ann)                             => Force(go(t), ann)
        case Delay(t, ann)                             => Delay(go(t), ann)
        case Constr(tag, args, ann)                    => Constr(tag, args.map(go), ann)
        case Case(scrutinee, cases, ann)               => Case(go(scrutinee), cases.map(go), ann)
        case _: Var | _: Const | _: Builtin | _: Error => term

    /** Peels a maximal chain of nested lets, outermost first. */
    private def collectChain(term: Term): (List[Binding], Term) = term match
        case Apply(LamAbs(name, body, lamAnn), rhs, applyAnn) =>
            val (rest, inner) = collectChain(body)
            (Binding(name, rhs, lamAnn, applyAnn) :: rest, inner)
        case other => (Nil, other)

    /** Splits a chain into maximal contiguous runs of mutually independent bindings. */
    private def runs(chain: List[Binding]): List[List[Binding]] = {
        val out = List.newBuilder[List[Binding]]
        var current = List.newBuilder[Binding]
        var bound = Set.empty[String]
        var empty = true
        for b <- chain do
            if !empty && (bound.contains(b.name) || (b.rhs.freeVars & bound).nonEmpty) then
                out += current.result()
                current = List.newBuilder[Binding]
                bound = Set.empty
            current += b
            bound += b.name
            empty = false
        if !empty then out += current.result()
        out.result()
    }

    /** Rebuilds the chain, flattening profitable runs and leaving shorter ones nested. */
    private def rebuild(groups: List[List[Binding]], body: Term): Term =
        groups.foldRight(body) { (group, inner) =>
            if group.sizeIs < minRunSize then
                group.foldRight(inner) { (b, acc) =>
                    Apply(LamAbs(b.name, acc, b.lamAnn), b.rhs, b.applyAnn)
                }
            else
                logger.log(
                  s"LetChainRegroup: grouped ${group.size} bindings (saves ${group.size - 2} steps): ${group.map(_.name).mkString(", ")}"
                )
                val lambdas = group.foldRight(inner)((b, acc) => LamAbs(b.name, acc, b.lamAnn))
                group.foldLeft(lambdas)((f, b) => Apply(f, b.rhs, b.applyAnn))
        }
}

object LetChainRegroup:

    /** Smallest run of independent bindings worth grouping.
      *
      * A run of `N` saves `N - 2` machine steps (6.92 lovelace each on mainnet) and costs about one
      * script byte (15 lovelace per transaction), so grouping breaks even at `N = 5`.
      */
    val MinRunSize: Int = 5

    /** Applies let-chain regrouping to a term using default settings. */
    def apply(term: Term): Term = new LetChainRegroup().apply(term)
