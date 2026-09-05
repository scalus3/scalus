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
  * Steps are not the whole fee: the encoding also changes the script's size, and the two pull in
  * opposite directions below `N = 7`.
  *
  * In the flat encoding an application chain is pure tags, `4N` bits. The case-constr form pays
  * fixed framing first — `Case` tag 4, `Constr` tag 4, constructor index 8, field-list framing
  * `N+1` (one bit per field plus a terminator), branch-list framing 2 — so `19 + N` bits. The
  * difference is therefore
  *
  * {{{
  * Δbits = (19 + N) - 4N = 19 - 3N
  * }}}
  *
  * which is '''larger''' below `N = 7` and smaller from `N = 7` up (verified against the encoder
  * for N = 1..12). On mainnet a script byte costs 15 lovelace of reference-script fee in every
  * transaction using the script, while a machine step costs 6.92 lovelace (`100 mem * 0.0577 +
  * 16,000 cpu * 0.0000721`) per ''execution''. So the break-even depends on how often the chain
  * runs: for a script executed once per transaction it sits just under `N = 4`; at `N = 3` the
  * grouping loses about 12 lovelace, and only turns positive from roughly three executions per
  * transaction.
  *
  * [[MinRunSize]] is 5 rather than 4 because the theoretical `N = 4` margin (+0.72 lovelace) does
  * not survive measurement: flat is bit-packed, so a group's 7 theoretical bits round up to a whole
  * byte in the encoded script. Measured over the ten example validators: a threshold of 4 nets +511
  * lovelace for +19 bytes and still makes one of them worse, while 5 nets +533 for no extra bytes
  * at all and makes none worse.
  *
  * All of this is conservative for chains that run more than once per transaction (inside a loop,
  * or a script spending several inputs), where the step saving multiplies but the bytes are paid
  * once.
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
  * That argument covers ordering '''within''' the chain. It does not by itself cover a chain in the
  * function position of an enclosing `Apply`, where [[CaseConstrApply]] would otherwise merge the
  * enclosing arguments into the same `constr` and hoist them ahead of the chain body; `rebuild`
  * leaves the outermost run nested in that case, which keeps the two forms identical. See
  * [[rebuild]].
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

    /** @param inFunctionPosition
      *   this term is the function of an enclosing `Apply`, so its outermost run must stay nested —
      *   see [[rebuild]]
      */
    private def go(term: Term, inFunctionPosition: Boolean = false): Term = term match
        case Apply(LamAbs(_, _, _), _, _) =>
            val (chain, body) = collectChain(term)
            val optimised = chain.map(b => b.copy(rhs = go(b.rhs)))
            rebuild(runs(optimised), go(body), suppressOutermost = inFunctionPosition)
        case Apply(f, arg, ann)          => Apply(go(f, inFunctionPosition = true), go(arg), ann)
        case LamAbs(name, body, ann)     => LamAbs(name, go(body), ann)
        case Force(t, ann)               => Force(go(t), ann)
        case Delay(t, ann)               => Delay(go(t), ann)
        case Constr(tag, args, ann)      => Constr(tag, args.map(go(_)), ann)
        case Case(scrutinee, cases, ann) => Case(go(scrutinee), cases.map(go(_)), ann)
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
        for b <- chain do
            if bound.nonEmpty && (bound.contains(b.name) || (b.rhs.freeVars & bound).nonEmpty) then
                out += current.result()
                current = List.newBuilder[Binding]
                bound = Set.empty
            current += b
            bound += b.name
        if bound.nonEmpty then out += current.result()
        out.result()
    }

    /** Rebuilds the chain, flattening profitable runs and leaving shorter ones nested.
      *
      * `suppressOutermost` keeps the outermost run nested even when it is long enough to group.
      * That is required when the chain sits in the function position of an enclosing `Apply`:
      * [[CaseConstrApply]] flattens the whole left spine, so a grouped outermost run would pull the
      * enclosing application's own arguments into the same `constr` — and `case` evaluates every
      * field before entering the branch, moving those arguments ahead of the chain body. With the
      * run left nested the spine is exactly the one the unoptimized term presents, so the two
      * encode identically. Only the outermost run is reachable from that spine; every deeper run
      * sits inside a lambda body, where `applyToList` cannot reach it.
      */
    private def rebuild(
        groups: List[List[Binding]],
        body: Term,
        suppressOutermost: Boolean
    ): Term = {
        def nest(group: List[Binding], inner: Term): Term =
            group.foldRight(inner) { (b, acc) =>
                Apply(LamAbs(b.name, acc, b.lamAnn), b.rhs, b.applyAnn)
            }

        def flatten(group: List[Binding], inner: Term): Term = {
            logger.log(
              s"LetChainRegroup: grouped ${group.size} bindings (saves ${group.size - 2} steps): ${group.map(_.name).mkString(", ")}"
            )
            val lambdas = group.foldRight(inner)((b, acc) => LamAbs(b.name, acc, b.lamAnn))
            group.foldLeft(lambdas)((f, b) => Apply(f, b.rhs, b.applyAnn))
        }

        def emit(group: List[Binding], inner: Term, suppressed: Boolean): Term =
            if suppressed || group.sizeIs < minRunSize then nest(group, inner)
            else flatten(group, inner)

        groups match
            case Nil => body
            case outermost :: rest =>
                val inner = rest.foldRight(body)((g, acc) => emit(g, acc, suppressed = false))
                emit(outermost, inner, suppressed = suppressOutermost)
    }
}

object LetChainRegroup:

    /** Smallest run of independent bindings worth grouping.
      *
      * A run of `N` saves `N - 2` machine steps per execution and changes the script size by
      * `19 - 3N` bits. Both matter to the fee; see the threshold discussion on [[LetChainRegroup]]
      * for the arithmetic and the corpus measurement behind this value.
      */
    val MinRunSize: Int = 5

    /** Applies let-chain regrouping to a term using default settings. */
    def apply(term: Term): Term = new LetChainRegroup().apply(term)
