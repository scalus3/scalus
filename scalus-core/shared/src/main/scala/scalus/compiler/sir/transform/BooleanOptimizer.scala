package scalus.compiler.sir.transform

import scalus.compiler.sir.SIR.*
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, Binding, SIR, SIRType}
import scalus.uplc.Constant

import scala.collection.mutable

/** Boolean-algebra simplification on SIR, run before lowering.
  *
  * Every rule here must be *semantics preserving*, not just truth-table preserving: in UPLC a
  * subterm can `Error` or `trace`, so a rule may never drop an operand that the original expression
  * would have evaluated. `And`/`Or`/`Not` all lower to [[SIR.IfThenElse]]
  * ([[scalus.compiler.sir.lowering.Lowering]]):
  * {{{
  * And(a, b) => if a then b     else false
  * Or(a, b)  => if a then true  else b
  * Not(a)    => if a then false else true
  * }}}
  * so `a` is always evaluated and `b` only conditionally. That asymmetry decides which folds are
  * legal - see [[mkAnd]]/[[mkOr]].
  *
  * Implemented rules:
  *   - double negation: `Not(Not(a))` => `a`
  *   - conditional negation: `If(Not(c), t, f)` => `If(c, f, t)` (saves one `Case`/`ifThenElse`)
  *   - reverse De Morgan (one node fewer): `Or(Not(a), Not(b))` => `Not(And(a, b))` and
  *     `And(Not(a), Not(b))` => `Not(Or(a, b))`
  *   - constant folding of `Not`, and of `If` with a constant condition
  *   - identity/annihilation folds that do not drop an evaluated operand
  *
  * Deliberately NOT implemented, because they drop an operand the original evaluates:
  * `a && !a => false`, `a || !a => true`, `And(a, false) => false`, `Or(a, true) => true`,
  * `If(c, t, t) => t`. Idempotence (`a && a => a`) is out for the same reason: it evaluates `a`
  * once where the original evaluates it twice, halving a duplicated `trace`.
  *
  * The pass is a single bottom-up rebuild: children are optimized first, then the smart
  * constructors apply the rules to already-optimized children. Rules never re-enter a full
  * traversal, so the cost is linear in the tree size.
  */
object BooleanOptimizer {

    /** Which rules fired during one run, and where. Recording is off for plain [[optimize]], and a
      * fresh instance is allocated per call, so the pass holds no shared mutable state.
      */
    final class Stats private[sir] (recording: Boolean) {
        private val counts = mutable.LinkedHashMap.empty[String, Int]
        private val siteList = mutable.ListBuffer.empty[(String, String)]

        private[sir] def bump(rule: String, anns: AnnotationsDecl): Unit =
            if recording then {
                counts.updateWith(rule)(c => Some(c.getOrElse(0) + 1))
                siteList += (rule -> anns.pos.show)
            }

        /** How many times each rule fired. */
        def hits: Map[String, Int] = counts.toMap

        /** Total number of rewrites. */
        def total: Int = counts.valuesIterator.sum

        /** Every rewrite as `(rule, source position)`, in the order they were applied. */
        def sites: Seq[(String, String)] = siteList.toSeq
    }

    /** Optimizes boolean expressions in `sir`. */
    def optimize(sir: SIR): SIR = go(sir, new Stats(recording = false))

    /** Same as [[optimize]], but also reports which rules fired and where. */
    def optimizeCounting(sir: SIR): (SIR, Stats) = {
        val stats = new Stats(recording = true)
        (go(sir, stats), stats)
    }

    private def go(sir: SIR, st: Stats): SIR =
        sir match
            case Decl(data, term)    => Decl(data, go(term, st))
            case ansir: AnnotatedSIR => goExpr(ansir, st)

    private def goExpr(sir: AnnotatedSIR, st: Stats): AnnotatedSIR =
        sir match
            case And(a, b, anns) => mkAnd(goExpr(a, st), goExpr(b, st), anns, st)
            case Or(a, b, anns)  => mkOr(goExpr(a, st), goExpr(b, st), anns, st)
            case Not(a, anns)    => mkNot(goExpr(a, st), anns, st)
            case IfThenElse(cond, t, f, tp, anns) =>
                mkIf(goExpr(cond, st), goExpr(t, st), goExpr(f, st), tp, anns, st)
            case Apply(f, arg, tp, anns) => Apply(goExpr(f, st), goExpr(arg, st), tp, anns)
            case Select(scrutinee, field, tp, anns) => Select(go(scrutinee, st), field, tp, anns)
            case Match(scrutinee, cases, tp, anns) =>
                val optCases = cases.map(c => Case(c.pattern, go(c.body, st), c.anns))
                Match(goExpr(scrutinee, st), optCases, tp, anns)
            case Let(bindings, body, flags, anns) =>
                val optBindings = bindings.map(b => Binding(b.name, b.tp, go(b.value, st)))
                Let(optBindings, go(body, st), flags, anns)
            case LamAbs(param, term, typeParams, anns) =>
                LamAbs(param, go(term, st), typeParams, anns)
            case Constr(name, data, args, tp, anns) =>
                Constr(name, data, args.map(go(_, st)), tp, anns)
            case Cast(term, tp, anns)    => Cast(goExpr(term, st), tp, anns)
            case Error(msg, anns, cause) => Error(goExpr(msg, st), anns, cause)
            // Leaves enumerated rather than matched by a catch-all, so that a node type added
            // to SIR later fails to compile here instead of silently going untraversed - which
            // is exactly how the original pass came to skip Let bindings, Constr, Cast and Error.
            case _: Var | _: ExternalVar | _: Const | _: Builtin => sir

    private def boolConst(value: Boolean, anns: AnnotationsDecl): Const =
        Const(Constant.Bool(value), SIRType.Boolean, anns)

    /** `And(a, b)` lowers to `if a then b else false`, so `a` is always evaluated and `b` only when
      * `a` is true. Folds that drop `a` (e.g. `And(a, false) => false`) are therefore illegal.
      */
    private def mkAnd(
        a: AnnotatedSIR,
        b: AnnotatedSIR,
        anns: AnnotationsDecl,
        st: Stats
    ): AnnotatedSIR =
        (a, b) match
            // b is unevaluated when a is false, so dropping it is safe
            case (Const(Constant.Bool(false), _, _), _) =>
                st.bump("and-false-short-circuit", anns); boolConst(false, anns)
            case (Const(Constant.Bool(true), _, _), _) => st.bump("and-true-identity", anns); b
            // `if a then true else false` == a; a is evaluated either way
            case (_, Const(Constant.Bool(true), _, _)) => st.bump("and-true-identity", anns); a
            // reverse De Morgan: !a && !b => !(a || b), one node fewer, same evaluation order
            case (Not(x, _), Not(y, _)) =>
                st.bump("de-morgan-and", anns)
                mkNot(mkOr(x, y, anns, st), anns, st)
            case _ => And(a, b, anns)

    /** `Or(a, b)` lowers to `if a then true else b`: `a` is always evaluated, `b` only when `a` is
      * false. Folds that drop `a` (e.g. `Or(a, true) => true`) are therefore illegal.
      */
    private def mkOr(
        a: AnnotatedSIR,
        b: AnnotatedSIR,
        anns: AnnotationsDecl,
        st: Stats
    ): AnnotatedSIR =
        (a, b) match
            case (Const(Constant.Bool(true), _, _), _) =>
                st.bump("or-true-short-circuit", anns); boolConst(true, anns)
            case (Const(Constant.Bool(false), _, _), _) => st.bump("or-false-identity", anns); b
            // `if a then true else false` == a
            case (_, Const(Constant.Bool(false), _, _)) => st.bump("or-false-identity", anns); a
            // reverse De Morgan: !a || !b => !(a && b)
            case (Not(x, _), Not(y, _)) =>
                st.bump("de-morgan-or", anns)
                mkNot(mkAnd(x, y, anns, st), anns, st)
            case _ => Or(a, b, anns)

    private def mkNot(a: AnnotatedSIR, anns: AnnotationsDecl, st: Stats): AnnotatedSIR =
        a match
            case Const(Constant.Bool(v), _, _) => st.bump("not-const", anns); boolConst(!v, anns)
            case Not(inner, _)                 => st.bump("double-negation", anns); inner
            case _                             => Not(a, anns)

    private def mkIf(
        cond: AnnotatedSIR,
        t: AnnotatedSIR,
        f: AnnotatedSIR,
        tp: SIRType,
        anns: AnnotationsDecl,
        st: Stats
    ): AnnotatedSIR =
        cond match
            case Const(Constant.Bool(true), _, _)  => st.bump("if-const-cond", anns); t
            case Const(Constant.Bool(false), _, _) => st.bump("if-const-cond", anns); f
            // if !c then t else f  ==  if c then f else t, and saves the Not's Case node.
            // `inner` is already optimized and mkNot guarantees it is not itself a Not,
            // so a single swap is enough - no re-traversal.
            case Not(inner, _) =>
                st.bump("if-not-swap", anns)
                mkIf(inner, f, t, tp, anns, st)
            case _ => IfThenElse(cond, t, f, tp, anns)
}
