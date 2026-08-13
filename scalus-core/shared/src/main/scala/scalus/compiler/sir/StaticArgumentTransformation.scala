package scalus.compiler.sir

import scalus.compiler.sir.SIR.*

import scala.collection.mutable

/** Static-argument transformation (T1 of `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md`).
  *
  * A parameter of a single-binding recursive `Let` is *static* when every self-call passes exactly
  * that parameter's own variable in the same position. Static parameters are bound once by a
  * wrapper lambda, and the inner recursion re-passes only the changing ones:
  *
  * {{{
  *   let rec f = λp1...pn. body                     // some pi static in every self-call
  *   -->
  *   let f = λp1...pn.
  *       let rec f$sat = λq1...qk. body[ f e1...en := f$sat e_q1...e_qk ]
  *       in f$sat q1...qk                           // q = changing params, in source order
  * }}}
  *
  * The wrapper keeps the original name, arity and type, so external uses of `f` (partial
  * applications, higher-order uses, eta-lets) are unaffected. The fixpoint is built once per
  * *entry* into `f` instead of once per iteration, and each iteration saves one `Apply` per lifted
  * argument.
  *
  * The transform is skipped (input returned unchanged) when it cannot be proven safe:
  *   - multi-binding recursive `Let`s (mutual-recursion groups) and lazy lets;
  *   - a rhs that is not a lambda, or has duplicate parameter names;
  *   - any self-reference that is not the head of a self-call saturated to full arity (a bare
  *     reference or a partial application could observe the original arity);
  *   - no static parameter at all.
  *
  * If *every* parameter is static the last one is demoted to changing, because a nullary strict
  * letrec would diverge.
  *
  * Runs only when optimization is enabled - see `Compiled.toUplc` and `scalus/package.scala`. Note
  * it runs *before* `MutualRecursionElimination` (which sits at the backend entry points), so the
  * peers-as-params static arguments that pass introduces are not lifted here.
  */
object StaticArgumentTransformation {

    /** Suffix of the generated inner recursive binding. */
    val SatSuffix: String = "$sat"

    def apply(sir: SIR): SIR = transform(sir)

    private def transform(sir: SIR): SIR = sir match
        case Decl(data, term)   => Decl(data, transform(term))
        case expr: AnnotatedSIR => transformExpr(expr)

    private def transformExpr(sir: AnnotatedSIR): AnnotatedSIR = sir match
        case Let(List(b), body, flags, anns) if flags.isRec && !flags.isLazy =>
            val newRhs = transform(b.value)
            val newBody = transform(body)
            trySat(b.name, newRhs) match
                case Some(wrapper) =>
                    Let(
                      List(Binding(b.name, b.tp, wrapper)),
                      newBody,
                      flags.remove(LetFlags.Recursivity),
                      anns
                    )
                case None =>
                    Let(List(Binding(b.name, b.tp, newRhs)), newBody, flags, anns)
        case Let(bindings, body, flags, anns) =>
            Let(
              bindings.map(b => Binding(b.name, b.tp, transform(b.value))),
              transform(body),
              flags,
              anns
            )
        case LamAbs(param, term, tps, anns) => LamAbs(param, transform(term), tps, anns)
        case Apply(f, arg, tp, anns)        => Apply(transformExpr(f), transformExpr(arg), tp, anns)
        case Select(s, field, tp, anns)     => Select(transform(s), field, tp, anns)
        case IfThenElse(c, t, f, tp, anns) =>
            IfThenElse(transformExpr(c), transformExpr(t), transformExpr(f), tp, anns)
        case And(a, b, anns) => And(transformExpr(a), transformExpr(b), anns)
        case Or(a, b, anns)  => Or(transformExpr(a), transformExpr(b), anns)
        case Not(a, anns)    => Not(transformExpr(a), anns)
        case Match(scrutinee, cases, tp, anns) =>
            Match(
              transformExpr(scrutinee),
              cases.map(c => c.copy(body = transform(c.body))),
              tp,
              anns
            )
        case Constr(name, data, args, tp, anns) =>
            Constr(name, data, args.map(transform), tp, anns)
        case Cast(expr, tp, anns)    => Cast(transformExpr(expr), tp, anns)
        case Error(msg, anns, cause) => Error(transformExpr(msg), anns, cause)
        case _: Builtin | _: Var | _: ExternalVar | _: Const => sir

    // ------------------------------------------------------------------ helpers

    /** One shell of the outer lambda chain of a recursive rhs. */
    private final case class Lam(
        param: Var,
        typeParams: List[SIRType.TypeVar],
        anns: AnnotationsDecl
    )

    /** Peels the outer lambda chain: returns the lambda shells and the innermost body. */
    private def peel(rhs: SIR): (List[Lam], SIR) = rhs match
        case LamAbs(p, t, tps, anns) =>
            val (rest, body) = peel(t)
            (Lam(p, tps, anns) :: rest, body)
        case other => (Nil, other)

    /** Unwinds an application spine: `Apply(Apply(h, a1), a2)` -> `(h, List(a1, a2))`. */
    private def spine(e: AnnotatedSIR): (AnnotatedSIR, List[AnnotatedSIR]) = e match
        case Apply(f, arg, _, _) =>
            val (h, args) = spine(f)
            (h, args :+ arg)
        case other => (other, Nil)

    private def applyOne(
        f: AnnotatedSIR,
        arg: AnnotatedSIR,
        anns: AnnotationsDecl
    ): AnnotatedSIR = {
        val resTp = f.tp match
            case SIRType.Fun(_, out)                        => out
            case SIRType.TypeLambda(_, SIRType.Fun(_, out)) => out
            case other                                      => other
        Apply(f, arg, resTp, anns)
    }

    private def isSelfRef(e: AnnotatedSIR, name: String, shadowed: Set[String]): Boolean =
        e match
            case Var(n, _, _)            => n == name && !shadowed.contains(name)
            case ExternalVar(_, n, _, _) => n == name && !shadowed.contains(name)
            case _                       => false

    // ------------------------------------------------------------------ SAT core

    private def trySat(name: String, rhs: SIR): Option[SIR] = {
        val (lams, innerBody) = peel(rhs)
        val params = lams.map(_.param)
        val n = params.length
        if n == 0 then return None
        if params.map(_.name).distinct.length != n then return None

        val analysis = new Analysis(name, params)
        analysis.run(innerBody, Set.empty)
        if analysis.blocked || analysis.masks.isEmpty then return None

        val combined = analysis.masks.reduce((a, b) => a.lazyZip(b).map(_ && _))
        val staticMask =
            if combined.forall(identity) then combined.updated(n - 1, false)
            else combined
        if !staticMask.exists(identity) then return None

        val changing = params.lazyZip(staticMask).collect { case (p, false) => p }.toList
        val satName = name + SatSuffix
        val satTp = changing.foldRight(innerBody.tp)((p, acc) => SIRType.Fun(p.tp, acc))
        val anns = lams.head.anns

        val rewritten =
            new Rewriter(name, satName, satTp, n, staticMask).rewrite(innerBody, Set.empty)

        val satLam = changing.foldRight(rewritten)((p, acc) => LamAbs(p, acc, List.empty, anns))
        val entry = changing.foldLeft(Var(satName, satTp, anns): AnnotatedSIR) { (acc, p) =>
            applyOne(acc, Var(p.name, p.tp, anns), anns)
        }
        val innerLet =
            Let(List(Binding(satName, satTp, satLam)), entry, LetFlags.Recursivity, anns)
        val wrapper = lams.foldRight(innerLet: SIR) { (lam, acc) =>
            LamAbs(lam.param, acc, lam.typeParams, lam.anns)
        }
        Some(wrapper)
    }

    /** Pass 1. Collects one static mask per self-call, and sets `blocked` when a self-reference
      * appears anywhere other than the head of a saturated self-call spine.
      */
    private final class Analysis(name: String, params: List[Var]) {
        var blocked: Boolean = false
        private val collected = mutable.ListBuffer.empty[List[Boolean]]
        private val arity = params.length

        def masks: List[List[Boolean]] = collected.toList

        def run(sir: SIR, shadowed: Set[String]): Unit = sir match
            case Decl(_, term)      => run(term, shadowed)
            case expr: AnnotatedSIR => runExpr(expr, shadowed)

        private def runExpr(sir: AnnotatedSIR, shadowed: Set[String]): Unit = sir match
            case app: Apply =>
                val (head, args) = spine(app)
                if isSelfRef(head, name, shadowed) then
                    if args.length < arity then blocked = true
                    else
                        collected += params
                            .lazyZip(args.take(arity))
                            .map { (p, a) =>
                                a match
                                    case Var(an, _, _) => an == p.name && !shadowed.contains(an)
                                    case _             => false
                            }
                            .toList
                else runExpr(head, shadowed)
                args.foreach(runExpr(_, shadowed))
            case v: Var         => if isSelfRef(v, name, shadowed) then blocked = true
            case v: ExternalVar => if isSelfRef(v, name, shadowed) then blocked = true
            case Let(bs, body, flags, _) =>
                val names = bs.map(_.name).toSet
                val rhsShadow = if flags.isRec then shadowed ++ names else shadowed
                bs.foreach(b => run(b.value, rhsShadow))
                run(body, shadowed ++ names)
            case LamAbs(param, term, _, _) => run(term, shadowed + param.name)
            case Select(s, _, _, _)        => run(s, shadowed)
            case IfThenElse(c, t, f, _, _) =>
                runExpr(c, shadowed); runExpr(t, shadowed); runExpr(f, shadowed)
            case And(a, b, _) => runExpr(a, shadowed); runExpr(b, shadowed)
            case Or(a, b, _)  => runExpr(a, shadowed); runExpr(b, shadowed)
            case Not(a, _)    => runExpr(a, shadowed)
            case Match(scrutinee, cases, _, _) =>
                runExpr(scrutinee, shadowed)
                cases.foreach { c =>
                    val caseShadowed = c.pattern match
                        case Pattern.Constr(_, patBindings, _) => shadowed ++ patBindings
                        case _                                 => shadowed
                    run(c.body, caseShadowed)
                }
            case Constr(_, _, args, _, _) => args.foreach(run(_, shadowed))
            case Cast(expr, _, _)         => runExpr(expr, shadowed)
            case Error(msg, _, _)         => runExpr(msg, shadowed)
            case _: Builtin | _: Const    => ()
    }

    /** Pass 2. Replaces every saturated self-call spine `f e1...en extra...` with
      * `f$sat e_c1...e_ck extra...`, dropping the static arguments (which are, by definition of
      * static, plain variable references and therefore effect-free).
      */
    private final class Rewriter(
        name: String,
        satName: String,
        satTp: SIRType,
        arity: Int,
        staticMask: List[Boolean]
    ) {

        def rewrite(sir: SIR, shadowed: Set[String]): SIR = sir match
            case Decl(data, term)   => Decl(data, rewrite(term, shadowed))
            case expr: AnnotatedSIR => rewriteExpr(expr, shadowed)

        def rewriteExpr(sir: AnnotatedSIR, shadowed: Set[String]): AnnotatedSIR = sir match
            case app: Apply =>
                val (head, args) = spine(app)
                if isSelfRef(head, name, shadowed) && args.length >= arity then
                    val newArgs = args.map(rewriteExpr(_, shadowed))
                    val changingArgs =
                        newArgs.take(arity).lazyZip(staticMask).collect { case (a, false) => a }
                    val base = changingArgs.foldLeft(Var(satName, satTp, head.anns): AnnotatedSIR) {
                        (acc, a) => applyOne(acc, a, app.anns)
                    }
                    newArgs.drop(arity).foldLeft(base)((acc, a) => applyOne(acc, a, app.anns))
                else
                    Apply(
                      rewriteExpr(app.f, shadowed),
                      rewriteExpr(app.arg, shadowed),
                      app.tp,
                      app.anns
                    )
            case Let(bs, body, flags, anns) =>
                val names = bs.map(_.name).toSet
                val rhsShadow = if flags.isRec then shadowed ++ names else shadowed
                Let(
                  bs.map(b => Binding(b.name, b.tp, rewrite(b.value, rhsShadow))),
                  rewrite(body, shadowed ++ names),
                  flags,
                  anns
                )
            case LamAbs(param, term, tps, anns) =>
                LamAbs(param, rewrite(term, shadowed + param.name), tps, anns)
            case Select(s, field, tp, anns) => Select(rewrite(s, shadowed), field, tp, anns)
            case IfThenElse(c, t, f, tp, anns) =>
                IfThenElse(
                  rewriteExpr(c, shadowed),
                  rewriteExpr(t, shadowed),
                  rewriteExpr(f, shadowed),
                  tp,
                  anns
                )
            case And(a, b, anns) => And(rewriteExpr(a, shadowed), rewriteExpr(b, shadowed), anns)
            case Or(a, b, anns)  => Or(rewriteExpr(a, shadowed), rewriteExpr(b, shadowed), anns)
            case Not(a, anns)    => Not(rewriteExpr(a, shadowed), anns)
            case Match(scrutinee, cases, tp, anns) =>
                Match(
                  rewriteExpr(scrutinee, shadowed),
                  cases.map { c =>
                      val caseShadowed = c.pattern match
                          case Pattern.Constr(_, patBindings, _) => shadowed ++ patBindings
                          case _                                 => shadowed
                      c.copy(body = rewrite(c.body, caseShadowed))
                  },
                  tp,
                  anns
                )
            case Constr(cn, data, args, tp, anns) =>
                Constr(cn, data, args.map(rewrite(_, shadowed)), tp, anns)
            case Cast(expr, tp, anns)    => Cast(rewriteExpr(expr, shadowed), tp, anns)
            case Error(msg, anns, cause) => Error(rewriteExpr(msg, shadowed), anns, cause)
            case _: Builtin | _: Const | _: Var | _: ExternalVar => sir
    }
}
