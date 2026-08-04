package scalus.compiler.sir

import scalus.compiler.sir.SIR.*

/** Eliminates multi-binding recursive `Let` groups by rewriting each group into nested
  * single-binding lets (peers-as-params), so the lowering backends only ever see single
  * self-recursion, which they encode via self-application.
  *
  * For a group `f1..fN` (binding order) it emits, outermost first:
  * {{{
  *   let fNp = λf1...f(N-1). rhsN'   // fresh name fN$mutrec
  *   ...
  *   let f2p = λf1. rhs2'
  *   let f1  = rhs1'
  *   in body'
  * }}}
  * where inside `rhs_i'` (context `i`; context 1 also covers `rhs1'` and the body):
  *   - a reference to `fj` with `j < i` stays the (param) variable `fj`;
  *   - a reference to `fj` with `j >= i` becomes `fjp E(1) ... E(j-1)` with the same rule applied
  *     to the arguments recursively.
  * Each member's rhs must be a lambda; a cyclic group of plain values is rejected. See
  * docs/superpowers/specs/2026-08-04-mutual-recursion-design.md.
  */
object MutualRecursionElimination {

    def apply(sir: SIR): SIR = transform(sir)

    private def transform(sir: SIR): SIR = sir match
        case Decl(data, term)   => Decl(data, transform(term))
        case expr: AnnotatedSIR => transformExpr(expr)

    private def transformExpr(sir: AnnotatedSIR): AnnotatedSIR = sir match
        case Let(bindings, body, flags, anns) if flags.isRec && bindings.sizeIs >= 2 =>
            val nBindings = bindings.map(b => Binding(b.name, b.tp, transform(b.value)))
            eliminate(nBindings, transform(body), flags, anns)
        case Let(bindings, body, flags, anns) =>
            Let(
              bindings.map(b => Binding(b.name, b.tp, transform(b.value))),
              transform(body),
              flags,
              anns
            )
        case LamAbs(param, term, tps, anns) => LamAbs(param, transform(term), tps, anns)
        case Apply(f, arg, tp, anns) =>
            Apply(transformExpr(f), transformExpr(arg), tp, anns)
        case Select(s, field, tp, anns) => Select(transform(s), field, tp, anns)
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
        case Cast(expr, tp, anns) => Cast(transformExpr(expr), tp, anns)
        case _: Builtin | _: Error | _: Var | _: ExternalVar | _: Const => sir

    private def eliminate(
        bindings: List[Binding],
        body: SIR,
        flags: SIR.LetFlags,
        anns: AnnotationsDecl
    ): AnnotatedSIR = {
        val n = bindings.size
        val names = bindings.map(_.name).toVector
        val tps = bindings.map(_.tp).toVector
        bindings.foreach { b =>
            b.value match
                case _: LamAbs => ()
                case other =>
                    throw new IllegalArgumentException(
                      s"mutually recursive values are not supported (only functions): " +
                          s"'${b.name}' in group ${names.mkString(", ")} at ${anns.pos.file}:${anns.pos.startLine}"
                    )
        }
        val pNames = names.map(_ + "$mutrec")
        // fip takes f1..f(i-1) as params: tps(0) -> ... -> tps(i-2) -> tps(i-1); i is 1-based
        def pTp(i: Int): SIRType =
            (0 until i - 1).foldRight(tps(i - 1))((k, acc) => SIRType.Fun(tps(k), acc))

        def applyChain(f: AnnotatedSIR, args: Seq[AnnotatedSIR], anns: AnnotationsDecl) =
            args.foldLeft(f) { (acc, arg) =>
                val resTp = acc.tp match
                    case SIRType.Fun(_, out) => out
                    case t                   => t
                Apply(acc, arg, resTp, anns)
            }

        // E(j) in context i, 1-based: the expression denoting fj where the vars
        // f1..f(i-1) (or f1 itself for i == 1) are in scope.
        def peerExpr(j: Int, i: Int, occAnns: AnnotationsDecl): AnnotatedSIR =
            if j < i || (i == 1 && j == 1) then Var(names(j - 1), tps(j - 1), occAnns)
            else
                applyChain(
                  Var(pNames(j - 1), pTp(j), occAnns),
                  (1 until j).map(k => peerExpr(k, i, occAnns)),
                  occAnns
                )

        def rewrite(sir: SIR, i: Int, shadowed: Set[String]): SIR = sir match
            case Decl(data, term)   => Decl(data, rewrite(term, i, shadowed))
            case expr: AnnotatedSIR => rewriteExpr(expr, i, shadowed)

        def rewriteExpr(sir: AnnotatedSIR, i: Int, shadowed: Set[String]): AnnotatedSIR =
            sir match
                case v @ Var(name, _, occAnns) if !shadowed(name) =>
                    val j = names.indexOf(name)
                    if j >= 0 then peerExpr(j + 1, i, occAnns) else v
                case v @ ExternalVar(_, name, _, occAnns) if !shadowed(name) =>
                    val j = names.indexOf(name)
                    if j >= 0 then peerExpr(j + 1, i, occAnns) else v
                case v: Var         => v
                case v: ExternalVar => v
                case Let(bs, b, fl, a) =>
                    val newShadowed = shadowed ++ bs.map(_.name)
                    // rec bindings see themselves; non-rec rhs uses the outer scope,
                    // but group names are full names that locals cannot collide with,
                    // so shadowing both sides is safe and simple
                    Let(
                      bs.map(bd => Binding(bd.name, bd.tp, rewrite(bd.value, i, newShadowed))),
                      rewrite(b, i, newShadowed),
                      fl,
                      a
                    )
                case LamAbs(param, term, tps, a) =>
                    LamAbs(param, rewrite(term, i, shadowed + param.name), tps, a)
                case Apply(f, arg, tp, a) =>
                    Apply(rewriteExpr(f, i, shadowed), rewriteExpr(arg, i, shadowed), tp, a)
                case Select(s, field, tp, a) => Select(rewrite(s, i, shadowed), field, tp, a)
                case IfThenElse(c, t, f, tp, a) =>
                    IfThenElse(
                      rewriteExpr(c, i, shadowed),
                      rewriteExpr(t, i, shadowed),
                      rewriteExpr(f, i, shadowed),
                      tp,
                      a
                    )
                case And(x, y, a) =>
                    And(rewriteExpr(x, i, shadowed), rewriteExpr(y, i, shadowed), a)
                case Or(x, y, a) => Or(rewriteExpr(x, i, shadowed), rewriteExpr(y, i, shadowed), a)
                case Not(x, a)   => Not(rewriteExpr(x, i, shadowed), a)
                case Match(scrutinee, cases, tp, a) =>
                    Match(
                      rewriteExpr(scrutinee, i, shadowed),
                      cases.map { c =>
                          val caseShadowed = c.pattern match
                              case Pattern.Constr(_, patBindings, _) => shadowed ++ patBindings
                              case _                                 => shadowed
                          c.copy(body = rewrite(c.body, i, caseShadowed))
                      },
                      tp,
                      a
                    )
                case Constr(name, data, args, tp, a) =>
                    Constr(name, data, args.map(rewrite(_, i, shadowed)), tp, a)
                case Cast(expr, tp, a)                => Cast(rewriteExpr(expr, i, shadowed), tp, a)
                case _: Builtin | _: Error | _: Const => sir

        def recFlagsFor(letName: String, rhs: SIR): SIR.LetFlags =
            if RemoveRecursivity.isRecursive(letName, rhs) then flags
            else flags.remove(SIR.LetFlags.Recursivity)

        // innermost: f1 under its original name
        val rhs1 = rewrite(bindings.head.value, 1, Set.empty)
        val body1 = rewrite(body, 1, Set.empty)
        val innermost =
            Let(List(Binding(names(0), tps(0), rhs1)), body1, recFlagsFor(names(0), rhs1), anns)

        // wrap with f2p .. fNp, fNp outermost
        (2 to n).foldLeft(innermost: AnnotatedSIR) { (acc, i) =>
            val rewritten = rewrite(bindings(i - 1).value, i, Set.empty)
            val paramWrapped = (0 until i - 1).foldRight(rewritten) { (k, b) =>
                LamAbs(Var(names(k), tps(k), anns), b, List.empty, anns)
            }
            val pName = pNames(i - 1)
            Let(
              List(Binding(pName, pTp(i), paramWrapped)),
              acc,
              recFlagsFor(pName, rewritten),
              anns
            )
        }
    }
}
