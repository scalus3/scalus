package scalus.compiler.sir

import scalus.compiler.sir.SIR.*

import scala.collection.mutable

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
  * where inside `rhs_i'` (context `i`; context 1 also covers `rhs1'` and the body), a reference to
  * `fj` is rewritten to `argExpr(j, i)`:
  *   - `j < i`: stays the (param) variable `fj`;
  *   - `j == i`: the self-application chain `fip f1 .. f(i-1)` (or plain `f1` when `i == 1`);
  *   - `j == i + 1`: the adjacent chain `f(i+1)p f1 .. fi`, where the `fi` argument is the `j == i`
  *     self chain above - still built from plain variables only;
  *   - `j >= i + 2`: a *bounded* reference to `fj` under its original name, backed by a
  *     non-recursive `let fj = λ$eta. (fjp E(1) .. E(j-1)) $eta in ...` emitted once per context
  *     around the rewritten rhs/body. The eta-wrapper defers the (potentially expensive)
  *     construction of `fjp`'s argument chain until `fj` is actually called, which is required for
  *     correctness (a plain, non-eta-expanded `let fj = fjp E(1)..E(j-1)` would force that
  *     application immediately, re-entering the still-being-defined peers and diverging), and lets
  *     every `E(k)` for `i + 2 <= k < j` used inside that chain itself be *just* `Var(fk)`
  *     referring to an already-emitted eta-let, rather than being re-expanded from scratch. Without
  *     this, `j >= i + 2` references recursively re-expand their own `1 .. j-1` argument lists
  *     (each of which may again contain far references), which is exponential in the reference
  *     distance `j - i`. With the eta-lets, each context does O(distance) work and the whole group
  *     is O(N^2).
  *
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

        // The j == i self-application chain: fip f1 .. f(i-1), or plain f1 when i == 1
        // (there is no f1p - f1 is bound directly under its own name). Every argument here
        // has index < i, so this is O(i) and never recurses further.
        def selfChain(i: Int, occAnns: AnnotationsDecl): AnnotatedSIR =
            if i == 1 then Var(names(0), tps(0), occAnns)
            else
                applyChain(
                  Var(pNames(i - 1), pTp(i), occAnns),
                  (1 until i).map(k => Var(names(k - 1), tps(k - 1), occAnns)),
                  occAnns
                )

        // Defensive fallback only: the original, unbounded expansion (recomputes E(k) for
        // every k < j, including far ones), used solely when a group member's declared type
        // isn't a plain SIRType.Fun so an eta-wrapper's parameter type can't be derived. This
        // should not be reachable given the LamAbs-only guard above, but if it is, correctness
        // is preserved at the cost of the pre-fix exponential blowup for that single reference.
        def fullExpand(j: Int, i: Int, occAnns: AnnotationsDecl): AnnotatedSIR =
            if j < i || (i == 1 && j == 1) then Var(names(j - 1), tps(j - 1), occAnns)
            else
                applyChain(
                  Var(pNames(j - 1), pTp(j), occAnns),
                  (1 until j).map(k => fullExpand(k, i, occAnns)),
                  occAnns
                )

        // argExpr(j, i): the expression denoting fj in context i. Bounded: j == i and
        // j == i + 1 do a small amount of direct work (O(i)); j >= i + 2 defers to an
        // eta-let (registered into farRefs so the caller knows to emit it) instead of
        // re-expanding fjp's whole argument chain inline.
        def argExpr(
            j: Int,
            i: Int,
            occAnns: AnnotationsDecl,
            farRefs: mutable.Set[Int]
        ): AnnotatedSIR =
            if j < i then Var(names(j - 1), tps(j - 1), occAnns)
            else if j == i then selfChain(i, occAnns)
            else if j == i + 1 then
                applyChain(
                  Var(pNames(j - 1), pTp(j), occAnns),
                  (1 until j).map(k => argExpr(k, i, occAnns, farRefs)),
                  occAnns
                )
            else
                tps(j - 1) match
                    case _: SIRType.Fun =>
                        farRefs += j
                        Var(names(j - 1), tps(j - 1), occAnns)
                    case _ =>
                        fullExpand(j, i, occAnns)

        // The eta-let bound under fj's own name: let fj = λ$eta. (fjp E(1)..E(j-1)) $eta.
        // Its own argument chain only ever references indices < j, so the farRefs it might
        // touch are always <= maxRef already computed by the caller - nothing new to collect.
        def etaBinding(j: Int, i: Int): Binding = {
            val (etaIn, etaOut) = tps(j - 1) match
                case SIRType.Fun(in, out) => (in, out)
                case other =>
                    throw new IllegalStateException(
                      s"expected function type for '${names(j - 1)}', got ${other.show}"
                    )
            val discard = mutable.Set.empty[Int]
            val fjpApplied = applyChain(
              Var(pNames(j - 1), pTp(j), anns),
              (1 until j).map(k => argExpr(k, i, anns, discard)),
              anns
            )
            val etaVar = Var(names(j - 1) + "$eta", etaIn, anns)
            val etaBody = Apply(fjpApplied, etaVar, etaOut, anns)
            Binding(names(j - 1), tps(j - 1), LamAbs(etaVar, etaBody, List.empty, anns))
        }

        // Wraps `rewritten` with one non-recursive eta-let per far reference collected while
        // rewriting it, from i + 2 up to the largest referenced index, innermost let last
        // (closest to `rewritten`) so each later let can see the earlier ones.
        def wrapWithFarRefLets(rewritten: SIR, i: Int, farRefs: mutable.Set[Int]): SIR =
            farRefs.maxOption match
                case None => rewritten
                case Some(maxRef) =>
                    (i + 2 to maxRef).foldRight(rewritten) { (k, acc) =>
                        Let(List(etaBinding(k, i)), acc, LetFlags.None, anns)
                    }

        def rewrite(sir: SIR, i: Int, shadowed: Set[String], farRefs: mutable.Set[Int]): SIR =
            sir match
                case Decl(data, term)   => Decl(data, rewrite(term, i, shadowed, farRefs))
                case expr: AnnotatedSIR => rewriteExpr(expr, i, shadowed, farRefs)

        def rewriteExpr(
            sir: AnnotatedSIR,
            i: Int,
            shadowed: Set[String],
            farRefs: mutable.Set[Int]
        ): AnnotatedSIR =
            sir match
                case v @ Var(name, _, occAnns) if !shadowed(name) =>
                    val j = names.indexOf(name)
                    if j >= 0 then argExpr(j + 1, i, occAnns, farRefs) else v
                case v @ ExternalVar(_, name, _, occAnns) if !shadowed(name) =>
                    val j = names.indexOf(name)
                    if j >= 0 then argExpr(j + 1, i, occAnns, farRefs) else v
                case v: Var         => v
                case v: ExternalVar => v
                case Let(bs, b, fl, a) =>
                    val newShadowed = shadowed ++ bs.map(_.name)
                    // rec bindings see themselves; non-rec rhs uses the outer scope,
                    // but group names are full names that locals cannot collide with,
                    // so shadowing both sides is safe and simple
                    Let(
                      bs.map(bd =>
                          Binding(bd.name, bd.tp, rewrite(bd.value, i, newShadowed, farRefs))
                      ),
                      rewrite(b, i, newShadowed, farRefs),
                      fl,
                      a
                    )
                case LamAbs(param, term, tps, a) =>
                    LamAbs(param, rewrite(term, i, shadowed + param.name, farRefs), tps, a)
                case Apply(f, arg, tp, a) =>
                    Apply(
                      rewriteExpr(f, i, shadowed, farRefs),
                      rewriteExpr(arg, i, shadowed, farRefs),
                      tp,
                      a
                    )
                case Select(s, field, tp, a) =>
                    Select(rewrite(s, i, shadowed, farRefs), field, tp, a)
                case IfThenElse(c, t, f, tp, a) =>
                    IfThenElse(
                      rewriteExpr(c, i, shadowed, farRefs),
                      rewriteExpr(t, i, shadowed, farRefs),
                      rewriteExpr(f, i, shadowed, farRefs),
                      tp,
                      a
                    )
                case And(x, y, a) =>
                    And(
                      rewriteExpr(x, i, shadowed, farRefs),
                      rewriteExpr(y, i, shadowed, farRefs),
                      a
                    )
                case Or(x, y, a) =>
                    Or(
                      rewriteExpr(x, i, shadowed, farRefs),
                      rewriteExpr(y, i, shadowed, farRefs),
                      a
                    )
                case Not(x, a) => Not(rewriteExpr(x, i, shadowed, farRefs), a)
                case Match(scrutinee, cases, tp, a) =>
                    Match(
                      rewriteExpr(scrutinee, i, shadowed, farRefs),
                      cases.map { c =>
                          val caseShadowed = c.pattern match
                              case Pattern.Constr(_, patBindings, _) => shadowed ++ patBindings
                              case _                                 => shadowed
                          c.copy(body = rewrite(c.body, i, caseShadowed, farRefs))
                      },
                      tp,
                      a
                    )
                case Constr(name, data, args, tp, a) =>
                    Constr(name, data, args.map(rewrite(_, i, shadowed, farRefs)), tp, a)
                case Cast(expr, tp, a) => Cast(rewriteExpr(expr, i, shadowed, farRefs), tp, a)
                case _: Builtin | _: Error | _: Const => sir

        def recFlagsFor(letName: String, rhs: SIR): SIR.LetFlags =
            if RemoveRecursivity.isRecursive(letName, rhs) then flags
            else flags.remove(SIR.LetFlags.Recursivity)

        // innermost: f1 under its original name
        val rhs1FarRefs = mutable.Set.empty[Int]
        val rhs1 = wrapWithFarRefLets(
          rewrite(bindings.head.value, 1, Set.empty, rhs1FarRefs),
          1,
          rhs1FarRefs
        )
        val body1FarRefs = mutable.Set.empty[Int]
        val body1 = wrapWithFarRefLets(rewrite(body, 1, Set.empty, body1FarRefs), 1, body1FarRefs)
        val innermost =
            Let(List(Binding(names(0), tps(0), rhs1)), body1, recFlagsFor(names(0), rhs1), anns)

        // wrap with f2p .. fNp, fNp outermost
        (2 to n).foldLeft(innermost: AnnotatedSIR) { (acc, i) =>
            val farRefs = mutable.Set.empty[Int]
            val rewritten =
                wrapWithFarRefLets(
                  rewrite(bindings(i - 1).value, i, Set.empty, farRefs),
                  i,
                  farRefs
                )
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
