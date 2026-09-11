package scalus.uplc.transform

import scalus.uplc.{DeBruijn, Term}
import scalus.uplc.Term.*
import scala.collection.mutable
import scala.util.control.NoStackTrace

/** Independent, test-only translation validator, following Plutus's UCSE relation and ∈↓.
  *
  * Uses de Bruijn indices, not the optimizer's names, paths or grouping. A rewrite is accepted by
  * congruence, or by substituting an introduced strict let back into its body. This checks the CSE
  * relation; it does not prove trace preservation or equal execution budgets.
  *
  * Reference: plutus 1.63.0.0, plutus-metatheory/src/VerifiedCompilation/UCSE.lagda.md and
  * plutus-metatheory/src/Untyped/Strictness.lagda.md.
  */
private[transform] object CseTranslationValidation {
    def validate(before: Term, after: Term): Boolean = {
        try new Validator().relates(DeBruijn.deBruijnTerm(before), DeBruijn.deBruijnTerm(after))
        catch case _: ValidationLimit => false
    }

    private class ValidationLimit extends RuntimeException with NoStackTrace

    private class Validator {
        // Invalid output may contain self-application. Back-substitution is not necessarily
        // terminating, so reject cycles and bound the work instead of hanging the test runner.
        private val visiting = mutable.HashSet.empty[(Term, Term)]
        private var fuel = 100_000
        private def tick(): Unit = {
            fuel -= 1
            if fuel < 0 then throw new ValidationLimit()
        }

        /** No Case-branch, Delay-body or unapplied-lambda rule. */
        private def isStrictIn(index: Int, t: Term): Boolean = t match
            case Var(n, _) => n.index == index
            case Apply(LamAbs(_, body, _), arg, _) =>
                isStrictIn(index + 1, body) || isStrictIn(index, arg)
            case Apply(f, arg, _)      => isStrictIn(index, f) || isStrictIn(index, arg)
            case Force(body, _)        => isStrictIn(index, body)
            case Constr(_, args, _)    => args.exists(isStrictIn(index, _))
            case Case(scrutinee, _, _) => isStrictIn(index, scrutinee)
            case _                     => false

        /** Capture-avoiding substitution of the outermost binder, removing that binder. */
        private def substitute(body: Term, rhs: Term): Term = {
            def shift(t: Term, amount: Int, depth: Int): Term = t match
                case Var(n, ann) if n.index > depth => Var(n.copy(index = n.index + amount), ann)
                case _ => descend(t, depth, (child, d) => shift(child, amount, d))

            def go(t: Term, depth: Int): Term = t match
                case Var(n, _) if n.index == depth + 1  => shift(rhs, depth, 0)
                case Var(n, ann) if n.index > depth + 1 => Var(n.copy(index = n.index - 1), ann)
                case _                                  => descend(t, depth, go)

            go(body, 0)
        }

        private def descend(t: Term, depth: Int, f: (Term, Int) => Term): Term = {
            tick()
            t match
                case LamAbs(n, body, ann)   => LamAbs(n, f(body, depth + 1), ann)
                case Apply(fn, arg, ann)    => Apply(f(fn, depth), f(arg, depth), ann)
                case Force(body, ann)       => Force(f(body, depth), ann)
                case Delay(body, ann)       => Delay(f(body, depth), ann)
                case Constr(tag, args, ann) => Constr(tag, args.map(f(_, depth)), ann)
                case Case(s, branches, ann) => Case(f(s, depth), branches.map(f(_, depth)), ann)
                case _                      => t
        }

        def relates(before: Term, after: Term): Boolean = {
            tick()
            val pair = (before, after)
            if visiting.contains(pair) then return false
            visiting += pair
            val congruent = (before, after) match
                case (Var(a, _), Var(b, _)) =>
                    if a.index > 0 || b.index > 0 then a.index == b.index
                    else a.name == b.name
                case (Const(a, _), Const(b, _))         => a == b
                case (Builtin(a, _), Builtin(b, _))     => a == b
                case (_: Error, _: Error)               => true
                case (LamAbs(_, a, _), LamAbs(_, b, _)) => relates(a, b)
                case (Apply(f, a, _), Apply(g, b, _))   => relates(f, g) && relates(a, b)
                case (Force(a, _), Force(b, _))         => relates(a, b)
                case (Delay(a, _), Delay(b, _))         => relates(a, b)
                case (Constr(i, as, _), Constr(j, bs, _)) =>
                    i == j && as.size == bs.size && as.zip(bs).forall(relates)
                case (Case(s, as, _), Case(t, bs, _)) =>
                    as.size == bs.size && relates(s, t) && as.zip(bs).forall(relates)
                case _ => false

            val result = congruent || (after match
                case Apply(LamAbs(_, body, _), rhs, _) if isStrictIn(1, body) =>
                    relates(before, substitute(body, rhs))
                case _ => false)
            visiting -= pair
            result
        }
    }
}
