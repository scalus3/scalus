package scalus.verify.uplcblaster

import scalus.uplc.{Constant, DefaultFun, NamedDeBruijn, Term}
import scalus.uplc.Term.{lam, vr}
import scalus.uplc.internal.TermTraversal.mapChildren

/** A program's `dropList k xs` of a constant `k`, written as `k` steps of a list's tail.
  *
  * Blaster's optimizer resolves `tailList` of a list it has taken apart, but not Lean's
  * `List.drop`, which is how its model of the CEK machine reads `dropList`: it does not unfold a
  * recursive function. Two programs that read the same field of a `Data` value, one with `tailList`
  * and one with `dropList`, as code compiled from a field selection does, then take the list apart
  * twice, as two lists, and what one of them established, as a guard of a list's length does, holds
  * for the other one no more. Written as steps, every read is one of `tailList`. A step is the list
  * itself where it is empty, and its tail otherwise: past the end `dropList` gives the empty list.
  *
  * The tactic writes the tests a guard checks so ([[LimitGuards]]), and a proof is then of the
  * program written so: the rewriting is trusted, as the tactic's lowering is. The two agree on the
  * Scalus CEK (`DropListsTest`), and a counterexample is replayed on the program written so.
  */
private[uplcblaster] object DropLists {

    /** The most steps a `dropList` is written as: more than the fields of any type of the ledger.
      */
    val maxSteps = 64

    /** `term` with each `dropList` of a constant from 0 to [[maxSteps]] written as its steps. The
      * builtin is found also where the program binds it to a variable, as it does a builtin it uses
      * more than once: `(lam x body) builtin`, or a field of `case (constr 0 builtins)`.
      */
    def unrolled(term: Term): Term = new Unrolling().apply(term, Set.empty)

    private final class Unrolling {

        /** The steps written so far, which number the variables of the next one. */
        private var written = 0

        def apply(term: Term, aliases: Set[String]): Term = term match
            case Term.Apply(Term.Apply(function, Term.Const(Constant.Integer(k), _), _), list, _)
                if isDropList(function, aliases) && k >= 0 && k <= maxSteps =>
                (0 until k.toInt).foldLeft(apply(list, aliases))((dropped, _) => step(dropped))
            case Term.Apply(Term.LamAbs(name, body, binding), argument, annotation) =>
                val inBody =
                    if isDropList(argument, aliases) then aliases + name else aliases - name
                Term.Apply(
                  Term.LamAbs(name, apply(body, inBody), binding),
                  apply(argument, aliases),
                  annotation
                )
            case Term.Case(constr @ Term.Constr(_, fields, _), List(branch), annotation) =>
                Term.Case(apply(constr, aliases), List(bound(branch, fields, aliases)), annotation)
            case Term.LamAbs(name, body, annotation) =>
                Term.LamAbs(name, apply(body, aliases - name), annotation)
            case _ => term.mapChildren(apply(_, aliases))

        /** `branch` of a `case` over a constructor of `fields`, whose lambdas bind them in order.
          */
        private def bound(branch: Term, fields: List[Term], aliases: Set[String]): Term =
            (branch, fields) match
                case (Term.LamAbs(name, body, annotation), field :: rest) =>
                    val inBody =
                        if isDropList(field, aliases) then aliases + name else aliases - name
                    Term.LamAbs(name, bound(body, rest, inBody), annotation)
                case _ => apply(branch, aliases)

        /** `list` where it is empty, and its tail otherwise. */
        private def step(list: Term): Term = {
            written += 1
            val name = s"scalus_dropped_$written"
            val tail = forced(DefaultFun.TailList, 1) $ vr(name)
            lam(name)(!(forced(DefaultFun.ChooseList, 2) $ vr(name) $ ~vr(name) $ ~tail)) $ list
        }
    }

    /** The builtin `function` forced `forces` times, as many as its type parameters. */
    private[uplcblaster] def forced(function: DefaultFun, forces: Int): Term =
        (0 until forces).foldLeft(Term.Builtin(function): Term)((term, _) => !term)

    /** Whether `term` is the builtin `dropList`, forced, or a variable bound to it. */
    private def isDropList(term: Term, aliases: Set[String]): Boolean = term match
        case Term.Force(Term.Builtin(DefaultFun.DropList, _), _) => true
        case Term.Var(NamedDeBruijn(name, _), _)                 => aliases.contains(name)
        case _                                                   => false
}
