package scalus.compiler.sir.transform

import scalus.compiler.sir.{AnnotatedSIR, Binding, RemoveRecursivity, SIR, SIRPosition, SIRType}
import scalus.compiler.sir.linking.{SIRLinker, Wrappers}
import scalus.uplc.Constant

import scala.collection.mutable
import scala.util.boundary

/** Removes the specification clauses of [[scalus.cardano.onchain.plutus.prelude.Spec]] from SIR,
  * before it is lowered.
  *
  * A clause is a call of a function of the `Spec` module, which the compiler plugin leaves in the
  * SIR as it leaves any call, so that a verifier can read it there. It is not part of the script:
  *   - `Spec.expects(c)` and `Spec.ensures(c)` are removed, with their conditions, which are not
  *     evaluated;
  *   - `body.ensuring(r => c)` becomes `body`;
  *   - the definitions of the `Spec` functions, which linking put around the code, are dropped, and
  *     so is the definition of a function that only the clauses used: a function written for the
  *     specification is no part of the script either.
  *
  * The definitions that are left are nested again in the order the code alone gives. What comes out
  * is the SIR of the same code written without the clauses, so its script has the same bytes. Code
  * without clauses is returned as it is.
  */
object EraseSpecifications {

    /** The name of `Spec.expects` in SIR. */
    val Expects: String = "scalus.cardano.onchain.plutus.prelude.Spec$.expects"

    /** The name of `Spec.ensuring` in SIR. */
    val Ensuring: String = "scalus.cardano.onchain.plutus.prelude.Spec$.ensuring"

    /** The name in SIR of the function behind `Spec.ensures`, applied to `_ => condition`. */
    val Ensures: String = "scalus.cardano.onchain.plutus.prelude.Spec$.holdsOnReturn"

    private val functions = Set(Expects, Ensuring, Ensures)

    def apply(sir: SIR): SIR =
        if refersTo(sir, functions) then reordered(new Pass().erased(sir)) else sir

    /** The definitions around a program, nested again in the order linking gives them.
      *
      * Linking nests the definitions a program uses in the order it first meets them, and a clause
      * at the head of a function meets the functions it names before the code does. With the
      * clauses gone, the order has to be the one the code alone gives, or the script's bytes would
      * depend on its specification. So the program as it is now is walked as [[SIRLinker]] walks
      * one, and the definitions the walk reaches are nested by the linker's own rule. A definition
      * it does not reach was used by a clause only, and is left out.
      *
      * A program applied to its parameters, as `PlutusV3.apply` leaves a parameterized script, has
      * its definitions inside the application, around the function. They are nested again there.
      *
      * A program without clauses comes out as it is, which `SpecTest` checks.
      */
    private[compiler] def reordered(sir: SIR): SIR = sir match
        case SIR.Decl(data, term) => SIR.Decl(data, reordered(term))
        case SIR.Apply(f, arg, tp, anns) =>
            SIR.Apply(undeclared(reordered(f)), undeclared(reordered(arg)), tp, anns)
        case SIR.Let(bindings, _, _, anns) if bindings.forall(SIRLinker.isDefinition) =>
            val (definitions, root) = Wrappers.definitions(sir)
            nested(definitions.bindings, root, anns.pos)
        case other => other

    /** The ones among `definitions` that `root` reaches, around it, in the linker's order and by
      * its rule.
      */
    private def nested(definitions: List[Binding], root: SIR, pos: SIRPosition): SIR = {
        val byName = definitions.map(binding => binding.name -> binding).toMap
        // The order in which the linker finishes each definition: after everything its own code
        // refers to, on the walk the linker makes of the code.
        val started = mutable.Set.empty[String]
        val finished = mutable.ListBuffer.empty[Binding]
        def walk(code: SIR): Unit = SIRLinker.foreachLinked(code) {
            case SIR.ExternalVar(_, name, _, _) =>
                for definition <- byName.get(name) if started.add(name) do
                    walk(definition.value)
                    finished += definition
            case _ => ()
        }
        walk(root)
        val linked = finished.toList.map(binding =>
            SIRLinker.SIRLinkedBinding(
              binding.name,
              SIR.LetFlags.Recursivity,
              binding.value,
              Some(binding.tp)
            )
        )
        RemoveRecursivity(
          SIRLinker.nest(root, linked, pos, message => throw new IllegalStateException(message))
        )
    }

    /** One side of an application, nested again: no data declaration is around it, as none was
      * before.
      */
    private def undeclared(sir: SIR): AnnotatedSIR = sir match
        case annotated: AnnotatedSIR => annotated
        case SIR.Decl(data, _) =>
            throw new IllegalStateException(s"the declaration of ${data.name} is in an application")

    /** `Spec.expects(condition)` or `Spec.ensures(condition)`: a clause written as a statement, one
      * application of its function.
      */
    private def isStatement(sir: SIR): Boolean = sir match
        case SIR.Apply(SIR.ExternalVar(_, name, _, _), _, _, _) =>
            name == Expects || name == Ensures
        case SIR.Cast(term, _, _) => isStatement(term)
        case _                    => false

    /** A function, which can be dropped where nothing uses it: its definition evaluates nothing. */
    private def isFunction(sir: SIR): Boolean = sir match
        case _: SIR.LamAbs     => true
        case SIR.Decl(_, term) => isFunction(term)
        case _                 => false

    /** One run over a program. It remembers the names the removed code used: only a definition
      * among them can have become unused.
      */
    private final class Pass {
        private val specified = mutable.Set.empty[String]

        private def removed(code: SIR): Unit = specified ++= names(code)

        def erased(sir: SIR): SIR = sir match
            case annotated: AnnotatedSIR => erase(annotated)
            case SIR.Decl(data, term)    => SIR.Decl(data, erased(term))

        private def erase(sir: AnnotatedSIR): AnnotatedSIR = sir match {
            case SIR.Apply(SIR.ExternalVar(_, name, _, _), condition, _, anns)
                if name == Expects || name == Ensures =>
                removed(condition)
                SIR.Const(Constant.Unit, SIRType.Unit, anns)
            // body.ensuring(condition) is Spec.ensuring(body)(condition)
            case SIR.Apply(
                  SIR.Apply(SIR.ExternalVar(_, Ensuring, _, _), body, _, _),
                  condition,
                  _,
                  _
                ) =>
                removed(condition)
                erase(body)
            case SIR.Let(bindings, body, flags, anns) =>
                val erasedBody = erased(body)
                // Later bindings first. A clause's binding is dropped where nothing after it uses
                // it, as a clause written as a statement never is. So is the definition of a
                // function that only removed code used; what that function used is then looked
                // at in turn, further out. The bindings of a recursive `let` see each other, so
                // there one is also used where a binding before it refers to it.
                val recursive = SIR.LetFlags.isRec(flags)
                val (kept, _) = bindings.zipWithIndex.foldRight((List.empty[Binding], erasedBody)) {
                    case ((binding, index), (after, rest)) =>
                        val scope: SIR =
                            if after.isEmpty then rest else SIR.Let(after, rest, flags, anns)
                        def usedBefore = recursive && bindings
                            .take(index)
                            .exists(before => isFree(binding.name, before.value))
                        def unused = !isFree(binding.name, scope) && !usedBefore
                        val statement = isStatement(binding.value) && unused
                        val definition = functions.contains(binding.name) ||
                            (specified.contains(binding.name) &&
                                isFunction(binding.value) && unused)
                        if statement || definition then
                            removed(binding.value)
                            (after, rest)
                        else (binding.copy(value = erased(binding.value)) :: after, rest)
                }
                (kept, erasedBody) match
                    case (Nil, value: AnnotatedSIR) => value
                    case _                          => SIR.Let(kept, erasedBody, flags, anns)
            case SIR.LamAbs(param, term, typeParams, anns) =>
                SIR.LamAbs(param, erased(term), typeParams, anns)
            case SIR.Apply(f, arg, tp, anns) => SIR.Apply(erase(f), erase(arg), tp, anns)
            case SIR.Select(scrutinee, field, tp, anns) =>
                SIR.Select(erased(scrutinee), field, tp, anns)
            case SIR.And(a, b, anns) => SIR.And(erase(a), erase(b), anns)
            case SIR.Or(a, b, anns)  => SIR.Or(erase(a), erase(b), anns)
            case SIR.Not(a, anns)    => SIR.Not(erase(a), anns)
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                SIR.IfThenElse(erase(cond), erase(t), erase(f), tp, anns)
            case SIR.Error(msg, anns, cause) => SIR.Error(erase(msg), anns, cause)
            case SIR.Constr(name, data, args, tp, anns) =>
                SIR.Constr(name, data, args.map(erased), tp, anns)
            case SIR.Match(scrutinee, cases, tp, anns) =>
                SIR.Match(
                  erase(scrutinee),
                  cases.map(caze => caze.copy(body = erased(caze.body))),
                  tp,
                  anns
                )
            case SIR.Cast(term, tp, anns) => SIR.Cast(erase(term), tp, anns)
            case _: SIR.Var | _: SIR.ExternalVar | _: SIR.Const | _: SIR.Builtin => sir
        }
    }

    /** The names `sir` refers to, bound in it or not. */
    private def names(sir: SIR): Set[String] =
        SIR.accumulate[Set[String]](
          sir,
          Set.empty,
          Set.empty,
          (node, _, found) =>
              node match
                  case SIR.Var(name, _, _)            => found + name
                  case SIR.ExternalVar(_, name, _, _) => found + name
                  case _                              => found
        )

    /** Whether `sir` refers to one of `names`, bound in it or not. It stops at the first. */
    private def refersTo(sir: SIR, names: Set[String]): Boolean = boundary {
        SIR.accumulate[Unit](
          sir,
          (),
          Set.empty,
          (node, _, _) =>
              node match
                  case SIR.Var(name, _, _) if names(name)            => boundary.break(true)
                  case SIR.ExternalVar(_, name, _, _) if names(name) => boundary.break(true)
                  case _                                             => ()
        )
        false
    }

    /** Whether the variable `name` occurs free in `sir`. A binding of a recursive `let` is taken to
      * be free in its own value, which errs towards keeping it.
      */
    private def isFree(name: String, sir: SIR): Boolean = boundary {
        SIR.accumulate[Unit](
          sir,
          (),
          Set.empty,
          (node, bound, _) =>
              node match
                  case SIR.Var(`name`, _, _) if !bound(name)            => boundary.break(true)
                  case SIR.ExternalVar(_, `name`, _, _) if !bound(name) => boundary.break(true)
                  case _                                                => ()
        )
        false
    }
}
