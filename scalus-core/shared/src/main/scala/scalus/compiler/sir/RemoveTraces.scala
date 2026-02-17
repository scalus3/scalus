package scalus.compiler.sir

import scalus.compiler.sir.SIR.Case
import scalus.uplc.{Constant, DefaultFun}

/** Removes fully-applied `Trace` builtin calls from SIR, replacing them with their value argument.
  *
  * This is used for release builds to strip `log()` calls and their message computations (string
  * concatenation, show calls, etc.) from the script, reducing size and execution cost.
  *
  * The transformation also cleans up dead let bindings that become trivial `()` after trace removal
  * (the common pattern from `log("msg")` → `let _ = trace(msg)(()) in body` → `let _ = () in body`
  * → `body`).
  */
object RemoveTraces {

    /** Transforms a SIR tree by removing all fully-applied Trace calls.
      *
      * @param sir
      *   the SIR tree to transform
      * @return
      *   the transformed SIR with trace calls removed
      */
    def transform(sir: SIR): SIR = sir match {
        case a: AnnotatedSIR      => transformAnnotated(a)
        case SIR.Decl(data, term) => SIR.Decl(data, transform(term))
    }

    private def transformAnnotated(sir: AnnotatedSIR): AnnotatedSIR = sir match {
        // Fully-applied Trace: Apply(Apply(Builtin(Trace), msg), value) → value
        case SIR.Apply(
              SIR.Apply(SIR.Builtin(DefaultFun.Trace, _, _), _, _, _),
              value,
              _,
              _
            ) =>
            transformAnnotated(value)

        case SIR.Let(bindings, body, flags, anns) =>
            val newBindings = bindings.map(transformBinding)
            val newBody = transform(body)
            // Remove dead bindings: those whose RHS is Const(Unit) and whose name is unused
            val (liveBindings, _) =
                newBindings.foldRight((List.empty[Binding], newBody)) {
                    case (b, (acc, bodyAndTail)) =>
                        val restSir = acc match {
                            case Nil => bodyAndTail
                            case _ =>
                                SIR.Let(acc, bodyAndTail, flags, anns)
                        }
                        b.value match {
                            case SIR.Const(Constant.Unit, _, _) if !containsVar(restSir, b.name) =>
                                (acc, bodyAndTail)
                            case _ => (b :: acc, bodyAndTail)
                        }
                }
            (liveBindings, newBody) match {
                case (Nil, a: AnnotatedSIR) => a
                case _                      => SIR.Let(liveBindings, newBody, flags, anns)
            }

        case SIR.LamAbs(param, term, typeParams, anns) =>
            SIR.LamAbs(param, transform(term), typeParams, anns)

        case SIR.Apply(f, arg, tp, anns) =>
            SIR.Apply(transformAnnotated(f), transformAnnotated(arg), tp, anns)

        case SIR.And(a, b, anns) =>
            SIR.And(transformAnnotated(a), transformAnnotated(b), anns)

        case SIR.Or(a, b, anns) =>
            SIR.Or(transformAnnotated(a), transformAnnotated(b), anns)

        case SIR.Not(a, anns) =>
            SIR.Not(transformAnnotated(a), anns)

        case SIR.IfThenElse(cond, t, f, tp, anns) =>
            SIR.IfThenElse(
              transformAnnotated(cond),
              transformAnnotated(t),
              transformAnnotated(f),
              tp,
              anns
            )

        case SIR.Error(msg, anns, cause) =>
            SIR.Error(transformAnnotated(msg), anns, cause)

        case SIR.Constr(name, data, args, tp, anns) =>
            SIR.Constr(name, data, args.map(transform), tp, anns)

        case SIR.Match(scrutinee, cases, tp, anns) =>
            SIR.Match(transformAnnotated(scrutinee), cases.map(transformCase), tp, anns)

        case SIR.Select(scrutinee, field, tp, anns) =>
            SIR.Select(transform(scrutinee), field, tp, anns)

        case SIR.Cast(term, tp, anns) =>
            SIR.Cast(transformAnnotated(term), tp, anns)

        case _: SIR.Var | _: SIR.ExternalVar | _: SIR.Const | _: SIR.Builtin => sir
    }

    private def transformBinding(binding: Binding): Binding =
        binding.copy(value = transform(binding.value))

    private def transformCase(cse: Case): Case =
        cse.copy(body = transform(cse.body))

    /** Checks whether a variable name appears free in a SIR tree. */
    private def containsVar(sir: SIR, name: String): Boolean = sir match {
        case SIR.Decl(_, term) => containsVar(term, name)
        case a: AnnotatedSIR   => containsVarAnnotated(a, name)
    }

    private def containsVarAnnotated(sir: AnnotatedSIR, name: String): Boolean = sir match {
        case SIR.Var(n, _, _)            => n == name
        case SIR.ExternalVar(_, n, _, _) => n == name
        case SIR.Let(bindings, body, flags, _) =>
            val (found, shadowed) = bindings.foldLeft((false, false)) {
                case ((true, _), _) => (true, true) // already found
                case ((false, shadowed), b) =>
                    if !shadowed && containsVar(b.value, name) then (true, true)
                    else if b.name == name then (false, true) // shadowed from here on
                    else (false, shadowed)
            }
            found || (!shadowed && containsVar(body, name))
        case SIR.LamAbs(param, term, _, _) =>
            if param.name == name then false else containsVar(term, name)
        case SIR.Apply(f, arg, _, _) =>
            containsVarAnnotated(f, name) || containsVarAnnotated(arg, name)
        case SIR.And(a, b, _) =>
            containsVarAnnotated(a, name) || containsVarAnnotated(b, name)
        case SIR.Or(a, b, _) =>
            containsVarAnnotated(a, name) || containsVarAnnotated(b, name)
        case SIR.Not(a, _) => containsVarAnnotated(a, name)
        case SIR.IfThenElse(cond, t, f, _, _) =>
            containsVarAnnotated(cond, name) ||
            containsVarAnnotated(t, name) ||
            containsVarAnnotated(f, name)
        case SIR.Error(msg, _, _)         => containsVarAnnotated(msg, name)
        case SIR.Constr(_, _, args, _, _) => args.exists(containsVar(_, name))
        case SIR.Match(scrutinee, cases, _, _) =>
            containsVarAnnotated(scrutinee, name) ||
            cases.exists { c =>
                val shadowed = c.pattern match {
                    case SIR.Pattern.Constr(_, bindings, _) => bindings.contains(name)
                    case _                                  => false
                }
                !shadowed && containsVar(c.body, name)
            }
        case SIR.Select(scrutinee, _, _, _) => containsVar(scrutinee, name)
        case SIR.Cast(term, _, _)           => containsVarAnnotated(term, name)
        case _: SIR.Const | _: SIR.Builtin  => false
    }
}
