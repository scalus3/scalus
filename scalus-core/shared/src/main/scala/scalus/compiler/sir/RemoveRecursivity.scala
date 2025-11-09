package scalus.compiler.sir

import scalus.compiler.sir.SIR.*
import scalus.compiler.sir.{AnnotatedSIR, Binding, SIR}

object RemoveRecursivity:

    /** Makes a let expression non-recursive if its bindings are non-recursive */
    def apply(sir: SIR): SIR = removeRecursivity(sir)

    /** Makes a let expression non-recursive if its bindings are non-recursive */
    def removeRecursivity(sir: SIR): SIR =
        sir match
            case expr: AnnotatedSIR =>
                removeRecursivityInExpr(expr)
            case Decl(data, term) => Decl(data, removeRecursivity(term))

    /** Makes a let expression non-recursive if its bindings are non-recursive */
    def removeRecursivityInExpr(sir: AnnotatedSIR): AnnotatedSIR =
        sir match
            case Let(List(Binding(name, tp, binding)), body, flags, anns)
                if flags.isRec && !isRecursive(name, binding) =>
                removeRecursivityInExpr(
                  Let(
                    List(Binding(name, tp, binding)),
                    body,
                    flags.remove(LetFlags.Recursivity),
                    anns
                  )
                )
            case Let(bindings, body, flags, anns) =>
                Let(
                  bindings.map { case Binding(name, tp, rhs) =>
                      Binding(name, tp, removeRecursivity(rhs))
                  },
                  removeRecursivity(body),
                  flags,
                  anns
                )
            case LamAbs(name, term, tps, anns) => LamAbs(name, removeRecursivity(term), tps, anns)
            case Apply(f, arg, tp, anns) =>
                Apply(removeRecursivityInExpr(f), removeRecursivityInExpr(arg), tp, anns)
            case Select(s, field, tp, anns) => Select(removeRecursivity(s), field, tp, anns)
            case IfThenElse(cond, t, f, tp, anns) =>
                IfThenElse(
                  removeRecursivityInExpr(cond),
                  removeRecursivityInExpr(t),
                  removeRecursivityInExpr(f),
                  tp,
                  anns
                )
            case And(lhs, rhs, anns) =>
                And(removeRecursivityInExpr(lhs), removeRecursivityInExpr(rhs), anns)
            case Or(lhs, rhs, anns) =>
                Or(removeRecursivityInExpr(lhs), removeRecursivityInExpr(rhs), anns)
            case Not(term, anns) => Not(removeRecursivityInExpr(term), anns)
            case Match(scrutinee, cases, tp, anns) =>
                Match(
                  removeRecursivityInExpr(scrutinee),
                  cases.map {
                      case SIR.Case(Pattern.Constr(constr, bindings, typeBindings), body, anns) =>
                          Case(
                            Pattern.Constr(constr, bindings, typeBindings),
                            removeRecursivity(body),
                            anns
                          )
                      case SIR.Case(Pattern.Const(value), body, anns) =>
                          Case(Pattern.Const(value), removeRecursivity(body), anns)
                      case SIR.Case(Pattern.Wildcard, body, anns) =>
                          Case(Pattern.Wildcard, removeRecursivity(body), anns)
                  },
                  tp,
                  anns
                )
            case Constr(name, data, args, tp, anns) =>
                Constr(name, data, args.map(removeRecursivity), tp, anns)
            case Cast(expr, tp, anns) =>
                Cast(removeRecursivityInExpr(expr), tp, anns)
            case _: Builtin | _: Error | _: Var | _: ExternalVar | _: Const => sir

    def isRecursive(name: String, term: SIR, env: List[String] = Nil): Boolean =
        term match
            case Var(n, tp, _)            => n == name && !env.contains(n)
            case ExternalVar(_, n, tp, _) => n == name && !env.contains(n)
            case Let(bindings, body, flags, _) =>
                val newEnv = bindings.map(_.name) ++ env
                bindings.exists(b => isRecursive(name, b.value, newEnv))
                || isRecursive(name, body, newEnv)
            case LamAbs(n, t, _, _) => isRecursive(name, t, n.name :: env)
            case Apply(f, a, tp, _) => isRecursive(name, f, env) || isRecursive(name, a, env)
            case Select(s, _, _, _) => isRecursive(name, s, env)
            case And(a, b, _)       => isRecursive(name, a, env) || isRecursive(name, b, env)
            case Or(a, b, _)        => isRecursive(name, a, env) || isRecursive(name, b, env)
            case Not(a, _)          => isRecursive(name, a, env)
            case IfThenElse(c, t, f, tp, _) =>
                isRecursive(name, c, env) || isRecursive(name, t, env) || isRecursive(name, f, env)
            case Decl(_, t) => isRecursive(name, t, env)
            case Constr(_, _, args, tp, _) =>
                args.exists(a => isRecursive(name, a, env))
            case Match(scrutinee, cases, tp, _) =>
                isRecursive(name, scrutinee, env) || cases.exists {
                    case SIR.Case(Pattern.Constr(_, bindings, typeBindings), body, anns) =>
                        isRecursive(name, body, bindings ++ env)
                    case SIR.Case(Pattern.Const(_), body, anns) =>
                        isRecursive(name, body, env)
                    case SIR.Case(Pattern.Wildcard, body, anns) =>
                        isRecursive(name, body, env)
                }
            case Cast(expr, tp, anns) =>
                isRecursive(name, expr, env)
            case _: Builtin | _: Error | _: Const => false
