package scalus.verify.lean

import scalus.compiler.sir.{SIR, SIRType}
import scalus.uplc.{Constant, DefaultFun}
import scalus.verify.{Prop, PropExpr}

/** Translates a [[Prop]] into a Lean proposition.
  *
  * This first version supports `Boolean` and `BigInt` (SIR `Boolean` and `Integer`). Other data
  * types and SIR operations fail explicitly until their Lean representation is defined.
  */
object LeanExporter {

    def apply(prop: Prop): String = renderProp(prop, Map.empty)

    private type Names = Map[String, String]

    private def renderProp(prop: Prop, names: Names): String = prop match
        case Prop.Bool(expr) => s"(${renderExpr(expr, names)} = true)"
        case Prop.Denotes(_) => unsupported("denotes")
        case Prop.Equal(left, right) =>
            requireSameSupportedType(left, right)
            s"(${renderExpr(left, names)} = ${renderExpr(right, names)})"
        case Prop.Call(fn, arg, result, total, body) =>
            val resultName = identifier(result.name)
            val resultType = renderType(result.tp)
            val call = s"(${identifier(fn.name)} ${renderExpr(arg, names)})"
            val renderedBody = renderProp(body, names.updated(result.name, resultName))
            if total then
                s"(∃ ($resultName : $resultType), $call = some $resultName ∧ $renderedBody)"
            else s"(∀ ($resultName : $resultType), $call = some $resultName → $renderedBody)"
        case Prop.Forall(ident, body) =>
            val name = identifier(ident.name)
            s"(∀ ($name : ${renderType(ident.tp)}), ${renderProp(body, names.updated(ident.name, name))})"
        case Prop.Exists(ident, None, body) =>
            val name = identifier(ident.name)
            s"(∃ ($name : ${renderType(ident.tp)}), ${renderProp(body, names.updated(ident.name, name))})"
        case Prop.Exists(ident, Some(witness), body) =>
            val name = identifier(ident.name)
            val renderedWitness = renderExpr(witness, names)
            val renderedBody = renderProp(body, names.updated(ident.name, name))
            s"(let $name : ${renderType(ident.tp)} := $renderedWitness; $renderedBody)"
        case Prop.And(left, right) =>
            s"(${renderProp(left, names)} ∧ ${renderProp(right, names)})"
        case Prop.Or(left, right) =>
            s"(${renderProp(left, names)} ∨ ${renderProp(right, names)})"
        case Prop.Implies(left, right) =>
            s"(${renderProp(left, names)} → ${renderProp(right, names)})"
        case Prop.Iff(left, right) =>
            s"(${renderProp(left, names)} ↔ ${renderProp(right, names)})"
        case Prop.Not(inner) => s"(¬ ${renderProp(inner, names)})"

    private def renderExpr(expr: PropExpr[?], names: Names): String = expr match
        case PropExpr.Ident(name, _, tp) =>
            renderType(tp)
            names.getOrElse(name, identifier(name))
        case PropExpr.SIRExpr(sir) => renderSir(sir, names)

    private def renderSir(sir: SIR, names: Names): String = sir match
        case SIR.Var(name, tp, _) =>
            renderType(tp)
            names.getOrElse(name, identifier(name))
        case SIR.ExternalVar(_, name, tp, _) =>
            renderType(tp)
            identifier(name)
        case SIR.Const(Constant.Integer(value), SIRType.Integer, _) =>
            if value < 0 then s"($value)" else value.toString
        case SIR.Const(Constant.Bool(value), SIRType.Boolean, _) => value.toString
        case constant: SIR.Const => unsupported(s"constant of type ${constant.tp.show}")
        case SIR.And(left, right, _) =>
            s"(${renderSir(left, names)} && ${renderSir(right, names)})"
        case SIR.Or(left, right, _) =>
            s"(${renderSir(left, names)} || ${renderSir(right, names)})"
        case SIR.Not(inner, _) => s"(!${renderSir(inner, names)})"
        case SIR.IfThenElse(condition, ifTrue, ifFalse, tp, _) =>
            renderType(tp)
            s"(if ${renderSir(condition, names)} then ${renderSir(ifTrue, names)} else ${renderSir(ifFalse, names)})"
        case SIR.Let(bindings, body, flags, _) =>
            if SIR.LetFlags.isRec(flags) then unsupported("recursive let")
            bindings.foldRight(
              renderSir(body, names ++ bindings.map(b => b.name -> identifier(b.name)))
            ) { case (binding, renderedBody) =>
                val name = identifier(binding.name)
                s"(let $name : ${renderType(binding.tp)} := ${renderSir(binding.value, names)}; $renderedBody)"
            }
        case SIR.LamAbs(param, body, typeParams, _) =>
            if typeParams.nonEmpty then unsupported("polymorphic lambda")
            val name = identifier(param.name)
            s"(fun ($name : ${renderType(param.tp)}) => ${renderSir(body, names.updated(param.name, name))})"
        case application: SIR.Apply     => renderApplication(application, names)
        case SIR.Builtin(builtin, _, _) => renderBuiltin(builtin)
        case SIR.Cast(expr, tp, _) =>
            renderType(tp)
            renderSir(expr, names)
        case SIR.Decl(_, term) => renderSir(term, names)
        case _: SIR.Error      => unsupported("SIR error")
        case _: SIR.Select     => unsupported("field selection")
        case _: SIR.Constr     => unsupported("constructor")
        case _: SIR.Match      => unsupported("pattern match")

    private def renderApplication(application: SIR.Apply, names: Names): String = {
        val (function, arguments) = flattenApplication(application)
        function match
            case SIR.Builtin(builtin, _, _) => renderBuiltinCall(builtin, arguments, names)
            case other =>
                val rendered = renderSir(other, names) +: arguments.map(renderSir(_, names))
                rendered.mkString("(", " ", ")")
    }

    private def flattenApplication(sir: SIR.Apply): (SIR, List[SIR]) = {
        @annotation.tailrec
        def loop(current: SIR, arguments: List[SIR]): (SIR, List[SIR]) = current match
            case SIR.Apply(function, argument, _, _) => loop(function, argument :: arguments)
            case other                               => other -> arguments
        loop(sir, Nil)
    }

    private def renderBuiltinCall(
        builtin: DefaultFun,
        arguments: List[SIR],
        names: Names
    ): String = {
        def binary(operator: String): String = arguments match
            case List(left, right) =>
                s"(${renderSir(left, names)} $operator ${renderSir(right, names)})"
            case _ => unsupported(s"partially applied $builtin")

        builtin match
            case DefaultFun.AddInteger      => binary("+")
            case DefaultFun.SubtractInteger => binary("-")
            case DefaultFun.MultiplyInteger => binary("*")
            case DefaultFun.EqualsInteger =>
                arguments match
                    case List(left, right) =>
                        s"(decide (${renderSir(left, names)} = ${renderSir(right, names)}))"
                    case _ => unsupported(s"partially applied $builtin")
            case DefaultFun.LessThanInteger =>
                arguments match
                    case List(left, right) =>
                        s"(decide (${renderSir(left, names)} < ${renderSir(right, names)}))"
                    case _ => unsupported(s"partially applied $builtin")
            case DefaultFun.LessThanEqualsInteger =>
                arguments match
                    case List(left, right) =>
                        s"(decide (${renderSir(left, names)} ≤ ${renderSir(right, names)}))"
                    case _ => unsupported(s"partially applied $builtin")
            case DefaultFun.IfThenElse =>
                arguments match
                    case List(condition, ifTrue, ifFalse) =>
                        s"(if ${renderSir(condition, names)} then ${renderSir(ifTrue, names)} else ${renderSir(ifFalse, names)})"
                    case _ => unsupported("partially applied IfThenElse")
            case other => unsupported(s"builtin $other")
    }

    private def renderBuiltin(builtin: DefaultFun): String = builtin match
        case DefaultFun.AddInteger            => "(fun x y => x + y)"
        case DefaultFun.SubtractInteger       => "(fun x y => x - y)"
        case DefaultFun.MultiplyInteger       => "(fun x y => x * y)"
        case DefaultFun.EqualsInteger         => "(fun x y => decide (x = y))"
        case DefaultFun.LessThanInteger       => "(fun x y => decide (x < y))"
        case DefaultFun.LessThanEqualsInteger => "(fun x y => decide (x ≤ y))"
        case other                            => unsupported(s"builtin $other")

    private def renderType(tp: SIRType): String = tp match
        case SIRType.Boolean             => "Bool"
        case SIRType.Integer             => "Integer"
        case SIRType.Fun(in, out)        => s"(${renderType(in)} → ${renderType(out)})"
        case SIRType.TypeLambda(_, body) => renderType(body)
        case other                       => unsupported(s"data type ${other.show}")

    private def requireSameSupportedType(left: PropExpr[?], right: PropExpr[?]): Unit = {
        val leftType = expressionType(left)
        val rightType = expressionType(right)
        require(leftType == rightType, s"cannot compare ${leftType.show} with ${rightType.show}")
        renderType(leftType)
    }

    private def expressionType(expr: PropExpr[?]): SIRType = expr match
        case PropExpr.Ident(_, _, tp) => tp
        case PropExpr.SIRExpr(sir)    => sir.tp

    private def identifier(name: String): String = {
        val replaced = name.map(c => if c.isLetterOrDigit || c == '_' then c else '_')
        val nonEmpty = if replaced.isEmpty then "value" else replaced
        val prefixed = if nonEmpty.head.isDigit then s"v_$nonEmpty" else nonEmpty
        if leanKeywords.contains(prefixed) then s"${prefixed}_" else prefixed
    }

    private val leanKeywords = Set(
      "by",
      "def",
      "else",
      "end",
      "exists",
      "false",
      "forall",
      "fun",
      "if",
      "in",
      "let",
      "match",
      "namespace",
      "open",
      "then",
      "theorem",
      "true"
    )

    private def unsupported(what: String): Nothing =
        throw new UnsupportedOperationException(s"Lean export does not support $what")
}
