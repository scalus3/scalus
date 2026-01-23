package scalus.compiler.sir

import org.typelevel.paiges
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import org.typelevel.paiges.Style.XTerm.Fg
import scalus.*
import scalus.uplc.builtin.Data
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni
import scalus.uplc.Term
import scalus.utils.Utils
import scalus.utils.{Pretty, Style as PrettyStyle}

/** Pretty printers.
  *
  *   - Use `pretty` to pretty print to a [[org.typelevel.paiges.Doc]].
  *   - Use `prettyXTerm` to pretty print to a [[org.typelevel.paiges.Doc]] with syntax highlighting
  *     for XTerm.
  *
  * For example:
  * {{{
  *  PrettyPrinter.prettyXTerm(sir).render(80)
  *  // renders a single line if possible
  *  PrettyPrinter.pretty(sir).flatten.render(120)
  * }}}
  */
object PrettyPrinter:
    /** @deprecated Use scalus.utils.Style instead */
    @deprecated("Use scalus.utils.Style instead", "0.14.2")
    type Style = scalus.utils.Style

    /** @deprecated Use scalus.utils.Style instead */
    @deprecated("Use scalus.utils.Style instead", "0.14.2")
    val Style = scalus.utils.Style

    def inParens(d: Doc): Doc = char('(') + d + char(')')
    def inBraces(d: Doc): Doc = char('{') + d + char('}')
    def inBrackets(d: Doc): Doc = char('[') + d + char(']')
    def inOptBrackets(d: Doc): Doc = if d.isEmpty then empty else inBrackets(d)

    def pretty(df: DefaultFun): Doc = text(Utils.lowerFirst(df.toString))

    def prettyValue(c: Constant, dataParens: Boolean = false): Doc =
        import Constant.*
        c match
            case Integer(value)    => str(value)
            case ByteString(value) => text("#" + value.toHex)
            case String(value)     => text("\"" + value + "\"")
            case Unit              => text("()")
            case Bool(value)       => text(if value then "True" else "False")
            case Constant.Data(value) =>
                if dataParens then inParens(pretty(value)) else pretty(value)
            case Pair(a, b) =>
                inParens(prettyValue(a) + text(", ") + prettyValue(b))
            case List(tpe, values) =>
                text("[") + intercalate(
                  text(", ") + space,
                  values.map(v => prettyValue(v))
                ) + text("]")
            case Constant.Array(tpe, values) =>
                text("[") + intercalate(
                  text(", ") + space,
                  values.map(v => prettyValue(v))
                ) + text("]")
            case BLS12_381_G1_Element(value) => text(s"0x${value.toCompressedByteString.toHex}")
            case BLS12_381_G2_Element(value) => text(s"0x${value.toCompressedByteString.toHex}")
            case BLS12_381_MlResult(_) =>
                throw new IllegalArgumentException("Cannot print to BLS12_381_MlResult")
            case Constant.BuiltinValue(value) => text(s"BuiltinValue(${value.toString})")

    def pretty(d: Data): Doc =
        d match
            case Data.I(value) => text("I") & str(value)
            case Data.B(value) => text("B") & text("#" + value.toHex)
            case Data.Constr(tag, args) =>
                text("Constr") & str(tag) &
                    text("[") + intercalate(
                      text(",") + space,
                      args.toScalaList.map(pretty)
                    ) + text("]")
            case Data.List(values) =>
                text("List") & text("[") + intercalate(
                  text(",") + space,
                  values.toScalaList.map(pretty)
                ) + text("]")
            case Data.Map(entries) =>
                text("Map") & text("[") + intercalate(
                  text(",") + space,
                  entries.toScalaList.map { case (k, v) =>
                      inParens(pretty(k) + text(",") & pretty(v))
                  }
                ) + text("]")

    def pretty(c: Constant): Doc = pretty(c.tpe) & prettyValue(c, true)

    def pretty(du: DefaultUni): Doc = du match
        case DefaultUni.Integer    => text("integer")
        case DefaultUni.ByteString => text("bytestring")
        case DefaultUni.String     => text("string")
        case DefaultUni.Unit       => text("unit")
        case DefaultUni.Bool       => text("bool")
        case DefaultUni.Apply(DefaultUni.ProtoList, arg) =>
            inParens(text("list") & pretty(arg))
        case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a), b) =>
            inParens(text("pair") & pretty(a) & pretty(b))
        case DefaultUni.Data                 => text("data")
        case DefaultUni.BLS12_381_G1_Element => text("bls12_381_G1_element")
        case DefaultUni.BLS12_381_G2_Element => text("bls12_381_G2_element")
        case _                               => sys.error(s"Unexpected default uni: $du")

    def pretty(sir: SIR, style: PrettyStyle): Doc =
        import SIR.*
        extension (d: Doc)
            def styled(s: paiges.Style): Doc = if style == PrettyStyle.XTerm then d.style(s) else d
        def kw(s: String): Doc = text(s).styled(Fg.colorCode(172))
        def ctr(s: String): Doc = text(s).styled(Fg.colorCode(27))
        def typ(s: Doc): Doc = s.styled(Fg.colorCode(55))
        def typedName(name: String, tp: SIRType): Doc = text(name) + char(':') & typ(pretty(tp))
        sir match
            case Decl(DataDecl(name, constructors, typeParams, anns), term) =>
                val prettyConstrs = constructors.map { constr =>
                    val params = constr.params match
                        case Nil => empty
                        case _ =>
                            intercalate(
                              text(",") + line,
                              constr.params.map(tb => typedName(tb.name, tb.tp))
                            )
                                .tightBracketBy(text("("), text(")"))
                    (ctr(constr.name) + params).aligned
                }
                val prettyGenDecl = typeParams match
                    case Nil   => empty
                    case other => intercalate(text(","), typeParams.map(x => text(x.name)))
                kw("data") & text(name) & prettyGenDecl &
                    (text("=") & intercalate(
                      line + text("|") + space,
                      prettyConstrs
                    )).grouped.aligned
                    / pretty(term, style)
            case Constr(name, _, args, _, _) =>
                ctr(name) + intercalate(
                  text(",") + line,
                  args.map(pretty(_, style))
                )
                    .tightBracketBy(text("("), text(")"))
            case Match(scrutinee, cases, tp, anns) =>
                val prettyCases =
                    stack(cases.map {
                        case SIR.Case(Pattern.Constr(constr, bindings, typeBindings), body, anns) =>
                            val typedConst = inOptBrackets(
                              intercalate(text(",") + space, typeBindings.map(pretty))
                            )
                            val params = bindings match
                                case Nil => empty
                                case _ =>
                                    intercalate(text(",") + line, bindings.map(text))
                                        .tightBracketBy(text("("), text(")"))
                            (kw("case") & ctr(constr.name) + typedConst + params & text(
                              "->"
                            ) + (line + pretty(body, style))
                                .nested(2)).grouped.aligned
                        case SIR.Case(Pattern.Const(value), body, _) =>
                            (kw("case") & pretty(value, style) & text("->") + (line + pretty(
                              body,
                              style
                            ))
                                .nested(2)).grouped.aligned
                        case SIR.Case(Pattern.Wildcard, body, _) =>
                            (kw("case") & text("_") & text("->") + (line + pretty(body, style))
                                .nested(2)).grouped.aligned
                    })
                ((kw("match") & pretty(scrutinee, style) & kw(
                  "with"
                )).grouped + (line + prettyCases)
                    .nested(
                      2
                    )).aligned

            case Var(name, tp, _)                     => text(name)
            case ExternalVar(moduleName, name, tp, _) => text(name)
            case Let(List(Binding(name, tp, body)), inExpr, flags, anns) if !flags.isRec =>
                val eqText = if flags.isLazy then text("=[lazy]") else text("=")
                pretty(body, style).bracketBy(
                  kw("let") & typedName(name, tp) & eqText,
                  kw("in")
                ) / pretty(inExpr, style)
            case Let(List(Binding(name, tp, body)), inExpr, flags, anns) if flags.isRec =>
                val (args, body1) = SirDSL.lamAbsToList(body)
                val prettyArgs = inParens(intercalate(text(",") + space, args.map(text)))
                val eqText = if flags.isLazy then text("=[lazy]") else text("=")
                val signatureLine =
                    (kw("fun") & text(name) + (prettyArgs + char(':') & typ(
                      pretty(tp)
                    ) & eqText).nested(2)).grouped
                (signatureLine + (line + pretty(body1, style))
                    .nested(4)
                    .grouped).grouped.aligned / kw(
                  "in"
                ) & pretty(inExpr, style)
            // TODO: support multiple bindings
            case Let(_, _, inExpr, _) => sys.error(s"Multiple bindings not supported: $sir")
            case LamAbs(name, term, typeParams, anns) =>
                val (args, body1) = SirDSL.lamAbsToList(sir)
                val prettyArgs = stack(args.map(text))
                val decl =
                    (text("{λ") + (line + prettyArgs & text("->")).nested(4)).grouped
                val declBody = ((decl + (line + pretty(body1, style)).nested(2)).grouped / text(
                  "}"
                )).grouped.aligned
                if typeParams.isEmpty then declBody
                else
                    (text("∀") + intercalate(text(",") + space, typeParams.map(p => text(p.name))) +
                        text(".") + line + declBody).grouped.aligned
            case a @ Apply(f, arg, tp, anns) =>
                val (t, args) = SirDSL.applyToList(a)
                val prettyArgs = args match
                    case List() => text("()")
                    case _ =>
                        intercalate(text(",") + line, args.map(pretty(_, style)))
                            .tightBracketBy(text("("), text(")"))

                pretty(t, style) + prettyArgs
            case Select(scrutinee, field, tp, anns) =>
                pretty(scrutinee, style) + text("." + field)
            case Const(const, _, _) => prettyValue(const).styled(Fg.colorCode(64))
            case And(a, b, _)       =>
                // We don't add parentheses for nested Ands, because they are associative.
                // But we add parentheses for nested Ors and Nots.
                val docA = a match {
                    case _: Or | _: Not => inParens(pretty(a, style))
                    case _              => pretty(a, style)
                }
                val docB = b match {
                    case _: Or | _: Not => inParens(pretty(b, style))
                    case _              => pretty(b, style)
                }
                (docA / kw("and") / docB).grouped.aligned

            case Or(a, b, _) =>
                // We add parentheses for nested Ors and Nots.
                val docA = a match {
                    case _: Or | _: Not => inParens(pretty(a, style))
                    case _              => pretty(a, style)
                }
                val docB = b match {
                    case _: Or | _: Not => inParens(pretty(b, style))
                    case _              => pretty(b, style)
                }
                (docA / kw("or") / docB).grouped.aligned

            case Not(a, _) =>
                // We add parentheses for nested Nots, Ands, and Ors.
                val docA = a match {
                    case _: Not | _: And | _: Or => inParens(pretty(a, style) + line)
                    case _                       => pretty(a, style)
                }
                (kw("not") / docA).grouped.aligned

            case IfThenElse(cond, t, f, tp, ann) =>
                ((kw("if") + (line + pretty(cond, style)).nested(4)).grouped
                    + (line + kw("then") + (line + pretty(t, style)).nested(4)).grouped
                    + (line + kw("else") + (line + pretty(f, style)).nested(
                      4
                    )).grouped).aligned
            case Builtin(bn, _, _) => pretty(bn).styled(Fg.colorCode(176))
            case Error(msg, _, _) => (text(s"ERROR") & pretty(msg, style)).styled(Fg.colorCode(124))
            case Cast(expr, tp, anns) =>
                (kw("cast") & (lineOrSpace + inParens(pretty(expr, style))).nested(4) + char(
                  ':'
                ) & typ(
                  pretty(tp)
                )).grouped.aligned

    def pretty(sirType: SIRType): Doc =
        sirType match
            case SIRType.TypeVar(name, optId, isBuiltin) =>
                text(name + optId.fold("")(id => s"#${id}") + (if isBuiltin then "(b)" else ""))
            case SIRType.Fun(in, out) =>
                inParens(pretty(in) + text(" -> ") + pretty(out))
            case SIRType.TypeLambda(params, body) =>
                inParens(
                  text("λ.") + intercalate(text(",") + space, params.map(p => text(p.show))) + text(
                    " =>> "
                  ) + pretty(body)
                )
            case p: SIRType.Primitive => text(p.show)
            case SIRType.CaseClass(constrDecl, typeParams, _) =>
                text(constrDecl.name) + inOptBrackets(
                  intercalate(text(",") + space, typeParams.map(pretty))
                )
            case SIRType.SumCaseClass(decl, typeParams) =>
                text(decl.name) + inOptBrackets(
                  intercalate(text(",") + space, typeParams.map(pretty))
                )
            case t => text(t.show)

    def pretty(p: Program): Doc =
        val (major, minor, patch) = p.version
        inParens(
          text("program") & text(
            s"$major.$minor.$patch"
          ) & pretty(p.term, PrettyStyle.Normal)
        )

    /** @deprecated Use Pretty[Term] instance instead */
    @deprecated("Use Pretty[Term] instance from Term companion object instead", "0.14.2")
    def pretty(term: Term, style: PrettyStyle): Doc =
        Pretty[Term].pretty(term, style)

    /** @deprecated Use Pretty[Program] instance instead */
    @deprecated("Use Pretty[Program] instance from Program companion object instead", "0.14.2")
    def pretty(program: uplc.Program, style: PrettyStyle): Doc =
        Pretty[uplc.Program].pretty(program, style)
