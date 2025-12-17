package scalus.compiler.sir

import org.typelevel.paiges
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import org.typelevel.paiges.Style.XTerm.Fg
import scalus.*
import scalus.builtin.Data
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni
import scalus.uplc.Term
import scalus.uplc.TermSanitizer
import scalus.utils.Utils

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
    enum Style:
        case Normal, XTerm

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

    def pretty(sir: SIR, style: Style): Doc =
        import SIR.*
        extension (d: Doc)
            def styled(s: paiges.Style): Doc = if style == Style.XTerm then d.style(s) else d
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
          ) & pretty(p.term, Style.Normal)
        )

    private var bracketColorIndex = 1
    private def nextBracketColor(): Int =
        val color = bracketColorIndex
        bracketColorIndex = (bracketColorIndex + 1) % 15 + 1
        color

    /** Bracket a document for UPLC syntax where the opening bracket and keyword must stay together.
      * Uses a different approach than tightBracketBy: keeps `(keyword` as a single unit, then uses
      * bracketBy to handle line breaks for the body.
      */
    private def uplcBracket(
        keyword: Doc,
        body: Doc,
        openParen: Doc = text("("),
        closeParen: Doc = text(")")
    ): Doc =
        // Keep "(keyword" together as a single unit that never breaks
        val prefix = openParen + keyword
        // The body can break after the keyword
        (prefix & body).grouped + closeParen

    /** Bracket with multiple arguments that can break between them */
    private def uplcBracketArgs(
        keyword: Doc,
        args: List[Doc],
        openParen: Doc = text("("),
        closeParen: Doc = text(")")
    ): Doc =
        if args.isEmpty then openParen + keyword + closeParen
        else
            val prefix = openParen + keyword
            val body = intercalate(lineOrSpace, args)
            ((prefix & body).nested(2).grouped + closeParen).grouped

    def pretty(term: Term, style: Style): Doc =
        // Sanitize variable names to ensure they conform to UPLC text format
        val sanitizedTerm = TermSanitizer.sanitizeNames(term)

        import Term.*
        extension (d: Doc)
            def styled(s: paiges.Style): Doc = if style == Style.XTerm then d.style(s) else d
        def kw(s: String): Doc = text(s).styled(Fg.colorCode(172))
        sanitizedTerm match
            case Var(name) => text(name.name)
            case LamAbs(name, term) =>
                val color = nextBracketColor()
                val openP = char('(').styled(Fg.colorCode(color))
                val closeP = char(')').styled(Fg.colorCode(color))
                // (lam name body) - keyword and name should stay with opening paren
                val prefix = openP + kw("lam") & text(name)
                ((prefix / pretty(term, style)).nested(2).grouped + closeP).grouped
            case a @ Apply(f, arg) =>
                val (t, args) = a.applyToList
                val color = nextBracketColor()
                // [f arg1 arg2 ...] - square brackets, can break between args
                val openB = text("[").styled(Fg.colorCode(color))
                val closeB = text("]").styled(Fg.colorCode(color))
                val allTerms = (t :: args).map(pretty(_, style))
                val body = intercalate(lineOrSpace, allTerms)
                ((openB + body).nested(2).grouped + closeB).grouped
            case Force(term) =>
                // (force term)
                uplcBracket(kw("force"), pretty(term, style))
            case Delay(term) =>
                // (delay term)
                uplcBracket(kw("delay"), pretty(term, style))
            case Const(const) =>
                // (con type value) - should not break
                inParens(kw("con") & const.pretty.styled(Fg.colorCode(64)))
            case Builtin(bn) =>
                inParens(kw("builtin") & pretty(bn).styled(Fg.colorCode(176)))
            case Error             => kw("(error)")
            case Constr(tag, args) =>
                // (constr tag arg1 arg2 ...)
                uplcBracketArgs(kw("constr") & str(tag.value), args.map(pretty(_, style)))
            case Case(arg, cases) =>
                // (case scrutinee branch1 branch2 ...)
                uplcBracketArgs(kw("case"), pretty(arg, style) :: cases.map(pretty(_, style)))

    def pretty(program: uplc.Program, style: Style): Doc =
        val (major, minor, patch) = program.version
        // (program version term) - keep (program together
        val prefix = text("(program")
        val version = text(s"$major.$minor.$patch")
        val term = pretty(program.term, style)
        ((prefix & version & term).nested(2).grouped + text(")")).grouped
