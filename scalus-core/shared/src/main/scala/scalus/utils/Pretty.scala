package scalus.utils

import org.typelevel.paiges
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import org.typelevel.paiges.Style.XTerm.Fg
import scalus.builtin.ByteString

/** Pretty printing style: Normal (plain text) or XTerm (with color highlighting) */
enum Style:
    case Normal, XTerm

/** Typeclass for pretty printing values to [[org.typelevel.paiges.Doc]].
  *
  * Pretty instances should be defined in companion objects of the types they print. This typeclass
  * supports two rendering modes:
  *   - `pretty`: concise, single-line friendly output
  *   - `prettyDetailed`: expanded, multi-line output with full details
  *
  * Example usage:
  * {{{
  * import scalus.utils.Pretty.given
  *
  * val value: Value = ...
  * println(value.show)           // concise output
  * println(value.showDetailed)   // detailed output
  * println(value.showHighlighted) // with XTerm colors
  * }}}
  */
trait Pretty[A]:
    /** Pretty print to a Doc in concise format */
    def pretty(a: A, style: Style = Style.Normal): Doc

    /** Pretty print to a Doc in detailed format. Defaults to `pretty` if not overridden. */
    def prettyDetailed(a: A, style: Style = Style.Normal): Doc = pretty(a, style)

object Pretty:
    /** Summon a Pretty instance */
    def apply[A](using p: Pretty[A]): Pretty[A] = p

    /** Create a Pretty instance from a function */
    def instance[A](f: (A, Style) => Doc): Pretty[A] = new Pretty[A]:
        def pretty(a: A, style: Style): Doc = f(a, style)

    /** Create a Pretty instance with both concise and detailed implementations */
    def instanceWithDetailed[A](
        concise: (A, Style) => Doc,
        detailed: (A, Style) => Doc
    ): Pretty[A] = new Pretty[A]:
        def pretty(a: A, style: Style): Doc = concise(a, style)
        override def prettyDetailed(a: A, style: Style): Doc = detailed(a, style)

    // === Doc Helpers ===

    /** Wrap in parentheses */
    def inParens(d: Doc): Doc = d.tightBracketBy(char('('), char(')'), indent = 0)

    /** Wrap in braces */
    def inBraces(d: Doc): Doc = d.tightBracketBy(char('{'), char('}'), indent = 0)

    /** Wrap in brackets */
    def inBrackets(d: Doc): Doc = d.tightBracketBy(char('['), char(']'), indent = 0)

    /** Comma-separated list in braces: { a, b, c } or multiline with indent if needed */
    def braceList(entries: List[Doc]): Doc = entries match
        case Nil => text("{}")
        case _   => fill(comma & space, entries).bracketBy(text("{ "), text(" }"))

    /** Field with label: "name: value" */
    def field(label: String, value: Doc, style: Style): Doc =
        kw(label + ":", style) & value

    /** Optional field - returns empty Doc if None */
    def optField[A](label: String, opt: Option[A], style: Style)(using p: Pretty[A]): Doc =
        opt.fold(empty)(v => line + field(label, p.pretty(v, style), style))

    /** Bullet list field with label */
    def bulletList(label: String, items: List[Doc]): Doc =
        if items.isEmpty then empty
        else text(label + ":") / stack(items.map(text("- ") + _)).nested(2)

    // === Styling Helpers ===

    /** Apply XTerm color styling if in XTerm mode */
    extension (d: Doc)
        def styled(s: paiges.Style, style: Style): Doc =
            if style == Style.XTerm then d.style(s) else d

    /** Keyword styling (orange/yellow) */
    def kw(s: String, style: Style): Doc = text(s).styled(Fg.colorCode(172), style)

    /** Constructor/type name styling (blue) */
    def ctr(s: String, style: Style): Doc = text(s).styled(Fg.colorCode(27), style)

    /** Type styling (purple) */
    def typ(d: Doc, style: Style): Doc = d.styled(Fg.colorCode(55), style)

    /** Value/literal styling (green) */
    def lit(d: Doc, style: Style): Doc = d.styled(Fg.colorCode(64), style)

    /** Error styling (red) */
    def err(d: Doc, style: Style): Doc = d.styled(Fg.colorCode(124), style)

    // === Formatting Helpers ===

    /** Format lovelace as ADA with 6 decimal places */
    def formatAda(lovelace: Long): String =
        val ada = lovelace / 1000000L
        val remainder = math.abs(lovelace % 1000000L)
        if lovelace < 0 && ada == 0 then s"-0.${"%06d".format(remainder)}"
        else s"$ada.${"%06d".format(remainder)}"

    /** Format bytes as hex string */
    def formatHex(bytes: Array[Byte]): String =
        Hex.bytesToHex(bytes)

    // === Primitive Instances ===

    given Pretty[String] with
        def pretty(a: String, style: Style): Doc = text(a)

    given Pretty[Int] with
        def pretty(a: Int, style: Style): Doc = str(a)

    given Pretty[Long] with
        def pretty(a: Long, style: Style): Doc = str(a)

    given Pretty[BigInt] with
        def pretty(a: BigInt, style: Style): Doc = str(a)

    given Pretty[Boolean] with
        def pretty(a: Boolean, style: Style): Doc = text(if a then "true" else "false")

    given Pretty[ByteString] with
        def pretty(a: ByteString, style: Style): Doc = text(a.toHex)

    /** Pretty instance for Option - shows the value or empty */
    given [A](using p: Pretty[A]): Pretty[Option[A]] with
        def pretty(a: Option[A], style: Style): Doc = a match
            case Some(v) => p.pretty(v, style)
            case None    => empty

    /** Pretty instance for Seq */
    given [A](using p: Pretty[A]): Pretty[Seq[A]] with
        def pretty(a: Seq[A], style: Style): Doc =
            if a.isEmpty then text("[]")
            else
                fill(comma & space, a.toList.map(p.pretty(_, style)))
                    .tightBracketBy(char('['), char(']'))

/** Extension methods for types with a Pretty instance */
extension [A](a: A)(using p: Pretty[A])
    /** Get pretty Doc (concise) */
    def pretty: Doc = p.pretty(a, Style.Normal)

    /** Get pretty Doc with XTerm colors (concise) */
    def prettyXTerm: Doc = p.pretty(a, Style.XTerm)

    /** Get pretty Doc (detailed) */
    def prettyDetailed: Doc = p.prettyDetailed(a, Style.Normal)

    /** Get pretty Doc with XTerm colors (detailed) */
    def prettyDetailedXTerm: Doc = p.prettyDetailed(a, Style.XTerm)

    /** Render to string (concise, 80 columns) */
    def show: String = pretty.render(80)

    /** Render to string with XTerm colors (concise, 80 columns) */
    def showHighlighted: String = prettyXTerm.render(80)

    /** Render to string (detailed, 80 columns) */
    def showDetailed: String = prettyDetailed.render(80)

    /** Render to string with XTerm colors (detailed, 80 columns) */
    def showDetailedHighlighted: String = prettyDetailedXTerm.render(80)
