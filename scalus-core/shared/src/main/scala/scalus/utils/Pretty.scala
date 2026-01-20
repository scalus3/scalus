package scalus.utils

import org.typelevel.paiges
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.*
import org.typelevel.paiges.Style.XTerm.Fg
import scalus.builtin.ByteString

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

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

/** Low-priority Pretty instances for automatic ADT derivation. These are lower priority than
  * manually defined instances.
  */
trait LowPriorityPrettyInstances {

    /** Extract element labels from the MirroredElemLabels tuple type */
    inline def getElemLabels[Labels <: Tuple]: List[String] =
        inline erasedValue[Labels] match
            case _: EmptyTuple => Nil
            case _: (head *: tail) =>
                constValue[head].toString :: getElemLabels[tail]

    /** Summon Pretty instances for all element types in a tuple */
    inline def summonPrettyInstances[Types <: Tuple]: List[Pretty[Any]] =
        inline erasedValue[Types] match
            case _: EmptyTuple => Nil
            case _: (head *: tail) =>
                summonInline[Pretty[head]].asInstanceOf[Pretty[Any]] ::
                    summonPrettyInstances[tail]

    /** Format a product (case class) with field names and values */
    private def prettyProduct[A <: Product](
        a: A,
        typeName: String,
        labels: List[String],
        instances: List[Pretty[Any]],
        style: Style
    ): Doc = {
        import Pretty.*
        if labels.isEmpty then
            // Case object or empty case class: just show the name
            ctr(typeName, style)
        else
            val fieldDocs = a.productIterator
                .zip(labels)
                .zip(instances)
                .map { case ((value, label), instance) =>
                    kw(label, style) + Doc.text(" = ") + instance.pretty(value, style)
                }
                .toList
            // Format: TypeName(field1 = value1, field2 = value2)
            ctr(typeName, style) +
                Doc.fill(Doc.comma + Doc.space, fieldDocs)
                    .tightBracketBy(Doc.char('('), Doc.char(')'))
    }

    /** Create a Pretty instance for a product type */
    inline def derivePrettyProduct[A](using p: Mirror.ProductOf[A]): Pretty[A] = {
        val typeName = constValue[p.MirroredLabel]
        val labels = getElemLabels[p.MirroredElemLabels]
        lazy val instances = summonPrettyInstances[p.MirroredElemTypes]

        new Pretty[A] {
            def pretty(a: A, style: Style): Doc =
                prettyProduct(a.asInstanceOf[Product], typeName, labels, instances, style)
        }
    }

    /** Create a Pretty instance for a sum type */
    inline def derivePrettySum[A](using s: Mirror.SumOf[A]): Pretty[A] = {
        lazy val instances = summonPrettyInstances[s.MirroredElemTypes]

        new Pretty[A] {
            def pretty(a: A, style: Style): Doc = {
                val ordinal = s.ordinal(a)
                instances(ordinal).pretty(a, style)
            }
        }
    }

    /** Main derivation entry point - lower priority than explicit instances */
    inline given derived[A](using m: Mirror.Of[A]): Pretty[A] =
        inline m match
            case s: Mirror.SumOf[A]     => derivePrettySum[A](using s)
            case p: Mirror.ProductOf[A] => derivePrettyProduct[A](using p)
}

object Pretty extends LowPriorityPrettyInstances:
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
    def braceList(entries: List[Doc]): Doc =
        if entries.isEmpty then text("{}")
        else fill(comma & space, entries).bracketBy(text("{ "), text(" }"))

    /** Field with label: "name: value" */
    def field(label: String, value: Doc, style: Style): Doc =
        kw(label + ":", style) & value

    /** Optional field - returns empty Doc if None */
    def optField[A](label: String, opt: Option[A], style: Style)(using p: Pretty[A]): Doc =
        opt.fold(empty)(v => line + field(label, p.pretty(v, style), style))

    /** Bullet list field with label */
    def bulletList(label: String, items: List[Doc]): Doc =
        if items.isEmpty then empty
        else text(label + ":") / stack(items.map(text("- ") + _)).indent(2)

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

    // === Rainbow Brackets ===

    /** 16 distinct XTerm colors for rainbow brackets (red -> orange -> yellow -> green -> cyan ->
      * blue -> purple -> magenta). Avoids colors used by other styling helpers (27=ctr, 64=lit,
      * 172=kw).
      */
    private val rainbowColors: Array[Int] = Array(
      196, // red
      208, // orange
      214, // light orange
      220, // yellow-orange
      226, // yellow
      118, // lime
      48, // green
      49, // teal
      51, // cyan
      45, // light blue
      39, // sky blue
      33, // blue
      57, // indigo (changed from 27 to avoid collision with ctr)
      93, // purple
      129, // magenta
      201 // pink
    )

    /** Get rainbow color for a given nesting depth */
    private def rainbowColor(depth: Int): paiges.Style =
        Fg.colorCode(rainbowColors(depth % rainbowColors.length))

    /** Create a bracket Doc with rainbow coloring based on depth */
    def rainbowChar(c: Char, depth: Int, style: Style): Doc =
        char(c).styled(rainbowColor(depth), style)

    /** Wrap content in rainbow-colored brackets based on nesting depth */
    def rainbowBracket(d: Doc, open: Char, close: Char, depth: Int, style: Style): Doc =
        val openDoc = rainbowChar(open, depth, style)
        val closeDoc = rainbowChar(close, depth, style)
        d.tightBracketBy(openDoc, closeDoc, indent = 0)

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

    given [A, B](using a: Pretty[A], b: Pretty[B]): Pretty[(A, B)] with
        def pretty(t: (A, B), style: Style): Doc =
            inParens(a.pretty(t._1, style) + comma & space & b.pretty(t._2, style))

    /** Pretty instance for Seq */
    given [A](using p: Pretty[A]): Pretty[Seq[A]] with
        def pretty(a: Seq[A], style: Style): Doc =
            if a.isEmpty then text("[]")
            else
                fill(comma + space, a.toList.map(p.pretty(_, style)))
                    .tightBracketBy(char('['), char(']'))

    /** Pretty instance for Map (works with any scala.collection.Map) */
    given [K, V, M[K, V] <: scala.collection.Map[K, V]](using
        pk: Pretty[K],
        pv: Pretty[V]
    ): Pretty[M[K, V]] with
        def pretty(a: M[K, V], style: Style): Doc =
            val entries = a.toList.map { (k, v) =>
                pk.pretty(k, style) + text(" -> ") + pv.pretty(v, style)
            }
            braceList(entries)

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
