package scalus.uplc.internal

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import org.typelevel.paiges.Doc
import scalus.uplc.{Constant, Term, TermPrinter, TermSanitizer}
import scalus.utils.Style

import java.util.IdentityHashMap
import scala.collection.mutable

/** One mapped region of rendered UPLC text.
  *
  * @param s
  *   start character offset into [[UplcSourceMap.uplc]], inclusive
  * @param e
  *   end character offset into [[UplcSourceMap.uplc]], exclusive
  * @param n
  *   the node's post-order index in the term tree (children before parent, fields in declaration
  *   order). Post-order is used because it keeps the indices of an already-rendered program stable
  *   when that program is later wrapped in `Apply` nodes to apply parameters.
  * @param file
  *   index into [[UplcSourceMap.files]]
  * @param sl
  *   0-based start line of the source position
  * @param sc
  *   0-based start column of the source position
  * @param el
  *   0-based end line of the source position
  * @param ec
  *   0-based end column of the source position
  * @param fn
  *   index into [[UplcSourceMap.functions]], absent when the node carries no enclosing function
  *   name
  * @note
  *   lines are 0-based here, as in [[scalus.utils.ScalusSourcePos]]. The `profile.json` report uses
  *   1-based lines; do not mix the two.
  */
final case class UplcSpan(
    s: Int,
    e: Int,
    n: Int,
    file: Int,
    sl: Int,
    sc: Int,
    el: Int,
    ec: Int,
    fn: Option[Int]
)

/** The `<key>.uplc.json` document consumed by the Scalus VS Code extension: the UPLC text of a
  * compiled program plus a table mapping ranges of that text back to Scala source positions.
  *
  * `files` and `functions` are string tables referenced by [[UplcSpan.file]] and [[UplcSpan.fn]].
  *
  * @param spans
  *   the mapped regions, sorted by [[UplcSpan.s]] ascending, then [[UplcSpan.e]] descending, which
  *   orders enclosing spans before the spans they contain: binary-searching by offset and resolving
  *   a cursor to the innermost containing span work directly on the table. The spans are properly
  *   nested, so that resolution is well defined. (Documents written by earlier schema-1 producers
  *   carried the spans unsorted, so a defensive consumer may still sort.) The table is empty when
  *   nothing could be mapped; [[uplc]] is always the full program text.
  */
final case class UplcSourceMap(
    schemaVersion: Int,
    uplc: String,
    files: Seq[String],
    functions: Seq[String],
    spans: Seq[UplcSpan]
)

/** Renders a [[scalus.uplc.Term]] to UPLC text together with the text-range to source-position map
  * that the UPLC source view needs.
  *
  * The text comes from the ordinary pretty-printer at the same width as `Term.show`, so the
  * rendered UPLC is exactly what every other Scalus output shows. Offsets are recovered by passing
  * zero-width markers through the printer ([[org.typelevel.paiges.Doc.zeroWidth]], the same
  * mechanism the printer already uses for ANSI styling): markers take part in no layout decision,
  * are emitted verbatim into the rendered string, and are stripped afterwards while recording where
  * each node's text starts and ends.
  *
  * Public for tooling, but in `scalus.uplc.internal` and thus with no binary-compatibility
  * guarantees: the `<key>.uplc.json` format (versioned by [[SchemaVersion]]) is the contract, not
  * this API.
  */
object UplcSourceMapRenderer {

    /** Schema version of the `<key>.uplc.json` document. Bump on any incompatible change to its
      * shape so consumers (e.g. the Scalus VS Code extension) can detect and reject documents they
      * don't understand.
      */
    val SchemaVersion = 1

    /** The width `Term.show` renders at. Used here too, so the artifact holds the familiar text. */
    private val RenderWidth = TermPrinter.DefaultRenderWidth

    // Always emit `files`/`functions`/`spans`, even when empty, so consumers can index them without
    // a presence check. `fn` is still omitted when absent (jsoniter's transientNone default).
    private given JsonValueCodec[UplcSourceMap] =
        JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))

    // Control characters that printed UPLC does not contain: names are sanitized identifiers, byte
    // strings are hex, numbers are digits. A `string` constant is printed verbatim and could in
    // principle hold them; `render` verifies the stripped text against an unmarked render, so such
    // a program loses its spans rather than getting corrupted text.
    private val MarkerStart = '\u0001'
    private val MarkerEnd = '\u0002'

    /** True when a `string` constant somewhere in `term` contains a marker character — the only way
      * rendered UPLC can collide with the marker encoding. String constants are the single
      * verbatim-printed value (also inside list/pair/array constants); everything else prints as
      * identifiers, hex, or digits. When this is false, every marker character in the marked render
      * is a genuine marker and stripping is exact, so [[render]] skips its verification pass.
      */
    private def mayCollideWithMarkers(t: Term): Boolean = t match
        case Term.Const(c, _)      => constantHasMarkerChar(c)
        case Term.LamAbs(_, b, _)  => mayCollideWithMarkers(b)
        case Term.Apply(f, a, _)   => mayCollideWithMarkers(f) || mayCollideWithMarkers(a)
        case Term.Force(b, _)      => mayCollideWithMarkers(b)
        case Term.Delay(b, _)      => mayCollideWithMarkers(b)
        case Term.Constr(_, as, _) => as.exists(mayCollideWithMarkers)
        case Term.Case(a, cs, _)   => mayCollideWithMarkers(a) || cs.exists(mayCollideWithMarkers)
        case _                     => false

    private def constantHasMarkerChar(c: Constant): Boolean = c match
        case Constant.String(s)    => s.exists(ch => ch == MarkerStart || ch == MarkerEnd)
        case Constant.Pair(a, b)   => constantHasMarkerChar(a) || constantHasMarkerChar(b)
        case Constant.List(_, vs)  => vs.exists(constantHasMarkerChar)
        case Constant.Array(_, vs) => vs.exists(constantHasMarkerChar)
        case _                     => false

    /** True when at least one node of `term` carries a usable source position, i.e. rendering it
      * would produce a non-empty span table. Terms decoded from CBOR carry no annotations at all,
      * and a source map for them would map nothing.
      */
    def hasSourceInfo(term: Term): Boolean =
        !term.annotation.pos.effectivePos.isEffectivelyEmpty || (term match
            case Term.LamAbs(_, b, _)  => hasSourceInfo(b)
            case Term.Apply(f, a, _)   => hasSourceInfo(f) || hasSourceInfo(a)
            case Term.Force(b, _)      => hasSourceInfo(b)
            case Term.Delay(b, _)      => hasSourceInfo(b)
            case Term.Constr(_, as, _) => as.exists(hasSourceInfo)
            case Term.Case(a, cs, _)   => hasSourceInfo(a) || cs.exists(hasSourceInfo)
            case _                     => false)

    /** Renders `term` and maps every positioned node to the text it printed.
      *
      * The rendered text is always identical to `term.show`; when the program could spoof a marker
      * this is checked, not merely intended (see below). Nodes without an effective source position
      * get no span, and neither do the inner `Apply` nodes of an application chain: the printer
      * flattens `[[[f a] b] c]` to `[f a b c]` and prints only the outermost `Apply`, whose span
      * covers the whole chain.
      *
      * A `string` constant is printed verbatim, so a program can contain text indistinguishable
      * from a marker. Rather than reason about which spoofs are recoverable, whenever a string
      * constant holds a marker character the marked render is verified against an unmarked one, and
      * the whole span table is dropped when they disagree: a source view with no spans is a
      * degraded view, one with wrong text is a wrong one. Programs without such constants — all
      * real ones — need no verification render: every marker in their marked render is genuine.
      */
    def render(term: Term): UplcSourceMap = {
        // Name sanitization is what the printer does anyway, and it preserves both the tree
        // structure and the annotations, so the post-order indices computed here are equally valid
        // for the caller's term.
        val sanitized = TermSanitizer.sanitizeNames(term)

        // Post-order index per node. Identity-based, because the tree may contain structurally
        // equal subterms. A node instance shared by several parents (as common-subexpression
        // elimination produces) is printed once per occurrence, and every occurrence then reports
        // the index this numbering gave its last visit.
        val postOrder = new IdentityHashMap[Term, Integer]()
        var next = 0
        def index(t: Term): Unit = {
            t match
                case Term.LamAbs(_, b, _)  => index(b)
                case Term.Apply(f, a, _)   => index(f); index(a)
                case Term.Force(b, _)      => index(b)
                case Term.Delay(b, _)      => index(b)
                case Term.Constr(_, as, _) => as.foreach(index)
                case Term.Case(a, cs, _)   => index(a); cs.foreach(index)
                case _                     => ()
            postOrder.put(t, next)
            next += 1
        }
        index(sanitized)

        // Collect the positioned nodes in printing order, wrapping each one's text in markers.
        val nodes = mutable.ArrayBuffer.empty[Term]
        val doc = TermPrinter.prettySanitized(
          sanitized,
          Style.Normal,
          (t, d) =>
              if t.annotation.pos.effectivePos.isEffectivelyEmpty then d
              else {
                  val id = nodes.length
                  nodes += t
                  Doc.zeroWidth(s"$MarkerStart$id$MarkerEnd") + d +
                      Doc.zeroWidth(s"$MarkerStart/$id$MarkerEnd")
              }
        )
        val (uplc, starts, ends) = stripMarkers(doc.render(RenderWidth), nodes.length)

        // Only a `string` constant can spoof the marker encoding (see mayCollideWithMarkers), so
        // the ground-truth check — the same printer with no markers at all — runs only when one
        // holds a marker character. Markers are zero-width, so they cannot change a layout
        // decision, and stripping them must restore the unmarked string exactly. If it does not,
        // the program's own text collided with the marker encoding; report the clean text and no
        // spans.
        if mayCollideWithMarkers(sanitized) then {
            val plain = TermPrinter
                .prettySanitized(sanitized, Style.Normal, (_, d) => d)
                .render(RenderWidth)
            if uplc != plain then return UplcSourceMap(SchemaVersion, plain, Nil, Nil, Nil)
        }

        val files = mutable.LinkedHashMap.empty[String, Int]
        val functions = mutable.LinkedHashMap.empty[String, Int]
        def intern(table: mutable.LinkedHashMap[String, Int], s: String): Int =
            table.getOrElseUpdate(s, table.size)

        val spans = nodes.indices.flatMap { id =>
            // Defensive: a node whose markers did not both survive the scan is dropped rather than
            // reported at a wrong offset.
            if starts(id) < 0 || ends(id) <= starts(id) then None
            else {
                val t = nodes(id)
                val pos = t.annotation.pos.effectivePos
                val fn = t.annotation.functionName
                Some(
                  UplcSpan(
                    s = starts(id),
                    e = ends(id),
                    n = postOrder.get(t).intValue,
                    file = intern(files, pos.file),
                    sl = pos.startLine,
                    sc = pos.startColumn,
                    el = pos.endLine,
                    ec = pos.endColumn,
                    fn = if fn.isEmpty then None else Some(intern(functions, fn))
                  )
                )
            }
        }

        // Sorted so enclosing spans precede the spans they contain: consumers can binary-search
        // by offset or resolve the innermost span without sorting first.
        UplcSourceMap(
          SchemaVersion,
          uplc,
          files.keys.toSeq,
          functions.keys.toSeq,
          spans.sortBy(sp => (sp.s, -sp.e))
        )
    }

    /** Serializes a source map to the `<key>.uplc.json` bytes, indented for readability. */
    def toJson(map: UplcSourceMap): Array[Byte] =
        writeToArray(map, WriterConfig.withIndentionStep(2))

    /** Removes the markers [[render]] injected, returning the clean text plus, per marker id, the
      * offsets into that text where the node's text starts and ends (`-1` when the marker was not
      * found).
      *
      * A marker is consumed only when it parses completely: start char, an optional `/`, digits
      * forming an id below `count` that this scan has not seen yet, end char. Anything else is
      * content and is copied through, so a control character inside a `string` constant can never
      * make this throw or index out of bounds.
      *
      * It can still mislead this scan, though: a `string` constant holding the exact encoding of a
      * marker that is printed *before* the node it names is consumed as that marker, which both
      * eats the constant's characters and leaves the genuine marker to be copied through as text.
      * Detecting that here would mean re-deriving what the text should have been, so [[render]]
      * checks the result against an unmarked render instead and discards the spans on a mismatch.
      */
    private def stripMarkers(marked: String, count: Int): (String, Array[Int], Array[Int]) = {
        val clean = new StringBuilder(marked.length)
        val starts = Array.fill(count)(-1)
        val ends = Array.fill(count)(-1)
        var i = 0
        while i < marked.length do {
            var consumed = false
            if marked.charAt(i) == MarkerStart then {
                var j = i + 1
                val closing = j < marked.length && marked.charAt(j) == '/'
                if closing then j += 1
                var id = 0
                var digits = 0
                while j < marked.length && marked.charAt(j).isDigit && id < count do {
                    id = id * 10 + (marked.charAt(j) - '0')
                    digits += 1
                    j += 1
                }
                if digits > 0 && id < count && j < marked.length && marked.charAt(j) == MarkerEnd
                then {
                    val slots = if closing then ends else starts
                    if slots(id) < 0 then {
                        slots(id) = clean.length
                        consumed = true
                        i = j + 1
                    }
                }
            }
            if !consumed then {
                clean.append(marked.charAt(i))
                i += 1
            }
        }
        (clean.toString, starts, ends)
    }
}
