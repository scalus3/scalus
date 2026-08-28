package scalus.tsexport

import scala.util.matching.Regex

/** Converts a raw Scaladoc comment (including the frame) into TSDoc body lines. */
object DocConverter {

    /** `[[target]]`, reluctant but never stopping on a `]]` that another `]` follows, so a target
      * with brackets of its own - `[[Map[K, V]]]` - keeps them.
      */
    private val linkRe = raw"\[\[(.+?)\]\](?!\])".r

    /** A markdown list item: folding a tag continuation into it would collapse the list. */
    private val listItem = raw"(?:[-*+]|\d+\.)\s.*".r

    private def isFence(line: String): Boolean = {
        val t = line.trim
        t.startsWith("```") || t.startsWith("~~~")
    }

    private def startsBlock(line: String): Boolean =
        listItem.matches(line.trim) || isFence(line)

    /** Applies `f` to every line outside a fenced code block; fenced lines pass through. */
    private def mapUnfenced(lines: List[String])(f: String => String): List[String] =
        lines
            .foldLeft((List.empty[String], false)) { case ((acc, inFence), line) =>
                if isFence(line) then (line :: acc, !inFence)
                else if inFence then (line :: acc, true)
                else (f(line) :: acc, false)
            }
            ._1
            .reverse

    private def rewriteTag(line: String): String =
        if line == "@return" || line.startsWith("@return ") then
            line.replaceFirst("^@return", "@returns")
        else if line == "@tparam" || line.startsWith("@tparam ") then
            line.replaceFirst("^@tparam", "@typeParam")
        else line

    def convert(rawScaladoc: String): Option[TsDoc] = {
        val trimmed = rawScaladoc.trim
        if trimmed.isEmpty then return None
        val body = trimmed
            .stripPrefix("/**")
            .stripSuffix("*/")
        // strip each line's leading whitespace + optional '*' + one space
        val stripped = body.linesIterator.toList.map { line =>
            val noWs = line.dropWhile(c => c == ' ' || c == '\t')
            val noStar = if noWs.startsWith("*") then noWs.drop(1) else noWs
            (if noStar.startsWith(" ") then noStar.drop(1) else noStar).stripTrailing
        }
        // Fold tag continuations onto the @tag line, but never inside a fenced code block and
        // never a line that opens a markdown block: that would collapse lists and code onto
        // one line, which is why @example blocks used to be impossible.
        val folded = stripped
            .foldLeft((List.empty[String], false)) { case ((acc, inFence), line) =>
                if isFence(line) then (line :: acc, !inFence)
                else if inFence then (line :: acc, true)
                else
                    val rewritten = rewriteTag(line)
                    acc match
                        case prev :: rest
                            if prev.startsWith("@") && rewritten.nonEmpty &&
                                !rewritten.startsWith("@") && !startsBlock(rewritten) =>
                            (s"$prev ${rewritten.trim}" :: rest, false)
                        case _ => (rewritten :: acc, false)
            }
            ._1
            .reverse
        val cleaned = tidy(folded)
        if cleaned.isEmpty then None else Some(TsDoc(cleaned))
    }

    /** Rewrites Scaladoc `[[target]]` links.
      *
      * A target that names an exported declaration becomes a live `{@link Name}`; anything else is
      * Scala-only, so it degrades to backticked code instead of a link that goes nowhere. Fenced
      * code blocks are left untouched.
      */
    def resolveLinks(doc: TsDoc, tsNameOf: String => Option[String]): TsDoc =
        TsDoc(mapUnfenced(doc.lines) { line =>
            linkRe.replaceAllIn(
              line,
              m =>
                  Regex.quoteReplacement(
                    tsNameOf(m.group(1)).fold(s"`${m.group(1)}`")(n => s"{@link $n}")
                  )
            )
        })

    /** Collapses blank runs and drops leading and trailing blank lines. */
    private def tidy(lines: List[String]): List[String] = {
        val collapsed = lines
            .foldLeft(List.empty[String]) { (acc, line) =>
                if line.isEmpty && acc.headOption.contains("") then acc else line :: acc
            }
            .reverse
        collapsed.dropWhile(_.isEmpty).reverse.dropWhile(_.isEmpty).reverse
    }

    /** Splits a class doc into the class doc proper and its `@constructor` section.
      *
      * Scaladoc documents the primary constructor with an `@constructor` tag inside the class
      * comment; TSDoc has no such tag, so that text belongs on the `constructor` signature.
      */
    def splitConstructorTag(doc: TsDoc): (Option[TsDoc], Option[TsDoc]) = {
        val (ctorLines, classLines) = doc.lines.partition(_.startsWith("@constructor"))
        val ctorDoc = ctorLines.headOption
            .map(_.stripPrefix("@constructor").trim)
            .filter(_.nonEmpty)
            .map(text => TsDoc(List(text)))
        val rest = tidy(classLines)
        (if rest.isEmpty then None else Some(TsDoc(rest)), ctorDoc)
    }
}
