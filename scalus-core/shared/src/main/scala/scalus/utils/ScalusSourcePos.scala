package scalus.utils

/** Source position in Scala code, used by both SIR and UPLC Term for tracing.
  *
  * @param file
  *   file path. Empty string means position is unknown.
  * @param startLine
  *   0-based start line
  * @param startColumn
  *   0-based start column
  * @param endLine
  *   0-based end line
  * @param endColumn
  *   0-based end column
  * @param inlinedFrom
  *   chain of positions from which this was inlined
  */
case class ScalusSourcePos(
    file: String,
    startLine: Int,
    startColumn: Int,
    endLine: Int,
    endColumn: Int,
    inlinedFrom: List[ScalusSourcePos] = Nil
) {

    def show: String = {
        val basePos = s"$file:${startLine + 1}:${startColumn} - ${endLine + 1}:${endColumn}"
        if inlinedFrom.isEmpty then basePos
        else s"$basePos\n  inlined from: ${inlinedFrom.map(_.showSingle).mkString(" -> ")}"
    }

    def showSingle: String = s"$file:${startLine + 1}:${startColumn}"

    def isEmpty: Boolean = file.isEmpty && startLine == 0 && startColumn == 0 &&
        endLine == 0 && endColumn == 0

    /** True when this position carries no useful attribution: either genuinely unknown
      * ([[isEmpty]]) or the synthetic compile-boundary root
      * ([[ScalusSourcePos.syntheticMarkerFile]]), which is compiler glue rather than user code. The
      * post-optimization position-fill uses this so the program root never wins as a fallback
      * location — generated spine is attributed to the nearest real user code instead of pooling in
      * one fake hot-spot.
      */
    def isEffectivelyEmpty: Boolean =
        isEmpty || file.endsWith(ScalusSourcePos.syntheticMarkerFile)

    /** The position to attribute a node to, preferring real provenance over a synthetic label.
      *
      *   - a real position resolves to itself (so genuine library/user lines keep their own
      *     attribution — no "blame the caller" flip);
      *   - the synthetic compile-boundary root resolves to the outermost real entry of its inline
      *     chain ([[inlinedFrom]], whose last element is the original user `compile(...)` call) —
      *     real compile-time provenance rather than a guess from tree structure;
      *   - if neither is available it stays [[ScalusSourcePos.empty]], leaving the position-fill to
      *     fall back to the nearest positioned neighbour.
      */
    def effectivePos: ScalusSourcePos =
        if !isEffectivelyEmpty then this
        else inlinedFrom.reverse.find(!_.isEffectivelyEmpty).getOrElse(ScalusSourcePos.empty)

}

object ScalusSourcePos {

    val empty: ScalusSourcePos = ScalusSourcePos("", 0, 0, 0, 0, Nil)

    /** Basename of the synthetic compile-boundary marker file
      * ([[scalus.compiler.CompiledProgramRoot]]). The macro layer leaks this file's position onto
      * the outermost node of every compiled program; [[ScalusSourcePos.isEffectivelyEmpty]] treats
      * positions here as empty so they are not mistaken for real user code. Keep in sync with that
      * file's name.
      */
    val syntheticMarkerFile: String = "CompiledProgramRoot.scala"

}
