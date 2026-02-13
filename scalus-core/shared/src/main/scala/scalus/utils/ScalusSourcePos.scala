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

}

object ScalusSourcePos {

    val empty: ScalusSourcePos = ScalusSourcePos("", 0, 0, 0, 0, Nil)

}
