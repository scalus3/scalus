package scalus.uplc

import scalus.utils.ScalusSourcePos

case class UplcAnnotation(
    pos: ScalusSourcePos = ScalusSourcePos.empty,
    functionName: String = ""
) {
    def isEmpty: Boolean = pos.isEmpty && functionName.isEmpty
}

object UplcAnnotation {
    val empty: UplcAnnotation = UplcAnnotation()
}
