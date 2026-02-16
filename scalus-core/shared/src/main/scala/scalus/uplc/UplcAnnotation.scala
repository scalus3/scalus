package scalus.uplc

import scalus.utils.ScalusSourcePos

case class UplcAnnotation(pos: ScalusSourcePos = ScalusSourcePos.empty) {
    def isEmpty: Boolean = pos.isEmpty
}

object UplcAnnotation {
    val empty: UplcAnnotation = UplcAnnotation()
}
