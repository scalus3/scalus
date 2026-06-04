package scalus.uplc

import scalus.utils.ScalusSourcePos

case class UplcAnnotation(
    pos: ScalusSourcePos = ScalusSourcePos.empty,
    functionName: String = ""
) {
    def isEmpty: Boolean = pos.isEmpty && functionName.isEmpty

    /** Like [[isEmpty]] but also treats the synthetic compile-boundary root position as empty (see
      * [[scalus.utils.ScalusSourcePos.isEffectivelyEmpty]]).
      */
    def isEffectivelyEmpty: Boolean = pos.isEffectivelyEmpty && functionName.isEmpty
}

object UplcAnnotation {
    val empty: UplcAnnotation = UplcAnnotation()
}
