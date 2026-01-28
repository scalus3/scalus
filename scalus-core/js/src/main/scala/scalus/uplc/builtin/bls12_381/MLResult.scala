package scalus.uplc.builtin.bls12_381

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.compiletime.asMatchable
import scala.annotation.targetName

class MLResult(private val gt: BLS.GT):
    @targetName("multiply")
    def *(that: MLResult): MLResult =
        new MLResult(BLS.GT.multiply(gt, that.gt))

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: MLResult => BLS.GT.isEquals(gt, that.gt)
        case _              => false

object MLResult:
    def apply(elemG1: G1Element, elemG2: G2Element): MLResult =
        new MLResult(BLS.pairing(elemG1.point, elemG2.point))
