package scalus.uplc.builtin.bls12_381

import scalus.utils.scalajs.internal.toByteString

import scala.compiletime.asMatchable
import scala.annotation.targetName

class MLResult(private val gt: BLS.GT):
    @targetName("multiply")
    def *(that: MLResult): MLResult =
        new MLResult(BLS.GT.multiply(gt, that.gt))

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: MLResult => BLS.GT.isEquals(gt, that.gt)
        case _              => false

    override def hashCode: Int = BLS.GT.toBytes(gt).toByteString.hashCode

object MLResult:
    /** noble refuses to pair the point at infinity. blst, and so the node, accepts it: the pairing
      * with a zero point is the identity of GT.
      */
    def apply(elemG1: G1Element, elemG2: G2Element): MLResult =
        if elemG1.point.is0() || elemG2.point.is0() then new MLResult(BLS.GT.one)
        else new MLResult(BLS.pairing(elemG1.point, elemG2.point))
