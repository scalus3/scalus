package scalus.uplc.builtin.bls12_381

import supranational.blst.*

import scala.compiletime.asMatchable

class MLResult(private[builtin] val value: PT):
    override def equals(that: Any): Boolean = that.asMatchable match
        case that: MLResult => value.is_equal(that.value)
        case _              => false

object MLResult:
    def apply(value: PT): MLResult = new MLResult(value)
