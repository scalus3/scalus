package scalus.uplc.builtin.bls12_381

import scala.compiletime.asMatchable

/** BLS12-381 Miller loop result (GT element) for Native platform.
  *
  * Stores the raw blst_fp12 structure (576 bytes).
  */
class MLResult private[builtin] (private[builtin] val fp12Bytes: Array[Byte]):
    require(fp12Bytes.length == 576, s"fp12 must be 576 bytes, got ${fp12Bytes.length}")

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: MLResult =>
            java.util.Arrays.equals(fp12Bytes, that.fp12Bytes)
        case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(fp12Bytes)

object MLResult:
    def apply(fp12Bytes: Array[Byte]): MLResult =
        new MLResult(fp12Bytes)
