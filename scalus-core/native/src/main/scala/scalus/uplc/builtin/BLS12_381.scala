package scalus.uplc.builtin

import scalus.utils.Hex

import scala.compiletime.asMatchable

/** BLS12-381 G1 group element for Native platform.
  *
  * Stores the compressed representation (48 bytes).
  */
class BLS12_381_G1_Element private[builtin] (private[builtin] val compressed: Array[Byte]):
    require(compressed.length == 48, s"G1 compressed must be 48 bytes, got ${compressed.length}")

    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(compressed)

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G1_Element =>
            java.util.Arrays.equals(compressed, that.compressed)
        case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(compressed)

    override def toString: String = s"0x${Hex.bytesToHex(compressed)}"

object BLS12_381_G1_Element extends BLS12_381_G1_ElementOffchainApi:
    def apply(compressed: Array[Byte]): BLS12_381_G1_Element =
        new BLS12_381_G1_Element(compressed)

    def fromCompressedByteString(bs: ByteString): BLS12_381_G1_Element =
        new BLS12_381_G1_Element(bs.bytes)

/** BLS12-381 G2 group element for Native platform.
  *
  * Stores the compressed representation (96 bytes).
  */
class BLS12_381_G2_Element private[builtin] (private[builtin] val compressed: Array[Byte]):
    require(compressed.length == 96, s"G2 compressed must be 96 bytes, got ${compressed.length}")

    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(compressed)

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G2_Element =>
            java.util.Arrays.equals(compressed, that.compressed)
        case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(compressed)

    override def toString: String = s"0x${Hex.bytesToHex(compressed)}"

object BLS12_381_G2_Element extends BLS12_381_G2_ElementOffchainApi:
    def apply(compressed: Array[Byte]): BLS12_381_G2_Element =
        new BLS12_381_G2_Element(compressed)

    def fromCompressedByteString(bs: ByteString): BLS12_381_G2_Element =
        new BLS12_381_G2_Element(bs.bytes)

/** BLS12-381 Miller loop result (GT element) for Native platform.
  *
  * Stores the raw blst_fp12 structure (576 bytes).
  */
class BLS12_381_MlResult private[builtin] (private[builtin] val fp12Bytes: Array[Byte]):
    require(fp12Bytes.length == 576, s"fp12 must be 576 bytes, got ${fp12Bytes.length}")

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_MlResult =>
            java.util.Arrays.equals(fp12Bytes, that.fp12Bytes)
        case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(fp12Bytes)

object BLS12_381_MlResult:
    def apply(fp12Bytes: Array[Byte]): BLS12_381_MlResult =
        new BLS12_381_MlResult(fp12Bytes)
