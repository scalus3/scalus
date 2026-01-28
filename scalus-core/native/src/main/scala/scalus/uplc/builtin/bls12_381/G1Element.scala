package scalus.uplc.builtin.bls12_381

import scalus.uplc.builtin.ByteString
import scalus.utils.Hex

import scala.compiletime.asMatchable

/** BLS12-381 G1 group element for Native platform.
  *
  * Stores the compressed representation (48 bytes).
  */
class G1Element private[builtin] (private[builtin] val compressed: Array[Byte]):
    require(compressed.length == 48, s"G1 compressed must be 48 bytes, got ${compressed.length}")

    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(compressed)

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: G1Element =>
            java.util.Arrays.equals(compressed, that.compressed)
        case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(compressed)

    override def toString: String = s"0x${Hex.bytesToHex(compressed)}"

object G1Element extends G1ElementOffchainApi:
    def apply(compressed: Array[Byte]): G1Element =
        new G1Element(compressed)

    def fromCompressedByteString(bs: ByteString): G1Element =
        new G1Element(bs.bytes)
