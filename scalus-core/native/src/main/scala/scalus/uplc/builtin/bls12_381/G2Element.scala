package scalus.uplc.builtin.bls12_381

import scalus.uplc.builtin.ByteString
import scalus.utils.Hex

import scala.compiletime.asMatchable

/** BLS12-381 G2 group element for Native platform.
  *
  * Stores the compressed representation (96 bytes).
  */
class G2Element private[builtin] (private[builtin] val compressed: Array[Byte]):
    require(compressed.length == 96, s"G2 compressed must be 96 bytes, got ${compressed.length}")

    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(compressed)

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: G2Element =>
            java.util.Arrays.equals(compressed, that.compressed)
        case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(compressed)

    override def toString: String = s"0x${Hex.bytesToHex(compressed)}"

object G2Element extends G2ElementOffchainApi:
    def apply(compressed: Array[Byte]): G2Element =
        new G2Element(compressed)

    def fromCompressedByteString(bs: ByteString): G2Element =
        new G2Element(bs.bytes)
