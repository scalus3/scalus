package scalus.uplc.builtin.bls12_381

import scalus.crypto.jni.Blst
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex

import scala.compiletime.asMatchable

/** A G2 point, held as a raw `blst_p2` (288 bytes) from scalus-crypto-jni. */
class G2Element private[builtin] (private[builtin] val value: Array[Byte]):
    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(Blst.p2Compress(value))

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: G2Element => Blst.p2IsEqual(value, that.value)
        case _               => false

    override def hashCode(): Int = java.util.Arrays.hashCode(Blst.p2Compress(value))

    override def toString: String = s"0x${Hex.bytesToHex(Blst.p2Compress(value))}"

object G2Element extends G2ElementOffchainApi:
    /** Uncompresses a 96-byte compressed G2 point; fails if it is not a point in G2. */
    def apply(value: ByteString): G2Element = new G2Element(Blst.p2Uncompress(value.bytes))
