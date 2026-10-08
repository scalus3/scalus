package scalus.uplc.builtin.bls12_381

import scalus.crypto.jni.Blst
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex

import scala.compiletime.asMatchable

/** A G1 point, held as a raw `blst_p1` (144 bytes) from scalus-crypto-jni. */
class G1Element private[builtin] (private[builtin] val value: Array[Byte]):
    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(Blst.p1Compress(value))

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: G1Element => Blst.p1IsEqual(value, that.value)
        case _               => false

    override def hashCode(): Int = java.util.Arrays.hashCode(Blst.p1Compress(value))

    override def toString: String = s"0x${Hex.bytesToHex(Blst.p1Compress(value))}"

object G1Element extends G1ElementOffchainApi:
    /** Uncompresses a 48-byte compressed G1 point; fails if it is not a point in G1. */
    def apply(value: ByteString): G1Element = new G1Element(Blst.p1Uncompress(value.bytes))
