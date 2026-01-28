package scalus.uplc.builtin.bls12_381

import scalus.uplc.builtin.ByteString
import scalus.utils.Hex
import supranational.blst.*

import scala.compiletime.asMatchable

class G1Element(private[builtin] val value: P1):
    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(value.compress())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: G1Element => value.is_equal(that.value)
        case _               => false

    // TODO: check if this is correct
    override def toString: String = s"0x${Hex.bytesToHex(value.compress())}"

object G1Element extends G1ElementOffchainApi:
    def apply(value: P1): G1Element = new G1Element(value)
    def apply(value: ByteString): G1Element = new G1Element(
      new P1(value.bytes)
    )
