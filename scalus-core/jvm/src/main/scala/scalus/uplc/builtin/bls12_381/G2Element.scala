package scalus.uplc.builtin.bls12_381

import scalus.uplc.builtin.ByteString
import scalus.utils.Hex
import supranational.blst.*

import scala.compiletime.asMatchable

class G2Element(private[builtin] val value: P2):
    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(value.compress())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: G2Element => value.is_equal(that.value)
        case _               => false

    override def toString: String = s"0x${Hex.bytesToHex(value.compress())}"

object G2Element extends G2ElementOffchainApi:
    def apply(value: P2): G2Element = new G2Element(value)
    def apply(value: ByteString): G2Element = new G2Element(
      new P2(value.bytes)
    )
