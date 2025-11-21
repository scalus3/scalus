package scalus.builtin
import scalus.utils.Hex
import supranational.blst.*

import scala.compiletime.asMatchable

class BLS12_381_G1_Element(private[builtin] val value: P1):
    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(value.compress())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G1_Element => value.is_equal(that.value)
        case _                          => false

    // TODO: check if this is correct
    override def toString: String = s"0x${Hex.bytesToHex(value.compress())}"

object BLS12_381_G1_Element:
    def apply(value: P1): BLS12_381_G1_Element = new BLS12_381_G1_Element(value)
    def apply(value: ByteString): BLS12_381_G1_Element = new BLS12_381_G1_Element(
      new P1(value.bytes)
    )

class BLS12_381_G2_Element(private[builtin] val value: P2):
    def toCompressedByteString: ByteString = ByteString.unsafeFromArray(value.compress())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G2_Element => value.is_equal(that.value)
        case _                          => false

    override def toString: String = s"0x${Hex.bytesToHex(value.compress())}"

object BLS12_381_G2_Element:
    def apply(value: P2): BLS12_381_G2_Element = new BLS12_381_G2_Element(value)
    def apply(value: ByteString): BLS12_381_G2_Element = new BLS12_381_G2_Element(
      new P2(value.bytes)
    )

class BLS12_381_MlResult(private[builtin] val value: PT):
    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_MlResult => value.is_equal(that.value)
        case _                        => false
