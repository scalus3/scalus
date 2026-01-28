package scalus.uplc.builtin.bls12_381

import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.NodeJsPlatformSpecific.{toByteString, toJsBigInt, toUint8Array}
import scalus.uplc.builtin.PlatformSpecific

import scala.scalajs.js
import scala.compiletime.asMatchable
import scala.annotation.targetName

class G2Element(private[builtin] val point: BLS.G2.Point):
    def toCompressedByteString: ByteString = point.toRawBytes().toByteString

    @targetName("add")
    def +(that: G2Element): G2Element = G2Element(
      point.add(that.point)
    )

    @targetName("multiply")
    def *(scalar: BigInt): G2Element =
        val modScalar = scalar % PlatformSpecific.bls12_381_scalar_period
        val signum = modScalar.signum

        if signum > 0 then G2Element(point.multiply(modScalar.toJsBigInt))
        else if signum < 0 then G2Element(point.multiply(modScalar.abs.toJsBigInt).negate())
        else G2Element.zero

    @targetName("negate")
    def unary_- : G2Element = G2Element(point.negate())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: G2Element => point.isEquals(that.point)
        case _               => false

    override def hashCode: Int = toCompressedByteString.hashCode
    override def toString: String = s"0x${point.toHex()}"

object G2Element extends G2ElementOffchainApi:
    def apply(point: BLS.G2.Point): G2Element = new G2Element(point)

    def fromCompressedByteString(byteString: ByteString): G2Element = {
        if byteString.size != 96 then
            throw js.JavaScriptException(
              js.Error(
                s"Invalid length of bytes for compressed point of G2: expected 96, actual: ${byteString.size}, byteString: $byteString"
              )
            )

        if (byteString.bytes(0) & 0x40) != 0 && (byteString.bytes(0) & 0x30) != 0 then
            throw js.JavaScriptException(
              js.Error(
                s"invalid encoding for compressed zero point of G2, byteString: $byteString"
              )
            )

        G2Element(
          BLS.G2.Point.fromRawBytes(byteString.toUint8Array)
        )
    }

    def hashToGroup(byteString: ByteString, dst: ByteString): G2Element = {
        if dst.size > 255 then
            throw js.JavaScriptException(
              js.Error(
                s"Invalid length of bytes for dst parameter of hashToGroup of G2, expected: <= 255, actual: ${dst.size}"
              )
            )

        G2Element(
          BLS.g2
              .hashToGroup(byteString.toUint8Array, BLS.HtfBasicOpts(dst.toUint8Array))
        )
    }
