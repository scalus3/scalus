package scalus.uplc.builtin.bls12_381

import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.NodeJsPlatformSpecific.{toByteString, toJsBigInt, toUint8Array}
import scalus.uplc.builtin.PlatformSpecific

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Uint8Array
import scala.compiletime.asMatchable
import scala.annotation.targetName

class G1Element(private[builtin] val point: BLS.G1.Point):
    def toCompressedByteString: ByteString = point.toRawBytes().toByteString

    @targetName("add")
    def +(that: G1Element): G1Element = G1Element(
      point.add(that.point)
    )

    @targetName("multiply")
    def *(scalar: BigInt): G1Element =
        val modScalar = scalar % PlatformSpecific.bls12_381_scalar_period
        val signum = modScalar.signum

        if signum > 0 then G1Element(point.multiply(modScalar.toJsBigInt))
        else if signum < 0 then G1Element(point.multiply(modScalar.abs.toJsBigInt).negate())
        else G1Element.zero

    @targetName("negate")
    def unary_- : G1Element = G1Element(point.negate())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: G1Element => point.isEquals(that.point)
        case _               => false

    override def hashCode: Int = toCompressedByteString.hashCode
    override def toString: String = s"0x${point.toHex()}"

object G1Element extends G1ElementOffchainApi:
    def apply(point: BLS.G1.Point): G1Element = new G1Element(point)

    def fromCompressedByteString(byteString: ByteString): G1Element = {
        if byteString.size != 48 then
            throw js.JavaScriptException(
              js.Error(
                s"Invalid length of bytes for compressed point of G1: expected 48, actual: ${byteString.size}, byteString: $byteString"
              )
            )

        if (byteString.bytes(0) & 0x40) != 0 && (byteString.bytes(0) & 0x30) != 0 then
            throw js.JavaScriptException(
              js.Error(
                s"invalid encoding for compressed zero point of G1, byteString: $byteString"
              )
            )

        G1Element(
          BLS.G1.Point.fromRawBytes(byteString.toUint8Array)
        )
    }

    def hashToGroup(byteString: ByteString, dst: ByteString): G1Element = {
        if dst.size > 255 then
            throw js.JavaScriptException(
              js.Error(
                s"Invalid length of bytes for dst parameter of hashToGroup of G1, expected: <= 255, actual: ${dst.size}"
              )
            )

        G1Element(
          BLS.g1
              .hashToGroup(byteString.toUint8Array, BLS.HtfBasicOpts(dst.toUint8Array))
        )
    }
