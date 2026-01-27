package scalus.uplc.builtin

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Uint8Array
import NodeJsPlatformSpecific.{toByteString, toJsBigInt, toUint8Array}

import scala.compiletime.asMatchable
import scala.annotation.targetName

class BLS12_381_G1_Element(private[builtin] val point: BLS.G1.Point):
    def toCompressedByteString: ByteString = point.toRawBytes().toByteString

    @targetName("add")
    def +(that: BLS12_381_G1_Element): BLS12_381_G1_Element = BLS12_381_G1_Element(
      point.add(that.point)
    )

    @targetName("multiply")
    def *(scalar: BigInt): BLS12_381_G1_Element =
        val modScalar = scalar % PlatformSpecific.bls12_381_scalar_period
        val signum = modScalar.signum

        if signum > 0 then BLS12_381_G1_Element(point.multiply(modScalar.toJsBigInt))
        else if signum < 0 then
            BLS12_381_G1_Element(point.multiply(modScalar.abs.toJsBigInt).negate())
        else BLS12_381_G1_Element.zero

    @targetName("negate")
    def unary_- : BLS12_381_G1_Element = BLS12_381_G1_Element(point.negate())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G1_Element => point.isEquals(that.point)
        case _                          => false

    override def hashCode: Int = toCompressedByteString.hashCode
    override def toString: String = s"0x${point.toHex()}"

object BLS12_381_G1_Element extends BLS12_381_G1_ElementOffchainApi:
    def fromCompressedByteString(byteString: ByteString): BLS12_381_G1_Element = {
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

        BLS12_381_G1_Element(
          BLS.G1.Point.fromRawBytes(byteString.toUint8Array)
        )
    }

    def hashToGroup(byteString: ByteString, dst: ByteString): BLS12_381_G1_Element = {
        if dst.size > 255 then
            throw js.JavaScriptException(
              js.Error(
                s"Invalid length of bytes for dst parameter of hashToGroup of G1, expected: <= 255, actual: ${dst.size}"
              )
            )

        BLS12_381_G1_Element(
          BLS.g1
              .hashToGroup(byteString.toUint8Array, BLS.HtfBasicOpts(dst.toUint8Array))
        )
    }

class BLS12_381_G2_Element(private[builtin] val point: BLS.G2.Point):
    def toCompressedByteString: ByteString = point.toRawBytes().toByteString

    @targetName("add")
    def +(that: BLS12_381_G2_Element): BLS12_381_G2_Element = BLS12_381_G2_Element(
      point.add(that.point)
    )

    @targetName("multiply")
    def *(scalar: BigInt): BLS12_381_G2_Element =
        val modScalar = scalar % PlatformSpecific.bls12_381_scalar_period
        val signum = modScalar.signum

        if signum > 0 then BLS12_381_G2_Element(point.multiply(modScalar.toJsBigInt))
        else if signum < 0 then
            BLS12_381_G2_Element(point.multiply(modScalar.abs.toJsBigInt).negate())
        else BLS12_381_G2_Element.zero

    @targetName("negate")
    def unary_- : BLS12_381_G2_Element = BLS12_381_G2_Element(point.negate())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G2_Element => point.isEquals(that.point)
        case _                          => false

    override def hashCode: Int = toCompressedByteString.hashCode
    override def toString: String = s"0x${point.toHex()}"

object BLS12_381_G2_Element extends BLS12_381_G2_ElementOffchainApi:
    def fromCompressedByteString(byteString: ByteString): BLS12_381_G2_Element = {
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

        BLS12_381_G2_Element(
          BLS.G2.Point.fromRawBytes(byteString.toUint8Array)
        )
    }

    def hashToGroup(byteString: ByteString, dst: ByteString): BLS12_381_G2_Element = {
        if dst.size > 255 then
            throw js.JavaScriptException(
              js.Error(
                s"Invalid length of bytes for dst parameter of hashToGroup of G2, expected: <= 255, actual: ${dst.size}"
              )
            )

        BLS12_381_G2_Element(
          BLS.g2
              .hashToGroup(byteString.toUint8Array, BLS.HtfBasicOpts(dst.toUint8Array))
        )
    }

class BLS12_381_MlResult(private val gt: BLS.GT):
    @targetName("multiply")
    def *(that: BLS12_381_MlResult): BLS12_381_MlResult =
        new BLS12_381_MlResult(BLS.GT.multiply(gt, that.gt))

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_MlResult => BLS.GT.isEquals(gt, that.gt)
        case _                        => false

object BLS12_381_MlResult:
    def apply(elemG1: BLS12_381_G1_Element, elemG2: BLS12_381_G2_Element): BLS12_381_MlResult =
        new BLS12_381_MlResult(BLS.pairing(elemG1.point, elemG2.point))

private[builtin] object BLS:
    // import { bls12_381 as bls } from '@noble/curves/bls12-381
    @JSImport("@noble/curves/bls12-381", "bls12_381")
    @js.native
    private object bls12_381 extends js.Object:
        def G1: G1 = js.native
        def G2: G2 = js.native

        def pairing(
            pointG1: BLS.G1.Point,
            pointG2: BLS.G2.Point,
            withFinalExponent: Boolean = true
        ): GT =
            js.native

        def fields: Fields = js.native

    def g1: G1 = bls12_381.G1
    def g2: G2 = bls12_381.G2
    def pairing(pointG1: G1.Point, pointG2: G2.Point): GT = bls12_381.pairing(pointG1, pointG2)

    class HtfBasicOpts(val DST: Uint8Array) extends js.Object

    @js.native
    trait G1 extends js.Object:
        @JSName("ProjectivePoint")
        def pointModule: G1.PointModule = js.native

        @JSName("hashToCurve")
        def hashToGroup(msg: Uint8Array, options: HtfBasicOpts): G1.Point = js.native

    object G1:
        @js.native
        trait PointModule extends js.Object:
            @JSName("fromHex")
            def fromRawBytes(bytes: Uint8Array): Point = js.native

        @js.native
        trait Point extends js.Object:
            @JSName("equals")
            def isEquals(other: Point): Boolean = js.native
            def add(other: Point): Point = js.native
            def multiply(scalar: js.BigInt): Point = js.native
            def negate(): Point = js.native
            def toRawBytes(isCompressed: Boolean = true): Uint8Array = js.native
            def toHex(isCompressed: Boolean = true): String = js.native

        object Point:
            def fromRawBytes(bytes: Uint8Array): Point = g1.pointModule.fromRawBytes(bytes)
    end G1

    @js.native
    trait G2 extends js.Object:
        @JSName("ProjectivePoint")
        def pointModule: G2.PointModule = js.native

        @JSName("hashToCurve")
        def hashToGroup(msg: Uint8Array, options: HtfBasicOpts): G2.Point = js.native

    object G2:
        @js.native
        trait PointModule extends js.Object:
            @JSName("fromHex")
            def fromRawBytes(bytes: Uint8Array): Point = js.native

        @js.native
        trait Point extends js.Object:
            @JSName("equals")
            def isEquals(other: Point): Boolean = js.native
            def add(other: Point): Point = js.native
            def multiply(scalar: js.BigInt): Point = js.native
            def negate(): Point = js.native
            def toRawBytes(isCompressed: Boolean = true): Uint8Array = js.native
            def toHex(isCompressed: Boolean = true): String = js.native

        object Point:
            def fromRawBytes(bytes: Uint8Array): Point = g2.pointModule.fromRawBytes(bytes)
    end G2

    @js.native
    trait GT extends js.Object

    object GT:
        def isEquals(lhs: GT, rhs: GT): Boolean = bls12_381.fields.gtModule.isEquals(lhs, rhs)
        def multiply(lhs: GT, rhs: GT): GT = bls12_381.fields.gtModule.multiply(lhs, rhs)

    @js.native
    trait Fields extends js.Object:
        @JSName("Fp12")
        def gtModule: GTModule = js.native

    @js.native
    trait GTModule extends js.Object:
        @JSName("eql")
        def isEquals(lhs: GT, rhs: GT): Boolean = js.native
        @JSName("mul")
        def multiply(lhs: GT, rhs: GT): GT = js.native
end BLS
