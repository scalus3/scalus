package scalus.uplc.builtin.bls12_381

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Uint8Array

/** BLS12-381 JS bindings for the @noble/curves library */
private[bls12_381] object BLS:
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
