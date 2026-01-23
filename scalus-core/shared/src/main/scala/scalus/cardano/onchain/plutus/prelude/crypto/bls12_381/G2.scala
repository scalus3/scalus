package scalus.cardano.onchain.plutus.prelude.crypto.bls12_381

import scalus.Compile
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{BLS12_381_G2_Element, ByteString, PlatformSpecific}
import scalus.prelude.Eq

import scala.annotation.nowarn

@Compile
object G2 {

    given Eq[BLS12_381_G2_Element] = bls12_381_G2_equal

    /** BLS12 G2 zero element.
      *
      * This is the point at infinity in the BLS12-381 G2 group
      *
      * @note
      *   This is a function, not a constant deliberately. Being a constant would make it always
      *   initialize in your Plutus script (which is not cheap) even if the BLS12-381 is not used in
      *   the evaluated code. Hence, to avoid multiple initializations, please create a local
      *   variable in the scope where you need it, like:
      *   {{{
      *     val zeroG2 = G2.zero
      *   }}}
      */
    def zero: BLS12_381_G2_Element = uncompress(zeroCompressed)

    val zeroCompressed: ByteString = PlatformSpecific.bls12_381_G2_compressed_zero

    /** The compressed generator of the G2 group of the BLS12-381 curve.
      *
      * @note
      *   This is a function, not a constant deliberately. Being a constant would make it always
      *   initialize in your Plutus script (which is not cheap) even if the BLS12-381 is not used in
      *   the evaluated code. Hence, to avoid multiple initializations, please create a local
      *   variable in the scope where you need it, like:
      *   {{{
      *     val genG2 = G2.generator
      *   }}}
      */
    def generator: BLS12_381_G2_Element = uncompress(generatorCompressed)

    val generatorCompressed: ByteString = PlatformSpecific.bls12_381_G2_compressed_generator

    /** Uncompresses a point in the G2 group from its compressed form. */
    inline def uncompress(bs: ByteString): BLS12_381_G2_Element = {
        bls12_381_G2_uncompress(bs)
    }

    /** Hashes a [[scalus.uplc.builtin.ByteString]] to a point in the G2 group.
      *
      * @param bs
      *   The byte string to hash.
      * @param dst
      *   The domain separation tag, which should be a short byte string (up to 255 bytes).
      * @return
      *   A point in the G2 group.
      */
    inline def hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element = {
        bls12_381_G2_hashToGroup(bs, dst)
    }

    // The `+` and `unary_-` extensions must stay in shared code (not platform-specific)
    // because Scalus compiler plugin compiles this @Compile-annotated object to Plutus.
    // The plugin recognizes builtin calls like bls12_381_G2_add but not class methods.
    // On JS platform, BLS12_381_G2_Element class already has these methods, causing warnings.
    extension (self: BLS12_381_G2_Element) {

        /** Checks if two points in the G2 group are equal */
        inline def equal(rhs: BLS12_381_G2_Element): Boolean = bls12_381_G2_equal(self, rhs)

        /** Adds two points in the G2 group */
        @nowarn("msg=Extension method .* will never be selected")
        infix inline def +(rhs: BLS12_381_G2_Element): BLS12_381_G2_Element =
            bls12_381_G2_add(self, rhs)

        /** Exponentiates a point in the G2 group with a `scalar`. This operation is equivalent to
          * the repeated addition of the point with itself `e` times.
          *
          * @param scalar
          *   The scalar to multiply the point by.
          * @return
          *   A new point in the G2 group, which is the result of multiplying the original point by
          *   the scalar.
          */
        inline def scale(scalar: BigInt): BLS12_381_G2_Element = {
            bls12_381_G2_scalarMul(scalar, self)
        }

        /** Negates the point in the G2 group */
        @nowarn("msg=Extension method .* will never be selected")
        inline def unary_- : BLS12_381_G2_Element = {
            bls12_381_G2_neg(self)
        }

        /** Compresses the point in the G2 group to its compressed form.
          *
          * @return
          *   A [[scalus.uplc.builtin.ByteString]] representing the compressed point.
          */
        inline def compress: ByteString = bls12_381_G2_compress(self)
    }
}
