package scalus.uplc.builtin.bls12_381

import scalus.uplc.builtin.{platform, ByteString, PlatformSpecific}

/** Offchain StringContext extension for creating BLS12-381 G1 elements from compressed hex
  * bytestrings.
  *
  * @example
  *   {{{
  * import scalus.uplc.builtin.bls12_381.G1Element.g1
  *
  * // Create G1 generator
  * val g1Gen = g1"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
  *
  * // With spaces for readability
  * val g1Zero = g1"c0000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  *   }}}
  */
private[bls12_381] trait G1ElementOffchainApi {
    self: G1Element.type =>

    /** The zero (identity) element in G1.
      *
      * Works on and offchain.
      *
      * @note
      *   TODO: change to lazy val when lazy vals are implemented in the compiler
      */
    def zero: G1Element =
        platform.bls12_381_G1_uncompress(PlatformSpecific.bls12_381_G1_compressed_zero)

    /** The generator element in G1.
      *
      * Works on and offchain.
      *
      * @note
      *   TODO: change to lazy val when lazy vals are implemented in the compiler
      */
    def generator: G1Element =
        platform.bls12_381_G1_uncompress(PlatformSpecific.bls12_381_G1_compressed_generator)

    extension (sc: StringContext)
        /** G1 element string interpolator
          *
          * Works on and offchain. Converts a compressed hex bytestring (48 bytes) to a G1 element.
          *
          * @note
          *   This method is specially treated by the Scalus compiler plugin.
          */
        def g1(args: Any*): G1Element =
            val hexString = sc.s(args*).replace(" ", "")
            val bs = ByteString.fromHex(hexString)
            if bs.size != 48 then
                throw new IllegalArgumentException(
                  s"G1 compressed element must be 48 bytes, got ${bs.size}"
                )
            platform.bls12_381_G1_uncompress(bs)
}

/** Offchain StringContext extension for creating BLS12-381 G2 elements from compressed hex
  * bytestrings.
  *
  * @example
  *   {{{
  * import scalus.uplc.builtin.bls12_381.G2Element.g2
  *
  * // Create G2 generator
  * val g2Gen = g2"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
  *
  * // With spaces for readability
  * val g2Zero = g2"c000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000 0000000000000000"
  *   }}}
  */
private[bls12_381] trait G2ElementOffchainApi {
    self: G2Element.type =>

    /** The zero (identity) element in G2.
      *
      * Works on and offchain.
      *
      * @note
      *   TODO: change to lazy val when lazy vals are implemented in the compiler
      */
    def zero: G2Element =
        platform.bls12_381_G2_uncompress(PlatformSpecific.bls12_381_G2_compressed_zero)

    /** The generator element in G2.
      *
      * Works on and offchain.
      *
      * @note
      *   TODO: change to lazy val when lazy vals are implemented in the compiler
      */
    def generator: G2Element =
        platform.bls12_381_G2_uncompress(PlatformSpecific.bls12_381_G2_compressed_generator)

    extension (sc: StringContext)
        /** G2 element string interpolator
          *
          * Works on and offchain. Converts a compressed hex bytestring (96 bytes) to a G2 element.
          *
          * @note
          *   This method is specially treated by the Scalus compiler plugin.
          */
        def g2(args: Any*): G2Element =
            val hexString = sc.s(args*).replace(" ", "")
            val bs = ByteString.fromHex(hexString)
            if bs.size != 96 then
                throw new IllegalArgumentException(
                  s"G2 compressed element must be 96 bytes, got ${bs.size}"
                )
            platform.bls12_381_G2_uncompress(bs)
}
