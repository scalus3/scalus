package scalus

/** Backward compatibility package for scalus.builtin.
  *
  * Provides BLS12-381 type aliases that are still in transition.
  */
package object builtin {
    private inline val blsPkg = "scalus.uplc.builtin.bls12_381"

    // BLS12-381 types - new short names (canonical)
    type G1Element = scalus.uplc.builtin.bls12_381.G1Element
    type G2Element = scalus.uplc.builtin.bls12_381.G2Element
    type MLResult = scalus.uplc.builtin.bls12_381.MLResult

    // BLS12-381 types - old long names (deprecated, point to new package)
    @deprecated(s"Use $blsPkg.G1Element instead", "0.15.0")
    type BLS12_381_G1_Element = scalus.uplc.builtin.bls12_381.G1Element

    @deprecated(s"Use $blsPkg.G2Element instead", "0.15.0")
    type BLS12_381_G2_Element = scalus.uplc.builtin.bls12_381.G2Element

    @deprecated(s"Use $blsPkg.MLResult instead", "0.15.0")
    type BLS12_381_MlResult = scalus.uplc.builtin.bls12_381.MLResult

    // BLS12-381 companion objects - new short names (canonical)
    val G1Element = scalus.uplc.builtin.bls12_381.G1Element
    val G2Element = scalus.uplc.builtin.bls12_381.G2Element
    // Note: MLResult doesn't have a usable companion object from this package

    // BLS12-381 companion objects - old long names (deprecated, point to new package)
    @deprecated(s"Use $blsPkg.G1Element instead", "0.15.0")
    val BLS12_381_G1_Element = scalus.uplc.builtin.bls12_381.G1Element

    @deprecated(s"Use $blsPkg.G2Element instead", "0.15.0")
    val BLS12_381_G2_Element = scalus.uplc.builtin.bls12_381.G2Element
}
