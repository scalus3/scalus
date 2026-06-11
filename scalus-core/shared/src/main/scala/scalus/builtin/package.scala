package scalus

/** Backward compatibility package for scalus.builtin.
  *
  * Provides BLS12-381 type aliases that are still in transition.
  */
package object builtin {
    // BLS12-381 types - new short names (canonical)
    type G1Element = scalus.uplc.builtin.bls12_381.G1Element
    type G2Element = scalus.uplc.builtin.bls12_381.G2Element
    type MLResult = scalus.uplc.builtin.bls12_381.MLResult

    // BLS12-381 companion objects - new short names (canonical)
    val G1Element = scalus.uplc.builtin.bls12_381.G1Element
    val G2Element = scalus.uplc.builtin.bls12_381.G2Element
    // Note: MLResult doesn't have a usable companion object from this package
}
