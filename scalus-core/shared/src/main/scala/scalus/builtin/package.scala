package scalus

/** Deprecated: use [[scalus.uplc.builtin]] and [[scalus.uplc.builtin.bls12_381]] instead.
  *
  * Kept so that code using the old BLS12-381 names still compiles.
  */
package object builtin {
    @deprecated("use scalus.uplc.builtin.bls12_381.G1Element instead", "1.3.0")
    type G1Element = scalus.uplc.builtin.bls12_381.G1Element
    @deprecated("use scalus.uplc.builtin.bls12_381.G2Element instead", "1.3.0")
    type G2Element = scalus.uplc.builtin.bls12_381.G2Element
    @deprecated("use scalus.uplc.builtin.bls12_381.MLResult instead", "1.3.0")
    type MLResult = scalus.uplc.builtin.bls12_381.MLResult

    @deprecated("use scalus.uplc.builtin.bls12_381.G1Element instead", "1.3.0")
    val G1Element = scalus.uplc.builtin.bls12_381.G1Element
    @deprecated("use scalus.uplc.builtin.bls12_381.G2Element instead", "1.3.0")
    val G2Element = scalus.uplc.builtin.bls12_381.G2Element
}
