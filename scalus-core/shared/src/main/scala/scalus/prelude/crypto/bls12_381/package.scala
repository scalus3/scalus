package scalus.prelude.crypto

/** Backward compatibility package for scalus.prelude.crypto.bls12_381.
  *
  * All types and values have been moved to
  * [[scalus.cardano.onchain.plutus.prelude.crypto.bls12_381]]. This package provides deprecated
  * aliases for migration.
  */
package object bls12_381 {
    private inline val version = "0.15.0"
    private inline val pkg = "scalus.cardano.onchain.plutus.prelude.crypto.bls12_381"

    @deprecated(s"Use $pkg.G1 instead", version)
    val G1 = scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.G1

    @deprecated(s"Use $pkg.G2 instead", version)
    val G2 = scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.G2

    @deprecated(s"Use $pkg.Scalar instead", version)
    val Scalar = scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.Scalar

    @deprecated(s"Use $pkg.Scalar instead", version)
    type Scalar = scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.Scalar
}
