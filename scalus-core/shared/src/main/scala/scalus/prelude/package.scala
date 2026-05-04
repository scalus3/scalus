package scalus

/** Re-exports common extension methods from [[scalus.cardano.onchain.plutus.prelude]] for use in
  * this package.
  */
package object prelude {
    export scalus.cardano.onchain.plutus.prelude.{`!==`, `<=>`, `===`, `?`, asScalus, list, show}

    // Export BigInt math extension methods
    export scalus.cardano.onchain.plutus.prelude.{absolute, clamp, exp2, gcf, isSqrt, log2, logarithm, maximum, minimum, pow, sqRoot}
}
