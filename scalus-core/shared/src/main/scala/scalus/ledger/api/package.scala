package scalus.ledger

/** Backward compatibility package for scalus.ledger.api.
  *
  * All types and values have been moved to [[scalus.cardano.onchain.plutus]]. This package provides
  * deprecated aliases for migration.
  */
package object api {
    private inline val version = "0.14.2"
    private inline val pkg = "scalus.cardano.onchain.plutus"

    // Union type for all script context versions
    @deprecated(
      s"Use versioned types: $pkg.v1.ScriptContext, $pkg.v2.ScriptContext, or $pkg.v3.ScriptContext instead",
      version
    )
    type ScriptContext = scalus.cardano.onchain.plutus.v1.ScriptContext |
        scalus.cardano.onchain.plutus.v2.ScriptContext |
        scalus.cardano.onchain.plutus.v3.ScriptContext

    // Re-export the ScriptContext types for each version
    @deprecated(s"Use scalus.cardano.onchain.plutus.v1.ScriptContext instead", version)
    type PlutusV1ScriptContext = scalus.cardano.onchain.plutus.v1.ScriptContext

    @deprecated(s"Use scalus.cardano.onchain.plutus.v2.ScriptContext instead", version)
    type PlutusV2ScriptContext = scalus.cardano.onchain.plutus.v2.ScriptContext

    @deprecated(s"Use scalus.cardano.onchain.plutus.v3.ScriptContext instead", version)
    type PlutusV3ScriptContext = scalus.cardano.onchain.plutus.v3.ScriptContext

    // ScriptContext companion object with foldMap
    @deprecated(s"Use $pkg.ScriptContext instead", version)
    val ScriptContext = scalus.cardano.onchain.plutus.ScriptContext
}
