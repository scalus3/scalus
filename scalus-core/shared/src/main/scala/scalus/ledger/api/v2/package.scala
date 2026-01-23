package scalus.ledger.api

/** Backward compatibility package for scalus.ledger.api.v2.
  *
  * All types and values have been moved to [[scalus.cardano.onchain.plutus.v2]]. This package
  * provides deprecated aliases for migration.
  */
package object v2 {
    private inline val version = "0.15.0"
    private inline val v1pkg = "scalus.cardano.onchain.plutus.v1"
    private inline val v2pkg = "scalus.cardano.onchain.plutus.v2"

    // Re-export all v1 types that v2 exports (via v1 backward compat)
    // Note: TxInInfo is v2-specific (uses v2.TxOut), so NOT exported from v1
    // Note: Extended[A] is not available - use IntervalBoundType instead
    export scalus.ledger.api.v1.{Address, Credential, CurrencySymbol, DCert, Datum, DatumHash, Hash, Interval, IntervalBound, IntervalBoundType, POSIXTime, POSIXTimeRange, PolicyId, PosixTime, PosixTimeRange, PubKeyHash, Redeemer, RedeemerHash, ScriptHash, StakingCredential, TokenName, TxId, TxOutRef, ValidatorHash, Value}

    // v2 reuses v1's ScriptPurpose
    @deprecated(s"Use $v1pkg.ScriptPurpose instead", version)
    type ScriptPurpose = scalus.cardano.onchain.plutus.v1.ScriptPurpose

    @deprecated(s"Use $v1pkg.ScriptPurpose instead", version)
    val ScriptPurpose = scalus.cardano.onchain.plutus.v1.ScriptPurpose

    // v2-specific types
    @deprecated(s"Use $v2pkg.OutputDatum instead", version)
    type OutputDatum = scalus.cardano.onchain.plutus.v2.OutputDatum

    @deprecated(s"Use $v2pkg.TxOut instead", version)
    type TxOut = scalus.cardano.onchain.plutus.v2.TxOut

    @deprecated(s"Use $v2pkg.TxInInfo instead", version)
    type TxInInfo = scalus.cardano.onchain.plutus.v2.TxInInfo

    @deprecated(s"Use $v2pkg.TxInfo instead", version)
    type TxInfo = scalus.cardano.onchain.plutus.v2.TxInfo

    @deprecated(s"Use $v2pkg.ScriptContext instead", version)
    type ScriptContext = scalus.cardano.onchain.plutus.v2.ScriptContext

    // Companion objects
    @deprecated(s"Use $v2pkg.OutputDatum instead", version)
    val OutputDatum = scalus.cardano.onchain.plutus.v2.OutputDatum

    @deprecated(s"Use $v2pkg.TxOut instead", version)
    val TxOut = scalus.cardano.onchain.plutus.v2.TxOut

    @deprecated(s"Use $v2pkg.TxInInfo instead", version)
    val TxInInfo = scalus.cardano.onchain.plutus.v2.TxInInfo

    @deprecated(s"Use $v2pkg.TxInfo instead", version)
    val TxInfo = scalus.cardano.onchain.plutus.v2.TxInfo

    @deprecated(s"Use $v2pkg.ScriptContext instead", version)
    val ScriptContext = scalus.cardano.onchain.plutus.v2.ScriptContext

    @deprecated(s"Use $v2pkg.Utils instead", version)
    val Utils = scalus.cardano.onchain.plutus.v2.Utils
}
