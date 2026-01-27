package scalus.ledger.api

/** Backward compatibility package for scalus.ledger.api.v1.
  *
  * All types and values have been moved to [[scalus.cardano.onchain.plutus.v1]]. This package
  * provides deprecated aliases for migration.
  */
package object v1 {
    private inline val version = "0.14.2"
    private inline val pkg = "scalus.cardano.onchain.plutus.v1"

    // Type aliases - matching the actual types in v1/Contexts.scala
    @deprecated(s"Use $pkg.Hash instead", version)
    type Hash = scalus.cardano.onchain.plutus.v1.Hash

    @deprecated(s"Use $pkg.PubKeyHash instead", version)
    type PubKeyHash = scalus.cardano.onchain.plutus.v1.PubKeyHash

    @deprecated(s"Use $pkg.ValidatorHash instead", version)
    type ValidatorHash = scalus.cardano.onchain.plutus.v1.ValidatorHash

    @deprecated(s"Use $pkg.DatumHash instead", version)
    type DatumHash = scalus.cardano.onchain.plutus.v1.DatumHash

    @deprecated(s"Use $pkg.Redeemer instead", version)
    type Redeemer = scalus.cardano.onchain.plutus.v1.Redeemer

    @deprecated(s"Use $pkg.Datum instead", version)
    type Datum = scalus.cardano.onchain.plutus.v1.Datum

    @deprecated(s"Use $pkg.ScriptHash instead", version)
    type ScriptHash = scalus.cardano.onchain.plutus.v1.ScriptHash

    @deprecated(s"Use $pkg.RedeemerHash instead", version)
    type RedeemerHash = scalus.cardano.onchain.plutus.v1.RedeemerHash

    @deprecated(s"Use $pkg.PolicyId instead", version)
    type CurrencySymbol = scalus.cardano.onchain.plutus.v1.PolicyId

    @deprecated(s"Use $pkg.PolicyId instead", version)
    type PolicyId = scalus.cardano.onchain.plutus.v1.PolicyId

    @deprecated(s"Use $pkg.TokenName instead", version)
    type TokenName = scalus.cardano.onchain.plutus.v1.TokenName

    @deprecated(s"Use $pkg.Value instead", version)
    type Value = scalus.cardano.onchain.plutus.v1.Value

    @deprecated(s"Use $pkg.TxId instead", version)
    type TxId = scalus.cardano.onchain.plutus.v1.TxId

    @deprecated(s"Use $pkg.TxOutRef instead", version)
    type TxOutRef = scalus.cardano.onchain.plutus.v1.TxOutRef

    @deprecated(s"Use $pkg.Credential instead", version)
    type Credential = scalus.cardano.onchain.plutus.v1.Credential

    @deprecated(s"Use $pkg.StakingCredential instead", version)
    type StakingCredential = scalus.cardano.onchain.plutus.v1.StakingCredential

    @deprecated(s"Use $pkg.Address instead", version)
    type Address = scalus.cardano.onchain.plutus.v1.Address

    @deprecated(s"Use $pkg.PosixTime instead", version)
    type POSIXTime = scalus.cardano.onchain.plutus.v1.PosixTime

    @deprecated(s"Use $pkg.PosixTime instead", version)
    type PosixTime = scalus.cardano.onchain.plutus.v1.PosixTime

    @deprecated(s"Use $pkg.PosixTimeRange instead", version)
    type POSIXTimeRange = scalus.cardano.onchain.plutus.v1.PosixTimeRange

    @deprecated(s"Use $pkg.PosixTimeRange instead", version)
    type PosixTimeRange = scalus.cardano.onchain.plutus.v1.PosixTimeRange

    // Extended doesn't exist in new API, use IntervalBoundType
    // Note: Parameterized Interval[A], Extended[A], IntervalBound[A] are not supported
    // in backward compat - use the non-parameterized versions instead

    @deprecated(s"Use $pkg.IntervalBoundType instead", version)
    type IntervalBoundType = scalus.cardano.onchain.plutus.v1.IntervalBoundType

    @deprecated(s"Use $pkg.IntervalBound instead", version)
    type IntervalBound = scalus.cardano.onchain.plutus.v1.IntervalBound

    @deprecated(s"Use $pkg.Interval instead", version)
    type Interval = scalus.cardano.onchain.plutus.v1.Interval

    @deprecated(s"Use $pkg.TxOut instead", version)
    type TxOut = scalus.cardano.onchain.plutus.v1.TxOut

    @deprecated(s"Use $pkg.TxInInfo instead", version)
    type TxInInfo = scalus.cardano.onchain.plutus.v1.TxInInfo

    @deprecated(s"Use $pkg.TxInfo instead", version)
    type TxInfo = scalus.cardano.onchain.plutus.v1.TxInfo

    @deprecated(s"Use $pkg.ScriptPurpose instead", version)
    type ScriptPurpose = scalus.cardano.onchain.plutus.v1.ScriptPurpose

    @deprecated(s"Use $pkg.ScriptContext instead", version)
    type ScriptContext = scalus.cardano.onchain.plutus.v1.ScriptContext

    @deprecated(s"Use $pkg.DCert instead", version)
    type DCert = scalus.cardano.onchain.plutus.v1.DCert

    // Companion objects
    @deprecated(s"Use $pkg.Value instead", version)
    val Value = scalus.cardano.onchain.plutus.v1.Value

    @deprecated(s"Use $pkg.Credential instead", version)
    val Credential = scalus.cardano.onchain.plutus.v1.Credential

    @deprecated(s"Use $pkg.StakingCredential instead", version)
    val StakingCredential = scalus.cardano.onchain.plutus.v1.StakingCredential

    @deprecated(s"Use $pkg.Address instead", version)
    val Address = scalus.cardano.onchain.plutus.v1.Address

    @deprecated(s"Use $pkg.IntervalBoundType instead", version)
    val Extended = scalus.cardano.onchain.plutus.v1.IntervalBoundType

    @deprecated(s"Use $pkg.IntervalBound instead", version)
    val IntervalBound = scalus.cardano.onchain.plutus.v1.IntervalBound

    @deprecated(s"Use $pkg.IntervalBoundType instead", version)
    val IntervalBoundType = scalus.cardano.onchain.plutus.v1.IntervalBoundType

    @deprecated(s"Use $pkg.Interval instead", version)
    val Interval = scalus.cardano.onchain.plutus.v1.Interval

    @deprecated(s"Use $pkg.TxOutRef instead", version)
    val TxOutRef = scalus.cardano.onchain.plutus.v1.TxOutRef

    @deprecated(s"Use $pkg.TxOut instead", version)
    val TxOut = scalus.cardano.onchain.plutus.v1.TxOut

    @deprecated(s"Use $pkg.TxInInfo instead", version)
    val TxInInfo = scalus.cardano.onchain.plutus.v1.TxInInfo

    @deprecated(s"Use $pkg.TxInfo instead", version)
    val TxInfo = scalus.cardano.onchain.plutus.v1.TxInfo

    @deprecated(s"Use $pkg.ScriptPurpose instead", version)
    val ScriptPurpose = scalus.cardano.onchain.plutus.v1.ScriptPurpose

    @deprecated(s"Use $pkg.ScriptContext instead", version)
    val ScriptContext = scalus.cardano.onchain.plutus.v1.ScriptContext

    @deprecated(s"Use $pkg.DCert instead", version)
    val DCert = scalus.cardano.onchain.plutus.v1.DCert

    @deprecated(s"Use $pkg.TxId instead", version)
    val TxId = scalus.cardano.onchain.plutus.v1.TxId

    @deprecated(s"Use $pkg.PubKeyHash instead", version)
    val PubKeyHash = scalus.cardano.onchain.plutus.v1.PubKeyHash
}
