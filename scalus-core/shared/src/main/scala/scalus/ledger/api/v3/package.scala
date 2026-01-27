package scalus.ledger.api

/** Backward compatibility package for scalus.ledger.api.v3.
  *
  * All types and values have been moved to [[scalus.cardano.onchain.plutus.v3]]. This package
  * provides deprecated aliases for migration.
  */
package object v3 {
    private inline val version = "0.14.2"
    private inline val v1pkg = "scalus.cardano.onchain.plutus.v1"
    private inline val v2pkg = "scalus.cardano.onchain.plutus.v2"
    private inline val v3pkg = "scalus.cardano.onchain.plutus.v3"

    // Re-export v1/v2 types that v3 also uses (via v2 backward compat)
    // Note: Extended[A] is not available - use IntervalBoundType instead
    // Note: TxInInfo, TxId, TxOutRef are v3-specific (defined in v3.Contexts)
    export scalus.ledger.api.v2.{Address, Credential, CurrencySymbol, DCert, Datum, DatumHash, Hash, Interval, IntervalBound, IntervalBoundType, OutputDatum, POSIXTime, POSIXTimeRange, PolicyId, PosixTime, PosixTimeRange, PubKeyHash, Redeemer, RedeemerHash, ScriptHash, StakingCredential, TokenName, ValidatorHash, Value}

    // v3-specific types (v3 has its own TxId, TxOutRef, TxInInfo, Lovelace)
    @deprecated(s"Use $v3pkg.Lovelace instead", version)
    type Lovelace = scalus.cardano.onchain.plutus.v3.Lovelace

    @deprecated(s"Use $v3pkg.TxId instead", version)
    type TxId = scalus.cardano.onchain.plutus.v3.TxId

    @deprecated(s"Use $v3pkg.TxId instead", version)
    val TxId = scalus.cardano.onchain.plutus.v3.TxId

    @deprecated(s"Use $v3pkg.TxOutRef instead", version)
    type TxOutRef = scalus.cardano.onchain.plutus.v3.TxOutRef

    @deprecated(s"Use $v3pkg.TxOutRef instead", version)
    val TxOutRef = scalus.cardano.onchain.plutus.v3.TxOutRef

    @deprecated(s"Use $v3pkg.TxInInfo instead", version)
    type TxInInfo = scalus.cardano.onchain.plutus.v3.TxInInfo

    @deprecated(s"Use $v3pkg.TxInInfo instead", version)
    val TxInInfo = scalus.cardano.onchain.plutus.v3.TxInInfo

    // TxOut comes from v2
    @deprecated(s"Use $v2pkg.TxOut instead", version)
    type TxOut = scalus.cardano.onchain.plutus.v2.TxOut

    @deprecated(s"Use $v2pkg.TxOut instead", version)
    val TxOut = scalus.cardano.onchain.plutus.v2.TxOut

    // v3-specific types
    @deprecated(s"Use $v3pkg.ColdCommitteeCredential instead", version)
    type ColdCommitteeCredential = scalus.cardano.onchain.plutus.v3.ColdCommitteeCredential

    @deprecated(s"Use $v3pkg.HotCommitteeCredential instead", version)
    type HotCommitteeCredential = scalus.cardano.onchain.plutus.v3.HotCommitteeCredential

    @deprecated(s"Use $v3pkg.DRepCredential instead", version)
    type DRepCredential = scalus.cardano.onchain.plutus.v3.DRepCredential

    @deprecated(s"Use $v3pkg.DRep instead", version)
    type DRep = scalus.cardano.onchain.plutus.v3.DRep

    @deprecated(s"Use $v3pkg.Delegatee instead", version)
    type Delegatee = scalus.cardano.onchain.plutus.v3.Delegatee

    @deprecated(s"Use $v3pkg.TxCert instead", version)
    type TxCert = scalus.cardano.onchain.plutus.v3.TxCert

    @deprecated(s"Use $v3pkg.Voter instead", version)
    type Voter = scalus.cardano.onchain.plutus.v3.Voter

    @deprecated(s"Use $v3pkg.Vote instead", version)
    type Vote = scalus.cardano.onchain.plutus.v3.Vote

    @deprecated(s"Use $v3pkg.GovernanceActionId instead", version)
    type GovernanceActionId = scalus.cardano.onchain.plutus.v3.GovernanceActionId

    @deprecated(s"Use $v3pkg.Committee instead", version)
    type Committee = scalus.cardano.onchain.plutus.v3.Committee

    @deprecated(s"Use $v3pkg.Constitution instead", version)
    type Constitution = scalus.cardano.onchain.plutus.v3.Constitution

    @deprecated(s"Use $v3pkg.ProtocolVersion instead", version)
    type ProtocolVersion = scalus.cardano.onchain.plutus.v3.ProtocolVersion

    @deprecated(s"Use $v3pkg.ChangedParameters instead", version)
    type ChangedParameters = scalus.cardano.onchain.plutus.v3.ChangedParameters

    @deprecated(s"Use $v3pkg.GovernanceAction instead", version)
    type GovernanceAction = scalus.cardano.onchain.plutus.v3.GovernanceAction

    @deprecated(s"Use $v3pkg.ProposalProcedure instead", version)
    type ProposalProcedure = scalus.cardano.onchain.plutus.v3.ProposalProcedure

    @deprecated(s"Use $v3pkg.ScriptPurpose instead", version)
    type ScriptPurpose = scalus.cardano.onchain.plutus.v3.ScriptPurpose

    @deprecated(s"Use $v3pkg.ScriptInfo instead", version)
    type ScriptInfo = scalus.cardano.onchain.plutus.v3.ScriptInfo

    @deprecated(s"Use $v3pkg.TxInfo instead", version)
    type TxInfo = scalus.cardano.onchain.plutus.v3.TxInfo

    @deprecated(s"Use $v3pkg.ScriptContext instead", version)
    type ScriptContext = scalus.cardano.onchain.plutus.v3.ScriptContext

    // Companion objects
    @deprecated(s"Use $v3pkg.DRep instead", version)
    val DRep = scalus.cardano.onchain.plutus.v3.DRep

    @deprecated(s"Use $v3pkg.Delegatee instead", version)
    val Delegatee = scalus.cardano.onchain.plutus.v3.Delegatee

    @deprecated(s"Use $v3pkg.TxCert instead", version)
    val TxCert = scalus.cardano.onchain.plutus.v3.TxCert

    @deprecated(s"Use $v3pkg.Voter instead", version)
    val Voter = scalus.cardano.onchain.plutus.v3.Voter

    @deprecated(s"Use $v3pkg.Vote instead", version)
    val Vote = scalus.cardano.onchain.plutus.v3.Vote

    @deprecated(s"Use $v3pkg.GovernanceActionId instead", version)
    val GovernanceActionId = scalus.cardano.onchain.plutus.v3.GovernanceActionId

    @deprecated(s"Use $v3pkg.Committee instead", version)
    val Committee = scalus.cardano.onchain.plutus.v3.Committee

    // Constitution is a type alias, no companion object

    @deprecated(s"Use $v3pkg.ProtocolVersion instead", version)
    val ProtocolVersion = scalus.cardano.onchain.plutus.v3.ProtocolVersion

    @deprecated(s"Use $v3pkg.GovernanceAction instead", version)
    val GovernanceAction = scalus.cardano.onchain.plutus.v3.GovernanceAction

    @deprecated(s"Use $v3pkg.ProposalProcedure instead", version)
    val ProposalProcedure = scalus.cardano.onchain.plutus.v3.ProposalProcedure

    @deprecated(s"Use $v3pkg.ScriptPurpose instead", version)
    val ScriptPurpose = scalus.cardano.onchain.plutus.v3.ScriptPurpose

    @deprecated(s"Use $v3pkg.ScriptInfo instead", version)
    val ScriptInfo = scalus.cardano.onchain.plutus.v3.ScriptInfo

    @deprecated(s"Use $v3pkg.TxInfo instead", version)
    val TxInfo = scalus.cardano.onchain.plutus.v3.TxInfo

    @deprecated(s"Use $v3pkg.ScriptContext instead", version)
    val ScriptContext = scalus.cardano.onchain.plutus.v3.ScriptContext

    @deprecated(s"Use $v3pkg.Utils instead", version)
    val Utils = scalus.cardano.onchain.plutus.v3.Utils
}
