package scalus.cardano.onchain.plutus.v3

import scalus.compiler.Compile
import scala.annotation.tailrec
import scalus.compiler.{UplcRepr, UplcRepresentation}
import scalus.uplc.builtin
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.FromData
import scalus.uplc.builtin.Data.ToData
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.FromData
import scalus.uplc.builtin.ToData
import scalus.cardano.onchain.plutus.v1.*
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.builtin.ByteString.*

export scalus.cardano.onchain.plutus.v1.Hash
export scalus.cardano.onchain.plutus.v1.Address
export scalus.cardano.onchain.plutus.v1.Closure
//export scalus.cardano.onchain.plutus.v1.Credential
type Credential = scalus.cardano.onchain.plutus.v1.Credential
val Credential = scalus.cardano.onchain.plutus.v1.Credential
export scalus.cardano.onchain.plutus.v1.PolicyId
export scalus.cardano.onchain.plutus.v1.DCert
export scalus.cardano.onchain.plutus.v1.Datum
export scalus.cardano.onchain.plutus.v1.DatumHash
export scalus.cardano.onchain.plutus.v1.IntervalBoundType
export scalus.cardano.onchain.plutus.v1.Interval
export scalus.cardano.onchain.plutus.v1.IntervalBound
export scalus.cardano.onchain.plutus.v1.PosixTime
export scalus.cardano.onchain.plutus.v1.PosixTimeRange
export scalus.cardano.onchain.plutus.v1.PubKeyHash
export scalus.cardano.onchain.plutus.v1.Redeemer
export scalus.cardano.onchain.plutus.v1.RedeemerHash
export scalus.cardano.onchain.plutus.v1.ScriptHash
export scalus.cardano.onchain.plutus.v1.StakingCredential
export scalus.cardano.onchain.plutus.v1.TokenName
export scalus.cardano.onchain.plutus.v1.ValidatorHash
export scalus.cardano.onchain.plutus.v2.TxOut
//type Value = scalus.cardano.onchain.plutus.v1.Value
export scalus.cardano.onchain.plutus.v1.Value
import scalus.uplc.builtin.Builtins

@UplcRepr(UplcRepresentation.ProductCaseOneElement)
case class TxId(hash: ByteString)

@Compile
object TxId:
    given Eq[TxId] = Eq.structural((a: TxId, b: TxId) => a.hash === b.hash)
    given Ord[TxId] = (a: TxId, b: TxId) => a.hash <=> b.hash
    given FromData[TxId] = (d: Data) => TxId(d.toByteString)
    given ToData[TxId] = (x: TxId) => bData(x.hash)
    extension (sc: StringContext)
        inline def txid(args: Any*): TxId =
            TxId(sc.hex(args*))

case class TxOutRef(id: TxId, idx: BigInt)

@Compile
object TxOutRef:
    given Eq[TxOutRef] =
        Eq.structural((a: TxOutRef, b: TxOutRef) => a.id === b.id && a.idx === b.idx)
    given Ord[TxOutRef] = (a: TxOutRef, b: TxOutRef) =>
        (a.id <=> b.id) ifEqualThen (a.idx <=> b.idx)
    given FromData[TxOutRef] = FromData.derived
    given ToData[TxOutRef] = ToData.derived

    extension (self: TxOutRef)
        /** A token name unique to this output reference: `blake2b_256(serialiseData(ref.toData))`.
          *
          * The one-shot / state-token name. An off-chain mint must produce the identical bytes, so
          * compute it here on both sides. Also the natural double-satisfaction tag:
          * `OutputDatum.OutputDatum(ref.deriveTokenName.toData)`.
          */
        def deriveTokenName: TokenName = blake2b_256(serialiseData(self.toData))

type Lovelace = BigInt
type ColdCommitteeCredential = Credential

type HotCommitteeCredential = Credential

type DRepCredential = Credential

enum DRep:
    case DRep(credential: DRepCredential)
    case AlwaysAbstain
    case AlwaysNoConfidence

@Compile
object DRep:

    given Eq[scalus.cardano.onchain.plutus.v3.DRep] =
        Eq.structural: (lhs, rhs) =>
            lhs match
                case DRep(lhsCredential) =>
                    rhs match
                        case DRep(rhsCredential) => lhsCredential === rhsCredential
                        case _                   => false
                case AlwaysAbstain =>
                    rhs match
                        case AlwaysAbstain => true
                        case _             => false
                case AlwaysNoConfidence =>
                    rhs match
                        case AlwaysNoConfidence => true
                        case _                  => false

    given Ord[scalus.cardano.onchain.plutus.v3.DRep] =
        (x: scalus.cardano.onchain.plutus.v3.DRep, y: scalus.cardano.onchain.plutus.v3.DRep) =>
            x match
                case DRep(cred1) =>
                    y match
                        case DRep(cred2) => cred1 <=> cred2
                        case _           => Order.Less

                case AlwaysAbstain =>
                    y match
                        case DRep(_)       => Order.Greater
                        case AlwaysAbstain => Order.Equal
                        case _             => Order.Less

                case AlwaysNoConfidence =>
                    y match
                        case AlwaysNoConfidence => Order.Equal
                        case _                  => Order.Greater

    given FromData[scalus.cardano.onchain.plutus.v3.DRep] = FromData.derived

    given ToData[scalus.cardano.onchain.plutus.v3.DRep] = ToData.derived

end DRep

enum Delegatee:
    case Stake(pubKeyHash: PubKeyHash)
    case Vote(dRep: DRep)
    case StakeVote(pubKeyHash: PubKeyHash, dRep: DRep)

@Compile
object Delegatee:

    given Eq[Delegatee] = Eq.structural: (lhs: Delegatee, rhs: Delegatee) =>
        lhs match
            case Stake(lhsPubKeyHash) =>
                rhs match
                    case Stake(rhsPubKeyHash) => lhsPubKeyHash === rhsPubKeyHash
                    case _                    => false
            case Vote(lhsDRep) =>
                rhs match
                    case Vote(rhsDRep) => lhsDRep === rhsDRep
                    case _             => false
            case StakeVote(lhsPubKeyHash, lhsDRep) =>
                rhs match
                    case StakeVote(rhsPubKeyHash, rhsDRep) =>
                        lhsPubKeyHash === rhsPubKeyHash && lhsDRep === rhsDRep
                    case _ => false

    given Ord[Delegatee] = (x: Delegatee, y: Delegatee) =>
        x match
            case Delegatee.Stake(hash1) =>
                y match
                    case Delegatee.Stake(hash2) => hash1 <=> hash2
                    case _                      => Order.Less

            case Delegatee.Vote(drep1) =>
                y match
                    case Delegatee.Stake(_)    => Order.Greater
                    case Delegatee.Vote(drep2) => drep1 <=> drep2
                    case _                     => Order.Less

            case Delegatee.StakeVote(hash1, drep1) =>
                y match
                    case Delegatee.StakeVote(hash2, drep2) =>
                        (hash1 <=> hash2) ifEqualThen (drep1 <=> drep2)
                    case _ => Order.Greater

    given FromData[Delegatee] = FromData.derived
    given ToData[Delegatee] = ToData.derived

end Delegatee

enum TxCert:
    case RegStaking(credential: Credential, deposit: Option[Lovelace])
    case UnRegStaking(credential: Credential, refund: Option[Lovelace])
    case DelegStaking(credential: Credential, delegatee: Delegatee)
    case RegDeleg(credential: Credential, delegatee: Delegatee, deposit: Lovelace)
    case RegDRep(credential: DRepCredential, deposit: Lovelace)
    case UpdateDRep(credential: DRepCredential)
    case UnRegDRep(credential: DRepCredential, refund: Lovelace)
    case PoolRegister(poolId: PubKeyHash, poolVFR: PubKeyHash)
    case PoolRetire(pubKeyHash: PubKeyHash, epoch: BigInt)
    case AuthHotCommittee(cold: ColdCommitteeCredential, hot: HotCommitteeCredential)
    case ResignColdCommittee(cold: ColdCommitteeCredential)

@Compile
object TxCert:

    given Eq[TxCert] = Eq.structural: (lhs: TxCert, rhs: TxCert) =>
        lhs match
            case RegStaking(lhsCredential, lhsDeposit) =>
                rhs match
                    case RegStaking(rhsCredential, rhsDeposit) =>
                        lhsCredential === rhsCredential && lhsDeposit === rhsDeposit
                    case _ => false
            case UnRegStaking(lhsCredential, lhsRefund) =>
                rhs match
                    case UnRegStaking(rhsCredential, rhsRefund) =>
                        lhsCredential === rhsCredential && lhsRefund === rhsRefund
                    case _ => false
            case DelegStaking(lhsCredential, lhsDelegatee) =>
                rhs match
                    case DelegStaking(rhsCredential, rhsDelegatee) =>
                        lhsCredential === rhsCredential && lhsDelegatee === rhsDelegatee
                    case _ => false
            case RegDeleg(lhsCredential, lhsDelegatee, lhsDeposit) =>
                rhs match
                    case RegDeleg(rhsCredential, rhsDelegatee, rhsDeposit) =>
                        lhsCredential === rhsCredential && lhsDelegatee === rhsDelegatee && lhsDeposit === rhsDeposit
                    case _ => false
            case RegDRep(lhsCredential, lhsDeposit) =>
                rhs match
                    case RegDRep(rhsCredential, rhsDeposit) =>
                        lhsCredential === rhsCredential && lhsDeposit === rhsDeposit
                    case _ => false
            case UpdateDRep(lhsCredential) =>
                rhs match
                    case UpdateDRep(rhsCredential) => lhsCredential === rhsCredential
                    case _                         => false
            case UnRegDRep(lhsCredential, lhsRefund) =>
                rhs match
                    case UnRegDRep(rhsCredential, rhsRefund) =>
                        lhsCredential === rhsCredential && lhsRefund === rhsRefund
                    case _ => false
            case PoolRegister(lhsPoolId, lhsPoolVFR) =>
                rhs match
                    case PoolRegister(rhsPoolId, rhsPoolVFR) =>
                        lhsPoolId === rhsPoolId && lhsPoolVFR === rhsPoolVFR
                    case _ => false
            case PoolRetire(lhsPubKeyHash, lhsEpoch) =>
                rhs match
                    case PoolRetire(rhsPubKeyHash, rhsEpoch) =>
                        lhsPubKeyHash === rhsPubKeyHash && lhsEpoch === rhsEpoch
                    case _ => false
            case AuthHotCommittee(lhsCold, lhsHot) =>
                rhs match
                    case AuthHotCommittee(rhsCold, rhsHot) =>
                        lhsCold === rhsCold && lhsHot === rhsHot
                    case _ => false
            case ResignColdCommittee(lhsCold) =>
                rhs match
                    case ResignColdCommittee(rhsCold) => lhsCold === rhsCold
                    case _                            => false

    given Ord[TxCert] = (x: TxCert, y: TxCert) =>
        x match
            case RegStaking(cred1, dep1) =>
                y match
                    case RegStaking(cred2, dep2) =>
                        (cred1 <=> cred2) ifEqualThen (dep1 <=> dep2)
                    case _ => Order.Less

            case UnRegStaking(cred1, ref1) =>
                y match
                    case RegStaking(_, _) => Order.Greater
                    case UnRegStaking(cred2, ref2) =>
                        (cred1 <=> cred2) ifEqualThen (ref1 <=> ref2)
                    case _ => Order.Less

            case DelegStaking(cred1, del1) =>
                y match
                    case RegStaking(_, _)   => Order.Greater
                    case UnRegStaking(_, _) => Order.Greater
                    case DelegStaking(cred2, del2) =>
                        (cred1 <=> cred2) ifEqualThen (del1 <=> del2)
                    case _ => Order.Less

            case RegDeleg(cred1, del1, dep1) =>
                y match
                    case RegStaking(_, _)   => Order.Greater
                    case UnRegStaking(_, _) => Order.Greater
                    case DelegStaking(_, _) => Order.Greater
                    case RegDeleg(cred2, del2, dep2) =>
                        (cred1 <=> cred2) ifEqualThen (del1 <=> del2) ifEqualThen (dep1 <=> dep2)
                    case _ => Order.Less

            case RegDRep(cred1, dep1) =>
                y match
                    case RegStaking(_, _)   => Order.Greater
                    case UnRegStaking(_, _) => Order.Greater
                    case DelegStaking(_, _) => Order.Greater
                    case RegDeleg(_, _, _)  => Order.Greater
                    case RegDRep(cred2, dep2) =>
                        (cred1 <=> cred2) ifEqualThen (dep1 <=> dep2)
                    case _ => Order.Less

            case UpdateDRep(cred1) =>
                y match
                    case RegStaking(_, _)   => Order.Greater
                    case UnRegStaking(_, _) => Order.Greater
                    case DelegStaking(_, _) => Order.Greater
                    case RegDeleg(_, _, _)  => Order.Greater
                    case RegDRep(_, _)      => Order.Greater
                    case UpdateDRep(cred2)  => cred1 <=> cred2
                    case _                  => Order.Less

            case UnRegDRep(cred1, ref1) =>
                y match
                    case RegStaking(_, _)   => Order.Greater
                    case UnRegStaking(_, _) => Order.Greater
                    case DelegStaking(_, _) => Order.Greater
                    case RegDeleg(_, _, _)  => Order.Greater
                    case RegDRep(_, _)      => Order.Greater
                    case UpdateDRep(_)      => Order.Greater
                    case UnRegDRep(cred2, ref2) =>
                        (cred1 <=> cred2) ifEqualThen (ref1 <=> ref2)
                    case _ => Order.Less

            case PoolRegister(cred1, vfr1) =>
                y match
                    case RegStaking(_, _)   => Order.Greater
                    case UnRegStaking(_, _) => Order.Greater
                    case DelegStaking(_, _) => Order.Greater
                    case RegDeleg(_, _, _)  => Order.Greater
                    case RegDRep(_, _)      => Order.Greater
                    case UpdateDRep(_)      => Order.Greater
                    case UnRegDRep(_, _)    => Order.Greater
                    case PoolRegister(cred2, vfr2) =>
                        (cred1 <=> cred2) ifEqualThen (vfr1 <=> vfr2)
                    case _ => Order.Less

            case PoolRetire(cred1, epoch1) =>
                y match
                    case RegStaking(_, _)   => Order.Greater
                    case UnRegStaking(_, _) => Order.Greater
                    case DelegStaking(_, _) => Order.Greater
                    case RegDeleg(_, _, _)  => Order.Greater
                    case RegDRep(_, _)      => Order.Greater
                    case UpdateDRep(_)      => Order.Greater
                    case UnRegDRep(_, _)    => Order.Greater
                    case PoolRegister(_, _) => Order.Greater
                    case PoolRetire(cred2, epoch2) =>
                        (cred1 <=> cred2) ifEqualThen (epoch1 <=> epoch2)
                    case _ => Order.Less

            case AuthHotCommittee(cred1, hot1) =>
                y match
                    case RegStaking(_, _)   => Order.Greater
                    case UnRegStaking(_, _) => Order.Greater
                    case DelegStaking(_, _) => Order.Greater
                    case RegDeleg(_, _, _)  => Order.Greater
                    case RegDRep(_, _)      => Order.Greater
                    case UpdateDRep(_)      => Order.Greater
                    case UnRegDRep(_, _)    => Order.Greater
                    case PoolRegister(_, _) => Order.Greater
                    case PoolRetire(_, _)   => Order.Greater
                    case AuthHotCommittee(cred2, hot2) =>
                        (cred1 <=> cred2) ifEqualThen (hot1 <=> hot2)
                    case _ => Order.Less

            case ResignColdCommittee(cred1) =>
                y match
                    case ResignColdCommittee(cred2) => cred1 <=> cred2
                    case _                          => Order.Greater

    given FromData[TxCert] = FromData.derived
    given ToData[TxCert] = ToData.derived

end TxCert

enum Voter:
    case CommitteeVoter(credential: HotCommitteeCredential)
    case DRepVoter(credential: DRepCredential)
    case StakePoolVoter(pubKeyHash: PubKeyHash)

@Compile
object Voter:

    given Eq[Voter] = Eq.structural: (lhs: Voter, rhs: Voter) =>
        lhs match
            case CommitteeVoter(lhsCredential) =>
                rhs match
                    case CommitteeVoter(rhsCredential) => lhsCredential === rhsCredential
                    case _                             => false
            case DRepVoter(lhsCredential) =>
                rhs match
                    case DRepVoter(rhsCredential) => lhsCredential === rhsCredential
                    case _                        => false
            case StakePoolVoter(lhsPubKeyHash) =>
                rhs match
                    case StakePoolVoter(rhsPubKeyHash) => lhsPubKeyHash === rhsPubKeyHash
                    case _                             => false

    given Ord[Voter] = (x: Voter, y: Voter) =>
        x match
            case CommitteeVoter(cred1) =>
                y match
                    case CommitteeVoter(cred2) => cred1 <=> cred2
                    case _                     => Order.Less

            case DRepVoter(cred1) =>
                y match
                    case CommitteeVoter(_) => Order.Greater
                    case DRepVoter(cred2)  => cred1 <=> cred2
                    case _                 => Order.Less

            case StakePoolVoter(pub1) =>
                y match
                    case StakePoolVoter(pub2) => pub1 <=> pub2
                    case _                    => Order.Greater

    given ToData[Voter] = ToData.derived
    given FromData[Voter] = FromData.derived

end Voter

enum Vote:
    case No, Yes, Abstain

@Compile
object Vote:

    given Eq[Vote] = Eq.structural: (lhs: Vote, rhs: Vote) =>
        lhs match
            case No =>
                rhs match
                    case No => true
                    case _  => false
            case Yes =>
                rhs match
                    case Yes => true
                    case _   => false
            case Abstain =>
                rhs match
                    case Abstain => true
                    case _       => false

    given Ord[Vote] = (x: Vote, y: Vote) =>
        x match
            case No =>
                y match
                    case No => Order.Equal
                    case _  => Order.Less

            case Yes =>
                y match
                    case No  => Order.Greater
                    case Yes => Order.Equal
                    case _   => Order.Less

            case Abstain =>
                y match
                    case Abstain => Order.Equal
                    case _       => Order.Greater

    given ToData[Vote] = ToData.derived
    given FromData[Vote] = FromData.derived

end Vote

case class GovernanceActionId(txId: TxId, govActionIx: BigInt)

@Compile
object GovernanceActionId:

    given Eq[GovernanceActionId] = Eq.structural:
        (lhs: GovernanceActionId, rhs: GovernanceActionId) =>
            lhs.txId === rhs.txId && lhs.govActionIx === rhs.govActionIx

    given Ord[GovernanceActionId] = (x: GovernanceActionId, y: GovernanceActionId) =>
        (x.txId <=> y.txId) ifEqualThen (x.govActionIx <=> y.govActionIx)

    given FromData[GovernanceActionId] = FromData.derived
    given ToData[GovernanceActionId] = ToData.derived

end GovernanceActionId

case class Committee(
    members: SortedMap[ColdCommitteeCredential, BigInt],
    quorum: BigInt
)

@Compile
object Committee:
    given Eq[Committee] = Eq.structural: (lhs: Committee, rhs: Committee) =>
        lhs.members === rhs.members && lhs.quorum === rhs.quorum

    given Ord[Committee] = (x: Committee, y: Committee) =>
        (x.members <=> y.members) ifEqualThen (x.quorum <=> y.quorum)

    given FromData[Committee] = FromData.derived
    given ToData[Committee] = ToData.derived

end Committee

type Constitution = Option[ScriptHash]

case class ProtocolVersion(pvMajor: BigInt, pvMinor: BigInt)

@Compile
object ProtocolVersion:
    given Eq[ProtocolVersion] = Eq.structural: (lhs: ProtocolVersion, rhs: ProtocolVersion) =>
        lhs.pvMajor === rhs.pvMajor && lhs.pvMinor === rhs.pvMinor

    given Ord[ProtocolVersion] = (x: ProtocolVersion, y: ProtocolVersion) =>
        (x.pvMajor <=> y.pvMajor) ifEqualThen (x.pvMinor <=> y.pvMinor)

    given FromData[ProtocolVersion] = FromData.derived
    given ToData[ProtocolVersion] = ToData.derived

end ProtocolVersion

type ChangedParameters = Data

enum GovernanceAction:
    case ParameterChange(
        id: Option[GovernanceActionId],
        parameters: ChangedParameters,
        constitutionScript: Option[ScriptHash]
    )
    case HardForkInitiation(id: Option[GovernanceActionId], protocolVersion: ProtocolVersion)
    case TreasuryWithdrawals(
        withdrawals: SortedMap[Credential, Lovelace],
        constitutionScript: Option[ScriptHash]
    )
    case NoConfidence(id: Option[GovernanceActionId])
    case UpdateCommittee(
        id: Option[GovernanceActionId],
        removedMembers: List[ColdCommitteeCredential],
        addedMembers: SortedMap[ColdCommitteeCredential, BigInt],
        newQuorum: Rational
    )
    case NewConstitution(id: Option[GovernanceActionId], constitution: Constitution)
    case InfoAction

@Compile
object GovernanceAction:
    given Eq[GovernanceAction] = Eq.structural: (lhs: GovernanceAction, rhs: GovernanceAction) =>
        lhs match
            case ParameterChange(lhsId, lhsParameters, lhsConstitutionScript) =>
                rhs match
                    case ParameterChange(rhsId, rhsParameters, rhsConstitutionScript) =>
                        lhsId === rhsId && lhsParameters === rhsParameters
                        && lhsConstitutionScript === rhsConstitutionScript
                    case _ => false
            case HardForkInitiation(lhsId, lhsProtocolVersion) =>
                rhs match
                    case HardForkInitiation(rhsId, rhsProtocolVersion) =>
                        lhsId === rhsId && lhsProtocolVersion === rhsProtocolVersion
                    case _ => false
            case TreasuryWithdrawals(lhsWithdrawals, lhsConstitutionScript) =>
                rhs match
                    case TreasuryWithdrawals(rhsWithdrawals, rhsConstitutionScript) =>
                        lhsWithdrawals === rhsWithdrawals && lhsConstitutionScript === rhsConstitutionScript
                    case _ => false
            case NoConfidence(lhsId) =>
                rhs match
                    case NoConfidence(rhsId) => lhsId === rhsId
                    case _                   => false
            case UpdateCommittee(lhsId, lhsRemovedMembers, lhsAddedMembers, lhsNewQuorum) =>
                rhs match
                    case UpdateCommittee(rhsId, rhsRemovedMembers, rhsAddedMembers, rhsNewQuorum) =>
                        lhsId === rhsId && lhsRemovedMembers === rhsRemovedMembers
                        && lhsAddedMembers === rhsAddedMembers
                        // Rational has no Eq. This is a Data-faithful ledger type, so compare the
                        // quorum structurally (numerator+denominator) — what a derived Eq would do,
                        // and matching the historical behavior. (Not RationalEq.equals, which would
                        // be value/cross-multiply equality.)
                        && lhsNewQuorum.numerator === rhsNewQuorum.numerator
                        && lhsNewQuorum.denominator === rhsNewQuorum.denominator
                    case _ => false
            case NewConstitution(lhsId, lhsConstitution) =>
                rhs match
                    case NewConstitution(rhsId, rhsConstitution) =>
                        lhsId === rhsId
                        && lhsConstitution === rhsConstitution
                    case _ => false
            case InfoAction =>
                rhs match
                    case InfoAction => true
                    case _          => false

    given Ord[GovernanceAction] = (x: GovernanceAction, y: GovernanceAction) =>
        x match
            case ParameterChange(id1, params1, const1) =>
                y match
                    case ParameterChange(id2, params2, const2) =>
                        (id1 <=> id2) ifEqualThen (params1 <=> params2) ifEqualThen (const1 <=> const2)
                    case _ => Order.Less

            case HardForkInitiation(id1, pv1) =>
                y match
                    case ParameterChange(_, _, _) => Order.Greater
                    case HardForkInitiation(id2, pv2) =>
                        (id1 <=> id2) ifEqualThen (pv1 <=> pv2)
                    case _ => Order.Less

            case TreasuryWithdrawals(w1, c1) =>
                y match
                    case ParameterChange(_, _, _) => Order.Greater
                    case HardForkInitiation(_, _) => Order.Greater
                    case TreasuryWithdrawals(w2, c2) =>
                        (w1 <=> w2) ifEqualThen (c1 <=> c2)
                    case _ => Order.Less

            case NoConfidence(id1) =>
                y match
                    case ParameterChange(_, _, _)  => Order.Greater
                    case HardForkInitiation(_, _)  => Order.Greater
                    case TreasuryWithdrawals(_, _) => Order.Greater
                    case NoConfidence(id2)         => id1 <=> id2
                    case _                         => Order.Less

            case UpdateCommittee(id1, rm1, add1, q1) =>
                y match
                    case ParameterChange(_, _, _)  => Order.Greater
                    case HardForkInitiation(_, _)  => Order.Greater
                    case TreasuryWithdrawals(_, _) => Order.Greater
                    case NoConfidence(_)           => Order.Greater
                    case UpdateCommittee(id2, rm2, add2, q2) =>
                        (id1 <=> id2) ifEqualThen (rm1 <=> rm2) ifEqualThen (add1 <=> add2) ifEqualThen (q1 <=> q2)
                    case _ => Order.Less

            case NewConstitution(id1, const1) =>
                y match
                    case ParameterChange(_, _, _)    => Order.Greater
                    case HardForkInitiation(_, _)    => Order.Greater
                    case TreasuryWithdrawals(_, _)   => Order.Greater
                    case NoConfidence(_)             => Order.Greater
                    case UpdateCommittee(_, _, _, _) => Order.Greater
                    case NewConstitution(id2, const2) =>
                        (id1 <=> id2) ifEqualThen (const1 <=> const2)
                    case _ => Order.Less

            case InfoAction =>
                y match
                    case InfoAction => Order.Equal
                    case _          => Order.Greater

    given FromData[GovernanceAction] = FromData.derived
    given ToData[GovernanceAction] = ToData.derived

end GovernanceAction

case class ProposalProcedure(
    deposit: Lovelace,
    returnAddress: Credential,
    governanceAction: GovernanceAction
)

@Compile
object ProposalProcedure:

    given Eq[ProposalProcedure] = Eq.structural: (lhs: ProposalProcedure, rhs: ProposalProcedure) =>
        lhs.deposit === rhs.deposit && lhs.returnAddress === rhs.returnAddress
            && lhs.governanceAction === rhs.governanceAction

    given Ord[ProposalProcedure] = (x: ProposalProcedure, y: ProposalProcedure) =>
        (x.deposit <=> y.deposit) ifEqualThen
            (x.returnAddress <=> y.returnAddress) ifEqualThen
            (x.governanceAction <=> y.governanceAction)

    given ToData[ProposalProcedure] = ToData.derived
    given FromData[ProposalProcedure] = FromData.derived

end ProposalProcedure

enum ScriptPurpose:
    case Minting(policyId: PolicyId)
    case Spending(txOutRef: TxOutRef)
    case Rewarding(credential: Credential)
    case Certifying(index: BigInt, cert: TxCert)
    case Voting(voter: Voter)
    case Proposing(index: BigInt, procedure: ProposalProcedure)

@Compile
object ScriptPurpose:
    given Eq[ScriptPurpose] = Eq.structural: (lhs: ScriptPurpose, rhs: ScriptPurpose) =>
        lhs match
            case Minting(lhsCurrencySymbol) =>
                rhs match
                    case Minting(rhsCurrencySymbol) => lhsCurrencySymbol === rhsCurrencySymbol
                    case _                          => false
            case Spending(lhsTxOutRef) =>
                rhs match
                    case Spending(rhsTxOutRef) => lhsTxOutRef === rhsTxOutRef
                    case _                     => false
            case Rewarding(lhsCredential) =>
                rhs match
                    case Rewarding(rhsCredential) => lhsCredential === rhsCredential
                    case _                        => false
            case Certifying(lhsIndex, lhsCert) =>
                rhs match
                    case Certifying(rhsIndex, rhsCert) =>
                        lhsIndex === rhsIndex && lhsCert === rhsCert
                    case _ => false
            case Voting(lhsVoter) =>
                rhs match
                    case Voting(rhsVoter) => lhsVoter === rhsVoter
                    case _                => false
            case Proposing(lhsIndex, lhsProcedure) =>
                rhs match
                    case Proposing(rhsIndex, rhsProcedure) =>
                        lhsIndex === rhsIndex && lhsProcedure === rhsProcedure
                    case _ => false

    given Ord[ScriptPurpose] = (x: ScriptPurpose, y: ScriptPurpose) =>
        x match
            case Minting(cs1) =>
                y match
                    case Minting(cs2) => cs1 <=> cs2
                    case _            => Order.Less

            case Spending(ref1) =>
                y match
                    case Minting(_)     => Order.Greater
                    case Spending(ref2) => ref1 <=> ref2
                    case _              => Order.Less

            case Rewarding(cred1) =>
                y match
                    case Minting(_)       => Order.Greater
                    case Spending(_)      => Order.Greater
                    case Rewarding(cred2) => cred1 <=> cred2
                    case _                => Order.Less

            case Certifying(idx1, cert1) =>
                y match
                    case Minting(_)   => Order.Greater
                    case Spending(_)  => Order.Greater
                    case Rewarding(_) => Order.Greater
                    case Certifying(idx2, cert2) =>
                        (idx1 <=> idx2) ifEqualThen (cert1 <=> cert2)
                    case _ => Order.Less

            case Voting(voter1) =>
                y match
                    case Minting(_)       => Order.Greater
                    case Spending(_)      => Order.Greater
                    case Rewarding(_)     => Order.Greater
                    case Certifying(_, _) => Order.Greater
                    case Voting(voter2)   => voter1 <=> voter2
                    case _                => Order.Less

            case Proposing(idx1, proc1) =>
                y match
                    case Proposing(idx2, proc2) =>
                        (idx1 <=> idx2) ifEqualThen (proc1 <=> proc2)
                    case _ => Order.Greater

    given FromData[ScriptPurpose] = FromData.derived
    given ToData[ScriptPurpose] = ToData.derived

end ScriptPurpose

enum ScriptInfo:
    case MintingScript(policyId: PolicyId)
    case SpendingScript(txOutRef: TxOutRef, datum: Option[Datum] = Option.None)
    case RewardingScript(credential: Credential)
    case CertifyingScript(index: BigInt, cert: TxCert)
    case VotingScript(voter: Voter)
    case ProposingScript(index: BigInt, procedure: ProposalProcedure)

@Compile
object ScriptInfo:
    given Eq[ScriptInfo] = Eq.structural: (lhs: ScriptInfo, rhs: ScriptInfo) =>
        lhs match
            case MintingScript(lhsCurrencySymbol) =>
                rhs match
                    case MintingScript(rhsCurrencySymbol) => lhsCurrencySymbol === rhsCurrencySymbol
                    case _                                => false
            case SpendingScript(lhsTxOutRef, lhsDatum) =>
                rhs match
                    case SpendingScript(rhsTxOutRef, rhsDatum) =>
                        lhsTxOutRef === rhsTxOutRef && lhsDatum === rhsDatum
                    case _ => false
            case RewardingScript(lhsCredential) =>
                rhs match
                    case RewardingScript(rhsCredential) => lhsCredential === rhsCredential
                    case _                              => false
            case CertifyingScript(lhsIndex, lhsCert) =>
                rhs match
                    case CertifyingScript(rhsIndex, rhsCert) =>
                        lhsIndex === rhsIndex && lhsCert === rhsCert
                    case _ => false
            case VotingScript(lhsVoter) =>
                rhs match
                    case VotingScript(rhsVoter) => lhsVoter === rhsVoter
                    case _                      => false
            case ProposingScript(lhsIndex, lhsProcedure) =>
                rhs match
                    case ProposingScript(rhsIndex, rhsProcedure) =>
                        lhsIndex === rhsIndex && lhsProcedure === rhsProcedure
                    case _ => false

    given Ord[ScriptInfo] = (x: ScriptInfo, y: ScriptInfo) =>
        x match
            case MintingScript(cs1) =>
                y match
                    case MintingScript(cs2) => cs1 <=> cs2
                    case _                  => Order.Less

            case SpendingScript(ref1, dat1) =>
                y match
                    case MintingScript(_) => Order.Greater
                    case SpendingScript(ref2, dat2) =>
                        (ref1 <=> ref2) ifEqualThen (dat1 <=> dat2)
                    case _ => Order.Less

            case RewardingScript(cred1) =>
                y match
                    case MintingScript(_)       => Order.Greater
                    case SpendingScript(_, _)   => Order.Greater
                    case RewardingScript(cred2) => cred1 <=> cred2
                    case _                      => Order.Less

            case CertifyingScript(idx1, cert1) =>
                y match
                    case MintingScript(_)     => Order.Greater
                    case SpendingScript(_, _) => Order.Greater
                    case RewardingScript(_)   => Order.Greater
                    case CertifyingScript(idx2, cert2) =>
                        (idx1 <=> idx2) ifEqualThen (cert1 <=> cert2)
                    case _ => Order.Less

            case VotingScript(voter1) =>
                y match
                    case MintingScript(_)       => Order.Greater
                    case SpendingScript(_, _)   => Order.Greater
                    case RewardingScript(_)     => Order.Greater
                    case CertifyingScript(_, _) => Order.Greater
                    case VotingScript(voter2)   => voter1 <=> voter2
                    case _                      => Order.Less

            case ProposingScript(idx1, proc1) =>
                y match
                    case ProposingScript(idx2, proc2) =>
                        (idx1 <=> idx2) ifEqualThen (proc1 <=> proc2)
                    case _ => Order.Greater

    given FromData[ScriptInfo] = FromData.derived
    given ToData[ScriptInfo] = ToData.derived

end ScriptInfo

case class TxInInfo(
    outRef: TxOutRef,
    resolved: v2.TxOut
)

@Compile
object TxInInfo:
    given Eq[TxInInfo] = Eq.structural: (a: TxInInfo, b: TxInInfo) =>
        a.outRef === b.outRef && a.resolved === b.resolved

    given Ord[TxInInfo] = (x: TxInInfo, y: TxInInfo) =>
        (x.outRef <=> y.outRef) ifEqualThen (x.resolved <=> y.resolved)

    given FromData[TxInInfo] = FromData.derived
    given ToData[TxInInfo] = ToData.derived

end TxInInfo

case class TxInfo(
    inputs: List[TxInInfo],
    referenceInputs: List[TxInInfo] = List.Nil,
    outputs: List[v2.TxOut] = List.Nil,
    fee: Lovelace = 0,
    mint: Value = Value.zero,
    certificates: List[TxCert] = List.Nil,
    withdrawals: SortedMap[Credential, Lovelace] = SortedMap.empty,
    validRange: Interval = Interval.always,
    signatories: List[PubKeyHash] = List.Nil,
    redeemers: SortedMap[ScriptPurpose, Redeemer] = SortedMap.empty,
    data: SortedMap[DatumHash, Datum] = SortedMap.empty,
    id: TxId,
    votes: SortedMap[Voter, SortedMap[GovernanceActionId, Vote]] = SortedMap.empty,
    proposalProcedures: List[ProposalProcedure] = List.Nil,
    currentTreasuryAmount: Option[Lovelace] = Option.None,
    treasuryDonation: Option[Lovelace] = Option.None
)

@Compile
object TxInfo {

    given Eq[TxInfo] = Eq.structural: (lhs: TxInfo, rhs: TxInfo) =>
        lhs.inputs === rhs.inputs &&
            lhs.referenceInputs === rhs.referenceInputs &&
            lhs.outputs === rhs.outputs &&
            lhs.fee === rhs.fee &&
            lhs.mint === rhs.mint &&
            lhs.certificates === rhs.certificates &&
            lhs.withdrawals === rhs.withdrawals &&
            lhs.validRange === rhs.validRange &&
            lhs.signatories === rhs.signatories &&
            lhs.redeemers === rhs.redeemers &&
            lhs.data === rhs.data &&
            lhs.id === rhs.id &&
            lhs.votes === rhs.votes &&
            lhs.proposalProcedures === rhs.proposalProcedures &&
            lhs.currentTreasuryAmount === rhs.currentTreasuryAmount &&
            lhs.treasuryDonation === rhs.treasuryDonation

    given Ord[TxInfo] = (x: TxInfo, y: TxInfo) =>
        given Ord[Value] = Value.valueOrd
        (x.inputs <=> y.inputs) ifEqualThen
            (x.referenceInputs <=> y.referenceInputs) ifEqualThen
            (x.outputs <=> y.outputs) ifEqualThen
            (x.fee <=> y.fee) ifEqualThen
            (x.mint <=> y.mint) ifEqualThen
            (x.certificates <=> y.certificates) ifEqualThen
            (x.withdrawals <=> y.withdrawals) ifEqualThen
            (x.validRange <=> y.validRange) ifEqualThen
            (x.signatories <=> y.signatories) ifEqualThen
            (x.redeemers <=> y.redeemers) ifEqualThen
            (x.data <=> y.data) ifEqualThen
            (x.id <=> y.id) ifEqualThen
            (x.votes <=> y.votes) ifEqualThen
            (x.proposalProcedures <=> y.proposalProcedures) ifEqualThen
            (x.currentTreasuryAmount <=> y.currentTreasuryAmount) ifEqualThen
            (x.treasuryDonation <=> y.treasuryDonation)

    given FromData[TxInfo] = FromData.derived
    given ToData[TxInfo] = ToData.derived

    val placeholder: TxInfo = TxInfo(
      inputs = List.empty,
      id = TxId(hex"0000000000000000000000000000000000000000000000000000000000000000")
    )

    extension (self: TxInfo) {

        /** Finds a transaction input by its output reference.
          *
          * @return
          *   `Some(TxInInfo)` if an input spends `outRef`, `None` otherwise
          */
        def findInput(outRef: TxOutRef): Option[TxInInfo] = Utils.findInput(self.inputs, outRef)

        /** Finds a transaction input by its output reference, or fails with `message`.
          *
          * A direct scan with no intermediate `Option`. As a statement it is also the one-shot
          * check: `tx.findInputOrFail(seed, "seed must be spent")`.
          *
          * @example
          *   {{{
          *   val ownInput = tx.findInputOrFail(ownRef, "own input not found")
          *   }}}
          */
        inline def findInputOrFail(
            outRef: TxOutRef,
            inline message: String = "Tx input not found"
        ): TxInInfo = {
            // Inlined on purpose: a non-inline scan taking the failure as a continuation (the
            // findUniqueOrElse shape) measured worse here on every example, including one with
            // four call sites, and grew the CAPE scripts by 22 B each.
            @tailrec
            def go(inputs: List[TxInInfo]): TxInInfo = inputs match
                case List.Nil => fail(message)
                case List.Cons(input, tail) =>
                    if input.outRef === outRef then input else go(tail)
            go(self.inputs)
        }

        /** Finds a datum by its hash in this transaction's outputs or datum witness map. */
        def findDatum(datumHash: DatumHash): Option[Datum] =
            Utils.findDatum(self.outputs, self.data, datumHash)

        /** Finds all outputs whose payment credential is the given script hash.
          *
          * Matches the PAYMENT credential only: the staking part of the address is not constrained.
          * Do not use this to locate the continuing output of a script - an attacker can keep the
          * payment credential and redirect the delegation rewards. Use
          * [[findContinuingOutputOrFail]], which compares the whole address.
          */
        def findOutputsByScriptHash(scriptHash: ValidatorHash): List[v2.TxOut] =
            Utils.findScriptOutputs(self.outputs, scriptHash)

        /** Finds all inputs whose resolved address has the given payment credential.
          *
          * Matches the payment credential only; the staking part is not constrained.
          */
        def findInputsByCredential(cred: Credential): List[TxInInfo] =
            self.inputs.filter(_.resolved.address.credential === cred)

        /** Finds all outputs whose address has the given payment credential.
          *
          * Matches the PAYMENT credential only: the staking part of the address is not constrained.
          * Do not use this to locate the continuing output of a script - an attacker can keep the
          * payment credential and redirect the delegation rewards. Use
          * [[findContinuingOutputOrFail]], which compares the whole address.
          */
        def findOutputsByCredential(cred: Credential): List[v2.TxOut] =
            self.outputs.filter(_.address.credential === cred)

        /** Finds the unique output paying back to `ownInput`'s address, or fails with `message`.
          *
          * Compares the WHOLE address, staking credential included. The credential-only finders
          * ([[findOutputsByScriptHash]], [[findOutputsByCredential]]) leave the staking part
          * unconstrained, which lets an attacker redirect the continuing output's delegation
          * rewards. Fails when no output or more than one output pays to that address.
          *
          * @example
          *   {{{
          *   val ownInput = tx.findInputOrFail(ownRef)
          *   val continuing = tx.findContinuingOutputOrFail(ownInput, "expected one continuing output")
          *   require(continuing.value.hasSameTokensAndAtLeastAda(ownInput.resolved.value), "value")
          *   }}}
          */
        inline def findContinuingOutputOrFail(
            ownInput: TxInInfo,
            inline message: String
        ): v2.TxOut =
            self.outputs.findUniqueOrFail(_.address === ownInput.resolved.address, message)

        /** Total value paid to `addr` across all outputs, as a whole `Value`.
          *
          * Compares the whole address. Sums every asset, not only lovelace; project with
          * `.getLovelace` afterwards when only ADA matters.
          */
        def valuePaidTo(addr: Address): Value =
            self.outputs.foldLeft(Value.zero) { (acc, out) =>
                if out.address === addr then acc + out.value else acc
            }

        /** Total value spent from `addr` across all inputs, as a whole `Value`.
          *
          * Compares the whole address. Sums every asset, not only lovelace.
          */
        def valueSpentFrom(addr: Address): Value =
            self.inputs.foldLeft(Value.zero) { (acc, input) =>
                if input.resolved.address === addr then acc + input.resolved.value else acc
            }

        /** Deprecated spelling of [[findInput]]. Keeps its original body so existing budgets do not
          * move until callers migrate.
          */
        @deprecated("use findInput", "1.1.1")
        def findOwnInput(outRef: TxOutRef): Option[TxInInfo] = Utils.findInput(self.inputs, outRef)

        /** Deprecated spelling of [[findInputOrFail]]. Keeps its original `Option`-based body so
          * the budgets of existing callers do not move until they migrate.
          */
        @deprecated("use findInputOrFail", "1.1.1")
        inline def findOwnInputOrFail(
            outRef: TxOutRef,
            inline message: String = "Tx input not found"
        ): TxInInfo = findInput(outRef).getOrFail(message)

        /** Deprecated spelling of [[findDatum]]; original body kept. */
        @deprecated("use findDatum", "1.1.1")
        def findOwnDatum(datumHash: DatumHash): Option[Datum] =
            Utils.findDatum(self.outputs, self.data, datumHash)

        /** Deprecated spelling of [[findOutputsByScriptHash]]; original body kept. */
        @deprecated("use findOutputsByScriptHash", "1.1.1")
        def findOwnScriptOutputs(scriptHash: ValidatorHash): List[v2.TxOut] =
            Utils.findScriptOutputs(self.outputs, scriptHash)

        /** Deprecated spelling of [[findInputsByCredential]]; original body kept. */
        @deprecated("use findInputsByCredential", "1.1.1")
        def findOwnInputsByCredential(cred: Credential): List[TxInInfo] =
            self.inputs.filter(_.resolved.address.credential === cred)

        /** Deprecated spelling of [[findOutputsByCredential]]; original body kept. */
        @deprecated("use findOutputsByCredential", "1.1.1")
        def findOwnOutputsByCredential(cred: Credential): List[v2.TxOut] =
            self.outputs.filter(_.address.credential === cred)

        /** Same as `inputs.filter(pred)`. */
        @deprecated("use inputs.filter", "1.1.1")
        def findOwnInputs(pred: TxInInfo => Boolean): List[TxInInfo] =
            self.inputs.filter(pred)

        /** Same as `outputs.filter(pred)`. */
        @deprecated("use outputs.filter", "1.1.1")
        def findOwnOutputs(pred: v2.TxOut => Boolean): List[v2.TxOut] =
            self.outputs.filter(pred)

        /** @return
          *   `true` if the transaction signatories list includes the given keyHash, `false`
          *   otherwise
          */
        def isSignedBy(pubKeyHash: PubKeyHash): Boolean =
            self.signatories.contains(pubKeyHash)

        /** `true` if any of `keys` signed the transaction.
          *
          * A direct scan; `keys.exists(tx.isSignedBy)` allocates an `Option` per call.
          */
        def isSignedByAny(keys: List[PubKeyHash]): Boolean = {
            @tailrec
            def go(remaining: List[PubKeyHash]): Boolean = remaining match
                case List.Nil => false
                case List.Cons(key, tail) =>
                    if self.signatories.contains(key) then true else go(tail)
            go(keys)
        }

        /** The validity range's lower bound, or fails with `message` if it is unbounded.
          *
          * INCLUSIVE: the ledger builds it with `lowerBound` (closed), so this is the earliest time
          * at which the transaction can be included. It is the value to compare against a "not
          * before" deadline: `require(tx.validFromOrFail(msg) >= deadline, "too early")`.
          *
          * Pairs with `TxBuilder.validFrom`. Replaces `getValidityStartTime`, which returned `0`
          * for an unbounded range and let a transaction with no lower bound pass every deadline.
          */
        inline def validFromOrFail(inline message: String): PosixTime =
            self.validRange.from.boundType match
                case IntervalBoundType.Finite(t) => t
                case _                           => fail(message)

        /** The validity range's upper bound, or fails with `message` if it is unbounded.
          *
          * EXCLUSIVE: the ledger builds it with `strictUpperBound` (open), so the transaction
          * cannot be included at or after this time. A timestamp written into a datum from this
          * value is an upper bound on the real posting time - it can be late, never early. It is
          * the value to compare against a "not after" deadline:
          * `require(tx.validToOrFail(msg) <= deadline, "too late")`.
          *
          * Pairs with `TxBuilder.validTo`.
          */
        inline def validToOrFail(inline message: String): PosixTime =
            self.validRange.to.boundType match
                case IntervalBoundType.Finite(t) => t
                case _                           => fail(message)

        /** `true` if at least one token is minted or burned under `policy` and every quantity under
          * it is negative.
          *
          * Not `mint.tokens(policy).forall(_._2 < 0)`: `forall` over an EMPTY map is vacuously
          * `true`, so that spelling passes a transaction that burns nothing at all. An auction that
          * closes by burning its state NFT with the unguarded check lets a `Close` that mints
          * nothing pay out while the NFT survives for replay. The emptiness guard is the operation.
          * When the token name is known, `mint.hasOnly(policy, name, -1)` is stronger.
          */
        def onlyBurnsUnder(policy: PolicyId): Boolean = {
            val burned = self.mint.tokens(policy)
            burned.nonEmpty && burned.forall(_._2 < 0)
        }

        /** `true` if some output pays EXACTLY `value` to `addr` and carries `tag` as its datum.
          *
          * The tagged-output defence against double satisfaction: with a `TxOutRef`-derived tag
          * (see `TxOutRef.deriveTokenName`) no single output can satisfy two script instances. All
          * three comparisons are exact; a `>=` on the value would reopen the hole.
          */
        def hasPaidTagged(addr: Address, value: Value, tag: v2.OutputDatum): Boolean = {
            @tailrec
            def go(outputs: List[v2.TxOut]): Boolean = outputs match
                case List.Nil => false
                case List.Cons(out, tail) =>
                    if out.address === addr && out.value === value && out.datum === tag then true
                    else go(tail)
            go(self.outputs)
        }

        /** Extracts the start time from the transaction's validity interval, or `0` if the range
          * has no finite lower bound.
          *
          * A transaction with NO lower bound is then treated as happening at the Unix epoch, and
          * every "has the deadline passed?" comparison flips. Use [[validFromOrFail]].
          */
        @deprecated("use validFromOrFail", "1.1.1")
        def getValidityStartTime: BigInt = self.validRange.from.boundType match
            case IntervalBoundType.Finite(t) => t
            case _                           => BigInt(0)
    }
}

case class ScriptContext(
    txInfo: TxInfo,
    redeemer: Redeemer = Builtins.constrData(BigInt(0), Builtins.mkNilData()),
    scriptInfo: ScriptInfo
)

@Compile
object ScriptContext {
    given Eq[ScriptContext] = Eq.structural: (lhs: ScriptContext, rhs: ScriptContext) =>
        lhs.txInfo === rhs.txInfo && lhs.redeemer === rhs.redeemer && lhs.scriptInfo === rhs.scriptInfo

    given Ord[ScriptContext] = (x: ScriptContext, y: ScriptContext) =>
        (x.txInfo <=> y.txInfo) ifEqualThen
            (x.redeemer <=> y.redeemer) ifEqualThen
            (x.scriptInfo <=> y.scriptInfo)

    given FromData[ScriptContext] = FromData.derived
    given ToData[ScriptContext] = ToData.derived

}

@Compile
object Utils {

    /** Finds an input in the list of inputs by its out reference.
      *
      * @param inputs
      *   The list of inputs to search in.
      * @param outRef
      *   The output reference to find.
      * @return
      *   An `Option` containing the found input, or `None` if not found.
      */
    def findInput(inputs: List[TxInInfo], outRef: TxOutRef): Option[TxInInfo] = {
        inputs.find(_.outRef === outRef)
    }

    export scalus.cardano.onchain.plutus.v2.Utils.{findDatum, findScriptOutputs}

    /** Calculates the total amount of ADA (Lovelace) from a list of transaction outputs.
      *
      * LOVELACE ONLY: native tokens in the outputs are ignored, so a check built on this sum lets
      * them be stripped. Use `TxInfo.valuePaidTo(addr)` and compare the whole `Value`.
      *
      * @param outputs
      *   the list of transaction outputs to sum
      * @return
      *   the total Lovelace amount from all outputs
      * @example
      *   {{{
      * val outputs = List(TxOut(...), TxOut(...))
      * val totalAda = Utils.getAdaFromOutputs(outputs)
      * // Returns the sum of Lovelace from all outputs
      * // Returns BigInt(0) if the list is empty
      *   }}}
      */
    @deprecated(
      "lovelace-only; sum whole Values: TxInfo.valuePaidTo(addr) for a full address, or " +
          "outputs.filter(_.address.credential === cred).foldLeft(Value.zero)(_ + _.value) for a key payee",
      "1.1.1"
    )
    def getAdaFromOutputs(outputs: List[v2.TxOut]): Lovelace = {
        outputs.map(_.value.getLovelace).foldLeft(BigInt(0))(_ + _)
    }

    /** Calculates the total amount of ADA (Lovelace) from a list of transaction inputs.
      *
      * LOVELACE ONLY: native tokens in the inputs are ignored. Use `TxInfo.valueSpentFrom(addr)`
      * and compare the whole `Value`.
      *
      * @param inputs
      *   the list of transaction inputs to sum
      * @return
      *   the total Lovelace amount from all resolved inputs
      * @example
      *   {{{
      * val inputs = List(TxInInfo(...), TxInInfo(...))
      * val totalAda = Utils.getAdaFromInputs(inputs)
      * // Returns the sum of Lovelace from all resolved input outputs
      * // Returns BigInt(0) if the list is empty
      *   }}}
      */
    @deprecated(
      "lovelace-only; sum whole Values: TxInfo.valueSpentFrom(addr) for a full address, or " +
          "inputs.filter(_.resolved.address.credential === cred).foldLeft(Value.zero)(_ + _.resolved.value) for a key payee",
      "1.1.1"
    )
    def getAdaFromInputs(inputs: List[TxInInfo]): Lovelace = {
        inputs.map(_.resolved.value.getLovelace).foldLeft(BigInt(0))(_ + _)
    }
}
