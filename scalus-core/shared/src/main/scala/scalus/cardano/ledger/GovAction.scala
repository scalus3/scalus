package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

import scala.collection.immutable.{Map, Set as ScalaSet}
import io.bullet.borer.NullOptions.given

/** Represents a governance action in the Cardano blockchain.
  *
  * Governance actions are proposals submitted to the blockchain for voting. Different types of
  * actions exist for different governance activities.
  */
enum GovAction {

    /** An action to change protocol parameters.
      *
      * @param prevActionId
      *   Optional ID of a previous action this one depends on
      * @param protocolParamUpdate
      *   The proposed parameter updates
      * @param policyHash
      *   Optional policy hash for a constitution script
      */
    case ParameterChange(
        prevActionId: Option[GovActionId],
        protocolParamUpdate: ProtocolParamUpdate,
        policyHash: Option[PolicyHash]
    )

    /** An action to initiate a hard fork.
      *
      * @param prevActionId
      *   Optional ID of a previous action this one depends on
      * @param protocolVersion
      *   The new protocol version to fork to
      */
    case HardForkInitiation(
        prevActionId: Option[GovActionId],
        protocolVersion: ProtocolVersion
    )

    /** An action to withdraw funds from the treasury.
      *
      * @param prevActionId
      *   Optional ID of a previous action this one depends on
      * @param withdrawals
      *   Map of reward accounts to withdrawal amounts
      * @param policyHash
      *   Optional policy hash for a treasury withdrawal policy
      */
    case TreasuryWithdrawals(
        withdrawals: Map[RewardAccount, Coin],
        policyHash: Option[PolicyHash]
    )

    /** An action to express no confidence in the current constitutional committee.
      *
      * @param prevActionId
      *   Optional ID of a previous action this one depends on
      */
    case NoConfidence(
        prevActionId: Option[GovActionId]
    )

    /** An action to update the constitutional committee.
      *
      * @param prevActionId
      *   Optional ID of a previous action this one depends on
      * @param removedMembers
      *   Set of committee members to remove
      * @param addedMembers
      *   Map of new committee members to their terms (epoch numbers)
      * @param threshold
      *   Voting threshold for the committee
      */
    case UpdateCommittee(
        prevActionId: Option[GovActionId],
        removedMembers: ScalaSet[Credential],
        addedMembers: Map[Credential, Long],
        threshold: UnitInterval
    )

    /** An action to establish a new constitution.
      *
      * @param prevActionId
      *   Optional ID of a previous action this one depends on
      * @param constitution
      *   The new constitution
      */
    case NewConstitution(
        prevActionId: Option[GovActionId],
        constitution: Constitution
    )

    /** An informational action. This can be used for non-binding votes or information sharing.
      */
    case InfoAction

    def scriptHashOption: Option[ScriptHash] = this match
        case parameterChange: GovAction.ParameterChange => parameterChange.policyHash
        case treasuryWithdrawals: GovAction.TreasuryWithdrawals =>
            treasuryWithdrawals.policyHash
        case _ => None
}

object GovAction {

    /** Ordering matches Haskell's derived Ord constructor order. Note: This only compares by
      * constructor tag (ordinal), not by fields. Full field comparison would require Ordering for
      * many nested types.
      */
    given Ordering[GovAction] = Ordering.by(_.ordinal)

    /** CBOR Encoder for GovAction. Encodes as a tagged structure based on the action type.
      */
    given Encoder[GovAction] = new Encoder[GovAction] {
        def write(w: Writer, value: GovAction): Writer = value match {
            case GovAction.ParameterChange(prevActionId, protocolParamUpdate, policyHash) =>
                w.writeArrayOpen(4)
                    .writeInt(0) // Tag for ParameterChange
                    .write(prevActionId)
                    .write(protocolParamUpdate)
                    .write(policyHash)
                    .writeArrayClose()

            case GovAction.HardForkInitiation(prevActionId, protocolVersion) =>
                w.writeArrayOpen(3)
                    .writeInt(1) // Tag for HardForkInitiation
                    .write(prevActionId)
                    .write(protocolVersion)
                    .writeArrayClose()

            case GovAction.TreasuryWithdrawals(withdrawals, policyHash) =>
                w.writeArrayOpen(3)
                    .writeInt(2) // Tag for TreasuryWithdrawals
                    .write(withdrawals)
                    .write(policyHash)
                    .writeArrayClose()

            case GovAction.NoConfidence(prevActionId) =>
                w.writeArrayOpen(2)
                    .writeInt(3) // Tag for NoConfidence
                    .write(prevActionId)
                    .writeArrayClose()

            case GovAction.UpdateCommittee(prevActionId, removedMembers, addedMembers, threshold) =>
                w.writeArrayOpen(5)
                    .writeInt(4) // Tag for UpdateCommittee
                    .write(prevActionId)
                    .write(removedMembers)
                    .write(addedMembers)
                    .write(threshold)
                    .writeArrayClose()

            case GovAction.NewConstitution(prevActionId, constitution) =>
                w.writeArrayOpen(3)
                    .writeInt(5) // Tag for NewConstitution
                    .write(prevActionId)
                    .write(constitution)
                    .writeArrayClose()

            case GovAction.InfoAction =>
                w.writeArrayOpen(1)
                    .writeInt(6) // Tag for InfoAction
                    .writeArrayClose()
        }
    }

    /** CBOR Decoder for GovAction. Decodes from a tagged structure based on the action type.
      */
    given Decoder[GovAction] = new Decoder[GovAction] {
        def read(r: Reader): GovAction = {
            // For other actions, read the array and the tag
            r.readArrayHeader()
            val tag = r.readInt()

            val action = tag match {
                case 0 => // ParameterChange
                    val prevActionId = r.read[Option[GovActionId]]()
                    val protocolParamUpdate = r.read[ProtocolParamUpdate]()
                    val policyHash = r.read[Option[PolicyHash]]()
                    GovAction.ParameterChange(prevActionId, protocolParamUpdate, policyHash)

                case 1 => // HardForkInitiation
                    val prevActionId = r.read[Option[GovActionId]]()
                    val protocolVersion = r.read[ProtocolVersion]()
                    GovAction.HardForkInitiation(prevActionId, protocolVersion)

                case 2 => // TreasuryWithdrawals
                    // Read withdrawals map (as ByteString -> Coin)
                    val withdrawals = r.read[Map[RewardAccount, Coin]]()
                    val policyHash = r.read[Option[PolicyHash]]()
                    GovAction.TreasuryWithdrawals(withdrawals, policyHash)

                case 3 => // NoConfidence
                    val prevActionId = r.read[Option[GovActionId]]()
                    GovAction.NoConfidence(prevActionId)

                case 4 => // UpdateCommittee
                    val prevActionId = r.read[Option[GovActionId]]()
                    val removedMembers = r.read[ScalaSet[Credential]]()
                    val addedMembers = r.read[Map[Credential, Long]]()
                    val threshold = r.read[UnitInterval]()
                    GovAction.UpdateCommittee(prevActionId, removedMembers, addedMembers, threshold)

                case 5 => // NewConstitution
                    val prevActionId = r.read[Option[GovActionId]]()
                    val constitution = r.read[Constitution]()
                    GovAction.NewConstitution(prevActionId, constitution)
                case 6 => GovAction.InfoAction

                case _ =>
                    throw new IllegalArgumentException(s"Invalid GovAction tag: $tag")
            }

            action
        }
    }
}
