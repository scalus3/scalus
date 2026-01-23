package scalus.testing.regression.hydrozoa20250804

//import com.bloxbean.cardano.client.address
//import com.bloxbean.cardano.client.address.AddressProvider
//import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
//import hydrozoa.infra.{encodeHex, toBB}
import scalus.uplc.builtin.Builtins
import scalus.compiler.compile
import scalus.testing.regression.hydrozoa20250804.DisputeResolutionValidator.TallyRedeemer.{Continuing, Removed}
import scalus.testing.regression.hydrozoa20250804.TreasuryValidator.TreasuryDatum.Unresolved
import scalus.testing.regression.hydrozoa20250804.TreasuryValidator.{cip67BeaconTokenPrefix, TreasuryDatum}
//import TreasuryValidatorScript.plutusScript
import scalus.testing.regression.hydrozoa20250804.ByteStringExtensions.take
import scalus.testing.regression.hydrozoa20250804.TxOutExtensions.inlineDatumOfType
import scalus.testing.regression.hydrozoa20250804.ValueExtensions.{containsCurrencySymbol, containsExactlyOneAsset, onlyNonAdaAsset}
//import hydrozoa.l2.block.BlockTypeL2
//import hydrozoa.{PosixTime, *}
import scalus.*
import scalus.uplc.builtin.Builtins.{serialiseData, verifyEd25519Signature}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.IntervalBoundType.Finite
import scalus.cardano.onchain.plutus.v1.Value.+
import scalus.cardano.onchain.plutus.v1.{PosixTime, Value}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.{!==, ===, fail, log, require, Eq, List, Option, SortedMap, Validator}

@Compile
object DisputeResolutionValidator extends Validator:

    // EdDSA / ed25519 signature
    private type Signature = ByteString

    // The result of `bls12_381_G2_compress` function
    private type UtxoSetCommitment = ByteString

    // Datum
    case class VoteDatum(
        key: BigInt,
        link: BigInt,
        peer: Option[PubKeyHash],
        voteStatus: VoteStatus
    )

    given FromData[VoteDatum] = FromData.derived
    given ToData[VoteDatum] = ToData.derived

    enum VoteStatus:
        case NoVote
        case Vote(voteDetails: VoteDetails)

    given Eq[VoteStatus] = (a: VoteStatus, b: VoteStatus) =>
        a match
            case VoteStatus.NoVote =>
                b match
                    case VoteStatus.NoVote  => true
                    case VoteStatus.Vote(_) => false
            case VoteStatus.Vote(as) =>
                a match {
                    case VoteStatus.NoVote   => false
                    case VoteStatus.Vote(bs) => as === bs
                }

    given FromData[VoteStatus] = FromData.derived
    given ToData[VoteStatus] = ToData.derived

    case class VoteDetails(
        utxosActive: UtxoSetCommitment,
        versionMinor: BigInt
    )

    given Eq[VoteDetails] = (a: VoteDetails, b: VoteDetails) =>
        a.utxosActive == b.utxosActive && a.versionMinor == b.versionMinor

    given FromData[VoteDetails] = FromData.derived
    given ToData[VoteDetails] = ToData.derived

    // Redeemer
    enum DisputeRedeemer:
        case Vote(voteRedeemer: MinorBlockL1Effect)
        case Tally(tallyRedeemer: TallyRedeemer)
        case Resolve

    given FromData[DisputeRedeemer] = FromData.derived
    given ToData[DisputeRedeemer] = ToData.derived

    case class MinorBlockL1Effect(
        blockHeader: BlockHeader,
        multisig: List[Signature]
    )

    given FromData[MinorBlockL1Effect] = FromData.derived
    given ToData[MinorBlockL1Effect] = ToData.derived

    enum TallyRedeemer:
        case Continuing
        case Removed

    given FromData[TallyRedeemer] = FromData.derived
    given ToData[TallyRedeemer] = ToData.derived

    // TODO: should we re-use Hydrozoa's type more broadly?
    // issues: some types from scalus won't work well with Hydrozoa's transport
    case class BlockHeader(
        blockNum: BigInt,
        // TODO: should we re-use Hydrozoa's type more broadly?
        blockType: BlockTypeL2,
        timeCreation: PosixTime,
        versionMajor: BigInt,
        versionMinor: BigInt,
        utxosActive: UtxoSetCommitment
    )

    given FromData[BlockHeader] = FromData.derived
    given ToData[BlockHeader] = ToData.derived

    given FromData[BlockTypeL2] = FromData.derived
    given ToData[BlockTypeL2] = ToData.derived

    inline def cip67DisputeTokenPrefix = hex"00d950b0"

    // Common errors
    private inline val DatumIsMissing = "Vote datum should be present"

    // Vote redeemer
    private inline val VoteOnlyOneVoteUtxoIsSpent = "Only one vote utxo can be spent"
    private inline val VoteAlreadyCast = "Vote is already has been cast"
    private inline val VoteMustBeSignedByPeer = "Transaction must be signed by peer"
    private inline val VoteOneRefInputTreasury = "Only one ref input (treasury) is required"
    private inline val VoteTreasuryBeacon = "Treasury should contain beacon token"
    private inline val VoteTreasuryDatum = "Treasury datum is missing"
    private inline val VoteTreasuryDatumHeadMp = "Treasury datum headMp mismatch"
    private inline val VoteTreasuryDatumDisputeId = "Treasury datum disputeId mismatch"
    private inline val VoteTimeValidityCheck =
        "The transaction validity upper bound must not exceed the deadlineVoting"
    private inline val VoteMultisigCheck =
        "Redeemer should contain all valid signatures for the block voted"
    private inline val VoteMajorVersionCheck =
        "The versionMajor field must match between treasury and voteRedeemer"
    private inline val VoteVoteOutputExists =
        "There should exist one continuing vote output with the same set of tokens"
    private inline val VoteOutputDatumCheck =
        "voteStatus field of voteOutput must be a correct Vote"
    private inline val VoteOutputDatumAdditionalChecks =
        "other fields of voteOutput must match voteInput"

    // Tally redeemer
    private inline val VotingInputsNotFound =
        "Continuing and removed inputs not found"
    private inline val TwoVotingInputsExpected =
        "Exactly two voting inputs are expected"
    private inline val VotingInputsDoNotMatch =
        "Voting inputs must match on voting tokens"
    private inline val KeyLinkFieldsDoNotMatch =
        "Key and link fields in voting inputs must match"
    private inline val NoOtherInputs =
        "No other inputs are allowed"
    private inline val TreasuryReferenceInputExists =
        "Treasury reference should present"
    private inline val TreasuryDatumIsUnresolved =
        "Treasury datum should be unresolved"
    private inline val TreasuryDatumMatchesHeadMp =
        "Treasury datum should match voting inputs on head policy"
    private inline val TreasuryDatumMatchesDisputeId =
        "Treasury datum should match voting inputs on dispute id"
    private inline val TimeValidityCheck =
        "The transaction validity upper bound must not exceed the deadlineVoting"
    private inline val HighestVoteCheck =
        "continuingOutput must match the highest voteStatus"
    private inline val AbsentOrWrongContinuingOutput =
        "The continuing output was not found or contains wrong value"
    private inline val LinkCheck =
        "The link field of removedInput and continuingOutput must match"
    private inline val KeyCheck =
        "Key field of continuingInput and continuingOutput must match"

    // Resolve redeemer
    private inline val ResolveTreasurySpent =
        "Treasury that holds head beacon must be spent"
    private inline val ResolveDatumIsUnresolved =
        "Treasury input datum should be unresolved"
    private inline val ResolveTreasuryVoteMatch =
        "Treasury datum should match vote datum on (headMp, disputeId)"

    // Utility to decide on higher vote
    def maxVote(a: VoteStatus, b: VoteStatus): VoteStatus =
        import VoteStatus.{NoVote, Vote}
        a match {
            case NoVote => b
            case Vote(ad) =>
                b match {
                    case NoVote   => a
                    case Vote(bd) => if ad.versionMinor > bd.versionMinor then a else b
                }
        }

    // Entry point
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit =

        log("DisputeResolution")

        // Parse datum
        val voteDatum: VoteDatum = datum match
            case Some(d) => d.to[VoteDatum]
            case None    => fail(DatumIsMissing)

        redeemer.to[DisputeRedeemer] match
            case DisputeRedeemer.Vote(voteRedeemer) =>
                log("Vote")

                // There must not be any other spent input matching voteOutref on transaction hash
                val voteOutref = tx.inputs.filter(_.outRef.id === ownRef.id) match
                    case List.Cons(voteOutref, tail) =>
                        require(tail.isEmpty, VoteOnlyOneVoteUtxoIsSpent)
                        voteOutref
                    // Unreachable
                    case _ => fail()

                // Check vote status
                require(voteDatum.voteStatus === VoteStatus.NoVote, VoteAlreadyCast)

                // Check signature
                require(voteDatum.peer.forall(tx.signatories.contains(_)), VoteMustBeSignedByPeer)

                // Let(headMp, disputeId) be the minting policy and asset name of the only non-ADA
                // tokens in voteInput.
                val voteInput = voteOutref.resolved
                val (headMp, disputeId, voteTokenAmount) = voteInput.value.onlyNonAdaAsset

                // Verify the treasury reference input
                // Let treasury be the only reference input matching voteOutref on tx hash.
                val treasuryReference = tx.referenceInputs match {
                    case List.Cons(input, otherInputs) =>
                        require(
                          otherInputs.isEmpty && input.outRef.id === voteOutref.outRef.id,
                          VoteOneRefInputTreasury
                        )
                        input
                    case List.Nil => fail(VoteOneRefInputTreasury)
                }

                // A head beacon token of headMp and CIP-67 prefix 4937 must be in treasury.
                treasuryReference.resolved.value.toSortedMap
                    .get(headMp)
                    .getOrElse(SortedMap.empty)
                    .toList match
                    case List.Cons((tokenName, amount), none) =>
                        require(
                          none.isEmpty && tokenName.take(4) == cip67BeaconTokenPrefix,
                          VoteTreasuryBeacon
                        )
                    case _ => fail(VoteTreasuryBeacon)

                //  headMp and disputeId must match the corresponding fields of the Unresolved datum in treasury.
                val treasuryDatum =
                    treasuryReference.resolved.inlineDatumOfType[TreasuryDatum] match {
                        case Unresolved(unresolvedDatum) => unresolvedDatum
                        case _                           => fail(VoteTreasuryDatum)
                    }
                require(treasuryDatum.headMp === headMp, VoteTreasuryDatumHeadMp)
                require(treasuryDatum.disputeId === disputeId, VoteTreasuryDatumDisputeId)

                // The transaction’s time -validity upper bound must not exceed the deadlineVoting
                // field of treasury.
                tx.validRange.to.boundType match {
                    case Finite(toTime) =>
                        require(toTime <= treasuryDatum.deadlineVoting, VoteTimeValidityCheck)
                    case _ => fail(VoteTimeValidityCheck)
                }

                // The multisig field of voteRedeemer must have signatures of the blockHeader
                // field of voteRedeemer for all the public keys in the peers field of treasury.
                val msg = voteRedeemer.blockHeader.toData |> serialiseData
                require(
                  treasuryDatum.peers.length == voteRedeemer.multisig.length,
                  VoteMultisigCheck
                )
                List.map2(treasuryDatum.peers, voteRedeemer.multisig)((vk, sig) =>
                    require(verifyEd25519Signature(vk, msg, sig), VoteMultisigCheck)
                )
                // The versionMajor field must match between treasury and voteRedeemer.
                require(
                  voteRedeemer.blockHeader.versionMajor == treasuryDatum.versionMajor,
                  VoteMajorVersionCheck
                )

                // Vote output
                val voteOutput = tx.outputs.find(o =>
                    o.value.containsExactlyOneAsset(headMp, disputeId, voteTokenAmount)
                ) match
                    case Some(e) => e
                    case None    => fail(VoteVoteOutputExists)

                require(voteOutput.address === voteInput.address, VoteVoteOutputExists)

                val voteOutputDatum = voteOutput.inlineDatumOfType[VoteDatum]

                // voteStatus field of voteOutput must be a Vote matching voteRedeemer
                // on the utxosActive and versionMinor fields.
                voteOutputDatum.voteStatus match {
                    case VoteStatus.Vote(voteDetails) =>
                        require(
                          voteDetails.versionMinor == voteRedeemer.blockHeader.versionMinor,
                          VoteOutputDatumCheck
                        )
                        require(
                          voteDetails.utxosActive == voteRedeemer.blockHeader.utxosActive,
                          VoteOutputDatumCheck
                        )
                    case _ => fail(VoteOutputDatumCheck)
                }

                // All other fields of voteInput and voteOutput must match.
                require(voteDatum.key === voteOutputDatum.key, VoteOutputDatumAdditionalChecks)
                require(voteDatum.peer === voteOutputDatum.peer, VoteOutputDatumAdditionalChecks)
                require(voteDatum.link === voteOutputDatum.link, VoteOutputDatumAdditionalChecks)

            /** Tallying is done as follows:
              *
              * +-----------+        +-----------+        +-----------+        +-----------+
              * |  key = 0  |        |  key = 1  |        |  key = 2  |        |  key = 3  |
              * | link = 1  |        | link = 2  |-+      | link = 3  |        | link = 0  |-+
              * |CONTINUING |        |  REMOVED  | |      |CONTINUING |        |  REMOVED  | |
              * |                                  |                 |                       |
              * v                                  |       v                                 |
              * +-----------+                      |       +-----------+                     |
              * |  key = 0  |                      |       |  key = 2  |                     |
              * | link = 2  | <------------------- +       | link = 0  |<-------+------------+
              * |CONTINUING |                              |  REMOVED  |        |
              * |                                                               |
              * v                                                               |
              * +-----------+                                                   |
              * |  key = 0  |                                                   |
              * | link = 0  |<--------------------------------------------------+
              * |   FINAL  |
              *
              * NB:
              * 1. `peer` is always set to None
              * 2. the higher `vote` is selected
              * 3. we start pair-wise from the beginning, but an arbitrary adjacent votes can be tallied
              */

            case DisputeRedeemer.Tally(tallyRedeemer) =>
                log("Tally")

                // TODO: hide `ownInput` and `otherInput` so they can't be used accidentally
                val ownInput = tx.inputs.find(_.outRef === ownRef).get
                val otherInput: TxInInfo =
                    tx.inputs.filter(i =>
                        i.resolved.address === ownInput.resolved.address && (i.outRef !== ownRef)
                    ) match
                        case List.Cons(other, empty) =>
                            require(empty.isEmpty, TwoVotingInputsExpected)
                            other
                        case _ => fail(VotingInputsNotFound)

                val ((continuingInputId, continuingInput), (removedInputId, removedInput)) =
                    tallyRedeemer match
                        case Continuing =>
                            // TODO: make a helper
                            (
                              (ownInput.outRef, ownInput.resolved),
                              (otherInput.outRef, otherInput.resolved)
                            )
                        case Removed =>
                            // TODO: make a helper
                            (
                              (otherInput.outRef, otherInput.resolved),
                              (ownInput.outRef, ownInput.resolved)
                            )

                // continuingInput and removedInput must have non-ADA tokens of only one asset
                // class, which must match between them
                val (contCs, contTn, _) = continuingInput.value.onlyNonAdaAsset
                val (removedCs, removedTn, _) = continuingInput.value.onlyNonAdaAsset
                require(contCs === removedCs && contTn === removedTn, VotingInputsDoNotMatch)

                // The key field of removedInput must be greater than the key field and equal to the
                // link field of continuingInput.
                val continuingDatum = continuingInput.inlineDatumOfType[VoteDatum]
                val removedDatum = removedInput.inlineDatumOfType[VoteDatum]
                require(
                  removedDatum.key > continuingDatum.key && removedDatum.key == continuingDatum.link,
                  KeyLinkFieldsDoNotMatch
                )

                // FIXME: we don't have address anymore since address is how we find the other input
                // There must be no other spent inputs from the same address as continuingInput
                // or holding any tokens of headMp.
                require(
                  tx.inputs
                      .filter(i =>
                          // Input other than we handle
                          (i.outRef !== continuingInputId) && (i.outRef !== removedInputId)
                          // holding any tokens of headMp
                              && (i.resolved.value.containsCurrencySymbol(contCs))
                      )
                      .isEmpty,
                  NoOtherInputs
                )

                // Verify the treasury reference input

                // If the voteStatus of either continuingInput or removedInput is NoVote,
                // all the following must be satisfied
                if continuingDatum.voteStatus === VoteStatus.NoVote
                    || removedDatum.voteStatus === VoteStatus.NoVote
                then {
                    // Let treasury be a reference input holding the head beacon token of headMp
                    // and CIP-67 prefix 4937
                    val treasuryReference = tx.referenceInputs
                        .find { i =>
                            i.resolved.value.toSortedMap
                                .get(contCs)
                                .getOrElse(SortedMap.empty)
                                .toList match
                                case List.Cons((tokenName, amount), none) =>
                                    tokenName.take(4) == cip67BeaconTokenPrefix
                                    && amount == BigInt(1)
                                    && none.isEmpty
                                case _ => fail(TreasuryReferenceInputExists)
                        }
                        .getOrFail(TreasuryReferenceInputExists)

                    // headMp and disputeId must match the corresponding fields of the Unresolved
                    // datum in treasury
                    val treasuryDatum =
                        treasuryReference.resolved.inlineDatumOfType[TreasuryDatum] match {
                            case Unresolved(unresolvedDatum) => unresolvedDatum
                            case _                           => fail(TreasuryDatumIsUnresolved)
                        }

                    require(treasuryDatum.headMp === contCs, TreasuryDatumMatchesHeadMp)
                    require(treasuryDatum.disputeId === contTn, TreasuryDatumMatchesDisputeId)

                    // The transaction’s time -validity upper bound must not exceed the deadlineVoting
                    // field of treasury.
                    tx.validRange.to.boundType match {
                        case Finite(toTime) =>
                            require(toTime <= treasuryDatum.deadlineVoting, TimeValidityCheck)
                        case _ => fail(TimeValidityCheck)
                    }
                }

                // Verify the vote output

                // 8. Let continuingOutput be an output with the same address and the sum of all
                // tokens (including ADA) in continuingInput and removedInput.
                val continuingOutput = tx.outputs
                    .find(o =>
                        o.address === continuingInput.address
                            && o.value === continuingInput.value + removedInput.value
                    )
                    .getOrFail(AbsentOrWrongContinuingOutput)

                // 9. The voteStatus field of continuingOutput must match the highest voteStatus
                // of continuingInput and removedInput
                val continuingOutputDatum = continuingOutput.inlineDatumOfType[VoteDatum]
                require(
                  continuingOutputDatum.voteStatus === maxVote(
                    continuingDatum.voteStatus,
                    removedDatum.voteStatus
                  ),
                  HighestVoteCheck
                )

                // 10. The link field of removedInput and continuingOutput must match.
                require(continuingOutputDatum.link == removedDatum.link, LinkCheck)

                // 11. All other fields of continuingInput and continuingOutput must match.
                require(continuingOutputDatum.key === continuingDatum.key, KeyCheck)
                require(continuingOutputDatum.peer === None)

            case DisputeRedeemer.Resolve =>
                log("Resolve")

                val voteInput = tx.inputs.find(_.outRef === ownRef).get
                val (headMp, disputeId, _) = voteInput.resolved.value.onlyNonAdaAsset

                // Let treasury be a spent input that holds a head beacon token of headMp and CIP-67
                // prefix 4937.
                val treasuryInput = tx.inputs
                    .find { i =>
                        i.resolved.value.toSortedMap
                            .get(headMp)
                            .getOrElse(SortedMap.empty)
                            .toList match
                            case List.Cons((tokenName, amount), none) =>
                                tokenName.take(4) == cip67BeaconTokenPrefix
                                && amount == BigInt(1)
                                && none.isEmpty
                            case _ => false
                    }
                    .getOrFail(ResolveTreasurySpent)

                // TODO: This is checked by the treasury validator
                val treasuryDatum =
                    treasuryInput.resolved.inlineDatumOfType[TreasuryDatum] match {
                        case Unresolved(unresolvedDatum) => unresolvedDatum
                        case _                           => fail(ResolveDatumIsUnresolved)
                    }

                // headMp and disputeId must match the corresponding fields of the Unresolved datum
                // in treasury.
                require(treasuryDatum.headMp === headMp, ResolveTreasuryVoteMatch)
                require(treasuryDatum.disputeId === disputeId, ResolveTreasuryVoteMatch)

end DisputeResolutionValidator

object DisputeResolutionScript {

    lazy val sir = compile(DisputeResolutionValidator.validate)
    lazy val script = sir.toUplcOptimized(generateErrorTraces = true).plutusV3

    lazy val cbor = script.cborByteString

    // any hash, we need just to check determenism
    lazy val hash = Builtins.sha3_256(cbor)

}

@main
def disputeResolutionValidatorRun(): Unit = {
    //    println(DisputeResolutionScript.sir.showHighlighted)
    println(DisputeResolutionScript.hash)
}
