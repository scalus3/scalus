package scalus.testing.regression.hydrozoa20250804

//import com.bloxbean.cardano.client.address.AddressProvider
//import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
//import hydrozoa.infra.{encodeHex, toBB}
//import hydrozoa.l1.multisig.state.L2ConsensusParamsH32
import Scalar as ScalusScalar

import scalus.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.*
import scalus.compiler.compile
import scalus.cardano.onchain.plutus.v1.Value.+
import scalus.cardano.onchain.plutus.v1.{PosixTime, Value}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.Validator
import scalus.cardano.onchain.plutus.prelude.List.Nil
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.G1
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.G1.scale
import scalus.testing.regression.hydrozoa20250804.ByteStringExtensions.take
import scalus.testing.regression.hydrozoa20250804.DisputeResolutionValidator.VoteDatum
import scalus.testing.regression.hydrozoa20250804.DisputeResolutionValidator.VoteStatus.{NoVote, Vote}
import scalus.testing.regression.hydrozoa20250804.TreasuryValidator.TreasuryDatum.{Resolved, Unresolved}
import scalus.testing.regression.hydrozoa20250804.TreasuryValidator.TreasuryRedeemer.{Deinit, Resolve, Withdraw}
import scalus.testing.regression.hydrozoa20250804.TxOutExtensions.inlineDatumOfType
import scalus.testing.regression.hydrozoa20250804.ValueExtensions.{containsExactlyOneAsset, unary_-}

type L2ConsensusParamsH32 = ByteString

@Compile
object TreasuryValidator extends Validator:

    val setup: List[BLS12_381_G1_Element] =
        List.Cons(
          hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
          List.Cons(
            hex"8ce3b57b791798433fd323753489cac9bca43b98deaafaed91f4cb010730ae1e38b186ccd37a09b8aed62ce23b699c48",
            List.Cons(
              hex"8ed36ed5fb9a1b099d84cba0686d8af9a2929a348797cd51c335cdcea1099e3d6f95126dfbc93abcfb3b56a7fc14477b",
              Nil
            )
          )
        ).map(G1.uncompress)

    // EdDSA / ed25519 Cardano verification key
    private type VerificationKey = ByteString

    // The result of `bls12_381_G2_compress` function
    private type MembershipProof = ByteString

    // Datum
    enum TreasuryDatum:
        case Unresolved(unresolvedDatum: UnresolvedDatum)
        case Resolved(resolvedDatum: ResolvedDatum)

    given FromData[TreasuryDatum] = FromData.derived
    given ToData[TreasuryDatum] = ToData.derived

    case class UnresolvedDatum(
        headMp: PolicyId,
        disputeId: TokenName,
        peers: List[VerificationKey],
        peersN: BigInt,
        deadlineVoting: PosixTime,
        versionMajor: BigInt,
        params: L2ConsensusParamsH32
    )

    given FromData[UnresolvedDatum] = FromData.derived
    given ToData[UnresolvedDatum] = ToData.derived

    case class ResolvedDatum(
        headMp: PolicyId,
        utxosActive: MembershipProof,
        version: (BigInt, BigInt),
        params: L2ConsensusParamsH32
    )

    given FromData[ResolvedDatum] = FromData.derived
    given ToData[ResolvedDatum] = ToData.derived

    // Script redeemer
    enum TreasuryRedeemer:
        case Resolve
        case Withdraw(withdrawRedeemer: WithdrawRedeemer)
        case Deinit

    given FromData[TreasuryRedeemer] = FromData.derived
    given ToData[TreasuryRedeemer] = ToData.derived

    case class WithdrawRedeemer(
        utxoIds: List[TxOutRef],
        // membership proof for utxoIds and the updated accumulator at the same time
        proof: MembershipProof
    )

    given FromData[WithdrawRedeemer] = FromData.derived
    given ToData[WithdrawRedeemer] = ToData.derived

    // Common errors
    private inline val DatumIsMissing =
        "Treasury datum should be present"

    // Resolve redeemer
    private inline val ResolveNeedsUnresolvedDatumInInput =
        "Resolve redeemer requires unresolved datum in treasury input"
    private inline val ResolveNeedsResolvedDatumInOutput =
        "Resolve redeemer requires resolved datum in treasury output"
    private inline val ResolveValueShouldBePreserved =
        "Value invariant should hold: treasuryOutput = treasuryInput + voteInput"
    private inline val ResolveVoteInputNotFound =
        "Vote input was not found"
    private inline val ResolveVersionCheck =
        "The version field of treasuryOutput must match (versionMajor, 0)"
    private inline val ResolveUtxoActiveCheck =
        "The activeUtxo in resolved treasury must match voting results"
    private inline val ResolveTreasuryInputOutputHeadMp =
        "headMp in treasuryInput and treasuryOutput must match"
    private inline val ResolveTreasuryInputOutputParams =
        "params in treasuryInput and treasuryOutput must match"
    private inline val ResolveTreasuryOutputFailure =
        "Exactly one treasury output should present"
    private inline val ResolveUnexpectedNoVote =
        "Unexpected NoVot when trying to resolve"

    // Withdraw redeemer
    private inline val WithdrawNeedsResolvedDatum =
        "Withdraw redeemer requires resolved datum"
    private inline val WithdrawWrongNumberOfWithdrawals =
        "Number of outputs should match the number of utxo ids"
    private inline val WithdrawBeaconTokenFailure =
        "Treasury should contain exactly one beacon token"
    private inline val WithdrawMembershipValidationFailed =
        "Withdrawals membership check failed"
    private inline val WithdrawBeaconTokenShouldBePreserved =
        "Beacon token should be preserves in treasury output"
    private inline val WithdrawValueShouldBePreserved =
        "Value invariant should hold: treasuryInput = treasuryOutput + Σ withdrawalOutput"
    private inline val WithdrawOutputAccumulatorUpdated =
        "Accumulator in the output should be properly updated"

    // Deinit redeemer
    private inline val DeinitTokensNotFound =
        "Head tokens was not found in treasury input"
    private inline val DeinitTokensNotBurned =
        "All head tokens should be burned"

    def cip67BeaconTokenPrefix = hex"01349900"

    // Entry point
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit =

        log("TreasuryValidator")

        // Parse datum
        val treasuryDatum: TreasuryDatum = datum match
            case Some(d) => d.to[TreasuryDatum]
            case None    => fail(DatumIsMissing)

        redeemer.to[TreasuryRedeemer] match
            case Resolve =>
                log("Resolve")

                // Treasury datum should be an "unresolved" one
                val unresolvedDatum = treasuryDatum match
                    case Unresolved(d) => d
                    case _             => fail(ResolveNeedsUnresolvedDatumInInput)

                // TODO: pass vote input's outRef in the redeemer?
                // Vote input
                val voteInput = tx.inputs
                    .find(e =>
                        e.resolved.value.containsExactlyOneAsset(
                          unresolvedDatum.headMp,
                          unresolvedDatum.disputeId,
                          unresolvedDatum.peersN + 1
                        )
                    )
                    .getOrFail(ResolveVoteInputNotFound)
                    .resolved

                // Treasury (own) input
                // TODO: factor out
                val treasuryInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail("Impossible happened: own input was not found")
                    .resolved

                // Total expected output
                val treasuryOutputExpected = voteInput.value + treasuryInput.value

                // TODO: pass output index in redeemer?
                // The only treasury output
                val treasuryOutput = tx.outputs
                    .filter(e => e.address === treasuryInput.address) match
                    case List.Cons(o, tail) =>
                        require(tail.isEmpty, ResolveTreasuryOutputFailure)
                        o
                    case _ => fail(ResolveTreasuryOutputFailure)

                // Check treasury output value
                require(
                  treasuryOutput.value === treasuryOutputExpected,
                  ResolveValueShouldBePreserved
                )

                val treasuryOutputDatum = treasuryOutput.inlineDatumOfType[TreasuryDatum] match
                    case Unresolved(_) => fail(ResolveNeedsResolvedDatumInOutput)
                    case Resolved(d)   => d

                val voteDatum = voteInput.inlineDatumOfType[VoteDatum]

                // 7. If voteStatus is Vote...
                voteDatum.voteStatus match
                    case NoVote            => fail(ResolveUnexpectedNoVote)
                    case Vote(voteDetails) =>
                        // (a) Let versionMinor be the corresponding field in voteStatus.
                        // (b) The version field of treasuryOutput must match (versionMajor, versionMinor).
                        require(
                          treasuryOutputDatum.version._1 == unresolvedDatum.versionMajor &&
                              treasuryOutputDatum.version._2 == voteDetails.versionMinor,
                          ResolveVersionCheck
                        )
                        // (c) voteStatus and treasuryOutput must match on utxosActive.
                        require(
                          treasuryOutputDatum.utxosActive === voteDetails.utxosActive,
                          ResolveUtxoActiveCheck
                        )

                // 8. treasuryInput and treasuryOutput must match on all other fields.
                require(
                  unresolvedDatum.headMp === treasuryOutputDatum.headMp,
                  ResolveTreasuryInputOutputHeadMp
                )

                require(
                  unresolvedDatum.params === treasuryOutputDatum.params,
                  ResolveTreasuryInputOutputParams
                )

            case Withdraw(WithdrawRedeemer(utxoIds, proof)) =>
                log("Withdraw")

                // Treasury datum should be "resolved" one
                val resolvedDatum = treasuryDatum match
                    case Resolved(d) => d
                    case _           => fail(WithdrawNeedsResolvedDatum)

                // headMp and headId

                // Let headMp be the corresponding field in treasuryInput.
                val headMp = resolvedDatum.headMp

                // TODO: factor out
                val treasuryInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail("Impossible happened: own input was not found")
                    .resolved

                // Let headId be the asset name of the only headMp token in treasuryInput with CIP-67
                // prefix 4937.
                val headId: TokenName =
                    treasuryInput.value.toSortedMap
                        .get(headMp)
                        .getOrFail(WithdrawBeaconTokenFailure)
                        .toList
                        .filter((tn, _) => tn.take(4) == cip67BeaconTokenPrefix) match
                        case List.Cons((tokenName, amount), none) =>
                            require(none.isEmpty && amount == BigInt(1), WithdrawBeaconTokenFailure)
                            tokenName
                        case _ => fail(WithdrawBeaconTokenFailure)

                // The beacon token should be preserved
                // By contract, we require the treasure utxo is always be the head, and the tail be withdrawals
                // FIXME: in reality the change outputs gets in
                //   Ideally we need to use treasury for fees.
                val List.Cons(treasuryOutput, withdrawalOutputs) = tx.outputs: @unchecked
                require(
                  treasuryOutput.value.toSortedMap
                      .get(headMp)
                      .getOrFail(WithdrawBeaconTokenShouldBePreserved)
                      .get(headId)
                      .getOrFail(WithdrawBeaconTokenShouldBePreserved) == BigInt(1),
                  WithdrawBeaconTokenShouldBePreserved
                )

                // Withdrawals
                // The number of withdrawals should match the number of utxos ids in the redeemer
                // FIXME: in reality the change outputs gets in - hence +1 for now
                require(
                  withdrawalOutputs.size == utxoIds.size + 1,
                  WithdrawWrongNumberOfWithdrawals
                )
                // Calculate the final poly for withdrawn subset
                // FIXME: this fails due to the same error:
                // Caused by: java.lang.IllegalArgumentException: Expected case class type, got TypeVar(T,Some(218919)) in expression: match d with
                // I blame this lines in Scalus, though it's not clear how to fix that since it uses

                // TODO:
                val withdrawalOutputsNoChange = withdrawalOutputs.reverse.tail.reverse

                // Zip utxo ids and outputs
                val withdrawnUtxos: List[ScalusScalar] = utxoIds
                    // .zip(withdrawalOutputs)
                    .zip(withdrawalOutputsNoChange)
                    // Convert to data, serialize, calculate a hash, convert to scalars
                    .map(e =>
                        e.toData
                            |> serialiseData
                            |> blake2b_224
                            |> ScalusScalar.fromByteStringBigEndianUnsafe
                    )

                // Decompress commitments and run the membership check
                val acc = bls12_381_G2_uncompress(resolvedDatum.utxosActive)
                val proof_ = bls12_381_G2_uncompress(proof)

                require(
                  checkMembership(setup, acc, withdrawnUtxos, proof_),
                  WithdrawMembershipValidationFailed
                )

                // Accumulator updated commitment
                val Resolved(outputResolvedDatum) =
                    treasuryOutput.inlineDatumOfType[TreasuryDatum]: @unchecked

                require(
                  outputResolvedDatum.utxosActive == proof,
                  WithdrawOutputAccumulatorUpdated
                )

                // treasuryInput must hold the sum of all tokens in treasuryOutput and the outputs of
                // withdrawals.
                // TODO: combine with iterating for poly calculation up above?
                val withdrawnValue =
                    withdrawalOutputsNoChange.foldLeft(Value.zero)((acc, o) => acc + o.value)

                val valueIsPreserved =
                    treasuryInput.value === (treasuryOutput.value + withdrawnValue)

                require(valueIsPreserved, WithdrawValueShouldBePreserved)

            case Deinit =>
                log("Deinit")

                // This redeemer does not require the treasury’s active utxo set to be empty,
                // but it implicitly requires the transaction to be multi-signed by all peers
                // to burn the headMp tokens.

                // Thus, the peers can use this redeemer to override the treasury’s
                // spending validator with their multi-signature.

                // Treasury datum might be "resolved" or "unresolved"
                val headMp = treasuryDatum match
                    case Resolved(d)   => d.headMp
                    case Unresolved(d) => d.headMp

                // TODO: factor out
                val treasuryInput = tx.inputs
                    .find(_.outRef === ownRef)
                    .getOrFail("Impossible happened: own input was not found")
                    .resolved

                val headTokensInput = treasuryInput.value.toSortedMap
                    .get(headMp)
                    .getOrFail(DeinitTokensNotFound)

                // TODO: this might be redundant
                require(
                  !headTokensInput.isEmpty,
                  DeinitTokensNotFound
                )

                // All head tokens should be burned
                val headTokensMint = (-tx.mint).toSortedMap
                    .getOrFail(headMp, DeinitTokensNotBurned)

                require(headTokensInput === headTokensMint, DeinitTokensNotBurned)

    // Utility functions
    /*
     * Multiply a list of n coefficients that belong to a binomial each to get a final polynomial of degree n+1
     * Example: for (x+2)(x+3)(x+5)(x+7)(x+11)=x^5 + 28 x^4 + 288 x^3 + 1358 x^2 + 2927 x + 2310
     * */
    def getFinalPolyScalus(binomial_poly: List[ScalusScalar]): List[ScalusScalar] = {
        binomial_poly
            .foldLeft(List.single(ScalusScalar.one)): (acc, term) =>
                val shiftedPoly: List[ScalusScalar] = List.Cons(ScalusScalar.zero, acc)
                val multipliedPoly = acc.map(s => s * term).appended(ScalusScalar.zero)
                List.map2(shiftedPoly, multipliedPoly)((l, r) => l + r)
    }

    def getG1Commitment(
        setup: List[BLS12_381_G1_Element],
        subset: List[ScalusScalar]
    ): BLS12_381_G1_Element = {
        val subsetInG1 =
            List.map2(getFinalPolyScalus(subset), setup): (sb, st) =>
                st.scale(sb.toInt)

        subsetInG1.foldLeft(G1.zero): (a, b) =>
            bls12_381_G1_add(a, b)
    }

    /** Checks the membership `proof` for a `subset` of elements against the given accumulator
      * `acc`.
      *
      * @param setup
      *   The setup of the accumulator.
      * @param acc
      *   The accumulator to check.
      * @param subset
      *   The subset of the setup.
      * @return
      *   True if the accumulator is valid, false otherwise.
      */
    def checkMembership(
        setup: List[BLS12_381_G1_Element],
        acc: BLS12_381_G2_Element,
        subset: List[ScalusScalar],
        proof: BLS12_381_G2_Element
    ): Boolean = {
        val g1 = setup !! 0
        val lhs = bls12_381_millerLoop(g1, acc)
        val rhs = bls12_381_millerLoop(getG1Commitment(setup, subset), proof)
        bls12_381_finalVerify(lhs, rhs)
    }

end TreasuryValidator

object TreasuryValidatorScript {
    val sir = compile(TreasuryValidator.validate)
    val script = sir.toUplcOptimized(generateErrorTraces = true).plutusV3
    //    val script = sir.toUplc().plutusV3

    /*
    // TODO: can we use Scalus for that?
    val plutusScript: PlutusV3Script = PlutusV3Script
        .builder()
        .`type`("PlutusScriptV3")
        .cborHex(script.doubleCborHex)
        .build()
        .asInstanceOf[PlutusV3Script]

    val scriptHash: ByteString = ByteString.fromArray(plutusScript.getScriptHash)

    val scriptHashString: String = encodeHex(IArray.unsafeFromArray(plutusScript.getScriptHash))

    def address(n: Network): AddressBechL1 = {
        val address = AddressProvider.getEntAddress(plutusScript, n.toBB)
        address.getAddress |> AddressBech[L1].apply
    }

     */
}
