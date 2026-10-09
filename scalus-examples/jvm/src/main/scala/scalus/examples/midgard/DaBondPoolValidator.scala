package scalus.examples.midgard

import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.Compile
import scalus.uplc.builtin.Builtins.{blake2b_256, unBData, unConstrData}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData

/** The pooled DA committee bond of Midgard (spec #685 §2.1, §4.1; ticket #687).
  *
  * Port of `onchain/aiken/validators/da-bond-pool.ak`. One UTxO at `Script(own policy)` holds the
  * committee's bond, identified by a one-shot NFT that is never burned. Anyone tops it up; only an
  * availability timeout slashes it; only the DA params owner quorum withdraws from it, in two steps
  * separated by `daBondWithdrawDelayMs`.
  *
  * Every continuation keeps the pool address (payment and stake), carries an inline datum and no
  * reference script, and holds exactly lovelace plus the NFT. Exactly one input per transaction may
  * sit at the pool's credential.
  *
  * Aiken's `expect x: T = data` shape checks are not ported: each decoded UTxO is authenticated by
  * an NFT whose minting policy fixes its datum shape, and every redeemer field fails at its first
  * use. See `docs/internal/MIDGARD_DA_BOND_POOL_SCALUS_VS_AIKEN.md`.
  */
@Compile
object DaBondPoolValidator {
    import DaBondPoolConstants.*

    /** The script. Takes the four Aiken parameters in Aiken's order, each applied as `Data`, then
      * the script context. Only the `mint` and `spend` purposes are accepted.
      */
    inline def validate(
        initRef: Data,
        hubOraclePolicyId: Data,
        daParamsPolicyId: Data,
        parameters: Data
    )(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(policyId) =>
                mint(
                  initRef.to[TxOutRef],
                  parameters.to[ParametersV1],
                  sc.redeemer,
                  policyId,
                  sc.txInfo
                )
            case ScriptInfo.SpendingScript(ownRef, _) =>
                spend(
                  hubOraclePolicyId.to[PolicyId],
                  daParamsPolicyId.to[PolicyId],
                  parameters.to[ParametersV1],
                  sc.redeemer,
                  sc.txInfo,
                  ownRef
                )
            case _ => fail(PurposeNotAllowed)
    }

    /** One-shot: spends `initRef` and mints the one NFT to a `Bonded` pool at `Script(own policy)`
      * with no stake credential. There is no burn arm, so every other mint or burn fails.
      */
    inline def mint(
        initRef: TxOutRef,
        parameters: ParametersV1,
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        val outputIndex = redeemer.to[DaBondPoolMintRedeemer] match
            case DaBondPoolMintRedeemer.InitPool(i) => i
        require(tx.mint.hasOnly(policyId, poolAssetName, 1), MintExactlyOneNft)
        tx.findInputOrFail(initRef, InitRefNotSpent)
        val out = tx.outputs.at(outputIndex)
        require(out.referenceScript.isEmpty, ReferenceScriptForbidden)
        require(
          out.address === Address(Credential.ScriptCredential(policyId), Option.None),
          InitAddressMismatch
        )
        require(out.hasInlineDatum(bondedData), InitDatumNotBonded)
        require(out.value.withoutLovelace === Value(policyId, poolAssetName, 1), PoolValueNotExact)
        require(out.value.getLovelace >= parameters.daBondPoolFloorLovelace, BelowFloor)
    }

    inline def spend(
        hubOraclePolicyId: PolicyId,
        daParamsPolicyId: PolicyId,
        parameters: ParametersV1,
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val poolIn = tx.findInputOrFail(ownRef, OwnInputMissing).resolved
        val poolAddress = poolIn.address
        val ownCredential = poolAddress.credential
        val ownPolicy = ownCredential.scriptHashOrFail(PoolInputNotScript)
        require(poolIn.value.hasNft(ownPolicy, poolAssetName), PoolInputNoNft)
        // A stray UTxO at the pool credential cannot ride along with a pool action.
        tx.inputs.findUniqueOrFail(_.resolved.address.credential === ownCredential, SecondPoolInput)
        val poolInDatum = inlineDatum(poolIn)
        val poolInLovelace = poolIn.value.getLovelace

        redeemer.to[DaBondPoolSpendRedeemer] match
            case DaBondPoolSpendRedeemer.TopUp(outputIndex) =>
                val out = continuingPoolOutput(tx.outputs, outputIndex, poolAddress, ownPolicy)
                require(out.hasInlineDatum(poolInDatum), DatumChanged)
                val outLovelace = out.value.getLovelace
                require(
                  outLovelace - poolInLovelace >= parameters.daBondMinTopUpLovelace,
                  TopUpTooSmall
                )

            // Slash binds to the first step of an unavailable-block removal, the only step that
            // runs the availability `TimeoutChallenge`: the tx's state-queue mint redeemer is
            // `RemoveUnavailableBlockAfterTimeout`, and the correction lock it spends is `Idle`.
            // Valid in both states; the pool gives up exactly `min(da_bond, backing)`.
            case DaBondPoolSpendRedeemer.Slash(
                  hubOracleRefInputIndex,
                  stateQueueMintRedeemerIndex,
                  correctionLockInputIndex,
                  outputIndex
                ) =>
                val stateQueuePolicy =
                    getStateQueuePolicy(
                      tx.referenceInputs,
                      hubOraclePolicyId,
                      hubOracleRefInputIndex
                    )
                val stateQueueRedeemer = getRedeemerAt(
                  tx.redeemers,
                  ScriptPurpose.Minting(stateQueuePolicy),
                  stateQueueMintRedeemerIndex
                )
                require(
                  unConstrData(stateQueueRedeemer).fst === removeUnavailableBlockAfterTimeoutConstr,
                  NotUnavailableRemoval
                )
                val lock = tx.inputs.at(correctionLockInputIndex).resolved
                require(
                  lock.value.hasNft(hubOraclePolicyId, correctionLockAssetName),
                  NotCorrectionLock
                )
                // `correction_lock.Idle` is constructor 0.
                require(
                  unConstrData(inlineDatum(lock)).fst === BigInt(
                    0
                  ),
                  CorrectionLockNotIdle
                )
                val taken =
                    Math.min(parameters.daBondLovelace, lovelaceBacking(poolInLovelace, parameters))
                val out = continuingPoolOutput(tx.outputs, outputIndex, poolAddress, ownPolicy)
                require(out.hasInlineDatum(poolInDatum), DatumChanged)
                val outLovelace = out.value.getLovelace
                require(outLovelace === poolInLovelace - taken, SlashAmountWrong)

            case DaBondPoolSpendRedeemer.BeginWithdraw(daParamsRefInputIndex, outputIndex) =>
                requireOwnerQuorum(tx, daParamsPolicyId, daParamsRefInputIndex)
                poolInDatum.to[DaBondPoolDatum] match
                    case DaBondPoolDatum.Bonded => ()
                    case _                      => fail(BeginNeedsBonded)
                // Anchored at the inclusive upper bound: the tx lands at or before it, so the
                // delay is never shortened. The ledger builds the range with an inclusive lower
                // and an exclusive upper bound, and phase 1 rejects an empty range, so the
                // inclusive upper bound is `validTo - 1`.
                val lower = tx.validFromOrFail(RangeNotShort)
                val upper = tx.validToOrFail(RangeNotShort) - 1
                require(upper - lower <= maxValidityRangeLength, RangeNotShort)
                val out = continuingPoolOutput(tx.outputs, outputIndex, poolAddress, ownPolicy)
                require(
                  out.hasInlineDatum(DaBondPoolDatum.Withdrawing(upper + daBondWithdrawDelayMs)),
                  UnlockAtWrong
                )
                val outLovelace = out.value.getLovelace
                require(outLovelace === poolInLovelace, LovelaceChanged)

            case DaBondPoolSpendRedeemer.CancelWithdraw(daParamsRefInputIndex, outputIndex) =>
                requireOwnerQuorum(tx, daParamsPolicyId, daParamsRefInputIndex)
                poolInDatum.to[DaBondPoolDatum] match
                    case DaBondPoolDatum.Withdrawing(_) => ()
                    case _                              => fail(CancelNeedsWithdrawing)
                val out = continuingPoolOutput(tx.outputs, outputIndex, poolAddress, ownPolicy)
                require(out.hasInlineDatum(bondedData), OutputNotBonded)
                val outLovelace = out.value.getLovelace
                require(outLovelace === poolInLovelace, LovelaceChanged)

            case DaBondPoolSpendRedeemer.CompleteWithdraw(
                  amount,
                  daParamsRefInputIndex,
                  outputIndex
                ) =>
                requireOwnerQuorum(tx, daParamsPolicyId, daParamsRefInputIndex)
                val unlockAt = poolInDatum.to[DaBondPoolDatum] match
                    case DaBondPoolDatum.Withdrawing(t) => t
                    case _                              => fail(CompleteNeedsWithdrawing)
                // The lower bound is inclusive: the tx lands no earlier than it, so never
                // before `unlockAt`.
                require(tx.validFromOrFail(RangeNoLowerBound) >= unlockAt, StillLocked)
                require(amount > 0, AmountNotPositive)
                require(amount <= lovelaceBacking(poolInLovelace, parameters), AmountAboveBacking)
                val out = continuingPoolOutput(tx.outputs, outputIndex, poolAddress, ownPolicy)
                require(out.hasInlineDatum(bondedData), OutputNotBonded)
                val outLovelace = out.value.getLovelace
                require(outLovelace === poolInLovelace - amount, WithdrawAmountWrong)
    }

    // ------------------------------------------------------------------------
    // Error messages
    // ------------------------------------------------------------------------

    inline val MintExactlyOneNft =
        "Must mint exactly one pool NFT and nothing else under the policy"
    inline val InitRefNotSpent = "Must spend the one-shot init_ref"
    inline val InitAddressMismatch = "New pool must sit at Script(own policy) with no stake"
    inline val InitDatumNotBonded = "New pool datum must be Bonded"
    inline val BelowFloor = "Pool lovelace must reach the floor"
    inline val PurposeNotAllowed = "Only mint and spend are allowed"
    inline val InlineDatumRequired = "Output must carry an inline datum"
    inline val ReferenceScriptForbidden = "Pool output must not carry a reference script"
    inline val PoolAddressChanged = "Continuing output must keep the pool address"
    inline val PoolValueNotExact = "Pool output must hold lovelace and the pool NFT only"
    inline val OwnInputMissing = "Own input not found"
    inline val PoolInputNotScript = "Pool input must sit at a script credential"
    inline val PoolInputNoNft = "Pool input must hold the pool NFT"
    inline val SecondPoolInput = "Exactly one input may sit at the pool credential"
    inline val DatumChanged = "Continuing datum must equal the input datum"
    inline val TopUpTooSmall = "Top-up is below the minimum"
    inline val HubNotAuthentic = "Hub oracle reference input is not authentic"
    inline val RedeemerPurposeMismatch = "Redeemer at the index must be the state-queue mint"
    inline val NotUnavailableRemoval =
        "State-queue redeemer must be RemoveUnavailableBlockAfterTimeout"
    inline val NotCorrectionLock = "Input at the index must hold the correction-lock token"
    inline val CorrectionLockNotIdle = "Correction lock must be Idle"
    inline val SlashAmountWrong = "Pool must give up exactly min(da_bond, backing)"
    inline val ParamsNoNft = "DA params input must hold the params NFT"
    inline val ParamsWrongCredential = "DA params input must sit at the params script"
    inline val ParamsReferenceScript = "DA params input must not carry a reference script"
    inline val CommitteeHashMismatch = "DA params committee hash mismatch"
    inline val QuorumNotMet = "DA params owner quorum must sign"
    inline val BeginNeedsBonded = "BeginWithdraw needs a Bonded pool"
    inline val CancelNeedsWithdrawing = "CancelWithdraw needs a Withdrawing pool"
    inline val CompleteNeedsWithdrawing = "CompleteWithdraw needs a Withdrawing pool"
    inline val UnlockAtWrong = "Withdrawing unlock_at must be the upper bound plus the delay"
    inline val LovelaceChanged = "Pool lovelace must not change"
    inline val OutputNotBonded = "Continuing datum must be Bonded"
    inline val StillLocked = "Withdrawal is still locked"
    inline val AmountNotPositive = "Amount must be positive"
    inline val AmountAboveBacking = "Amount must not exceed the backing"
    inline val WithdrawAmountWrong = "Pool must give up exactly the amount"
    inline val RangeNotShort = "Validity range must be bounded and at most 480 s long"
    inline val RangeNoLowerBound = "Validity range must have a lower bound"

    // ------------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------------

    /** `DaBondPoolDatum.Bonded` as Data. */
    def bondedData: Data = DaBondPoolDatum.Bonded.toData

    /** The inline datum of `out`. One shared function: `inlineOrFail` is `inline`, and expanding it
      * at each of the four call sites costs 35 B of script.
      */
    def inlineDatum(out: TxOut): Data = out.datum.inlineOrFail[Data](InlineDatumRequired)

    /** `max(0, lovelace - floor)`: the lovelace above the pool floor. */
    def lovelaceBacking(lovelace: BigInt, parameters: ParametersV1): BigInt =
        Math.max(BigInt(0), lovelace - parameters.daBondPoolFloorLovelace)

    /** The continuing pool output at `outputIndex`: the pool address, no reference script, and
      * lovelace plus the NFT only. Each arm then checks its inline datum with `hasInlineDatum`.
      */
    def continuingPoolOutput(
        outputs: List[TxOut],
        outputIndex: BigInt,
        poolAddress: Address,
        ownPolicy: PolicyId
    ): TxOut = {
        val out = outputs.at(outputIndex)
        require(out.referenceScript.isEmpty, ReferenceScriptForbidden)
        require(out.address === poolAddress, PoolAddressChanged)
        require(out.value.withoutLovelace === Value(ownPolicy, poolAssetName, 1), PoolValueNotExact)
        out
    }

    /** The DA params owner quorum signed the transaction. The params UTxO is authenticated by
      * credential, NFT and inline datum, and the count walks the owners, never the signers.
      */
    def requireOwnerQuorum(tx: TxInfo, daParamsPolicyId: PolicyId, refInputIndex: BigInt): Unit = {
        val params = getDaParams(tx.referenceInputs, daParamsPolicyId, refInputIndex)
        require(
          ownerQuorumMet(params.owners, tx.signatories, params.updateThreshold, 0),
          QuorumNotMet
        )
    }

    /** Port of `da_params.get_da_params`. */
    def getDaParams(
        referenceInputs: List[TxInInfo],
        daParamsPolicyId: PolicyId,
        refInputIndex: BigInt
    ): DaParamsDatum = {
        val out = referenceInputs.at(refInputIndex).resolved
        val datum = inlineDatum(out).to[DaParamsDatum]
        require(out.referenceScript.isEmpty, ParamsReferenceScript)
        require(
          out.address.credential === Credential.ScriptCredential(daParamsPolicyId),
          ParamsWrongCredential
        )
        require(
          out.value.hasNft(daParamsPolicyId, daParamsAssetName),
          ParamsNoNft
        )
        require(blake2b_256(datum.committee) === datum.committeeSignersHash, CommitteeHashMismatch)
        datum
    }

    /** Port of `da_params.owner_quorum_met`: stops as soon as `threshold` owners have signed.
      *
      * Takes `tx.signatories` once. Calling `tx.isSignedBy(owner)` per owner re-reads the field
      * from `TxInfo` every time: +4,728 memory and +440 lovelace per quorum transaction.
      */
    def ownerQuorumMet(
        owners: List[PubKeyHash],
        signers: List[PubKeyHash],
        threshold: BigInt,
        count: BigInt
    ): Boolean =
        if count >= threshold then true
        else
            owners match
                case List.Nil => false
                case List.Cons(owner, rest) =>
                    val next = if signers.contains(owner) then count + 1 else count
                    ownerQuorumMet(rest, signers, threshold, next)

    /** Port of `da_bond_pool.get_state_queue_policy`: field 4 of the authentic hub oracle datum. */
    def getStateQueuePolicy(
        referenceInputs: List[TxInInfo],
        hubOraclePolicyId: PolicyId,
        hubOracleRefInputIndex: BigInt
    ): PolicyId = {
        // `utils.get_authentic_input_of`: at `Script(hub)`, and the only non-ADA asset is the
        // hub NFT.
        val hub = referenceInputs.at(hubOracleRefInputIndex).resolved
        require(
          hub.address.credential === Credential.ScriptCredential(hubOraclePolicyId),
          HubNotAuthentic
        )
        require(
          hub.value.withoutLovelace === Value(hubOraclePolicyId, hubOracleAssetName, 1),
          HubNotAuthentic
        )
        val fields = unConstrData(inlineDatum(hub))
        require(fields.fst === BigInt(0), HubNotAuthentic)
        unBData(fields.snd.tail.tail.tail.tail.head)
    }

    /** Port of `utils.get_redeemer_at`: the redeemer at `index`, whose purpose must match. */
    def getRedeemerAt(
        redeemers: AssocMap[ScriptPurpose, Redeemer],
        purpose: ScriptPurpose,
        index: BigInt
    ): Redeemer = {
        val (entryPurpose, redeemer) = redeemers.toList.at(index)
        require(entryPurpose === purpose, RedeemerPurposeMismatch)
        redeemer
    }
}
