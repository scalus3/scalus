package scalus.examples.cape.linearvesting

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{FromData, ToData}

case class VestingAsset(currencySymbol: ByteString, tokenName: ByteString) derives FromData, ToData

case class VestingDatum(
    beneficiary: Address,
    asset: VestingAsset,
    totalVestingQty: BigInt,
    vestingPeriodStart: BigInt,
    vestingPeriodEnd: BigInt,
    firstUnlockPossibleAfter: BigInt,
    totalInstallments: BigInt
) derives FromData,
      ToData

enum VestingRedeemer derives FromData, ToData:
    case PartialUnlock
    case FullUnlock

/** UPLC-CAPE linear vesting validator: schedule-driven partial unlocks, full unlock after end.
  *
  * Funds locked under a [[VestingDatum]] unlock linearly over `[vestingPeriodStart,
  * vestingPeriodEnd]`, split into `totalInstallments` equal steps. Two redeemers:
  *
  *   - `PartialUnlock`: allowed only after `firstUnlockPossibleAfter`. The beneficiary may withdraw
  *     down to (but not below) the schedule's expected remaining quantity for the current time, and
  *     must re-lock the rest in a continuing output that carries the same datum unchanged.
  *   - `FullUnlock`: allowed only after `vestingPeriodEnd`. No continuing output is required; the
  *     beneficiary is trusted to withdraw everything.
  *
  * The remaining-quantity schedule rounds up at every step (`divCeil`): the number of installments
  * still owed after `currentTime`, and the quantity that must still remain locked for those
  * installments, are both computed with ceiling division so the last unlock never releases
  * fractionally more than the schedule allows.
  *
  * All context plumbing uses the standard prelude idioms: `TxInfo.findOwnInputOrFail`,
  * `TxInfo.isSignedBy`, `List.count`, a plain `txInfo.outputs.find` for the first continuing
  * output, and `OutputDatum.inlineOrFail`. Earlier revisions hand-rolled several of these because
  * the prelude versions measured slower; the local copies are dropped in favor of the canonical
  * forms anyway - library/compiler-level fixes, not validator-level workarounds, are the intended
  * remedy for the cost gap. A compiler fix that dispatches `List.contains` (and siblings) to the
  * `equalsData`-scan intrinsic for lazily-decoded (`PackedSumDataList`) receivers such as
  * `txInfo.signatories` is pending review on the `feat/intrinsic-eq-packed-list` branch and is NOT
  * part of this branch; the numbers here are measured without it. See
  * `docs/internal/CAPE_COMPETITIVE_ANALYSIS.md` for the deltas and leaderboard standing.
  *
  * The continuing output is the *first* output at the script credential, not a uniqueness check:
  * CAPE builds measurement fixtures by patching an `add_output_utxo` onto a baseline that already
  * carries one, so a second output can legitimately land at the script credential and the validator
  * inspects the newest. No fund-safety gap follows: a `PartialUnlock` tx is capped to one script
  * input (the count-based double-satisfaction guard) signed by that input's own beneficiary, and
  * only `quantityOf(asset)` is ever checked, never full `Value` equality.
  *
  * @see
  *   [[https://github.com/IntersectMBO/UPLC-CAPE]]
  */
@Compile
object LinearVestingValidator extends Validator {

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val datumData = datum.getOrFail(DatumNotFound)
        val d = datumData.to[VestingDatum]
        val beneficiaryPkh = d.beneficiary.credential match
            case Credential.PubKeyCredential(pkh) => pkh
            case _                                => fail(ExpectedPubKeyBeneficiary)
        require(txInfo.isSignedBy(beneficiaryPkh), NoBeneficiarySignature)
        // The ledger always constructs a finite validity-range lower bound as inclusive
        // (LedgerToPlutusTranslation.getInterval calls IntervalBound.finiteInclusive for every
        // finite-lower-bound case; there is no code path producing a finite *exclusive* lower
        // bound). finiteOrFail alone is therefore correct here without an inclusive/exclusive
        // adjustment -- unlike the upper bound, whose closure genuinely varies by protocol
        // version.
        val currentTime = txInfo.validRange.from.finiteOrFail(LowerBoundMustBeFinite)
        redeemer.to[VestingRedeemer] match
            case VestingRedeemer.FullUnlock =>
                require(currentTime > d.vestingPeriodEnd, VestingPeriodNotOver)
            case VestingRedeemer.PartialUnlock =>
                require(currentTime > d.firstUnlockPossibleAfter, TooEarlyToUnlock)
                val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
                val ownCred = ownInput.address.credential
                // Double-satisfaction guard: spending several script UTxOs at once would let one
                // continuing output satisfy all of them, siphoning off the extra inputs' funds.
                require(
                  txInfo.inputs.count(_.resolved.address.credential === ownCred) === BigInt(1),
                  MultipleScriptInputs
                )
                val oldRemaining =
                    ownInput.value.quantityOf(d.asset.currencySymbol, d.asset.tokenName)
                val continuing =
                    txInfo.outputs
                        .find(_.address.credential === ownCred)
                        .getOrFail(OwnOutputNotFound)
                val newRemaining =
                    continuing.value.quantityOf(d.asset.currencySymbol, d.asset.tokenName)
                require(newRemaining > BigInt(0), NothingLeftUseFullUnlock)
                require(newRemaining < oldRemaining, MustWithdrawSomething)
                val timeBetween =
                    divCeil(d.vestingPeriodEnd - d.vestingPeriodStart, d.totalInstallments)
                val futureInstallments = divCeil(d.vestingPeriodEnd - currentTime, timeBetween)
                val expectedRemaining =
                    divCeil(futureInstallments * d.totalVestingQty, d.totalInstallments)
                require(newRemaining === expectedRemaining, WrongRemainingQuantity)
                require(
                  continuing.datum.inlineOrFail[Data](ContinuingOutputMustCarryDatum) === datumData,
                  DatumMustBePreserved
                )
    }

    def divCeil(x: BigInt, y: BigInt): BigInt = 1 + ((x - 1) / y)

    // Error messages
    inline val DatumNotFound = "No datum"
    inline val ExpectedPubKeyBeneficiary = "Expected pubkey beneficiary"
    inline val NoBeneficiarySignature = "Beneficiary must sign"
    inline val LowerBoundMustBeFinite = "Lower bound must be finite"
    inline val VestingPeriodNotOver = "Vesting period not over"
    inline val TooEarlyToUnlock = "Too early to unlock"
    inline val MultipleScriptInputs = "Multiple script inputs"
    inline val OwnOutputNotFound = "Own output not found"
    inline val NothingLeftUseFullUnlock = "Nothing left: use FullUnlock"
    inline val MustWithdrawSomething = "Must withdraw something"
    inline val WrongRemainingQuantity = "Wrong remaining quantity"
    inline val DatumMustBePreserved = "Datum must be preserved"
    inline val ContinuingOutputMustCarryDatum = "Continuing output must carry the datum"
}
