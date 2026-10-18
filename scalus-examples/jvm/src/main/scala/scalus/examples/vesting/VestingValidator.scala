package scalus.examples.vesting

import scalus.compiler.Compile
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.v1.Value
import scalus.cardano.onchain.plutus.v1.Value.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.cardano.onchain.plutus.prelude.Spec.ensuring

// Datum
case class Config(
    beneficiary: PubKeyHash,
    startTimestamp: PosixTime,
    duration: PosixTime,
    initialAmount: Lovelace
) derives FromData,
      ToData

// Redeemer
case class Action(amount: Lovelace) derives FromData, ToData

/** Locks up funds and allows the beneficiary to withdraw the funds after the lockup period
  *
  * When a new employee joins an organization, they typically receive a promise of compensation to
  * be disbursed after a specified duration of employment. This arrangement often involves the
  * organization depositing the funds into a vesting contract, with the employee gaining access to
  * the funds upon the completion of a predetermined lockup period. Through the utilization of
  * vesting contracts, organizations establish a mechanism to encourage employee retention by
  * linking financial rewards to tenure.
  *
  * @see
  *   [[https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/vesting]]
  *   [[https://meshjs.dev/smart-contracts/vesting]]
  *   [[https://github.com/cardano-foundation/cardano-template-and-ecosystem-monitoring/tree/main/vesting]]
  */
@Compile
object VestingValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        // What holds of every spend the script accepts. These clauses are its specification, not
        // checks: they are no part of the script, and `VestingVerificationTest` proves them
        // about the compiled validator.

        // 1. Authorisation. The beneficiary named in the datum signed the transaction.
        Spec.ensures(txInfo.isSignedBy(configOf(datum).beneficiary))

        // 2. A withdrawal takes something.
        Spec.ensures(amountOf(redeemer) > 0)

        // 3. No early release. What stays locked is at least what has not vested yet, at the
        //    earliest time the transaction can be included.
        Spec.ensures(
          lockedBy(txInfo, txOutRef) - amountOf(redeemer) >=
              configOf(datum).initialAmount -
              linearVesting(configOf(datum), txInfo.validFromOrFail(NoValidityLowerBound))
        )

        val vestingDatum = datum.getOrFail(DatumNotFound)
        val vestingConfig = vestingDatum.to[Config]
        val Action(requestedAmount) = redeemer.to[Action]

        require(requestedAmount > 0, NonPositiveAmount)

        val ownInputInfo = txInfo.findInputOrFail(txOutRef)
        val ownInput = ownInputInfo.resolved
        val ownCredential = ownInput.address.credential

        // Reject spending more than one vesting UTxO at once: otherwise a single continuing
        // output could satisfy several script inputs (double satisfaction) and the remaining
        // locked funds of the extra inputs would be siphoned off.
        txInfo.inputs.findUniqueOrFail(
          _.resolved.address.credential === ownCredential,
          MultipleVestingInputs
        )

        val contractAmount = ownInput.value.getLovelace

        val txEarliestTime = txInfo.validFromOrFail(NoValidityLowerBound)

        val released = vestingConfig.initialAmount - contractAmount

        val availableAmount = linearVesting(vestingConfig, txEarliestTime) - released

        require(
          txInfo.isSignedBy(vestingConfig.beneficiary),
          NoBeneficiarySignature
        )
        require(
          requestedAmount <= availableAmount,
          AmountExceedsAvailable
        )

        val beneficiaryCred = Credential.PubKeyCredential(vestingConfig.beneficiary)

        val beneficiaryInputs = txInfo.findInputsByCredential(beneficiaryCred)
        val beneficiaryOutputs = txInfo.findOutputsByCredential(beneficiaryCred)

        // The beneficiary is a key, paid in lovelace by design; sum the whole value and project.
        val adaInInputs = beneficiaryInputs.foldLeft(Value.zero)(_ + _.resolved.value).getLovelace
        val adaInOutputs = beneficiaryOutputs.foldLeft(Value.zero)(_ + _.value).getLovelace

        val expectedOutput =
            requestedAmount + adaInInputs - txInfo.fee

        require(
          adaInOutputs === expectedOutput,
          BeneficiaryOutputMismatch
        )

        if requestedAmount === contractAmount then ()
        else
            // The unique output to the exact own input address: matching the payment credential
            // alone would let the staking credential (and thus delegation rewards) be redirected
            // to the attacker.
            val contractOutput =
                txInfo.findContinuingOutputOrFail(ownInputInfo, NotExactlyOneContractOutput)

            // The continuing output must preserve the entire remaining value — ADA and any
            // native tokens — minus only the withdrawn lovelace. A lovelace-only check would
            // let native tokens be stripped out of the locked UTxO.
            require(
              contractOutput.value === ownInput.value - Value.lovelace(requestedAmount),
              ContinuingValueMismatch
            )

            require(contractOutput.hasInlineDatum(vestingDatum), InvalidDatum)
    }

    /** The amount vested at `timestamp`: nothing before the start, everything from the end of the
      * period on, and a share of the initial amount in proportion to the time in between.
      *
      * The `ensuring` clause is its specification: the bound is conditional and not a precondition,
      * because the validator takes the amount from the datum as it is.
      */
    def linearVesting(vestingDatum: Config, timestamp: BigInt): BigInt = {
        val min = vestingDatum.startTimestamp
        val max = vestingDatum.startTimestamp + vestingDatum.duration
        if timestamp < min then BigInt(0)
        else if timestamp >= max then vestingDatum.initialAmount
        else
            val elapsed = timestamp - vestingDatum.startTimestamp
            vestingDatum.initialAmount * elapsed divFloor vestingDatum.duration
    }.ensuring(vested =>
        vestingDatum.initialAmount < 0 || (vested >= 0 && vested <= vestingDatum.initialAmount)
    )

    // What the `Spec` clauses of `spend` speak of. A clause sees the handler's parameters, not the
    // values its body computes, so it derives them again. Only the clauses use these functions,
    // and they are removed from the script with them.

    def configOf(datum: Option[Data]): Config = datum.getOrFail(DatumNotFound).to[Config]

    def amountOf(redeemer: Data): BigInt = redeemer.to[Action].amount

    /** The lovelace in the vesting output being spent. */
    def lockedBy(txInfo: TxInfo, txOutRef: TxOutRef): BigInt =
        txInfo.findInputOrFail(txOutRef).resolved.value.getLovelace

    // Error messages
    inline val DatumNotFound = "Datum not found"
    inline val NonPositiveAmount = "Withdrawal amount must be greater than 0"
    inline val MultipleVestingInputs = "Only one vesting input may be spent per transaction"
    inline val NoBeneficiarySignature = "No signature from beneficiary"
    inline val AmountExceedsAvailable = "Requested amount exceeds the available vested amount"
    inline val BeneficiaryOutputMismatch = "Beneficiary output mismatch"
    inline val NotExactlyOneContractOutput = "Expected exactly one contract output"
    inline val ContinuingValueMismatch =
        "Continuing output must preserve the remaining vested value"
    inline val InvalidDatum = "VestingDatum mismatch"
    inline val NoValidityLowerBound = "Transaction validity range must have a lower bound"
}
