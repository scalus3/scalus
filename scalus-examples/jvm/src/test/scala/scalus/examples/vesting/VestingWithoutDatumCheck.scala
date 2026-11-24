package scalus.examples.vesting

import scalus.cardano.onchain.plutus.v3.Value.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*

/** [[VestingValidator]] without its check of the datum of the output that keeps the rest: the
  * negative control of `VestingVerificationTest`'s statement that a withdrawal carries the state.
  *
  * Each withdrawal it accepts still meets the first three guarantees of `spend`, as each is of the
  * transaction's own datum. But a withdrawal may leave the rest under a datum of its own, of a
  * period of no time, and the next withdrawal then takes everything: a fault that shows only over
  * two transactions.
  */
@Compile
object VestingWithoutDatumCheck extends Validator {

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val vestingDatum = datum.getOrFail(VestingValidator.DatumNotFound)
        val vestingConfig = vestingDatum.to[Config]
        val Action(requestedAmount) = redeemer.to[Action]
        require(requestedAmount > 0, VestingValidator.NonPositiveAmount)
        val ownInputInfo = txInfo.findInputOrFail(txOutRef)
        val ownInput = ownInputInfo.resolved
        val ownCredential = ownInput.address.credential
        txInfo.inputs.findUniqueOrFail(
          _.resolved.address.credential === ownCredential,
          VestingValidator.MultipleVestingInputs
        )
        val contractAmount = ownInput.value.getLovelace
        val txEarliestTime = txInfo.validFromOrFail(VestingValidator.NoValidityLowerBound)
        val released = vestingConfig.initialAmount - contractAmount
        val availableAmount =
            VestingValidator.linearVesting(vestingConfig, txEarliestTime) - released
        require(
          txInfo.isSignedBy(vestingConfig.beneficiary),
          VestingValidator.NoBeneficiarySignature
        )
        require(requestedAmount <= availableAmount, VestingValidator.AmountExceedsAvailable)
        val beneficiaryCred = Credential.PubKeyCredential(vestingConfig.beneficiary)
        val beneficiaryInputs = txInfo.findInputsByCredential(beneficiaryCred)
        val beneficiaryOutputs = txInfo.findOutputsByCredential(beneficiaryCred)
        val adaInInputs = beneficiaryInputs.foldLeft(Value.zero)(_ + _.resolved.value).getLovelace
        val adaInOutputs = beneficiaryOutputs.foldLeft(Value.zero)(_ + _.value).getLovelace
        val expectedOutput = requestedAmount + adaInInputs - txInfo.fee
        require(adaInOutputs === expectedOutput, VestingValidator.BeneficiaryOutputMismatch)
        if requestedAmount === contractAmount then ()
        else
            val contractOutput = txInfo.findContinuingOutputOrFail(
              ownInputInfo,
              VestingValidator.NotExactlyOneContractOutput
            )
            require(
              contractOutput.value === ownInput.value - Value.lovelace(requestedAmount),
              VestingValidator.ContinuingValueMismatch
            )
            // VestingValidator checks here that the output keeps the datum:
            // require(contractOutput.hasInlineDatum(vestingDatum), InvalidDatum)
    }
}
