package scalus.examples.vesting

import scalus.compiler.Compile
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.v1.Value
import scalus.cardano.onchain.plutus.v1.Value.*
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*

// Datum
case class Config(
    beneficiary: PubKeyHash,
    startTimestamp: PosixTime,
    duration: PosixTime,
    initialAmount: Lovelace
) derives FromData,
      ToData

@Compile
object Config

// Redeemer
case class Action(amount: Lovelace) derives FromData, ToData

@Compile
object Action

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
        val vestingDatum = datum.getOrFail(DatumNotFound).to[Config]
        val Action(requestedAmount) = redeemer.to[Action]

        require(requestedAmount > 0, NonPositiveAmount)

        val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
        val contractAddress = ownInput.address

        // Reject spending more than one vesting UTxO at once: otherwise a single continuing
        // output could satisfy several script inputs (double satisfaction) and the remaining
        // locked funds of the extra inputs would be siphoned off.
        require(
          txInfo.findOwnInputsByCredential(contractAddress.credential).length === BigInt(1),
          MultipleVestingInputs
        )

        val contractAmount = ownInput.value.getLovelace

        val contractOutputs = txInfo.findOwnOutputsByCredential(contractAddress.credential)

        val txEarliestTime = txInfo.getValidityStartTime

        val released = vestingDatum.initialAmount - contractAmount

        val availableAmount = linearVesting(vestingDatum, txEarliestTime) - released

        require(
          txInfo.isSignedBy(vestingDatum.beneficiary),
          NoBeneficiarySignature
        )
        require(
          requestedAmount <= availableAmount,
          AmountExceedsAvailable
        )

        val beneficiaryCred = Credential.PubKeyCredential(vestingDatum.beneficiary)

        val beneficiaryInputs = txInfo.findOwnInputsByCredential(beneficiaryCred)
        val beneficiaryOutputs = txInfo.findOwnOutputsByCredential(beneficiaryCred)

        val adaInInputs = Utils.getAdaFromInputs(beneficiaryInputs)
        val adaInOutputs = Utils.getAdaFromOutputs(beneficiaryOutputs)

        val expectedOutput =
            requestedAmount + adaInInputs - txInfo.fee

        require(
          adaInOutputs === expectedOutput,
          BeneficiaryOutputMismatch
        )

        if requestedAmount === contractAmount then ()
        else
            require(contractOutputs.length === BigInt(1), NotExactlyOneContractOutput)

            val contractOutput = contractOutputs.head

            // Pin the continuing output to the exact own input address: matching the payment
            // credential alone would let the staking credential (and thus delegation rewards)
            // be redirected to the attacker.
            require(contractOutput.address === ownInput.address, ContinuingAddressMismatch)

            // The continuing output must preserve the entire remaining value — ADA and any
            // native tokens — minus only the withdrawn lovelace. A lovelace-only check would
            // let native tokens be stripped out of the locked UTxO.
            require(
              contractOutput.value === ownInput.value - Value.lovelace(requestedAmount),
              ContinuingValueMismatch
            )

            contractOutput.datum match
                case OutputDatum.OutputDatum(inlineData) =>
                    require(inlineData === datum.getOrFail(DatumNotFound), DatumMismatch)
                case _ => fail(ExpectedInlineDatum)
    }

    def linearVesting(vestingDatum: Config, timestamp: BigInt): BigInt = {
        val min = vestingDatum.startTimestamp
        val max = vestingDatum.startTimestamp + vestingDatum.duration
        if timestamp < min then 0
        else if timestamp >= max then vestingDatum.initialAmount
        else
            vestingDatum.initialAmount * (timestamp - vestingDatum.startTimestamp) / vestingDatum.duration
    }

    // Error messages
    inline val DatumNotFound = "Datum not found"
    inline val NonPositiveAmount = "Withdrawal amount must be greater than 0"
    inline val MultipleVestingInputs = "Only one vesting input may be spent per transaction"
    inline val NoBeneficiarySignature = "No signature from beneficiary"
    inline val AmountExceedsAvailable = "Requested amount exceeds the available vested amount"
    inline val BeneficiaryOutputMismatch = "Beneficiary output mismatch"
    inline val NotExactlyOneContractOutput = "Expected exactly one contract output"
    inline val ContinuingAddressMismatch = "Continuing output must keep the vesting address"
    inline val ContinuingValueMismatch =
        "Continuing output must preserve the remaining vested value"
    inline val DatumMismatch = "VestingDatum mismatch"
    inline val ExpectedInlineDatum = "Expected inline datum"
}
