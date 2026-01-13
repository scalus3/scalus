package scalus.examples.vesting

import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*

// Datum
case class Config(
    beneficiary: PubKeyHash,
    startTimestamp: PosixTime,
    duration: PosixTime,
    initialAmount: Lovelace
) derives FromData,
      ToData

@Compile
object Config {
    given Eq[Config] = Eq.derived
}

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
        val vestingDatum = datum.getOrFail("Datum not found").to[Config]
        val Action(requestedAmount) = redeemer.to[Action]

        require(requestedAmount > 0, "Withdrawal amount must be greater than 0")

        val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
        val contractAddress = ownInput.address
        val contractAmount = ownInput.value.getLovelace

        val contractOutputs = txInfo.findOwnOutputsByCredential(contractAddress.credential)

        val txEarliestTime = txInfo.getValidityStartTime

        val released = vestingDatum.initialAmount - contractAmount

        val availableAmount = linearVesting(vestingDatum, txEarliestTime) - released

        require(
          txInfo.isSignedBy(vestingDatum.beneficiary),
          "No signature from beneficiary"
        )
        require(
          requestedAmount <= availableAmount,
          "Declared amount does not match calculated amount"
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
          "Beneficiary output mismatch"
        )

        if requestedAmount === contractAmount then ()
        else require(contractOutputs.length === BigInt(1), "Expected exactly one contract output")

        val contractOutput = contractOutputs.head
        contractOutput.datum match
            case OutputDatum.OutputDatum(inlineData) =>
                require(
                  inlineData === datum.getOrFail("Datum not found"),
                  "VestingDatum mismatch"
                )
            case _ => fail("Expected inline datum")
    }

    def linearVesting(vestingDatum: Config, timestamp: BigInt): BigInt = {
        val min = vestingDatum.startTimestamp
        val max = vestingDatum.startTimestamp + vestingDatum.duration
        if timestamp < min then 0
        else if timestamp >= max then vestingDatum.initialAmount
        else
            vestingDatum.initialAmount * (timestamp - vestingDatum.startTimestamp) / vestingDatum.duration
    }
}
