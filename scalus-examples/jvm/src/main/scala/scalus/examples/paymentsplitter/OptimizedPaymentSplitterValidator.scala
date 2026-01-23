package scalus.examples.paymentsplitter

import scalus.*
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.Value.*
import scalus.cardano.onchain.plutus.v1.{Credential, PubKeyHash}
import scalus.cardano.onchain.plutus.v3.*
import scalus.patterns.StakeValidator
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.List.*
import scalus.cardano.onchain.plutus.prelude.Option.*

/** Redeemer for the spending validator containing the own input index.
  *
  * Using the input index directly avoids iterating through all inputs to find the own input.
  *
  * @param ownInputIndex
  *   Index of the own input in txInfo.inputs
  */
case class SpendRedeemer(ownInputIndex: BigInt) derives ToData, FromData

/** Redeemer for the stake validator containing pre-computed split verification data.
  *
  * Off-chain code computes these values and the stake validator verifies they're correct.
  *
  * @param payeeWithChange
  *   PubKeyHash of the payee who pays the fee and receives change
  * @param sumContractInputs
  *   Total lovelace from all contract UTxOs being spent
  * @param splitPerPayee
  *   Equal split amount each payee receives
  * @param nPayed
  *   Number of payees being paid
  */
case class SplitVerificationRedeemer(
    payeeWithChange: PubKeyHash,
    sumContractInputs: BigInt,
    splitPerPayee: BigInt,
    nPayed: BigInt
) derives ToData,
      FromData

/** Optimized Payment Splitter - Split payouts equally among a list of specified payees.
  *
  * This is an optimized implementation using the "stake validator" pattern (withdraw zero trick) to
  * reduce execution costs when spending multiple UTxOs.
  *
  * '''The Problem:''' In the naive [[NaivePaymentSplitterValidator]], each UTxO spend executes the
  * full validation logic - iterating through ALL inputs and outputs. When spending N UTxOs, this
  * results in O(N²) iteration cost.
  *
  * '''The Solution:''' Split the logic:
  *   - '''Stake validator (reward endpoint):''' Runs ONCE, does all heavy computation
  *   - '''Spending validator:''' Runs per UTxO, minimal - just checks stake validator executed
  *
  * '''How it works:'''
  *   1. Off-chain code computes: sumContractInputs, splitPerPayee, etc.
  *   2. These values are passed in the stake validator's redeemer
  *   3. Stake validator verifies the claimed values match actual transaction
  *   4. Spending validator just checks the stake validator ran (withdraw zero trick)
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/design-patterns/tree/main/stake-validator]]
  * @see
  *   [[scalus.patterns.StakeValidator]]
  */
@Compile
object OptimizedPaymentSplitterValidator extends DataParameterizedValidator {

    /** Spending endpoint - minimal logic.
      *
      * Just verifies that the stake validator (reward endpoint) was executed. The actual validation
      * happens in the reward endpoint.
      *
      * Uses the input index from the redeemer to avoid iterating through all inputs to find own
      * input.
      */
    inline override def spend(
        payeesData: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val spendRedeemer = redeemer.to[SpendRedeemer]

        // Get own input by index - still O(n) traversal but avoids equality comparison at each step
        val ownInput = tx.inputs.at(spendRedeemer.ownInputIndex)

        // Verify the input at the given index matches ownRef
        require(ownInput.outRef === ownRef, "Own input index mismatch")

        // Get own script hash
        val ownScriptHash =
            ownInput.resolved.address.credential.scriptOption
                .getOrFail("Own address must be Script")

        // Just check that reward endpoint was triggered (withdraw zero trick)
        StakeValidator.spendMinimal(ownScriptHash, tx)
    }

    /** Reward endpoint - heavy computation.
      *
      * Verifies that the pre-computed values in the redeemer match the actual transaction. This
      * runs only ONCE per transaction, regardless of how many UTxOs are spent.
      */
    inline override def reward(
        payeesData: Data,
        redeemer: Data,
        stakingKey: Credential,
        tx: TxInfo
    ): Unit = {
        val payees = payeesData
            .to[List[ByteString]]
            .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))

        val verification = redeemer.to[SplitVerificationRedeemer]
        val ownScriptHash = stakingKey.scriptOption.getOrFail("Staking key must be Script")
        val ownScriptCredential = Credential.ScriptCredential(ownScriptHash)
        val payeeWithChangeCredential = Credential.PubKeyCredential(verification.payeeWithChange)

        // Verify sumContractInputs and find fee payer
        val (foundFeePayer, actualSumContractInputs) = tx.inputs
            .foldLeft((false, BigInt(0))) { case ((foundPayer, sum), input) =>
                val inputCredential = input.resolved.address.credential
                if payees.contains(inputCredential) then
                    if inputCredential === payeeWithChangeCredential then
                        require(!foundPayer, "Already found a fee payer")
                        (true, sum)
                    else fail("Payee input must be from payeeWithChange")
                else if inputCredential === ownScriptCredential then
                    (foundPayer, sum + input.resolved.value.getLovelace)
                else fail("Input not from the contract or fee payer")
            }

        require(foundFeePayer, "Fee payer not found in inputs")
        require(
          actualSumContractInputs === verification.sumContractInputs,
          "sumContractInputs mismatch"
        )

        // Verify outputs match the claimed split
        val (actualNPayed, sumPayeeOutputs, payeeWithChangeSum) =
            tx.outputs.foldLeft((BigInt(0), BigInt(0), BigInt(0))) {
                case ((n, sum, changeSum), output) =>
                    val outputCredential = output.address.credential
                    val value = output.value.getLovelace

                    outputCredential match
                        case Credential.PubKeyCredential(pkh) =>
                            require(payees.contains(outputCredential), "Output must be to a payee")
                            if pkh === verification.payeeWithChange then
                                (n + 1, sum + value, changeSum + value)
                            else
                                // Non-change payee must receive exactly splitPerPayee
                                require(
                                  value === verification.splitPerPayee,
                                  "Payee must receive exact split"
                                )
                                (n + 1, sum + value, changeSum)
                        case _ => fail("Output to script is not allowed")
            }

        require(actualNPayed === verification.nPayed, "nPayed mismatch")
        require(payees.length === actualNPayed, "Not all payees were paid")

        // Verify the split math:
        // sumContractInputs should be distributed as:
        // - (nPayed - 1) * splitPerPayee to non-change payees
        // - remainder to change payee (splitPerPayee + fee compensation)
        // Allow small remainder (< nPayed) for rounding
        val expectedMinOutput = verification.nPayed * verification.splitPerPayee
        val remainder = verification.sumContractInputs - expectedMinOutput
        require(
          remainder >= 0 && remainder < verification.nPayed,
          "value to be payed to payees is too low"
        )

        // Change payee should have received at least splitPerPayee
        require(
          payeeWithChangeSum >= verification.splitPerPayee,
          "Change payee received less than split"
        )
    }
}
