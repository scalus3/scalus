package scalus.examples

import scalus.*
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.compiler.Options
import scalus.ledger.api.v1.Value.*
import scalus.ledger.api.v1.{Credential, PubKeyHash}
import scalus.ledger.api.v3.*
import scalus.patterns.StakeValidator
import scalus.prelude.*
import scalus.prelude.List.*
import scalus.prelude.Option.*
import scalus.uplc.PlutusV3

/** Merkelized Payment Splitter - demonstrates the Merkelized Validators pattern.
  *
  * This is an optimized version of [[scalus.examples.paymentsplitter.PaymentSplitterValidator]]
  * that uses the "merkelized validators" pattern to reduce execution costs when spending multiple
  * UTxOs.
  *
  * '''The Problem:''' In the original PaymentSplitter, each UTxO spend executes the full validation
  * logic - iterating through ALL inputs and outputs. When spending N UTxOs, this results in O(N²)
  * iteration cost.
  *
  * '''The Solution:''' Split the logic:
  *   - '''Stake validator (reward endpoint):''' Runs ONCE, does all heavy computation
  *   - '''Spending validator:''' Runs per UTxO, minimal - just checks stake validator executed
  *
  * '''How it works:'''
  *   1. Off-chain code computes: sumContractInputs, splitPerPayee, etc.
  *   2. These values are passed in the stake validator's redeemer
  *   3. Stake validator verifies the claimed values match actual transaction
  *   4. Spending validator just checks the stake validator ran
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/design-patterns/blob/main/merkelized-validators/merkelized-validators.md]]
  */

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

/** Merkelized Payment Splitter Validator
  *
  * A single validator that serves both spending and reward purposes:
  *   - '''spend:''' Minimal logic - delegates to stake validator
  *   - '''reward:''' Heavy computation - verifies the split is correct
  *
  * @param payeesData
  *   List of payee PubKeyHashes (as Data for parameterization)
  */
@Compile
object MerkelizedPaymentSplitterValidator extends DataParameterizedValidator {

    /** Spending endpoint - minimal logic.
      *
      * Just verifies that the stake validator (reward endpoint) was executed. The actual validation
      * happens in the reward endpoint.
      */
    inline override def spend(
        payeesData: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        // Get own script hash from the input being spent
        val ownCredential = tx.findOwnInputOrFail(ownRef).resolved.address.credential
        val ownScriptHash = ownCredential.scriptOption.getOrFail("Own address must be Script")

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
        require(remainder >= 0 && remainder < verification.nPayed, "Split calculation invalid")

        // Change payee should have received at least splitPerPayee
        require(
          payeeWithChangeSum >= verification.splitPerPayee,
          "Change payee received less than split"
        )
    }
}

private given merkelizedPaymentSplitterOptions: Options = Options.release

lazy val MerkelizedPaymentSplitterContract =
    PlutusV3.compile(MerkelizedPaymentSplitterValidator.validate)
