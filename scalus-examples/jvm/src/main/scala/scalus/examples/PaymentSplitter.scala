package scalus.examples

import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.cardano.blueprint.{Application, Blueprint}
import scalus.ledger.api.v1
import scalus.ledger.api.v1.Value.*
import scalus.ledger.api.v1.{Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.List.*
import scalus.prelude.Option.*
import scalus.prelude.*
import scalus.prelude.AssocMap

/** Split payouts equally among a list of specified payees
  *
  * A payment splitter can be used for example to create a shared project donation address, ensuring
  * that all payees receive the same amount
  *
  * Sending lovelace to the contract works similarly to sending lovelace to any other address. The
  * payout transaction can only be submitted by one of the payees, and the output addresses are
  * restricted to the payees. The output sum must be equally divided to ensure the transaction is
  * successful.
  *
  * @see
  *   [[https://meshjs.dev/smart-contracts/payment-splitter]]
  */
@Compile
object PaymentSplitter extends DataParameterizedValidator {

    inline override def spend(
        payeesData: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {

        val payees = payeesData
            .to[List[ByteString]]
            .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))

        val myTxInputCredential =
            tx.inputs.find(_.outRef === ownRef).get.resolved.address.credential

        // Find the first and single payee that triggers the payout and pays the fee
        //  and calculate the sum of contract inputs
        val (optPayeeInputWithChange, sumContractInputs) = tx.inputs
            .foldLeft(Option.empty[TxOut], BigInt(0)) {
                case ((optTxOut, sumContractInputs), input) =>
                    if payees.contains(input.resolved.address.credential)
                    then
                        if optTxOut.isEmpty then (Some(input.resolved), sumContractInputs)
                        else fail("Already found a fee payer")
                    else if input.resolved.address.credential === myTxInputCredential then
                        (optTxOut, sumContractInputs + input.resolved.value.getLovelace)
                    else
                        // TODO: think
                        fail("Input not from the contract or payer")
            }

        val payeeInputWithChange = optPayeeInputWithChange.getOrFail(
          "One of the payees must have an input to pay the fee and trigger the payout"
        )

        val (sumOutput, sumsPerPayee) =
            tx.outputs.foldLeft(
              (BigInt(0), AssocMap.empty[Credential.PubKeyCredential, BigInt])
            ) { case (state, output) =>
                val (sum, sumsPerPayee) = state
                val value = output.value.getLovelace
                val payee: Credential.PubKeyCredential = output.address.credential match
                    case Credential.PubKeyCredential(pkh) => Credential.PubKeyCredential(pkh)
                    case _                                => fail("Output to script is not allowed")
                sumsPerPayee.get(payee) match
                    case None => (sum + value, sumsPerPayee.insert(payee, value))
                    case Some(prevSum) =>
                        (sum + value, sumsPerPayee.insert(payee, prevSum + value))
            }

        val (optSplit, optPayeeSumWithChange, nPayed) =
            sumsPerPayee.toList.foldLeft(
              (Option.empty[BigInt], Option.empty[BigInt], BigInt(0))
            ) { case ((optSplit, optPayeeSumWithChange, nPayed), (payee, value)) =>
                require(payees.contains(payee), "Must pay to a payee")
                if payeeInputWithChange.address.credential === payee
                then (optSplit, Some(value), nPayed + 1)
                else
                    optSplit match
                        case None => (Some(value), optPayeeSumWithChange, nPayed + 1)
                        case Some(split) =>
                            require(split === value, "Split unequally")
                            (Some(split), optPayeeSumWithChange, nPayed + 1)
            }

        require(payees.length === nPayed, "Not all payees were paid")
        optSplit match
            case None => // one payee, no split
            case Some(split) =>
                val payeeSumWithChange = optPayeeSumWithChange.getOrFail("No change output")
                val eqSumValue = sumOutput - payeeSumWithChange + split
                val reminder = sumContractInputs - eqSumValue
                require(reminder < nPayed, "value to be payed to payees is too low")
                //    nOutputs * (split + 1) > sumContractInputs   <=>
                //    nOutputs * split + nOutputs > sumContractInputs <=>
                //    eqSumValue + nOutputs > sumContractInputs <=>
                //    nOutputs > reminder ( = sumContractInputs - eqSumValue)
                //
                // max number of payers ≈ 250 (16kB / 28 bytes / 2 (inputs and outputs))
                // thus, up to 250 lovelace of reminder is possible, so we can ignore it

    }
}
object PaymentSplitterContract {

    def application: Application = {
        Application.ofSingleValidator[List[ByteString], Unit](
          "Payment splitter",
          "Allows to split payouts equally among a list of specified payees",
          "1.0.0",
          PaymentSplitter.validate
        )
    }

    def blueprint: Blueprint = application.blueprint
}
