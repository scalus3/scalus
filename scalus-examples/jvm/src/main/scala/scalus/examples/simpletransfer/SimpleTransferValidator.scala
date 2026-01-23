package scalus.examples.simpletransfer

import scalus.Compile
import scalus.uplc.builtin.{Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

// Datum
case class Parties(
    owner: PubKeyHash,
    recipient: PubKeyHash
) derives ToData,
      FromData

@Compile
object Parties

// Redeemer
enum Action derives ToData, FromData:
    case Deposit(amount: Value)
    case Withdraw(amount: Value)

@Compile
object Action

/** https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/simple_transfer
  *
  * Simple transfer
  *
  * The contract allows a user (the owner) to deposit native cryptocurrency, and another user (the
  * recipient) to withdraw arbitrary fractions of the contract balance.
  *
  * At contract creation, the owner specifies the receiver's address.
  *
  * After contract creation, the contract supports two actions:
  *
  *   - deposit allows the owner to deposit an arbitrary amount of native cryptocurrency in the
  *     contract;
  *   - withdraw allows the receiver to withdraw any amount of the cryptocurrency deposited in the
  *     contract.
  */
@Compile
object SimpleTransferValidator extends Validator {

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val Parties(owner, recipient) = datum.get.to[Parties]
        val contract = tx.findOwnInputOrFail(ownRef).resolved
        val contractAddress = contract.address.credential
        val contractInputs = tx.findOwnInputsByCredential(contractAddress)
        val contractOutputs = tx.findOwnOutputsByCredential(contractAddress)
        val balance = contract.value

        // eliminate double satisfaction by ensuring exactly one contract own input and at most one own output
        require(contractInputs.size === BigInt(1), "Contract should have exactly one own input")
        require(
          contractOutputs.size <= BigInt(1),
          "Contract should have at most one own output"
        )

        redeemer.to[Action] match
            case Action.Deposit(amount) =>
                require(tx.isSignedBy(owner), "Deposit must be signed by owner")
                require(amount.isPositive, "Negative amount")
                // eliminate double satisfaction by ensuring exactly one contract own input and one own output
                require(
                  contractOutputs.size === BigInt(1),
                  "Contract should have exactly one own output"
                )
                val contractOutput = contractOutputs.head
                require(
                  contractOutput.value === balance + amount,
                  "Contract has received incorrect amount"
                )
                val expectedDatum = OutputDatum.OutputDatum(datum.get)
                require(contractOutput.datum === expectedDatum, "Output datum changed")
            case Action.Withdraw(withdraw) =>
                require(tx.isSignedBy(recipient), "Withdraw must be signed by recipient")
                require(withdraw.isPositive, "Negative amount")
                if withdraw === balance then
                    // if withdrawing all, there should be no contract output
                    require(contractOutputs.isEmpty, "Contract own output is not empty")
                else if (balance - withdraw).isPositive then
                    // eliminate double satisfaction by ensuring exactly one contract own input and one own output
                    require(
                      contractOutputs.size === BigInt(1),
                      "Contract should have exactly one own output"
                    )
                    val contractOutput = contractOutputs.head
                    require(
                      contractOutput.value === balance - withdraw,
                      "Contract balance is incorrect"
                    )
                    val expectedDatum = OutputDatum.OutputDatum(datum.get)
                    require(contractOutput.datum === expectedDatum, "Output datum changed")
                else fail("Withdraw exceeds balance")
    }
}
