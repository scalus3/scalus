package scalus.examples.escrow

import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.{show as _, *}

// Datum
case class Config(
    seller: PubKeyHash,
    buyer: PubKeyHash,
    escrowAmount: Lovelace,
    initializationAmount: Lovelace
) derives FromData,
      ToData

@Compile
object Config {
    given Eq[Config] = (x, y) =>
        x.buyer === y.buyer && x.seller === y.seller &&
            x.escrowAmount === y.escrowAmount && x.initializationAmount === y.initializationAmount
}

// Redeemer
enum Action derives FromData, ToData:
    case Deposit
    case Pay
    case Refund

@Compile
object Action

/** Secure exchange of assets between two parties
  *
  * The escrow smart contract allows two parties to exchange assets securely. The contract holds the
  * assets until both parties agree and sign off on the transaction.
  *
  * @see
  *   [[https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/escrow]]
  *   [[https://meshjs.dev/smart-contracts/escrow]]
  *   [[https://github.com/cardano-foundation/cardano-template-and-ecosystem-monitoring/tree/main/escrow]]
  */
@Compile
object EscrowValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val receivedData = datum.getOrFail("Datum not found")
        val escrowDatum: Config = receivedData.to[Config]
        val action = redeemer.to[Action]
        val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
        val contractAddress = ownInput.address
        val contractInputs = txInfo.findOwnInputsByCredential(contractAddress.credential)
        val contractBalance = Utils.getAdaFromInputs(contractInputs)

        action match {
            case Action.Deposit =>
                handleDeposit(escrowDatum, txInfo, contractAddress, contractBalance, receivedData)
            case Action.Pay =>
                handlePay(escrowDatum, txInfo, contractBalance)
            case Action.Refund =>
                handleRefund(escrowDatum, txInfo, contractBalance)
        }
    }

    private def handleDeposit(
        escrowDatum: Config,
        txInfo: TxInfo,
        contractAddress: Address,
        contractBalance: Lovelace,
        receivedData: Data
    ): Unit = {
        require(
          txInfo.isSignedBy(escrowDatum.buyer),
          "Buyer must sign deposit transaction"
        )

        val buyerOutputs =
            txInfo.findOwnOutputsByCredential(Credential.PubKeyCredential(escrowDatum.buyer))
        val contractOutputs = txInfo.findOwnOutputsByCredential(contractAddress.credential)

        require(contractOutputs.length === BigInt(1), "Expected exactly one contract output")
        val contractOutput = contractOutputs.head

        require(buyerOutputs.length === BigInt(1), "Expected exactly one buyer output")

        require(
          contractBalance != escrowDatum.escrowAmount,
          "Contract must contain only initialization amount before deposit"
        )

        require(
          Utils.getAdaFromOutputs(
            contractOutputs
          ) === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract output must contain exactly escrow amount plus initialization amount"
        )

        contractOutput.datum match {
            case OutputDatum.OutputDatum(inlineData) =>
                require(
                  inlineData === receivedData,
                  "EscrowDatum must be preserved"
                )
            case _ => fail("Expected inline datum")
        }
    }

    private def handlePay(
        escrowDatum: Config,
        txInfo: TxInfo,
        contractBalance: Lovelace
    ): Unit = {
        require(
          contractBalance === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract must be fully funded before payment"
        )

        val buyerOutputs =
            txInfo.findOwnOutputsByCredential(Credential.PubKeyCredential(escrowDatum.buyer))
        val sellerOutputs =
            txInfo.findOwnOutputsByCredential(Credential.PubKeyCredential(escrowDatum.seller))

        require(
          sellerOutputs.nonEmpty,
          "Seller outputs must not be empty"
        )

        require(
          buyerOutputs.nonEmpty,
          "Buyer outputs must not be empty"
        )

        require(
          txInfo.isSignedBy(escrowDatum.buyer),
          "Only buyer can release payment"
        )

        require(
          Utils.getAdaFromOutputs(
            sellerOutputs
          ) === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Seller must receive exactly escrow amount plus initialization amount"
        )
    }

    private def handleRefund(
        escrowDatum: Config,
        txInfo: TxInfo,
        contractBalance: Lovelace
    ): Unit = {
        require(
          contractBalance === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract must be fully funded before refund"
        )

        val buyerOutputs =
            txInfo.findOwnOutputsByCredential(Credential.PubKeyCredential(escrowDatum.buyer))
        val sellerOutputs =
            txInfo.findOwnOutputsByCredential(Credential.PubKeyCredential(escrowDatum.seller))

        require(
          sellerOutputs.nonEmpty,
          "Seller outputs must not be empty"
        )

        require(
          buyerOutputs.nonEmpty,
          "Buyer outputs must not be empty"
        )

        require(
          txInfo.isSignedBy(escrowDatum.seller),
          "Only seller can issue refund"
        )

        require(
          Utils.getAdaFromOutputs(buyerOutputs) === escrowDatum.escrowAmount,
          "Buyer must receive exactly the escrow amount back"
        )
    }
}
