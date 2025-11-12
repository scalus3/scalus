package scalus.examples.escrow

import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.{show as _, *}

// Datum
case class EscrowDatum(
    seller: PubKeyHash,
    buyer: PubKeyHash,
    escrowAmount: Lovelace,
    initializationAmount: Lovelace
) derives FromData,
      ToData

@Compile
object EscrowDatum {
    given Eq[EscrowDatum] = (x, y) =>
        x.buyer === y.buyer && x.seller === y.seller &&
            x.escrowAmount === y.escrowAmount && x.initializationAmount === y.initializationAmount
}

// Redeemer
enum EscrowAction derives FromData, ToData:
    case Deposit
    case Pay
    case Refund

@Compile
object EscrowAction

@Compile
object EscrowValidator extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val receivedData = datum.getOrFail("Datum not found")
        val escrowDatum: EscrowDatum = receivedData.to[EscrowDatum]
        val action = redeemer.to[EscrowAction]

        val ownInput = txInfo.inputs
            .find(input => input.outRef === txOutRef)
            .get
            .resolved
        val contractAddress = ownInput.address
        val contractInputs = getInputsByAddress(txInfo.inputs, contractAddress)
        val contractBalance = getAdaFromInputs(contractInputs)

        action match {
            case EscrowAction.Deposit =>
                handleDeposit(escrowDatum, txInfo, contractAddress, contractBalance, receivedData)
            case EscrowAction.Pay =>
                handlePay(escrowDatum, txInfo, contractBalance)
            case EscrowAction.Refund =>
                handleRefund(escrowDatum, txInfo, contractBalance)
        }
    }

    private def handleDeposit(
        escrowDatum: EscrowDatum,
        txInfo: TxInfo,
        contractAddress: Address,
        contractBalance: Lovelace,
        receivedData: Data
    ): Unit = {
        require(
          mustBeSignedBy(txInfo.signatories, escrowDatum.buyer),
          "Buyer must sign deposit transaction"
        )

        val buyerOutputs = getOutputsByVkh(txInfo.outputs, escrowDatum.buyer)
        val contractOutputs = getOutputsByAddress(txInfo.outputs, contractAddress)

        require(contractOutputs.length === BigInt(1), "Expected exactly one contract output")
        val contractOutput = contractOutputs.head

        require(buyerOutputs.length === BigInt(1), "Expected exactly one buyer output")

        require(
          contractBalance != escrowDatum.escrowAmount,
          "Contract must contain only initialization amount before deposit"
        )

        require(
          getAdaFromOutputs(
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
        escrowDatum: EscrowDatum,
        txInfo: TxInfo,
        contractBalance: Lovelace
    ): Unit = {
        require(
          contractBalance === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract must be fully funded before payment"
        )

        val buyerOutputs = getOutputsByVkh(txInfo.outputs, escrowDatum.buyer)
        val sellerOutputs = getOutputsByVkh(txInfo.outputs, escrowDatum.seller)

        require(
          sellerOutputs.nonEmpty,
          "Seller outputs must not be empty"
        )

        require(
          buyerOutputs.nonEmpty,
          "Buyer outputs must not be empty"
        )

        require(
          mustBeSignedBy(txInfo.signatories, escrowDatum.buyer),
          "Only buyer can release payment"
        )

        require(
          getAdaFromOutputs(
            sellerOutputs
          ) === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Seller must receive exactly escrow amount plus initialization amount"
        )
    }

    private def handleRefund(
        escrowDatum: EscrowDatum,
        txInfo: TxInfo,
        contractBalance: Lovelace
    ): Unit = {
        require(
          contractBalance === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract must be fully funded before refund"
        )

        val buyerOutputs = getOutputsByVkh(txInfo.outputs, escrowDatum.buyer)
        val sellerOutputs = getOutputsByVkh(txInfo.outputs, escrowDatum.seller)

        require(
          sellerOutputs.nonEmpty,
          "Seller outputs must not be empty"
        )

        require(
          buyerOutputs.nonEmpty,
          "Buyer outputs must not be empty"
        )

        require(
          mustBeSignedBy(txInfo.signatories, escrowDatum.seller),
          "Only seller can issue refund"
        )

        require(
          getAdaFromOutputs(buyerOutputs) === escrowDatum.escrowAmount,
          "Buyer must receive exactly the escrow amount back"
        )
    }

    // Helper functions

    private def getOutputsByVkh(outputs: List[TxOut], vkh: PubKeyHash): List[TxOut] = {
        outputs.filter(output =>
            output.address.credential match {
                case Credential.PubKeyCredential(pkh) => pkh === vkh
                case _                                => false
            }
        )
    }

    private def getOutputsByAddress(outputs: List[TxOut], addr: Address): List[TxOut] = {
        outputs.filter(_.address === addr)
    }

    private def getInputsByAddress(inputs: List[TxInInfo], addr: Address): List[TxInInfo] = {
        inputs.filter(_.resolved.address === addr)
    }

    private def getAdaFromOutputs(outputs: List[TxOut]): Lovelace = {
        outputs.map(_.value.getLovelace).foldLeft(BigInt(0))(_ + _)
    }

    private def getAdaFromInputs(inputs: List[TxInInfo]): Lovelace = {
        inputs.map(_.resolved.value.getLovelace).foldLeft(BigInt(0))(_ + _)
    }

    private def mustBeSignedBy(signatories: List[PubKeyHash], vkh: PubKeyHash): Boolean = {
        signatories.contains(vkh)
    }
}
