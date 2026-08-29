package scalus.examples.escrow

import scalus.compiler.Compile

import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.Validator
import scalus.cardano.onchain.plutus.prelude.Option.*

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
    given Eq[Config] = Eq.derived
}

// Redeemer
enum Action derives FromData, ToData:
    case Deposit
    case Pay
    case Refund

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
        val ownInput = txInfo.findInputOrFail(txOutRef)
        val contractAddress = ownInput.resolved.address
        // Exactly one escrow may be spent per transaction: the payout checks below sum the
        // buyer's and seller's outputs by credential, so two escrows spent together could be
        // settled by one payout (double satisfaction).
        txInfo.inputs.findUniqueOrFail(
          _.resolved.address.credential === contractAddress.credential,
          "Exactly one escrow input may be spent"
        )
        // Lovelace by design: the escrow holds ADA only. Summed by the full own address.
        val contractBalance = txInfo.valueSpentFrom(contractAddress).getLovelace

        action match {
            case Action.Deposit =>
                handleDeposit(escrowDatum, txInfo, ownInput, contractBalance, receivedData)
            case Action.Pay =>
                handlePay(escrowDatum, txInfo, contractBalance)
            case Action.Refund =>
                handleRefund(escrowDatum, txInfo, contractBalance)
        }
    }

    private inline def handleDeposit(
        escrowDatum: Config,
        txInfo: TxInfo,
        ownInput: TxInInfo,
        contractBalance: Lovelace,
        receivedData: Data
    ): Unit = {
        require(
          txInfo.isSignedBy(escrowDatum.buyer),
          "Buyer must sign deposit transaction"
        )

        val contractAddress = ownInput.resolved.address
        // Unique output to the WHOLE own address, staking part included.
        val contractOutput =
            txInfo.findContinuingOutputOrFail(ownInput, "Expected exactly one contract output")

        val buyerCredential = Credential.PubKeyCredential(escrowDatum.buyer)
        require(
          txInfo.outputs.count(_.address.credential === buyerCredential) === BigInt(1),
          "Expected exactly one buyer output"
        )

        require(
          contractBalance === escrowDatum.initializationAmount,
          "Contract must contain only initialization amount before deposit"
        )

        // Whole-value check: the continuing output carries exactly the escrow amount plus the
        // initialization amount, and nothing else. Summing lovelace alone would let native
        // tokens be stripped from (or dust added to) the escrow UTxO.
        require(
          txInfo.valuePaidTo(contractAddress) ===
              Value.lovelace(escrowDatum.escrowAmount + escrowDatum.initializationAmount),
          "Contract output must contain exactly escrow amount plus initialization amount"
        )

        require(contractOutput.hasInlineDatum(receivedData), "EscrowDatum must be preserved")
    }

    private inline def handlePay(
        escrowDatum: Config,
        txInfo: TxInfo,
        contractBalance: Lovelace
    ): Unit = {
        require(
          contractBalance === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract must be fully funded before payment"
        )

        val buyerOutputs =
            txInfo.findOutputsByCredential(Credential.PubKeyCredential(escrowDatum.buyer))
        val sellerOutputs =
            txInfo.findOutputsByCredential(Credential.PubKeyCredential(escrowDatum.seller))

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

        // The seller is a key, paid in lovelace by design.
        require(
          sellerOutputs.foldLeft(Value.zero)(_ + _.value).getLovelace ===
              escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Seller must receive exactly escrow amount plus initialization amount"
        )
    }

    private inline def handleRefund(
        escrowDatum: Config,
        txInfo: TxInfo,
        contractBalance: Lovelace
    ): Unit = {
        require(
          contractBalance === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
          "Contract must be fully funded before refund"
        )

        val buyerOutputs =
            txInfo.findOutputsByCredential(Credential.PubKeyCredential(escrowDatum.buyer))
        val sellerOutputs =
            txInfo.findOutputsByCredential(Credential.PubKeyCredential(escrowDatum.seller))

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

        // The buyer is a key, paid in lovelace by design.
        require(
          buyerOutputs.foldLeft(Value.zero)(_ + _.value).getLovelace === escrowDatum.escrowAmount,
          "Buyer must receive exactly the escrow amount back"
        )
    }
}
