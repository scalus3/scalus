package scalus.examples.cape.twopartyescrow

import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.uplc.builtin.ByteString.*
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.Validator
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.{show as _, *}

// CAPE spec: datum is Constr(0, [state, depositTime])
// state: Constr(0, []) = Deposited, Constr(1, []) = Accepted, Constr(2, []) = Refunded
enum EscrowState derives FromData, ToData:
    case Deposited
    case Accepted
    case Refunded

case class EscrowDatum(state: EscrowState, depositTime: BigInt) derives FromData, ToData

/** UPLC-CAPE Two-Party Escrow Validator
  *
  * Parameters baked in (per CAPE spec):
  *   - buyerKeyHash: 64 a's
  *   - sellerKeyHash: 64 b's
  *   - escrowPrice: 75 ADA (75_000_000 lovelace)
  *   - deadlineSeconds: 1800
  *
  * Redeemer: integer 0=Deposit, 1=Accept, 2=Refund
  *
  * @see
  *   [[https://github.com/IntersectMBO/UPLC-CAPE]]
  */
@Compile
object TwoPartyEscrowValidator extends Validator {

    // CAPE parameters baked in as top-level inline defs so they are properly inlined
    // into the @Compile object
    private inline def buyerKeyHash: PubKeyHash =
        PubKeyHash(hex"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    private inline def sellerKeyHash: PubKeyHash =
        PubKeyHash(hex"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
    private inline def escrowPrice: Lovelace = BigInt(75_000_000)
    private inline def deadlineSeconds: BigInt = BigInt(1800)

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val action = redeemer.to[BigInt]
        val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
        val ownCredential = ownInput.address.credential

        if action == BigInt(0) then handleDeposit(txInfo, ownCredential)
        else if action == BigInt(1) then handleAccept(datum, txInfo, ownCredential)
        else if action == BigInt(2) then handleRefund(datum, txInfo, ownCredential)
        else fail("Invalid redeemer")
    }

    private inline def handleDeposit(
        txInfo: TxInfo,
        ownCredential: Credential
    ): Unit = {
        require(txInfo.isSignedBy(buyerKeyHash), "Buyer must sign deposit")

        val scriptOutputs = txInfo.findOwnOutputsByCredential(ownCredential)
        val outputAda = Utils.getAdaFromOutputs(scriptOutputs)

        require(outputAda === escrowPrice, "Output must be exactly escrow price")

        // Verify datum is set on script output
        require(scriptOutputs.length === BigInt(1), "Expected one script output")
        val scriptOutput = scriptOutputs.head
        scriptOutput.datum match {
            case OutputDatum.OutputDatum(inlineData) =>
                val escrowDatum = inlineData.to[EscrowDatum]
                escrowDatum.state match
                    case EscrowState.Deposited =>
                        // Verify deposit time matches tx valid range start
                        require(
                          escrowDatum.depositTime === txInfo.getValidityStartTime,
                          "Deposit time must match validity start"
                        )
                    case _ => fail("Datum state must be Deposited")
            case _ => fail("Expected inline datum on script output")
        }
    }

    private inline def handleAccept(
        datum: Option[Data],
        txInfo: TxInfo,
        ownCredential: Credential
    ): Unit = {
        // Parse datum and verify state is Deposited
        val receivedData = datum.getOrFail("Datum not found")
        val escrowDatum = receivedData.to[EscrowDatum]
        escrowDatum.state match
            case EscrowState.Deposited => ()
            case _                     => fail("Escrow must be in Deposited state")

        require(txInfo.isSignedBy(sellerKeyHash), "Seller must sign accept")

        // Verify seller receives exactly escrow price
        val sellerOutputs =
            txInfo.findOwnOutputsByCredential(Credential.PubKeyCredential(sellerKeyHash))
        val sellerAda = Utils.getAdaFromOutputs(sellerOutputs)
        require(sellerAda === escrowPrice, "Seller must receive exactly escrow price")

        // No funds should remain in the script
        val scriptOutputs = txInfo.findOwnOutputsByCredential(ownCredential)
        require(scriptOutputs.isEmpty, "No funds should remain in script")
    }

    private inline def handleRefund(
        datum: Option[Data],
        txInfo: TxInfo,
        ownCredential: Credential
    ): Unit = {
        // Parse datum and verify state is Deposited
        val receivedData = datum.getOrFail("Datum not found")
        val escrowDatum = receivedData.to[EscrowDatum]
        escrowDatum.state match
            case EscrowState.Deposited => ()
            case _                     => fail("Escrow must be in Deposited state")

        require(txInfo.isSignedBy(buyerKeyHash), "Buyer must sign refund")

        // Time check: valid range must be entirely after deadline
        val deadline = escrowDatum.depositTime + deadlineSeconds
        require(txInfo.validRange.isEntirelyAfter(deadline), "Deadline has not passed")

        // Verify buyer receives exactly escrow price
        val buyerOutputs =
            txInfo.findOwnOutputsByCredential(Credential.PubKeyCredential(buyerKeyHash))
        val buyerAda = Utils.getAdaFromOutputs(buyerOutputs)
        require(buyerAda === escrowPrice, "Buyer must receive exactly escrow price")

        // No funds should remain in the script
        val scriptOutputs = txInfo.findOwnOutputsByCredential(ownCredential)
        require(scriptOutputs.isEmpty, "No funds should remain in script")
    }
}
