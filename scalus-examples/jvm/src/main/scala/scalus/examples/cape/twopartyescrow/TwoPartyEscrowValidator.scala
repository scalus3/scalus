package scalus.examples.cape.twopartyescrow

import scalus.cardano.onchain
import scalus.cardano.onchain.plutus
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
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
object TwoPartyEscrowValidator {

    // CAPE parameters baked in as top-level inline defs so they are properly inlined
    // into the @Compile object
    private inline def buyerKeyHash: PubKeyHash =
        PubKeyHash(hex"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    private inline def sellerKeyHash: PubKeyHash =
        PubKeyHash(hex"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
    private inline def escrowPrice: Lovelace = BigInt(75_000_000)
    private inline def deadlineSeconds: BigInt = BigInt(1800)

    inline def validate(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match {
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(datum, sc.redeemer, sc.txInfo, txOutRef)
            case _ => fail("Only spending scripts are supported by this validator")
        }
    }

    inline def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val action = redeemer.to[BigInt]
        val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
        val ownAddress = ownInput.address

        if action == BigInt(0) then handleDeposit(txInfo, ownAddress)
        else if action == BigInt(1) then handleAccept(datum, txInfo, ownAddress)
        else if action == BigInt(2) then handleRefund(datum, txInfo, ownAddress)
        else fail("Invalid redeemer")
    }

    inline def handleDeposit(
        txInfo: TxInfo,
        ownAddress: Address
    ): Unit = {
        require(txInfo.isSignedBy(buyerKeyHash), "Buyer must sign deposit")

        val expectedDatum = EscrowDatum(
          state = EscrowState.Deposited,
          depositTime = txInfo.getValidityStartTime
        ).toData

        val expectedOutput = TxOut(
          address = ownAddress,
          value = Value.lovelace(escrowPrice),
          datum = OutputDatum.OutputDatum(expectedDatum),
          referenceScript = Option.None
        )

        val output = txInfo.outputs.filter(out =>
            out.address.credential.toData == ownAddress.credential.toData
        ) match
            case List.Cons(head, List.Nil) => head
            case _                         => fail("Expected exactly one script output")
        require(output.toData == expectedOutput.toData, "Output must match expected deposit output")
    }

    inline def handleAccept(
        datum: Option[Data],
        txInfo: TxInfo,
        ownAddress: Address
    ): Unit = {
        // Parse datum and verify state is Deposited
        val receivedData = datum.getOrFail("Datum not found")
        val escrowDatum = receivedData.to[EscrowDatum]
        val ownCredential = ownAddress.credential
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

    inline def handleRefund(
        datum: Option[Data],
        txInfo: TxInfo,
        ownAddress: Address
    ): Unit = {
        val ownCredential = ownAddress.credential
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
