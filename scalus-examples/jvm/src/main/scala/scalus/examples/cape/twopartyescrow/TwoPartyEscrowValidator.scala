package scalus.examples.cape.twopartyescrow

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.*

// CAPE spec: datum is Constr(0, [state, depositTime])
// state: Constr(0, []) = Deposited, Constr(1, []) = Accepted, Constr(2, []) = Refunded
enum EscrowState derives FromData, ToData:
    case Deposited
    case Accepted
    case Refunded

case class EscrowDatum(state: EscrowState, depositTime: BigInt) derives FromData, ToData

/** UPLC-CAPE Two-Party Escrow validator: deposit, then accept or refund.
  *
  * Parameters baked in (per CAPE spec): buyer key hash (64 a's), seller key hash (64 b's), price 75
  * ADA, refund deadline 1800 seconds after the deposit. The redeemer is a bare integer: 0 =
  * Deposit, 1 = Accept, 2 = Refund; anything else fails.
  *
  *   - `Deposit` creates the escrow UTxO, so there is no own input to match against: the escrow
  *     output is the unique output at a script credential, it must carry exactly the price and an
  *     inline `EscrowDatum(Deposited, depositTime)`, where `depositTime` is the upper bound of the
  *     transaction's validity range (finite, `t - 1` when exclusive, per the CAPE convention).
  *   - `Accept` and `Refund` spend the escrow UTxO: the datum must be `Deposited`, the party must
  *     sign, the party must be paid exactly the price (across any number of outputs, datum or not),
  *     and nothing may stay at the script's own credential. `Refund` additionally requires the
  *     validity range to lie entirely after `depositTime + 1800`, which also rejects an unbounded
  *     lower bound.
  *
  * Spending several escrow UTxOs in one transaction is allowed by the CAPE fixtures
  * (`accept_with_multiple_inputs`), so there is deliberately no single-own-input guard here.
  *
  * All context plumbing is the standard prelude: `Validator`, `TxInfo.isSignedBy`,
  * `TxInfo.validToOrFail`, `Interval.isEntirelyAfter`, `List.findUniqueOrFail`,
  * `TxOut.hasInlineDatum`, `TxInfo.findInputOrFail` and `TxInfo.findOutputsByCredential`. An
  * earlier revision hand-navigated the raw `ScriptContext` `Data` and hand-rolled local copies of
  * all of these; those are dropped in favor of the canonical forms, since library and compiler
  * fixes are the intended remedy for a cost gap, not validator-level workarounds.
  *
  * @see
  *   [[https://github.com/IntersectMBO/UPLC-CAPE]]
  */
@Compile
object TwoPartyEscrowValidator extends Validator {

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
        if action == BigInt(0) then deposit(txInfo)
        else if action == BigInt(1) then accept(datum, txInfo, txOutRef)
        else if action == BigInt(2) then refund(datum, txInfo, txOutRef)
        else fail(InvalidRedeemer)
    }

    def deposit(txInfo: TxInfo): Unit = {
        require(txInfo.isSignedBy(buyerKeyHash), BuyerMustSignDeposit)
        // The deposit time is the validity range's upper bound. The ledger makes a finite upper
        // bound exclusive, and the CAPE convention records an exclusive bound `t` as `t - 1`.
        val depositTime =
            txInfo.validToOrFail(UpperBoundMustBeFinite) -
                (if txInfo.validRange.to.isInclusive then BigInt(0) else BigInt(1))
        // No own input exists yet (the escrow UTxO is being created), so the escrow output is the
        // unique output locked by a script credential.
        val output = txInfo.outputs.findUniqueOrFail(
          out => isScript(out.address.credential),
          ExpectedOneScriptOutput
        )
        require(output.value === Value.lovelace(escrowPrice), OutputMustHoldEscrowPrice)
        require(
          output.hasInlineDatum(EscrowDatum(EscrowState.Deposited, depositTime)),
          WrongDepositDatum
        )
        require(output.referenceScript.isEmpty, NoReferenceScript)
    }

    def accept(datum: Option[Data], txInfo: TxInfo, txOutRef: TxOutRef): Unit = {
        requireDeposited(datum)
        require(txInfo.isSignedBy(sellerKeyHash), SellerMustSignAccept)
        settle(txInfo, txOutRef, sellerKeyHash, SellerMustReceiveEscrowPrice)
    }

    def refund(datum: Option[Data], txInfo: TxInfo, txOutRef: TxOutRef): Unit = {
        val d = requireDeposited(datum)
        require(txInfo.isSignedBy(buyerKeyHash), BuyerMustSignRefund)
        require(
          txInfo.validRange.isEntirelyAfter(d.depositTime + deadlineSeconds),
          DeadlineNotPassed
        )
        settle(txInfo, txOutRef, buyerKeyHash, BuyerMustReceiveEscrowPrice)
    }

    /** Decodes the datum and requires the escrow to be in the `Deposited` state. */
    def requireDeposited(datum: Option[Data]): EscrowDatum = {
        val d = datum.getOrFail(DatumNotFound).to[EscrowDatum]
        d.state match
            case EscrowState.Deposited => ()
            case _                     => fail(NotDeposited)
        d
    }

    /** The party is paid exactly the escrow price, across any number of outputs, and nothing stays
      * at the script's own credential - both checked in one pass over the outputs. The party is a
      * key, so its outputs are matched by payment credential rather than by a full address.
      */
    def settle(txInfo: TxInfo, txOutRef: TxOutRef, party: PubKeyHash, message: String): Unit = {
        val ownCredential = txInfo.findInputOrFail(txOutRef).resolved.address.credential
        val partyCredential = Credential.PubKeyCredential(party)
        val paid = txInfo.outputs.foldLeft(BigInt(0)) { (sum, out) =>
            val credential = out.address.credential
            if credential === ownCredential then fail(FundsRemainInScript)
            else if credential === partyCredential then sum + out.value.getLovelace
            else sum
        }
        require(paid === escrowPrice, message)
    }

    def isScript(credential: Credential): Boolean = credential match
        case Credential.ScriptCredential(_) => true
        case _                              => false

    // Error messages
    inline val InvalidRedeemer = "Invalid redeemer"
    inline val DatumNotFound = "Datum not found"
    inline val NotDeposited = "Escrow must be in Deposited state"
    inline val BuyerMustSignDeposit = "Buyer must sign deposit"
    inline val UpperBoundMustBeFinite = "Valid range upper bound must be finite"
    inline val ExpectedOneScriptOutput = "Expected exactly one script output"
    inline val OutputMustHoldEscrowPrice = "Output must contain exactly the escrow price"
    inline val WrongDepositDatum = "Output must have the expected deposit datum"
    inline val NoReferenceScript = "Output must not carry a reference script"
    inline val SellerMustSignAccept = "Seller must sign accept"
    inline val SellerMustReceiveEscrowPrice = "Seller must receive exactly escrow price"
    inline val BuyerMustSignRefund = "Buyer must sign refund"
    inline val DeadlineNotPassed = "Deadline has not passed"
    inline val BuyerMustReceiveEscrowPrice = "Buyer must receive exactly escrow price"
    inline val FundsRemainInScript = "No funds should remain in script"
}
