package scalus.examples.cape.twopartyescrow

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.*

/** CAPE escrow datum: `Constr 0 [I depositTime]`, the last millisecond at which the deposit
  * transaction was valid. There is no state field: the escrow UTxO exists while the deposit is
  * open, and Accept or Refund consumes it.
  */
case class EscrowDatum(depositTime: BigInt) derives FromData, ToData

/** UPLC-CAPE Two-Party Escrow, revision 2: one script with a minting and a spending purpose.
  *
  * As a minting policy it creates the thread token `(H, escrow)` at the deposit and destroys it at
  * the end. As a spending validator it guards the 75 ADA in Accept and Refund. `H` is the hash of
  * this script, read from the context: the policy id when minting, the script hash of the spent
  * input when spending. The redeemer is a bare integer.
  *
  * | Purpose  | Redeemer | Action  | Rules              |
  * |:---------|:---------|:--------|:-------------------|
  * | Minting  | 0        | Deposit | D1 to D7           |
  * | Minting  | 1 or 2   | Burn    | B1                 |
  * | Spending | 1        | Accept  | S1 to S3, A1, A2   |
  * | Spending | 2        | Refund  | S1 to S3, R1 to R3 |
  *
  * Every other purpose and redeemer fails (X1, X2). Rule IDs refer to
  * `scenarios/two_party_escrow/two_party_escrow.md` in UPLC-CAPE.
  *
  * @see
  *   [[https://github.com/IntersectMBO/UPLC-CAPE]]
  */
@Compile
object TwoPartyEscrowValidator extends Validator {

    private inline def buyer: PubKeyHash =
        PubKeyHash(hex"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    private inline def seller: PubKeyHash =
        PubKeyHash(hex"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
    private inline def price: Lovelace = BigInt(75_000_000)
    private inline def refundDelay: PosixTime = BigInt(1_800_000)
    private inline def threadToken: TokenName = hex"657363726f77" // "escrow"

    inline override def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit = {
        val action = redeemer.to[BigInt]
        if action === BigInt(0) then deposit(policyId, tx)
        else if action === BigInt(1) || action === BigInt(2) then
            // B1: the burn is checked in full here; Accept and Refund run in the same transaction.
            require(tx.mint.hasOnly(policyId, threadToken, -1), MustBurnOneToken)
        else fail(InvalidRedeemer)
    }

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val action = redeemer.to[BigInt]
        if action === BigInt(1) then settle(tx, ownRef, seller) // A1, A2
        else if action === BigInt(2) then
            val depositTime = datum.getOrFail(DatumNotFound).to[EscrowDatum].depositTime
            // R2: strictly after the deadline; the ledger makes a finite lower bound inclusive.
            require(
              tx.validFromOrFail(LowerBoundMustBeFinite) > depositTime + refundDelay,
              TooEarly
            )
            settle(tx, ownRef, buyer) // R1, R3
        else fail(InvalidRedeemer)
    }

    /** Deposit, rules D1 to D7. */
    def deposit(policyId: PolicyId, tx: TxInfo): Unit = {
        require(tx.isSignedBy(buyer), BuyerMustSign) // D1
        // D2: the last instant of the validity range; the ledger makes a finite upper bound `t`
        // exclusive, so that instant is `t - 1`.
        val depositTime = tx.validToOrFail(UpperBoundMustBeFinite) - 1
        require(tx.mint.hasOnly(policyId, threadToken, 1), MustMintOneToken) // D3
        val escrow = tx.outputs.findUniqueOrFail( // D4: payment credential only
          _.address.credential === Credential.ScriptCredential(policyId),
          ExpectedOneEscrowOutput
        )
        require(escrow.value.getLovelace === price, WrongEscrowAmount) // D5
        require(
          escrow.value.quantityOf(policyId, threadToken) === BigInt(1),
          TokenNotInEscrow
        ) // D6
        require(escrow.hasInlineDatum(EscrowDatum(depositTime)), WrongDatum) // D7
    }

    /** The rules Accept and Refund share (S1 to S3), and the payment to `party`: it signs, and the
      * outputs at its payment credential hold at least the price in total.
      */
    def settle(tx: TxInfo, ownRef: TxOutRef, party: PubKeyHash): Unit = {
        require(tx.isSignedBy(party), PartyMustSign) // A1, R1
        val ownCredential = tx.findInputOrFail(ownRef).resolved.address.credential // S1
        tx.inputs.findUniqueOrFail( // S2
          _.resolved.address.credential === ownCredential,
          ExpectedOneEscrowInput
        )
        val scriptHash = ownCredential.scriptHashOrFail(OwnInputMustBeScript)
        require(tx.mint.quantityOf(scriptHash, threadToken) === BigInt(-1), MustBurnToken) // S3
        val partyCredential = Credential.PubKeyCredential(party)
        val paid = tx.outputs.foldLeft(BigInt(0)) { (sum, out) =>
            if out.address.credential === partyCredential then sum + out.value.getLovelace
            else sum
        }
        require(paid >= price, PartyMustBePaid) // A2, R3
    }

    // Error messages
    inline val InvalidRedeemer = "Invalid redeemer"
    inline val DatumNotFound = "Datum not found"
    inline val BuyerMustSign = "Buyer must sign the deposit"
    inline val UpperBoundMustBeFinite = "Validity range upper bound must be finite"
    inline val MustMintOneToken = "Deposit must mint exactly one thread token"
    inline val ExpectedOneEscrowOutput = "Expected exactly one output at the script"
    inline val WrongEscrowAmount = "Escrow output must hold exactly the price"
    inline val TokenNotInEscrow = "Escrow output must hold the thread token"
    inline val WrongDatum = "Escrow output must carry the deposit datum"
    inline val MustBurnOneToken = "Burn must destroy exactly one thread token"
    inline val PartyMustSign = "The receiving party must sign"
    inline val ExpectedOneEscrowInput = "Expected exactly one input at the script"
    inline val OwnInputMustBeScript = "Own input must be at a script address"
    inline val MustBurnToken = "The thread token must be burned"
    inline val LowerBoundMustBeFinite = "Validity range lower bound must be finite"
    inline val TooEarly = "Refund deadline has not passed"
    inline val PartyMustBePaid = "The receiving party must be paid the price"
}
