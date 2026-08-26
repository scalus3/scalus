package scalus.examples.cape.twopartyescrow

import scalus.compiler.{offsetOf, Compile}

import scalus.cardano.onchain
import scalus.cardano.onchain.plutus
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.Builtins
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.*

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
  * Deposit is invoked without a matching "own" spent input (the buyer is funding a fresh escrow
  * UTXO, not spending an existing one), so it locates the escrow output by credential type (any
  * `ScriptCredential`) instead of via the own-input's resolved address. Accept and Refund do spend
  * an existing script UTXO, so they resolve their own address via `findOwnInputOrFail` as before.
  *
  * The deposit time is recorded as the *upper* bound of the deposit transaction's valid range (it
  * must be finite); the refund deadline check reads the *lower* bound of the refund transaction's
  * valid range (it must also be finite, and strictly after `depositTime + deadlineSeconds`).
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
        // ScriptContext = Constr 0 [txInfo, redeemer, scriptInfo]. `scData.to[ScriptContext]`
        // would eagerly decode `txInfo` as a full 16-field `TxInfo` (`derives FromData`
        // materializes every constructor field, e.g. fee/mint/certificates/withdrawals/
        // redeemers/data/id/votes/proposalProcedures/currentTreasuryAmount/treasuryDonation --
        // none of which this validator reads); navigate to the 3 top-level fields directly
        // instead and keep `txInfo` as raw `Data` so `spend` can pull only what it needs.
        val scFields = Builtins.unConstrData(scData).snd
        val txInfoData = scFields.head
        val afterTxInfo = scFields.tail
        val redeemer = afterTxInfo.head
        afterTxInfo.tail.head.to[ScriptInfo] match
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(datum, redeemer, txInfoData, txOutRef)
            case _ => fail("Only spending scripts are supported by this validator")
    }

    inline def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfoData: Data,
        txOutRef: TxOutRef
    ): Unit = {
        val action = redeemer.to[BigInt]

        // TxInfo = Constr 0 [inputs, referenceInputs, outputs, fee, mint, certificates,
        // withdrawals, validRange, signatories, redeemers, data, id, votes, proposalProcedures,
        // currentTreasuryAmount, treasuryDonation]. `outputs`/`signatories` are read by every
        // action; `dropList` jumps straight to each one instead of decoding (and discarding) the
        // fields before it. `inputs` and `validRange` are only decoded on the branches that
        // actually read them (Deposit never resolves an own input; Accept never reads the valid
        // range), same as before this rewrite.
        val txInfoFields = Builtins.unConstrData(txInfoData).snd
        val outputs = Builtins
            .dropList(offsetOf[TxInfo](_.outputs), txInfoFields)
            .head
            .to[List[v2.TxOut]]
        val signatories = Builtins
            .dropList(offsetOf[TxInfo](_.signatories), txInfoFields)
            .head
            .to[List[PubKeyHash]]

        if action == BigInt(0) then
            val validRange = Builtins
                .dropList(offsetOf[TxInfo](_.validRange), txInfoFields)
                .head
                .to[Interval]
            handleDeposit(signatories, validRange, outputs)
        else
            // Accept and Refund both spend an existing script UTXO, so resolve the own
            // credential once and share it (Deposit never reaches this branch, so it never
            // pays for the own-input search).
            val inputs = txInfoFields.head.to[List[TxInInfo]]
            val ownCredential = findOwnCredential(inputs, txOutRef)
            if action == BigInt(1) then handleAccept(datum, signatories, outputs, ownCredential)
            else if action == BigInt(2) then
                val validRange =
                    Builtins
                        .dropList(offsetOf[TxInfo](_.validRange), txInfoFields)
                        .head
                        .to[Interval]
                handleRefund(datum, signatories, validRange, outputs, ownCredential)
            else fail("Invalid redeemer")
    }

    inline def handleDeposit(
        signatories: List[PubKeyHash],
        validRange: Interval,
        outputs: List[TxOut]
    ): Unit = {
        requireSignedBy(signatories, buyerKeyHash, "Buyer must sign deposit")

        // Deposit time is the upper bound of the valid range; it must be finite so the refund
        // deadline (depositTime + deadlineSeconds) can be computed safely.
        val depositTime = requireFiniteUpperBound(validRange)

        val expectedDatum = EscrowDatum(
          state = EscrowState.Deposited,
          depositTime = depositTime
        ).toData

        // No own input exists yet (the escrow UTXO is being created), so the escrow output is
        // identified by credential type rather than by matching an own-input address.
        val output =
            outputs.filter(out => out.address.credential.scriptOption.isDefined) match
                case List.Cons(head, List.Nil) => head
                case _                         => fail("Expected exactly one script output")

        require(
          output.value.toData == Value.lovelace(escrowPrice).toData,
          "Output must contain exactly the escrow price"
        )
        require(
          output.datum.toData == OutputDatum.OutputDatum(expectedDatum).toData,
          "Output must have the expected deposit datum"
        )
        output.referenceScript match
            case Option.None => ()
            case _           => fail("Output must not carry a reference script")
    }

    inline def handleAccept(
        datum: Option[Data],
        signatories: List[PubKeyHash],
        outputs: List[TxOut],
        ownCredential: Credential
    ): Unit = {
        // Parse datum and verify state is Deposited
        val receivedData = datum.getOrFail("Datum not found")
        val escrowDatum = receivedData.to[EscrowDatum]
        escrowDatum.state match
            case EscrowState.Deposited => ()
            case _                     => fail("Escrow must be in Deposited state")

        requireSignedBy(signatories, sellerKeyHash, "Seller must sign accept")

        // Verify seller receives exactly escrow price, and no funds remain in the script -- in
        // one traversal of `outputs` (the previous version walked it twice: once via foldLeft
        // for the sum, once via findOutputsByCredential + isEmpty for the "no funds remain"
        // check).
        settleAndVerify(
          outputs,
          Credential.PubKeyCredential(sellerKeyHash).toData,
          ownCredential,
          escrowPrice,
          "Seller must receive exactly escrow price"
        )
    }

    inline def handleRefund(
        datum: Option[Data],
        signatories: List[PubKeyHash],
        validRange: Interval,
        outputs: List[TxOut],
        ownCredential: Credential
    ): Unit = {
        // Parse datum and verify state is Deposited
        val escrowDatum = datum.getOrFail("Datum not found").to[EscrowDatum]
        escrowDatum.state match
            case EscrowState.Deposited => ()
            case _                     => fail("Escrow must be in Deposited state")

        requireSignedBy(signatories, buyerKeyHash, "Buyer must sign refund")

        // Time check: valid range must be entirely after deadline
        val deadline = escrowDatum.depositTime + deadlineSeconds
        require(validRange.isEntirelyAfter(deadline), "Deadline has not passed")

        // Verify buyer receives exactly escrow price, and no funds remain in the script -- see
        // the comment in handleAccept.
        settleAndVerify(
          outputs,
          Credential.PubKeyCredential(buyerKeyHash).toData,
          ownCredential,
          escrowPrice,
          "Buyer must receive exactly escrow price"
        )
    }

    /** Sums the lovelace paid to `partyCred` and asserts it equals `expectedAmount`, while also
      * asserting no output remains at `ownCredential` (the script's own address) -- both checks in
      * a single pass over `outputs`, decoding each output's credential exactly once.
      */
    def settleAndVerify(
        outputs: List[TxOut],
        partyCred: Data,
        ownCredential: Credential,
        expectedAmount: BigInt,
        message: String
    ): Unit = {
        // Hoisted out of `go` so it's computed once instead of once per output.
        val ownCredData = ownCredential.toData
        def go(outs: List[TxOut], sum: BigInt): BigInt = outs match
            case List.Nil => sum
            case List.Cons(out, tail) =>
                val credData = out.address.credential.toData
                if credData == ownCredData then fail("No funds should remain in script")
                else go(tail, if credData == partyCred then sum + out.value.lovelaceAmount else sum)
        require(go(outputs, 0) == expectedAmount, message)
    }

    def findOwnCredential(inputs: List[TxInInfo], txOutRef: TxOutRef): Credential =
        findOwnInputOrFail(inputs, txOutRef).resolved.address.credential

    def findOwnInputOrFail(inputs: List[TxInInfo], txOutRef: TxOutRef): TxInInfo = {
        // Hoisted out of `go` so it's computed once instead of once per input.
        val txOutRefData = txOutRef.toData
        def go(inputs: List[TxInInfo]): TxInInfo = inputs match
            case List.Cons(head, tail) =>
                if head.outRef.toData == txOutRefData then head
                else go(tail)
            case List.Nil => fail("Own input not found")
        go(inputs)
    }

    /** Extracts the finite upper bound of `range`, applying the exclusive/inclusive adjustment (a
      * finite exclusive bound `t` denotes upper bound `t - 1`). Fails if the upper bound is
      * infinite.
      */
    def requireFiniteUpperBound(range: Interval): BigInt =
        range.to.boundType match
            case IntervalBoundType.Finite(t) => if range.to.isInclusive then t else t - 1
            case _                           => fail("Valid range upper bound must be finite")

    def requireSignedBy(
        signatories: List[PubKeyHash],
        party: PubKeyHash,
        message: String
    ): Unit = {
        def go(signatories: List[PubKeyHash]): Unit = signatories match {
            case List.Nil              => fail(message)
            case List.Cons(head, tail) => if head.toData == party.toData then () else go(tail)
        }
        go(signatories)
    }
}
