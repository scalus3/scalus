package scalus.examples.cape.htlc

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.builtin.Builtins.sha2_256
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.*

case class HtlcDatum(
    payer: Address,
    recipient: Address,
    hash: ByteString,
    timeout: BigInt
) derives FromData,
      ToData

enum HtlcRedeemer derives FromData, ToData:
    case Claim(preimage: ByteString)
    case Refund

/** UPLC-CAPE HTLC validator: claim with SHA-256 preimage before timeout, refund after.
  *
  * Funds locked under an [[HtlcDatum]] have two exits, and exactly one of them is open at any point
  * in time:
  *
  *   - `Claim`: the recipient signs, reveals a preimage hashing to `hash`, and the transaction's
  *     whole validity range lies strictly before `timeout`.
  *   - `Refund`: the payer signs and the transaction's whole validity range lies strictly after
  *     `timeout`.
  *
  * Both deadline tests are the prelude's `Interval.isEntirelyBefore` / `isEntirelyAfter`, which is
  * what makes them safe as well as short. Each one compares against the far end of the range rather
  * than a single "current time", so a transaction cannot straddle the timeout and satisfy a rule it
  * only meets at one end; each accounts for the bound's closure, so it stays correct across the
  * protocol-version change that made finite upper bounds exclusive
  * (`LedgerToPlutusTranslation.getInterval`); and each returns `false` on an unbounded range, which
  * is what rejects the `claim_infinite_upper_bound` and `refund_infinite_lower_bound` fixtures.
  *
  * Everything else is the standard prelude plumbing shared with the other CAPE validators:
  * `TxInfo.findOwnInputOrFail`, `TxInfo.isSignedBy`, and `List.count` for the double-satisfaction
  * guard. An earlier revision hand-navigated the raw `ScriptContext` `Data` and hand-rolled local
  * copies of all of these to dodge the cost of the library versions; those are dropped in favor of
  * the canonical forms, since library and compiler fixes are the intended remedy for a cost gap,
  * not validator-level workarounds. See `docs/internal/CAPE_COMPETITIVE_ANALYSIS.md` for the
  * resulting deltas and leaderboard standing.
  *
  * @see
  *   [[https://github.com/IntersectMBO/UPLC-CAPE]]
  */
@Compile
object HtlcValidator extends Validator {

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val d = datum.getOrFail(DatumNotFound).to[HtlcDatum]
        val ownCred = txInfo.findOwnInputOrFail(txOutRef).resolved.address.credential
        // Double-satisfaction guard: spending several HTLC UTxOs in one transaction would let a
        // single preimage (or a single refund deadline) unlock all of them at once.
        require(
          txInfo.inputs.count(_.resolved.address.credential === ownCred) === BigInt(1),
          MultipleScriptInputs
        )
        redeemer.to[HtlcRedeemer] match
            case HtlcRedeemer.Claim(preimage) =>
                require(txInfo.isSignedBy(pkhOf(d.recipient)), NoRecipientSignature)
                require(sha2_256(preimage) == d.hash, PreimageMismatch)
                require(txInfo.validRange.isEntirelyBefore(d.timeout), TooLateToClaim)
            case HtlcRedeemer.Refund =>
                require(txInfo.isSignedBy(pkhOf(d.payer)), NoPayerSignature)
                require(txInfo.validRange.isEntirelyAfter(d.timeout), TooEarlyToRefund)
    }

    def pkhOf(address: Address): PubKeyHash = address.credential match
        case Credential.PubKeyCredential(pkh) => pkh
        case _                                => fail(ExpectedPubKeyAddress)

    // Error messages
    inline val DatumNotFound = "No datum"
    inline val ExpectedPubKeyAddress = "Expected pubkey address"
    inline val MultipleScriptInputs = "Multiple script inputs"
    inline val NoRecipientSignature = "Recipient must sign"
    inline val NoPayerSignature = "Payer must sign"
    inline val PreimageMismatch = "Preimage mismatch"
    inline val TooLateToClaim = "Too late to claim"
    inline val TooEarlyToRefund = "Too early to refund"
}
