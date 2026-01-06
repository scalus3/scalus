package scalus.examples.htlc

import scalus.*
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v3.*
import scalus.prelude.*

type Preimage = ByteString
type Image = ByteString

// Datum
case class Config(
    committer: PubKeyHash,
    receiver: PubKeyHash,
    image: Image,
    timeout: PosixTime
) derives FromData,
      ToData

@Compile
object Config

// Redeemer
enum Action derives FromData, ToData:
    case Timeout
    case Reveal(preimage: Preimage)

@Compile
object Action

/** A Hash Time-Locked Contract (HTLC) validator.
  *
  * The HTLC allows a receiver to claim funds by revealing a preimage of a hash before a timeout, or
  * allows the committer to reclaim the funds after the timeout.
  *
  * @see
  *   https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/htlc
  */
@Compile
object HtlcValidator {

    inline def validate(scData: Data): Unit = {
        val ctx = scData.to[ScriptContext]
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(datum, ctx.redeemer, ctx.txInfo, txOutRef)
            case _ => fail(MustBeSpending)
    }

    /** Spending script purpose validation
      */
    inline def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val config = datum.getOrFail(InvalidDatum).to[Config]
        redeemer.to[Action] match
            case Action.Timeout =>
                require(tx.isSignedBy(config.committer), UnsignedCommitterTransaction)
                require(tx.validRange.isEntirelyAfter(config.timeout), InvalidCommitterTimePoint)

            case Action.Reveal(preimage) =>
                require(tx.isSignedBy(config.receiver), UnsignedReceiverTransaction)
                require(!tx.validRange.isEntirelyAfter(config.timeout), InvalidReceiverTimePoint)
                require(sha3_256(preimage) == config.image, InvalidReceiverPreimage)
    }

    // Error messages
    inline val MustBeSpending = "Must be a spending script"
    inline val InvalidDatum = "Invalid Datum"
    inline val UnsignedCommitterTransaction = "Must be signed by a committer"
    inline val UnsignedReceiverTransaction = "Must be signed by a receiver"
    inline val InvalidCommitterTimePoint = "Must be exclusively after timeout"
    inline val InvalidReceiverTimePoint = "Must be inclusively before timeout"
    inline val InvalidReceiverPreimage = "Invalid preimage"
}
