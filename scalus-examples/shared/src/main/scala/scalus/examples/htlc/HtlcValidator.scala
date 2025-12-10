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
object HtlcValidator extends Validator {

    /** Spending script purpose validation
      */
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val Config(committer, receiver, image, timeout) =
            datum.map(_.to[Config]).getOrFail(InvalidDatum)

        redeemer.to[Action] match
            case Action.Timeout =>
                require(tx.isSignedBy(committer), UnsignedCommitterTransaction)
                require(tx.validRange.isEntirelyAfter(timeout), InvalidCommitterTimePoint)

            case Action.Reveal(preimage) =>
                require(tx.isSignedBy(receiver), UnsignedReceiverTransaction)
                require(!tx.validRange.isEntirelyAfter(timeout), InvalidReceiverTimePoint)
                require(sha3_256(preimage) === image, InvalidReceiverPreimage)
    }

    // Error messages
    inline val InvalidDatum = "Datum must be a Config(committer, receiver, image, timeout)"
    inline val UnsignedCommitterTransaction = "Transaction must be signed by a committer"
    inline val UnsignedReceiverTransaction = "Transaction must be signed by a receiver"
    inline val InvalidCommitterTimePoint = "Committer Transaction must be exclusively after timeout"
    inline val InvalidReceiverTimePoint = "Receiver Transaction must be inclusively before timeout"
    inline val InvalidReceiverPreimage = "Invalid receiver preimage"

}
