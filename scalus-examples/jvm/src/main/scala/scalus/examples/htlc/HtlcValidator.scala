package scalus.examples.htlc

import scalus.*
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v3.*
import scalus.prelude.*

type Preimage = ByteString
type Image = ByteString
type PubKeyHash = ByteString

// Contract Datum
case class ContractDatum(
    committer: PubKeyHash,
    receiver: PubKeyHash,
    image: Image,
    timeout: PosixTime
) derives FromData,
      ToData

@Compile
object ContractDatum

// Redeemer
enum Action derives FromData, ToData:
    case Timeout
    case Reveal(preimage: Preimage)

@Compile
object Action

@Compile
object HtlcValidator extends Validator:
    /** Spending script purpose validation
      */
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val ContractDatum(committer, receiver, image, timeout) =
            datum.map(_.to[ContractDatum]).getOrFail(InvalidDatum)

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
    inline val InvalidDatum = "Datum must be a ContractDatum(committer, receiver, image, timeout)"
    inline val UnsignedCommitterTransaction = "Transaction must be signed by a committer"
    inline val UnsignedReceiverTransaction = "Transaction must be signed by a receiver"
    inline val InvalidCommitterTimePoint = "Committer Transaction must be exclusively after timeout"
    inline val InvalidReceiverTimePoint = "Receiver Transaction must be inclusively before timeout"
    inline val InvalidReceiverPreimage = "Invalid receiver preimage"

end HtlcValidator
