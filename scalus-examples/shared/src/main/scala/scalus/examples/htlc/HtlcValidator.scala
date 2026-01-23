package scalus.examples.htlc

import scalus.Compile
import scalus.uplc.builtin.Builtins.sha3_256
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

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
                val validFrom = tx.validRange.from.finite(0)
                // validFrom is inclusive, hence 10 <= 10 is correct
                require(config.timeout <= validFrom, InvalidCommitterTimePoint)
                require(tx.isSignedBy(config.committer), UnsignedCommitterTransaction)
            case Action.Reveal(preimage) =>
                val validTo = tx.validRange.to.finiteOrFail(ValidRangeMustBeBound)
                // validTo is exclusive, hence 10 <= 10 is correct
                require(validTo <= config.timeout, InvalidReceiverTimePoint)
                require(tx.isSignedBy(config.receiver), UnsignedReceiverTransaction)
                require(sha3_256(preimage) == config.image, InvalidReceiverPreimage)
    }

    // Error messages
    inline val MustBeSpending = "Must be a spending script"
    inline val InvalidDatum = "Invalid Datum"
    inline val ValidRangeMustBeBound = "ValidTo must be set"
    inline val UnsignedCommitterTransaction = "Must be signed by a committer"
    inline val UnsignedReceiverTransaction = "Must be signed by a receiver"
    inline val InvalidCommitterTimePoint = "Must be exclusively after timeout"
    inline val InvalidReceiverTimePoint = "Must be inclusively before timeout"
    inline val InvalidReceiverPreimage = "Invalid preimage"
}
