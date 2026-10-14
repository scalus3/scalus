package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

import scala.collection.immutable.SortedMap

/** Represents withdrawals in a transaction.
  *
  * Withdrawals is a map from reward accounts to withdrawal amounts. It's used to withdraw staking
  * rewards.
  *
  * @param withdrawals
  *   Map from reward accounts to withdrawal amounts
  */
case class Withdrawals(withdrawals: SortedMap[RewardAccount, Coin]) {

    /** Checks if there are any withdrawals.
      *
      * @return
      *   true if there are no withdrawals, false otherwise
      */
    def isEmpty: Boolean = withdrawals.isEmpty
}

object Withdrawals {

    /** Creates an empty Withdrawals.
      *
      * @return
      *   An empty Withdrawals
      */
    val empty: Withdrawals = Withdrawals(SortedMap.empty)

    /** CBOR Encoder for Withdrawals. Encodes as a map from reward accounts to coin values.
      */
    given Encoder[Withdrawals] = (w: Writer, value: Withdrawals) => {
        w.write(value.withdrawals)
    }

    /** Reads withdrawals as the ledger does at protocol version `pv`: from version 9 a repeated
      * reward account fails decoding, before it the last amount wins.
      */
    private[ledger] def read(r: Reader)(using ProtocolVersion): Withdrawals =
        Withdrawals(SortedMap.from(LedgerMap.read[RewardAccount, Coin](r)))

    /** CBOR Decoder for Withdrawals. Decodes from a map from reward accounts to coin values, as at
      * protocol version 9 and later.
      */
    given Decoder[Withdrawals] = (r: Reader) => read(r)(using ProtocolVersion.conwayPV)
}
