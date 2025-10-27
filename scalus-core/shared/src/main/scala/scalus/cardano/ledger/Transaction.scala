package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.builtin.{platform, ByteString, given}

import scalus.serialization.cbor.Cbor

/** Represents a complete transaction in Cardano */
case class Transaction(
    /** The transaction body */
    body: KeepRaw[TransactionBody],

    /** Witness set containing signatures and scripts */
    witnessSet: TransactionWitnessSet,

    /** Is the transaction valid? */
    isValid: Boolean = true,

    /** Optional auxiliary data */
    auxiliaryData: Option[KeepRaw[AuxiliaryData]] = None
) {
    @transient lazy val id: TransactionHash = Hash(
      platform.blake2b_256(ByteString.unsafeFromArray(body.raw))
    )

    @transient lazy val validityInterval: ValidityInterval =
        ValidityInterval(body.value.validityStartSlot, body.value.ttl)

    def toCbor: Array[Byte] = {
        Cbor.encode(this)
    }
}

object Transaction {

    /** Create a transaction with the given body and witness set, assuming it is valid and has no
      * auxiliary data
      */
    def apply(
        body: TransactionBody,
        witnessSet: TransactionWitnessSet
    ): Transaction =
        new Transaction(KeepRaw(body), witnessSet, isValid = true, auxiliaryData = None)

    /** Create a transaction with the given body, witness set, and auxiliary data, assuming it is
      * valid
      */
    def apply(
        body: TransactionBody,
        witnessSet: TransactionWitnessSet,
        auxiliaryData: AuxiliaryData
    ): Transaction =
        new Transaction(
          KeepRaw(body),
          witnessSet,
          isValid = true,
          auxiliaryData = Some(KeepRaw(auxiliaryData))
        )

    /** An empty transaction */
    def empty: Transaction = Transaction(
      TransactionBody(TaggedSortedSet.empty, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty,
    )

    def fromCbor(bytes: Array[Byte]): Transaction = {
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        Cbor.decode(bytes)
    }

    /** CBOR codec for Transaction */
    given Encoder[Transaction] = Encoder.derived
    given decoder(using OriginalCborByteArray): Decoder[Transaction] =
        Decoder.derived[Transaction]
}
