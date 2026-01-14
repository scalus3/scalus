package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import org.typelevel.paiges.Doc
import scalus.builtin.{platform, ByteString, given}
import scalus.serialization.cbor.Cbor
import scalus.utils.{Pretty, Style}

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

    def validityInterval: ValidityInterval =
        ValidityInterval(body.value.validityStartSlot, body.value.ttl)

    /** Returns UTXOs that would be created by this transaction.
      *
      * Returns a map of TransactionInput -> TransactionOutput for all outputs, allowing them to be
      * used as inputs in subsequent chained transactions.
      */
    def utxos: Utxos =
        body.value.outputs.view.zipWithIndex.map { case (output, idx) =>
            TransactionInput(id, idx) -> output.value
        }.toMap

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

    def fromCbor(
        bytes: Array[Byte]
    )(using pv: ProtocolVersion = ProtocolVersion.conwayPV): Transaction = {
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        Cbor.decode[Transaction](bytes)
    }

    /** CBOR codec for Transaction */
    given Encoder[Transaction] = Encoder.derived
    given decoder(using OriginalCborByteArray, ProtocolVersion): Decoder[Transaction] =
        Decoder.derived[Transaction]

    import Doc.*

    /** Pretty prints Transaction with id, body, witnesses count, and validity */
    given Pretty[Transaction] with
        def pretty(a: Transaction, style: Style): Doc =
            val idDoc = text("id:") & text(a.id.toHex)
            val bodyDoc = text("body:") / Pretty[TransactionBody]
                .pretty(a.body.value, style)
                .indent(2)
            val witnessDoc = {
                val ws = a.witnessSet
                val vkeyCount = ws.vkeyWitnesses.toSet.size
                val scriptCount = ws.nativeScripts.toMap.size + ws.plutusV1Scripts.toMap.size +
                    ws.plutusV2Scripts.toMap.size + ws.plutusV3Scripts.toMap.size
                val redeemerCount = ws.redeemers.map(_.value.toMap.size).getOrElse(0)
                text("witnesses:") / stack(
                  List(
                    text(s"vkeys: $vkeyCount"),
                    text(s"scripts: $scriptCount"),
                    text(s"redeemers: $redeemerCount")
                  )
                ).indent(2)
            }
            val validDoc = text("valid:") & text(a.isValid.toString)
            (text("Transaction") / idDoc / bodyDoc / witnessDoc / validDoc).hang(2)
}
