package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import org.typelevel.paiges.Doc
import scalus.uplc.builtin.{platform, ByteString}
import scalus.serialization.cbor.Cbor
import scalus.utils.{Pretty, Style}

/** Represents a complete transaction in Cardano */
case class Transaction(
    /** The transaction body */
    body: KeepRaw[TransactionBody],

    /** Witness set containing signatures and scripts */
    witnessSetRaw: KeepRaw[TransactionWitnessSet],

    /** Is the transaction valid? */
    isValid: Boolean,

    /** Optional auxiliary data */
    auxiliaryData: Option[KeepRaw[AuxiliaryData]]
) {

    /** The transaction hash (blake2b-256 of the serialized body) */
    @transient lazy val id: TransactionHash = Hash(
      platform.blake2b_256(ByteString.unsafeFromArray(body.raw))
    )

    /** The unwrapped witness set */
    def witnessSet: TransactionWitnessSet = witnessSetRaw.value

    /** The transaction's validity interval (start slot and TTL) */
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

    /** Returns the CBOR encoding of this transaction */
    def toCbor: Array[Byte] = Cbor.encode(this)

    /** Returns CBOR encoding for transaction size/fee calculation.
      *
      * This encoding omits the `isValid` field for backward compatibility with Mary era, matching
      * Cardano ledger's `toCBORForSizeComputation` function.
      *
      * @see
      *   Cardano.Ledger.Alonzo.Tx.toCBORForSizeComputation
      */
    def toCborForFeeCalculation: Array[Byte] =
        Cbor.encode(this)(using Transaction.feeCalculationEncoder)

    /** Returns a copy with the body replaced */
    def withBody(b: TransactionBody): Transaction =
        copy(body = KeepRaw(b))

    /** Returns a copy with the body transformed by the given function */
    def withBody(f: TransactionBody => TransactionBody): Transaction =
        copy(body = KeepRaw(f(body.value)))

    /** Returns a copy with the witness set replaced */
    def withWitness(ws: TransactionWitnessSet): Transaction =
        copy(witnessSetRaw = KeepRaw(ws))

    /** Returns a copy with the witness set transformed by the given function */
    def withWitness(f: TransactionWitnessSet => TransactionWitnessSet): Transaction =
        copy(witnessSetRaw = KeepRaw(f(witnessSet)))
}

object Transaction {

    /** Create a transaction with the given body and empty witness set, assuming valid and no
      * auxiliary data
      */
    def apply(body: TransactionBody): Transaction =
        Transaction(KeepRaw(body), KeepRaw(TransactionWitnessSet.empty), true, None)

    /** Create a transaction with the given body and witness set, assuming valid and no auxiliary
      * data
      */
    def apply(body: TransactionBody, witnessSet: TransactionWitnessSet): Transaction =
        Transaction(KeepRaw(body), KeepRaw(witnessSet), true, None)

    /** Create a transaction with the given body, witness set, and auxiliary data, assuming valid */
    def apply(
        body: TransactionBody,
        witnessSet: TransactionWitnessSet,
        auxiliaryData: AuxiliaryData
    ): Transaction =
        Transaction(
          KeepRaw(body),
          KeepRaw(witnessSet),
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

    /** Encoder for fee calculation (omits isValid field for Mary-era compatibility) */
    private def feeCalculationEncoder: Encoder[Transaction] = Encoder { (w, tx) =>
        w.writeArrayHeader(3)
        w.output.writeBytes(tx.body.raw)
        w.output.writeBytes(tx.witnessSetRaw.raw)
        tx.auxiliaryData match
            case Some(auxData) => w.output.writeBytes(auxData.raw)
            case None          => w.writeNull()
        w
    }

    import Doc.*
    import Pretty.{bulletList, lit}

    /** Pretty prints Transaction with inlined body and witness fields */
    given Pretty[Transaction] with
        def pretty(a: Transaction, style: Style): Doc =
            val body = a.body.value
            val ws = a.witnessSet

            val fields = List.newBuilder[Doc]

            // Transaction id
            fields += text("id:") & lit(text(a.id.toHex), style)

            // Inputs
            val inputDocs = body.inputs.toSeq.map(Pretty[TransactionInput].pretty(_, style)).toList
            if inputDocs.nonEmpty then fields += bulletList("inputs", inputDocs)

            // Reference inputs
            val refInputDocs =
                body.referenceInputs.toSeq.map(Pretty[TransactionInput].pretty(_, style)).toList
            if refInputDocs.nonEmpty then fields += bulletList("referenceInputs", refInputDocs)

            // Outputs
            if body.outputs.nonEmpty then
                fields += text("outputs:") / stack(body.outputs.zipWithIndex.map { case (o, idx) =>
                    (text(s"[$idx]") & Pretty[TransactionOutput].pretty(o.value, style)).hang(2)
                }).indent(2)

            // Fee
            fields += text("fee:") & Pretty[Coin].pretty(body.fee, style)

            // TTL and validity
            body.ttl.foreach(t => fields += text(s"ttl: $t"))
            body.validityStartSlot.foreach(s => fields += text(s"validityStart: $s"))

            // Mint
            body.mint.foreach(m => fields += text("mint:") & Pretty[MultiAsset].pretty(m, style))

            // Collateral
            val collateralDocs =
                body.collateralInputs.toSeq.map(Pretty[TransactionInput].pretty(_, style)).toList
            if collateralDocs.nonEmpty then fields += bulletList("collateral", collateralDocs)

            // VKey witnesses (just hashes)
            val vkeyDocs = ws.vkeyWitnesses.toSeq.map(Pretty[VKeyWitness].pretty(_, style)).toList
            if vkeyDocs.nonEmpty then fields += bulletList("vkeys", vkeyDocs)

            // Scripts (hashes)
            val scriptDocs =
                ws.nativeScripts.toMap.values.map(Pretty[Script.Native].pretty(_, style)).toList ++
                    ws.plutusV1Scripts.toMap.values
                        .map(Pretty[Script.PlutusV1].pretty(_, style))
                        .toList ++
                    ws.plutusV2Scripts.toMap.values
                        .map(Pretty[Script.PlutusV2].pretty(_, style))
                        .toList ++
                    ws.plutusV3Scripts.toMap.values
                        .map(Pretty[Script.PlutusV3].pretty(_, style))
                        .toList
            if scriptDocs.nonEmpty then fields += bulletList("scripts", scriptDocs)

            // Redeemers (with full data)
            ws.redeemers.foreach { r =>
                val redeemerDocs = r.value.toSeq.map(Pretty[Redeemer].pretty(_, style)).toList
                if redeemerDocs.nonEmpty then fields += bulletList("redeemers", redeemerDocs)
            }

            // Plutus data
            if ws.plutusData.value.toMap.nonEmpty then
                val dataDocs = ws.plutusData.value.toMap.values
                    .map(kr => Pretty[scalus.uplc.builtin.Data].pretty(kr.value, style))
                    .toList
                fields += bulletList("datums", dataDocs)

            // Valid flag (only show if false)
            if !a.isValid then fields += text("valid: false")

            text("Transaction(") / stack(fields.result()).indent(2) / char(')')
}
