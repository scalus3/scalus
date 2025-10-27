package scalus.cardano.ledger

import io.bullet.borer.*
import scalus.builtin.Data

/** Represents the witness set for a transaction in Cardano */
case class TransactionWitnessSet(
    /** VKey witnesses */
    vkeyWitnesses: TaggedSortedSet[VKeyWitness] = TaggedSortedSet.empty,

    /** Native scripts */
    // TODO Map[ScriptHash, Script.Native]
    nativeScripts: TaggedSortedSet[Script.Native] = TaggedSortedSet.empty,

    /** Bootstrap witnesses (for Byron addresses) */
    bootstrapWitnesses: TaggedSortedSet[BootstrapWitness] = TaggedSortedSet.empty,

    /** Plutus V1 scripts */
    // TODO Map[ScriptHash, Script.PlutusV1]
    plutusV1Scripts: TaggedSortedSet[Script.PlutusV1] = TaggedSortedSet.empty,

    /** Plutus data values
      *
      * @note
      *   We need raw CBOR bytes of all `plutusData` for [[ScriptDataHash]] calculation. Also, we
      *   need raw bytes for each datum for [[DataHash]] calculation.
      */
    // TODO KeepRaw[Map[DataHash, KeepRaw[Data]]]
    plutusData: KeepRaw[TaggedSet[KeepRaw[Data]]] = KeepRaw(TaggedSet.empty),

    /** Redeemers */
    // TODO Map ...
    redeemers: Option[KeepRaw[Redeemers]] = None,

    /** Plutus V2 scripts */
    // TODO Map[ScriptHash, Script.PlutusV2]
    plutusV2Scripts: TaggedSortedSet[Script.PlutusV2] = TaggedSortedSet.empty,

    /** Plutus V3 scripts */
    // TODO Map[ScriptHash, Script.PlutusV3]
    plutusV3Scripts: TaggedSortedSet[Script.PlutusV3] = TaggedSortedSet.empty
):
    /** Check if the witness set is empty */
    def isEmpty: Boolean =
        vkeyWitnesses.toSortedSet.isEmpty &&
            nativeScripts.toSortedSet.isEmpty &&
            bootstrapWitnesses.toSortedSet.isEmpty &&
            plutusV1Scripts.toSortedSet.isEmpty &&
            plutusData.value.toIndexedSeq.isEmpty &&
            redeemers.isEmpty &&
            plutusV2Scripts.toSortedSet.isEmpty &&
            plutusV3Scripts.toSortedSet.isEmpty

object TransactionWitnessSet:
    /** Empty witness set */
    val empty: TransactionWitnessSet = TransactionWitnessSet()

    /** Create a witness set from a set of scripts and redeemers, and optional VKey witnesses */
    def apply(
        scripts: Seq[Script],
        redeemers: Redeemers,
        vkeyWitnesses: Set[VKeyWitness],
        plutusData: Seq[Data]
    ): TransactionWitnessSet = {
        val nativeScripts = TaggedSortedSet.from(scripts.collect { case s: Script.Native => s })
        val plutusV1Scripts = TaggedSortedSet.from(scripts.collect { case s: Script.PlutusV1 => s })
        val plutusV2Scripts = TaggedSortedSet.from(scripts.collect { case s: Script.PlutusV2 => s })
        val plutusV3Scripts = TaggedSortedSet.from(scripts.collect { case s: Script.PlutusV3 => s })

        TransactionWitnessSet(
          vkeyWitnesses = TaggedSortedSet.from(vkeyWitnesses),
          nativeScripts = nativeScripts,
          plutusV1Scripts = plutusV1Scripts,
          plutusV2Scripts = plutusV2Scripts,
          plutusV3Scripts = plutusV3Scripts,
          plutusData = KeepRaw(TaggedSet(plutusData.map(KeepRaw(_))*)),
          redeemers = Some(KeepRaw(redeemers))
        )
    }

    /** CBOR encoder for TransactionWitnessSet */
    given Encoder[TransactionWitnessSet] with
        def write(w: Writer, value: TransactionWitnessSet): Writer =
            // Count the number of fields to write
            var mapSize = 0

            if value.vkeyWitnesses.toSortedSet.nonEmpty then mapSize += 1
            if value.nativeScripts.toSortedSet.nonEmpty then mapSize += 1
            if value.bootstrapWitnesses.toSortedSet.nonEmpty then mapSize += 1
            if value.plutusV1Scripts.toSortedSet.nonEmpty then mapSize += 1
            if value.plutusData.value.toIndexedSeq.nonEmpty then mapSize += 1
            if value.redeemers.isDefined then mapSize += 1
            if value.plutusV2Scripts.toSortedSet.nonEmpty then mapSize += 1
            if value.plutusV3Scripts.toSortedSet.nonEmpty then mapSize += 1

            w.writeMapHeader(mapSize)

            // VKey witnesses (key 0)
            if value.vkeyWitnesses.toSortedSet.nonEmpty then
                w.writeInt(0)
                writeSet(w, value.vkeyWitnesses)

            // Native scripts (key 1)
            if value.nativeScripts.toSortedSet.nonEmpty then
                w.writeInt(1)
                writeSet(w, value.nativeScripts)

            // Bootstrap witnesses (key 2)
            if value.bootstrapWitnesses.toSortedSet.nonEmpty then
                w.writeInt(2)
                writeSet(w, value.bootstrapWitnesses)

            // Plutus V1 scripts (key 3)
            if value.plutusV1Scripts.toSortedSet.nonEmpty then
                w.writeInt(3)
                writeSet(w, value.plutusV1Scripts)

            // Plutus data (key 4)
            if value.plutusData.value.toIndexedSeq.nonEmpty then
                w.writeInt(4)
                // TODO: handle KeepRaw properly when this is implemented: https://github.com/sirthias/borer/issues/764
                w.write(value.plutusData)

            // Redeemers (key 5)
            value.redeemers.foreach { redeemers =>
                w.writeInt(5)
                w.write(redeemers)
            }

            // Plutus V2 scripts (key 6)
            if value.plutusV2Scripts.toSortedSet.nonEmpty then
                w.writeInt(6)
                writeSet(w, value.plutusV2Scripts)

            // Plutus V3 scripts (key 7)
            if value.plutusV3Scripts.toSortedSet.nonEmpty then
                w.writeInt(7)
                writeSet(w, value.plutusV3Scripts)

            w

    /** Helper to write a Set as CBOR */
    private inline def writeSet[A: Encoder](w: Writer, set: TaggedSortedSet[A]): Writer =
        summon[Encoder[TaggedSortedSet[A]]].write(w, set)

    /** CBOR decoder for TransactionWitnessSet */
    given decoder(using OriginalCborByteArray): Decoder[TransactionWitnessSet] with
        def read(r: Reader): TransactionWitnessSet =
            val mapSize = r.readMapHeader()

            var vkeyWitnesses = TaggedSortedSet.empty[VKeyWitness]
            var nativeScripts = TaggedSortedSet.empty[Script.Native]
            var bootstrapWitnesses = TaggedSortedSet.empty[BootstrapWitness]
            var plutusV1Scripts = TaggedSortedSet.empty[Script.PlutusV1]
            var plutusData = KeepRaw(TaggedSet.empty[KeepRaw[Data]])
            var redeemers: Option[KeepRaw[Redeemers]] = None
            var plutusV2Scripts = TaggedSortedSet.empty[Script.PlutusV2]
            var plutusV3Scripts = TaggedSortedSet.empty[Script.PlutusV3]

            for _ <- 0L until mapSize do
                val key = r.readInt()

                key match
                    case 0 => // VKey witnesses
                        vkeyWitnesses = readSet(r)

                    case 1 => // Native scripts
                        nativeScripts = readSet(r)

                    case 2 => // Bootstrap witnesses
                        bootstrapWitnesses = readSet(r)

                    case 3 => // Plutus V1 scripts
                        plutusV1Scripts = readSet(r)

                    case 4 => // Plutus data
                        plutusData = r.read[KeepRaw[TaggedSet[KeepRaw[Data]]]]()

                    case 5 => // Redeemers
                        redeemers = Some(r.read[KeepRaw[Redeemers]]())

                    case 6 => // Plutus V2 scripts
                        plutusV2Scripts = readSet(r)

                    case 7 => // Plutus V3 scripts
                        plutusV3Scripts = readSet(r)

                    case _ => r.skipDataItem() // Skip unknown fields

            TransactionWitnessSet(
              vkeyWitnesses = vkeyWitnesses,
              nativeScripts = nativeScripts,
              bootstrapWitnesses = bootstrapWitnesses,
              plutusV1Scripts = plutusV1Scripts,
              plutusData = plutusData,
              redeemers = redeemers,
              plutusV2Scripts = plutusV2Scripts,
              plutusV3Scripts = plutusV3Scripts
            )

    /** Helper to read a Set from CBOR */
    private inline def readSet[A: Decoder: Ordering](r: Reader): TaggedSortedSet[A] =
        r.read[TaggedSortedSet[A]]()

    given Ordering[Script.Native] = Ordering.by[Script.Native, ScriptHash](_.scriptHash)
    given Ordering[Script.PlutusV1] = Ordering.by[Script.PlutusV1, ScriptHash](_.scriptHash)
    given Ordering[Script.PlutusV2] = Ordering.by[Script.PlutusV2, ScriptHash](_.scriptHash)
    given Ordering[Script.PlutusV3] = Ordering.by[Script.PlutusV3, ScriptHash](_.scriptHash)
