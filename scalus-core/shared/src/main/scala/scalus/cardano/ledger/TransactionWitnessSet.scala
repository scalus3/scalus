package scalus.cardano.ledger

import io.bullet.borer.*
import scalus.builtin.Data

/** Represents the witness set for a transaction in Cardano */
case class TransactionWitnessSet(
    /** VKey witnesses */
    vkeyWitnesses: TaggedSortedSet[VKeyWitness] = TaggedSortedSet.empty,

    /** Native scripts */
    nativeScripts: TaggedSortedMap[ScriptHash, Script.Native] = TaggedSortedMap.empty,

    /** Bootstrap witnesses (for Byron addresses) */
    bootstrapWitnesses: TaggedSortedSet[BootstrapWitness] = TaggedSortedSet.empty,

    /** Plutus V1 scripts */
    plutusV1Scripts: TaggedSortedStrictMap[ScriptHash, Script.PlutusV1] =
        TaggedSortedStrictMap.empty,

    /** Plutus data values
      *
      * @note
      *   We need raw CBOR bytes of all `plutusData` for [[ScriptDataHash]] calculation. Also, we
      *   need raw bytes for each datum for [[DataHash]] calculation.
      */
    plutusData: KeepRaw[TaggedSortedMap[DataHash, KeepRaw[Data]]] = KeepRaw(TaggedSortedMap.empty),

    /** Redeemers */
    // TODO Map ...
    redeemers: Option[KeepRaw[Redeemers]] = None,

    /** Plutus V2 scripts */
    plutusV2Scripts: TaggedSortedStrictMap[ScriptHash, Script.PlutusV2] =
        TaggedSortedStrictMap.empty,

    /** Plutus V3 scripts */
    plutusV3Scripts: TaggedSortedStrictMap[ScriptHash, Script.PlutusV3] =
        TaggedSortedStrictMap.empty
):
    /** Check if the witness set is empty */
    def isEmpty: Boolean =
        vkeyWitnesses.toSet.isEmpty &&
            nativeScripts.toMap.isEmpty &&
            bootstrapWitnesses.toSet.isEmpty &&
            plutusV1Scripts.toMap.isEmpty &&
            plutusData.value.toMap.isEmpty &&
            redeemers.isEmpty &&
            plutusV2Scripts.toMap.isEmpty &&
            plutusV3Scripts.toMap.isEmpty

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
        val nativeScripts = TaggedSortedMap.from[ScriptHash, Script.Native](scripts.collect {
            case s: Script.Native => s
        })
        val plutusV1Scripts =
            TaggedSortedStrictMap.from[ScriptHash, Script.PlutusV1](scripts.collect {
                case s: Script.PlutusV1 => s
            })
        val plutusV2Scripts =
            TaggedSortedStrictMap.from[ScriptHash, Script.PlutusV2](scripts.collect {
                case s: Script.PlutusV2 => s
            })
        val plutusV3Scripts =
            TaggedSortedStrictMap.from[ScriptHash, Script.PlutusV3](scripts.collect {
                case s: Script.PlutusV3 => s
            })

        TransactionWitnessSet(
          vkeyWitnesses = TaggedSortedSet.from(vkeyWitnesses),
          nativeScripts = nativeScripts,
          plutusV1Scripts = plutusV1Scripts,
          plutusV2Scripts = plutusV2Scripts,
          plutusV3Scripts = plutusV3Scripts,
          plutusData = KeepRaw(TaggedSortedMap(plutusData.map(KeepRaw(_))*)),
          redeemers = Some(KeepRaw(redeemers))
        )
    }

    /** CBOR encoder for TransactionWitnessSet */
    given Encoder[TransactionWitnessSet] with
        def write(w: Writer, value: TransactionWitnessSet): Writer = {
            // Count the number of fields to write
            var mapSize = 0

            if value.vkeyWitnesses.toSet.nonEmpty then mapSize += 1
            if value.nativeScripts.toMap.nonEmpty then mapSize += 1
            if value.bootstrapWitnesses.toSet.nonEmpty then mapSize += 1
            if value.plutusV1Scripts.toMap.nonEmpty then mapSize += 1
            if value.plutusData.value.toMap.nonEmpty then mapSize += 1
            if value.redeemers.isDefined then mapSize += 1
            if value.plutusV2Scripts.toMap.nonEmpty then mapSize += 1
            if value.plutusV3Scripts.toMap.nonEmpty then mapSize += 1

            w.writeMapHeader(mapSize)

            // VKey witnesses (key 0)
            if value.vkeyWitnesses.toSet.nonEmpty then
                w.writeInt(0)
                w.write(value.vkeyWitnesses)

            // Native scripts (key 1)
            if value.nativeScripts.toMap.nonEmpty then
                w.writeInt(1)
                w.write(value.nativeScripts)

            // Bootstrap witnesses (key 2)
            if value.bootstrapWitnesses.toSet.nonEmpty then
                w.writeInt(2)
                w.write(value.bootstrapWitnesses)

            // Plutus V1 scripts (key 3)
            if value.plutusV1Scripts.toMap.nonEmpty then
                w.writeInt(3)
                w.write(value.plutusV1Scripts)

            // Plutus data (key 4)
            if value.plutusData.value.toMap.nonEmpty then
                w.writeInt(4)
                // TODO: handle KeepRaw properly when this is implemented: https://github.com/sirthias/borer/issues/764
                w.write(value.plutusData)

            // Redeemers (key 5)
            value.redeemers.foreach { redeemers =>
                w.writeInt(5)
                w.write(redeemers)
            }

            // Plutus V2 scripts (key 6)
            if value.plutusV2Scripts.toMap.nonEmpty then
                w.writeInt(6)
                w.write(value.plutusV2Scripts)

            // Plutus V3 scripts (key 7)
            if value.plutusV3Scripts.toMap.nonEmpty then
                w.writeInt(7)
                w.write(value.plutusV3Scripts)

            w
        }

    /** CBOR decoder for TransactionWitnessSet */
    given decoder(using OriginalCborByteArray, ProtocolVersion): Decoder[TransactionWitnessSet] with
        def read(r: Reader): TransactionWitnessSet = {
            val mapSize = r.readMapHeader()

            var vkeyWitnesses = TaggedSortedSet.empty[VKeyWitness]
            var nativeScripts = TaggedSortedMap.empty[ScriptHash, Script.Native]
            var bootstrapWitnesses = TaggedSortedSet.empty[BootstrapWitness]
            var plutusV1Scripts = TaggedSortedStrictMap.empty[ScriptHash, Script.PlutusV1]
            var plutusData = KeepRaw(TaggedSortedMap.empty[DataHash, KeepRaw[Data]])
            var redeemers: Option[KeepRaw[Redeemers]] = None
            var plutusV2Scripts = TaggedSortedStrictMap.empty[ScriptHash, Script.PlutusV2]
            var plutusV3Scripts = TaggedSortedStrictMap.empty[ScriptHash, Script.PlutusV3]

            for _ <- 0L until mapSize do
                val key = r.readInt()

                key match
                    case 0 => // VKey witnesses
                        vkeyWitnesses = r.read()

                    case 1 => // Native scripts
                        nativeScripts = r.read()

                    case 2 => // Bootstrap witnesses
                        bootstrapWitnesses = r.read()

                    case 3 => // Plutus V1 scripts
                        plutusV1Scripts = r.read()

                    case 4 => // Plutus data
                        plutusData = r.read[KeepRaw[TaggedSortedMap[DataHash, KeepRaw[Data]]]]()

                    case 5 => // Redeemers
                        redeemers = Some(r.read[KeepRaw[Redeemers]]())

                    case 6 => // Plutus V2 scripts
                        plutusV2Scripts = r.read()

                    case 7 => // Plutus V3 scripts
                        plutusV3Scripts = r.read()

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
        }

    given TaggedSortedMap.KeyOf[DataHash, KeepRaw[Data]] = _.dataHash
    given TaggedSortedMap.KeyOf[ScriptHash, Script.Native] = _.scriptHash
    given TaggedSortedStrictMap.KeyOf[ScriptHash, Script.PlutusV1] = _.scriptHash
    given TaggedSortedStrictMap.KeyOf[ScriptHash, Script.PlutusV2] = _.scriptHash
    given TaggedSortedStrictMap.KeyOf[ScriptHash, Script.PlutusV3] = _.scriptHash
