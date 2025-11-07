package scalus.cardano.ledger

import scalus.builtin.{platform, ByteString, Data, given}

import scala.collection.immutable.{ListMap, TreeSet}

import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}

object ScriptDataHashGenerator {

    /** Computes the script data hash for the given redeemers, datums, and used cost models.
      *
      * The script data hash is computed as the Blake2b-256 hash of the concatenation of:
      *   - the CBOR encoding of the redeemers (or an empty map if none)
      *   - the CBOR encoding of the datums (or an empty string if none)
      *   - the CBOR encoding of the cost models
      *
      * @note
      *   It's a low-level function that does not check if the redeemers and datums are actually
      *   used in the transaction. Use it if you know what you're doing.
      */
    def computeScriptDataHash(
        redeemers: Option[KeepRaw[Redeemers]],
        datums: KeepRaw[TaggedSortedMap[DataHash, KeepRaw[Data]]],
        usedCostModels: CostModels
    ): ScriptDataHash = {
        /* ; script data format:
         * ; [ redeemers | datums | language views ]
         * ; The redeemers are exactly the data present in the transaction witness set.
         * ; Similarly for the datums, if present. If no datums are provided, the middle
         * ; field is an empty string.
         */
        val redeemerBytes = redeemers match
            case Some(value) => value.raw
            case None        => Array(0xa0.toByte) // Empty map in CBOR
        val plutusDataBytes =
            if datums.value.toMap.isEmpty then Array.empty[Byte]
            else datums.raw
        val costModelsBytes = usedCostModels.getLanguageViewEncoding
        val encodedBytes = redeemerBytes ++ plutusDataBytes ++ costModelsBytes
        Hash(platform.blake2b_256(ByteString.unsafeFromArray(encodedBytes)))
    }

    /** Computes the script data hash for the given transaction witness set, protocol parameters,
      * used languages, redeemers, and datums.
      *
      * If there are no redeemers, no datums, and no cost models, returns None.
      *
      * @note
      *   Use it if you know what you're doing.
      */
    def computeScriptDataHash(
        witnessSet: TransactionWitnessSet,
        protocolParams: ProtocolParams,
        usedLanguages: TreeSet[Language],
        redeemers: Option[KeepRaw[Redeemers]],
        datums: KeepRaw[TaggedSortedMap[DataHash, KeepRaw[Data]]]
    ): Option[ScriptDataHash] = {
        val costModels = CostModels(
          usedLanguages.view
              .map(l => l.ordinal -> protocolParams.costModels.models(l.ordinal))
              .to(ListMap)
        )

        if redeemers.isEmpty && datums.value.toMap.isEmpty && costModels.models.isEmpty then None
        else Some(computeScriptDataHash(redeemers, datums, costModels))
    }

    /** Computes the script data hash for the given transaction and UTxO set.
      *
      * If there are no redeemers, no datums, and no cost models, returns None.
      *
      * @param transaction
      *   The transaction to compute the script data hash for.
      * @param utxos
      *   The UTxO set to use for resolving inputs and reference inputs.
      * @param protocolParams
      *   The protocol parameters to use for getting the cost models.
      * @return
      *   Either a BadInputsUTxOException or BadReferenceInputsUTxOException if some inputs or
      *   reference inputs are missing in the UTxO set, or an Option[ScriptDataHash] if successful.
      */
    def computeScriptDataHash(
        transaction: Transaction,
        utxos: Utxos,
        protocolParams: ProtocolParams
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Option[ScriptDataHash]
    ] = {
        val witnessSet = transaction.witnessSet
        val redeemers = witnessSet.redeemers
        val datums = witnessSet.plutusData

        for
            allNeededScriptHashes <- AllNeededScriptHashes.allNeededScriptHashes(transaction, utxos)

            allResolvedPlutusScripts <- AllResolvedScripts.allResolvedPlutusScriptsView(
              transaction,
              utxos
            )
        yield
            val usedLanguages = TreeSet.from(
              allResolvedPlutusScripts
                  .filter(plutusScript => allNeededScriptHashes.contains(plutusScript.scriptHash))
                  .map(_.language)
            )
            computeScriptDataHash(witnessSet, protocolParams, usedLanguages, redeemers, datums)
    }
}
