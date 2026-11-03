package scalus.cardano.ledger
package rules

// It's Conway.validateRefScriptSize of the Conway LEDGER rule in cardano-ledger
object TxRefScriptsSizeValidator extends STS.Validator {
    override final type Error = TransactionException.TxRefScriptsSizeTooBigException

    /** `ppMaxRefScriptSizePerTxG` in cardano-ledger, a constant in Conway. Dijkstra (protocol
      * version 12) makes it the protocol parameter `maxRefScriptSizePerTx`.
      */
    private val maxRefScriptSizePerTx = 200 * 1024

    /** Rejects a tx whose reference scripts total more than 200 KiB, spec [SC-24]. Conway LEDGER
      * runs this check only when `isValid` is true.
      *
      * The total is `txNonDistinctRefScriptsSize`: every UTxO in the union of inputs and reference
      * inputs adds the size of its reference script, so a script carried by two UTxOs counts twice.
      * An input missing from the UTxO adds nothing.
      */
    override def validate(context: Context, state: State, event: Event): Result =
        if event.isValid then check(event, state.utxos) else success

    /** The check of [[validate]] for any tx, `isValid` or not. The node's mempool measure applies
      * it to every tx it admits (`txMeasureRefScripts` in ouroboros-consensus,
      * Shelley/Ledger/Mempool.hs), so a tx with `isValid = false` over the limit, which LEDGER lets
      * through, never reaches a block. The Emulator's `submit` does the same, spec [BB-1].
      */
    private[cardano] def check(tx: Transaction, utxos: Utxos): Result = {
        val size = txNonDistinctRefScriptsSize(tx, utxos)
        if size <= maxRefScriptSizePerTx then success
        else
            failure(
              TransactionException.TxRefScriptsSizeTooBigException(
                tx.id,
                supplied = size,
                expected = maxRefScriptSizePerTx
              )
            )
    }

    /** `txNonDistinctRefScriptsSize` in cardano-ledger (C/UTxO.hs:171-175): the sizes of the
      * reference scripts of the union of the inputs and the reference inputs of `tx`, a script
      * carried by two UTxOs counted twice. An input missing from `utxos` adds nothing.
      */
    private[cardano] def txNonDistinctRefScriptsSize(tx: Transaction, utxos: Utxos): Int =
        utils.AllProvidedReferenceScripts.nonDistinctReferenceScripts(tx, utxos).map(scriptSize).sum

    /** `originalBytesSize` of a script in cardano-ledger: the original CBOR of a timelock, or the
      * bytes of a Plutus script.
      */
    private[ledger] def scriptSize(script: Script): Int = script match
        case s: Script.Native => s.binaryScript.raw.length
        case s: PlutusScript  => s.script.size
}
