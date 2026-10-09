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
    override def validate(context: Context, state: State, event: Event): Result = {
        if !event.isValid then return success
        val size = utils.AllProvidedReferenceScripts
            .nonDistinctReferenceScripts(event, state.utxos)
            .map(scriptSize)
            .sum
        if size <= maxRefScriptSizePerTx then success
        else
            failure(
              TransactionException.TxRefScriptsSizeTooBigException(
                event.id,
                supplied = size,
                expected = maxRefScriptSizePerTx
              )
            )
    }

    /** `originalBytesSize` of a script in cardano-ledger: the CBOR of a timelock, or the bytes of a
      * Plutus script.
      */
    private[ledger] def scriptSize(script: Script): Int = script match
        case s: Script.Native => s.script.toCbor.length
        case s: PlutusScript  => s.script.size
}
