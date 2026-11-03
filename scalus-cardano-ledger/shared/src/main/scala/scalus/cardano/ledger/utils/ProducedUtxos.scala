package scalus.cardano.ledger
package utils

/** The UTxOs a tx adds to the UTxO set, spec [BB-3], [BB-4]: `txouts` for a valid tx, `collOuts`
  * for a tx with `isValid = false` (libs/cardano-ledger-core/src/Cardano/Ledger/State/UTxO.hs:139,
  * eras/babbage/impl/src/Cardano/Ledger/Babbage/Collateral.hs:43-55). The UTXOS transition and the
  * block reference-script size of BBODY both add them.
  */
private[cardano] object ProducedUtxos {

    def of(tx: Transaction): Utxos =
        if tx.isValid then tx.utxos else collateralReturn(tx)

    /** `collOuts`: the collateral return at the index after the outputs, or, "in the impossible
      * event that there are more transaction outputs in the transaction than will fit into a
      * Word16", at its `maxBound`.
      */
    private def collateralReturn(tx: Transaction): Utxos = {
        val body = tx.body.value
        body.collateralReturnOutput
            .map(output => Input(tx.id, body.outputs.size.min(65535)) -> output.value)
            .toMap
    }
}
