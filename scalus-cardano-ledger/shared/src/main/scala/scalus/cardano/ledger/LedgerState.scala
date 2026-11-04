package scalus.cardano.ledger

/** The ledger state of the tx rules, the node's `LedgerState`
  * (eras/shelley/impl/src/Cardano/Ledger/Shelley/LedgerState/Types.hs:453-458). A read result: the
  * Emulator builds it, a caller reads it, ledger-rules spec D122.
  *
  * @param utxoState
  *   the UTxO set, the deposits, the fees, the gov state, the instant stake and the donations,
  *   `lsUTxOState`
  * @param certState
  *   the DReps, the pools and the accounts, `lsCertState`
  */
final case class LedgerState private[scalus] (utxoState: UTxOState, certState: CertState)
