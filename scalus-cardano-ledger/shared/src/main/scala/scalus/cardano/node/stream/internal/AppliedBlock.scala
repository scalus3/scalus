package scalus.cardano.node.stream.internal

import scalus.cardano.ledger.{Block, Transaction, Utxos}
import scalus.cardano.node.stream.{BlockNo, ChainPoint}

/** One applied transaction, with its UTxO effects already resolved.
  *
  * `spent` carries the *resolved* consumed outputs, not just the inputs: a subscriber watching an
  * address needs to know which of its UTxOs disappeared, and the input alone does not say. Every
  * provider can resolve this — the emulator from its ledger, a chain-following provider from the
  * block it just fetched — so resolving once at the source beats every subscriber doing it.
  */
case class AppliedTransaction(tx: Transaction, created: Utxos, spent: Utxos) {
    def txHash: scalus.cardano.ledger.TransactionHash = tx.id
}

/** A block as the subscription hub consumes it.
  *
  * `block` is the raw ledger block when the provider has one. Providers that synthesise blocks — an
  * emulator applying one transaction at a time — have nothing truthful to put here, which is why it
  * is optional and why such providers declare that they do not serve `Block` subscriptions rather
  * than fabricating a header.
  */
case class AppliedBlock(
    point: ChainPoint,
    blockNo: BlockNo,
    txs: Seq[AppliedTransaction],
    block: Option[Block] = None
)
