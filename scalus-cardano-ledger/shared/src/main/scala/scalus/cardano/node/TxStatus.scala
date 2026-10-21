package scalus.cardano.node

import scalus.cardano.node.stream.ChainPoint

/** Where a submitted tx stands, spec section 12. Richer than [[TransactionStatus]], which has no
  * payload and stays as it is.
  */
enum TxStatus {

    /** In the mempool. */
    case Pending

    /** In a block on the selected chain. `depth` is the depth of that block at the moment of the
      * call, so the tip has depth 1, spec [TS-2].
      */
    case Confirmed(depth: Int, point: ChainPoint)

    /** Removed from the mempool by revalidation, spec [RV-3]. */
    case Dropped(reason: DropReason)

    /** In a block that left the selected chain, under [[OrphanTxPolicy.Drop]], spec [OR-4]. */
    case RolledBack

    /** Never seen. */
    case NotFound
}

/** Why revalidation removed a tx from the mempool, spec [RV-4] .. [RV-6]. */
enum DropReason {

    /** The validity interval rule failed: the tx expired, spec [RV-4]. */
    case Expired

    /** The inputs rule failed: an input is spent or was removed, spec [RV-5]. */
    case InputsSpent

    /** Any other rule failed, spec [RV-6]. */
    case Invalid(error: SubmitError)
}

object DropReason {

    /** The drop reason for the error of the rule that failed, spec [RV-4] .. [RV-6]. */
    private[node] def of(error: SubmitError): DropReason = error match
        case _: NodeSubmitError.TransactionExpired => Expired
        case _: NodeSubmitError.UtxoNotAvailable   => InputsSpent
        case other                                 => Invalid(other)
}
