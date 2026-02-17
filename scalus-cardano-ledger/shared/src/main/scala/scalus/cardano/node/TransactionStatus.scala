package scalus.cardano.node

/** Status of a transaction on the blockchain.
  *
  * Used by [[BlockchainReaderTF.checkTransaction]] and
  * [[BlockchainProviderTF.pollForConfirmation]].
  */
enum TransactionStatus:
    /** Transaction is confirmed in a block. */
    case Confirmed

    /** Transaction is in the mempool, waiting for a block. */
    case Pending

    /** Transaction not seen on-chain or in mempool. */
    case NotFound
