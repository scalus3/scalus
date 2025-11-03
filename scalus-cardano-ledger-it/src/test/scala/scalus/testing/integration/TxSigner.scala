package scalus.testing.integration

import scalus.cardano.ledger.Transaction

trait TxSigner:
    def signTx(unsigned: Transaction): Transaction
