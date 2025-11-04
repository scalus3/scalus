package scalus.testing.integration

import scalus.cardano.ledger.Transaction

trait TxSubmitter:
    type Error
    def submit(tx: Transaction): Either[Error, Unit]
