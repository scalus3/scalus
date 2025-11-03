package scalus.testing.integration

import scalus.cardano.ledger.ProtocolParams

trait ProtocolParamFetcher:
    def fetchLatestParams(): ProtocolParams
    def fetchParamsOfEpoch(epoch: Long): ProtocolParams
