package scalus.cardano.node

import scalus.cardano.ledger.ProtocolParams

trait ProtocolParamFetcher:
    def fetchLatestParams(): ProtocolParams
    def fetchParamsOfEpoch(epoch: Long): ProtocolParams
