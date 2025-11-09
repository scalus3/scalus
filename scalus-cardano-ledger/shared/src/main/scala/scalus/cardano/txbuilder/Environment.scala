package scalus.cardano.txbuilder

import scalus.cardano.ledger.*

type Environment = CardanoInfo

object Environment {
    def apply(cardanoInfo: CardanoInfo): Environment = cardanoInfo
}
