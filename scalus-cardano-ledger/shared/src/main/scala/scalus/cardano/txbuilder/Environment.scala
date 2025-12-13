package scalus.cardano.txbuilder

import scalus.cardano.ledger.*

@deprecated("Use CardanoInfo directly instead", "0.13.0")
type Environment = CardanoInfo

@deprecated("Use CardanoInfo directly instead", "0.13.0")
object Environment {
    def apply(cardanoInfo: CardanoInfo): CardanoInfo = cardanoInfo
}
