package scalus.utxocells

import scalus.cardano.onchain.plutus.v1.Value

/** Off-chain action encodings — describe what to do with a UtxoCell. */
sealed trait ActionEncoding[+A]

object ActionEncoding {

    /** Mint a new cell (init action). */
    case class MintAction[A](action: A) extends ActionEncoding[A]

    /** Spend an existing cell (transition action). */
    case class SpendAction[A](action: A, value: Value) extends ActionEncoding[A]
}
