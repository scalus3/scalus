package scalus.cardano.txbuilder

import scalus.cardano.address.Address
import scalus.cardano.ledger.{Utxo, Utxos, Value}

/** Owns a set of pubkey-controlled UTXOs. */
trait Wallet {

    def selectInputs(required: Value): Option[Seq[(Utxo, Witness)]]

    def utxo: Utxos

    def collateralInputs: Seq[(Utxo, Witness)]

    def owner: Address
}

object Wallet {

    def empty(changeAddr: Address) = new Wallet {

        override def utxo: Utxos = Map.empty

        override def collateralInputs: Seq[(Utxo, Witness)] = Seq.empty

        override def owner: Address = changeAddr

        override def selectInputs(
            required: Value
        ): Option[Seq[(Utxo, Witness)]] = None
    }
}
