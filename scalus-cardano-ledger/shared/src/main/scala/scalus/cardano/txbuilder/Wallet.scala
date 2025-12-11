package scalus.cardano.txbuilder

import scalus.cardano.address.Address
import scalus.cardano.ledger.{Utxo, Utxos, Value}

/** Owns a set of pubkey-controlled UTXOs. */
@deprecated("Will be removed", "0.13.0")
trait Wallet {

    def selectInputs(required: Value): Option[Seq[(Utxo, Witness)]]

    def utxo: Utxos

    def collateralInputs: Seq[(Utxo, Witness)]

    def owner: Address
}

@deprecated("Will be removed", "0.13.0")
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
