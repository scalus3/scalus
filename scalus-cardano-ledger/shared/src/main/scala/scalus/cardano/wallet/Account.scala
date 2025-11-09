package scalus.cardano.wallet

import scalus.cardano.address.Address

trait Account {
    type KeyPair
    def address(index: Int): Address
    def keyPair(index: Int): KeyPair
}
