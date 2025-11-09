package scalus.cardano.wallet

trait KeyPair {
    type Underlying
    def underlying: Underlying
    def publicKeyBytes: Array[Byte]
    def privateKeyBytes: Array[Byte]
    def sign(message: Array[Byte]): Array[Byte]
}

trait Account {
    def paymentKeyPair: KeyPair
    def changeKeyPair: KeyPair
    def stakeKeyPair: KeyPair
    def drepKeyPair: KeyPair
}
