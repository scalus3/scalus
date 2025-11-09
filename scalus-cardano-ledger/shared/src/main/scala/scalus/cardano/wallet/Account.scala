package scalus.cardano.wallet

import scalus.builtin.{platform, ByteString}

trait KeyPair {
    type Underlying
    def underlying: Underlying
    def publicKeyBytes: Array[Byte]
    def privateKeyBytes: Array[Byte]
    def sign(message: Array[Byte]): Array[Byte] = {
        platform
            .signEd25519(
              ByteString.fromArray(privateKeyBytes),
              ByteString.fromArray(message)
            )
            .bytes
    }
    def verify(message: Array[Byte], signature: Array[Byte]): Boolean = {
        platform.verifyEd25519Signature(
          ByteString.fromArray(publicKeyBytes),
          ByteString.fromArray(message),
          ByteString.fromArray(signature)
        )
    }
}

trait Account {
    def paymentKeyPair: KeyPair
    def changeKeyPair: KeyPair
    def stakeKeyPair: KeyPair
    def drepKeyPair: KeyPair
}
