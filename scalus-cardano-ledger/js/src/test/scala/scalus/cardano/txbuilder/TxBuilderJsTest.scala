package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address.addr
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.{Ed25519Signer, JsEd25519Signer, Signature}

class TxBuilderJsTest extends AnyFunSuite {
    // Use JS Ed25519 signer for key derivation
    private given Ed25519Signer = JsEd25519Signer

    private val mnemonic = "test " * 23 + "sauce"
    private val cardanoInfo = CardanoInfo.mainnet

    private val account = HdAccount.fromMnemonic(mnemonic)

    private val alice = account.baseAddress(Network.Mainnet)
    // arbitrary test address
    private val bob: Address =
        addr"addr1qynntmjxqafgf4mpwhu5q5cu2g2zqwrfw60t4cgkk93k4cmafkn40uny60sfaeczvqrgc7h54329supn6pndsgey79yquafyhe"

    private val aliceUtxo: Utxo =
        Utxo(
          Input(TransactionHash.fromHex("0" * 64), 0) -> Output(
            alice,
            Value.ada(100)
          )
        )

    test("signs the transaction") {
        val payment: Value = Value.ada(10)
        val tx = TxBuilder(cardanoInfo)
            .payTo(bob, payment)
            .spend(aliceUtxo)
            .build(changeTo = alice)
            .sign(account.signerForUtxos)
            .transaction

        val witness = tx.witnessSet.vkeyWitnesses.toSeq.head
        val isValid = account.paymentKeyPair.verify(
          tx.id,
          Signature.unsafeFromByteString(witness.signature)
        )
        assert(isValid, "Signature must be cryptographically valid")

    }
}
