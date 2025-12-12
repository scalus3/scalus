package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.{CardanoInfo, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.wallet.LucidAccount

class TxBuilderJsTest extends AnyFunSuite {

    private val mnemonic = "test " * 23 + "sauce"
    private val derivationPath = "m/1852'/1815'/0'/0/0"
    private val cardanoInfo = CardanoInfo.mainnet

    private val account = LucidAccount(cardanoInfo.network, mnemonic, derivationPath)

    private val alice = Address.fromBech32(account.baseAddress)
    // arbitrary test address
    private val bob: Address =
        Address.fromBech32(
          "addr1qynntmjxqafgf4mpwhu5q5cu2g2zqwrfw60t4cgkk93k4cmafkn40uny60sfaeczvqrgc7h54329supn6pndsgey79yquafyhe"
        )

    private val aliceUtxo: Utxo =
        Utxo(
          TransactionInput(TransactionHash.fromHex("0" * 64), 0) -> TransactionOutput(
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
          tx.id.bytes,
          witness.signature.bytes
        )
        assert(isValid, "Signature must be cryptographically valid")

    }
}
