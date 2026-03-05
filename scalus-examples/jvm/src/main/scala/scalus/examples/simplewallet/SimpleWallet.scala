package scalus.examples.simplewallet

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.uplc.builtin.ByteString

/** Illustrates Cardano's native simple wallet, as stated in a respective rosetta contract spec.
  *
  * On EVM chains, a SimpleWallet contract could be used to hold funds, queue transactions, and
  * authorize withdrawals. On Cardano, a plain pubkey address covers these requirements out of the
  * box: the owner's signature authorizes every spend, transactions are constructed and submitted
  * directly (no on-chain queue needed), and the full balance can be withdrawn at any time, which
  * would equate to spending every UTxO at an address.
  */
object SimpleWallet {
    private val env: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("aa" * 32))

    // The owner's address acts as the wallet. No contract needed to hold or guard the funds.
    private val ownerAddress: Address = ???
    private val recipientAddress: Address = ???

    // Funds held in the wallet are ordinary UTxOs at the owner's address.
    val walletUtxo: (TransactionInput, TransactionOutput) =
        TransactionInput(genesisHash, 0) ->
            TransactionOutput(ownerAddress, Value.ada(10L))

    // deposit: send ADA to the owner's address -- just a regular payment transaction.
    val deposit =
        TxBuilder(env)
            .spend(Utxo(walletUtxo))
            .payTo(ownerAddress, Value.ada(5L))
            .build(changeTo = walletUtxo._2.address) // back to the wallet

    // createTransaction + executeTransaction: on Cardano these collapse into one step.
    // The transaction is fully specified off-chain and submitted directly.
    val transfer =
        TxBuilder(env)
            .spend(Utxo(walletUtxo))
            .payTo(recipientAddress, Value.ada(3L))
            .build(changeTo = walletUtxo._2.address)

    // withdraw: spend all UTxOs at the owner's address. The owner's signature is the only
    // authorization required — no contract withdrawal function needed.
    val withdraw =
        TxBuilder(env)
            .spend(Utxo(walletUtxo))
        // and spend the utxo however necessary
}
