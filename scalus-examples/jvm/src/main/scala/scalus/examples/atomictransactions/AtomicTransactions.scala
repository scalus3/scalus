package scalus.examples.atomictransactions

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.uplc.builtin.ByteString

/** Illustrates Cardano's native transaction atomicity, as stated in a respective rosetta contract
  * spec.
  *
  * On EVM chains, atomicity requires a smart contract to batch sub-calls and roll back on failure.
  * On Cardano, every transaction is atomic by the ledger rules: all inputs are consumed and all
  * outputs are created in one step, or nothing changes at all.
  */
object AtomicTransactions {
    private val env: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("aa" * 32))

    private val aliceAddress: Address = ???
    private val bobAddress: Address = ???

    // The ledger rejects the transaction if this UTxO is already spent.
    val utxo1: (TransactionInput, TransactionOutput) =
        TransactionInput(genesisHash, 0) ->
            TransactionOutput(aliceAddress, Value.ada(3L))

    // Same guarantee -- both inputs are validated together, atomically.
    val utxo2: (TransactionInput, TransactionOutput) =
        TransactionInput(genesisHash, 1) ->
            TransactionOutput(aliceAddress, Value.lovelace(7L))

    val txBuilder: TxBuilder =
        TxBuilder(env)
            // _Batching_ doesn't take any additional effort and happens by way of spending several UTxOs at once.
            .spend(Utxo(utxo1))
            .spend(Utxo(utxo2))
            .payTo(bobAddress, Value.lovelace(9L))
}
