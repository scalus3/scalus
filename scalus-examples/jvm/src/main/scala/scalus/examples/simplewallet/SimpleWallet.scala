package scalus.examples.simplewallet

import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
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

    // deposit: send ADA to the owner's address — just a regular payment transaction.
    val deposit =
        TxBuilder(env)
            .spend(Utxo(walletUtxo))
            .payTo(ownerAddress, Value.ada(5L))
            .build(changeTo = walletUtxo._2.address)

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
}

/** Going beyond the spec: a 2-of-3 multisig wallet using a Cardano native script.
  *
  * No Plutus contract is needed. A [[Timelock.MOf]] script defines the spending policy; any two of
  * the three owners must sign to authorize a transaction.
  */
object MultiSigWallet {
    private val env: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("aa" * 32))

    private val owner1: AddrKeyHash = ???
    private val owner2: AddrKeyHash = ???
    private val owner3: AddrKeyHash = ???

    // 2-of-3 native script: any two owners must sign.
    val policy: Script.Native = Script.Native(
      Timelock.MOf(
        2,
        IndexedSeq(
          Timelock.Signature(owner1),
          Timelock.Signature(owner2),
          Timelock.Signature(owner3)
        )
      )
    )

    // The wallet address is derived from the script hash — no Plutus execution, just signature checks.
    val walletAddress: Address = ShelleyAddress(
      env.network,
      ShelleyPaymentPart.Script(policy.scriptHash),
      ShelleyDelegationPart.Null
    )

    val walletUtxo: (TransactionInput, TransactionOutput) =
        TransactionInput(genesisHash, 0) ->
            TransactionOutput(walletAddress, Value.ada(10L))

    private val recipientAddress: Address = ???

    // Spending requires the native script witness plus signatures from any two owners.
    val transfer =
        TxBuilder(env)
            .spend(Utxo(walletUtxo), NativeScriptWitness.attached(policy))
            .requireSignature(owner1)
            .requireSignature(owner2)
            .payTo(recipientAddress, Value.ada(3L))
            .build(changeTo = walletAddress)
}
