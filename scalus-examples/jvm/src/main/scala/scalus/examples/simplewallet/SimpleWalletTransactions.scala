package scalus.examples.simplewallet

import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{NativeScriptWitness, TransactionSigner, TxBuilder}

/** Cardano's native "simple wallet" (rosetta `simple_wallet`).
  *
  * On EVM chains a SimpleWallet contract holds funds, queues transactions, and authorizes
  * withdrawals. On Cardano a plain pubkey address covers all of this out of the box: the owner's
  * signature authorizes every spend, transactions are built and submitted directly (no on-chain
  * queue needed), and the entire balance can be withdrawn at any time by spending every UTxO at the
  * address. No Plutus contract is required.
  */
case class SimpleWalletTransactions(env: CardanoInfo) {

    /** Pay `amount` to `recipient`, returning change to `owner`. This is the EVM
      * `createTransaction` + `executeTransaction` pair collapsed into a single step — the
      * transaction is fully specified off-chain and submitted directly.
      */
    def transfer(
        ownerUtxos: Utxos,
        recipient: Address,
        amount: Coin,
        owner: Address,
        signer: TransactionSigner
    ): Transaction =
        TxBuilder(env)
            .payTo(recipient, Value(amount))
            .complete(availableUtxos = ownerUtxos, owner)
            .sign(signer)
            .transaction

    /** Withdraw the whole balance: spend every owner UTxO and send it all to `recipient`. The
      * owner's signature is the only authorization — there is no contract withdrawal function.
      */
    def withdrawAll(
        ownerUtxos: Utxos,
        recipient: Address,
        signer: TransactionSigner
    ): Transaction =
        ownerUtxos
            .foldLeft(TxBuilder(env)) { case (builder, entry) => builder.spend(Utxo(entry)) }
            .complete(availableUtxos = ownerUtxos, recipient)
            .sign(signer)
            .transaction
}

/** Going beyond the spec: an m-of-n multisig wallet using a Cardano native script (no Plutus).
  *
  * A [[Timelock.MOf]] script defines the spending policy; any `required` of the `owners` must sign
  * to authorize a transaction. The wallet address is derived from the script hash, so spending is
  * just signature checking — no on-chain execution.
  */
case class MultiSigWallet(env: CardanoInfo, owners: IndexedSeq[AddrKeyHash], required: Int) {

    /** The m-of-n native script: any `required` of the `owners` must sign. */
    val policy: Script.Native =
        Script.Native(Timelock.MOf(required, owners.map(Timelock.Signature(_))))

    /** Wallet address derived from the native-script hash. */
    val address: Address = ShelleyAddress(
      env.network,
      ShelleyPaymentPart.Script(policy.scriptHash),
      ShelleyDelegationPart.Null
    )

    /** Spend `walletUtxo`, paying `amount` to `recipient` with change back to the wallet. Requires
      * the native-script witness plus the supplied signers, which together must cover `required` of
      * the owners.
      */
    def transfer(
        walletUtxo: Utxo,
        recipient: Address,
        amount: Coin,
        requiredSigners: Set[AddrKeyHash],
        signers: Seq[TransactionSigner]
    ): Transaction = {
        val completed = requiredSigners
            .foldLeft(
              TxBuilder(env)
                  .spend(walletUtxo, NativeScriptWitness.attached(policy))
                  .payTo(recipient, Value(amount))
            )((builder, owner) => builder.requireSignature(owner))
            .complete(availableUtxos = Map(walletUtxo.input -> walletUtxo.output), address)
        signers.foldLeft(completed)((tx, signer) => tx.sign(signer)).transaction
    }
}
