package scalus.examples.htlc

import scalus.cardano.address.Address
import scalus.cardano.blueprint.PlutusV3CompiledContract
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.PubKeyHash

case class HtlcTransactionCreator(
    env: Environment,
    evaluator: PlutusScriptEvaluator,
    signer: TransactionSigner,
    compiledContract: PlutusV3CompiledContract = HtlcContract.defaultCompiledContract
) {
    def script: Script.PlutusV3 = compiledContract.script
    val scriptAddress: Address = compiledContract.address(env.network)

    def lock(
        utxos: Utxos,
        value: Value,
        changeAddress: Address,
        committer: AddrKeyHash,
        receiver: AddrKeyHash,
        image: Image,
        timeout: Long
    ): Either[Throwable, Transaction] = {
        val datum = Config(PubKeyHash(committer), PubKeyHash(receiver), image, timeout)

        for {
            signedBuilder <- TxBuilder
                .withCustomEvaluator(env, evaluator)
                .spend(utxos)
                .payTo(scriptAddress, value, datum)
                .changeTo(changeAddress)
                .build()
                .sign(signer)
        } yield signedBuilder.transaction
    }

    def reveal(
        utxos: Utxos,
        collateralUtxos: Utxos,
        lockedUtxo: Utxo,
        payeeAddress: Address,
        changeAddress: Address,
        preimage: Preimage,
        receiverPkh: AddrKeyHash,
        time: Long
    ): Either[Throwable, Transaction] = {
        val redeemer = Action.Reveal(preimage)

        for {
            signedBuilder <- TxBuilder
                .withCustomEvaluator(env, evaluator)
                .spend(utxos)
                .collaterals(collateralUtxos)
                .spend(lockedUtxo, redeemer, script, Set(receiverPkh))
                .payTo(payeeAddress, lockedUtxo.output.value)
                .validFrom(java.time.Instant.ofEpochMilli(time))
                .changeTo(changeAddress)
                .build()
                .sign(signer)
        } yield signedBuilder.transaction
    }

    def timeout(
        utxos: Utxos,
        collateralUtxos: Utxos,
        lockedUtxo: Utxo,
        payeeAddress: Address,
        changeAddress: Address,
        committerPkh: AddrKeyHash,
        time: Long
    ): Either[Throwable, Transaction] = {
        val redeemer = Action.Timeout

        for {
            signedBuilder <- TxBuilder
                .withCustomEvaluator(env, evaluator)
                .spend(utxos)
                .collaterals(collateralUtxos)
                .spend(lockedUtxo, redeemer, script, Set(committerPkh))
                .payTo(payeeAddress, lockedUtxo.output.value)
                .validFrom(java.time.Instant.ofEpochMilli(time))
                .changeTo(changeAddress)
                .build()
                .sign(signer)
        } yield signedBuilder.transaction
    }
}
