package scalus.examples.htlc

import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.Provider
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.PubKeyHash
import scalus.uplc.PlutusV3

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class HtlcTransactionCreator(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    contract: PlutusV3[Data => Unit]
) {
    def script: Script.PlutusV3 = contract.script
    val scriptAddress: Address = contract.address(env.network)

    def lock(
        utxos: Utxos,
        value: Value,
        changeAddress: Address,
        committer: AddrKeyHash,
        receiver: AddrKeyHash,
        image: Image,
        timeout: Long,
        signer: TransactionSigner
    ): Transaction = {
        val datum = Config(PubKeyHash(committer), PubKeyHash(receiver), image, timeout)

        TxBuilder(env, evaluator)
            .payTo(scriptAddress, value, datum)
            .complete(availableUtxos = utxos, sponsor = changeAddress)
            .sign(signer)
            .transaction
    }

    /** Async version of lock that uses [[scalus.cardano.txbuilder.TxBuilder.complete]] for
      * cross-platform support.
      *
      * This method works on both JVM and JavaScript platforms. Returns the change to the sponsor.
      */
    def lockAsync(
        value: Value,
        sponsor: Address,
        committer: AddrKeyHash,
        receiver: AddrKeyHash,
        image: Image,
        timeout: Long,
        provider: Provider,
        signer: TransactionSigner
    )(using ExecutionContext): Future[Transaction] = {
        val datum = Config(PubKeyHash(committer), PubKeyHash(receiver), image, timeout)

        TxBuilder(env)
            .payTo(scriptAddress, value, datum)
            .complete(provider, sponsor)
            .map(_.sign(signer).transaction)
    }

    def reveal(
        utxos: Utxos,
        lockedUtxo: Utxo,
        payeeAddress: Address,
        changeAddress: Address,
        preimage: Preimage,
        receiverPkh: AddrKeyHash,
        time: SlotNo,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.Reveal(preimage)

        TxBuilder(env, evaluator)
            .spend(lockedUtxo, redeemer, script, Set(receiverPkh))
            .payTo(payeeAddress, lockedUtxo.output.value)
            .validTo(Instant.ofEpochMilli(time))
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    def timeout(
        utxos: Utxos,
        lockedUtxo: Utxo,
        payeeAddress: Address,
        changeAddress: Address,
        committerPkh: AddrKeyHash,
        time: Long,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.Timeout

        TxBuilder(env, evaluator)
            .spend(lockedUtxo, redeemer, script, Set(committerPkh))
            .payTo(payeeAddress, lockedUtxo.output.value)
            .validFrom(Instant.ofEpochMilli(time))
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }
}
