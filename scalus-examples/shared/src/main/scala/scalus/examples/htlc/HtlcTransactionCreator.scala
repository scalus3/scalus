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
        sponsor: Address,
        committer: AddrKeyHash,
        receiver: AddrKeyHash,
        image: Image,
        timeout: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val datum = Config(PubKeyHash(committer), PubKeyHash(receiver), image, timeout.toEpochMilli)

        TxBuilder(env, evaluator)
            .payTo(scriptAddress, value, datum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
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
        timeout: Instant,
        provider: Provider,
        signer: TransactionSigner
    )(using ExecutionContext): Future[Transaction] = {
        val datum = Config(PubKeyHash(committer), PubKeyHash(receiver), image, timeout.toEpochMilli)

        TxBuilder(env)
            .payTo(scriptAddress, value, datum)
            .complete(provider, sponsor)
            .map(_.sign(signer).transaction)
    }

    def reveal(
        utxos: Utxos,
        lockedUtxo: Utxo,
        payeeAddress: Address,
        sponsor: Address,
        preimage: Preimage,
        receiverPkh: AddrKeyHash,
        validTo: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.Reveal(preimage)

        TxBuilder(env, evaluator)
            .spend(lockedUtxo, redeemer, script, Set(receiverPkh))
            .payTo(payeeAddress, lockedUtxo.output.value)
            .validTo(validTo)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    def timeout(
        utxos: Utxos,
        lockedUtxo: Utxo,
        payeeAddress: Address,
        sponsor: Address,
        committerPkh: AddrKeyHash,
        validFrom: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.Timeout

        TxBuilder(env, evaluator)
            .spend(lockedUtxo, redeemer, script, Set(committerPkh))
            .payTo(payeeAddress, lockedUtxo.output.value)
            .validFrom(validFrom)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }
}
