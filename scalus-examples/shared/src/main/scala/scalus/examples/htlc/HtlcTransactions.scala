package scalus.examples.htlc

import scalus.uplc.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.uplc.PlutusV3

import java.time.Instant

case class HtlcTransactions(
    env: CardanoInfo,
    contract: PlutusV3[Data => Unit]
) {
    private val script: Script.PlutusV3 = contract.script
    private val scriptAddress: Address = contract.address(env.network)

    private val builder = TxBuilder(env)

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

        builder
            .payTo(scriptAddress, value, datum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
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

        builder
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

        builder
            .spend(lockedUtxo, redeemer, script, Set(committerPkh))
            .payTo(payeeAddress, lockedUtxo.output.value)
            .validFrom(validFrom)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }
}
