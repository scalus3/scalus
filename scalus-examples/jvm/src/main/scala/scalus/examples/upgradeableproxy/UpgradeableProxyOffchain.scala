package scalus.examples.upgradeableproxy

import scalus.*
import scalus.cardano.address.{Address, StakeAddress}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data

private object ProxyCompilation:
    private given Options = Options.release
    lazy val contract = PlutusV3.compile(ProxyValidator.validate)

lazy val ProxyContract = ProxyCompilation.contract

case class ProxyTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    contract: PlutusV3[Data => Unit]
) {
    val script: Script.PlutusV3 = contract.script
    val scriptAddress: Address = contract.address(env.network)

    /** Creates the proxy UTxO at the script address with an inline datum pointing to `logicHash`.
      */
    def deploy(
        utxos: Utxos,
        value: Value,
        logicHash: ScriptHash,
        owner: AddrKeyHash,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val datum = ProxyDatum(
          logicHash = logicHash,
          owner = scalus.cardano.onchain.plutus.v3.PubKeyHash(owner)
        )
        TxBuilder(env)
            .payTo(scriptAddress, value, datum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    /** Invokes the proxy by withdrawing from `logicStakeAddress`, triggering the logic script. */
    def call(
        utxos: Utxos,
        proxyUtxo: Utxo,
        logicStakeAddress: StakeAddress,
        logicWitness: ScriptWitness,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val datum = proxyUtxo.output.requireInlineDatum
        TxBuilder(env, evaluator)
            .spend(proxyUtxo, ProxyRedeemer.Call, script)
            .payTo(scriptAddress, proxyUtxo.output.value, datum)
            .withdrawRewards(logicStakeAddress, Coin.zero, logicWitness)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    /** Upgrades the proxy to a new logic stake validator; must be signed by `ownerPkh`. */
    def upgrade(
        utxos: Utxos,
        proxyUtxo: Utxo,
        newLogicHash: ScriptHash,
        ownerPkh: AddrKeyHash,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val oldDatum = proxyUtxo.output.requireInlineDatum.to[ProxyDatum]
        val newDatum = oldDatum.copy(logicHash = newLogicHash)
        TxBuilder(env, evaluator)
            .spend(proxyUtxo, ProxyRedeemer.Upgrade(newLogicHash), script)
            .requireSignature(ownerPkh)
            .payTo(scriptAddress, proxyUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }
}
