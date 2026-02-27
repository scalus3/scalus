package scalus.examples.setbench

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data

/** Transaction builder for SetBench contract operations.
  *
  * Uses a standard evaluator (not constMaxBudget) so that real execution costs are computed during
  * building. This works because the validator uses `>=` for the lovelace check, not `===`, so
  * iterative balancing converges without circular fee dependency.
  */
case class SetBenchTransactions(env: CardanoInfo) {
    private val builder = TxBuilder(env)

    /** Publish a script as a reference UTxO so subsequent txs can reference it instead of including
      * it in the witness set.
      *
      * The ref script UTxO is sent to `holder` (not the sponsor) so that coin selection in
      * subsequent transactions won't accidentally consume it.
      */
    def publishScript(
        utxos: Utxos,
        contract: PlutusV3[Data => Unit],
        holder: Address,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val scriptOutput = TransactionOutput(
          holder,
          Value.lovelace(10_000_000L),
          None,
          Some(ScriptRef(contract.script))
        )
        builder
            .output(scriptOutput)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    def lock(
        utxos: Utxos,
        contract: PlutusV3[Data => Unit],
        totalLovelace: Long,
        initialRoot: scalus.uplc.builtin.ByteString,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val datum = SetBenchDatum(BigInt(totalLovelace), initialRoot)
        builder
            .payTo(contract.address(env.network), Value.lovelace(totalLovelace), datum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    def withdraw(
        utxos: Utxos,
        contractUtxo: Utxo,
        refScriptUtxo: Utxo,
        contract: PlutusV3[Data => Unit],
        redeemer: Data,
        newDatum: SetBenchDatum,
        k: Long,
        withdrawTo: Address,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val newLovelace = contractUtxo.output.value.coin.value - k
        builder
            .references(refScriptUtxo, contract)
            .spend(contractUtxo, redeemer)
            .payTo(withdrawTo, Value.lovelace(k))
            .payTo(contract.address(env.network), Value.lovelace(newLovelace), newDatum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    def deposit(
        utxos: Utxos,
        contractUtxo: Utxo,
        refScriptUtxo: Utxo,
        contract: PlutusV3[Data => Unit],
        redeemer: Data,
        newDatum: SetBenchDatum,
        k: Long,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val newLovelace = contractUtxo.output.value.coin.value + k
        builder
            .references(refScriptUtxo, contract)
            .spend(contractUtxo, redeemer)
            .payTo(contract.address(env.network), Value.lovelace(newLovelace), newDatum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }
}
