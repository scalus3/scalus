package scalus.examples.vesting

import scalus.uplc.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.uplc.PlutusV3

import java.time.Instant

/** Transaction builder for Vesting contract operations. */
case class VestingTransactions(
    env: CardanoInfo,
    contract: PlutusV3[Data => Unit]
) {
    private val scriptAddress: Address = contract.address(env.network)

    private val builder = TxBuilder(env)

    // Separate builder for withdraw: the validator performs strict fee accounting
    // (adaInOutputs === requestedAmount + adaInInputs - fee) which fails during
    // TxBuilder's iterative balancing because the script is evaluated before fee/change
    // values converge. constMaxBudget skips script evaluation during building;
    // the actual validation happens at submission time (emulator or node).
    private val withdrawBuilder = TxBuilder.withConstMaxBudgetEvaluator(env)

    def lock(
        utxos: Utxos,
        value: Value,
        sponsor: Address,
        beneficiary: AddrKeyHash,
        startTimestamp: Long,
        duration: Long,
        signer: TransactionSigner
    ): Transaction = {
        val datum = Config(
          PubKeyHash(beneficiary),
          BigInt(startTimestamp),
          BigInt(duration),
          BigInt(value.coin.value)
        )

        builder
            .payTo(scriptAddress, value, datum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    def withdraw(
        utxos: Utxos,
        vestingUtxo: Utxo,
        amount: Long,
        beneficiaryAddress: Address,
        beneficiaryPkh: AddrKeyHash,
        sponsor: Address,
        validFrom: Instant,
        signer: TransactionSigner
    ): Transaction = {
        require(amount > 0, "Withdrawal amount must be positive")

        val datum = vestingUtxo.output.requireInlineDatum
        val redeemer = Action(BigInt(amount))
        val contractAmount = vestingUtxo.output.value.coin.value

        val b = withdrawBuilder
            .spend(vestingUtxo, redeemer, contract)
            .requireSignature(beneficiaryPkh)
            .payTo(beneficiaryAddress, Value.lovelace(amount))
            .validFrom(validFrom)

        val b2 =
            if amount == contractAmount then b
            else b.payTo(scriptAddress, Value.lovelace(contractAmount - amount), datum)

        b2.complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }
}
