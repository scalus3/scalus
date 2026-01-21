package scalus.examples.vault

import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.uplc.PlutusV3

case class VaultTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    signer: TransactionSigner,
    contract: PlutusV3[Data => Unit]
) {
    def script: Script.PlutusV3 = contract.script
    val scriptAddress: Address = contract.address(env.network)

    def lock(
        utxos: Utxos,
        ada: Coin,
        waitTime: Long,
        owner: Address,
        changeAddress: Address
    ): Transaction = {
        val ownerCredentialHash = owner match {
            case addr: scalus.cardano.address.ShelleyAddress =>
                scalus.builtin.ByteString.fromArray(addr.payment.asHash.bytes)
            case _ => throw new IllegalArgumentException("Shelley addresses only.")
        }
        val datum = State(
          ownerCredentialHash,
          Status.Idle,
          BigInt(ada.value),
          waitTime,
          BigInt(0)
        )

        TxBuilder(env, evaluator)
            .spend(utxos)
            .payTo(scriptAddress, Value(ada), datum)
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }

    def withdraw(
        utxos: Utxos,
        collateralUtxos: Utxos,
        vaultUtxo: Utxo,
        changeAddress: Address,
        validityStartTime: Long
    ): Transaction = {
        val currentDatum = vaultUtxo.output match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[State]
            case _ =>
                throw new IllegalArgumentException("Vault UTxO must have an inline datum")
        }

        val requestTime = BigInt(validityStartTime)
        val finalizationDeadline = requestTime + currentDatum.waitTime

        val newDatum = currentDatum.copy(
          status = Status.Pending,
          finalizationDeadline = finalizationDeadline
        )
        val vaultValue = vaultUtxo.output.value

        val ownerAddrKeyHash = AddrKeyHash.fromByteString(currentDatum.owner)

        TxBuilder(env, evaluator)
            .spend(utxos)
            .collaterals(collateralUtxos)
            .spend(vaultUtxo, Action.InitiateWithdrawal, script, Set(ownerAddrKeyHash))
            .validFrom(java.time.Instant.ofEpochMilli(validityStartTime))
            .payTo(scriptAddress, vaultValue, newDatum)
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }

    def deposit(
        utxos: Utxos,
        collateralUtxos: Utxos,
        vaultUtxo: Utxo,
        additionalValue: Value,
        changeAddress: Address
    ): Transaction = {
        val currentDatum = vaultUtxo.output match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[State]
            case _ =>
                throw new IllegalArgumentException("Vault UTxO must have an inline datum")
        }

        val currentValue = vaultUtxo.output.value
        val newValue = currentValue + additionalValue
        val newAmount = BigInt(newValue.coin.value)

        val newDatum = currentDatum.copy(
          amount = newAmount,
          waitTime = currentDatum.waitTime,
          finalizationDeadline = currentDatum.finalizationDeadline
        )

        TxBuilder(env, evaluator)
            .spend(utxos)
            .collaterals(collateralUtxos)
            .spend(vaultUtxo, Action.Deposit, script)
            .payTo(scriptAddress, newValue, newDatum)
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }

    def finalize(
        utxos: Utxos,
        collateralUtxos: Utxos,
        vaultUtxo: Utxo,
        ownerAddress: Address,
        changeAddress: Address,
        validityStartTime: Long
    ): Transaction = {
        val currentDatum = vaultUtxo.output match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[State]
            case _ =>
                throw new IllegalArgumentException("Vault UTxO must have an inline datum")
        }

        val vaultValue = vaultUtxo.output.value

        TxBuilder(env, evaluator)
            .spend(utxos)
            .collaterals(collateralUtxos)
            .spend(vaultUtxo, Action.FinalizeWithdrawal, script)
            .validFrom(java.time.Instant.ofEpochMilli(validityStartTime))
            .payTo(ownerAddress, vaultValue)
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }
}
