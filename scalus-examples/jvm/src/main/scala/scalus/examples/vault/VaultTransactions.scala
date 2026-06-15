package scalus.examples.vault

import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.uplc.PlutusV3

case class VaultTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    signer: TransactionSigner,
    contract: PlutusV3[Data => Unit]
) {
    val scriptAddress: Address = contract.address(env.network)

    private def credentialHash(address: Address): ByteString =
        address match {
            case addr: ShelleyAddress => ByteString.fromArray(addr.payment.asHash.bytes)
            case _ => throw new IllegalArgumentException("Shelley addresses only.")
        }

    def lock(
        utxos: Utxos,
        ada: Coin,
        waitTime: Long,
        owner: Address,
        recovery: Address,
        changeAddress: Address
    ): Transaction = {
        val datum = State(
          credentialHash(owner),
          credentialHash(recovery),
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
        validityEndTime: Long
    ): Transaction = {
        val currentDatum = vaultUtxo.output match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[State]
            case _ =>
                throw new IllegalArgumentException("Vault UTxO must have an inline datum")
        }

        // The deadline is anchored to the validity interval's *upper* bound so it cannot be
        // backdated: the ledger guarantees validTo >= now, hence deadline >= now + waitTime.
        val requestTime = BigInt(validityEndTime)
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
            .spend(vaultUtxo, Action.InitiateWithdrawal, contract)
            .requireSignature(ownerAddrKeyHash)
            .validTo(java.time.Instant.ofEpochMilli(validityEndTime))
            .payTo(scriptAddress, vaultValue, newDatum)
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }

    def cancel(
        utxos: Utxos,
        collateralUtxos: Utxos,
        vaultUtxo: Utxo,
        recoveryPkh: AddrKeyHash,
        changeAddress: Address
    ): Transaction = {
        val currentDatum = vaultUtxo.output match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[State]
            case _ =>
                throw new IllegalArgumentException("Vault UTxO must have an inline datum")
        }

        val newDatum = currentDatum.copy(status = Status.Idle, finalizationDeadline = BigInt(0))

        TxBuilder(env, evaluator)
            .spend(utxos)
            .collaterals(collateralUtxos)
            .spend(vaultUtxo, Action.Cancel, contract)
            .requireSignature(recoveryPkh)
            .payTo(scriptAddress, vaultUtxo.output.value, newDatum)
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
            .spend(vaultUtxo, Action.Deposit, contract)
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
            .spend(vaultUtxo, Action.FinalizeWithdrawal, contract)
            .validFrom(java.time.Instant.ofEpochMilli(validityStartTime))
            .payTo(ownerAddress, vaultValue)
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }
}
