package scalus.examples.vault

import scalus.builtin.Data.toData
import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Credential, DatumOption, Transaction, TransactionInput, TransactionOutput, Value}
import scalus.cardano.txbuilder.{BuilderContext, PaymentBuilder}

class Transactions(context: BuilderContext) {
    val wallet = context.wallet
    val script = VaultContract.defaultCompiledContract.script
    val scriptAddress = Address(context.env.network, Credential.ScriptHash(script.scriptHash))

    def lock(
        value: Value,
        waitTime: BigInt,
        owner: Address = wallet.owner
    ): Either[String, Transaction] = {
        val inputsToSpend = wallet.selectInputs(value).get
        val builder = inputsToSpend.foldLeft(PaymentBuilder(context)) {
            case (builder, (utxo, witness)) =>
                builder.spendOutputs(utxo, witness)
        }
        val ownerCredentialHash = owner match {
            case addr: scalus.cardano.address.ShelleyAddress =>
                scalus.builtin.ByteString.fromArray(addr.payment.asHash.bytes)
            case _ => return Left("Shelley addresses only.")
        }
        val datum = State(
          ownerCredentialHash,
          Status.Idle,
          BigInt(value.coin.value),
          waitTime,
          BigInt(0)
        )
        builder.payToScript(scriptAddress, value, datum.toData).build()
    }

    def withdraw(
        vaultUtxo: (TransactionInput, TransactionOutput),
        validityStartSlot: Long
    ): Either[String, Transaction] = {
        val currentDatum = vaultUtxo._2 match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[State]
            case _ =>
                return Left("Vault UTxO must have an inline datum")
        }

        // Calculate the absolute finalization deadline
        val requestTime =
            BigInt(context.env.slotConfig.slotToTime(validityStartSlot))
        val finalizationDeadline = requestTime + currentDatum.waitTime

        val newDatum = currentDatum.copy(
          status = Status.Pending,
          finalizationDeadline = finalizationDeadline
        )
        val vaultValue = vaultUtxo._2.value

        val redeemer = Action.InitiateWithdrawal.toData
        PaymentBuilder(context)
            .spendScriptOutputs(
              vaultUtxo,
              redeemer,
              script
            )
            .withStep(
              scalus.cardano.txbuilder.TransactionBuilderStep
                  .ValidityStartSlot(validityStartSlot)
            )
            .payToScript(scriptAddress, vaultValue, newDatum.toData)
            .build()
    }

    def deposit(
        vaultUtxo: (TransactionInput, TransactionOutput),
        additionalValue: Value
    ): Either[String, Transaction] = {
        val currentDatum = vaultUtxo._2 match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[State]
            case _ =>
                return Left("Vault UTxO must have an inline datum")
        }

        val currentValue = vaultUtxo._2.value
        val newValue = currentValue + additionalValue
        val newAmount = BigInt(newValue.coin.value)

        // Preserve waitTime and finalizationDeadline
        val newDatum = currentDatum.copy(
          amount = newAmount,
          waitTime = currentDatum.waitTime,
          finalizationDeadline = currentDatum.finalizationDeadline
        )

        val redeemer = Action.Deposit.toData
        PaymentBuilder(context)
            .spendScriptOutputs(vaultUtxo, redeemer, script)
            .payToScript(scriptAddress, newValue, newDatum.toData)
            .build()
    }

    def finalize(
        vaultUtxo: (TransactionInput, TransactionOutput),
        ownerAddress: Address,
        overrideValiditySlot: Option[Long] = None
    ): Either[String, Transaction] = {
        val currentDatum = vaultUtxo._2 match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[State]
            case _ =>
                return Left("Vault UTxO must have an inline datum")
        }

        val vaultValue = vaultUtxo._2.value

        val calculatedSlot =
            context.env.slotConfig
                .timeToSlot(currentDatum.finalizationDeadline.toLong) + 1
        val finalizationSlot = overrideValiditySlot.getOrElse(calculatedSlot)

        val redeemer = Action.FinalizeWithdrawal.toData
        PaymentBuilder(context)
            .spendScriptOutputs(vaultUtxo, redeemer, script)
            .withStep(
              scalus.cardano.txbuilder.TransactionBuilderStep
                  .ValidityStartSlot(finalizationSlot)
            )
            .payTo(ownerAddress, vaultValue)
            .build()
    }

}
