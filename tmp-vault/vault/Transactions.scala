package scalus.examples.vault

import scalus.builtin.Data.toData
import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Credential, DatumOption, Transaction, TransactionInput, TransactionOutput, Value}
import scalus.cardano.ledger.Script.PlutusV3
import scalus.cardano.ledger.txbuilder.{BuilderContext, PaymentBuilder}

class Transactions(context: BuilderContext) {
    val wallet = context.wallet
    val script = PlutusV3(VaultContract.script.cborByteString)
    val scriptAddress = Address(context.env.network, Credential.ScriptHash(script.scriptHash))

    def lock(
        value: Value,
        waitTime: BigInt,
        owner: Address = wallet.owner
    ): Either[String, Transaction] = {
        val inputsToSpend = wallet.selectInputs(value).get
        val builder = inputsToSpend.foldLeft(PaymentBuilder(context)) {
            case (builder, (utxo, witness)) =>
                builder.spendOutputs((utxo.input, utxo.output), witness)
        }
        val ownerCredentialHash = owner match {
            case addr: scalus.cardano.address.ShelleyAddress =>
                scalus.builtin.ByteString.fromArray(addr.payment.asHash.bytes)
        }
        val datum = Datum(
          ownerCredentialHash,
          State.Idle,
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
                d.to[Datum]
            case _ =>
                return Left("Vault UTxO must have an inline datum")
        }

        // Calculate the absolute finalization deadline
        val requestTime =
            BigInt(context.env.evaluator.slotConfig.slotToTime(validityStartSlot))
        val finalizationDeadline = requestTime + currentDatum.waitTime

        val newDatum = currentDatum.copy(
          state = State.Pending,
          finalizationDeadline = finalizationDeadline
        )
        val vaultValue = vaultUtxo._2.value

        val redeemer = Redeemer.Withdraw.toData
        PaymentBuilder(context)
            .spendScriptOutputs(
              vaultUtxo,
              redeemer,
              script
            )
            .withStep(
              scalus.cardano.ledger.txbuilder.TransactionBuilderStep
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
                d.to[Datum]
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

        val redeemer = Redeemer.Deposit.toData
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
                d.to[Datum]
            case _ =>
                return Left("Vault UTxO must have an inline datum")
        }

        val vaultValue = vaultUtxo._2.value

        val calculatedSlot =
            context.env.evaluator.slotConfig
                .timeToSlot(currentDatum.finalizationDeadline.toLong) + 1
        val finalizationSlot = overrideValiditySlot.getOrElse(calculatedSlot)

        val redeemer = Redeemer.Finalize.toData
        PaymentBuilder(context)
            .spendScriptOutputs(vaultUtxo, redeemer, script)
            .withStep(
              scalus.cardano.ledger.txbuilder.TransactionBuilderStep
                  .ValidityStartSlot(finalizationSlot)
            )
            .payTo(ownerAddress, vaultValue)
            .build()
    }

}
