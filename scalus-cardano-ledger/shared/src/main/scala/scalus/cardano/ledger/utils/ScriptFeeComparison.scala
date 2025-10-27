package scalus.cardano.ledger.utils

import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*

object ScriptFeeComparison {

    case class FeeComparison(directFee: Coin, referenceFee: Coin)

    def compareFees(
        script: PlutusScript,
        redeemer: Data,
        datum: Datum,
        context: BuilderContext,
        additionalSigners: Set[ExpectedSigner] = Set.empty,
        scriptValue: Value = Value.lovelace(10_000_000)
    ): Either[String, FeeComparison] = {

        val scriptAddress = ShelleyAddress(
          network = context.env.network,
          payment = ShelleyPaymentPart.Script(script.scriptHash),
          delegation = ShelleyDelegationPart.Null
        )

        val scriptUtxoInput = TransactionInput(
          TransactionHash.fromByteString(ByteString.fromHex("aa" * 32)),
          0
        )
        val scriptUtxoDatum = datum match
            case Datum.DatumInlined     => DatumOption.Inline(Data.I(0))
            case Datum.DatumValue(data) => DatumOption.Inline(data)
        val scriptUtxoOutput = TransactionOutput.Babbage(
          address = scriptAddress,
          value = scriptValue,
          datumOption = Some(scriptUtxoDatum),
          scriptRef = None
        )
        val scriptUtxo = TransactionUnspentOutput(scriptUtxoInput, scriptUtxoOutput)

        def createDirect = {
            val directWitness = ThreeArgumentPlutusScriptWitness(
              scriptSource = ScriptSource.PlutusScriptValue(script),
              redeemer = redeemer,
              datum = datum,
              additionalSigners = additionalSigners
            )

            PaymentBuilder(context)
                .withStep(TransactionBuilderStep.Spend(scriptUtxo, directWitness))
                .payTo(context.wallet.owner, scriptValue)
                .build()
        }

        def createRef = {
            val refScriptUtxoInput = TransactionInput(
              TransactionHash.fromByteString(ByteString.fromHex("bb" * 32)),
              0
            )
            val refScriptUtxoOutput = TransactionOutput.Babbage(
              address = context.wallet.owner,
              value = Value.lovelace(5_000_000),
              datumOption = None,
              scriptRef = Some(ScriptRef(script: Script))
            )
            val refScriptUtxo = TransactionUnspentOutput(refScriptUtxoInput, refScriptUtxoOutput)

            val refWitness = ThreeArgumentPlutusScriptWitness(
              scriptSource = ScriptSource.PlutusScriptAttached,
              redeemer = redeemer,
              datum = datum,
              additionalSigners = additionalSigners
            )

            PaymentBuilder(context)
                .withStep(TransactionBuilderStep.ReferenceOutput(refScriptUtxo))
                .withStep(TransactionBuilderStep.Spend(scriptUtxo, refWitness))
                .payTo(context.wallet.owner, scriptValue)
                .build()
        }

        for
            direct <- createDirect
            reference <- createRef
        yield FeeComparison(direct.body.value.fee, reference.body.value.fee)
    }
}
