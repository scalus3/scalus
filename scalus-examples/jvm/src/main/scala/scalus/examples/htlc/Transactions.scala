package scalus.examples.htlc

import scalus.builtin.ToData.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.PosixTime
import scalus.cardano.blueprint.PlutusV3CompiledContract

class Transactions(
    context: BuilderContext,
    compiledContract: PlutusV3CompiledContract = HtlcContract.defaultCompiledContract
) {

    val wallet = context.wallet
    val script = compiledContract.script
    val scriptAddress = Address(context.env.network, Credential.ScriptHash(script.scriptHash))

    def lock(
        value: Value,
        committer: PubKeyHash,
        receiver: PubKeyHash,
        image: Image,
        timeout: PosixTime
    ): Either[String, Transaction] = {
        val inputsToSpend = wallet.selectInputs(value).get
        val builder = inputsToSpend.foldLeft(PaymentBuilder(context)) {
            case (builder, (utxo, witness)) =>
                builder.spendOutputs((utxo.input, utxo.output), witness)
        }
        val datum = ContractDatum(committer, receiver, image, timeout).toData
        builder.payToScript(scriptAddress, value, datum).build()
    }

    def reveal(
        lockedUtxo: (TransactionInput, TransactionOutput),
        preimage: Preimage,
        recipientAddress: Address,
        receiverPkh: PubKeyHash,
        time: PosixTime
    ): Either[String, Transaction] = {
        val (input, output) = lockedUtxo
        val redeemer = Action.Reveal(preimage).toData
        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemer = redeemer,
          datum = Datum.DatumInlined,
          additionalSigners = Set(ExpectedSigner(AddrKeyHash.fromByteString(receiverPkh)))
        )
        val validityStartSlot = context.env.slotConfig.timeToSlot(time.toLong)

        PaymentBuilder(context)
            .withStep(TransactionBuilderStep.Spend(TransactionUnspentOutput(lockedUtxo), witness))
            .withStep(TransactionBuilderStep.ValidityStartSlot(validityStartSlot))
            .payTo(recipientAddress, output.value)
            .build()
    }

    def timeout(
        lockedUtxo: (TransactionInput, TransactionOutput),
        committerAddress: Address,
        committerPkh: PubKeyHash,
        time: PosixTime
    ): Either[String, Transaction] = {
        val (input, output) = lockedUtxo
        val redeemer = Action.Timeout.toData
        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemer = redeemer,
          datum = Datum.DatumInlined,
          additionalSigners = Set(ExpectedSigner(AddrKeyHash.fromByteString(committerPkh)))
        )
        val validityStartSlot = context.env.slotConfig.timeToSlot(time.toLong)

        PaymentBuilder(context)
            .withStep(TransactionBuilderStep.Spend(TransactionUnspentOutput(lockedUtxo), witness))
            .withStep(TransactionBuilderStep.ValidityStartSlot(validityStartSlot))
            .payTo(committerAddress, output.value)
            .build()
    }
}
