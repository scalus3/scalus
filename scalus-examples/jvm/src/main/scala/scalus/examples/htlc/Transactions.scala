package scalus.examples.htlc

import scalus.builtin.Data.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.cardano.blueprint.PlutusV3CompiledContract

class Transactions(
    context: BuilderContext,
    compiledContract: PlutusV3CompiledContract = HtlcContract.defaultCompiledContract
) {

    val wallet = context.wallet
    val script = compiledContract.script
    val scriptAddress = compiledContract.address(context.env.network)

    def lock(
        value: Value,
        committer: AddrKeyHash,
        receiver: AddrKeyHash,
        image: Image,
        timeout: PosixTime
    ): Either[String, Transaction] = {
        val inputsToSpend = wallet.selectInputs(value).get
        val builder = inputsToSpend.foldLeft(PaymentBuilder(context)) {
            case (builder, (utxo, witness)) =>
                builder.spendOutputs(utxo, witness)
        }
        val datum = Config(PubKeyHash(committer), PubKeyHash(receiver), image, timeout).toData
        builder.payToScript(scriptAddress, value, datum).build()
    }

    def reveal(
        lockedUtxo: Utxo,
        preimage: Preimage,
        recipientAddress: Address,
        receiverPkh: AddrKeyHash,
        time: PosixTime
    ): Either[String, Transaction] = {
        val Utxo(input, output) = lockedUtxo
        val redeemer = Action.Reveal(preimage).toData
        val (collat, collatWitness) = wallet.collateralInputs.head
        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemer = redeemer,
          datum = Datum.DatumInlined,
          additionalSigners = Set(ExpectedSigner(receiverPkh))
        )
        val validityStartSlot = context.env.slotConfig.timeToSlot(time.toLong)

        PaymentBuilder(context)
            .withStep(TransactionBuilderStep.Spend(lockedUtxo, witness))
            .withStep(TransactionBuilderStep.ValidityStartSlot(validityStartSlot))
            .payTo(recipientAddress, output.value)
            .collateral(collat, collatWitness)
            .build()
    }

    def timeout(
        lockedUtxo: Utxo,
        committerAddress: Address,
        committerPkh: AddrKeyHash,
        time: PosixTime
    ): Either[String, Transaction] = {
        val Utxo(input, output) = lockedUtxo
        val redeemer = Action.Timeout.toData
        val (collat, collatWitness) = wallet.collateralInputs.head
        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemer = redeemer,
          datum = Datum.DatumInlined,
          additionalSigners = Set(ExpectedSigner(committerPkh))
        )
        val validityStartSlot = context.env.slotConfig.timeToSlot(time.toLong)

        PaymentBuilder(context)
            .withStep(TransactionBuilderStep.Spend(lockedUtxo, witness))
            .withStep(TransactionBuilderStep.ValidityStartSlot(validityStartSlot))
            .payTo(committerAddress, output.value)
            .collateral(collat, collatWitness)
            .build()
    }
}
