package scalus.examples

import scalus.*
import scalus.prelude.Validator
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.cardano.ledger.Value
import scalus.ledger.api.v3.{ScriptContext, TxInfo, TxOutRef}
import scalus.uplc.Program

case class InputCountRedeemer(inputCount: BigInt) derives FromData, ToData

@Compile
object InputCountValidator extends Validator:
    inline override def spend(
        datum: prelude.Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val InputCountRedeemer(inputCount) = redeemer.to[InputCountRedeemer]

        trace("Input count received")(())
    }

object InputCountValidatorContract:
    val compiledValidator = compile(InputCountValidator.validate)
    val script: Program = compiledValidator.toUplc(generateErrorTraces = true).plutusV3

object Offchain:
    import scalus.builtin.ToData.toData
    import scalus.cardano.address.Address
    import scalus.cardano.ledger.{Transaction, TransactionInput, TransactionOutput}
    import scalus.cardano.ledger.Script.PlutusV3
    import scalus.cardano.txbuilder.{BuilderContext, PaymentBuilder}

    def makeTransaction(
        context: BuilderContext,
        scriptUtxo: (TransactionInput, TransactionOutput),
        recipientAddress: Address,
        paymentValue: Value
    ): Either[String, Transaction] = {
        val plutusScript = PlutusV3(InputCountValidatorContract.script.cborByteString)

        PaymentBuilder(context)
            .spendUsingDelayedRedeemer(
              utxo = scriptUtxo,
              redeemerBuilder = tx => {
                  val inputCount = tx.body.value.inputs.toSet.size
                  InputCountRedeemer(BigInt(inputCount)).toData
              },
              validator = plutusScript,
              datum = None
            )
            .payTo(recipientAddress, paymentValue)
            .build()
    }
