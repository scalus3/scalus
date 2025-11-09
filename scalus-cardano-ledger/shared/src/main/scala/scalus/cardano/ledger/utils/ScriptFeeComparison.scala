package scalus.cardano.ledger.utils

import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.compiler.sir.TargetLoweringBackend.{ScottEncodingLowering, SirToUplcV3Lowering, SumOfProductsLowering}
import scalus.{plutusV3, toUplc, Compiler}
import scalus.uplc.Program

object ScriptFeeComparison {

    case class FeeComparison(
        directFee: Coin,
        referenceFee: Coin,
        directExUnits: ExUnits,
        referenceExUnits: ExUnits
    )

    object FeeComparison {
        def apply(directTx: Transaction, refTx: Transaction): FeeComparison = FeeComparison(
          directTx.body.value.fee,
          refTx.body.value.fee,
          directTx.witnessSet.redeemers.get.value.toSeq.head.exUnits,
          refTx.witnessSet.redeemers.get.value.toSeq.head.exUnits
        )
    }

    enum ComparisonResult {
        case Ok(
            comparison: FeeComparison,
            options: Compiler.Options,
        )
        case Fail(message: String, options: Compiler.Options)
    }

    case class ComparisonMatrix(results: Map[Compiler.Options, ComparisonResult])

    def compareFees(
        script: PlutusScript,
        redeemer: Data,
        datum: Option[DatumOption],
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

        val scriptUtxoOutput = TransactionOutput.Babbage(
          address = scriptAddress,
          value = scriptValue,
          datumOption = datum,
          scriptRef = None
        )
        val scriptUtxo = Utxo(scriptUtxoInput, scriptUtxoOutput)

        def createDirect = {
            val directWitness = ThreeArgumentPlutusScriptWitness(
              scriptSource = ScriptSource.PlutusScriptValue(script),
              redeemer = redeemer,
              datum = DatumInlined,
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
            val refScriptUtxo = Utxo(refScriptUtxoInput, refScriptUtxoOutput)

            val refWitness = ThreeArgumentPlutusScriptWitness(
              scriptSource = ScriptSource.PlutusScriptAttached,
              redeemer = redeemer,
              datum = DatumInlined,
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
        yield FeeComparison(direct, reference)
    }

    inline def compareAll(
        inline code: Any,
        redeemer: Data,
        datum: Option[DatumOption],
        context: BuilderContext,
        additionalSigners: Set[ExpectedSigner] = Set.empty,
        scriptValue: Value = Value.lovelace(10_000_000)
    ): Map[Compiler.Options, ComparisonResult] = enumerateOptions
        .groupMap(identity) { options =>
            val plutusV3 = makePlutusV3(options, code)
            compareFees(plutusV3, redeemer, datum, context, additionalSigners, scriptValue) match {
                case Right(comparison)  => ComparisonResult.Ok(comparison, options)
                case Left(errorMessage) => ComparisonResult.Fail(errorMessage, options)
            }
        }
        .view
        .mapValues(_.head)
        .toMap

    private inline def makePlutusV3(options: Compiler.Options, inline code: Any) = {
        val p: Program = Compiler.compileInlineWithOptions(options, code).toUplc().plutusV3
        Script.PlutusV3(p.cborByteString)
    }

    private def enumerateOptions: Seq[Compiler.Options] = for {
        backend <- Seq(ScottEncodingLowering, SumOfProductsLowering, SirToUplcV3Lowering)
        traces <- Seq(true, false)
        optimize <- Seq(true, false)
        debug <- Seq(true, false)
    } yield Compiler.Options(backend, traces, optimize, debug = debug)
}
