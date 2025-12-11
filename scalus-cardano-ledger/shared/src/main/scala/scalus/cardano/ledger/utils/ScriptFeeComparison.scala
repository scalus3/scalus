package scalus.cardano.ledger.utils

import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
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

    private def arbAddress(env: Environment) = Address(
      env.network,
      Credential.KeyHash(
        AddrKeyHash(ByteString.fromString("a".repeat(28)))
      )
    )

    def compareFees(
        script: PlutusScript,
        redeemer: Data,
        datum: Option[DatumOption],
        env: Environment,
        additionalSigners: Set[ExpectedSigner] = Set.empty,
        scriptValue: Value = Value.ada(10)
    ): Either[String, FeeComparison] = {

        val scriptAddress = ShelleyAddress(
          network = env.network,
          payment = ShelleyPaymentPart.Script(script.scriptHash),
          delegation = ShelleyDelegationPart.Null
        )

        val scriptUtxoInput = TransactionInput(
          TransactionHash.fromByteString(ByteString.fromHex("aa" * 32)),
          0
        )

        val scriptUtxoOutput = TransactionOutput(
          address = scriptAddress,
          value = scriptValue,
          datumOption = datum,
          scriptRef = None
        )
        val scriptUtxo = Utxo(scriptUtxoInput, scriptUtxoOutput)

        // Create funding UTXO to cover transaction fees
        val fundingUtxoInput = TransactionInput(
          TransactionHash.fromByteString(ByteString.fromHex("cc" * 32)),
          0
        )
        val fundingUtxoOutput = TransactionOutput(
          address = arbAddress(env),
          value = Value.ada(100),
          datumOption = None,
          scriptRef = None
        )
        val fundingUtxo = Utxo(fundingUtxoInput, fundingUtxoOutput)

        // Create collateral UTXO for script execution
        val collateralUtxoInput = TransactionInput(
          TransactionHash.fromByteString(ByteString.fromHex("dd" * 32)),
          0
        )
        val collateralUtxoOutput = TransactionOutput(
          address = arbAddress(env),
          value = Value.ada(5),
          datumOption = None,
          scriptRef = None
        )
        val collateralUtxo = Utxo(collateralUtxoInput, collateralUtxoOutput)

        def createDirect = {
            val additionalSignerHashes = additionalSigners.map(_.hash)
            TxBuilder(env)
                .spend(fundingUtxo)
                .collaterals(collateralUtxo)
                .spend(scriptUtxo, redeemer, script, additionalSignerHashes)
                .payTo(arbAddress(env), scriptValue)
                .build(changeTo = arbAddress(env))
                .transaction
        }

        def createRef = {
            val refScriptUtxoInput = TransactionInput(
              TransactionHash.fromByteString(ByteString.fromHex("bb" * 32)),
              0
            )
            val refScriptUtxoOutput = TransactionOutput(
              address = arbAddress(env),
              value = Value.ada(5),
              datumOption = None,
              scriptRef = Some(ScriptRef(script: Script))
            )
            val refScriptUtxo = Utxo(refScriptUtxoInput, refScriptUtxoOutput)

            val additionalSignerHashes = additionalSigners.map(_.hash)
            TxBuilder(env)
                .spend(fundingUtxo)
                .collaterals(collateralUtxo)
                .references(refScriptUtxo)
                .spend(scriptUtxo, redeemer, additionalSignerHashes)
                .payTo(arbAddress(env), scriptValue)
                .build(changeTo = arbAddress(env))
                .transaction
        }

        try {
            val direct = createDirect
            val reference = createRef
            Right(FeeComparison(direct, reference))
        } catch {
            case e: Exception => Left(e.getMessage)
        }
    }

    inline def compareAll(
        inline code: Any,
        redeemer: Data,
        datum: Option[DatumOption],
        env: Environment,
        additionalSigners: Set[ExpectedSigner] = Set.empty,
        scriptValue: Value = Value.ada(10)
    ): Map[Compiler.Options, ComparisonResult] = enumerateOptions
        .groupMap(identity) { options =>
            val plutusV3 = makePlutusV3(options, code)
            compareFees(plutusV3, redeemer, datum, env, additionalSigners, scriptValue) match {
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
    } yield Compiler.Options(
      targetLoweringBackend = backend,
      generateErrorTraces = traces,
      optimizeUplc = optimize,
      debug = debug
    )
}
