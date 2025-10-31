package scalus.cardano.ledger
package rules

import scalus.uplc.eval.ExBudget

import scala.util.boundary
import scala.util.boundary.break
import scala.util.control.NonFatal

// It's conwayEvalScriptsTxValid in cardano-ledger
object PlutusScriptsTransactionMutator extends STS.Mutator {
    override final type Error = TransactionException.BadCollateralInputsUTxOException |
        TransactionException.IllegalArgumentException

    override def transit(context: Context, state: State, event: Event): Result = boundary {
        val body = event.body.value
        val slotConfig = context.slotConfig
        val protocolParameters = context.env.params
        val maxTxExecutionUnits = protocolParameters.maxTxExecutionUnits
        val protocolVersion = protocolParameters.protocolVersion
        val costModels = protocolParameters.costModels
        val utxo = state.utxos

        try {
            PlutusScriptEvaluator(
              slotConfig = slotConfig,
              initialBudget =
                  ExBudget.fromCpuAndMemory(maxTxExecutionUnits.steps, maxTxExecutionUnits.memory),
              protocolMajorVersion = MajorProtocolVersion(protocolVersion.major),
              costModels = protocolParameters.costModels,
              mode = EvaluatorMode.Validate,
              debugDumpFilesForTesting = false
            ).evalPlutusScripts(event, utxo)

            if event.isValid then
                val addedUtxo: Utxos = event.body.value.outputs.view.zipWithIndex.map {
                    case (Sized(output, _), index) =>
                        TransactionInput(event.id, index) -> output
                }.toMap

                // TODO full transition
                success(
                  state.copy(
                    utxos = state.utxos -- event.body.value.inputs.toSortedSet ++ addedUtxo,
                    fees = state.fees + event.body.value.fee,
                    donation = state.donation + event.body.value.donation.getOrElse(Coin.zero)
                  )
                )
            else
                // TODO: refine exception handling
                failure(
                  TransactionException.IllegalArgumentException(
                    s"Transaction with invalid flag passed script validation, transactionId: ${event.id}, flag: ${event.isValid}"
                  )
                )
        } catch {
            case e: PlutusScriptEvaluationException =>
                if event.isValid then
                    // TODO: refine exception handling
                    failure(
                      TransactionException.IllegalArgumentException(
                        s"Transaction with invalid flag passed script validation, transactionId: ${event.id}, flag: ${event.isValid}"
                      )
                    )
                else
                    val addedUtxo = event.body.value.collateralReturnOutput
                        .map(v =>
                            TransactionInput(event.id, event.body.value.outputs.size) -> v.value
                        )
                        .toMap

                    val collateralReturnCoins = event.body.value.collateralReturnOutput
                        .map(v => v.value.value.coin)
                        .getOrElse(Coin.zero)

                    val collateralCoins = event.body.value.collateralInputs.toSortedSet.view
                        .map { input =>
                            utxo.get(input) match {
                                case Some(output) => output.value.coin
                                case None         =>
                                    break(
                                      Left(
                                        TransactionException.BadCollateralInputsUTxOException(
                                          event.id
                                        )
                                      )
                                    )
                            }
                        }
                        .foldLeft(Coin.zero)(_ + _)

                    // TODO full transition
                    success(
                      state.copy(
                        utxos =
                            state.utxos -- event.body.value.collateralInputs.toSortedSet ++ addedUtxo,
                        fees = state.fees + (collateralCoins - collateralReturnCoins)
                      )
                    )
            case NonFatal(exception) =>
                // TODO: refine exception handling
                failure(
                  TransactionException.IllegalArgumentException(
                    s"Error during Plutus script evaluation: ${exception.getMessage}"
                  )
                )
        }
    }
}
