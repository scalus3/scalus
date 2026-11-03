package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.ProducedUtxos

import scala.util.boundary
import scala.util.boundary.break
import scala.util.control.NonFatal

// It's conwayEvalScriptsTxValid and babbageEvalScriptsTxInvalid in cardano-ledger
object PlutusScriptsTransactionMutator extends STS.Mutator {
    override final type Error = TransactionException.BadCollateralInputsUTxOException |
        TransactionException.IllegalArgumentException |
        TransactionException.PlutusScriptValidationException |
        TransactionException.PassedUnexpectedlyException |
        TransactionException.CollectErrorsException

    override def transit(context: Context, state: State, event: Event): Result =
        transit(context, state, event, failureFree = true)

    /** The UTXOS rule with the failures of the earlier rules known, spec [FL-5]: the collect errors
      * are reported in every case, and the scripts are evaluated only when `failureFree`, as
      * `whenFailureFree` does (B/Rules/Utxos.hs:184, 255). Without an evaluation it returns
      * `state`.
      */
    private[ledger] def transit(
        context: Context,
        state: State,
        event: Event,
        failureFree: Boolean
    ): Result = {
        val params = context.env.params
        val errors = PlutusScriptEvaluator.collectErrors(
          event,
          state.utxos,
          context.slotConfig,
          params.protocolVersion.toMajor,
          params.costModels
        )
        // A collect error is a phase-1 failure whatever isValid says, spec [FL-10], [TX-1]
        if errors.nonEmpty then
            failure(TransactionException.CollectErrorsException(event.id, errors))
        else if failureFree then evaluate(context, state, event)
        else success(state)
    }

    private def evaluate(context: Context, state: State, event: Event): Result = boundary {
        val slotConfig = context.slotConfig
        val protocolParameters = context.env.params
        val maxTxExecutionUnits = protocolParameters.maxTxExecutionUnits
        val protocolVersion = protocolParameters.protocolVersion
        val utxo = state.utxos

        try {
            PlutusScriptEvaluator(
              slotConfig = slotConfig,
              initialBudget = maxTxExecutionUnits,
              protocolMajorVersion = protocolVersion.toMajor,
              costModels = protocolParameters.costModels,
              mode = context.evaluatorMode
            ).evalPlutusScripts(event, utxo, context.debugScripts)

            if event.isValid then
                // TODO full transition
                success(
                  state.copy(
                    utxos = state.utxos -- event.body.value.inputs.toSet ++ ProducedUtxos.of(event),
                    fees = state.fees + event.body.value.fee,
                    donation = state.donation + event.body.value.donation.getOrElse(Coin.zero)
                  )
                )
            // ValidationTagMismatch (IsValid False) PassedUnexpectedly (B/Rules/Utxos.hs:255-260),
            // spec [FL-9]
            else failure(TransactionException.PassedUnexpectedlyException(event.id))
        } catch {
            case e: PlutusScriptEvaluationException =>
                if event.isValid then
                    failure(
                      TransactionException.PlutusScriptValidationException(
                        event.id,
                        e.getMessage,
                        e.logs.toSeq,
                        spentBudget = e.spentBudget,
                        scriptHash = Some(e.failedScriptHash)
                      )
                    )
                else
                    val collateralReturnCoins = event.body.value.collateralReturnOutput
                        .map(v => v.value.value.coin)
                        .getOrElse(Coin.zero)

                    val collateralCoins = event.body.value.collateralInputs.toSet.view
                        .map { input =>
                            utxo.get(input) match {
                                case Some(output) => output.value.coin
                                case None =>
                                    break(
                                      Left(
                                        TransactionException.BadCollateralInputsUTxOException(
                                          event.id
                                        )
                                      )
                                    )
                            }
                        }
                        .foldLeft(BigInt(0))(_ + _.value)

                    // The collateral inputs can hold more than `Long.MaxValue` lovelace together,
                    // so they are summed exactly before the return is taken off.
                    val collectedCollateral =
                        Coin.exact(collateralCoins - collateralReturnCoins.value)

                    // TODO full transition
                    success(
                      state.copy(
                        utxos = state.utxos -- event.body.value.collateralInputs.toSet ++
                            ProducedUtxos.of(event),
                        fees = state.fees + collectedCollateral
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
