package scalus.cardano.txbuilder

import scalus.builtin.Builtins.{blake2b_256, serialiseData}
import scalus.builtin.Data.toData
import scalus.builtin.{Data, ToData}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.ledger.utils.{AllNeededScriptHashes, MinCoinSizedTransactionOutput, TxBalance}
import scalus.cardano.node.Provider
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.mutable

/** A high-level, fluent API for building Cardano transactions.
  *
  * TxBuilder provides a convenient way to construct transactions by chaining method calls. It
  * handles input selection, output creation, script attachment, minting, and transaction
  * finalization including fee calculation and change handling.
  *
  * TxBuilder is purely functional; each method returns a new instance with the updated state. The
  * only methods that does effects is `complete`, others are pure.
  *
  * ==Usage==
  * {{{
  * val tx = TxBuilder(env)
  *   .spend(utxo)
  *   .payTo(recipientAddress, Value.ada(10))
  *   .changeTo(changeAddress)
  *   .build()
  *   .sign(signer)
  *   .transaction
  * }}}
  *
  * @param env
  *   the environment containing protocol parameters, network info, and slot configuration
  * @param context
  *   the current transaction builder context
  * @param evaluator
  *   the Plutus script evaluator used for script validation
  * @param diffHandlerOpt
  *   optional handler for managing transaction balance differences (change)
  * @param steps
  *   accumulated transaction building steps
  * @param attachedScripts
  *   scripts to be included in the transaction witness set
  * @param attachedData
  *   datum values to be included in the transaction witness set
  */
case class TxBuilder(
    env: Environment,
    context: TransactionBuilder.Context,
    evaluator: PlutusScriptEvaluator,
    diffHandlerOpt: Option[DiffHandler] = None,
    steps: Seq[TransactionBuilderStep] = Seq.empty,
    attachedScripts: Map[ScriptHash, Script] = Map.empty,
    attachedData: Map[DataHash, Data] = Map.empty,
) {

    /** Adds the specified pubkey utxo to the list of inputs, thus spending it.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to [[changeTo]] or [[diffHandler]].
      * @param utxo
      *   utxo to spend
      * @note
      *   use [[spend]] with utxo and redeemer to spend script-protected utxos. Otherwise, [[build]]
      *   throws.
      */
    def spend(utxo: Utxo): TxBuilder =
        addSteps(TransactionBuilderStep.Spend(utxo, PubKeyWitness))

    /** Adds the specified pubkey utxos to the list of inputs, thus spending them.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to [[changeTo]] or [[diffHandler]].
      * @param utxos
      *   utxos to spend
      * @note
      *   use [[spend]] with utxo and redeemer to spend script-protected utxos. Otherwise, [[build]]
      *   throws.
      */
    def spend(utxos: Utxos): TxBuilder = {
        utxos.foldLeft(this) { case (builder, utxo) => builder.spend(Utxo(utxo)) }
    }

    /** Adds the specified script-protected utxo to the list of inputs and the specified redeemer to
      * the witness set.
      *
      * Make sure to also call [[attach]] with the script that locks these utxos. If the script that
      * protects the `utxo` fails with the specified `redeemer`, [[build]] is going to throw.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to [[changeTo]] or [[diffHandler]].
      * @param utxo
      *   utxo to spend
      * @param redeemer
      *   redeemer to pass to the script to unlock the inputs
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      */
    def spend[T: ToData](
        utxo: Utxo,
        redeemer: T,
        requiredSigners: Set[AddrKeyHash] = Set.empty
    ): TxBuilder = {
        val scriptHash = extractScriptHash(utxo)
        val datum = buildDatumWitness(utxo)

        val scriptSource = attachedScripts.get(scriptHash) match {
            case Some(ps: PlutusScript) => ScriptSource.PlutusScriptValue(ps)
            case _                      => ScriptSource.PlutusScriptAttached
        }

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = scriptSource,
          redeemer = redeemer.toData,
          datum = datum,
          additionalSigners = requiredSigners.map(ExpectedSigner.apply)
        )
        addSteps(TransactionBuilderStep.Spend(utxo, witness))
    }

    /** Adds the specified script-protected utxo with a delayed redeemer that is computed from the
      * built transaction.
      *
      * Use this method when the redeemer depends on the final transaction structure (e.g., for
      * self-referential scripts). The redeemer is computed after the transaction is assembled but
      * before script evaluation.
      *
      * The script must be attached via [[attach]] or references via [[references]] before calling
      * this method.
      *
      * @param utxo
      *   utxo to spend
      * @param redeemerBuilder
      *   function that computes the redeemer from the assembled transaction
      * @throws IllegalArgumentException
      *   if the script for the utxo is not found in attachedScripts
      */
    def spend(
        utxo: Utxo,
        redeemerBuilder: Transaction => Data
    ): TxBuilder = {
        val scriptHash = extractScriptHash(utxo)
        val datum = buildDatumWitness(utxo)

        val validator = attachedScripts.get(scriptHash) match {
            case Some(ps: PlutusScript) => ps
            case _ =>
                throw new IllegalArgumentException(
                  s"Validator not found in attachedScripts for script hash: $scriptHash"
                )
        }

        addSteps(
          TransactionBuilderStep.SpendWithDelayedRedeemer(
            utxo,
            redeemerBuilder,
            validator,
            datum match {
                case Datum.DatumInlined     => None
                case Datum.DatumValue(data) => Some(data)
            }
          )
        )
    }

    /** Adds the specified script-protected utxo to the list of inputs and the specified redeemer to
      * the witness set.
      *
      * If the specified `script` fails with the specified redeemer, [[build]] is going to throw.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to [[changeTo]] or [[diffHandler]].
      *
      * @param utxo
      *   utxo to spend
      * @param redeemer
      *   redeemer to pass to the script to unlock the inputs
      * @param script
      *   script that protects the `utxo`
      */
    def spend[T: ToData](
        utxo: Utxo,
        redeemer: T,
        script: PlutusScript
    ): TxBuilder = {
        spend(utxo, redeemer, script, Set.empty)
    }

    /** Adds the specified script-protected utxo to the list of inputs and the specified redeemer to
      * the witness set, with additional required signers.
      *
      * Use this method when the validator script requires specific signatures beyond the spender.
      * The public key hashes in `additionalSigners` will be added to the transaction's required
      * signers field.
      *
      * If the specified `script` fails with the specified redeemer, [[build]] is going to throw.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to [[changeTo]] or [[diffHandler]].
      *
      * @param utxo
      *   utxo to spend
      * @param redeemer
      *   redeemer to pass to the script to unlock the inputs
      * @param script
      *   script that protects the `utxo`
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      */
    def spend[T: ToData](
        utxo: Utxo,
        redeemer: T,
        script: PlutusScript,
        requiredSigners: Set[AddrKeyHash]
    ): TxBuilder = {
        val datum = buildDatumWitness(utxo)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemer = redeemer.toData,
          datum = datum,
          additionalSigners = requiredSigners.map(ExpectedSigner.apply)
        )
        addSteps(TransactionBuilderStep.Spend(utxo, witness))
    }

    /** Adds the specified utxos to the list of reference inputs.
      *
      * Reference inputs allow scripts to read UTXOs without consuming them.
      */
    def references(utxo: Utxo, rest: Utxo*): TxBuilder =
        addSteps((utxo :: rest.toList).map(TransactionBuilderStep.ReferenceOutput.apply)*)

    /** Adds the specified utxos to the list of collateral inputs.
      *
      * Collateral inputs are used to cover transaction fees if script execution fails. They are
      * only consumed if a script fails validation.
      */
    def collaterals(utxo: Utxo, rest: Utxo*): TxBuilder =
        addSteps((utxo :: rest.toList).map(TransactionBuilderStep.AddCollateral.apply)*)

    /** Adds the specified utxos to the list of collateral inputs.
      *
      * Collateral inputs are used to cover transaction fees if script execution fails. They are
      * only consumed if a script fails validation.
      */
    def collaterals(utxos: Utxos): TxBuilder =
        addSteps(
          utxos.view.map(utxo => TransactionBuilderStep.AddCollateral(Utxo(utxo))).toSeq*
        )

    /** Adds the specified output to the list of transaction outputs.
      *
      * Use this method for fine-grained control over output construction. For simpler cases, use
      * [[payTo]].
      *
      * @param output
      *   the transaction output to add
      */
    def output(output: TransactionOutput): TxBuilder =
        addSteps(TransactionBuilderStep.Send(output))

    /** Sends the specified value to the given address without a datum.
      *
      * @param address
      *   recipient address
      * @param value
      *   amount to send
      */
    def payTo(address: Address, value: Value): TxBuilder =
        addSteps(
          TransactionBuilderStep.Send(
            TransactionOutput(address, value, None, None)
          )
        )

    /** Sends the specified value to the given address with an inline datum.
      *
      * @param address
      *   recipient address
      * @param value
      *   amount to send
      * @param datum
      *   inline datum to attach to the output
      */
    def payTo[T: ToData](address: Address, value: Value, datum: T): TxBuilder =
        addSteps(
          TransactionBuilderStep.Send(
            TransactionOutput(address, value, Some(DatumOption.Inline(datum.toData)), None)
          )
        )

    /** Sends the specified value to the given address with a datum hash.
      *
      * Make sure to call [[attach]] with the corresponding datum data before calling [[build]].
      *
      * @param address
      *   recipient address
      * @param value
      *   amount to send
      * @param datumHash
      *   hash of the datum (the actual datum must be attached via [[attach]])
      */
    def payTo(address: Address, value: Value, datumHash: DataHash): TxBuilder =
        addSteps(
          TransactionBuilderStep.Send(
            TransactionOutput(address, value, Some(DatumOption.Hash(datumHash)), None)
          )
        )

    /** Attaches a script to the transaction witness set.
      *
      * Use this method to make scripts available for spending script-locked UTXOs or minting
      * tokens. The script will be included in the transaction's witness set.
      *
      * @param script
      *   the script to attach
      */
    def attach(script: Script): TxBuilder =
        copy(attachedScripts = attachedScripts + (script.scriptHash -> script))

    /** Attaches a datum to the transaction witness set.
      *
      * Use this method when spending UTXOs that have datum hashes instead of inline datums. The
      * datum hash is computed automatically and the datum is stored for inclusion in the witness
      * set.
      *
      * @param datum
      *   the datum data to attach
      */
    def attach(datum: Data): TxBuilder = {
        val dataHash = DataHash.fromByteString(blake2b_256(serialiseData(datum)))
        copy(attachedData = attachedData + (dataHash -> datum))
    }

    /** Adds transaction metadata (auxiliary data).
      *
      * Metadata is optional data attached to a transaction that does not affect validation but can
      * be used for off-chain purposes.
      *
      * @param auxiliaryData
      *   the auxiliary data to attach
      */
    def metadata(auxiliaryData: AuxiliaryData): TxBuilder =
        addSteps(
          TransactionBuilderStep.ModifyAuxiliaryData(_ => Some(auxiliaryData))
        )

    /** Mints or burns native tokens under a minting policy.
      *
      * Use positive amounts to mint tokens and negative amounts to burn tokens. The minting policy
      * script should be attached via [[attach]] before calling this method, or it must be present
      * as a reference input.
      *
      * @param redeemer
      *   redeemer to pass to the minting policy script
      * @param policyId
      *   the policy ID (script hash) of the minting policy
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      * @tparam T
      *   type of the redeemer (must have a ToData instance)
      */
    def mint[T: ToData](
        redeemer: T,
        policyId: PolicyId,
        assets: collection.Map[AssetName, Long],
        requiredSigners: Set[AddrKeyHash] = Set.empty
    ): TxBuilder = {

        val scriptSource = attachedScripts.get(policyId) match {
            case Some(ps: PlutusScript) => ScriptSource.PlutusScriptValue(ps)
            case _                      => ScriptSource.PlutusScriptAttached
        }

        val mintSteps = assets.map { case (assetName, amount) =>
            TransactionBuilderStep.Mint(
              scriptHash = policyId,
              assetName = assetName,
              amount = amount,
              witness = TwoArgumentPlutusScriptWitness(
                scriptSource = scriptSource,
                redeemer = redeemer.toData,
                additionalSigners = requiredSigners.map(ExpectedSigner.apply)
              )
            )
        }.toSeq

        addSteps(mintSteps*)
    }

    /** Mints or burns native tokens and attaches the minting policy script in one call.
      *
      * This is a convenience method that combines [[mint]] and [[attach]] for the common case where
      * you have the script available directly.
      *
      * Use positive amounts to mint tokens and negative amounts to burn tokens.
      *
      * @param redeemer
      *   redeemer to pass to the minting policy script
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param script
      *   the minting policy script
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      * @tparam T
      *   type of the redeemer (must have a ToData instance)
      */
    def mintAndAttach[T: ToData](
        redeemer: T,
        assets: collection.Map[AssetName, Long],
        script: PlutusScript,
        requiredSigners: Set[AddrKeyHash] = Set.empty
    ): TxBuilder = {

        val mintSteps = assets.map { case (assetName, amount) =>
            TransactionBuilderStep.Mint(
              scriptHash = script.scriptHash,
              assetName = assetName,
              amount = amount,
              witness = TwoArgumentPlutusScriptWitness(
                scriptSource = ScriptSource.PlutusScriptValue(script),
                redeemer = redeemer.toData,
                additionalSigners = requiredSigners.map(ExpectedSigner.apply)
              )
            )
        }.toSeq

        addSteps(mintSteps*)
    }

    /** Sets a minimum fee for the transaction.
      *
      * This overrides the automatically calculated fee. Use with caution; the actual fee may need
      * to be higher to cover script execution costs.
      *
      * @param minFee
      *   the minimum fee in lovelace
      */
    def minFee(minFee: Coin): TxBuilder =
        addSteps(TransactionBuilderStep.Fee(minFee))

    /** Sets the transaction validity interval using slot numbers.
      *
      * The transaction is only valid for inclusion in a block within the specified slot range. This
      * is important for time-sensitive contracts.
      *
      * @param interval
      *   the validity interval specifying the valid slot range
      */
    def validDuring(interval: ValidityInterval): TxBuilder = {
        val steps = Seq(
          interval.invalidBefore.map(TransactionBuilderStep.ValidityStartSlot.apply),
          interval.invalidHereafter.map(TransactionBuilderStep.ValidityEndSlot.apply)
        ).flatten
        addSteps(steps*)
    }

    /** Sets the earliest time from which the transaction is valid.
      *
      * The timestamp is converted to a slot number using the environment's slot configuration.
      *
      * @param from
      *   the earliest valid time for the transaction
      */
    def validFrom(from: Instant): TxBuilder = {
        val slot = env.slotConfig.timeToSlot(from.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityStartSlot(slot))
    }

    /** Sets the latest time until which the transaction is valid.
      *
      * The timestamp is converted to a slot number using the environment's slot configuration. The
      * transaction becomes invalid after this time.
      *
      * @param to
      *   the latest valid time for the transaction (exclusive)
      */
    def validTo(to: Instant): TxBuilder = {
        val slot = env.slotConfig.timeToSlot(to.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityEndSlot(slot))
    }

    /** Sets a custom diff handler for managing transaction balance.
      *
      * The diff handler is called during [[build]] to handle the difference between consumed inputs
      * and produced outputs (including fees). Use this for custom change handling logic.
      *
      * @param handler
      *   the diff handler function
      * @see
      *   [[changeTo]] for the common case of sending change to an address
      */
    def diffHandler(handler: DiffHandler): TxBuilder =
        copy(diffHandlerOpt = Some(handler))

    /** Sets the change address for the transaction.
      *
      * Any excess value (inputs minus outputs and fees) will be sent to this address. This is
      * equivalent to calling [[diffHandler]] with the standard change handling logic.
      *
      * @param address
      *   the address to send change to
      */
    def changeTo(address: Address): TxBuilder = {
        val handler: DiffHandler = (diff, tx) =>
            Change.handleChange(diff, tx, address, env.protocolParams)
        copy(diffHandlerOpt = Some(handler))
    }

    /** Builds and finalizes the transaction.
      *
      * This method assembles the transaction from all the accumulated steps, calculates fees,
      * handles change, validates and runs all Plutus scripts, and produces a ready-to-sign
      * transaction.
      *
      * A diff handler must be set via [[changeTo]] or [[diffHandler]] before calling this method.
      *
      * @return
      *   a new TxBuilder with the finalized transaction
      * @throws RuntimeException
      *   if no diff handler is set, if script execution fails, or if the transaction cannot be
      *   balanced
      */
    def build(): TxBuilder = {
        val network = env.network
        val params = env.protocolParams
        val handler = this.diffHandlerOpt.getOrElse(
          throw new RuntimeException("Called `build` without setting a diff handler.")
        )
        // Could be a good idea to immediately `modify` on every step, maybe not tho.
        val finalizedContext = for {
            built <- TransactionBuilder.modify(context, steps)
            withAttachments = addAttachmentsToContext(
              built
            ) // TODO: remove after fixes with attachments
            finalized <- withAttachments.finalizeContext(
              params,
              handler,
              evaluator,
              Seq.empty // todo: validators
            )
        } yield finalized

        finalizedContext match {
            case Right(finalized) =>
                copy(context = finalized)
            case Left(error) =>
                throw new RuntimeException(error.reason)
        }
    }

    /** Signs the transaction with the provided signer.
      *
      * This method should be called after [[build]] to add signatures to the transaction. Multiple
      * signers can be applied by chaining sign calls.
      *
      * @param signer
      *   the transaction signer
      * @return
      *   a new TxBuilder with the signed transaction
      */
    def sign(signer: TransactionSigner): TxBuilder = {
        val tx = context.transaction
        val signedTx = signer.sign(transaction)
        copy(context = context.copy(transaction = signedTx))
    }

    /** Returns the current transaction.
      *
      * Call this after [[build]] and optionally [[sign]] to get the final transaction ready for
      * submission.
      */
    def transaction: Transaction = context.transaction

    /** Automatically completes the transaction by selecting UTXOs and collateral.
      *
      * This method queries the provider for available UTXOs at the sponsor address, selects inputs
      * to cover all outputs and fees, selects collateral if needed (for script transactions), and
      * sets up change handling to the sponsor address.
      *
      * After calling this method, you should call [[build]] to finalize the transaction.
      *
      * @param provider
      *   the provider to query for UTXOs
      * @param sponsor
      *   the address to use for input selection, collateral, and change
      * @return
      *   a new TxBuilder with additional spend and collateral steps
      * @throws RuntimeException
      *   if insufficient UTXOs are available to cover the transaction requirements
      */
    def complete(provider: Provider, sponsor: Address): TxBuilder = {
        // TODO: suspiciously similar to the rest of balancing logic that we have, good refactoring candidate
        def doBalance(
            currentSteps: Seq[TransactionBuilderStep],
            iteration: Int
        ): Seq[TransactionBuilderStep] = {
            if iteration >= 10 then
                throw new RuntimeException("Could not balance transaction after 10 iterations")

            val (inputs, outputs, mint, certificates) = extractFieldsFromSteps(currentSteps)

            // draft up a tx without running the scripts to get the fee estimate
            val draftContext = TransactionBuilder
                .build(env.network, currentSteps :+ TransactionBuilderStep.Fee(Coin(0)))
                .getOrElse(throw new RuntimeException("Failed to build draft transaction"))

            val feeEstimate = estimateFee(draftContext.transaction)

            // Calculate consumed and produced values
            val consumed = TxBalance
                .consumedFromFields(
                  inputs.keySet,
                  mint,
                  None,
                  certificates,
                  inputs,
                  CertState.empty,
                  env.protocolParams
                )
                .getOrElse(Value.zero)

            // ensure min ADA
            val adjustedOutputs =
                outputs.map(TransactionBuilder.ensureMinAda(_, env.protocolParams))

            val produced = TxBalance.producedFromFields(
              adjustedOutputs,
              mint,
              feeEstimate,
              certificates,
              None
            )

            val gap = produced - consumed

            // excess produced tokens
            val hasTokensInChange = gap.assets.assets.exists { case (_, assetMap) =>
                assetMap.exists { case (_, amount) => amount < 0 }
            }

            // no excess production
            if gap.coin.value <= 0 && !hasTokensInChange then return currentSteps

            // If we do have excess tokens -- we need to make a change output.
            // Ensure this change output has the tokens and min ADA
            val gapToUse =
                if gap.coin.value <= 0 && hasTokensInChange then {
                    val excess = -gap.coin.value
                    val changeTokens = MultiAsset(
                      gap.assets.assets.map { case (policyId, assetMap) =>
                          policyId -> assetMap.filter(_._2 < 0).map { case (name, amt) =>
                              name -> (-amt)
                          }
                      }
                    )
                    // Calculate actual min ADA for change output with these tokens
                    val estimatedChangeOutput = TransactionOutput(
                      address = sponsor,
                      value = Value(Coin(1_000_000), changeTokens) // mock 1 ada to pad the size
                    )
                    val minAdaRequired =
                        MinCoinSizedTransactionOutput(
                          Sized(estimatedChangeOutput),
                          env.protocolParams
                        )

                    // If we have can cover those, we're done
                    if excess >= minAdaRequired.value then return currentSteps

                    // Otherwise, request exactly the shortfall
                    val shortfall = minAdaRequired.value - excess
                    Value(Coin(shortfall))
                } else gap

            // Find more utxos, but exclude the ones we've already selected
            val newInputSteps =
                selectAdditionalUtxos(provider, sponsor, gapToUse, excludeInputs = inputs.keySet)
            if newInputSteps.isEmpty then
                throw new RuntimeException(s"No UTXOs found to cover gap: $gapToUse")

            doBalance(currentSteps ++ newInputSteps, iteration + 1)
        }

        val balancedSteps = doBalance(steps, 0)

        val collateralSteps = if needsCollateral(steps) && !hasCollateral(steps) then {
            val context = TransactionBuilder
                .build(env.network, balancedSteps)
                .getOrElse(throw new RuntimeException("Failed to build draft for collateral"))
            val feeEstimate = estimateFee(context.transaction)
            val requiredCollateral = estimateRequiredCollateral(context.transaction, feeEstimate)
            selectCollateral(provider, sponsor, requiredCollateral)
        } else Seq.empty

        copy(steps = balancedSteps ++ collateralSteps).changeTo(sponsor)
    }

    /*
    def complete2(provider: Provider, sponsor: Address): TxBuilder = {

        def needsCollateral2(tx: Transaction, resolvedUtxos: ResolvedUtxos): Boolean = {
            AllNeededScriptHashes
                .allNeededScriptHashesView(tx, resolvedUtxos.utxos)
                .map(_.nonEmpty)
                .getOrElse(
                  throw new IllegalStateException("Could not determine needed script hashes")
                )
        }

        def diffHandler(context: TransactionBuilder.Context)(diff: Value, tx: Transaction): Either[TxBalancingError, Transaction] = {
            // TODO:
            // - handle collateral: if scripts and not enough collateral =>
            //      add collateral and return
            //      Right(tx)
            // - if diff > 0 then Change.diffHandler
            // - if diff == 0 then Right(tx)
            //
//            if needsCollateral2(tx, )
        }

        // Could be a good idea to immediately `modify` on every step, maybe not tho.
        val finalizedContext = for {
            built <- TransactionBuilder.modify(context, steps)
            finalized <- built.finalizeContext(
              env.protocolParams,
              diffHandler,
              evaluator,
              Seq.empty // todo: validators
            )
        } yield finalized

        finalizedContext match {
            case Right(finalized) =>
                copy(context = finalized)
            case Left(error) =>
                throw new RuntimeException(error.reason)
        }

    }
     */

    private def estimateFee(tx: Transaction): Coin = {
        val txSize = tx.toCbor.length
        val minFeeA = env.protocolParams.txFeePerByte
        val minFeeB = env.protocolParams.txFeeFixed
        Coin(minFeeA * txSize + minFeeB)
    }

    private def estimateRequiredCollateral(tx: Transaction, fee: Coin): Coin = {
        val collateralPercentage = env.protocolParams.collateralPercentage
        val requiredCollateral = (fee.value * collateralPercentage) / 100
        Coin(requiredCollateral)
    }

    private def extractFieldsFromSteps(
        steps: Seq[TransactionBuilderStep]
    ): (Utxos, Seq[TransactionOutput], Option[MultiAsset], Seq[Certificate]) = {
        val inputs = steps
            .collect {
                case TransactionBuilderStep.Spend(utxo, _)                          => utxo
                case TransactionBuilderStep.SpendWithDelayedRedeemer(utxo, _, _, _) => utxo
            }
            .map(_.toTuple)
            .toMap

        val outputs = steps.collect { case TransactionBuilderStep.Send(out) =>
            out
        }

        // Aggregate mints/burns
        val mintMap = steps
            .collect { case TransactionBuilderStep.Mint(policyId, assetName, amount, _) =>
                (policyId, assetName, amount)
            }
            .groupBy(_._1) // Group by policy ID
            .map { case (policyId, mints) =>
                val assetMap = mints.map { case (_, assetName, amount) =>
                    assetName -> amount
                }.toMap
                policyId -> SortedMap.from(assetMap)
            }

        val mint =
            if mintMap.nonEmpty then Some(MultiAsset(SortedMap.from(mintMap)))
            else None

        val certificates = steps.collect { case TransactionBuilderStep.IssueCertificate(cert, _) =>
            cert
        }

        (inputs, outputs, mint, certificates)
    }

    private def selectAdditionalUtxos(
        provider: Provider,
        address: Address,
        gap: Value,
        excludeInputs: Set[TransactionInput]
    ): Seq[TransactionBuilderStep] = {
        val selectedUtxos = mutable.Map.empty[TransactionInput, TransactionOutput]

        // Fulfill the token requirement first
        if gap.assets.assets.nonEmpty then {
            val tokenUtxos =
                selectUtxosWithTokens(
                  provider,
                  address,
                  gap.assets,
                  excludeInputs ++ selectedUtxos.keySet
                )
            selectedUtxos.addAll(tokenUtxos)
        }

        // Then cover the insufficient ADA, which is now less than it initially was because the token utxos
        // have ADA too.
        if gap.coin.value > 0 then {
            val alreadySelectedAda = selectedUtxos.values.map(_.value.coin.value).sum
            val remainingAdaNeeded = gap.coin.value - alreadySelectedAda

            if remainingAdaNeeded > 0 then {
                // Here, we get all of them, which is bad.
                // Ideally the provider would allow us to exclude utxos.
                val allUtxos = provider
                    .findUtxos(address = address)
                    .getOrElse(Map.empty)
                    .filterNot { case (input, _) =>
                        excludeInputs.contains(input) || selectedUtxos.contains(input)
                    }

                var accumulatedAda = 0L
                val adaUtxos = allUtxos.takeWhile { case (_, output) =>
                    val shouldTake = accumulatedAda < remainingAdaNeeded
                    if shouldTake then accumulatedAda += output.value.coin.value
                    shouldTake
                }

                if accumulatedAda < remainingAdaNeeded then
                    throw new RuntimeException(
                      s"Insufficient ADA: need $remainingAdaNeeded more lovelace, found only $accumulatedAda"
                    )

                selectedUtxos.addAll(adaUtxos)
            }
        }

        selectedUtxos.map { case (input, output) =>
            TransactionBuilderStep.Spend(Utxo(input, output), PubKeyWitness)
        }.toSeq
    }

    /** Selects UTXOs that contain the required native tokens */
    private def selectUtxosWithTokens(
        provider: Provider,
        address: Address,
        requiredAssets: MultiAsset,
        excludeInputs: Set[TransactionInput]
    ): Map[TransactionInput, TransactionOutput] = {
        // Query all UTXOs: might be expensive, think about a better query
        val allUtxos = provider
            .findUtxos(address = address)
            .getOrElse(Map.empty)
            .filterNot { case (input, _) => excludeInputs.contains(input) }

        var selectedUtxos = Map.empty[TransactionInput, TransactionOutput]

        // For each required token, find UTXOs that contain it
        requiredAssets.assets.foreach { case (policyId, assets) =>
            assets.foreach { case (assetName, requiredAmount) =>
                if requiredAmount > 0 then {
                    var collected = 0L

                    // First, count tokens from already-selected UTXOs
                    selectedUtxos.foreach { case (_, output) =>
                        output.value.assets.assets
                            .get(policyId)
                            .flatMap(_.get(assetName))
                            .foreach { amount => collected += amount }
                    }

                    // Then select additional UTXOs if needed
                    if collected < requiredAmount then {
                        allUtxos.foreach { case (input, output) =>
                            if collected < requiredAmount && !selectedUtxos.contains(input) then {
                                output.value.assets.assets
                                    .get(policyId)
                                    .flatMap(_.get(assetName))
                                    .foreach { amount =>
                                        selectedUtxos = selectedUtxos + (input -> output)
                                        collected += amount
                                    }
                            }
                        }
                    }

                    if collected < requiredAmount then {
                        throw new RuntimeException(
                          s"Insufficient tokens: need $requiredAmount of ${policyId.toHex}.${assetName.toString}, found only $collected"
                        )
                    }
                }
            }
        }

        selectedUtxos
    }

    /** Selects a pure ADA UTXO for collateral */
    private def selectCollateral(
        provider: Provider,
        address: Address,
        requiredAmount: Coin
    ): Seq[TransactionBuilderStep] = {
        val collateralUtxo = provider
            .findUtxo(
              address = address,
              minAmount = Some(requiredAmount)
            )
            .getOrElse(
              throw new RuntimeException(
                s"Could not find suitable collateral UTXO (need at least ${requiredAmount.value} lovelace) at address $address"
              )
            )

        if collateralUtxo.output.value.assets.assets.nonEmpty then {
            throw new RuntimeException(
              "Collateral UTXO must contain only ADA, no assets"
            )
        }

        Seq(TransactionBuilderStep.AddCollateral(collateralUtxo))
    }

    /** Checks if any step requires collateral (script spending or minting) */
    private def needsCollateral(steps: Seq[TransactionBuilderStep]): Boolean = {
        steps.exists {
            case TransactionBuilderStep.Spend(_, witness) =>
                witness match {
                    case _: ThreeArgumentPlutusScriptWitness => true
                    case _                                   => false
                }
            case TransactionBuilderStep.SpendWithDelayedRedeemer(_, _, _, _) => true
            case TransactionBuilderStep.Mint(_, _, _, witness) =>
                witness match {
                    case _: TwoArgumentPlutusScriptWitness => true
                    case _                                 => false
                }
            case _ => false
        }
    }

    private def hasCollateral(steps: Seq[TransactionBuilderStep]): Boolean = {
        steps.exists {
            case TransactionBuilderStep.AddCollateral(_) => true
            case _                                       => false
        }
    }

    private def extractScriptHash(utxo: Utxo): ScriptHash = {
        utxo.output.address match {
            case sa: ShelleyAddress =>
                sa.payment match {
                    case s: ShelleyPaymentPart.Script => s.hash
                    case _ =>
                        throw new IllegalArgumentException(
                          s"Cannot spend from non-script address: ${utxo.output.address}"
                        )
                }
            case _ =>
                throw new IllegalArgumentException(
                  s"Cannot spend from non-Shelley address: ${utxo.output.address}"
                )
        }
    }

    private def buildDatumWitness(utxo: Utxo): Datum = {
        utxo.output.datumOption match {
            case None                        => Datum.DatumInlined
            case Some(DatumOption.Inline(_)) => Datum.DatumInlined
            case Some(DatumOption.Hash(datumHash)) =>
                attachedData
                    .get(datumHash)
                    .map(Datum.DatumValue.apply)
                    .getOrElse(Datum.DatumInlined)
        }
    }

    private def addAttachmentsToContext(
        ctx: TransactionBuilder.Context
    ): TransactionBuilder.Context = {
        var updatedTx = ctx.transaction

        attachedScripts.values.foreach {
            case ns: Script.Native =>
                val currentScripts = updatedTx.witnessSet.nativeScripts.toMap.values.toSeq
                updatedTx = updatedTx.copy(
                  witnessSet = updatedTx.witnessSet.copy(
                    nativeScripts = TaggedSortedMap.from(currentScripts :+ ns)
                  )
                )
            case v1: Script.PlutusV1 =>
                val currentScripts = updatedTx.witnessSet.plutusV1Scripts.toMap.values.toSeq
                updatedTx = updatedTx.copy(
                  witnessSet = updatedTx.witnessSet.copy(
                    plutusV1Scripts = TaggedSortedStrictMap.from(currentScripts :+ v1)
                  )
                )
            case v2: Script.PlutusV2 =>
                val currentScripts = updatedTx.witnessSet.plutusV2Scripts.toMap.values.toSeq
                updatedTx = updatedTx.copy(
                  witnessSet = updatedTx.witnessSet.copy(
                    plutusV2Scripts = TaggedSortedStrictMap.from(currentScripts :+ v2)
                  )
                )
            case v3: Script.PlutusV3 =>
                val currentScripts = updatedTx.witnessSet.plutusV3Scripts.toMap.values.toSeq
                updatedTx = updatedTx.copy(
                  witnessSet = updatedTx.witnessSet.copy(
                    plutusV3Scripts = TaggedSortedStrictMap.from(currentScripts :+ v3)
                  )
                )
        }

        if attachedData.nonEmpty then {
            val currentData = updatedTx.witnessSet.plutusData.value.toMap.values.toSeq
            val allData = currentData ++ attachedData.values.map(KeepRaw(_))
            updatedTx = updatedTx.copy(
              witnessSet = updatedTx.witnessSet.copy(
                plutusData = KeepRaw(TaggedSortedMap(allData*))
              )
            )
        }

        ctx.copy(transaction = updatedTx)
    }

    private def addSteps(s: TransactionBuilderStep*) = copy(steps = steps ++ s)
}

/** Factory methods for creating TxBuilder instances. */
object TxBuilder {

    /** Creates a TxBuilder with the default Plutus script evaluator.
      *
      * The evaluator will both validate scripts and compute execution costs for fee calculation.
      *
      * @param env
      *   the environment containing protocol parameters, network info, and slot configuration
      */
    def apply(env: Environment): TxBuilder = {
        val evaluator = PlutusScriptEvaluator(
          CardanoInfo(env.protocolParams, env.network, env.slotConfig),
          EvaluatorMode.EvaluateAndComputeCost
        )
        withCustomEvaluator(env, evaluator)
    }

    /** Creates a TxBuilder with an evaluator that uses constant maximum budget.
      *
      * This is useful for testing or when you want to skip actual script execution cost
      * calculation. The evaluator will still validate scripts but use a fixed cost.
      *
      * @param env
      *   the environment containing protocol parameters, network info, and slot configuration
      */
    def withConstMaxBudgetEvaluator(env: Environment): TxBuilder = {
        val evaluator = PlutusScriptEvaluator.constMaxBudget(env)
        withCustomEvaluator(env, evaluator)
    }

    /** Creates a TxBuilder with a custom Plutus script evaluator.
      *
      * Use this method when you need fine-grained control over script evaluation behavior.
      *
      * @param env
      *   the environment containing protocol parameters, network info, and slot configuration
      * @param evaluator
      *   the custom Plutus script evaluator
      */
    def withCustomEvaluator(
        env: Environment,
        evaluator: PlutusScriptEvaluator
    ): TxBuilder = {
        val context = TransactionBuilder.Context.empty(env.network)
        TxBuilder(env, context, evaluator)
    }
}
