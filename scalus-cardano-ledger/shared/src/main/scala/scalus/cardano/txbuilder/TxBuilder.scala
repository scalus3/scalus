package scalus.cardano.txbuilder

import scalus.builtin.Builtins.{blake2b_256, serialiseData}
import scalus.builtin.Data.toData
import scalus.builtin.{Data, ToData}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.node.Provider

import java.time.Instant

case class TxBuilder(
    env: Environment,
    context: TransactionBuilder.Context,
    evaluator: PlutusScriptEvaluator,
    diffHandlerOpt: Option[DiffHandler] = None,
    steps: Seq[TransactionBuilderStep] = Seq.empty,
    attachedScripts: Map[ScriptHash, Script] = Map.empty,
    attachedData: Map[DataHash, Data] = Map.empty,
    changeAddressOpt: Option[Address] = None,
    providerOpt: Option[Provider] = None,
) {

    /** Adds the specified **pubkey** utxo to the list of inputs, thus spending it.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to [[changeTo]] or [[diffHandler]].
      * @param utxo
      *   utxo to spend
      * @note
      *   use [[spend]] with utxo **and redeemer** to spend script protected utxos. Otherwise,
      *   [[build]] throws.
      */
    def spend(utxo: Utxo): TxBuilder =
        addSteps(
          TransactionBuilderStep.Spend(
            utxo,
            PubKeyWitness
          )
        )

    /** Adds the specified **pubkey** utxos to the list of inputs, thus spending them.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to [[changeTo]] or [[diffHandler]].
      * @param utxos
      *   utxos to spend
      * @note
      *   use [[spend]] with utxo **and redeemer** to spend script protected utxos. Otherwise,
      *   [[build]] throws.
      */
    def spend(utxos: Utxos): TxBuilder = {
        utxos.foldLeft(this) { case (builder, utxo) => builder.spend(Utxo(utxo)) }
    }

    /** Adds the specified **script protected** utxo to the list of inputs and the specified
      * redeemer to the witness set.
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
      */
    def spend[T: ToData](
        utxo: Utxo,
        redeemer: T
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
          additionalSigners = Set.empty
        )
        addSteps(TransactionBuilderStep.Spend(utxo, witness))
    }

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

    /** Adds the specified **script protected** utxo to the list of inputs and the specified
      * redeemer to the witness set.
      *
      * If the specified `script` fails with the specified redeemer`, [[build]] is going to throw.
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
        val datum = buildDatumWitness(utxo)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemer = redeemer.toData,
          datum = datum,
          additionalSigners = Set.empty
        )
        addSteps(TransactionBuilderStep.Spend(utxo, witness))
    }

    /** Adds the specified **script protected** utxo to the list of inputs and the specified
      * redeemer to the witness set, with additional required signers.
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
      * @param additionalSigners
      *   set of public key hashes that must sign the transaction
      */
    def spend[T: ToData](
        utxo: Utxo,
        redeemer: T,
        script: PlutusScript,
        additionalSigners: Set[AddrKeyHash]
    ): TxBuilder = {
        val datum = buildDatumWitness(utxo)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemer = redeemer.toData,
          datum = datum,
          additionalSigners = additionalSigners.map(ExpectedSigner.apply)
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
        addSteps(utxos.toList.map(utxo => TransactionBuilderStep.AddCollateral.apply(Utxo(utxo)))*)

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

    /** Attaches datum data to the transaction witness set.
      *
      * Use this method when sending to an address with a datum hash (via
      * [[payTo(address:scalus\.cardano\.address\.Address,value:scalus\.cardano\.ledger\.Value,datumHash:scalus\.cardano\.ledger\.DataHash)* payTo]]).
      * The datum will be included in the transaction's witness set and can be referenced by its
      * hash.
      *
      * @param data
      *   the datum to attach
      */
    def attach(data: Data): TxBuilder = {
        val dataHash = DataHash.fromByteString(blake2b_256(serialiseData(data)))
        copy(attachedData = attachedData + (dataHash -> data))
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

    def mint[T: ToData](
        redeemer: T,
        policyId: PolicyId,
        assets: collection.Map[AssetName, Long]
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
                additionalSigners = Set.empty
              )
            )
        }.toSeq

        addSteps(mintSteps*)
    }

    def mint[T: ToData](
        redeemer: T,
        assets: collection.Map[AssetName, Long],
        script: PlutusScript
    ): TxBuilder = {

        val mintSteps = assets.map { case (assetName, amount) =>
            TransactionBuilderStep.Mint(
              scriptHash = script.scriptHash,
              assetName = assetName,
              amount = amount,
              witness = TwoArgumentPlutusScriptWitness(
                scriptSource = ScriptSource.PlutusScriptValue(script),
                redeemer = redeemer.toData,
                additionalSigners = Set.empty
              )
            )
        }.toSeq

        addSteps(mintSteps*)
    }

    def minFee(minFee: Coin): TxBuilder =
        addSteps(TransactionBuilderStep.Fee(minFee))

    def validDuring(interval: ValidityInterval): TxBuilder = {
        val steps = Seq(
          interval.invalidBefore.map(TransactionBuilderStep.ValidityStartSlot.apply),
          interval.invalidHereafter.map(TransactionBuilderStep.ValidityEndSlot.apply)
        ).flatten
        addSteps(steps*)
    }

    def validFrom(from: Instant): TxBuilder = {
        val slot = env.slotConfig.timeToSlot(from.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityStartSlot(slot))
    }

    def validTo(to: Instant): TxBuilder = {
        val slot = env.slotConfig.timeToSlot(to.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityEndSlot(slot))
    }

    def diffHandler(handler: DiffHandler): TxBuilder =
        copy(diffHandlerOpt = Some(handler))

    def changeTo(address: Address): TxBuilder = {
        val handler: DiffHandler = (diff, tx) =>
            Change.handleChange(diff, tx, address, env.protocolParams)
        copy(changeAddressOpt = Some(address), diffHandlerOpt = Some(handler))
    }

    def build(): TxBuilder = {
        val network = env.network
        val params = env.protocolParams
        val handler = this.diffHandlerOpt.getOrElse(
          throw new RuntimeException("Called `build` without setting a diff handler.")
        )
        // Could be a good idea to immediately `modify` on every step, maybe not tho.
        val buildResult = TransactionBuilder.modify(context, steps)
        val finalizedContext = for {
            built <- TransactionBuilder.build(network, steps)
            withAttachments = addAttachmentsToContext(built)
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
            case Left(error) => throw new RuntimeException(error.reason)
        }
    }

    def sign(signer: TransactionSigner, rest: TransactionSigner*): Either[Throwable, TxBuilder] = {
        val utxos = context.getUtxos
        val tx = context.transaction
        val signedByAll =
            (signer +: rest).foldLeft[Either[Throwable, Transaction]](Right(tx)) {
                case (Right(transaction), signer) =>
                    signer.sign(transaction, utxos)
                case (left @ Left(_), _) => left
            }
        signedByAll.map { signedTx =>
            val newContext = context.copy(transaction = signedTx)
            copy(context = newContext)
        }
    }

    def transaction: Transaction = context.transaction

    def provider: Provider =
        providerOpt.getOrElse(
          throw new IllegalStateException("Provider not set. Call provider() first.")
        )

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

object TxBuilder {
    def apply(env: Environment): TxBuilder = {
        val evaluator = PlutusScriptEvaluator(
          CardanoInfo(env.protocolParams, env.network, env.slotConfig),
          EvaluatorMode.EvaluateAndComputeCost
        )
        withCustomEvaluator(env, evaluator)
    }

    def withConstMaxBudgetEvaluator(env: Environment): TxBuilder = {
        val evaluator = PlutusScriptEvaluator.constMaxBudget(env)
        withCustomEvaluator(env, evaluator)
    }

    def withCustomEvaluator(
        env: Environment,
        evaluator: PlutusScriptEvaluator
    ): TxBuilder = {
        val context = TransactionBuilder.Context.empty(env.network)
        TxBuilder(env, context, evaluator)
    }
}
