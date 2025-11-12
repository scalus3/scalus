package scalus.cardano.txbuilder

import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.Data
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.ledger.*
import scalus.cardano.node.Provider

import java.time.Instant

case class TxBuilder(
    env: Environment,
    context: TransactionBuilder.Context,
    evaluator: PlutusScriptEvaluator,
    diffHandlerOpt: Option[DiffHandler] = None,
    steps: Seq[TransactionBuilderStep] = Seq.empty,
    attachedScripts: Seq[Script] = Seq.empty,
    attachedData: Seq[Data] = Seq.empty,
    changeAddressOpt: Option[Address] = None,
    providerOpt: Option[Provider] = None,
) extends Builder {

    override def spend(utxo: Utxo): Builder =
        addSteps(
          TransactionBuilderStep.Spend(
            utxo,
            PubKeyWitness
          )
        )

    override def spend(
        utxo: Utxo,
        redeemer: Data
    ): Builder = {
        val scriptHash = extractScriptHash(utxo)
        val datum = buildDatumWitness(utxo)

        val scriptSource = attachedScripts
            .collectFirst {
                case ps: PlutusScript if ps.scriptHash == scriptHash =>
                    ScriptSource.PlutusScriptValue(ps)
            }
            .getOrElse(ScriptSource.PlutusScriptAttached)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = scriptSource,
          redeemer = redeemer,
          datum = datum,
          additionalSigners = Set.empty
        )
        addSteps(TransactionBuilderStep.Spend(utxo, witness))
    }

    override def spend(
        utxo: Utxo,
        redeemerBuilder: Transaction => Data
    ): Builder = {
        val scriptHash = extractScriptHash(utxo)
        val datum = buildDatumWitness(utxo)

        val validator = attachedScripts
            .collectFirst {
                case ps: PlutusScript if ps.scriptHash == scriptHash => ps
            }
            .getOrElse(
              throw new IllegalArgumentException(
                s"Validator not found in attachedScripts for script hash: $scriptHash"
              )
            )

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

    override def references(utxos: Utxo*): Builder =
        addSteps(utxos.map(TransactionBuilderStep.ReferenceOutput.apply)*)

    override def collaterals(utxos: Utxo*): Builder =
        addSteps(utxos.map(TransactionBuilderStep.AddCollateral.apply)*)

    override def output(output: TransactionOutput): Builder =
        addSteps(TransactionBuilderStep.Send(output))

    override def payTo(address: Address, value: Value): Builder =
        addSteps(
          TransactionBuilderStep.Send(
            TransactionOutput(address, value, None, None)
          )
        )

    override def payTo(address: Address, value: Value, datum: Data): Builder =
        addSteps(
          TransactionBuilderStep.Send(
            TransactionOutput(address, value, Some(DatumOption.Inline(datum)), None)
          )
        )

    override def payTo(address: Address, value: Value, datumHash: DataHash): Builder =
        addSteps(
          TransactionBuilderStep.Send(
            TransactionOutput(address, value, Some(DatumOption.Hash(datumHash)), None)
          )
        )

    override def attach(script: Script): Builder =
        copy(attachedScripts = attachedScripts :+ script)

    override def attach(data: Data): Builder =
        copy(attachedData = attachedData :+ data)

    override def metadata(auxiliaryData: AuxiliaryData): Builder =
        addSteps(
          TransactionBuilderStep.ModifyAuxiliaryData(_ => Some(auxiliaryData))
        )

    override def mint(
        redeemer: Data,
        policyId: PolicyId,
        assets: collection.Map[AssetName, Long]
    ): Builder = {

        val scriptOpt = attachedScripts.collectFirst {
            case ps: PlutusScript if ps.scriptHash == policyId => ps
        }

        val scriptSource = scriptOpt match {
            case Some(script) => ScriptSource.PlutusScriptValue(script)
            case None         => ScriptSource.PlutusScriptAttached
        }

        val mintSteps = assets.map { case (assetName, amount) =>
            TransactionBuilderStep.Mint(
              scriptHash = policyId,
              assetName = assetName,
              amount = amount,
              witness = TwoArgumentPlutusScriptWitness(
                scriptSource = scriptSource,
                redeemer = redeemer,
                additionalSigners = Set.empty
              )
            )
        }.toSeq

        addSteps(mintSteps*)
    }

    override def minFee(minFee: Coin): Builder =
        addSteps(TransactionBuilderStep.Fee(minFee))

    override def validDuring(interval: ValidityInterval): Builder = {
        val steps = Seq(
          interval.invalidBefore.map(TransactionBuilderStep.ValidityStartSlot.apply),
          interval.invalidHereafter.map(TransactionBuilderStep.ValidityEndSlot.apply)
        ).flatten
        addSteps(steps*)
    }

    override def validFrom(from: Instant): Builder = {
        val slot = env.slotConfig.timeToSlot(from.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityStartSlot(slot))
    }

    override def validTo(to: Instant): Builder = {
        val slot = env.slotConfig.timeToSlot(to.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityEndSlot(slot))
    }

    override def diffHandler(handler: DiffHandler): Builder =
        copy(diffHandlerOpt = Some(handler))

    override def feePayer(address: Address): Builder =
        copy(changeAddressOpt = Some(address))

    override def changeTo(address: Address): Builder =
        copy(changeAddressOpt = Some(address))

    override def build(): Builder = {
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
            case Left(error) =>
                throw new IllegalStateException(s"Failed to build transaction: $error")
        }
    }

    override def sign(signers: TxSigner*): Builder = ???

    override def transaction: Transaction = context.transaction

    override def provider: Provider =
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
                    .find { data =>
                        val computedHash =
                            DataHash.fromByteString(blake2b_224(serialiseData(data)))
                        computedHash == datumHash
                    }
                    .map(Datum.DatumValue.apply)
                    .getOrElse(Datum.DatumInlined)
        }
    }

    private def addAttachmentsToContext(
        ctx: TransactionBuilder.Context
    ): TransactionBuilder.Context = {
        var updatedTx = ctx.transaction

        attachedScripts.foreach {
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
            val allData = currentData ++ attachedData.map(KeepRaw(_))
            updatedTx = updatedTx.copy(
              witnessSet = updatedTx.witnessSet.copy(
                plutusData = KeepRaw(TaggedSortedMap(allData*))
              )
            )
        }

        ctx.copy(transaction = updatedTx)
    }

    private def addSteps(s: TransactionBuilderStep*) =
        copy(steps = steps ++ s)
}

object TxBuilder {
    def apply(env: Environment): TxBuilder = {
        val evaluator = PlutusScriptEvaluator(
          CardanoInfo(env.protocolParams, env.network, env.slotConfig),
          EvaluatorMode.EvaluateAndComputeCost
        )
        val context = TransactionBuilder.Context.empty(env.network)
        TxBuilder(
          env,
          context,
          evaluator
        )
    }
}

trait Builder {
    def spend(utxo: Utxo): Builder
    def spend(utxo: Utxo, redeemer: Data): Builder
    def spend(utxo: Utxo, redeemerBuilder: Transaction => Data): Builder
    def references(utxos: Utxo*): Builder
    def collaterals(utxos: Utxo*): Builder
    def output(output: TransactionOutput): Builder
    def payTo(address: Address, value: Value): Builder
    def payTo(address: Address, value: Value, datum: Data): Builder
    def payTo(address: Address, value: Value, datumHash: DataHash): Builder
    def attach(script: Script): Builder
    def attach(data: Data): Builder
    def metadata(auxiliaryData: AuxiliaryData): Builder
    def mint(redeemer: Data, policyId: PolicyId, assets: collection.Map[AssetName, Long]): Builder
    def minFee(minFee: Coin): Builder
    def validDuring(interval: ValidityInterval): Builder
    def validFrom(from: Instant): Builder
    def validTo(to: Instant): Builder
    def diffHandler(diffHandler: DiffHandler): Builder
    def feePayer(address: Address): Builder
    def changeTo(address: Address): Builder
    def build(): Builder
    def context: TransactionBuilder.Context
    def sign(signers: TxSigner*): Builder
    def transaction: Transaction
    def provider: Provider
}

trait TxSigner
