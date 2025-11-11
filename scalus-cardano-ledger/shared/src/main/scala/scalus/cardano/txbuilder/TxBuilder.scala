package scalus.cardano.txbuilder

import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.ledger.*
import scalus.cardano.node.Provider

import java.time.Instant

case class Context(
    env: Environment,
)

case class TxBuilder(
    ctx: Context,
    evaluator: PlutusScriptEvaluator,
    diffHandlerOpt: Option[DiffHandler] = None,
    steps: Seq[TransactionBuilderStep] = Seq.empty,
    attachedScripts: Seq[Script] = Seq.empty,
    attachedData: Seq[Data] = Seq.empty,
    changeAddressOpt: Option[Address] = None,
    providerOpt: Option[Provider] = None,
    builtContext: Option[TransactionBuilder.Context] = None
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
        redeemerBuilder: Transaction => Data,
        validator: PlutusScript,
        datum: Option[Data] = None
    ): Builder = addSteps(
      TransactionBuilderStep.SpendWithDelayedRedeemer(
        utxo,
        redeemerBuilder,
        validator,
        datum
      )
    )

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

    override def mint(mint: Mint): Builder = {
        val mintSteps = mint.assets.toSeq.flatMap { case (scriptHash, assets) =>
            assets.map { case (assetName, amount) =>
                TransactionBuilderStep.Mint(
                  scriptHash,
                  assetName,
                  amount,
                  TwoArgumentPlutusScriptWitness(
                    ScriptSource.PlutusScriptAttached,
                    Data.I(0),
                    Set.empty
                  )
                )
            }
        }
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
        val slot = ctx.env.slotConfig.timeToSlot(from.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityStartSlot(slot))
    }

    override def validTo(to: Instant): Builder = {
        val slot = ctx.env.slotConfig.timeToSlot(to.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityEndSlot(slot))
    }

    override def diffHandler(handler: DiffHandler): Builder =
        copy(diffHandlerOpt = Some(handler))

    override def feePayer(address: Address): Builder =
        copy(changeAddressOpt = Some(address))

    override def changeTo(address: Address): Builder =
        copy(changeAddressOpt = Some(address))

    override def build(): Builder = {
        val network = ctx.env.network
        val params = ctx.env.protocolParams
        val handler = this.diffHandlerOpt.getOrElse(
          throw new RuntimeException("Called `build` without setting a diff handler.")
        )
        val buildResult = TransactionBuilder.build(network, steps)
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
                copy(builtContext = Some(finalized))
            case Left(error) =>
                throw new IllegalStateException(s"Failed to build transaction: $error")
        }
    }

    override def context: TransactionBuilder.Context =
        builtContext.getOrElse {
            val built = build()
            built.context
        }

    override def sign(signers: TxSigner*): Builder = ???

    override def transaction: Transaction =
        builtContext
            .map(_.transaction)
            .getOrElse(
              throw new IllegalStateException("Transaction not built yet. Call build() first.")
            )

    override def provider: Provider =
        providerOpt.getOrElse(
          throw new IllegalStateException("Provider not set. Call provider() first.")
        )

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

trait Builder {
    def spend(utxo: Utxo): Builder
    def spend(
        utxo: Utxo,
        redeemerBuilder: Transaction => Data,
        validator: PlutusScript,
        datum: Option[Data] = None
    ): Builder
    def references(utxos: Utxo*): Builder
    def collaterals(utxos: Utxo*): Builder
    def output(output: TransactionOutput): Builder
    def payTo(address: Address, value: Value): Builder
    def payTo(address: Address, value: Value, datum: Data): Builder
    def payTo(address: Address, value: Value, datumHash: DataHash): Builder
    def attach(script: Script): Builder
    def attach(data: Data): Builder
    def metadata(auxiliaryData: AuxiliaryData): Builder
    def mint(mint: Mint): Builder
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
