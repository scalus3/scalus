package scalus.bloxbean

import com.bloxbean.cardano.client.api.exception.ApiException
import com.bloxbean.cardano.client.api.model.{EvaluationResult, ProtocolParams, Result, Utxo}
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.api.{TransactionEvaluator, UtxoSupplier}
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.{Transaction, TransactionInput, TransactionOutput}
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import com.bloxbean.cardano.client.util.JsonUtil
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger
import scalus.cardano.ledger.{MajorProtocolVersion, PlutusScriptEvaluationException, PlutusScriptEvaluator, SlotConfig}
import scalus.cardano.onchain.plutus.ScriptContext
import scalus.uplc.eval.ExBudget

import java.math.BigInteger
import java.util
import scala.beans.BeanProperty
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

/** Implements [[com.bloxbean.cardano.client.api.TransactionEvaluator]] to evaluate a transaction to
  * get script costs using Scalus' [[scalus.cardano.ledger.PlutusScriptEvaluator]].
  *
  * @param slotConfig
  *   Slot configuration
  * @param protocolParams
  *   Protocol parameters
  * @param utxoSupplier
  *   Utxo supplier
  * @param scriptSupplier
  *   Additional script supplier
  * @param mode
  *   Evaluator mode.
  *   - [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] will evaluate the transaction
  *     and compute the cost
  *   - [[scalus.bloxbean.EvaluatorMode.VALIDATE]] will validate the transaction and fail if
  *     execution budget exceeds
  * @param debugDumpFilesForTesting
  *   If true, dumps script files for testing purposes
  */
class ScalusTransactionEvaluator(
    @BeanProperty val slotConfig: SlotConfig,
    @BeanProperty val protocolParams: ProtocolParams,
    @BeanProperty val utxoSupplier: UtxoSupplier,
    @BeanProperty val scriptSupplier: ScriptSupplier,
    @BeanProperty val mode: EvaluatorMode,
    @BeanProperty val debugDumpFilesForTesting: Boolean = false
) extends TransactionEvaluator {

    private val utxoResolver = ScalusUtxoResolver(utxoSupplier, scriptSupplier)

    private lazy val plutusScriptEvaluator = PlutusScriptEvaluator(
      slotConfig,
      ledger.ExUnits(
        protocolParams.getMaxTxExMem.toLong,
        protocolParams.getMaxTxExSteps.toLong
      ),
      MajorProtocolVersion(protocolParams.getProtocolMajorVer),
      Interop.getCostModels(protocolParams),
      mode match
          case EvaluatorMode.EVALUATE_AND_COMPUTE_COST =>
              ledger.EvaluatorMode.EvaluateAndComputeCost
          case EvaluatorMode.VALIDATE => ledger.EvaluatorMode.Validate
      ,
      debugDumpFilesForTesting
    )

    /** Constructor with protocol params, utxo supplier, script supplier and mode. Uses
      * [[scalus.cardano.ledger.SlotConfig.mainnet]].
      *
      * @param protocolParams
      * @param utxoSupplier
      * @param scriptSupplier
      * @param mode
      */
    def this(
        protocolParams: ProtocolParams,
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier,
        mode: EvaluatorMode
    ) = this(SlotConfig.mainnet, protocolParams, utxoSupplier, scriptSupplier, mode)

    /** Constructor with protocol params and utxo supplier. Uses
      * [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode and
      * [[scalus.cardano.ledger.SlotConfig.mainnet]].
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      */
    def this(protocolParams: ProtocolParams, utxoSupplier: UtxoSupplier) =
        this(
          SlotConfig.mainnet,
          protocolParams,
          utxoSupplier,
          NoScriptSupplier(),
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    /** Constructor with slot config, protocol params and utxo supplier. Uses
      * [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode.
      * @param slotConfig
      *   Slot configuration
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      */
    def this(slotConfig: SlotConfig, protocolParams: ProtocolParams, utxoSupplier: UtxoSupplier) =
        this(
          slotConfig,
          protocolParams,
          utxoSupplier,
          NoScriptSupplier(),
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    /** Constructor with protocol params, utxo supplier and script supplier. Uses
      * [[scalus.cardano.ledger.SlotConfig.mainnet]] and
      * [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode.
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      * @param scriptSupplier
      *   Additional script supplier
      */
    def this(
        protocolParams: ProtocolParams,
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier
    ) =
        this(
          SlotConfig.mainnet,
          protocolParams,
          utxoSupplier,
          scriptSupplier,
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    /** Constructor with slot config, protocol params, utxo supplier and script supplier. Uses
      * [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode.
      * @param slotConfig
      *   Slot configuration
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      * @param scriptSupplier
      *   Additional script supplier
      */
    def this(
        slotConfig: SlotConfig,
        protocolParams: ProtocolParams,
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier
    ) =
        this(
          slotConfig,
          protocolParams,
          utxoSupplier,
          scriptSupplier,
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    override def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        evaluateTx(transaction.serialize(), inputUtxos)
    }

    override def evaluateTx(
        cbor: Array[Byte],
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        try {
            // Parse CBOR to Scalus Transaction
            val scalusTx = ledger.Transaction.fromCbor(cbor)

            // Convert Bloxbean UTxOs to Scalus format
            val inputUtxosMap = convertInputUtxos(inputUtxos)

            // Resolve all UTxOs using ScalusUtxoResolver
            val scalusUtxos = utxoResolver.resolveUtxos(scalusTx, inputUtxosMap)

            // Evaluate Plutus scripts
            val redeemers = plutusScriptEvaluator.evalPlutusScripts(scalusTx, scalusUtxos)

            // Convert results to Bloxbean format
            val results = redeemers.map(toEvaluationResult).asJava
            Result
                .success("Evaluation successful")
                .asInstanceOf[Result[util.List[EvaluationResult]]]
                .withValue(results)
                .asInstanceOf[Result[util.List[EvaluationResult]]]
        } catch {
            case e: PlutusScriptEvaluationException =>
                Result
                    .error(e.getMessage)
                    .asInstanceOf[Result[util.List[EvaluationResult]]]
            case e: Exception =>
                throw ApiException("Error evaluating transaction", e)
        }
    }

    private def convertInputUtxos(
        inputUtxos: util.Set[Utxo]
    ): Map[ledger.TransactionInput, ledger.TransactionOutput] =
        inputUtxos.asScala.map(utxo => Interop.toUtxoEntry(utxo, scriptSupplier)).toMap

    private def toEvaluationResult(redeemer: ledger.Redeemer): EvaluationResult = {
        EvaluationResult.builder
            .redeemerTag(toBloxbeanRedeemerTag(redeemer.tag))
            .index(redeemer.index)
            .exUnits(
              ExUnits.builder
                  .mem(BigInteger.valueOf(redeemer.exUnits.memory))
                  .steps(BigInteger.valueOf(redeemer.exUnits.steps))
                  .build()
            )
            .build
    }

    private def toBloxbeanRedeemerTag(tag: ledger.RedeemerTag): RedeemerTag = {
        tag match
            case ledger.RedeemerTag.Spend     => RedeemerTag.Spend
            case ledger.RedeemerTag.Mint      => RedeemerTag.Mint
            case ledger.RedeemerTag.Cert      => RedeemerTag.Cert
            case ledger.RedeemerTag.Reward    => RedeemerTag.Reward
            case ledger.RedeemerTag.Voting    => RedeemerTag.Voting
            case ledger.RedeemerTag.Proposing => RedeemerTag.Proposing
    }

    // Deprecated methods for backwards compatibility with LegacyScalusTransactionEvaluator

    /** @deprecated Use the new PlutusScriptEvaluator API instead */
    @deprecated("No longer supported in the new API", "0.14.2")
    @BeanProperty
    lazy val costMdls: CostMdls = ???

    /** @deprecated Use the new PlutusScriptEvaluator API instead */
    @deprecated("No longer supported in the new API", "0.14.2")
    def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo],
        datums: util.List[scalus.uplc.builtin.ByteString],
        txhash: String
    ): Result[util.List[EvaluationResult]] = ???

    /** @deprecated Use the new PlutusScriptEvaluator API instead */
    @deprecated("No longer supported in the new API", "0.14.2")
    def evaluateTxWithContexts(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo],
        datums: util.List[scalus.uplc.builtin.ByteString],
        txhash: String
    ): Either[TxEvaluationException, collection.Seq[(EvaluationResult, ScriptContext)]] = ???
}

/** Implements [[com.bloxbean.cardano.client.api.TransactionEvaluator]] to evaluate a transaction to
  * get script costs using Scalus evaluator. This is a wrapper around
  * [[scalus.bloxbean.TxEvaluator]].
  * @param slotConfig
  *   Slot configuration
  * @param protocolParams
  *   Protocol parameters
  * @param utxoSupplier
  *   Utxo supplier
  * @param scriptSupplier
  *   Additional script supplier
  * @param mode
  *   Evaluator mode.
  *   - [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] will evaluate the transaction
  *     and compute the cost
  *   - [[scalus.bloxbean.EvaluatorMode.VALIDATE]] will validate the transaction and fail if
  *     execution budget exceeds
  */
@deprecated("Use ScalusTransactionEvaluator instead", "0.14.2")
class LegacyScalusTransactionEvaluator(
    @BeanProperty val slotConfig: SlotConfig,
    @BeanProperty val protocolParams: ProtocolParams,
    @BeanProperty val utxoSupplier: UtxoSupplier,
    @BeanProperty val scriptSupplier: ScriptSupplier,
    @BeanProperty val mode: EvaluatorMode,
    @BeanProperty val debugDumpFilesForTesting: Boolean = false
) extends TransactionEvaluator {

    /** Constructor with protocol params, utxo supplier, script supplier and mode. Uses
      * [[scalus.cardano.ledger.SlotConfig.mainnet]].
      *
      * @param protocolParams
      * @param utxoSupplier
      * @param scriptSupplier
      * @param mode
      */
    def this(
        protocolParams: ProtocolParams,
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier,
        mode: EvaluatorMode
    ) = this(SlotConfig.mainnet, protocolParams, utxoSupplier, scriptSupplier, mode)

    /** Constructor with protocol params and utxo supplier. Uses
      * [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode and
      * [[scalus.cardano.ledger.SlotConfig.mainnet]].
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      */
    def this(protocolParams: ProtocolParams, utxoSupplier: UtxoSupplier) =
        this(
          SlotConfig.mainnet,
          protocolParams,
          utxoSupplier,
          NoScriptSupplier(),
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    /** Constructor with slot config, protocol params and utxo supplier. Uses
      * [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode.
      * @param slotConfig
      *   Slot configuration
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      */
    def this(slotConfig: SlotConfig, protocolParams: ProtocolParams, utxoSupplier: UtxoSupplier) =
        this(
          slotConfig,
          protocolParams,
          utxoSupplier,
          NoScriptSupplier(),
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    /** Constructor with protocol params, utxo supplier and script supplier. Uses
      * [[scalus.cardano.ledger.SlotConfig.mainnet]] and
      * [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode.
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      * @param scriptSupplier
      *   Additional script supplier
      */
    def this(
        protocolParams: ProtocolParams,
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier
    ) =
        this(
          SlotConfig.mainnet,
          protocolParams,
          utxoSupplier,
          scriptSupplier,
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    /** Constructor with slot config, protocol params, utxo supplier and script supplier. Uses
      * [[scalus.bloxbean.EvaluatorMode.EVALUATE_AND_COMPUTE_COST]] mode.
      * @param slotConfig
      *   Slot configuration
      * @param protocolParams
      *   Protocol parameters
      * @param utxoSupplier
      *   Utxo supplier
      * @param scriptSupplier
      *   Additional script supplier
      */
    def this(
        slotConfig: SlotConfig,
        protocolParams: ProtocolParams,
        utxoSupplier: UtxoSupplier,
        scriptSupplier: ScriptSupplier
    ) =
        this(
          slotConfig,
          protocolParams,
          utxoSupplier,
          scriptSupplier,
          EvaluatorMode.EVALUATE_AND_COMPUTE_COST
        )

    @BeanProperty
    lazy val costMdls: CostMdls = {
        val cm = CostMdls()
        CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V1)
            .toScala
            .foreach(cm.add)
        CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V2)
            .toScala
            .foreach(cm.add)
        CostModelUtil
            .getCostModelFromProtocolParams(protocolParams, Language.PLUTUS_V3)
            .toScala
            .foreach(cm.add)
        cm
    }

    // Initialize tx evaluator
    private lazy val txEvaluator =
        val txBudget = ExBudget.fromCpuAndMemory(
          cpu = protocolParams.getMaxTxExSteps.toLong,
          memory = protocolParams.getMaxTxExMem.toLong
        )

        TxEvaluator(
          slotConfig,
          txBudget,
          protocolParams.getProtocolMajorVer.intValue(),
          costMdls,
          mode,
          debugDumpFilesForTesting
        )

    private val utxoResolver = CclUtxoResolver(utxoSupplier, scriptSupplier)

    override def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        val datumHashes = for
            ws <- Option(transaction.getWitnessSet)
            dataList <- Option(ws.getPlutusDataList)
        yield dataList
            .stream()
            .map(data => ByteString.fromArray(data.getDatumHashAsBytes))
            .collect(util.stream.Collectors.toList())
        evaluateTx(
          transaction,
          inputUtxos,
          datumHashes.getOrElse(util.List.of()),
          TransactionUtil.getTxHash(transaction)
        )
    }

    def evaluateTx(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo],
        datums: util.List[scalus.uplc.builtin.ByteString],
        txhash: String
    ): Result[util.List[EvaluationResult]] =
        try {
            evaluateTxWithContexts(transaction, inputUtxos, datums, txhash)
                .fold(
                  e =>
                      Result
                          .error(e.getMessage)
                          .asInstanceOf[Result[util.List[EvaluationResult]]],
                  r =>
                      Result
                          .success(JsonUtil.getPrettyJson(r))
                          .asInstanceOf[Result[util.List[EvaluationResult]]]
                          .withValue(r.map(_._1).asJava)
                          .asInstanceOf[Result[util.List[EvaluationResult]]]
                )
        } catch {
            case e: Exception =>
                throw ApiException("Error evaluating transaction", e)
        }

    def evaluateTxWithContexts(
        transaction: Transaction,
        inputUtxos: util.Set[Utxo],
        datums: util.List[scalus.uplc.builtin.ByteString],
        txhash: String
    ): Either[TxEvaluationException, collection.Seq[(EvaluationResult, ScriptContext)]] =
        try {
            val resolvedUtxos: Map[TransactionInput, TransactionOutput] =
                utxoResolver.resolveUtxos(transaction, inputUtxos)

            val redeemers =
                txEvaluator.evaluateTxWithContexts(
                  transaction,
                  resolvedUtxos,
                  datums.asScala,
                  txhash
                )
            val evaluationResults = redeemers.map { case (redeemer, sc) =>
                EvaluationResult.builder
                    .redeemerTag(redeemer.getTag)
                    .index(redeemer.getIndex.intValue)
                    .exUnits(redeemer.getExUnits)
                    .build -> sc
            }
            Right(evaluationResults)
        } catch {
            case e: TxEvaluationException => Left(e)
        }

    override def evaluateTx(
        cbor: Array[Byte],
        inputUtxos: util.Set[Utxo]
    ): Result[util.List[EvaluationResult]] = {
        evaluateTx(Transaction.deserialize(cbor), inputUtxos)
    }
}
