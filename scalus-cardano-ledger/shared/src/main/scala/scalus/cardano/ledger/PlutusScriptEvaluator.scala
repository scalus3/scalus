package scalus.cardano.ledger

import scalus.builtin.Data.toData
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Language.*
import scalus.cardano.ledger.LedgerToPlutusTranslation.*
import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}
import scalus.ledger
import scalus.ledger.api
import scalus.ledger.api.{v1, v2, v3, ScriptContext}
import scalus.uplc.Term.Const
import scalus.uplc.eval.*
import scalus.uplc.{Constant, DeBruijnedProgram, Term}
import scribe.Logger

import scala.annotation.threadUnsafe
import scala.util.control.NonFatal

enum EvaluatorMode extends Enum[EvaluatorMode] {
    case EvaluateAndComputeCost, Validate
}

class PlutusScriptEvaluationException(
    message: String,
    cause: Throwable,
    val logs: Array[String]
) extends RuntimeException(message, cause) {
    override def toString: String = {
        super.toString + s"\nlogs: ${logs.mkString("\n")}"
    }
}

/** Evaluates Plutus V1, V2 or V3 scripts using the provided transaction and UTxO set.
  *
  * @note
  *   It's an experimental API and may change in future versions, even in patch releases.
  */
trait PlutusScriptEvaluator {

    /** Evaluates Plutus scripts in a transaction.
      *
      * This is the main evaluation orchestrator that:
      *   1. Extracts redeemers from the transaction
      *   2. Builds datum and script lookup tables
      *   3. Evaluates each redeemer sequentially
      *   4. Tracks total budget consumption
      *   5. Returns all evaluated redeemers
      *
      * @param tx
      *   The transaction containing Plutus scripts and redeemers
      * @param utxos
      *   The UTxO set used for script resolution
      * @return
      *   Seq of evaluated redeemers with updated execution units
      * @throws java.lang.IllegalStateException
      *   if the transaction does not contain redeemers or if any script resolution fails
      */
    def evalPlutusScripts(
        tx: Transaction,
        utxos: Utxos,
    ): Seq[Redeemer] = evalPlutusScriptsWithContexts(tx, utxos).map(_._1)

    def evalPlutusScriptsWithContexts(
        tx: Transaction,
        utxos: Utxos,
    ): Seq[(Redeemer, ScriptContext, ScriptHash)]
}

object PlutusScriptEvaluator {

    /** A no-op evaluator that returns empty sequences.
      *
      * Useful for testing scenarios where script evaluation is not needed, such as transaction
      * building tests that don't require actual Plutus script execution.
      */
    @threadUnsafe lazy val noop: PlutusScriptEvaluator = new PlutusScriptEvaluator {
        override def evalPlutusScriptsWithContexts(
            tx: Transaction,
            utxos: Utxos
        ): Seq[(Redeemer, ScriptContext, ScriptHash)] = Seq.empty
    }

    /** Factory method to create a PlutusScriptEvaluator instance that always returns a constant
      * budget.
      *
      * This is useful for testing scenarios where you want to simulate script evaluations without
      * executing actual logic.
      *
      * @param budget
      *   The constant execution units to return for any script evaluation
      * @return
      *   PlutusScriptEvaluator instance that always returns the specified budget
      */
    def const(budget: ExUnits): PlutusScriptEvaluator = apply((_, _, _, _, _) => budget)

    /** Factory method to create a PlutusScriptEvaluator instance that uses the maximum budget
      * defined in Cardano protocol parameters.
      *
      * This is useful for testing scenarios where you want to simulate script evaluations with the
      * maximum allowed resources.
      *
      * @param cardanoInfo
      *   The CardanoInfo containing protocol parameters
      * @return
      *   PlutusScriptEvaluator instance that always returns the maximum budget
      */
    def constMaxBudget(cardanoInfo: CardanoInfo): PlutusScriptEvaluator = const(
      ExUnits(
        cardanoInfo.protocolParams.maxTxExecutionUnits.memory,
        cardanoInfo.protocolParams.maxTxExecutionUnits.steps
      )
    )

    /** Factory method to create a PlutusScriptEvaluator instance with custom evaluation logic.
      *
      * This allows injecting custom evaluation behavior for testing or specialized scenarios.
      *
      * @param evalBudget
      *   Function that computes execution units for a given redeemer and script
      * @return
      *   PlutusScriptEvaluator instance using the provided evaluation function
      */
    def apply(
        evalBudget: (Redeemer, String, PlutusVM, ByteString, Seq[Data]) => ExUnits
    ): PlutusScriptEvaluator = new DefaultImpl(
      CardanoInfo.mainnet.slotConfig,
      ExUnits(
        CardanoInfo.mainnet.protocolParams.maxTxExecutionUnits.memory,
        CardanoInfo.mainnet.protocolParams.maxTxExecutionUnits.steps
      ),
      CardanoInfo.mainnet.majorProtocolVersion,
      CardanoInfo.mainnet.protocolParams.costModels,
      EvaluatorMode.EvaluateAndComputeCost,
      debugDumpFilesForTesting = false
    ) {
        override protected def evalScript(
            redeemer: Redeemer,
            txhash: String,
            vm: PlutusVM,
            script: ByteString,
            args: Data*
        ): Result = {
            Result.Success(
              Term.Error,
              evalBudget(
                redeemer,
                txhash,
                vm,
                script,
                Seq.from(args)
              ),
              Map.empty,
              Seq.empty
            )
        }
    }

    /** Factory method to create a PlutusScriptEvaluator instance.
      *
      * @param slotConfig
      * @param initialBudget
      * @param protocolMajorVersion
      * @param costModels
      * @param mode
      * @param debugDumpFilesForTesting
      * @param logBudgetDifferences
      *   If true, logs differences between original redeemer budgets and computed budgets. Useful
      *   for debugging budget discrepancies. Default is false.
      */
    def apply(
        slotConfig: SlotConfig,
        initialBudget: ExUnits,
        protocolMajorVersion: MajorProtocolVersion,
        costModels: CostModels,
        mode: EvaluatorMode = EvaluatorMode.EvaluateAndComputeCost,
        debugDumpFilesForTesting: Boolean = false,
        logBudgetDifferences: Boolean = false
    ): PlutusScriptEvaluator =
        new DefaultImpl(
          slotConfig,
          initialBudget,
          protocolMajorVersion,
          costModels,
          mode,
          debugDumpFilesForTesting,
          logBudgetDifferences
        )

    /** Creates a PlutusScriptEvaluator from [[CardanoInfo]].
      * @param cardanoInfo
      *   The CardanoInfo containing protocol parameters and slot configuration
      * @param mode
      *   The evaluator mode
      */
    def apply(
        cardanoInfo: CardanoInfo,
        mode: EvaluatorMode
    ): PlutusScriptEvaluator =
        apply(
          cardanoInfo.slotConfig,
          ExUnits(
            cardanoInfo.protocolParams.maxTxExecutionUnits.memory,
            cardanoInfo.protocolParams.maxTxExecutionUnits.steps
          ),
          cardanoInfo.majorProtocolVersion,
          cardanoInfo.protocolParams.costModels,
          mode
        )

    private class DefaultImpl(
        val slotConfig: SlotConfig,
        val initialBudget: ExUnits,
        val protocolMajorVersion: MajorProtocolVersion,
        val costModels: CostModels,
        val mode: EvaluatorMode,
        val debugDumpFilesForTesting: Boolean,
        val logBudgetDifferences: Boolean = false
    ) extends PlutusScriptEvaluator {

        private val log = Logger()
        //        .withHandler(minimumLevel = Some(Level.Debug))

        // Lazy-initialized Plutus VMs for different versions
        // Each VM is configured with version-specific cost models and protocol parameters
        private lazy val plutusV1VM =
            PlutusVM.makePlutusV1VM(
              MachineParams.fromCostModels(
                costModels,
                PlutusV1,
                protocolMajorVersion
              )
            )

        private lazy val plutusV2VM =
            PlutusVM.makePlutusV2VM(
              MachineParams.fromCostModels(
                costModels,
                PlutusV2,
                protocolMajorVersion
              )
            )

        private lazy val plutusV3VM =
            PlutusVM.makePlutusV3VM(
              MachineParams.fromCostModels(
                costModels,
                PlutusV3,
                protocolMajorVersion
              )
            )

        override def evalPlutusScriptsWithContexts(
            tx: Transaction,
            utxos: Utxos,
        ): Seq[(Redeemer, ScriptContext, ScriptHash)] = {
            log.debug(s"Starting Phase 2 evaluation for transaction: ${tx.id}")

            val redeemers = tx.witnessSet.redeemers.map(_.value.toMap).getOrElse(Map.empty)

            // Build datum lookup table with hash mapping
            // According to Babbage spec, we lookup datums only in witness set
            // and do not consider reference input inline datums
            // (getDatum, Figure 3: Functions related to scripts)
            val datumsMapping = tx.witnessSet.plutusData.value.toMap.view.mapValues(_.value).toSeq

            val lookupTable = LookupTable(allResolvedScripts(tx, utxos), datumsMapping.toMap)

            log.debug(
              s"Built lookup table with ${lookupTable.scripts.size} scripts and ${lookupTable.datums.size} datums"
            )

            // Pre-compute TxInfo for each Plutus version to reuse across redeemers
            // This is lazily initialized only if needed
            lazy val txInfoV1 =
                getTxInfoV1(tx, datumsMapping, utxos, slotConfig, protocolMajorVersion)
            lazy val txInfoV2 =
                getTxInfoV2(tx, datumsMapping, utxos, slotConfig, protocolMajorVersion)
            lazy val txInfoV3 =
                getTxInfoV3(tx, datumsMapping, utxos, slotConfig, protocolMajorVersion)

            // Evaluate each redeemer
            var remainingBudget = initialBudget

            val neededScriptsData =
                AllNeededScriptHashes.allNeededScriptData(tx, utxos) match
                    case Right(data) =>
                        data.map { case (tag, index, hash, outputOpt) =>
                            val datum = outputOpt.flatMap(extractDatumFromOutput(_, lookupTable))
                            (tag, index, hash, datum)
                        }
                    case Left(error) => throw error

            val evaluatedRedeemers =
                (for
                    (redeemerTag, index, scriptHash, datum) <- neededScriptsData
                    plutusScript <- lookupTable.scripts.get(scriptHash) match {
                        case Some(plutusScript: PlutusScript) => Some(plutusScript)
                        case Some(other)                      => None
                        case None =>
                            throw new IllegalStateException(s"Script not found: $scriptHash")
                    }
                yield {
                    if redeemerTag == RedeemerTag.Spend then {
                        // V1 and V2 scripts require datums
                        plutusScript match
                            case _: Script.PlutusV1 | _: Script.PlutusV2 =>
                                if datum.isEmpty then
                                    throw new IllegalStateException(
                                      s"Missing required datum for plutus script: $plutusScript"
                                    )
                            case _ => // V3 and Native scripts don't require datums in the traditional sense
                    }

                    val redeemer = redeemers.get((redeemerTag, index)) match
                        case Some(data, exUnits) =>
                            Redeemer(
                              tag = redeemerTag,
                              index = index,
                              data = data,
                              exUnits = exUnits
                            )
                        case None =>
                            throw new IllegalStateException(
                              s"Redeemer not found for tag $redeemerTag and index $index"
                            )

                    val (evaluatedRedeemer, sc) =
                        evalRedeemer(
                          tx,
                          txInfoV1,
                          txInfoV2,
                          txInfoV3,
                          redeemer,
                          plutusScript,
                          datum
                        )

                    // Log execution unit differences for debugging (only if enabled)
                    if logBudgetDifferences && evaluatedRedeemer.exUnits != redeemer.exUnits then
                        val orig = redeemer.exUnits
                        val comp = evaluatedRedeemer.exUnits
                        val memDiff = comp.memory - orig.memory
                        val cpuDiff = comp.steps - orig.steps
                        log.info(
                          s"Budget difference for ${redeemer.tag}[${redeemer.index}]: " +
                              s"original(mem=${orig.memory}, cpu=${orig.steps}) -> " +
                              s"computed(mem=${comp.memory}, cpu=${comp.steps}), " +
                              s"diff(mem=${if memDiff >= 0 then "+" else ""}$memDiff, " +
                              s"cpu=${if cpuDiff >= 0 then "+" else ""}$cpuDiff)"
                        )

                    // Update remaining budget (safe subtraction as evaluation would fail if budget exceeded)
                    remainingBudget = ExUnits(
                      remainingBudget.memory - evaluatedRedeemer.exUnits.memory,
                      remainingBudget.steps - evaluatedRedeemer.exUnits.steps
                    )

                    (evaluatedRedeemer, sc, scriptHash)
                }).toSeq

            log.debug(s"Phase 2 evaluation completed. Remaining budget: $remainingBudget")
            evaluatedRedeemers
        }

        /** Evaluate a single redeemer and its associated script.
          *
          * This is the core evaluation method that:
          *   1. Resolves the script and datum for the redeemer
          *   2. Builds the appropriate script context for the Plutus version
          *   3. Applies the script arguments (datum, redeemer, context)
          *   4. Executes the script using the appropriate Plutus VM
          *   5. Returns the redeemer with computed execution units
          *
          * @param tx
          *   The transaction being evaluated
          * @param txInfoV1
          *   Pre-computed TxInfo for Plutus V1 (lazily initialized)
          * @param txInfoV2
          *   Pre-computed TxInfo for Plutus V2 (lazily initialized)
          * @param txInfoV3
          *   Pre-computed TxInfo for Plutus V3 (lazily initialized)
          * @param redeemer
          *   The redeemer to evaluate
          * @param plutusScript
          *   The Plutus script to execute
          * @param datum
          *   Optional datum for spending scripts
          */
        private def evalRedeemer(
            tx: Transaction,
            txInfoV1: => v1.TxInfo,
            txInfoV2: => v2.TxInfo,
            txInfoV3: => v3.TxInfo,
            redeemer: Redeemer,
            plutusScript: PlutusScript,
            datum: Option[Data]
        ): (Redeemer, ScriptContext) = {
            val result = plutusScript match
                case Script.PlutusV1(script) =>
                    evalPlutusV1Script(tx, txInfoV1, redeemer, script, datum)

                case Script.PlutusV2(script) =>
                    evalPlutusV2Script(tx, txInfoV2, redeemer, script, datum)

                case Script.PlutusV3(script) =>
                    evalPlutusV3Script(tx, txInfoV3, redeemer, script, datum)

            val cost = result._1.budget
            log.debug(s"Evaluation result: $result")

            // Return redeemer with computed execution units
            redeemer.copy(exUnits = cost) -> result._2
        }

        private def extractDatumFromOutput(
            output: TransactionOutput,
            lookupTable: LookupTable
        ): Option[Data] = {
            output.datumOption match
                case Some(DatumOption.Hash(hash))   => lookupTable.datums.get(hash)
                case Some(DatumOption.Inline(data)) => Some(data)
                case None                           => None
        }

        /** Evaluate a Plutus V1 script with the V1 script context.
          *
          * @param tx
          *   The transaction being evaluated
          * @param txInfoV1
          *   Pre-computed TxInfo for Plutus V1
          * @param redeemer
          *   The redeemer to evaluate
          * @param script
          *   The CBOR-encoded script bytecode
          * @param datum
          *   Optional datum for spending scripts
          */
        private def evalPlutusV1Script(
            tx: Transaction,
            txInfoV1: v1.TxInfo,
            redeemer: Redeemer,
            script: ByteString,
            datum: Option[Data]
        ): (Result, v1.ScriptContext) = {
            // Build V1 script context using pre-computed TxInfo
            val purpose = getScriptPurposeV1(tx, redeemer)
            val scriptContext = v1.ScriptContext(txInfoV1, purpose)
            val ctxData = scriptContext.toData
            val txhash = tx.id.toHex

            log.debug(s"Evaluating PlutusV1 script, purpose: $purpose")
            log.debug(s"Datum: ${datum.map(_.toJson)}")
            log.debug(s"Redeemer: ${redeemer.data.toJson}")
            log.debug(s"Script context: ${ctxData.toJson}")

            // Apply script arguments based on whether datum is present
            evalScript(
              redeemer,
              txhash,
              plutusV1VM,
              script,
              datum.toSeq :+ redeemer.data :+ ctxData*
            ) -> scriptContext
        }

        /** Evaluate a Plutus V2 script with the V2 script context.
          *
          * @param tx
          *   The transaction being evaluated
          * @param txInfoV2
          *   Pre-computed TxInfo for Plutus V2
          * @param redeemer
          *   The redeemer to evaluate
          * @param script
          *   The CBOR-encoded script bytecode
          * @param datum
          *   Optional datum for spending scripts
          */
        private def evalPlutusV2Script(
            tx: Transaction,
            txInfoV2: v2.TxInfo,
            redeemer: Redeemer,
            script: ByteString,
            datum: Option[Data]
        ): (Result, v2.ScriptContext) = {
            // Build V2 script context using pre-computed TxInfo
            val purpose = getScriptPurposeV2(tx, redeemer)
            val scriptContext = v2.ScriptContext(txInfoV2, purpose)
            val ctxData = scriptContext.toData
            val txhash = tx.id.toHex

            log.debug(s"Evaluating PlutusV2 script, purpose: $purpose")
            log.debug(s"Datum: ${datum.map(_.toJson)}")
            log.debug(s"Redeemer: ${redeemer.data.toJson}")
            log.debug(s"Script context: ${ctxData.toJson}")

            // Apply script arguments
            evalScript(
              redeemer,
              txhash,
              plutusV2VM,
              script,
              datum.toSeq :+ redeemer.data :+ ctxData*
            ) -> scriptContext
        }

        /** Evaluate a Plutus V3 script with the V3 script context.
          *
          * @param tx
          *   The transaction being evaluated
          * @param txInfoV3
          *   Pre-computed TxInfo for Plutus V3
          * @param redeemer
          *   The redeemer to evaluate
          * @param script
          *   The CBOR-encoded script bytecode
          * @param datum
          *   Optional datum for spending scripts
          */
        private def evalPlutusV3Script(
            tx: Transaction,
            txInfoV3: v3.TxInfo,
            redeemer: Redeemer,
            script: ByteString,
            datum: Option[Data]
        ): (Result, v3.ScriptContext) = {
            // Build V3 script context using pre-computed TxInfo
            val scriptInfo = getScriptInfoV3(tx, redeemer, datum)
            val scriptContext = v3.ScriptContext(txInfoV3, redeemer.data, scriptInfo)
            val ctxData = scriptContext.toData
            val txhash = tx.id.toHex

            log.debug(s"Evaluating PlutusV3 script, scriptInfo: $scriptInfo")
            log.debug(s"Datum: ${datum.map(_.toJson)}")
            log.debug(s"Redeemer: ${redeemer.data.toJson}")
            log.debug(s"Script context: ${ctxData.toJson}")

            // V3 scripts only take the script context as argument
            evalScript(redeemer, txhash, plutusV3VM, script, ctxData) -> scriptContext
        }

        /** Execute a UPLC script with the given arguments.
          *
          * This method handles the low-level script execution:
          *   1. Parses the CBOR-encoded script into a UPLC program
          *   2. Applies the script arguments as constants
          *   3. Executes the program using the appropriate Plutus VM
          *   4. Tracks execution budget and handles budget exhaustion
          *   5. Optionally dumps script data for debugging
          */
        protected def evalScript(
            redeemer: Redeemer,
            txhash: String,
            vm: PlutusVM,
            script: ByteString,
            args: Data*
        ): Result = {
            // Parse UPLC program from CBOR
            val program = DeBruijnedProgram.fromCbor(script.bytes)

            // Apply arguments to the program
            val applied = args.foldLeft(program): (acc, arg) =>
                acc $ Const(Constant.Data(arg))

            // Optional debug dumping
            if debugDumpFilesForTesting then
                dumpScriptForDebugging(applied, redeemer, txhash, vm.language)

            // Create budget spender based on evaluation mode
            val spender = mode match
                case EvaluatorMode.EvaluateAndComputeCost => CountingBudgetSpender()
                case EvaluatorMode.Validate               =>
                    // Create budget from redeemer execution units
                    val budget = ExBudget.fromCpuAndMemory(
                      cpu = redeemer.exUnits.steps,
                      memory = redeemer.exUnits.memory
                    )
                    RestrictingBudgetSpenderWithScriptDump(budget, debugDumpFilesForTesting)

            val logger = Log()
            // Execute the script
            try
                val resultTerm = vm.evaluateScript(applied, spender, logger)
                Result.Success(resultTerm, spender.getSpentBudget, Map.empty, logger.getLogs.toSeq)
            catch
                case e: StackTraceMachineError =>
                    //                println()
                    //                println(s"Script ${vm.language} ${redeemer.tag} evaluation failed: ${e.getMessage}")
                    //                println(e.env.view.reverse.take(20).mkString("\n"))
                    throw new PlutusScriptEvaluationException(e.getMessage, e, logger.getLogs)
                case NonFatal(e) =>
                    throw new PlutusScriptEvaluationException(e.getMessage, e, logger.getLogs)
        }

        /** Dump script information for debugging purposes.
          */
        private def dumpScriptForDebugging(
            program: DeBruijnedProgram,
            redeemer: Redeemer,
            txhash: String,
            language: Language
        ): Unit = {
            val filename = s"script-$txhash-$language-${redeemer.tag}-${redeemer.index}.flat"
            platform.writeFile(filename, program.flatEncoded)
        }

        /** Extract all scripts from transaction and UTxOs.
          */
        private def allResolvedScripts(
            tx: Transaction,
            utxos: Utxos
        ): Map[ScriptHash, Script] =
            AllResolvedScripts.allResolvedScriptsMap(tx, utxos) match
                case Right(allResolvedScriptsMap) => allResolvedScriptsMap
                case Left(error)                  => throw error

    }

    /** Lookup table for resolving scripts and datums during evaluation.
      *
      * @param scripts
      *   Map from script hash to script
      * @param datums
      *   Map from datum hash to datum data
      */
    private case class LookupTable(
        scripts: Map[ScriptHash, Script],
        // cache of `getDatum`
        datums: Map[DataHash, Data]
    )
}
