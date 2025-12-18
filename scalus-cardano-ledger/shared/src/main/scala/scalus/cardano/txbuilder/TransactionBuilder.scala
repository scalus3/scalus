package scalus.cardano.txbuilder

/** This module contains declarative transaction building types and utilities ported from
  * purescript-cardano-transaction-builder with significant modifications and additions.
  *   - The main entry-point: [[TransactionBuilder.build]]
  *   - The definition of steps: [[TransactionBuilderStep]]
  */

import io.bullet.borer.Encoder
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{Context as SContext, State as SState, UtxoEnv}
import scalus.cardano.ledger.utils.{CollateralSufficient, MinCoinSizedTransactionOutput, MinTransactionFee, TxBalance}
import scalus.cardano.txbuilder.Datum.DatumValue
import scalus.cardano.txbuilder.SomeBuildError.{BalancingError, SomeRedeemerIndexingError, SomeStepError, ValidationError}
import scalus.cardano.txbuilder.StepError.*
import scalus.cardano.txbuilder.TransactionBuilder.Context
import scalus.|>

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

// Type alias for compatibility - DiffHandler is now a function type in new Scalus API
type DiffHandler = (Value, Transaction) => Either[TxBalancingError, Transaction]

// Async variant of DiffHandler for use with async UTXO selection (e.g., provider queries)
type DiffHandlerAsync = (Value, Transaction) => Future[Either[TxBalancingError, Transaction]]

case class DelayedRedeemerSpec(
    purpose: RedeemerPurpose,
    redeemerBuilder: Transaction => Data,
    step: TransactionBuilderStep
)

// -----------------------------------------------------------------------------
// Witness
// -----------------------------------------------------------------------------

/** A witness to conduct an authorized operation on-chain. This could be spending an input, minting,
  * rewarding, governance ops, certificate ops, etc.
  *
  * The only ways to do this as of writing (2025-10-03) are
  *   - PubKey
  *   - Native Script
  *   - Plutus Script
  *
  * The types include all additional data required to authorize the operation.
  */
enum WitnessKind:
    case KeyBased
    case ScriptBased

sealed trait Witness:
    def witnessKind: WitnessKind

trait SpendWitness extends Witness

/** Use this value to indicate there will be a signature. The corresponding verification key hash
  * will be tracked automatically in the context.
  */
case object PubKeyWitness extends SpendWitness {
    def witnessKind: WitnessKind = WitnessKind.KeyBased
}

/** Witnesses for native scripts. Can appear several times, but with the same [[additionalSigners]].
  */
case class NativeScriptWitness(
    scriptSource: ScriptSource[Script.Native],
    additionalSigners: Set[ExpectedSigner]
) extends SpendWitness {
    def witnessKind: WitnessKind = WitnessKind.ScriptBased
}

// For operations that only take a redeemer and script context
case class TwoArgumentPlutusScriptWitness(
    scriptSource: ScriptSource[PlutusScript],
    redeemerBuilder: Transaction => Data,
    additionalSigners: Set[ExpectedSigner]
) extends Witness {
    def witnessKind: WitnessKind = WitnessKind.ScriptBased
}

object TwoArgumentPlutusScriptWitness {
    def apply(
        scriptSource: ScriptSource[PlutusScript],
        redeemer: Data,
        additionalSigners: Set[ExpectedSigner]
    ): TwoArgumentPlutusScriptWitness =
        apply(scriptSource, _ => redeemer, additionalSigners)
}

// For operations that take a datum, redeemer, and script context
case class ThreeArgumentPlutusScriptWitness(
    scriptSource: ScriptSource[PlutusScript],
    redeemerBuilder: Transaction => Data,
    datum: Datum,
    additionalSigners: Set[ExpectedSigner]
) extends SpendWitness {
    def witnessKind: WitnessKind = WitnessKind.ScriptBased
}

object ThreeArgumentPlutusScriptWitness {
    def apply(
        scriptSource: ScriptSource[PlutusScript],
        redeemer: Data,
        datum: Datum,
        additionalSigners: Set[ExpectedSigner]
    ): ThreeArgumentPlutusScriptWitness =
        apply(scriptSource, tx => redeemer, datum, additionalSigners)
}

// -----------------------------------------------------------------------------
// ScriptSource
// -----------------------------------------------------------------------------

/** Specifies how the transaction should find the source code for the script.
  */
sealed trait ScriptSource[+A <: Script]

object ScriptSource {

    /** Contains a script itself, will be included to the witness set. */
    case class NativeScriptValue(script: Script.Native) extends ScriptSource[Script.Native]

    /** Tries to use a CIP-33 reference script or a script manually passed in the builder. */
    case object NativeScriptAttached extends ScriptSource[Script.Native]

    case class PlutusScriptValue(script: PlutusScript) extends ScriptSource[PlutusScript]

    case object PlutusScriptAttached extends ScriptSource[PlutusScript]
}

// -----------------------------------------------------------------------------
// Datum
// -----------------------------------------------------------------------------

/** Datums in UTxOs can be stored in two forms: inline datums or datum hashes. When there's a hash,
  * we need to provide a datum corresponding to this hash, which can be done by either providing the
  * value literally, or using a reference input where it is stored inline. The latter is not
  * supported, since we haven't seen it in the wild - you can work with the datum of a
  * reference/other input directly. Please open an issue if you need it.
  */

sealed trait Datum

object Datum {
    case object DatumInlined extends Datum
    case class DatumValue(datum: Data) extends Datum
}

// -----------------------------------------------------------------------------
// ExpectedSigner
// -----------------------------------------------------------------------------

/** An [[scalus.cardano.address.AddrKeyHash]] that is expected to sign some
  * [[scalus.cardano.ledger.Transaction]].
  *
  * The purpose for signing is not presently tracked. For a sketch, see commit
  * https://github.com/cardano-hydrozoa/hydrozoa/commit/1a8c9c73fbfb33e79456a0a8b9f08688ef39b749
  */
case class ExpectedSigner(hash: AddrKeyHash)

// -----------------------------------------------------------------------------
// Transaction Builder
// -----------------------------------------------------------------------------

object TransactionBuilder {

    /** Represents different types of authorized operations (except the spending, which goes
      * separately).
      */
    sealed trait Operation:
        def explain: String

    object Operation {
        case class Minting(scriptHash: PolicyId) extends Operation:
            override def explain: String = "This mint"

        case class CertificateOperation(cert: Certificate) extends Operation:
            override def explain: String = "This stake certificate"

        case class Withdraw(address: StakeAddress) extends Operation:
            override def explain: String = "This stake rewards withdrawal"

        case class Proposing(proposal: ProposalProcedure) extends Operation:
            override def explain: String = "This voting proposal"

        case class Voting(voter: Voter) extends Operation:
            override def explain: String = "Voting procedure"
    }

    /** Calculates the total value of all collateral inputs.
      *
      * This helper is shared between TransactionBuilder and TxBuilder to avoid code duplication.
      *
      * @param tx
      *   the transaction containing collateral inputs
      * @param utxos
      *   resolved UTXOs to look up collateral input values
      * @return
      *   the sum of all collateral input values
      */
    def totalCollateralValue(tx: Transaction, utxos: Utxos): Value = {
        tx.body.value.collateralInputs.toSeq.foldLeft(Value.zero) { case (acc, input) =>
            utxos.get(input).fold(acc)(output => acc + output.value)
        }
    }

    /** A wrapper around a UTxO set that prevents adding conflicting pairs */
    case class ResolvedUtxos private (utxos: Utxos) {

        /**   - If the UTxO does not exist in the map, add it.
          *   - If the UTxO exists in the map with a different output associated, return None
          *   - If the UTxO exists in the map with the same output, return the map unmodified
          */
        def addUtxo(utxo: Utxo): Option[ResolvedUtxos] =
            utxos.get(utxo.input) match {
                case None => Some(ResolvedUtxos(utxos + utxo.toTuple))
                case Some(existingOutput) =>
                    if existingOutput == utxo.output
                    then Some(ResolvedUtxos(utxos))
                    else None
            }

        /** Tries to add multiple UTxOs, returning invalid additions. See [[addUtxo]] */
        def addUtxos(
            utxos: Seq[Utxo]
        ): Either[Seq[Utxo], ResolvedUtxos] = {
            val res: (Seq[Utxo], ResolvedUtxos) =
                utxos.foldLeft((Seq.empty[Utxo], this))((acc, utxo) =>
                    acc._2.addUtxo(utxo) match {
                        case Some(newResolved) => (acc._1, newResolved)
                        case None              => (acc._1.appended(utxo), acc._2)
                    }
                )
            if res._1.isEmpty
            then Right(res._2)
            else Left(res._1)
        }
    }

    object ResolvedUtxos:
        val empty: ResolvedUtxos = ResolvedUtxos(Map.empty)
        def apply(utxos: Utxos): ResolvedUtxos = new ResolvedUtxos(utxos)

    /** An opaque context in which the builder operates.
      *
      * TODO: make a class, remove toTuple()?
      */
    case class Context(
        transaction: Transaction,
        redeemers: Seq[DetachedRedeemer],
        network: Network,
        expectedSigners: Set[ExpectedSigner],
        /** Invariants:
          *   - The union of transaction.body.value.inputs, transaction.body.value.referenceInputs,
          *     and transaction.body.value.collateralInputs must exactly match resolvedUtxos.inputs
          */
        resolvedUtxos: ResolvedUtxos,
        delayedRedeemerSpecs: Seq[DelayedRedeemerSpec] = Seq.empty,
        /** Optional address for collateral return output. If not set, defaults to first collateral
          * input's address when collateral return is needed.
          */
        collateralReturnAddress: Option[Address] = None
    ) {

        /** Extract tupled information from a Context. This method is provided to avoid breaking
          * opacity while making it easier to check for equality in testing. Note:
          * delayedRedeemerSpecs is excluded since it contains lambdas that can't be compared.
          */
        val toTuple: (
            Transaction,
            Seq[DetachedRedeemer],
            Network,
            Set[ExpectedSigner],
            ResolvedUtxos
        ) = (
          this.transaction,
          this.redeemers,
          this.network,
          this.expectedSigners,
          this.resolvedUtxos
        )

        /** Add additional signers to the Context.
          */
        def addSigners(additionalSigners: Set[ExpectedSigner]): Context = {
            this |> Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners)
        }

        def replaceRedeemers(newRedeemers: Seq[DetachedRedeemer]): Context = {
            this |> Focus[Context](_.redeemers).replace(newRedeemers)
        }

        def addDelayedRedeemer(spec: DelayedRedeemerSpec): Context = {
            this |> Focus[Context](_.delayedRedeemerSpecs).modify(_ :+ spec)
        }

        /** Ensure that all transaction outputs in the context have min ada. */
        def ensureMinAdaAll(protocolParams: ProtocolParams): Context = {
            this |> unsafeCtxBodyL
                .refocus(_.outputs)
                .modify(os =>
                    os.map((to: Sized[TransactionOutput]) =>
                        Sized(ensureMinAda(to.value, protocolParams))
                    )
                )
        }

        /** Balance the transaction in a context, adding and removing mock signatures where
          * necessary.
          */
        def balance(
            // TODO: @Ilia leave comment about not messing with inputs, etc. If your diff handler
            // adds or removes components needing signatures, the fees won't be calculated correctly.
            // It also won't update .resolvedUtxos.
            // TODO: @Ilia Wrap this so that we can only modify the transaction outputs. Basically inject
            // a (Coin, Set[TransactionOutput]) => Either[TxBalancingError, Set[TransactionOutput]]
            // into a DiffHandler
            diffHandler: DiffHandler,
            protocolParams: ProtocolParams,
            evaluator: PlutusScriptEvaluator
        ): Either[TxBalancingError, Context] = {
            // println(s"txWithDummySignatures=${HexUtil.encodeHexString(txWithDummySignatures.toCbor)}")

            for {
                balanced <- balanceFeeAndChangeWithTokens(
                  initial = this.transaction,
                  diffHandler = diffHandler,
                  protocolParams = protocolParams,
                  resolvedUtxo = this.getUtxos,
                  evaluator = evaluator
                )

                // _ = println(HexUtil.encodeHexString(txWithoutDummySignatures.toCbor))
            } yield copy(transaction = balanced)
        }

        /** Conversion help to Scalus [[scalus.cardano.ledger.Utxos]] */
        def getUtxos: Utxos = this.resolvedUtxos.utxos

        /** Validate a context according so a set of ledger rules */
        def validate(
            validators: Seq[Validator],
            protocolParams: ProtocolParams
        ): Either[TransactionException, Context] = {
            val certState = CertState.empty
            val context = SContext(
              this.transaction.body.value.fee,
              UtxoEnv(1L, protocolParams, certState, network)
            )
            val state = SState(this.getUtxos, certState)
            validators
                .map(_.validate(context, state, this.transaction))
                .collectFirst { case l: Left[?, ?] => l.value }
                .toLeft(this)
        }

        /** Set min ada, balance, and validate a context. TODO: @Ilia consider putting PP,
          * evaluator, and validators, into the parameters for the transaction builder class
          */
        def finalizeContext(
            protocolParams: ProtocolParams,
            diffHandler: DiffHandler,
            evaluator: PlutusScriptEvaluator,
            validators: Seq[Validator]
        ): Either[SomeBuildError, Context] = {
            val txWithDummySignatures: Transaction =
                addDummySignatures(this.expectedSigners.size, this.transaction)
            val contextWithSignatures = this.copy(transaction = txWithDummySignatures)

            // Create a combined diff handler that also handles collateral return
            // This ensures collateral return is set within the balancing loop,
            // so any size increase is accounted for in fee calculation
            val combinedDiffHandler: DiffHandler = (diff, tx) => {
                for {
                    afterDiff <- diffHandler(diff, tx)
                    afterCollateral <- TransactionBuilder.ensureCollateralReturn(
                      afterDiff,
                      this.resolvedUtxos.utxos,
                      this.collateralReturnAddress,
                      protocolParams
                    )
                } yield afterCollateral
            }

            for {
                balancedCtx <- contextWithSignatures
                    .ensureMinAdaAll(protocolParams)
                    .balance(combinedDiffHandler, protocolParams, evaluator)
                    .left
                    .map(BalancingError(_, this))

                validatedCtx <- balancedCtx
                    .validate(validators, protocolParams)
                    .left
                    .map(ValidationError(_, this))

                validatedCtxWithoutSignatures = validatedCtx.copy(
                  transaction =
                      removeDummySignatures(this.expectedSigners.size, validatedCtx.transaction)
                )

            } yield validatedCtxWithoutSignatures
        }

    }

    object Context {
        def empty(networkId: Network) = Context(
          transaction = Transaction.empty,
          redeemers = Seq.empty,
          network = networkId,
          expectedSigners = Set.empty,
          resolvedUtxos = ResolvedUtxos.empty,
          delayedRedeemerSpecs = Seq.empty
        )
    }

    val unsafeCtxBodyL: Lens[Context, TransactionBody] = {
        Focus[Context](_.transaction) >>> txBodyL
    }

    val unsafeCtxWitnessL: Lens[Context, TransactionWitnessSet] =
        Focus[Context](_.transaction).refocus(_.witnessSet)

    /** Modifications of tx's outputs (so far) is relatively "safe" operation in terms that it can't
      * break the transaction validity as long as outputs are correct. Moreover, the DiffHandler to
      * some extend does the same thing, so this lens is the only way to manually edit the tx'
      * outputs in the context, which may be useful together with [[modify]].
      */
    val unsafeCtxTxOutputsL: Lens[Context, IndexedSeq[Sized[TransactionOutput]]] =
        Focus[Context](_.transaction) >>> txOutputsL

    /** Hydrozoa use case: tx upgrade that requires promoting a reference input into a spent input.
      */
    val unsafeCtxTxReferenceInputsL: Lens[Context, TaggedSortedSet[TransactionInput]] =
        Focus[Context](_.transaction) >>> txReferenceInputsL

    /** Update the given transaction output to have the minimum required ada, only changing its
      * Coin.
      */

    def ensureMinAda(
        candidateOutput: TransactionOutput,
        params: ProtocolParams
    ): TransactionOutput = {
        val minAda = MinCoinSizedTransactionOutput.ensureMinAda(Sized(candidateOutput), params)
        if candidateOutput.value.coin < minAda
        then candidateOutput |> TransactionOutput.valueLens.refocus(_.coin).replace(minAda)
        else candidateOutput
    }

    /** Build a transaction from scratch, starting with an "empty" transaction and no signers. */
    def build(
        network: Network,
        steps: Seq[TransactionBuilderStep]
    ): Either[SomeBuildError, Context] =
        modify(Context.empty(network), steps)

    /** Modify a transaction within a context. */
    def modify(
        ctx: Context,
        steps: Seq[TransactionBuilderStep]
    ): Either[SomeBuildError, Context] = {

        val stepsProcessor = new TransactionStepsProcessor(ctx)
        // context at either the time of computation termination -- either success or first error
        val (finalContext, result) = stepsProcessor.applySteps(steps)
        result match {
            case Left(error) =>
                val buildError = error match {
                    case e: StepError => SomeStepError(e, finalContext)
                    case e: RedeemerIndexingInternalError =>
                        SomeRedeemerIndexingError(e, finalContext)
                }
                Left(buildError)
            case Right(_) =>
                Right(finalContext)
        }
    }

    def replaceDelayedRedeemers(
        redeemers: Seq[DetachedRedeemer],
        specs: Seq[DelayedRedeemerSpec],
        sortedTx: Transaction
    ): Either[StepError, Seq[DetachedRedeemer]] = {
        specs.foldLeft[Either[StepError, Seq[DetachedRedeemer]]](Right(redeemers)) { (acc, spec) =>
            acc.flatMap { currentRedeemers =>
                try {
                    val realRedeemerData = spec.redeemerBuilder(sortedTx)
                    val updated = currentRedeemers.map {
                        case dr @ DetachedRedeemer(_, purpose) if purpose == spec.purpose =>
                            dr.copy(datum = realRedeemerData)
                        case other => other
                    }
                    Right(updated)
                } catch {
                    case e: Exception =>
                        Left(RedeemerComputationFailed(e.getMessage, spec.step))
                }
            }
        }
    }

    // -------------------------------------------------------------------------
    // Transaction Modification Helpers
    // -------------------------------------------------------------------------

    def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody): Transaction = {
        val newBody = f(tx.body.value)
        tx.copy(body = KeepRaw(newBody))
    }

    def modifyWs(
        tx: Transaction,
        f: TransactionWitnessSet => TransactionWitnessSet
    ): Transaction = {
        val newWs = f(tx.witnessSet)
        tx.copy(witnessSet = newWs)
    }

    /** Ensure collateral return output is set when beneficial.
      *
      * Per Babbage spec (Figure 4), if script validation FAILS without collateralReturnOutput, ALL
      * collateral ADA is taken as fees. With collateralReturnOutput set, only the difference
      * (inputs - return) is taken as fees, and the return output is created.
      *
      * This method creates a collateral return output when:
      *   1. Collateral contains tokens (MUST be returned per protocol)
      *   2. Excess ADA above required collateral can cover min ADA for return output
      *
      * This prevents users from losing their entire collateral UTXO if a script fails.
      *
      * This is the transaction-level version used within the balancing loop.
      */
    def ensureCollateralReturn(
        tx: Transaction,
        resolvedUtxos: Utxos,
        collateralReturnAddress: Option[Address],
        protocolParams: ProtocolParams
    ): Either[TxBalancingError, Transaction] = {
        val collateralInputs = tx.body.value.collateralInputs.toSeq
        if collateralInputs.isEmpty then return Right(tx)

        // Get first collateral input and its UTXO (invariant: all collateral inputs are in resolvedUtxos)
        val firstCollateralInput = collateralInputs.head
        val firstCollateralUtxo = resolvedUtxos.getOrElse(
          firstCollateralInput,
          throw new IllegalStateException(
            s"Collateral input $firstCollateralInput not found in resolvedUtxos. " +
                "This indicates a bug in transaction building - all collateral inputs should be resolved."
          )
        )

        // Calculate total collateral value
        val totalCollateralVal = TransactionBuilder.totalCollateralValue(tx, resolvedUtxos)

        // Calculate required collateral
        val requiredCollateral = CollateralSufficient.calculateRequiredCollateral(
          tx.body.value.fee,
          protocolParams.collateralPercentage
        )

        // Determine return address - use explicit setting or default to first collateral's address
        val returnAddr: Address = collateralReturnAddress.getOrElse(firstCollateralUtxo.address)

        // Check if tokens are present (MUST have return output)
        val hasTokens = totalCollateralVal.assets.nonEmpty

        // Calculate potential return value
        val potentialReturnAda = totalCollateralVal.coin.value - requiredCollateral.value
        val potentialReturnValue = Value(Coin(potentialReturnAda), totalCollateralVal.assets)
        val minAdaForReturn = MinCoinSizedTransactionOutput
            .computeMinAda(
              Sized(TransactionOutput(returnAddr, potentialReturnValue)),
              protocolParams
            )
            .value

        // Decide whether to create return output:
        // 1. If tokens present: MUST create return output (error if insufficient ADA)
        // 2. If ADA-only: create return output only if excess ADA >= minAda for return
        if hasTokens && potentialReturnAda < minAdaForReturn then
            // Tokens present but not enough ADA for valid return output
            return Left(
              TxBalancingError.InsufficientCollateralForReturn(
                totalCollateralAda = totalCollateralVal.coin,
                requiredCollateral = requiredCollateral,
                minAdaForReturn = Coin(minAdaForReturn)
              )
            )

        val shouldCreateReturn = hasTokens || potentialReturnAda >= minAdaForReturn

        if !shouldCreateReturn then return Right(tx)

        // Create return output with the available ADA (already validated sufficient if tokens present)
        val returnAda = potentialReturnAda
        val actualTotalCollateral = Coin(totalCollateralVal.coin.value - returnAda)

        val returnValue = Value(Coin(returnAda), totalCollateralVal.assets)
        val returnOutput = TransactionOutput(returnAddr, returnValue)

        val newTx = modifyBody(
          tx,
          _.copy(
            collateralReturnOutput = Some(Sized(returnOutput)),
            totalCollateral = Some(actualTotalCollateral)
          )
        )
        Right(newTx)
    }

    def setFee(amount: Coin)(tx: Transaction): Transaction = modifyBody(tx, _.copy(fee = amount))

    def calculateChangeValue(tx: Transaction, utxo: Utxos, params: ProtocolParams): Value = {
        val produced = TxBalance.produced(tx, params)
        val consumed = TxBalance.consumed(tx, CertState.empty, utxo, params).toTry.get
        consumed - produced
    }

    // -------------------------------------------------------------------------
    // Transaction Balancing
    // -------------------------------------------------------------------------

    /** Balances the transaction using a diff handler to adjust the transaction.
      *
      * Invariants:
      *   - both ADA and native tokens are adjusted by the diff handler
      *   - fees never go below the initial fee
      */
    def balanceFeeAndChange(
        initial: Transaction,
        changeOutputIdx: Int,
        protocolParams: ProtocolParams,
        resolvedUtxo: Utxos,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] = {
        balanceFeeAndChangeWithTokens(
          initial,
          ChangeOutputDiffHandler(protocolParams, changeOutputIdx).changeOutputDiffHandler,
          protocolParams,
          resolvedUtxo,
          evaluator
        )
    }

    /** Balances the transaction using a diff handler to adjust the transaction.
      *
      * Invariants:
      *   - both ADA and native tokens are adjusted by the diff handler
      *   - fees never go below the initial fee
      *
      * @param resolvedUtxo
      *   The resolved UTXOs for inputs in the transaction.
      */
    def balanceFeeAndChangeWithTokens(
        initial: Transaction,
        diffHandler: (Value, Transaction) => Either[TxBalancingError, Transaction],
        protocolParams: ProtocolParams,
        resolvedUtxo: Utxos,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] = {
        var iteration = 0

        @tailrec def loop(tx: Transaction): Either[TxBalancingError, Transaction] = {
            iteration += 1
            if iteration > 20 then return Left(TxBalancingError.BalanceDidNotConverge(iteration))

            val eTrialTx = for {
                txWithExUnits <- computeScriptsWitness(resolvedUtxo, evaluator, protocolParams)(tx)
                minFee <- MinTransactionFee
                    .ensureMinFee(txWithExUnits, resolvedUtxo, protocolParams)
                    .left
                    .map(TxBalancingError.Failed(_))
                // Don't go below initial fee
                fee = Coin(math.max(minFee.value, initial.body.value.fee.value))
                txWithFees = setFee(fee)(txWithExUnits)
                diff = calculateChangeValue(txWithFees, resolvedUtxo, protocolParams)
                // try to balance it
                balanced <- diffHandler(diff, txWithFees)
            } yield balanced
            eTrialTx match {
                case Left(e)                         => Left(e)
                case Right(trialTx) if tx == trialTx => Right(tx)
                case Right(trialTx)                  => loop(trialTx)
            }
        }
        loop(initial)
    }

    private[txbuilder] def computeScriptsWitness(
        utxos: Utxos,
        evaluator: PlutusScriptEvaluator,
        protocolParams: ProtocolParams
    )(tx: Transaction): Either[TxBalancingError, Transaction] = Try {
        val redeemers = evaluator.evalPlutusScripts(tx, utxos)
        setupRedeemers(protocolParams, tx, utxos, redeemers)
    }.toEither.left.map {
        case psee: PlutusScriptEvaluationException => TxBalancingError.EvaluationFailed(psee)
        case other                                 => TxBalancingError.Failed(other)
    }

    private def setupRedeemers(
        protocolParams: ProtocolParams,
        tx: Transaction,
        utxos: Utxos,
        redeemers: Seq[Redeemer]
    ): Transaction = {
        val txWithRedeemers =
            if redeemers.nonEmpty then
                val rawRedeemers = KeepRaw(Redeemers.from(redeemers))
                tx.copy(witnessSet = tx.witnessSet.copy(redeemers = Some(rawRedeemers)))
            else tx

        val scriptDataHash =
            ScriptDataHashGenerator
                .computeScriptDataHash(
                  txWithRedeemers,
                  utxos,
                  protocolParams
                )
                .toOption
                .get

        if scriptDataHash.nonEmpty then
            txWithRedeemers.copy(body =
                KeepRaw(tx.body.value.copy(scriptDataHash = scriptDataHash))
            )
        else txWithRedeemers
    }
}

/** Transaction balancing error types.
  *
  * These errors can occur during the transaction balancing process, which iteratively adjusts fees
  * and change outputs until the transaction is balanced (consumed == produced).
  */
enum TxBalancingError {

    /** Plutus script evaluation failed during balancing.
      *
      * This occurs when a Plutus validator or minting policy fails execution. The cause contains
      * detailed error information including execution logs.
      *
      * @param cause
      *   the Plutus script evaluation exception with logs and error details
      */
    case EvaluationFailed(cause: PlutusScriptEvaluationException)

    /** Generic failure during balancing.
      *
      * Catches unexpected errors that don't fit other categories, such as CBOR encoding issues or
      * internal errors.
      *
      * @param cause
      *   the underlying exception
      */
    case Failed(cause: Throwable)

    /** Balancing loop exceeded maximum iterations without converging.
      *
      * This typically indicates a pathological case where fee adjustments and change calculations
      * keep oscillating. The transaction structure may need to be simplified.
      *
      * @param iterations
      *   the number of iterations attempted before giving up
      */
    case BalanceDidNotConverge(iterations: Int)

    /** Insufficient funds to balance the transaction.
      *
      * This error indicates that the transaction outputs (including fees) exceed what the inputs
      * can provide. The `valueDiff` contains the full deficit including both ADA and native tokens.
      *
      * When `valueDiff` has negative values, those represent the amounts needed:
      *   - Negative ADA means more lovelace is needed
      *   - Negative token amounts mean more of those tokens are needed
      *
      * @param valueDiff
      *   the value difference (consumed - produced). Negative values indicate deficit.
      * @param minRequired
      *   minimum additional lovelace needed, accounting for minAda requirements on change outputs
      */
    case InsufficientFunds(valueDiff: Value, minRequired: Long)

    /** Error when collateral contains tokens but there's insufficient ADA to create a valid
      * collateral return output.
      *
      * Per Babbage spec, tokens in collateral MUST be returned via collateralReturnOutput, which
      * requires meeting the minAda requirement. This error occurs when:
      * `totalCollateralAda < requiredCollateral + minAdaForReturn`
      *
      * @param totalCollateralAda
      *   total ADA in all collateral inputs
      * @param requiredCollateral
      *   ADA needed for collateral (percentage of fee)
      * @param minAdaForReturn
      *   minimum ADA needed for the collateral return output
      */
    case InsufficientCollateralForReturn(
        totalCollateralAda: Coin,
        requiredCollateral: Coin,
        minAdaForReturn: Coin
    )
}

// -------------------------------------------------------------------------
// auxiliary types, extensions, helpers
// -------------------------------------------------------------------------

@deprecated("Use scalus.cardano.ledger.Utxo instead", "0.13.0")
type TransactionUnspentOutput = Utxo
@deprecated("Use scalus.cardano.ledger.Utxo instead", "0.13.0")
val TransactionUnspentOutput = Utxo

// NOTE (Peter, 2025-09-23): this comes from https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/StakeCredential.purs
case class StakeCredential(credential: Credential)

extension (network: Network)
    @deprecated("Use Network.networkId instead", "0.13.0")
    def toNetworkId: Int = network.networkId.toInt

object NetworkExtensions:
    /** Convert integer network ID to Network */
    @deprecated("Use Network.fromNetworkId instead", "0.13.0")
    def fromNetworkId(networkId: Int): Option[Network] = networkId match
        case 0                      => Some(Network.Testnet)
        case 1                      => Some(Network.Mainnet)
        case v if v >= 2 && v <= 15 => Some(Network.Other(v.toByte))
        case _                      => None

/** Append an element to a sequence, returning distinct values only and preserving the order of
  * elements.
  */
def appendDistinct[A](elem: A, seq: Seq[A]): Seq[A] =
    seq.appended(elem).distinct

/** These are the sum type for any errors that may occur during different phases and that can be
  * returned thrown by a higher-level TxBuilder
  */
enum SomeBuildError:
    case SomeStepError(e: StepError, context: Context)
    case SomeRedeemerIndexingError(e: RedeemerIndexingInternalError, context: Context)
    // case EvaluationError(e: PlutusScriptEvaluationException)
    case BalancingError(e: TxBalancingError, context: Context)
    case ValidationError(e: TransactionException, context: Context)

    /** Error when collateral contains tokens but there's insufficient ADA to create a valid
      * collateral return output. Per Babbage spec, tokens in collateral MUST be returned via
      * collateralReturnOutput, which requires meeting the minAda requirement.
      */
    case InsufficientCollateralForReturn(
        totalCollateralAda: Coin,
        requiredCollateral: Coin,
        minAdaForReturn: Coin,
        context: Context
    )

    def reason: Throwable =
        this match {
            case SomeBuildError.SomeStepError(e, context) =>
                new RuntimeException(
                  s"Step error: ${e.explain}"
                )

            case SomeBuildError.SomeRedeemerIndexingError(e, context) =>
                new RuntimeException(
                  s"Redeemer indexing error: ${e}"
                )
            case SomeBuildError.BalancingError(e, context) =>
                e match {
                    case TxBalancingError.EvaluationFailed(cause) =>
                        new RuntimeException(
                          s"Plutus script evaluation failed. Logs: ${cause.logs.mkString(System.lineSeparator)}",
                          cause
                        )
                    case TxBalancingError.Failed(cause) => cause
                    case TxBalancingError.BalanceDidNotConverge(iterations) =>
                        new RuntimeException(
                          s"Balancing did not converge after $iterations iterations"
                        )
                    case TxBalancingError.InsufficientFunds(valueDiff, minRequired) =>
                        val tokenInfo =
                            if valueDiff.assets.nonEmpty then s", tokens=${valueDiff.assets}"
                            else ""
                        new RuntimeException(
                          s"Balancing failure: insufficient funds: adaDiff=${valueDiff.coin.value}$tokenInfo, minRequired=$minRequired"
                        )
                    case TxBalancingError.InsufficientCollateralForReturn(
                          totalAda,
                          required,
                          minAda
                        ) =>
                        new RuntimeException(
                          s"Collateral contains tokens but insufficient ADA for return output. " +
                              s"Total: ${totalAda.value}, required: ${required.value}, minAda: ${minAda.value}"
                        )
                }
            case SomeBuildError.ValidationError(e, context) => e
            case SomeBuildError.InsufficientCollateralForReturn(
                  totalAda,
                  required,
                  minAda,
                  _
                ) =>
                new RuntimeException(
                  s"Collateral contains tokens but insufficient ADA for return output. " +
                      s"Total collateral ADA: ${totalAda.value}, required collateral: ${required.value}, " +
                      s"min ADA for return: ${minAda.value}. Need at least ${required.value + minAda.value} lovelace."
                )
        }

    override def toString: String = this match {
        case SomeStepError(e, _) =>
            s"Step processing error: ${e.getClass.getSimpleName} - ${e.explain}"
        case SomeRedeemerIndexingError(e, _) =>
            s"Internal redeemer indexing error. A redeemer was added but its corresponding transaction component is missing. " +
                s"Problematic redeemer: ${e.detachedRedeemer} (purpose: ${e.detachedRedeemer.purpose})"
        case BalancingError(TxBalancingError.EvaluationFailed(psee), _) =>
            s"Plutus script evaluation failed: ${psee.getMessage}, execution trace: ${psee.logs.mkString(" <CR> ")}"
        case BalancingError(TxBalancingError.Failed(other), _) =>
            s"Exception during balancing: ${other.getMessage}"
        case BalancingError(TxBalancingError.BalanceDidNotConverge(iterations), _) =>
            s"Balancing did not converge after $iterations iterations"
        case BalancingError(TxBalancingError.InsufficientFunds(valueDiff, required), _) =>
            val tokenInfo =
                if valueDiff.assets.nonEmpty then s", tokens=${valueDiff.assets}" else ""
            s"Insufficient funds: adaDiff=${valueDiff.coin.value}$tokenInfo, need $required more"
        case BalancingError(
              TxBalancingError.InsufficientCollateralForReturn(totalAda, required, minAda),
              _
            ) =>
            s"Collateral contains tokens but insufficient ADA for return output. " +
                s"Total: ${totalAda.value}, required: ${required.value}, minAda: ${minAda.value}"
        case ValidationError(e, _) =>
            s"Transaction validation failed: ${e.getClass.getSimpleName} - ${e.getMessage}"
        case InsufficientCollateralForReturn(totalAda, required, minAda, _) =>
            s"Collateral contains tokens but insufficient ADA for return output. " +
                s"Total: ${totalAda.value}, required: ${required.value}, minAda: ${minAda.value}"
    }

def keepRawL[A: Encoder](): Lens[KeepRaw[A], A] = {
    val get: KeepRaw[A] => A = kr => kr.value
    val replace: A => KeepRaw[A] => KeepRaw[A] = a => kr => KeepRaw(a)
    Lens[KeepRaw[A], A](get)(replace)
}

def txBodyL: Lens[Transaction, TransactionBody] = {
    val get: Transaction => TransactionBody = tx =>
        tx.focus(_.body).andThen(keepRawL[TransactionBody]()).get
    val replace: TransactionBody => Transaction => Transaction = body =>
        tx => tx.focus(_.body).andThen(keepRawL[TransactionBody]()).replace(body)
    Lens(get)(replace)
}

// ----

/** add at most 256 keys */
def addDummySignatures(numberOfKeys: Int, tx: Transaction): Transaction = {
    TransactionBuilder.modifyWs(
      tx,
      ws =>
          ws.copy(vkeyWitnesses =
              TaggedSortedSet.from(ws.vkeyWitnesses.toSet ++ generateUniqueKeys(numberOfKeys))
          )
    )
}

/** remove at most 256 keys, must be used in conjunction with addDummyVKeys */
def removeDummySignatures(numberOfKeys: Int, tx: Transaction): Transaction = {
    TransactionBuilder.modifyWs(
      tx,
      ws =>
          ws.copy(vkeyWitnesses =
              TaggedSortedSet.from(ws.vkeyWitnesses.toSet -- generateUniqueKeys(numberOfKeys))
          )
    )
}

private def generateVKeyWitness(counter: Int): VKeyWitness = {
    val value1 = ByteString.fromArray(Array.fill(32)(counter.toByte)) // 32 bytes
    val value2 = ByteString.fromArray(Array.fill(64)(counter.toByte)) // 64 bytes
    VKeyWitness(value1, value2)
}

private def generateUniqueKeys(n: Int): Set[VKeyWitness] = {
    (0 until n).map(i => generateVKeyWitness(i)).toSet
}

def txInputsL: Lens[Transaction, TaggedSortedSet[TransactionInput]] = {
    txBodyL.refocus(_.inputs)
}

def txOutputsL: Lens[Transaction, IndexedSeq[Sized[TransactionOutput]]] = {
    txBodyL.refocus(_.outputs)
}

def txReferenceInputsL: Lens[Transaction, TaggedSortedSet[TransactionInput]] = {
    txBodyL.refocus(_.referenceInputs)
}

def txRequiredSignersL: Lens[Transaction, TaggedSortedSet[AddrKeyHash]] = {
    txBodyL.refocus(_.requiredSigners)
}

def txRedeemersL: Lens[Transaction, Option[KeepRaw[Redeemers]]] = {
    Focus[Transaction](_.witnessSet.redeemers)
}

// ---

extension [S, A, B](lens: Lens[S, A])
    def >>>[C](other: Lens[A, C]): Lens[S, C] =
        lens.andThen(other)
