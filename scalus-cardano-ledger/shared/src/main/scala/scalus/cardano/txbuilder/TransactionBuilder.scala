package scalus.cardano.txbuilder

/** This module contains declarative transaction building types and utilities ported from
  * purescript-cardano-transaction-builder with significant modifications and additions.
  *   - The main entry-point: [[TransactionBuilder.build]]
  *   - The definition of steps: [[TransactionBuilderStep]]
  */

import io.bullet.borer.{Cbor, Encoder}
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{Context as SContext, State as SState, UtxoEnv}
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.txbuilder.Datum.DatumValue
import scalus.cardano.txbuilder.SomeBuildError.{BalancingError, SomeRedeemerIndexingError, SomeStepError, ValidationError}
import scalus.cardano.txbuilder.StepError.*
import scalus.cardano.txbuilder.TransactionBuilder.{Context, Operation, WitnessKind}
import scalus.cardano.txbuilder.modifyWs
import scalus.|>

// Type alias for compatibility - DiffHandler is now a function type in new Scalus API
type DiffHandler = (Value, Transaction) => Either[TxBalancingError, Transaction]

// ===================================
// Tx Builder steps
// ===================================

sealed trait TransactionBuilderStep

/** Steps to build the transaction:
  *   - generally non-commutative, so the order matters
  *   - some are additive (e.g. [[Mint]]), some are not, e.g., ([[Spend]])
  */
object TransactionBuilderStep {

    /** Spend any utxo. An attempt to consume (reference or spend) the same utxo twice will error.
      * If a reference script is used, the containing utxo should be added beforehand with
      * [[ReferenceOutput]] or [[Spend]] steps.
      */
    case class Spend(
        utxo: Utxo,
        witness: PubKeyWitness.type | NativeScriptWitness | ThreeArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    /** Spend a utxo guarded by plutus script.
      *
      * The [[redeemerBuilder]] is invoked after [[TransactionBuilder.build()]], but before it's
      * balanced by the low lever builder. As a result, the number and order of inputs, outputs,
      * certificates etc. is predetermined.
      *
      * Use this instead of [[Spend]] when assembling the redeemer requires the knowledge of the
      * transaction contents, e.g. to include the indices of inputs or outputs.
      */
    case class SpendWithDelayedRedeemer(
        utxo: Utxo,
        redeemerBuilder: Transaction => Data,
        validator: PlutusScript,
        datum: Option[Data] = None
    ) extends TransactionBuilderStep

    /** Send some funds/data to an address. Multiple identical steps are acceptable. */
    case class Send(output: TransactionOutput) extends TransactionBuilderStep

    /** Mint/burn tokens using a native/plutus script. Additive - sum monoid over amount. You should
      * determine your aggregate mint amount _outside_ of the builder. Chaining steps together to
      * calculate the mint amount.
      *
      * WARNING: If you explicitly pass amount == 0, this will return a Left.
      *
      * WARNING: If you do a reciprocal pair of mint/burn of the same amount (i.e., Mint 4, Mint
      * -4), you will nullify the mint amount, but the additionalSigners/requiredSigners/witnesses
      * will not be removed.
      */
    case class Mint(
        scriptHash: ScriptHash,
        assetName: AssetName,
        amount: Long,
        witness: NativeScriptWitness | TwoArgumentPlutusScriptWitness
    ) extends TransactionBuilderStep

    /** Add a [[scalus.cardano.ledger.Utxo]] as a CIP-31 reference input. Consuming the same UTxO
      * twice (reference or spend) is an error
      *
      * The reason that action is represented as a step is that reference utxos should be added to
      * the context and also may be required to create a [[WitnessForSpend]].
      *
      * @param utxo
      *   any utxo
      */
    case class ReferenceOutput(utxo: Utxo) extends TransactionBuilderStep

    /** Set the minimal fee. */
    case class Fee(fee: Coin) extends TransactionBuilderStep

    /** Set transactions validity start slot, can be used once.
      */
    case class ValidityStartSlot(slot: Long) extends TransactionBuilderStep

    /** Set transaction validity end slot (aka TTL), can be used once. */
    case class ValidityEndSlot(slot: Long) extends TransactionBuilderStep

    /** Add a utxo as a collateral input. Utxo should contain ada only and be controlled by a key,
      * not a script. If you need set collateral outputs at `totalCollateral` field, please use
      * optics.
      */
    case class AddCollateral(
        utxo: Utxo
    ) extends TransactionBuilderStep

    case class ModifyAuxiliaryData(f: Option[AuxiliaryData] => Option[AuxiliaryData])
        extends TransactionBuilderStep

    case class IssueCertificate(
        cert: Certificate,
        witness: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class WithdrawRewards(
        stakeCredential: StakeCredential,
        amount: Coin,
        witness: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class SubmitProposal(
        proposal: ProposalProcedure,
        witness: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class SubmitVotingProcedure(
        voter: Voter,
        votes: Map[GovActionId, VotingProcedure],
        witness: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep
}

case class DelayedRedeemerSpec(
    utxo: Utxo,
    redeemerBuilder: Transaction => Data,
    validator: PlutusScript,
    datum: Option[Data],
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
sealed trait Witness

/** Use this value to indicate there will be a signature. The corresponding verification key hash
  * will be tracked automatically in the context.
  */
case object PubKeyWitness extends Witness

/** Witnesses for native scripts. Can appear several times, but with the same [[additionalSigners]].
  */
case class NativeScriptWitness(
    scriptSource: ScriptSource[Script.Native],
    additionalSigners: Set[ExpectedSigner]
) extends Witness

// For operations that only take a redeemer and script context
case class TwoArgumentPlutusScriptWitness(
    scriptSource: ScriptSource[PlutusScript],
    redeemer: Data,
    additionalSigners: Set[ExpectedSigner]
) extends Witness

// For operations that take a datum, redeemer, and script context
case class ThreeArgumentPlutusScriptWitness(
    scriptSource: ScriptSource[PlutusScript],
    redeemer: Data,
    datum: Datum,
    additionalSigners: Set[ExpectedSigner]
) extends Witness

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

object TransactionBuilder:

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

    /** TODO: this is a good candidate to be removed, we use it only in
      * `assertCredentialMatchesWitness`.
      */
    trait HasWitnessKind[A]:
        def witnessKind: WitnessKind

    enum WitnessKind:
        case KeyBased
        case ScriptBased

    object HasWitnessKind:
        given HasWitnessKind[PubKeyWitness.type] with
            override def witnessKind: WitnessKind = WitnessKind.KeyBased

        given HasWitnessKind[NativeScriptWitness] with
            override def witnessKind: WitnessKind = WitnessKind.ScriptBased

        given HasWitnessKind[TwoArgumentPlutusScriptWitness] with
            override def witnessKind: WitnessKind = WitnessKind.ScriptBased

        given HasWitnessKind[ThreeArgumentPlutusScriptWitness] with
            override def witnessKind: WitnessKind = WitnessKind.ScriptBased

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
        delayedRedeemerSpecs: Seq[DelayedRedeemerSpec] = Seq.empty
    ) {

        /** Extract tupled information from a Context. This method is provided to avoid breaking
          * opacity while making it easier to check for equality in testing.
          */
        val toTuple: (
            Transaction,
            Seq[DetachedRedeemer],
            Network,
            Set[ExpectedSigner],
            ResolvedUtxos,
            Seq[DelayedRedeemerSpec]
        ) = (
          this.transaction,
          this.redeemers,
          this.network,
          this.expectedSigners,
          this.resolvedUtxos,
          this.delayedRedeemerSpecs
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
                balanced <- LowLevelTxBuilder.balanceFeeAndChangeWithTokens(
                  initial = this.transaction,
                  diffHandler = diffHandler,
                  protocolParams = protocolParams,
                  resolvedUtxo = this.getUtxos,
                  evaluator = evaluator
                )

                // _ = println(HexUtil.encodeHexString(txWithoutDummySignatures.toCbor))
            } yield Context(
              transaction = balanced,
              redeemers = this.redeemers,
              network = this.network,
              expectedSigners = this.expectedSigners,
              resolvedUtxos = this.resolvedUtxos
            )
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
            for {

                balancedCtx <- contextWithSignatures
                    .ensureMinAdaAll(protocolParams)
                    .balance(diffHandler, protocolParams, evaluator)
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
        val minAda = MinCoinSizedTransactionOutput.findMinAda(Sized(candidateOutput), params)
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
                        case dr @ DetachedRedeemer(_, RedeemerPurpose.ForSpend(input))
                            if input == spec.utxo.input =>
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

// ===================================
// Step processing errors
// ===================================

sealed trait StepError:
    def step: TransactionBuilderStep
    def explain: String

case class RedeemerIndexingInternalError(
    detachedRedeemer: DetachedRedeemer,
    steps: Seq[TransactionBuilderStep]
)

object StepError {
    case class Unimplemented(description: String, step: TransactionBuilderStep) extends StepError {
        override def explain: String = s"$description is not yet implemented. If you need it, " +
            s"submit a request at $bugTrackerUrl."
    }

    // TODO: Remove this error and just use a nonzero type
    case class CannotMintZero(
        scriptHash: ScriptHash,
        assetName: AssetName,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You cannot pass a \"amount = zero\" to a mint step, but we recieved it for" +
                s"(policyId, assetName) == ($scriptHash, $assetName)." +
                "\n You should not use the Mint step to calculate your mint amounts."
    }

    // TODO: more verbose error -- we'll need to pass more information from the assertion/step.
    case class InputAlreadyExists(input: TransactionInput, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"The transaction input $input already exists in the transaction as " +
                "either an input or reference input."
    }

    case class ResolvedUtxosIncoherence(
        input: TransactionInput,
        existingOutput: TransactionOutput,
        incoherentOutput: TransactionOutput,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "The context's resolvedUtxos already contain an input associated with a different output." +
                s"\nInput: $input" +
                s"\nExisting Output: $existingOutput" +
                s"\nIncoherent Output: $incoherentOutput"
    }

    case class CollateralNotPubKey(utxo: Utxo, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            s"The UTxO passed as a collateral input is not a PubKey UTxO. UTxO: $utxo"
    }

    // TODO: This error could probably be improved.
    case class CannotExtractSignatures(step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            s"Could not extract signatures via _.additionalSigners from $step"
    }

    case class DatumIsMissing(utxo: Utxo, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            s"Given witness to spend an output requires a datum that is missing: $utxo"
    }

    case class IncorrectDatumHash(
        utxo: Utxo,
        datum: Data,
        datumHash: DataHash,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain
            : String = "You provided a `DatumWitness` with a datum that does not match the datum hash present in a transaction output.\n " +
            s" Datum: $datum (CBOR: ${ByteString.fromArray(Cbor.encode(datum).toByteArray).toHex})\n  " +
            s" Datum hash: ${ByteString.fromArray(Cbor.encode(datumHash).toByteArray)}\n  " +
            s"UTxO: $utxo"
    }

    case class IncorrectScriptHash(
        script: Script,
        hash: ScriptHash,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String = script match {
            case nativeScript: Script.Native =>
                s"Provided script hash ($hash) does not match the provided native script ($nativeScript)"
            case plutusScript: PlutusScript =>
                s"Provided script hash ($hash) does not match the provided Plutus script ($plutusScript)"
        }
    }

    case class RedeemerComputationFailed(message: String, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"Failed to compute delayed redeemer: $message"
    }

    case class WrongOutputType(
        expectedType: WitnessKind,
        utxo: Utxo,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            s"The UTxO you provided has the wrong type. We require a `$expectedType`. " +
                s"UTxO: $utxo"
    }

    case class WrongCredentialType(
        action: Operation,
        expectedType: WitnessKind,
        cred: Credential,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            s"${action.explain} ($action) requires a $expectedType witness: $cred"
    }

    case class DatumWitnessNotProvided(utxo: Utxo, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            "The output you are trying to spend contains a datum hash, you need to provide " +
                s"a `DatumValue`, output: $utxo"
    }

    case class DatumValueForUtxoWithInlineDatum(
        utxo: Utxo,
        datum: Datum,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You can't provide datum value for a utxo with inlined datum: " +
                s"You tried to provide: $datum for the UTxO: $utxo"
    }

    case class UnneededDeregisterWitness(
        stakeCredential: StakeCredential,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, " +
                "but the stake credential you are trying to issue a deregistering certificate for " +
                "is a PubKeyHash credential. You should omit the provided credential witness for this " +
                s"credential: $stakeCredential. Provided witness: $witness"
    }

    case class UnneededSpoVoteWitness(
        cred: Credential,
        witness: TwoArgumentPlutusScriptWitness | NativeScriptWitness,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, but the corresponding Voter is " +
                "SPO (Stake Pool Operator). You should omit the provided credential witness " +
                s"for this credential: $cred. Provided witness: $witness"
    }

    case class UnneededProposalPolicyWitness(
        proposal: ProposalProcedure,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, but the corresponding proposal" +
                " does not need to validate against the proposal policy. You should omit the " +
                s"provided credential witness for this proposal: $proposal. Provided witness: $witness"
    }

    case class RedeemerIndexingError(redeemer: Redeemer, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"Redeemer indexing error. Problematic redeemer that does not have a valid index: $redeemer"
    }

    private val bugTrackerUrl: String = "https://github.com/cardano-hydrozoa/hydrozoa/issues"

    case class WrongNetworkId(address: Address, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            "The following `Address` that was specified in one of the UTxOs has a `NetworkId`" +
                s" different from the one `TransactionBody` has: $address"
    }

    case class CollateralWithTokens(
        utxo: Utxo,
        step: TransactionBuilderStep
    ) extends StepError {
        override def explain: String =
            "The UTxO you provided as a collateral must contain only ada. " +
                s"UTxO: $utxo"
    }

    case class AttachedScriptNotFound(scriptHash: ScriptHash, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"No witness or ref/spent output is found for script matching $scriptHash. " +
                "Note that the builder steps are not commutative: you must attach the script " +
                "before using an AttachedScript ScriptWitness."
    }

    case class ByronAddressesNotSupported(address: Address, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"Byron addresses are not supported: $address."
    }

    // We can't really return meaningful information, because scalus doesn't
    // provide it. See [[assertAttachedScriptExists]]
    case class ScriptResolutionError(step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            "An error was returned when trying to resolve scripts for the transaction."
    }

    case class FeeAlreadySet(currentFee: Long, step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            s"The fee ($currentFee) is already set. You cannot set the fee more than once."
    }

    case class ValidityStartSlotAlreadySet(slot: Long, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"The validity start slot ($slot) is already set. You cannot set the validity start slot more than once."
    }

    case class ValidityEndSlotAlreadySet(slot: Long, step: TransactionBuilderStep)
        extends StepError {
        override def explain: String =
            s"The validity end slot ($slot) is already set. You cannot set the validity end slot more than once."
    }
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
                    case TxBalancingError.CantBalance(lastDiff) =>
                        new RuntimeException(
                          s"Balancing failure. Last seen diff (sum(outputs) - sum(inputs)) = $lastDiff"
                        )
                    case TxBalancingError.InsufficientFunds(diff, minRequired) =>
                        new RuntimeException(
                          s"Balancing failure: insufficient funds: diff=$diff, minRequired=$minRequired"
                        )
                }
            case SomeBuildError.ValidationError(e, context) => e
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
        case BalancingError(TxBalancingError.CantBalance(lastDiff), _) =>
            s"Can't balance: last diff $lastDiff"
        case BalancingError(TxBalancingError.InsufficientFunds(diff, required), _) =>
            s"Insufficient funds: need $required more"
        case ValidationError(e, _) =>
            s"Transaction validation failed: ${e.getClass.getSimpleName} - ${e.getMessage}"
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
    tx.focus(_.witnessSet.vkeyWitnesses)
        .modify(s => TaggedSortedSet.from(s.toSet ++ generateUniqueKeys(numberOfKeys)))
}

/** remove at most 256 keys, must be used in conjunction with addDummyVKeys */
def removeDummySignatures(numberOfKeys: Int, tx: Transaction): Transaction = {
    modifyWs(
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
