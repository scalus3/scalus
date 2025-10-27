package scalus.cardano.txbuilder

/** This module contains declarative transaction building types and utilities ported from
  * purescript-cardano-transaction-builder with significant modifications and additions.
  *   - The main entry-point: [[TransactionBuilder.build]]
  *   - The definition of steps: [[TransactionBuilderStep]]
  */

import cats.*
import cats.data.*
import cats.implicits.*
import io.bullet.borer.{Cbor, Encoder}
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.GovAction.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{Context as SContext, STS, State as SState, UtxoEnv}
import scalus.cardano.ledger.utils.{AllResolvedScripts, MinCoinSizedTransactionOutput}
import scalus.cardano.txbuilder.Datum.DatumValue
import scalus.cardano.txbuilder.SomeBuildError.{BalancingError, SomeStepError, ValidationError}
import scalus.cardano.txbuilder.StepError.*
import scalus.cardano.txbuilder.TransactionBuilder.{Operation, WitnessKind}
import scalus.cardano.txbuilder.modifyWs
import TransactionWitnessSet.given

// Type alias for compatibility - DiffHandler is now a function type in new Scalus API
type DiffHandler = (Long, Transaction) => Either[TxBalancingError, Transaction]

import scalus.|>

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

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
        utxo: TransactionUnspentOutput,
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
        utxo: TransactionUnspentOutput,
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

    /** Add a [[TransactionUnspentOutput]] as a CIP-31 reference input. Consuming the same UTxO
      * twice (reference or spend) is an error
      *
      * The reason that action is represented as a step is that reference utxos should be added to
      * the context and also may be required to create a [[WitnessForSpend]].
      *
      * @param utxo
      *   any utxo
      */
    case class ReferenceOutput(utxo: TransactionUnspentOutput) extends TransactionBuilderStep

    /** Set the minimal fee. */
    case class Fee(fee: Coin) extends TransactionBuilderStep

    /** Set transactions validity start slot, can be used once.
      */
    case class ValidityStartSlot(slot: Long) extends TransactionBuilderStep

    /** Set transaction validity end slot (aka TTL), can be used once. */
    case class ValidityEndSlot(slot: Long) extends TransactionBuilderStep

    /** Add a utxo as a collateral input. Utxo should contain ada only and be controlled by a key,
      * not a script. If you need set collateral outputs ot `totalCollateral` field, please use
      * optics.
      */
    case class AddCollateral(
        utxo: TransactionUnspentOutput
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
    utxo: TransactionUnspentOutput,
    redeemerBuilder: Transaction => Data,
    validator: PlutusScript,
    datum: Option[Data]
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

/** An [[AddrKeyHash]] that is expected to sign some [[Transaction]].
  *
  * The purpose for signing is not presently tracked. For a sketch, see commit
  * https://github.com/cardano-hydrozoa/hydrozoa/commit/1a8c9c73fbfb33e79456a0a8b9f08688ef39b749
  */
case class ExpectedSigner(hash: AddrKeyHash)

// -----------------------------------------------------------------------------
// Transaction Builder
// -----------------------------------------------------------------------------

object TransactionBuilder:

    /** Builder is a state monad over Context. */
    private type BuilderM[A] = StateT[[X] =>> Either[StepError, X], Context, A]

    // Helpers to cut down on type signature noise
    private def pure0[A] = StateT.pure[[X] =>> Either[StepError, X], Context, A]
    private def liftF0[A] = StateT.liftF[[X] =>> Either[StepError, X], Context, A]
    private def modify0 = StateT.modify[[X] =>> Either[StepError, X], Context]
    private def get0 = StateT.get[[X] =>> Either[StepError, X], Context]

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
        def addUtxo(utxo: TransactionUnspentOutput): Option[ResolvedUtxos] =
            utxos.get(utxo.input) match {
                case None => Some(ResolvedUtxos(utxos + utxo.toTuple))
                case Some(existingOutput) =>
                    if existingOutput == utxo.output
                    then Some(ResolvedUtxos(utxos))
                    else None
            }

        /** Tries to add multiple UTxOs, returning invalid additions. See [[addUtxo]] */
        def addUtxos(
            utxos: Seq[TransactionUnspentOutput]
        ): Either[Seq[TransactionUnspentOutput], ResolvedUtxos] = {
            val res: (Seq[TransactionUnspentOutput], ResolvedUtxos) =
                utxos.foldLeft((Seq.empty[TransactionUnspentOutput], this))((acc, utxo) =>
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
    case class Context private[TransactionBuilder] (
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
                balanced <- LowLevelTxBuilder.balanceFeeAndChange(
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

        /** Conversion help to Scalus [[Utxos]] */
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
                    .map(BalancingError(_))

                validatedCtx <- balancedCtx
                    .validate(validators, protocolParams)
                    .left
                    .map(ValidationError(_))

                validatedCtxWithoutSignatures = validatedCtx.copy(
                  transaction =
                      removeDummySignatures(this.expectedSigners.size, validatedCtx.transaction)
                )

            } yield validatedCtxWithoutSignatures
        }
    }

    object Context:
        def empty(networkId: Network) = Context(
          transaction = Transaction.empty,
          redeemers = Seq.empty,
          network = networkId,
          expectedSigners = Set.empty,
          resolvedUtxos = ResolvedUtxos.empty,
          delayedRedeemerSpecs = Seq.empty
        )

        // Tries to add the output to the resolved utxo set, throwing an error if
        // the input is already mapped to another output
        def addResolvedUtxo(utxo: TransactionUnspentOutput): BuilderM[Unit] =
            for {
                ctx <- get0
                mbNewUtxos = ctx.resolvedUtxos.addUtxo(utxo)
                _ <- mbNewUtxos match {
                    case None =>
                        liftF0(
                          Left(
                            ResolvedUtxosIncoherence(
                              input = utxo.input,
                              existingOutput = ctx.resolvedUtxos.utxos(utxo.input),
                              incoherentOutput = utxo.output
                            )
                          )
                        )
                    case Some(utxos) => modify0(Focus[Context](_.resolvedUtxos).replace(utxos))
                }
            } yield ()

    private val unsafeCtxBodyL: Lens[Context, TransactionBody] = {
        Focus[Context](_.transaction) >>> txBodyL
    }

    private val unsafeCtxWitnessL: Lens[Context, TransactionWitnessSet] =
        Focus[Context](_.transaction).refocus(_.witnessSet)

    /** Update the given transaction output to have the minimum required ada, only changing its
      * Coin.
      */

    def ensureMinAda(
        candidateOutput: TransactionOutput,
        params: ProtocolParams
    ): TransactionOutput = {
        val minAda = MinCoinSizedTransactionOutput(Sized(candidateOutput), params)
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
        val modifyBuilderM: BuilderM[Unit] = {
            for {
                _ <- processSteps(steps)
                ctx0 <- get0
                res <- liftF0(
                  TransactionConversion.fromEditableTransactionSafe(
                    EditableTransaction(
                      transaction = ctx0.transaction,
                      redeemers = ctx0.redeemers.toVector
                    )
                  ) match {
                      case None    => Left(RedeemerIndexingInternalError(ctx.transaction, steps))
                      case Some(x) => Right(x)
                  }
                )
                // Replace the transaction in the context, keeping the rest
                _ <- modify0(Focus[Context](_.transaction).replace(res))

                // Replace delayed redeemers if any exist
                _ <-
                    if ctx0.delayedRedeemerSpecs.nonEmpty then {
                        for {
                            ctx1 <- get0
                            updatedRedeemers <- liftF0(
                              replaceDelayedRedeemers(
                                ctx1.redeemers,
                                ctx1.delayedRedeemerSpecs,
                                ctx1.transaction
                              )
                            )
                            _ <- modify0(_.replaceRedeemers(updatedRedeemers))
                        } yield ()
                    } else {
                        pure0(())
                    }
            } yield ()
        }
        modifyBuilderM.run(ctx).map(_._1).left.map(SomeStepError(_))
    }

    private def replaceDelayedRedeemers(
        redeemers: Seq[DetachedRedeemer],
        specs: Seq[DelayedRedeemerSpec],
        sortedTx: Transaction
    ): Either[StepError, Seq[DetachedRedeemer]] = {
        try {
            val updatedRedeemers = specs.foldLeft(redeemers) { (redeemers, spec) =>
                val realRedeemerData = spec.redeemerBuilder(sortedTx)
                redeemers.map {
                    case dr @ DetachedRedeemer(_, RedeemerPurpose.ForSpend(input))
                        if input == spec.utxo.input =>
                        dr.copy(datum = realRedeemerData)
                    case other => other
                }
            }
            Right(updatedRedeemers)
        } catch {
            case e: Exception =>
                Left(RedeemerComputationFailed(e.getMessage))
        }
    }

    trait HasBuilderEffect[A]:
        def runEffect(): BuilderM[Unit]

    private def processSteps(steps: Seq[TransactionBuilderStep]): BuilderM[Unit] =
        steps.traverse_(processStep)

    private def processStep(step: TransactionBuilderStep): BuilderM[Unit] = step match {

        case spend: TransactionBuilderStep.Spend =>
            useSpend(spend)

        case delayedSpend: TransactionBuilderStep.SpendWithDelayedRedeemer =>
            useSpendWithDelayedRedeemer(delayedSpend)

        case send: TransactionBuilderStep.Send =>
            useSend(send)

        case mint: TransactionBuilderStep.Mint =>
            useMint(mint)

        case referenceOutput: TransactionBuilderStep.ReferenceOutput =>
            useReferenceOutput(referenceOutput)

        case fee: TransactionBuilderStep.Fee =>
            useFee(fee.fee)

        case validityStartSlot: TransactionBuilderStep.ValidityStartSlot =>
            useValidityStartSlot(validityStartSlot.slot)

        case validityEndSlot: TransactionBuilderStep.ValidityEndSlot =>
            useValidityEndSlot(validityEndSlot.slot)

        case addCollateral: TransactionBuilderStep.AddCollateral =>
            useAddCollateral(addCollateral)

        case modifyAuxiliaryData: TransactionBuilderStep.ModifyAuxiliaryData =>
            useModifyAuxiliaryData(modifyAuxiliaryData)

        case issueCertificate: TransactionBuilderStep.IssueCertificate =>
            useIssueCertificate(issueCertificate)

        case withdrawRewards: TransactionBuilderStep.WithdrawRewards =>
            useWithdrawRewards(withdrawRewards)

        case submitProposal: TransactionBuilderStep.SubmitProposal =>
            useSubmitProposal(submitProposal)

        case submitVotingProcedure: TransactionBuilderStep.SubmitVotingProcedure =>
            useSubmitVotingProcedure(submitVotingProcedure)
    }

    // -------------------------------------------------------------------------
    // Spend step
    // -------------------------------------------------------------------------

    /** Tries to modify the transaction to make it consume a given output and add the requisite
      * signature(s) to the Context's _.expectedSigners. Uses witness to try to satisfy spending
      * requirements.
      */
    private def useSpend(
        spend: TransactionBuilderStep.Spend
    ): BuilderM[Unit] = {

        val utxo = spend.utxo
        val witness = spend.witness

        // Extract the key hash, erroring if not a Shelley PKH address
        def getPaymentVerificationKeyHash(address: Address): BuilderM[AddrKeyHash] =
            liftF0(address match {
                case sa: ShelleyAddress =>
                    sa.payment match {
                        case kh: ShelleyPaymentPart.Key => Right(kh.hash)
                        case _: ShelleyPaymentPart.Script =>
                            Left(
                              WrongOutputType(WitnessKind.KeyBased, utxo)
                            )
                    }
                case _ => Left(WrongOutputType(WitnessKind.KeyBased, utxo))
            })

        def getPaymentScriptHash(address: Address): BuilderM[ScriptHash] =
            liftF0(address match {
                case sa: ShelleyAddress =>
                    sa.payment match {
                        case s: ShelleyPaymentPart.Script => Right(s.hash)
                        case _: ShelleyPaymentPart.Key =>
                            Left(
                              WrongOutputType(WitnessKind.ScriptBased, utxo)
                            )

                    }
                case _ =>
                    Left(WrongOutputType(WitnessKind.ScriptBased, utxo))
            })

        for {
            _ <- assertNetworkId(utxo.output.address)
            _ <- assertInputDoesNotAlreadyExist(utxo.input)

            // Add input
            _ <- modify0(
              unsafeCtxBodyL
                  .refocus(_.inputs)
                  .modify(inputs => TaggedSortedSet.from(appendDistinct(utxo.input, inputs.toSeq)))
            )
            // Add utxo to resolvedUtxos
            _ <- Context.addResolvedUtxo(utxo)
            // Handle the witness
            _ <- witness match {
                // Case 1: Key-locked input
                case _: PubKeyWitness.type =>
                    for {
                        // Extract the key hash, erroring if not a Shelley PKH address
                        keyHash <- getPaymentVerificationKeyHash(utxo.output.address)
                        _ <- usePubKeyWitness(ExpectedSigner(keyHash))
                    } yield ()
                // Case 2: Native script-locked input
                // Ensure the hash matches the witness, handle the output components,
                // defer to witness handling
                case native: NativeScriptWitness =>
                    for {
                        scriptHash <- getPaymentScriptHash(utxo.output.address)
                        _ <- assertScriptHashMatchesSource(scriptHash, native.scriptSource)
                        _ <- useNativeScript(native.scriptSource, native.additionalSigners)
                    } yield ()

                // Case 3: Plutus script-locked input
                // Ensure the hash matches the witness, handle the output components,
                // defer to witness handling
                case plutus: ThreeArgumentPlutusScriptWitness =>
                    for {
                        scriptHash <- getPaymentScriptHash(utxo.output.address)
                        _ <- assertScriptHashMatchesSource(scriptHash, plutus.scriptSource)
                        _ <- usePlutusScript(plutus.scriptSource, plutus.additionalSigners)

                        detachedRedeemer = DetachedRedeemer(
                          plutus.redeemer,
                          RedeemerPurpose.ForSpend(utxo.input)
                        )
                        _ <- modify0(ctx =>
                            ctx.focus(_.redeemers)
                                .modify(r => appendDistinct(detachedRedeemer, r))
                        )
                        _ <- useDatum(utxo, plutus.datum)
                    } yield ()
            }
        } yield ()
    }

    private def useDatum(
        // TODO: this is used for errors only, I think utxoId should be sufficient
        utxo: TransactionUnspentOutput,
        datum: Datum
    ): BuilderM[Unit] =
        for {
            _ <- utxo.output.datumOption match {
                case None =>
                    liftF0(
                      Left(DatumIsMissing(utxo))
                    )
                case Some(DatumOption.Inline(_)) =>
                    datum match {
                        case Datum.DatumInlined => pure0(())
                        case Datum.DatumValue(_) =>
                            liftF0(
                              Left(DatumValueForUtxoWithInlineDatum(utxo, datum))
                            )
                    }
                case Some(DatumOption.Hash(datumHash)) =>
                    datum match {
                        case Datum.DatumInlined =>
                            liftF0(Left(DatumWitnessNotProvided(utxo)))
                        case Datum.DatumValue(providedDatum) =>
                            // TODO: is that correct? Upstream Data.dataHash extension?
                            val computedHash: DataHash =
                                DataHash.fromByteString(blake2b_224(serialiseData(providedDatum)))

                            if datumHash == computedHash then {
                                modify0(
                                  unsafeCtxWitnessL
                                      .refocus(_.plutusData)
                                      .modify(plutusData =>
                                          KeepRaw.apply(
                                            TaggedSet.from(
                                              appendDistinct(
                                                KeepRaw.apply(providedDatum),
                                                plutusData.value.toIndexedSeq
                                              )
                                            )
                                          )
                                      )
                                )
                            } else {
                                liftF0(
                                  Left(
                                    IncorrectDatumHash(utxo, providedDatum, datumHash)
                                  )
                                )
                            }
                    }
            }
        } yield ()

    // -------------------------------------------------------------------------
    // Send step
    // -------------------------------------------------------------------------

    private def useSend(send: TransactionBuilderStep.Send): BuilderM[Unit] =
        for {
            _ <- assertNetworkId(send.output.address)
            _ <- modify0(
              unsafeCtxBodyL
                  .refocus(_.outputs)
                  // Intentionally not using pushUnique: we can create multiple outputs of the same shape
                  .modify(outputs => outputs :+ Sized(send.output))
            )
        } yield ()

    // -------------------------------------------------------------------------
    // MintAsset step
    // -------------------------------------------------------------------------

    private def useMint(
        mint: TransactionBuilderStep.Mint
    ): BuilderM[Unit] = {
        val scriptHash = mint.scriptHash
        val assetName = mint.assetName
        val amount = mint.amount
        val witness = mint.witness

        for {
            // Not allowed to mint 0
            _ <-
                if amount == 0
                then liftF0(Left(CannotMintZero(scriptHash, assetName)))
                else pure0(())

            // Since we allow monoidal mints, only the final redeemer is kept. We have to remove the old redeemer
            // before adding the new one, as well as if the monoidal sum of the amounts of this mint
            // and the existing mint cause the policyId entry to be removed from the mint map.
            removeRedeemer: BuilderM[Unit] =
                modify0(
                  Focus[Context](_.redeemers).modify(
                    _.filter(detachedRedeemer =>
                        detachedRedeemer.purpose match {
                            case RedeemerPurpose.ForMint(hash) => hash != scriptHash
                            case _                             => true
                        }
                    )
                  )
                )

            _ <- removeRedeemer

            // Common witness handling
            _ <- useNonSpendingWitness(
              Operation.Minting(scriptHash),
              Credential.ScriptHash(scriptHash),
              witness
            )

            // This is the tricky part. We handle `Mint` steps monoidally, so we can end up with
            // reciprocal mints/burns (i.e., +5, -5) that can do one of 3 main things:
            // 1.) If a mint for the given policyId does not already exist, it creates a new entry.
            // 2.) If a mint for the given policyId already exists, but no entry for the given asset name exists,
            //     it creates it.
            // 3.) If an entry exists for both the policyId and the assetname, it adds the amount in the step to
            //     the existing amount in the map.
            //
            //
            // In addition
            //  - Case (1) can:
            //    - a.) turn a `None : Option[Mint]` into a `Some`.
            //    - b.) add the policyId to an existing non-empty map
            //  - Case (3) can either:
            //    - a.) remove a policyId in the map entirely (if the sum of the amounts are 0 are there are no other
            //          assets in the map)
            //    - b.) Turn a `Some(mint) : Option[Mint]` into a `None`, if (a) is true and there were, in addition,
            //          no other policies left in the map.
            //
            // When case (3a) is true, the redeemer corresponding to the policyId must also be removed from the
            // detached redeemers set in the context.
            ctx <- get0
            currentMint = ctx |> unsafeCtxBodyL.refocus(_.mint).get
            thisMint = MultiAsset.asset(scriptHash, assetName, amount)
            replaceMint = (newMint: Option[Mint]) =>
                modify0(unsafeCtxBodyL.refocus(_.mint).replace(newMint))

            _ <- currentMint match {
                // In this case the mint map was originally completely empty.
                case None =>
                    // Above, we check that the amount != 0. Thus:
                    // Case (1, a) -- create an entirely new, non-empty map
                    replaceMint(Some(Mint(thisMint)))

                // If this is "Some", we know we have a non-empty mint map -- at least one policyId with at least 1
                // asset.
                case Some(existing: Mint) =>
                    existing.assets.get(scriptHash) match {
                        // No current entry for the script hash; this means that "currentAmount" would be 0
                        // (by invariants of the Mint type) and thus newAmount != 0. So we can add it to the existing map.
                        case None =>
                            // Case (1, b)
                            replaceMint(
                              Some(
                                Mint(
                                  MultiAsset(
                                    existing.assets
                                        .updated(scriptHash, SortedMap(assetName -> amount))
                                  )
                                )
                              )
                            )

                        // There is a current entry for the script hash; thus, we need to look at the inner
                        // map to decide what to do.
                        case Some(innerMap) =>
                            innerMap.get(assetName) match {
                                // No current entry for the asset name, but there must be at least one other
                                // asset name associated with the script hash (by the invariants of the Mint type).
                                // Thus, currentAmount == 0, amount != 0 => newAmount != 0
                                case None => {
                                    // Case 2: add a new asset name to an existing policy map
                                    val newInnerMap = innerMap.updated(assetName, amount)
                                    val newOuterMap =
                                        existing.assets.updated(scriptHash, newInnerMap)
                                    replaceMint(Some(Mint(MultiAsset(newOuterMap))))
                                }
                                case Some(currentAmount) =>
                                    val newAmount: Long = currentAmount + amount
                                    val newInnerMap: SortedMap[AssetName, Long] =
                                        if newAmount == 0
                                        then innerMap.removed(assetName)
                                        else innerMap.updated(assetName, newAmount)

                                    if newInnerMap.isEmpty
                                    // The new inner map is empty -- this means that we must remove the policyId
                                    // from the outer map, and the corresponding redeemer from the Context
                                    then {
                                        val newOuterMap = existing.assets.removed(scriptHash)
                                        val removeRedeemer = modify0(
                                          Focus[Context](_.redeemers).modify(
                                            _.filter(detachedRedeemer =>
                                                detachedRedeemer.purpose match {
                                                    case RedeemerPurpose.ForMint(hash) =>
                                                        hash != scriptHash
                                                    case _ => true
                                                }
                                            )
                                          )
                                        )

                                        if newOuterMap.isEmpty
                                        then {
                                            // The new outerMap is empty. Thus, we have to set the TxBody Mint field to None
                                            // and remove the redeemer from the context
                                            for {
                                                _ <- replaceMint(None)
                                                _ <- removeRedeemer

                                            } yield ()
                                        } else
                                            for {
                                                // The new outer map is NOT empty. Thus we must only replace the current
                                                // outer map with OUR outer map, and remove our redeemer
                                                _ <- replaceMint(
                                                  Some(Mint(MultiAsset(newOuterMap)))
                                                )
                                                _ <- removeRedeemer
                                            } yield ()
                                    }
                                    // In this case, the new inner map is NOT empty. Thus, we only must replace
                                    // the outer map with the updated inner map.
                                    else {
                                        val newOuterMap =
                                            existing.assets.updated(scriptHash, newInnerMap)
                                        replaceMint(Some(Mint(MultiAsset(newOuterMap))))
                                    }
                            }
                    }
            }
        } yield ()
    }

    private def useSpendWithDelayedRedeemer(
        delayedSpend: TransactionBuilderStep.SpendWithDelayedRedeemer
    ): BuilderM[Unit] = {
        val utxo = delayedSpend.utxo
        val validator = delayedSpend.validator
        val datum = delayedSpend.datum

        val dummyRedeemerData = Data.I(0)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(validator),
          redeemer = dummyRedeemerData,
          datum = datum.map(Datum.DatumValue.apply).getOrElse(Datum.DatumInlined),
          additionalSigners = Set.empty
        )

        val spec = DelayedRedeemerSpec(
          utxo = utxo,
          redeemerBuilder = delayedSpend.redeemerBuilder,
          validator = validator,
          datum = datum
        )

        for {
            _ <- useSpend(TransactionBuilderStep.Spend(utxo, witness))
            _ <- modify0(_.addDelayedRedeemer(spec))
        } yield ()
    }

    // -------------------------------------------------------------------------
    // ReferenceOutput step
    // -------------------------------------------------------------------------

    private def useReferenceOutput(
        referenceOutput: TransactionBuilderStep.ReferenceOutput
    ): BuilderM[Unit] =
        for {
            _ <- assertNetworkId(referenceOutput.utxo.output.address)
            _ <- assertInputDoesNotAlreadyExist(referenceOutput.utxo.input)

            _ <- modify0(
              // Add the referenced utxo id to the tx body
              unsafeCtxBodyL
                  .refocus(_.referenceInputs)
                  .modify(inputs =>
                      TaggedSortedSet.from(
                        appendDistinct(referenceOutput.utxo.input, inputs.toSeq)
                      )
                  )
            )

            _ <- Context.addResolvedUtxo(referenceOutput.utxo)
        } yield ()

    // -------------------------------------------------------------------------
    // Fee step
    // -------------------------------------------------------------------------

    private def useFee(fee: Coin): BuilderM[Unit] = for {
        ctx <- get0
        currentFee = ctx.transaction.body.value.fee.value
        _ <- currentFee match {
            case 0 =>
                modify0(
                  unsafeCtxBodyL
                      .refocus(_.fee)
                      .replace(fee)
                )
            case nonZero => liftF0(Left(FeeAlreadySet(nonZero)))
        }
    } yield ()

    // -------------------------------------------------------------------------
    // ValidityStartSlot step
    // -------------------------------------------------------------------------

    private def useValidityStartSlot(slot: Long): BuilderM[Unit] = for {
        ctx <- get0
        currentValidityStartSlot = ctx.transaction.body.value.validityStartSlot
        _ <- currentValidityStartSlot match {
            case Some(existingSlot) =>
                liftF0(Left(ValidityStartSlotAlreadySet(existingSlot)))
            case None =>
                modify0(
                  unsafeCtxBodyL
                      .refocus(_.validityStartSlot)
                      .replace(Some(slot))
                )
        }
    } yield ()

    // -------------------------------------------------------------------------
    // ValidityEndSlot step
    // -------------------------------------------------------------------------

    private def useValidityEndSlot(slot: Long): BuilderM[Unit] = for {
        ctx <- get0
        currentValidityEndSlot = ctx.transaction.body.value.ttl
        _ <- currentValidityEndSlot match {
            case Some(existingSlot) =>
                liftF0(Left(ValidityEndSlotAlreadySet(existingSlot)))
            case None =>
                modify0(
                  unsafeCtxBodyL
                      .refocus(_.ttl)
                      .replace(Some(slot))
                )
        }
    } yield ()

    // -------------------------------------------------------------------------
    // AddCollateral step
    // -------------------------------------------------------------------------

    private def useAddCollateral(
        addCollateral: TransactionBuilderStep.AddCollateral
    ): BuilderM[Unit] =
        for {
            _ <- assertNetworkId(addCollateral.utxo.output.address)
            _ <- assertAdaOnlyPubkeyUtxo(addCollateral.utxo)
            _ <- Context.addResolvedUtxo(addCollateral.utxo)
            _ <- modify0(
              // Add the collateral utxo to the tx body
              unsafeCtxBodyL
                  .refocus(_.collateralInputs)
                  .modify(inputs =>
                      TaggedSortedSet.from(appendDistinct(addCollateral.utxo.input, inputs.toSeq))
                  )
            )
        } yield ()

    /** Ensure that the output is a pubkey output containing only ada. */
    private def assertAdaOnlyPubkeyUtxo(utxo: TransactionUnspentOutput): BuilderM[Unit] =
        for {
            _ <-
                if !utxo.output.value.assets.isEmpty
                then
                    liftF0(
                      Left(CollateralWithTokens(utxo))
                    )
                else pure0(())
            addr: ShelleyAddress <- utxo.output.address match {
                case sa: ShelleyAddress  => pure0(sa)
                case by: ByronAddress    => liftF0(Left(ByronAddressesNotSupported(by)))
                case stake: StakeAddress => liftF0(Left(CollateralNotPubKey(utxo)))
            }
            _ <- addr.payment match {
                case ShelleyPaymentPart.Key(_: AddrKeyHash) => pure0(())
                case _ => liftF0(Left(CollateralNotPubKey(utxo)))
            }
        } yield ()

    // -------------------------------------------------------------------------
    // ModifyAuxiliaryData step
    // -------------------------------------------------------------------------

    private def useModifyAuxiliaryData(
        modifyAuxiliaryData: TransactionBuilderStep.ModifyAuxiliaryData
    ): BuilderM[Unit] = {
        for {
            ctx <- get0
            oldData = ctx.transaction.auxiliaryData

            newData = modifyAuxiliaryData.f(oldData.map(_.value)).map(KeepRaw(_))
            _ <- modify0(
              Focus[Context](_.transaction)
                  .refocus(_.auxiliaryData)
                  // Fixed for Scalus 0.12.1+ - auxiliaryData is now wrapped in KeepRaw
                  .replace(newData)
            )

            newHash = newData
                .map(someData => platform.blake2b_256(ByteString.unsafeFromArray(someData.raw)))
                .map(AuxiliaryDataHash.fromByteString)
            _ <- modify0(unsafeCtxBodyL.refocus(_.auxiliaryDataHash).replace(newHash))
        } yield ()
    }

    // -------------------------------------------------------------------------
    // IssueCertificate step
    // -------------------------------------------------------------------------

    private def useIssueCertificate(
        issueCertificate: TransactionBuilderStep.IssueCertificate
    ): BuilderM[Unit] =
        for {
            _ <- modify0(
              unsafeCtxBodyL
                  .refocus(_.certificates)
                  .modify(certificates =>
                      TaggedOrderedSet.from(
                        appendDistinct(issueCertificate.cert, certificates.toIndexedSeq)
                      )
                  )
            )
            _ <- useCertificateWitness(issueCertificate.cert, issueCertificate.witness)
        } yield ()

    def useCertificateWitness(
        cert: Certificate,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ): BuilderM[Unit] = cert match {
        // FIXME: verify
        case Certificate.UnregCert(credential, _) =>
            for {
                _ <- (credential, witness) match {
                    // Credential.KeyHash
                    case (Credential.KeyHash(_), PubKeyWitness) => pure0(())
                    case (Credential.KeyHash(_), witness: TwoArgumentPlutusScriptWitness) =>
                        liftF0(
                          Left(
                            UnneededDeregisterWitness(
                              StakeCredential(credential),
                              witness
                            )
                          )
                        )
                    case (Credential.KeyHash(_), witness: NativeScriptWitness) =>
                        liftF0(
                          Left(
                            UnneededDeregisterWitness(
                              StakeCredential(credential),
                              witness
                            )
                          )
                        )
                    // Credential.ScriptHash
                    case (Credential.ScriptHash(_), PubKeyWitness) =>
                        liftF0(
                          Left(
                            WrongCredentialType(
                              Operation.CertificateOperation(cert),
                              WitnessKind.KeyBased,
                              credential
                            )
                          )
                        )
                    case (
                          Credential.ScriptHash(scriptHash),
                          witness: TwoArgumentPlutusScriptWitness
                        ) =>
                        assertScriptHashMatchesSource(scriptHash, witness.scriptSource)
                    case (Credential.ScriptHash(scriptHash), witness: NativeScriptWitness) =>
                        assertScriptHashMatchesSource(scriptHash, witness.scriptSource)
                }
                _ <- useNonSpendingWitness(
                  Operation.CertificateOperation(cert),
                  credential,
                  witness
                )
            } yield ()
        case Certificate.StakeDelegation(credential, _) =>
            useNonSpendingWitness(Operation.CertificateOperation(cert), credential, witness)
        // FIXME: verify
        case Certificate.RegCert(_, _) =>
            pure0(())
        case Certificate.PoolRegistration(_, _, _, _, _, _, _, _, _) =>
            pure0(())
        case Certificate.PoolRetirement(_, _) =>
            pure0(())
        case Certificate.VoteDelegCert(credential, _) =>
            useNonSpendingWitness(Operation.CertificateOperation(cert), credential, witness)
        case Certificate.StakeVoteDelegCert(credential, _, _) =>
            useNonSpendingWitness(Operation.CertificateOperation(cert), credential, witness)
        case Certificate.StakeRegDelegCert(credential, _, _) =>
            useNonSpendingWitness(Operation.CertificateOperation(cert), credential, witness)
        case Certificate.VoteRegDelegCert(credential, _, _) =>
            useNonSpendingWitness(Operation.CertificateOperation(cert), credential, witness)
        case Certificate.StakeVoteRegDelegCert(credential, _, _, _) =>
            useNonSpendingWitness(Operation.CertificateOperation(cert), credential, witness)
        case Certificate.AuthCommitteeHotCert(_, _)    => pure0(()) // not supported
        case Certificate.ResignCommitteeColdCert(_, _) => pure0(()) // not supported
        case Certificate.RegDRepCert(credential, _, _) =>
            useNonSpendingWitness(Operation.CertificateOperation(cert), credential, witness)
        case Certificate.UnregDRepCert(credential, _) =>
            useNonSpendingWitness(Operation.CertificateOperation(cert), credential, witness)
        case Certificate.UpdateDRepCert(credential, _) =>
            useNonSpendingWitness(Operation.CertificateOperation(cert), credential, witness)
    }

    // -------------------------------------------------------------------------
    // WithdrawRewards step
    // -------------------------------------------------------------------------

    def useWithdrawRewards(
        withdrawRewards: TransactionBuilderStep.WithdrawRewards
    ): BuilderM[Unit] =
        for {
            ctx <- get0

            rewardAccount = withdrawRewards.stakeCredential.credential match {
                case Credential.KeyHash(keyHash) =>
                    // Convert AddrKeyHash to StakeKeyHash - they're likely the same underlying type?
                    val stakeKeyHash = keyHash.asInstanceOf[StakeKeyHash]
                    val stakeAddress = StakeAddress(ctx.network, StakePayload.Stake(stakeKeyHash))
                    RewardAccount(stakeAddress)
                case Credential.ScriptHash(scriptHash) =>
                    val stakeAddress = StakeAddress(ctx.network, StakePayload.Script(scriptHash))
                    RewardAccount(stakeAddress)
            }

            _ <- modify0(
              unsafeCtxBodyL
                  .refocus(_.withdrawals)
                  .modify(withdrawals => {
                      val currentWithdrawals = withdrawals.map(_.withdrawals).getOrElse(Map.empty)
                      Some(
                        Withdrawals(
                          SortedMap
                              .from(currentWithdrawals + (rewardAccount -> withdrawRewards.amount))
                        )
                      )
                  })
            )

            _ <- useNonSpendingWitness(
              Operation.Withdraw(rewardAccount.address),
              withdrawRewards.stakeCredential.credential,
              withdrawRewards.witness
            )
        } yield ()

    // -------------------------------------------------------------------------
    // SubmitProposal step
    // -------------------------------------------------------------------------

    private def useSubmitProposal(
        submitProposal: TransactionBuilderStep.SubmitProposal
    ): BuilderM[Unit] =
        for {
            _ <- modify0(
              unsafeCtxBodyL
                  .refocus(_.proposalProcedures)
                  .modify(proposals =>
                      TaggedOrderedSet.from(
                        appendDistinct(submitProposal.proposal, proposals.toIndexedSeq)
                      )
                  )
            )
            _ <- {
                def getPolicyHash(govAction: GovAction): Option[ScriptHash] = govAction match {
                    case GovAction.ParameterChange(_, _, policyHash)  => policyHash
                    case GovAction.TreasuryWithdrawals(_, policyHash) => policyHash
                    case _                                            => None
                }

                getPolicyHash(submitProposal.proposal.govAction) match {
                    case None =>
                        pure0(())
                    case Some(policyHash) =>
                        useNonSpendingWitness(
                          Operation.Proposing(submitProposal.proposal),
                          Credential.ScriptHash(policyHash),
                          submitProposal.witness
                        )
                }
            }
        } yield ()

    // -------------------------------------------------------------------------
    // SubmitVotingProcedure step
    // -------------------------------------------------------------------------

    private def useSubmitVotingProcedure(
        submitVotingProcedure: TransactionBuilderStep.SubmitVotingProcedure
    ): BuilderM[Unit] =
        for {
            _ <- modify0(
              unsafeCtxBodyL
                  .refocus(_.votingProcedures)
                  .modify(procedures => {
                      val currentProcedures = procedures
                          .map(_.procedures)
                          .getOrElse(
                            SortedMap.empty[Voter, SortedMap[GovActionId, VotingProcedure]]
                          )
                      Some(
                        VotingProcedures(
                          currentProcedures + (submitVotingProcedure.voter -> SortedMap
                              .from(submitVotingProcedure.votes))
                        )
                      )
                  })
            )
            _ <- for {
                cred <- submitVotingProcedure.voter match {
                    case Voter.StakingPoolKey(poolKeyHash) =>
                        val credential = Credential.KeyHash(poolKeyHash)
                        submitVotingProcedure.witness match {
                            case _: PubKeyWitness.type => pure0(credential)
                            case witness: TwoArgumentPlutusScriptWitness =>
                                liftF0(
                                  Left(UnneededSpoVoteWitness(credential, witness))
                                )
                            case witness: NativeScriptWitness =>
                                liftF0(
                                  Left(UnneededSpoVoteWitness(credential, witness))
                                )
                        }
                    case Voter.ConstitutionalCommitteeHotKey(credential) =>
                        pure0(
                          Credential.KeyHash(credential)
                        )
                    case Voter.ConstitutionalCommitteeHotScript(scriptHash) =>
                        pure0(
                          Credential.ScriptHash(scriptHash)
                        )
                    case Voter.DRepKey(credential) =>
                        pure0(
                          Credential.KeyHash(credential)
                        )
                    case Voter.DRepScript(scriptHash) =>
                        pure0(
                          Credential.ScriptHash(scriptHash)
                        )
                }
                _ <- useNonSpendingWitness(
                  Operation.Voting(submitVotingProcedure.voter),
                  cred,
                  submitVotingProcedure.witness
                )
            } yield ()
        } yield ()

    // -------------------------------------------------------------------------
    // Common functions - using non-spending witness
    // -------------------------------------------------------------------------

    def useNonSpendingWitness(
        credAction: Operation,
        cred: Credential,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ): BuilderM[Unit] =
        for {
            _ <- witness match {
                // Pubkey credential witness: add to expected signers
                case PubKeyWitness =>
                    for {
                        _ <- assertCredentialMatchesWitness(
                          credAction,
                          PubKeyWitness,
                          cred
                        )
                        // Add key hash to expected signers
                        _ <- cred match {
                            case Credential.KeyHash(keyHash) =>
                                usePubKeyWitness(ExpectedSigner(keyHash))
                            case _ =>
                                liftF0(
                                  Left(
                                    WrongCredentialType(
                                      credAction,
                                      WitnessKind.KeyBased,
                                      cred
                                    )
                                  )
                                )
                        }
                    } yield ()
                case witness: NativeScriptWitness =>
                    for {
                        _ <- assertCredentialMatchesWitness(
                          credAction,
                          witness,
                          cred
                        )
                        _ <- useNativeScript(witness.scriptSource, witness.additionalSigners)
                    } yield ()
                case witness: TwoArgumentPlutusScriptWitness =>
                    for {
                        _ <- assertCredentialMatchesWitness(
                          credAction,
                          witness,
                          cred
                        )
                        _ <- usePlutusScript(witness.scriptSource, witness.additionalSigners)
                        _ <- {
                            val detachedRedeemer = DetachedRedeemer(
                              datum = witness.redeemer,
                              purpose = credAction match {
                                  case Operation.Withdraw(stakeAddress) =>
                                      RedeemerPurpose.ForReward(
                                        RewardAccount(stakeAddress)
                                      )
                                  case Operation.CertificateOperation(cert) =>
                                      RedeemerPurpose.ForCert(cert)
                                  case Operation.Minting(scriptHash) =>
                                      RedeemerPurpose.ForMint(scriptHash)
                                  case Operation.Voting(voter) =>
                                      RedeemerPurpose.ForVote(voter)
                                  case Operation.Proposing(proposal) =>
                                      RedeemerPurpose.ForPropose(proposal)
                              }
                            )
                            modify0(ctx =>
                                ctx.focus(_.redeemers)
                                    .modify(redeemers =>
                                        appendDistinct(detachedRedeemer, redeemers)
                                    )
                            )
                        }
                    } yield ()
            }
        } yield ()

    def assertCredentialMatchesWitness[
        A <: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness
    ](
        action: Operation,
        witness: A,
        cred: Credential
    )(using hwk: HasWitnessKind[A]): BuilderM[Unit] = {

        val wrongCredErr = WrongCredentialType(action, hwk.witnessKind, cred)

        val result: BuilderM[Unit] = witness match {
            case PubKeyWitness =>
                cred.keyHashOption match {
                    case Some(_) => pure0(())
                    case None    => liftF0(Left(wrongCredErr))
                }

            case witness: NativeScriptWitness =>
                for {
                    scriptHash <- cred.scriptHashOption match {
                        case Some(hash) => pure0(hash)
                        case None       => liftF0(Left(wrongCredErr))
                    }
                    _ <- assertScriptHashMatchesSource(scriptHash, witness.scriptSource)
                } yield ()

            case witness: TwoArgumentPlutusScriptWitness =>
                for {
                    scriptHash <- cred.scriptHashOption match {
                        case Some(hash) => pure0(hash)
                        case None       => liftF0(Left(wrongCredErr))
                    }
                    _ <- assertScriptHashMatchesSource(scriptHash, witness.scriptSource)
                } yield ()
        }
        result
    }

    // -------------------------------------------------------------------------
    // Common functions for spending/non-spending witnesses
    // -------------------------------------------------------------------------

    /** Assert that the given script hash either matches the script provided directly, or is
      * otherwise already attached to the transaction as a CIP-33 script or as a pre-existing
      * witness.
      * @param neededScriptHash
      *   The script hash we are expecting to find
      * @param scriptSource
      *   Where we should look for the script
      * @return
      */
    private def assertScriptHashMatchesSource(
        neededScriptHash: ScriptHash,
        scriptSource: ScriptSource[Script]
    ): BuilderM[Unit] =
        scriptSource match {
            case ScriptSource.NativeScriptValue(script) =>
                assertScriptHashMatchesScript(neededScriptHash, script)
            case ScriptSource.NativeScriptAttached =>
                assertAttachedScriptExists(neededScriptHash)
            case ScriptSource.PlutusScriptValue(script) =>
                assertScriptHashMatchesScript(neededScriptHash, script)
            case ScriptSource.PlutusScriptAttached => assertAttachedScriptExists(neededScriptHash)
        }

    private def assertScriptHashMatchesScript(
        scriptHash: ScriptHash,
        script: Script
    ): BuilderM[Unit] = {
        if scriptHash != script.scriptHash then {
            liftF0(
              Left(IncorrectScriptHash(script, scriptHash))
            )
        } else {
            pure0(())
        }
    }

    /** Given a script hash, check the context to ensure that a script matching the given script
      * hash is attached to the transaction either as a CIP-33 ref script or in the witness set
      */
    private def assertAttachedScriptExists(scriptHash: ScriptHash): BuilderM[Unit] =
        for {
            ctx <- get0
            resolvedScripts <- liftF0(
              AllResolvedScripts
                  .allResolvedScripts(
                    ctx.transaction,
                    ctx.resolvedUtxos.utxos
                  )
                  .left
                  .map(_ => ScriptResolutionError)
            )
            _ <-
                if resolvedScripts.map(_.scriptHash).contains(scriptHash)
                then pure0(())
                else
                    liftF0(
                      Left(
                        AttachedScriptNotFound(scriptHash)
                      )
                    )
        } yield ()

    // -------------------------------------------------------------------------
    // ScriptSource
    // -------------------------------------------------------------------------

    private def usePubKeyWitness(expectedSigner: ExpectedSigner): BuilderM[Unit] =
        modify0(Focus[Context](_.expectedSigners).modify(_ + expectedSigner))

    private def useNativeScript(
        nativeScript: ScriptSource[Script.Native],
        additionalSigners: Set[ExpectedSigner]
    ): BuilderM[Unit] = {
        for {
            // Regardless of how the witness is passed, add the additional signers
            _ <- modify0(
              Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners)
            )

            _ <- nativeScript match {
                case ScriptSource.NativeScriptValue(ns) =>
                    modify0(
                      // Add the native script to the witness set
                      unsafeCtxWitnessL
                          .refocus(_.nativeScripts)
                          .modify(s => TaggedSortedSet.from(appendDistinct(ns, s.toSeq)))
                    )
                // Script should already be attached, see [[assertAttachedScriptExists]]
                case ScriptSource.NativeScriptAttached => pure0(())
            }
        } yield ()
    }

    /** Returns Left if the input already exists in txBody.inputs or txBody.refInputs */
    private def assertInputDoesNotAlreadyExist(input: TransactionInput): BuilderM[Unit] =
        for {
            state <- get0
            _ <-
                if (state.transaction.body.value.inputs.toSortedSet ++ state.transaction.body.value.referenceInputs.toSortedSet)
                        .contains(input)
                then liftF0(Left(InputAlreadyExists(input)))
                else pure0(())
        } yield ()

    private def usePlutusScript(
        plutusScript: ScriptSource[PlutusScript],
        additionalSigners: Set[ExpectedSigner]
    ): BuilderM[Unit] =
        for {
            // Add script's additional signers to txBody.requiredSigners
            _ <- modify0(
              (Focus[Context](_.transaction) >>> txBodyL)
                  .refocus(_.requiredSigners)
                  .modify((s: TaggedSortedSet[AddrKeyHash]) =>
                      TaggedSortedSet.from(
                        s.toSortedSet ++ additionalSigners.map(_.hash)
                      )
                  )
            )

            // Add to expected signers
            _ <- modify0(
              Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners)
            )

            _ <- plutusScript match {
                case ScriptSource.PlutusScriptValue(ps: PlutusScript) =>
                    // Add the script value to the appropriate field
                    ps match {
                        case v1: Script.PlutusV1 =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV1Scripts)
                                  .modify(s => TaggedSortedSet.from(appendDistinct(v1, s.toSeq)))
                            )
                        case v2: Script.PlutusV2 =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV2Scripts)
                                  .modify(s => TaggedSortedSet.from(appendDistinct(v2, s.toSeq)))
                            )
                        case v3: Script.PlutusV3 =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV3Scripts)
                                  .modify(s => TaggedSortedSet.from(appendDistinct(v3, s.toSeq)))
                            )
                    }
                // Script should already be attached, see [[assertAttachedScriptExists]]
                case ScriptSource.PlutusScriptAttached => pure0(())
            }
        } yield ()

    // -------------------------------------------------------------------------
    // Common assertions
    // -------------------------------------------------------------------------

    /** Ensure that the network id of the address matches the network id of the builder context.
      */
    private def assertNetworkId(addr: Address): BuilderM[Unit] =
        for {
            context: Context <- get0
            addrNetwork <- addr.getNetwork match
                case Some(network) => pure0(network)
                case None =>
                    liftF0(
                      Left(ByronAddressesNotSupported(addr))
                    )
            _ <-
                if context.network != addrNetwork
                then
                    liftF0(
                      Left(WrongNetworkId(addr))
                    )
                else pure0(())
        } yield ()

// ===================================
// Step processing errors
// ===================================

sealed trait StepError:
    def explain: String

object StepError {
    case class Unimplemented(description: String) extends StepError {
        override def explain: String = s"$description is not yet implemented. If you need it, " +
            s"submit a request at $bugTrackerUrl."
    }

    // TODO: Remove this error and just use a nonzero type
    case class CannotMintZero(scriptHash: ScriptHash, assetName: AssetName) extends StepError {
        override def explain: String =
            "You cannot pass a \"amount = zero\" to a mint step, but we recieved it for" +
                s"(policyId, assetName) == ($scriptHash, $assetName)." +
                "\n You should not use the Mint step to calculate your mint amounts."
    }

    // TODO: more verbose error -- we'll need to pass more information from the assertion/step.
    case class InputAlreadyExists(input: TransactionInput) extends StepError {
        override def explain: String =
            s"The transaction input $input already exists in the transaction as " +
                "either an input or reference input."
    }

    case class ResolvedUtxosIncoherence(
        input: TransactionInput,
        existingOutput: TransactionOutput,
        incoherentOutput: TransactionOutput
    ) extends StepError {
        override def explain: String =
            "The context's resolvedUtxos already contain an input associated with a different output." +
                s"\nInput: $input" +
                s"\nExisting Output: $existingOutput" +
                s"\nIncoherent Output: $incoherentOutput"
    }

    case class CollateralNotPubKey(utxo: TransactionUnspentOutput) extends StepError {
        override def explain: String =
            s"The UTxO passed as a collateral input is not a PubKey UTxO. UTxO: $utxo"
    }

    // TODO: This error could probably be improved.
    case class CannotExtractSignatures(step: TransactionBuilderStep) extends StepError {
        override def explain: String =
            s"Could not extract signatures via _.additionalSigners from $step"
    }

    case class DatumIsMissing(utxo: TransactionUnspentOutput) extends StepError {
        override def explain: String =
            "Given witness to spend an output requires a datum that is missing: $utxo"
    }

    case class IncorrectDatumHash(
        utxo: TransactionUnspentOutput,
        datum: Data,
        datumHash: DataHash
    ) extends StepError {
        override def explain
            : String = "You provided a `DatumWitness` with a datum that does not match the datum hash present in a transaction output.\n " +
            s" Datum: $datum (CBOR: ${ByteString.fromArray(Cbor.encode(datum).toByteArray).toHex})\n  " +
            s" Datum hash: ${ByteString.fromArray(Cbor.encode(datumHash).toByteArray)}\n  " +
            s"UTxO: $utxo"
    }

    case class IncorrectScriptHash(
        script: Script,
        hash: ScriptHash
    ) extends StepError {
        override def explain: String = script match {
            case nativeScript: Script.Native =>
                s"Provided script hash ($hash) does not match the provided native script ($nativeScript)"
            case plutusScript: PlutusScript =>
                s"Provided script hash ($hash) does not match the provided Plutus script ($plutusScript)"
        }
    }

    case class RedeemerComputationFailed(message: String) extends StepError {
        override def explain: String =
            s"Failed to compute delayed redeemer: $message"
    }

    case class WrongOutputType(
        expectedType: WitnessKind,
        utxo: TransactionUnspentOutput
    ) extends StepError {
        override def explain: String =
            "The UTxO you provided requires no witness, because the payment credential of the address is a `PubKeyHash`. " +
                s"UTxO: $utxo"
    }

    case class WrongCredentialType(
        action: Operation,
        expectedType: WitnessKind,
        cred: Credential
    ) extends StepError {
        override def explain: String =
            s"${action.explain} ($action) requires a $expectedType witness: $cred"
    }

    case class DatumWitnessNotProvided(utxo: TransactionUnspentOutput) extends StepError {
        override def explain: String =
            "The output you are trying to spend contains a datum hash, you need to provide " +
                s"a `DatumValue`, output: $utxo"
    }

    case class DatumValueForUtxoWithInlineDatum(utxo: TransactionUnspentOutput, datum: Datum)
        extends StepError {
        override def explain: String =
            "You can't provide datum value for a utxo with inlined datum: " +
                s"You tried to provide: $datum for the UTxO: $utxo"
    }

    case class UnneededDeregisterWitness(
        stakeCredential: StakeCredential,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ) extends StepError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, " +
                "but the stake credential you are trying to issue a deregistering certificate for " +
                "is a PubKeyHash credential. You should omit the provided credential witness for this " +
                s"credential: $stakeCredential. Provided witness: $witness"
    }

    case class UnneededSpoVoteWitness(
        cred: Credential,
        witness: TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ) extends StepError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, but the corresponding Voter is " +
                "SPO (Stake Pool Operator). You should omit the provided credential witness " +
                s"for this credential: $cred. Provided witness: $witness"
    }

    case class UnneededProposalPolicyWitness(
        proposal: ProposalProcedure,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ) extends StepError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, but the corresponding proposal" +
                " does not need to validate against the proposal policy. You should omit the " +
                s"provided credential witness for this proposal: $proposal. Provided witness: $witness"
    }

    case class RedeemerIndexingError(redeemer: Redeemer) extends StepError {
        override def explain: String =
            s"Redeemer indexing error. Problematic redeemer that does not have a valid index: $redeemer"
    }

    case class RedeemerIndexingInternalError(
        tx: Transaction,
        steps: Seq[TransactionBuilderStep]
    ) extends StepError {
        override def explain: String =
            s"Internal redeemer indexing error. Please report as bug: $bugTrackerUrl\nDebug info: " +
                s"Transaction: $tx, steps: ${steps
                        .mkString(", ")}"
    }

    private val bugTrackerUrl: String = "https://github.com/cardano-hydrozoa/hydrozoa/issues"

    case class WrongNetworkId(address: Address) extends StepError {
        override def explain: String =
            "The following `Address` that was specified in one of the UTxOs has a `NetworkId`" +
                s" different from the one `TransactionBody` has: $address"
    }

    case class CollateralWithTokens(
        utxo: TransactionUnspentOutput
    ) extends StepError {
        override def explain: String =
            "The UTxO you provided as a collateral must contain only ada. " +
                s"UTxO: $utxo"
    }

    case class AttachedScriptNotFound(scriptHash: ScriptHash) extends StepError {
        override def explain: String =
            s"No witness or ref/spent output is found for script matching $scriptHash." +
                "Note that the builder steps are not commutative: you must attach the script " +
                "before using an AttachedScript ScriptWitness."
    }

    case class ByronAddressesNotSupported(address: Address) extends StepError {
        override def explain: String =
            s"Byron addresses are not supported: $address."
    }

    // We can't really return meaningful information, because scalus doesn't
    // provide it. See [[assertAttachedScriptExists]]
    case object ScriptResolutionError extends StepError {
        override def explain: String =
            "An error was returned when trying to resolve scripts for the transaction."
    }

    case class FeeAlreadySet(currentFee: Long) extends StepError {
        override def explain: String =
            s"The fee ($currentFee) is already set. You cannot set the fee more than once."
    }

    case class ValidityStartSlotAlreadySet(slot: Long) extends StepError {
        override def explain: String =
            s"The validity start slot ($slot) is already set. You cannot set the validity start slot more than once."
    }

    case class ValidityEndSlotAlreadySet(slot: Long) extends StepError {
        override def explain: String =
            s"The validity end slot ($slot) is already set. You cannot set the validity end slot more than once."
    }
}

// -------------------------------------------------------------------------
// auxiliary types, extensions, helpers
// -------------------------------------------------------------------------

// TODO: itd be nice to make this opaque and only return from chain queries
// NOTE (Peter, 2025-09-23): this comes from
// https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/TransactionUnspentOutput.purs
case class TransactionUnspentOutput(input: TransactionInput, output: TransactionOutput):
    def toTuple: (TransactionInput, TransactionOutput) = (input, output)

object TransactionUnspentOutput:
    def apply(utxo: (TransactionInput, TransactionOutput)): TransactionUnspentOutput =
        TransactionUnspentOutput(utxo._1, utxo._2)

// NOTE (Peter, 2025-09-23): this comes from https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/StakeCredential.purs
case class StakeCredential(credential: Credential)

extension (network: Network)
    def toNetworkId: Int = network match
        case Network.Testnet  => 0
        case Network.Mainnet  => 1
        case Network.Other(b) => b.toInt

object NetworkExtensions:
    /** Convert integer network ID to Network */
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
    case SomeStepError(e: StepError)
    // case EvaluationError(e: PlutusScriptEvaluationException)
    case BalancingError(e: TxBalancingError)
    case ValidationError(e: TransactionException)

    override def toString: String = this match {
        case SomeStepError(e) =>
            s"Step processing error: ${e.getClass.getSimpleName} - ${e.explain}"
        case BalancingError(TxBalancingError.EvaluationFailed(psee)) =>
            s"Plutus script evaluation failed: ${psee.getMessage}, execution trace: ${psee.logs.mkString(" <CR> ")}"
        case BalancingError(TxBalancingError.Failed(other)) =>
            s"Exception during balancing: ${other.getMessage}"
        case BalancingError(TxBalancingError.CantBalance(lastDiff)) =>
            s"Can't balance: last diff $lastDiff"
        case BalancingError(TxBalancingError.InsufficientFunds(diff, required)) =>
            s"Insufficient funds: need $required more"
        case ValidationError(e) =>
            s"Transaction validation failed: ${e.getClass.getSimpleName} - ${e.getMessage}"
    }

// -------------------------------------------------------------------------
// Extra stuff to keep here
// -------------------------------------------------------------------------

extension (self: TransactionOutput)
    def datumOption: Option[DatumOption] =
        self match {
            case TransactionOutput.Shelley(_, _, datumHash) =>
                datumHash.map(DatumOption.Hash(_))
            case Babbage(_, _, datumOption, _) =>
                datumOption match {
                    case Some(value) => Some(value)
                    case None        => None
                }
        }

// ----

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
        .modify(s => TaggedSortedSet.from(s.toSortedSet ++ generateUniqueKeys(numberOfKeys)))
}

/** remove at most 256 keys, must be used in conjunction with addDummyVKeys */
def removeDummySignatures(numberOfKeys: Int, tx: Transaction): Transaction = {
    modifyWs(
      tx,
      ws =>
          ws.copy(vkeyWitnesses =
              TaggedSortedSet.from(ws.vkeyWitnesses.toSortedSet -- generateUniqueKeys(numberOfKeys))
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
