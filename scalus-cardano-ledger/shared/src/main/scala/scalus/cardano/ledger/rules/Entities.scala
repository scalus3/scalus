package scalus.cardano.ledger
package rules

import scalus.cardano.address.Network
import scalus.uplc.DebugScript

import scala.annotation.nowarn

// It's mutable state for transient calculation
case class Context(
    var fee: Coin = Coin.zero,
    env: UtxoEnv = UtxoEnv.default,
    slotConfig: SlotConfig = SlotConfig.mainnet,
    /** Evaluator mode for Plutus script execution.
      *   - `Validate`: Enforce budget limits (default, production mode)
      *   - `EvaluateAndComputeCost`: Ignore budget limits, just compute costs (for testing)
      */
    evaluatorMode: EvaluatorMode = EvaluatorMode.Validate,
    /** Debug scripts for diagnostic replay when release scripts fail with empty logs. */
    debugScripts: Map[ScriptHash, DebugScript] = Map.empty
)

object Context {
    def testMainnet(): Context = testMainnet(0)
    def testMainnet(slot: SlotNo): Context = Context(env = UtxoEnv.testMainnet(slot))
}

case class State(
    utxos: Utxos = Utxos.empty,
    certState: CertState = CertState.empty,
    deposited: Coin = Coin.zero, // The deposits the ledger holds, `utxosDeposited`, spec [BK-1]
    fees: Coin = Coin.zero, // Accumulated transaction fees
    govState: GovState = GovState.empty, // Governance state
    stakeDistribution: StakeMap = StakeMap.empty, // Stake distribution
    donation: Coin = Coin.zero // Donation amount
)

/** The environment a transaction is validated in.
  *
  * @param treasury
  *   the treasury as of the last epoch boundary, the `casTreasury` Haskell passes to LEDGER. A tx
  *   that states a `currentTreasuryValue` must state this value, spec [SC-6].
  */
case class UtxoEnv(
    slot: SlotNo,
    params: ProtocolParams,
    certState: CertState,
    network: Network,
    treasury: Coin
) {

    /** An environment with an empty treasury, as the deprecated 4-argument `apply`. */
    @deprecated("use the constructor that takes the treasury", "1.3.0")
    def this(slot: SlotNo, params: ProtocolParams, certState: CertState, network: Network) =
        this(slot, params, certState, network, Coin.zero)
}
object UtxoEnv {

    /** An environment with an empty treasury. */
    @deprecated("use the overload that takes the treasury", "1.3.0")
    def apply(
        slot: SlotNo,
        params: ProtocolParams,
        certState: CertState,
        network: Network
    ): UtxoEnv =
        UtxoEnv(slot, params, certState, network, Coin.zero)

    // Uses CardanoInfo.mainnet.protocolParams (inlined at compile time via macro)
    // TODO: remove
    lazy val default: UtxoEnv = UtxoEnvDefaults.default

    // TODO: remove
    def testMainnet(): UtxoEnv = testMainnet(0)
    def testMainnet(slot: SlotNo): UtxoEnv = UtxoEnvDefaults.testMainnet(slot)
}

sealed trait STS {
    final type Context = scalus.cardano.ledger.rules.Context
    final type State = scalus.cardano.ledger.rules.State
    final type Event = Transaction
    type Value
    type Error <: TransactionException
    final type Result = Either[Error, Value]

    def name: String = this.getClass.getSimpleName.stripSuffix("$")

    def apply(context: Context, state: State, event: Event): Result

    protected final def failure(error: Error): Result = Left(error)
}

object STS {
    trait Validator extends STS {
        override final type Value = Unit

        def validate(context: Context, state: State, event: Event): Result

        override final def apply(context: Context, state: State, event: Event): Result =
            validate(context, state, event)

        protected final def success: Result = Validator.success
    }

    object Validator {
        def apply[ErrorT <: TransactionException](
            validator: (Validator#Context, Validator#State, Validator#Event) => (Validator {
                type Error = ErrorT
            })#Result,
            validatorName: String = Validator.defaultName
        ): Validator { type Error = ErrorT } = new Validator {
            override final type Error = ErrorT

            override def name: String = validatorName

            override def validate(context: Context, state: State, event: Event): Result =
                validator(context, state, event)
        }

        def apply[ErrorT <: TransactionException](
            validators: Iterable[Validator { type Error <: ErrorT }],
            validatorName: String
        ): Validator { type Error = ErrorT } =
            Validator[ErrorT](Validator.validate[ErrorT](validators, _, _, _), validatorName)

        def apply[ErrorT <: TransactionException](
            validators: Iterable[Validator { type Error <: ErrorT }]
        ): Validator { type Error = ErrorT } = Validator[ErrorT](validators, Validator.defaultName)

        def validate[ErrorT <: TransactionException](
            validators: Iterable[Validator { type Error <: ErrorT }],
            context: Validator#Context,
            state: Validator#State,
            event: Validator#Event
        ): (Validator { type Error = ErrorT })#Result = {
            validators.foldLeft(success: (Validator { type Error = ErrorT })#Result) {
                (acc, validator) => acc.flatMap(_ => validator.validate(context, state, event))
            }
        }

        val success: (Validator { type Error = Nothing })#Result = Right(())

        private val defaultName: String = "AnonymousValidator"
    }

    trait Mutator extends STS {
        override final type Value = State

        def transit(context: Context, state: State, event: Event): Result

        override final def apply(context: Context, state: State, event: Event): Result =
            transit(context, state, event)

        protected final def success(state: State): Result = Mutator.success(state)
    }

    object Mutator {
        def apply[ErrorT <: TransactionException](
            mutator: (Mutator#Context, Mutator#State, Mutator#Event) => (Mutator {
                type Error = ErrorT
            })#Result,
            mutatorName: String = Mutator.defaultName
        ): Mutator { type Error = ErrorT } = new Mutator {
            override final type Error = ErrorT

            override def name: String = mutatorName

            override def transit(context: Context, state: State, event: Event): Result =
                mutator(context, state, event)
        }

        def apply[ErrorT <: TransactionException](
            mutators: Iterable[Mutator { type Error <: ErrorT }],
            mutatorName: String
        ): Mutator { type Error = ErrorT } =
            Mutator[ErrorT](Mutator.transit[ErrorT](mutators, _, _, _), mutatorName)

        def apply[ErrorT <: TransactionException](
            mutators: Iterable[Mutator { type Error <: ErrorT }]
        ): Mutator { type Error = ErrorT } = Mutator[ErrorT](mutators, Mutator.defaultName)

        def apply[ErrorT <: TransactionException](
            validators: Iterable[Validator { type Error <: ErrorT }],
            mutators: Iterable[Mutator { type Error <: ErrorT }],
            mutatorName: String
        ): Mutator { type Error = ErrorT } =
            Mutator[ErrorT](Mutator.transit[ErrorT](validators, mutators, _, _, _), mutatorName)

        def apply[ErrorT <: TransactionException](
            validators: Iterable[Validator { type Error <: ErrorT }],
            mutators: Iterable[Mutator { type Error <: ErrorT }]
        ): Mutator { type Error = ErrorT } =
            Mutator[ErrorT](validators, mutators, Mutator.defaultName)

        /** Applies the mutators in order. A set with [[CertsMutator]] skips the deprecated per-kind
          * certificate mutators, which would apply the certificates a second time, spec [SC-22].
          */
        def transit[ErrorT <: TransactionException](
            mutators: Iterable[Mutator { type Error <: ErrorT }],
            context: Mutator#Context,
            state: Mutator#State,
            event: Mutator#Event
        ): (Mutator { type Error = ErrorT })#Result = {
            effective(mutators).foldLeft(success(state): (Mutator { type Error = ErrorT })#Result) {
                (acc, mutator) =>
                    acc.flatMap(currentState => mutator.transit(context, currentState, event))
            }
        }

        def transit[ErrorT <: TransactionException](
            validators: Iterable[Validator { type Error <: ErrorT }],
            mutators: Iterable[Mutator { type Error <: ErrorT }],
            context: Mutator#Context,
            state: Mutator#State,
            event: Mutator#Event
        ): (Mutator { type Error = ErrorT })#Result = {
            Validator.validate[ErrorT](validators, context, state, event).flatMap { _ =>
                Mutator.transit[ErrorT](mutators, context, state, event)
            }
        }

        /** Every failure of `event`, as the ledger collects them, spec [FL-1]: the failures of all
          * `validators`, then of all `mutators`, in the order of the ledger's interpreter, spec
          * [FL-7a]. `Right` holds the state after the mutators when nothing fails.
          *
          * Every validator and every mutator runs, whatever an earlier one returned; a mutator that
          * fails passes its input state to the next one, spec [FL-2]. [[GovMutator]] reports each
          * of its failures, and [[PlutusScriptsTransactionMutator]] reports its collect errors in
          * every case and evaluates the scripts only when nothing failed before it, as
          * `whenFailureFree` does, spec [FL-5]. The `Bad*InputsUTxOException`s that repeat a part
          * of `BadAllInputsUTxOException` are dropped, spec [FL-4]. The list is sorted stably by
          * the first ledger name of each failure, [[TransactionException.ledgerNames]].
          *
          * [[transit]] does not change: it stops at the first failure, in the iteration order of
          * the sets, spec [FL-3].
          */
        def transitAll(
            validators: Iterable[Validator],
            mutators: Iterable[Mutator],
            context: Mutator#Context,
            state: Mutator#State,
            event: Mutator#Event
        ): Either[Seq[TransactionException], Mutator#State] = {
            val validated: Seq[TransactionException] =
                validators.toSeq.flatMap(_.validate(context, state, event).left.toOption)
            val (failures, after) = effective(mutators).foldLeft((validated, state)) {
                case ((failures, current), mutator) =>
                    val result: Either[Seq[TransactionException], State] = mutator match
                        case GovMutator => GovMutator.transitAll(context, current, event, event.id)
                        case PlutusScriptsTransactionMutator =>
                            val failureFree = reported(failures).isEmpty
                            PlutusScriptsTransactionMutator
                                .transit(context, current, event, failureFree)
                                .left
                                .map(Seq(_))
                        case other => other.transit(context, current, event).left.map(Seq(_))
                    result match
                        case Right(next)  => (failures, next)
                        case Left(errors) => (failures ++ errors, current)
            }
            // CertsMutator repeats the check of CertsValidator, so the same failure comes twice
            val all = reported(failures).distinct
            if all.isEmpty then Right(after)
            else
                Left(
                  all.sortBy(e =>
                      TransactionException.ledgerOrder(TransactionException.ledgerNames(e).head)
                  )
                )
        }

        /** `failures` without the failures that repeat a part of `BadAllInputsUTxOException`, spec
          * [FL-4]; all of them when nothing else is left, so a failing tx keeps a failure, spec
          * [FL-4a].
          */
        private def reported(failures: Seq[TransactionException]): Seq[TransactionException] = {
            val kept = failures.filter {
                case _: TransactionException.BadInputsUTxOException |
                    _: TransactionException.BadCollateralInputsUTxOException |
                    _: TransactionException.BadReferenceInputsUTxOException =>
                    false
                case _ => true
            }
            if kept.isEmpty then failures else kept
        }

        def success(state: STS#State): (Mutator { type Error = Nothing })#Result = Right(state)

        private val defaultName: String = "AnonymousMutator"

        /** The 1.3 mutators that [[CertsMutator]] replaces. */
        @nowarn("cat=deprecation")
        private val perKindCertMutators: Set[Mutator] =
            Set(StakeCertificatesMutator, StakePoolCertificatesMutator, VotingCertificatesMutator)

        /** `mutators` without the per-kind certificate mutators when [[CertsMutator]] is among
          * them, spec [SC-22]; [[transit]] and [[transitAll]] run the same set, spec [FL-3].
          */
        private def effective[M <: Mutator](mutators: Iterable[M]): Iterable[M] =
            if mutators.exists(_ == CertsMutator) then mutators.filterNot(perKindCertMutators)
            else mutators
    }
}
