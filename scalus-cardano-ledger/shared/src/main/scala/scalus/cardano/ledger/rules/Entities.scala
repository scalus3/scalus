package scalus.cardano.ledger
package rules

import scalus.cardano.address.Network

import scala.annotation.threadUnsafe

// It's mutable state for transient calculation
class Context(
    var fee: Coin = Coin.zero,
    val env: UtxoEnv = UtxoEnv.default,
    val slotConfig: SlotConfig = SlotConfig.mainnet,
    /** Evaluator mode for Plutus script execution.
      *   - `Validate`: Enforce budget limits (default, production mode)
      *   - `EvaluateAndComputeCost`: Ignore budget limits, just compute costs (for testing)
      */
    val evaluatorMode: EvaluatorMode = EvaluatorMode.Validate
)

object Context {
    def testMainnet(slot: SlotNo = 0): Context = Context(env = UtxoEnv.testMainnet(slot))
}

case class State(
    utxos: Utxos = Utxos.empty,
    certState: CertState = CertState.empty,
    deposited: Coin = Coin.zero, // Lazy field used only for assertions
    fees: Coin = Coin.zero, // Accumulated transaction fees
    govState: GovState = GovState.empty, // Governance state
    stakeDistribution: StakeMap = StakeMap.empty, // Stake distribution
    donation: Coin = Coin.zero // Donation amount
)

case class UtxoEnv(slot: SlotNo, params: ProtocolParams, certState: CertState, network: Network)
object UtxoEnv {
    // Delegates to platform-specific UtxoEnvDefaults (JVM loads from resources, JS embeds JSON)
    // TODO: remove
    @threadUnsafe lazy val default: UtxoEnv = UtxoEnvDefaults.default

    // TODO: remove
    def testMainnet(slot: SlotNo = 0): UtxoEnv = UtxoEnvDefaults.testMainnet(slot)
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

        def transit[ErrorT <: TransactionException](
            mutators: Iterable[Mutator { type Error <: ErrorT }],
            context: Mutator#Context,
            state: Mutator#State,
            event: Mutator#Event
        ): (Mutator { type Error = ErrorT })#Result = {
            mutators.foldLeft(success(state): (Mutator { type Error = ErrorT })#Result) {
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

        def success(state: STS#State): (Mutator { type Error = Nothing })#Result = Right(state)

        private val defaultName: String = "AnonymousMutator"
    }
}
