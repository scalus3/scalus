package scalus.cardano.ledger
package rules

import scalus.cardano.address.Network

// It's mutable state for transient calculation
class Context(
    var fee: Coin = Coin.zero,
    val env: UtxoEnv = UtxoEnv.default,
    val slotConfig: SlotConfig = SlotConfig.Mainnet
)

case class State(
    utxo: Utxos = Map.empty,
    certState: CertState = CertState.empty,
    deposited: Coin = Coin.zero, // Lazy field used only for assertions
    fees: Coin = Coin.zero, // Accumulated transaction fees
    govState: GovState = (), // Governance state
    stakeDistribution: StakeMap = Map.empty, // Stake distribution
    donation: Coin = Coin.zero // Donation amount
)

case class UtxoEnv(slot: SlotNo, params: ProtocolParams, certState: CertState, network: Network)
object UtxoEnv {
    // TODO: remove
    val default: UtxoEnv =

        val params: ProtocolParams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )

        // Load protocol parameters from a JSON file
        UtxoEnv(
          0,
          params,
          CertState.empty,
          Network.Testnet
        )
}

sealed trait STS {
    final type Context = scalus.cardano.ledger.rules.Context
    final type State = scalus.cardano.ledger.rules.State
    final type Event = Transaction
    type Value
    type Error <: TransactionException
    final type Result = Either[Error, Value]

    lazy val name: String = {
        val simpleName = this.getClass.getSimpleName
        if simpleName.endsWith("$") then simpleName.substring(0, simpleName.length - 1)
        else simpleName
    }

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
            })#Result
        ): Validator { type Error = ErrorT } = new Validator {
            override final type Error = ErrorT

            override def validate(context: Context, state: State, event: Event): Result =
                validator(context, state, event)
        }

        def apply[ErrorT <: TransactionException](
            validators: Iterable[Validator { type Error <: ErrorT }]
        ): Validator { type Error = ErrorT } = new Validator {
            override final type Error = ErrorT

            override def validate(context: Context, state: State, event: Event): Result =
                Validator.validate[ErrorT](validators, context, state, event)
        }

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
            })#Result
        ): Mutator { type Error = ErrorT } = new Mutator {
            override final type Error = ErrorT

            override def transit(context: Context, state: State, event: Event): Result =
                mutator(context, state, event)
        }

        def apply[ErrorT <: TransactionException](
            mutators: Iterable[Mutator { type Error <: ErrorT }]
        ): Mutator { type Error = ErrorT } = new Mutator {
            override final type Error = ErrorT

            override def transit(context: Context, state: State, event: Event): Result =
                Mutator.transit[ErrorT](mutators, context, state, event)
        }

        def apply[ErrorT <: TransactionException](
            validators: Iterable[Validator { type Error <: ErrorT }],
            mutators: Iterable[Mutator { type Error <: ErrorT }]
        ): Mutator { type Error = ErrorT } = new Mutator {
            override final type Error = ErrorT

            override def transit(context: Context, state: State, event: Event): Result =
                Mutator.transit[ErrorT](validators, mutators, context, state, event)
        }

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
    }
}
