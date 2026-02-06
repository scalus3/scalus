package scalus.testing

import org.scalacheck.commands.Commands
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Prop}
import scalus.cardano.ledger.{Transaction, TransactionHash}
import scalus.cardano.node.{BlockchainReader, Emulator, SubmitError}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Failure, Success, Try}

/** State for ScalaCheck Commands testing (abstract model).
  *
  * This state is used by ScalaCheck's Commands framework to track the abstract model state during
  * property-based testing. It wraps an [[ImmutableEmulator]] (which provides the blockchain state),
  * the contract-specific state of type `S`, and an RNG seed for deterministic command generation.
  *
  * @param emulator
  *   the immutable emulator representing blockchain state
  * @param contractState
  *   the contract-specific state extracted from UTxOs
  * @param rng
  *   ScalaCheck RNG seed for deterministic generation
  * @tparam S
  *   the contract state type
  */
case class CommandsState[S](
    emulator: ImmutableEmulator,
    contractState: S,
    rng: Seed
)

/** Bridges [[ContractStepVariations]] with ScalaCheck's stateful testing framework.
  *
  * This adapter enables property-based testing of smart contract interactions by:
  *   - Using [[ImmutableEmulator]] as the abstract model (State)
  *   - Using mutable [[scalus.cardano.node.Emulator]] as the system under test (Sut)
  *   - Generating commands from [[ContractStepVariations.allActions]]
  *   - Checking invariants after successful transactions
  *
  * ==Two Emulators Pattern==
  *
  * Per ScalaCheck Commands pattern, we maintain two separate emulators:
  *   - '''State''' ([[CommandsState]]): The abstract model - wraps [[ImmutableEmulator]] + contract
  *     state `S` + RNG seed. Used for `genCommand`, `nextState`, and `preCondition`.
  *   - '''Sut''' ([[scalus.cardano.node.Emulator]]): The system under test - the existing mutable
  *     Emulator class. Used for `run` (actual execution) and `postCondition` verification.
  *
  * This separation allows ScalaCheck to detect discrepancies between model and implementation.
  *
  * ==Slot Advancement==
  *
  * Slot advancement is controlled by overriding [[ContractStepVariations.slotDelays]] in the step.
  * This returns slot delays that are included as [[StepAction.Wait]] actions alongside transaction
  * submissions.
  *
  * ==JVM-Only==
  *
  * This class uses `Await.result` for synchronous execution and is JVM-only. For cross-platform
  * testing, use [[ScenarioExplorer]] instead.
  *
  * ==Example Usage==
  * {{{
  * val auctionBidStep = new ContractStepVariations[AuctionState] {
  *   def extractState(reader: BlockchainReader)(using EC) = ???
  *   def makeBaseTx(reader: BlockchainReader, state: AuctionState)(using EC) = ???
  *   def variations = TxVariations.standard.default(...)
  *   override def slotDelays(state: AuctionState) = Seq(10L, 100L)
  * }
  *
  * // Without invariant checking
  * val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
  * val commands = ContractScalaCheckCommands(emulator, auctionBidStep)()
  * commands.property().check()
  *
  * // With invariant checking
  * val commandsWithInvariants = ContractScalaCheckCommands(emulator, auctionBidStep) {
  *   (reader, state) => Future {
  *     Prop(state.balance >= Coin.zero) && Prop(state.highestBidder.isDefined)
  *   }
  * }
  * }}}
  *
  * @param initialEmulator
  *   the starting emulator state with funded addresses
  * @param step
  *   the contract step variations to test
  * @param timeout
  *   timeout for async operations (default: 30 seconds)
  * @param checkInvariants
  *   async function to check invariants after successful transactions
  * @param ec
  *   execution context for async operations
  * @tparam S
  *   the contract state type
  */
class ContractScalaCheckCommands[S](
    initialEmulator: ImmutableEmulator,
    step: ContractStepVariations[S],
    timeout: FiniteDuration = Duration(30, "seconds")
)(
    checkInvariants: (BlockchainReader, S) => scala.concurrent.Future[Prop] =
        (_: BlockchainReader, _: S) => scala.concurrent.Future.successful(Prop.passed)
)(using ec: ExecutionContext)
    extends Commands {

    override type State = CommandsState[S]
    override type Sut = scalus.cardano.node.Emulator

    /** Generate initial abstract state.
      *
      * Extracts the contract state from the initial emulator and creates a CommandsState.
      */
    override def genInitialState: Gen[State] = {
        val reader = initialEmulator.asReader
        val contractStateFuture = step.extractState(reader)
        val contractState = Await.result(contractStateFuture, timeout)
        val initialSeed = Seed.random()
        Gen.const(CommandsState(initialEmulator, contractState, initialSeed))
    }

    /** Generate a command based on current abstract state.
      *
      * Uses [[ContractStepVariations.allActions]] to generate actions (transactions + slot delays),
      * then randomly selects one.
      */
    override def genCommand(state: State): Gen[Command] = {
        val reader = state.emulator.asReader
        val actionsFuture = step.allActions(reader, state.contractState)
        val actions = Await.result(actionsFuture, timeout)

        val commands = actions.map {
            case StepAction.Submit(tx)  => SubmitTxCommand(tx)
            case StepAction.Wait(slots) => AdvanceSlotCommand(slots)
        }

        if commands.isEmpty then Gen.fail
        else Gen.oneOf(commands)
    }

    /** Create a new system under test from abstract state.
      *
      * Converts the [[ImmutableEmulator]] to a mutable [[scalus.cardano.node.Emulator]].
      */
    override def newSut(state: State): Sut = state.emulator.toEmulator

    /** Clean up system under test (no-op for Emulator). */
    override def destroySut(sut: Sut): Unit = ()

    /** Check if a new Sut can be created.
      *
      * Always returns true since Emulator has no resource constraints.
      */
    override def canCreateNewSut(
        newState: State,
        initSuts: scala.collection.Iterable[State],
        runningSuts: scala.collection.Iterable[Sut]
    ): Boolean = true

    /** Check if initial state is valid for testing.
      *
      * Always returns true since we construct valid initial states.
      */
    override def initialPreCondition(state: State): Boolean = true

    /** Command to submit a transaction to the emulator. */
    case class SubmitTxCommand(tx: Transaction) extends Command {
        override type Result = Either[SubmitError, TransactionHash]

        /** Execute the transaction on the system under test. */
        override def run(sut: Sut): Result = {
            import scala.concurrent.Future
            val resultFuture: Future[Either[SubmitError, TransactionHash]] = sut.submit(tx)
            Await.result(resultFuture, timeout)
        }

        /** Compute the next abstract state after this command.
          *
          * If the transaction succeeds in the abstract model, update the emulator state and extract
          * new contract state. If it fails, just advance the RNG.
          */
        override def nextState(state: State): State = {
            state.emulator.submit(tx) match {
                case Right((_, newEmulator)) =>
                    val newContractState = Try(
                      Await.result(step.extractState(newEmulator.asReader), timeout)
                    ).getOrElse(state.contractState)
                    state.copy(
                      emulator = newEmulator,
                      contractState = newContractState,
                      rng = state.rng.next
                    )
                case Left(_) =>
                    state.copy(rng = state.rng.next)
            }
        }

        /** Check if this command can be executed in the given state.
          *
          * Always returns true - transaction rejection is expected behavior for attack variations.
          */
        override def preCondition(state: State): Boolean = true

        /** Verify the actual result matches expectations.
          *
          * On success, run invariant checks. On transaction rejection, pass (expected for attack
          * variations). On exception, fail with the exception.
          *
          * @param state
          *   the abstract state ''after'' nextState was called
          * @param result
          *   the actual result from running on the Sut
          */
        override def postCondition(state: State, result: Try[Result]): Prop = {
            result match {
                case Success(Right(_)) =>
                    // Transaction succeeded - check invariants
                    val reader = state.emulator.asReader
                    val invariantProp = Await.result(
                      checkInvariants(reader, state.contractState),
                      timeout
                    )
                    invariantProp
                case Success(Left(_)) =>
                    // Transaction rejected - expected for attack variations
                    Prop.passed
                case Failure(e) =>
                    Prop.exception(e)
            }
        }
    }

    /** Command to advance the slot in the emulator. */
    case class AdvanceSlotCommand(slots: Long) extends Command {
        override type Result = Unit

        override def run(sut: Sut): Unit = {
            import scala.concurrent.Future
            val slotFuture: Future[Long] = sut.currentSlot
            val currentSlot = Await.result(slotFuture, timeout)
            sut.setSlot(currentSlot + slots)
        }

        override def nextState(state: State): State = {
            state.copy(
              emulator = state.emulator.advanceSlot(slots),
              rng = state.rng.next
            )
        }

        override def preCondition(state: State): Boolean = slots > 0

        override def postCondition(state: State, result: Try[Result]): Prop = {
            result match {
                case Success(_) => Prop.passed
                case Failure(e) => Prop.exception(e)
            }
        }
    }
}

object ContractScalaCheckCommands {

    /** Create a ContractScalaCheckCommands adapter.
      *
      * @param emulator
      *   the starting emulator state with funded addresses (will be converted to ImmutableEmulator)
      * @param step
      *   the contract step variations to test
      * @param timeout
      *   timeout for async operations (default: 30 seconds)
      * @param checkInvariants
      *   async function to check invariants after successful transactions
      * @return
      *   a Commands instance ready for property testing
      */
    def apply[S](
        emulator: Emulator,
        step: ContractStepVariations[S],
        timeout: FiniteDuration = Duration(30, "seconds")
    )(
        checkInvariants: (BlockchainReader, S) => scala.concurrent.Future[Prop] =
            (_: BlockchainReader, _: S) => scala.concurrent.Future.successful(Prop.passed)
    )(using ExecutionContext): ContractScalaCheckCommands[S] =
        new ContractScalaCheckCommands[S](ImmutableEmulator.fromEmulator(emulator), step, timeout)(
          checkInvariants
        )
}
