package scalus.testing

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, DefaultMutators, DefaultValidators, PlutusScriptsTransactionMutator, STS, State, UtxoEnv}
import scalus.cardano.node.*

import scala.concurrent.{ExecutionContext, Future}

/** An immutable emulator for Cardano transactions.
  *
  * Unlike [[Emulator]], this emulator is fully immutable: every state-changing operation (submit,
  * advanceSlot) returns a new [[ImmutableEmulator]] instance, leaving the original unchanged.
  *
  * This makes it suitable for use in functional state-threading patterns such as the [[Scenario]]
  * monad, where branching and backtracking require independent state copies.
  */
case class ImmutableEmulator(
    state: State,
    env: UtxoEnv,
    slotConfig: SlotConfig = SlotConfig.mainnet,
    evaluatorMode: EvaluatorMode = EvaluatorMode.Validate,
    validators: Iterable[STS.Validator] = Emulator.defaultValidators,
    mutators: Iterable[STS.Mutator] = Emulator.defaultMutators
) {

    /** Current UTxO set. */
    def utxos: Utxos = state.utxos

    /** Current slot number. */
    def currentSlot: SlotNo = env.slot

    /** Current protocol parameters. */
    def protocolParams: ProtocolParams = env.params

    /** Current CardanoInfo snapshot. */
    def cardanoInfo: CardanoInfo = CardanoInfo(env.params, env.network, slotConfig)

    /** Submit a transaction, returning the transaction hash and a new emulator with updated state.
      *
      * The original emulator is not modified.
      */
    def submit(tx: Transaction): Either[SubmitError, (TransactionHash, ImmutableEmulator)] = {
        val context = Context(env = env, slotConfig = slotConfig, evaluatorMode = evaluatorMode)
        STS.Mutator.transit(validators, mutators, context, state, tx) match {
            case Right(newState) =>
                Right((tx.id, copy(state = newState)))
            case Left(e: TransactionException) =>
                Left(SubmitError.fromException(e))
        }
    }

    /** Advance the slot by the given number of slots. */
    def advanceSlot(n: Long): ImmutableEmulator = copy(env = env.copy(slot = env.slot + n))

    /** Set the slot to a specific value. */
    def setSlot(slot: SlotNo): ImmutableEmulator = copy(env = env.copy(slot = slot))

    /** Find UTxOs matching the given query. */
    def findUtxos(query: UtxoQuery): Either[UtxoQueryError, Utxos] =
        Right(EmulatorBase.evalQuery(utxos, query))

    /** Create a read-only [[BlockchainProvider]] snapshot.
      *
      * Submit through this provider works but discards the resulting state changes (the provider
      * cannot update the immutable emulator).
      */
    def asProvider: BlockchainProvider = new BlockchainProvider {
        override def executionContext: ExecutionContext = ExecutionContext.parasitic
        override def cardanoInfo: CardanoInfo = ImmutableEmulator.this.cardanoInfo
        override def fetchLatestParams: Future[ProtocolParams] =
            Future.successful(env.params)
        override def submit(
            transaction: Transaction
        ): Future[Either[SubmitError, TransactionHash]] =
            Future.successful(ImmutableEmulator.this.submit(transaction).map(_._1))
        override def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] =
            Future.successful(ImmutableEmulator.this.findUtxos(query))
        override def currentSlot: Future[SlotNo] =
            Future.successful(env.slot)
    }

    /** Convert to a mutable [[Emulator]]. */
    def toEmulator: Emulator = {
        val context = Context(env = env, slotConfig = slotConfig, evaluatorMode = evaluatorMode)
        new Emulator(
          initialUtxos = utxos,
          initialContext = context,
          validators = validators,
          mutators = mutators
        )
    }
}

object ImmutableEmulator {

    /** Create an ImmutableEmulator from a mutable [[EmulatorBase]].
      *
      * Captures a snapshot of the emulator's current state.
      */
    def fromEmulator(emulator: EmulatorBase): ImmutableEmulator = {
        val info = emulator.cardanoInfo
        val slot =
            emulator.currentSlot.value.get.get // EmulatorBase always returns completed Future
        val env = UtxoEnv(
          slot = slot,
          params = info.protocolParams,
          certState = CertState.empty,
          network = info.network
        )
        ImmutableEmulator(
          state = State(utxos = emulator.utxos),
          env = env,
          slotConfig = info.slotConfig,
          validators = emulator.validators,
          mutators = emulator.mutators
        )
    }

    /** An empty ImmutableEmulator with no UTxOs and default test-mainnet parameters. */
    def empty: ImmutableEmulator = ImmutableEmulator(
      state = State(),
      env = UtxoEnv.testMainnet()
    )

    /** Create an ImmutableEmulator with funded addresses.
      *
      * @param addresses
      *   addresses to fund
      * @param initialValue
      *   initial value per address (default: 10,000 ADA)
      */
    def withAddresses(
        addresses: Seq[Address],
        initialValue: Value = Value.ada(10_000L)
    ): ImmutableEmulator = {
        val utxos = EmulatorBase.createInitialUtxos(addresses, initialValue)
        val env = UtxoEnv.testMainnet()
        ImmutableEmulator(
          state = State(utxos = utxos),
          env = env,
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }
}
