package scalus.cardano.node

import scalus.uplc.DebugScript
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, DefaultMutators, DefaultValidators, STS, State}
import scalus.cardano.ledger.*

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

/** An in-memory bare-bones node implementation (JVM version with thread-safe state).
  *
  * Allows submitting transaction and querying UTxO state. Runs [[validators]] and [[mutators]]
  * against all submitted transactions. The default validator and mutator lists reflect the Cardano
  * Node UTxO related ledger rules.
  *
  * @see
  *   [[scalus.cardano.ledger.rules]] for the ledger rules
  */
class Emulator(
    initialUtxos: Utxos = Map.empty,
    initialContext: Context = Context.testMainnet(),
    val validators: Iterable[STS.Validator] = Emulator.defaultValidators,
    val mutators: Iterable[STS.Mutator] = Emulator.defaultMutators,
    initialCertState: CertState = CertState.empty
) extends EmulatorBase {
    private val stateRef =
        new AtomicReference[State](State(initialUtxos, certState = initialCertState))
    private val contextRef = new AtomicReference[Context](initialContext)

    def utxos: Utxos = stateRef.get().utxos
    protected def currentContext: Context = contextRef.get()

    @tailrec
    final def submitSync(
        transaction: Transaction
    ): Either[SubmitError, TransactionHash] = {
        val currentState = stateRef.get()
        val ctx = contextRef.get()

        processTransaction(ctx, currentState, transaction) match {
            case Right(newState) =>
                if stateRef.compareAndSet(currentState, newState) then Right(transaction.id)
                else submitSync(transaction)
            case Left(t: TransactionException) =>
                Left(SubmitError.fromException(t))
        }
    }

    @tailrec
    final def submitSync(
        transaction: Transaction,
        debugScripts: Map[ScriptHash, DebugScript]
    ): Either[SubmitError, TransactionHash] = {
        val currentState = stateRef.get()
        val ctx = contextRef.get()
        val ctxWithDebug = ctx.copy(debugScripts = debugScripts)

        processTransaction(ctxWithDebug, currentState, transaction) match {
            case Right(newState) =>
                if stateRef.compareAndSet(currentState, newState) then Right(transaction.id)
                else submitSync(transaction, debugScripts)
            case Left(t: TransactionException) =>
                Left(SubmitError.fromException(t))
        }
    }

    @tailrec
    final def setSlot(slot: SlotNo): Unit = {
        val ctx = contextRef.get()
        val newContext = Context(
          fee = ctx.fee,
          env = ctx.env.copy(slot = slot),
          slotConfig = ctx.slotConfig
        )
        if !contextRef.compareAndSet(ctx, newContext) then setSlot(slot)
    }

    def snapshot(): Emulator = Emulator(
      initialUtxos = this.utxos,
      initialContext = this.contextRef.get(),
      validators = this.validators,
      mutators = this.mutators,
      initialCertState = this.stateRef.get().certState
    )
}

object Emulator {
    val defaultValidators: Set[STS.Validator] = DefaultValidators.all
    val defaultMutators: Set[STS.Mutator] = DefaultMutators.all

    /** Creates an Emulator with the specified addresses, each with the given initial value.
      *
      * @param addresses
      *   The addresses to initialize with funds
      * @param initialValue
      *   Initial value per address (default: 10,000 ADA like Yaci Devkit)
      * @return
      *   An Emulator instance with the addresses funded
      */
    def withAddresses(
        addresses: Seq[Address],
        initialValue: Value = Value.ada(10_000L)
    ): Emulator = {
        Emulator(
          initialUtxos = EmulatorBase.createInitialUtxos(addresses, initialValue),
          initialContext = Context.testMainnet(),
          mutators = defaultMutators
        )
    }

    /** Creates an Emulator with pre-registered stake credentials.
      *
      * Useful for the zero-withdrawal trick: the staking address must be registered before a
      * zero-value withdrawal can trigger a script reward validator, without needing a registration
      * transaction.
      *
      * @param initialUtxos
      *   Initial UTxO set
      * @param stakeCredentials
      *   Credentials to pre-register (added to deposits and rewards maps)
      * @param initialContext
      *   Context (default: testMainnet)
      * @return
      *   An Emulator with the credentials already registered
      */
    def withRegisteredStakeCredentials(
        initialUtxos: Utxos,
        stakeCredentials: Seq[Credential],
        initialContext: Context = Context.testMainnet()
    ): Emulator = {
        Emulator(
          initialUtxos = initialUtxos,
          initialContext = initialContext,
          initialCertState = EmulatorBase.certStateWithRegisteredCredentials(
            stakeCredentials,
            initialContext
          )
        )
    }
}
