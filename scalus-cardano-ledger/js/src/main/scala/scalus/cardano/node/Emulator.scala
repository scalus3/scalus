package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator, STS, State}
import scalus.cardano.ledger.*

/** An in-memory bare-bones node implementation (JS version with single-threaded state).
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
    val mutators: Iterable[STS.Mutator] = Emulator.defaultMutators
) extends EmulatorBase {
    // JavaScript is single-threaded, so simple vars are safe
    private var state: State = State(initialUtxos)
    private var context: Context = initialContext

    def utxos: Utxos = state.utxos
    protected def currentContext: Context = context

    protected def submitSync(transaction: Transaction): Either[SubmitError, TransactionHash] = {
        processTransaction(context, state, transaction) match {
            case Right(newState) =>
                state = newState
                Right(transaction.id)
            case Left(t: TransactionException) =>
                Left(SubmitError.fromException(t))
        }
    }

    def setSlot(slot: SlotNo): Unit = {
        context = Context(
          fee = context.fee,
          env = context.env.copy(slot = slot),
          slotConfig = context.slotConfig
        )
    }

    def snapshot(): Emulator = Emulator(
      initialUtxos = this.utxos,
      initialContext = this.context,
      validators = this.validators,
      mutators = this.mutators
    )
}

object Emulator {
    // JS cannot use CardanoMutator (requires JVM-only reflection/JAR scanning)
    // Default to PlutusScriptsTransactionMutator which handles script evaluation
    val defaultValidators: Set[STS.Validator] = Set.empty
    val defaultMutators: Set[STS.Mutator] = Set(PlutusScriptsTransactionMutator)

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
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }
}
