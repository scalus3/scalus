package scalus.cardano.node

import scalus.uplc.DebugScript
import scalus.uplc.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, DefaultMutators, DefaultValidators, STS, State}
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
    val mutators: Iterable[STS.Mutator] = Emulator.defaultMutators,
    initialCertState: CertState = CertState.empty,
    initialDatums: Map[DataHash, Data] = Map.empty
) extends EmulatorBase {
    // JavaScript is single-threaded, so simple vars are safe
    private var state: State = State(initialUtxos, certState = initialCertState)
    private var context: Context = initialContext
    private var _datums: Map[DataHash, Data] = initialDatums
    private var _appliedTxs: Set[TransactionHash] = Set.empty

    def utxos: Utxos = state.utxos
    def certState: CertState = state.certState
    def currentContext: Context = context
    def datums: Map[DataHash, Data] = _datums
    def appliedTxs: Set[TransactionHash] = _appliedTxs

    private def recordApplied(tx: Transaction): Unit = {
        _appliedTxs = _appliedTxs + tx.id
        _datums = _datums ++ EmulatorBase.extractDatums(tx)
    }

    def submitSync(transaction: Transaction): Either[SubmitError, TransactionHash] = {
        processTransaction(context, state, transaction) match {
            case Right(newState) =>
                state = newState
                recordApplied(transaction)
                Right(transaction.id)
            case Left(t: TransactionException) =>
                Left(SubmitError.fromException(t))
        }
    }

    def submitSync(
        transaction: Transaction,
        debugScripts: Map[ScriptHash, DebugScript]
    ): Either[SubmitError, TransactionHash] = {
        val ctxWithDebug = context.copy(debugScripts = debugScripts)
        processTransaction(ctxWithDebug, state, transaction) match {
            case Right(newState) =>
                state = newState
                recordApplied(transaction)
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
      mutators = this.mutators,
      initialCertState = this.state.certState,
      initialDatums = this._datums
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

    def withState(
        initState: EmulatorInitialState,
        context: Context = Context.testMainnet()
    ): Emulator = {
        val (certState, datums) = EmulatorBase.buildInitialState(initState, context)
        Emulator(
          initialUtxos = initState.utxos,
          initialContext = context,
          initialCertState = certState,
          initialDatums = datums
        )
    }

    /** Creates an Emulator with pre-registered stake credentials and specified reward balances.
      *
      * Useful for the zero-withdrawal trick: the staking address must be registered before a
      * zero-value withdrawal can trigger a script reward validator, without needing a registration
      * transaction.
      *
      * @param initialUtxos
      *   Initial UTxO set
      * @param initialStakeRewards
      *   Map from stake credential to its initial reward balance
      * @param initialContext
      *   Context (default: testMainnet)
      * @return
      *   An Emulator with the credentials already registered
      */
    def withRegisteredStakeCredentials(
        initialUtxos: Utxos,
        initialStakeRewards: Map[Credential, Coin],
        initialContext: Context = Context.testMainnet()
    ): Emulator = {
        Emulator(
          initialUtxos = initialUtxos,
          initialContext = initialContext,
          initialCertState = EmulatorBase.certStateWithRegisteredCredentials(
            initialStakeRewards,
            initialContext
          )
        )
    }
}
