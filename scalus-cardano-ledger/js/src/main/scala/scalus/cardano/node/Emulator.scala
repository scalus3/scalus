package scalus.cardano.node

import scalus.uplc.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, DefaultMutators, DefaultValidators, STS}
import scalus.cardano.ledger.*

/** An in-memory bare-bones node implementation (JS version with single-threaded state).
  *
  * Allows submitting transaction and querying UTxO state. Runs [[validators]] and [[mutators]]
  * against all submitted transactions. The default validator and mutator lists reflect the Cardano
  * Node UTxO related ledger rules.
  *
  * The rules themselves live in [[EmulatorBase]], shared with the JVM emulator, so the two cannot
  * drift apart; all this class adds is the state cell — a plain `var`, since JavaScript is
  * single-threaded.
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
    initialDatums: Map[DataHash, Data] = Map.empty,
    initialAppliedTxLog: Vector[AppliedTx] = Vector.empty
) extends EmulatorBase {
    // JavaScript is single-threaded, so a simple var is safe
    private var state: EmulatorState = EmulatorState.initial(
      utxos = initialUtxos,
      certState = initialCertState,
      context = initialContext,
      datums = initialDatums,
      appliedTxLog = initialAppliedTxLog
    )

    override protected def readState: EmulatorState = state

    override protected def modifyState[A](f: EmulatorState => (EmulatorState, A)): A = {
        val (next, result) = f(state)
        state = next
        result
    }

    /** Narrowed to `Vector` — the type this class has always returned. */
    override def appliedTxLog: Vector[AppliedTx] = state.appliedTxLog

    /** Public here, unlike the `protected` declaration in [[EmulatorBase]]: [[JEmulator]] reads the
      * slot config and the env off it while translating between the ledger types and JavaScript.
      */
    override def currentContext: Context = state.context
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
