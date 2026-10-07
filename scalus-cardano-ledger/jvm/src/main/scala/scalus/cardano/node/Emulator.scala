package scalus.cardano.node

import scalus.uplc.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, DefaultMutators, DefaultValidators, STS}
import scalus.cardano.ledger.*

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

/** An in-memory bare-bones node implementation (JVM version with thread-safe state).
  *
  * Allows submitting transaction and querying UTxO state. Runs [[validators]] and [[mutators]]
  * against all submitted transactions. The default validator and mutator lists reflect the Cardano
  * Node UTxO related ledger rules.
  *
  * The rules themselves live in [[EmulatorBase]], shared with the JavaScript emulator; all this
  * class adds is the state cell and the compare-and-set retry that makes concurrent submission
  * safe.
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
) extends EmulatorBase
    with EmulatorJavaApi {

    /** The whole emulator state in one atomically swapped cell, so a transaction and the log entry
      * describing it become visible together.
      */
    private val stateRef = new AtomicReference[EmulatorState](
      EmulatorState.initial(
        utxos = initialUtxos,
        certState = initialCertState,
        context = initialContext,
        datums = initialDatums,
        appliedTxLog = initialAppliedTxLog
      )
    )

    override protected def readState: EmulatorState = stateRef.get()

    override protected def modifyState[A](f: EmulatorState => (EmulatorState, A)): A = {
        @tailrec def attempt(): A = {
            val current = stateRef.get()
            val (next, result) = f(current)
            // A rejected submission returns the state it was handed and writes nothing, so it never
            // contends. A losing compare-and-set re-runs `f` against the state that won, which for
            // a submission means re-validating the transaction rather than applying it over a
            // ledger it was never checked against.
            if (next eq current) || stateRef.compareAndSet(current, next) then result
            else attempt()
        }
        attempt()
    }

    /** Narrowed to `Vector` — the type this class has always returned. */
    override def appliedTxLog: Vector[AppliedTx] = readState.appliedTxLog
}

object Emulator {
    val defaultValidators: Set[STS.Validator] = DefaultValidators.all
    val defaultMutators: Set[STS.Mutator] = DefaultMutators.all

    /** Creates an Emulator with the specified addresses, each funded with 10,000 ADA (like Yaci
      * Devkit).
      */
    def withAddresses(addresses: Seq[Address]): Emulator =
        withAddresses(addresses, Value.ada(10_000L))

    /** Creates an Emulator with the specified addresses, each with the given initial value.
      *
      * @param addresses
      *   The addresses to initialize with funds
      * @param initialValue
      *   Initial value per address
      * @return
      *   An Emulator instance with the addresses funded
      */
    def withAddresses(addresses: Seq[Address], initialValue: Value): Emulator = {
        Emulator(
          initialUtxos = EmulatorBase.createInitialUtxos(addresses, initialValue),
          initialContext = Context.testMainnet(),
          mutators = defaultMutators
        )
    }

    /** Java-friendly overload of [[withAddresses]]. */
    def withAddresses(addresses: java.util.List[Address]): Emulator = {
        import scala.jdk.CollectionConverters.*
        withAddresses(addresses.asScala.toSeq)
    }

    /** Java-friendly overload of [[withAddresses]]. */
    def withAddresses(addresses: java.util.List[Address], initialValue: Value): Emulator = {
        import scala.jdk.CollectionConverters.*
        withAddresses(addresses.asScala.toSeq, initialValue)
    }

    def withState(initState: EmulatorInitialState): Emulator =
        withState(initState, Context.testMainnet())

    def withState(initState: EmulatorInitialState, context: Context): Emulator = {
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
        initialStakeRewards: Map[Credential, Coin]
    ): Emulator =
        withRegisteredStakeCredentials(initialUtxos, initialStakeRewards, Context.testMainnet())

    def withRegisteredStakeCredentials(
        initialUtxos: Utxos,
        initialStakeRewards: Map[Credential, Coin],
        initialContext: Context
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
