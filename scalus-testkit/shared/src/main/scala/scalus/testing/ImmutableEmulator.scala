package scalus.testing

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator, STS, State, UtxoEnv}
import scalus.cardano.node.*
import scalus.uplc.builtin.Data

import scala.annotation.targetName
import scala.concurrent.{ExecutionContext, Future}

/** An immutable emulator for Cardano transactions.
  *
  * Unlike [[scalus.cardano.node.Emulator]], this emulator is fully immutable: every state-changing
  * operation (submit, advanceSlot) returns a new [[ImmutableEmulator]] instance, leaving the
  * original unchanged.
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
    mutators: Iterable[STS.Mutator] = Emulator.defaultMutators,
    /** Ordered log of applied transactions (oldest first); maintained by [[submit]]. */
    appliedTxLog: Vector[AppliedTx] = Vector.empty,
    /** By-hash index of [[appliedTxLog]] for O(1) lookup. Invariant: the by-hash view of
      * `appliedTxLog`; both advance together in [[submit]]. Build it via
      * `EmulatorBase.indexAppliedTxs` rather than setting it independently.
      */
    appliedTxIndex: Map[TransactionHash, AppliedTx] = Map.empty,
    /** Datum resolution cache (inline + witness datums of applied transactions), with the bytes
      * each datum arrived in. Maintained by [[submit]] so [[getDatum]] and [[asReader]] can resolve
      * datum-hash references.
      */
    binaryDatums: Map[DataHash, KeepRaw[Data]] = Map.empty
) {

    /** [[binaryDatums]], decoded. */
    def datums: Map[DataHash, Data] = binaryDatums.view.mapValues(_.value).toMap

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
                val applied = AppliedTx(tx, env.slot, EmulatorBase.resolveSpent(state.utxos, tx))
                val next = copy(
                  state = newState,
                  appliedTxLog = appliedTxLog :+ applied,
                  appliedTxIndex = appliedTxIndex + (applied.txHash -> applied),
                  binaryDatums = binaryDatums ++ EmulatorBase.binaryDatumsOf(tx)
                )
                Right((tx.id, next))
            case Left(e: TransactionException) =>
                Left(SubmitError.fromException(e))
        }
    }

    /** True if the given transaction has been applied to this emulator. */
    def hasTx(txHash: TransactionHash): Boolean = appliedTxIndex.contains(txHash)

    /** Look up a previously applied transaction by hash, or `None` if it was never applied. */
    def getTransaction(txHash: TransactionHash): Option[Transaction] =
        appliedTxIndex.get(txHash).map(_.tx)

    /** Look up the full applied-tx record (transaction + slot + spent inputs) by hash. */
    def getAppliedTx(txHash: TransactionHash): Option[AppliedTx] =
        appliedTxIndex.get(txHash)

    /** Resolve a datum by its hash, or `None` if unknown. */
    def getDatum(datumHash: DataHash): Option[Data] = binaryDatums.get(datumHash).map(_.value)

    /** Clear the applied-transaction bookkeeping (log + index), keeping ledger state. */
    def clearAppliedTxs: ImmutableEmulator =
        copy(appliedTxLog = Vector.empty, appliedTxIndex = Map.empty)

    /** Advance the slot by the given number of slots. See [[setSlot]] for an epoch boundary. */
    def advanceSlot(n: Long): ImmutableEmulator = setSlot(env.slot + n)

    /** Set the slot to a specific value. Crossing an epoch boundary forwards moves the donations
      * into the treasury, as the mutable emulator does.
      */
    def setSlot(slot: SlotNo): ImmutableEmulator = {
        val (newState, newEnv) = EmulatorBase.atSlot(state, env, slotConfig, slot)
        copy(state = newState, env = newEnv)
    }

    /** Find UTxOs matching the given query. */
    def findUtxos(query: UtxoQuery): Either[UtxoQueryError, Utxos] =
        Right(EmulatorBase.evalQuery(utxos, query))

    /** Create a read-only [[scalus.cardano.node.BlockchainReader]] snapshot.
      *
      * This provides read-only access to the emulator's state without submit capability. Uses
      * `ExecutionContext.global` to avoid deadlocks when callers use `Await.result` on the returned
      * futures.
      */
    def asReader: BlockchainReader = new BlockchainReader {
        override def executionContext: ExecutionContext = ExecutionContext.global
        override def cardanoInfo: CardanoInfo = ImmutableEmulator.this.cardanoInfo
        override def fetchLatestParams: Future[ProtocolParams] =
            Future.successful(env.params)
        override def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] =
            Future.successful(ImmutableEmulator.this.findUtxos(query))
        override def currentSlot: Future[SlotNo] =
            Future.successful(env.slot)
        def getDatum(datumHash: DataHash): Future[Option[Data]] =
            Future.successful(ImmutableEmulator.this.getDatum(datumHash))
    }

    /** Convert to a mutable [[scalus.cardano.node.Emulator]], keeping the whole ledger state and
      * the treasury.
      */
    def toEmulator: Emulator = {
        val context = Context(env = env, slotConfig = slotConfig, evaluatorMode = evaluatorMode)
        EmulatorBase.fromState(
          EmulatorState.initial(state, context, binaryDatums, appliedTxLog),
          validators,
          mutators
        )
    }
}

object ImmutableEmulator {

    /** The 1.3 parameter list: the datums decoded, without their original bytes. Each datum keeps
      * the bytes of its canonical encoding.
      */
    @deprecated("pass binaryDatums: Map[DataHash, KeepRaw[Data]] instead", "1.3.0")
    @targetName("applyWithDecodedDatums")
    def apply(
        state: State,
        env: UtxoEnv,
        slotConfig: SlotConfig,
        evaluatorMode: EvaluatorMode,
        validators: Iterable[STS.Validator],
        mutators: Iterable[STS.Mutator],
        appliedTxLog: Vector[AppliedTx],
        appliedTxIndex: Map[TransactionHash, AppliedTx],
        datums: Map[DataHash, Data]
    ): ImmutableEmulator = ImmutableEmulator(
      state,
      env,
      slotConfig,
      evaluatorMode,
      validators,
      mutators,
      appliedTxLog,
      appliedTxIndex,
      datums.view.mapValues(KeepRaw(_)).toMap
    )

    /** Create an ImmutableEmulator from a mutable [[scalus.cardano.node.EmulatorBase]].
      *
      * Captures a snapshot of the emulator's current state, the whole ledger state and the treasury
      * included. The state is read once, so a concurrent epoch crossing cannot split the donations
      * between two states.
      */
    def fromEmulator(emulator: EmulatorBase): ImmutableEmulator = {
        val snapshot = EmulatorBase.stateOf(emulator)
        val context = snapshot.context
        ImmutableEmulator(
          state = snapshot.ledger,
          // ledger rules read cert state from `state.certState`, not `env.certState`; mirror the
          // mutable Emulator and keep this empty (the real cert state lives in `state`)
          env = context.env.copy(certState = CertState.empty),
          slotConfig = context.slotConfig,
          evaluatorMode = context.evaluatorMode,
          validators = emulator.validators,
          mutators = emulator.mutators,
          appliedTxLog = snapshot.appliedTxLog,
          appliedTxIndex = snapshot.appliedTxIndex,
          binaryDatums = snapshot.binaryDatums
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

    /** Create an ImmutableEmulator from a [[Preconfiguration]].
      *
      * Resolves party names and addresses, then initializes the emulator with the resulting UTxOs.
      */
    def fromPreconfiguration(
        config: Preconfiguration,
        network: Network = Network.Testnet
    ): ImmutableEmulator = {
        val utxos = Preconfiguration.resolveUtxos(config, network)
        ImmutableEmulator(
          state = State(utxos = utxos),
          env = UtxoEnv.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }

    /** Create an ImmutableEmulator from a JSON string.
      *
      * @see
      *   [[Preconfiguration]] for the JSON format
      */
    def fromJson(
        json: String,
        network: Network = Network.Testnet
    ): ImmutableEmulator = fromPreconfiguration(Preconfiguration.fromJson(json), network)
}
