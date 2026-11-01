package scalus.testing

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator, STS, State, UtxoEnv}
import scalus.cardano.node.*
import scalus.cardano.node.emulator.{ChainConfig, ForgeError, RollbackError, TxStatus}
import scalus.cardano.node.stream.ChainPoint
import scalus.uplc.builtin.Data

import scala.annotation.targetName
import scala.concurrent.{ExecutionContext, Future}

/** An immutable emulator for Cardano transactions.
  *
  * Unlike [[scalus.cardano.node.Emulator]], this emulator is fully immutable: every state-changing
  * operation (submit, produceBlock, rollback, setSlot) returns a new [[ImmutableEmulator]]
  * instance, leaving the original unchanged.
  *
  * It wraps the same [[EmulatorState]] the mutable emulator runs on, so the chain, the mempool and
  * the presets of [[scalus.cardano.node.emulator.ChainConfig]] behave the same way, spec [SF-11].
  * The validators and mutators stay parameters, spec [SF-12]. Every transition goes through the
  * `EmulatorState` transitions that revalidate the mempool, spec [DE-3].
  *
  * This makes it suitable for use in functional state-threading patterns such as the [[Scenario]]
  * monad, where branching and backtracking require independent state copies.
  *
  * @param emulatorState
  *   the chain, the mempool and the ledger state
  */
final case class ImmutableEmulator private (
    emulatorState: EmulatorState,
    validators: Iterable[STS.Validator],
    mutators: Iterable[STS.Mutator]
) {

    /** The ledger state of the tip. */
    def state: State = emulatorState.ledger

    /** The environment of the tip, at the current slot. */
    def env: UtxoEnv = emulatorState.context.env

    def slotConfig: SlotConfig = emulatorState.context.slotConfig

    def evaluatorMode: EvaluatorMode = emulatorState.context.evaluatorMode

    /** The applied-transaction log, oldest first: the imported log, then the txs of the selected
      * chain, spec [TS-5].
      */
    def appliedTxLog: Vector[AppliedTx] = emulatorState.appliedTxLog

    /** [[appliedTxLog]] by transaction hash. */
    def appliedTxIndex: Map[TransactionHash, AppliedTx] = emulatorState.appliedTxIndex

    /** The datum store, with the bytes each datum arrived in. */
    def binaryDatums: Map[DataHash, KeepRaw[Data]] = emulatorState.binaryDatums

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

    /** How a transition validates a tx: the ledger rules of this emulator. */
    private def validate(
        context: Context,
        ledger: State,
        tx: Transaction
    ): Either[TransactionException, State] =
        STS.Mutator.transit(validators, mutators, context, ledger, tx)

    private def withState(next: EmulatorState): ImmutableEmulator = copy(emulatorState = next)

    /** Submit a transaction, returning the transaction hash and a new emulator with updated state.
      * In the `Instant` preset the tx is in a block at once; in `Manual` it waits in the mempool
      * until [[produceBlock]], spec section 7.
      *
      * The original emulator is not modified.
      */
    def submit(tx: Transaction): Either[SubmitError, (TransactionHash, ImmutableEmulator)] = {
        val (next, result) = emulatorState.submit(tx, identity, validate)
        result.map(hash => (hash, withState(next)))
    }

    /** Forge a block from the mempool, spec section 8.1, and return its point and the new emulator.
      * The original emulator is not modified, spec [SF-14].
      */
    def produceBlock(): (ChainPoint, ImmutableEmulator) = {
        val (next, point) = emulatorState.produceBlock(validate)
        (point, withState(next))
    }

    /** Forge a block of `txs` on `on`, at slot `on.slot + 1`, spec [FG-11], [SF-14]. */
    def produceBlock(
        on: ChainPoint,
        txs: Seq[Transaction]
    ): Either[ForgeError, (ChainPoint, ImmutableEmulator)] =
        produceBlock(on, on.slot + 1, txs)

    /** Forge a block of `txs` at `slot` on `on`, a block of the tree or [[ChainPoint.origin]], then
      * run chain selection, spec section 8.2, [SF-14].
      */
    def produceBlock(
        on: ChainPoint,
        slot: SlotNo,
        txs: Seq[Transaction]
    ): Either[ForgeError, (ChainPoint, ImmutableEmulator)] = {
        val (next, result) = emulatorState.produceBlock(on, slot, txs, validate)
        result.map(point => (point, withState(next)))
    }

    /** Remove the last `n` blocks of the selected chain, and return the tip after chain selection
      * and the new emulator, spec section 11.3, [SF-14].
      */
    def rollback(n: Int): Either[RollbackError, (ChainPoint, ImmutableEmulator)] = {
        val (next, result) = emulatorState.rollback(n, validate)
        result.map(point => (point, withState(next)))
    }

    /** The status of the tx `txHash`, spec [TS-1]. It reads the mempool and the blocks, not the
      * applied-tx log: a tx known only from the log the emulator was built with is `NotFound`.
      */
    def txStatus(txHash: TransactionHash): TxStatus = emulatorState.txStatus(txHash)

    /** Whether the tx is in [[appliedTxLog]]: in a block of the selected chain above the
      * `clearAppliedTxs` cursor, or in the log the emulator was built with. A pending tx, in the
      * `Manual` preset, reads `false`. [[txStatus]] reads the blocks only, so a tx of the imported
      * log is `NotFound` there.
      */
    def hasTx(txHash: TransactionHash): Boolean = appliedTxIndex.contains(txHash)

    /** Look up a previously applied transaction by hash, or `None` if it was never applied. */
    def getTransaction(txHash: TransactionHash): Option[Transaction] =
        appliedTxIndex.get(txHash).map(_.tx)

    /** Look up the full applied-tx record (transaction + slot + spent inputs) by hash. */
    def getAppliedTx(txHash: TransactionHash): Option[AppliedTx] =
        appliedTxIndex.get(txHash)

    /** Resolve a datum by its hash, or `None` if unknown. */
    def getDatum(datumHash: DataHash): Option[Data] = binaryDatums.get(datumHash).map(_.value)

    /** A copy with `datum` in the datum store [[getDatum]] reads, keyed by the hash of its bytes.
      * The immutable form of `EmulatorBase.addDatum`.
      */
    def withDatum(datum: KeepRaw[Data]): ImmutableEmulator =
        withState(
          emulatorState.withSeededDatums(Map(datum.dataHash -> datum)).revalidated(validate)
        )

    /** `withDatum` for a datum without its original bytes: the store keeps its canonical CBOR. */
    def withDatum(data: Data): ImmutableEmulator = withDatum(KeepRaw(data))

    /** Clear the applied-transaction log, keeping ledger state, see `EmulatorBase.clearAppliedTxs`.
      */
    def clearAppliedTxs: ImmutableEmulator = withState(emulatorState.withClearedAppliedTxs)

    /** Advance the slot by the given number of slots. See [[setSlot]] for an epoch boundary. */
    def advanceSlot(n: Long): ImmutableEmulator = setSlot(env.slot + n)

    /** Set the slot to a specific value, then revalidate the mempool, spec [DE-3]. Crossing an
      * epoch boundary forwards moves the donations into the treasury, as the mutable emulator does.
      *
      * @throws IllegalArgumentException
      *   in the `Manual` preset, for a slot below the slot of the tip block, spec [RV-8]
      */
    def setSlot(slot: SlotNo): ImmutableEmulator =
        withState(emulatorState.withSlot(slot).revalidated(validate))

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

    /** Convert to a mutable [[scalus.cardano.node.Emulator]] with the same state: the chain, the
      * mempool, the preset and the whole ledger state.
      */
    def toEmulator: Emulator = EmulatorBase.fromState(emulatorState, validators, mutators)
}

object ImmutableEmulator {

    /** An emulator that starts from `state` at `env.slot`, in the `Instant` preset, spec [CF-4].
      *
      * @param appliedTxLog
      *   Deprecated since 1.4.1: the log has no blocks, so status APIs report `NotFound` for its
      *   txs; use `ImmutableEmulator.fromEmulator`/`toEmulator` or `Emulator.snapshot()`, which
      *   keep the blocks. No compiler warning: the parameter has a default.
      * @param appliedTxIndex
      *   derived from `appliedTxLog`; kept for source compatibility. A non-empty index must equal
      *   the index of `appliedTxLog`.
      */
    def apply(
        state: State,
        env: UtxoEnv,
        slotConfig: SlotConfig = SlotConfig.mainnet,
        evaluatorMode: EvaluatorMode = EvaluatorMode.Validate,
        validators: Iterable[STS.Validator] = Emulator.defaultValidators,
        mutators: Iterable[STS.Mutator] = Emulator.defaultMutators,
        appliedTxLog: Vector[AppliedTx] = Vector.empty,
        appliedTxIndex: Map[TransactionHash, AppliedTx] = Map.empty,
        binaryDatums: Map[DataHash, KeepRaw[Data]] = Map.empty
    ): ImmutableEmulator = {
        require(
          appliedTxIndex.isEmpty || appliedTxIndex == EmulatorBase.indexAppliedTxs(appliedTxLog),
          "appliedTxIndex must be the index of appliedTxLog"
        )
        val context = Context(env = env, slotConfig = slotConfig, evaluatorMode = evaluatorMode)
        new ImmutableEmulator(
          EmulatorState.initial(state, context, binaryDatums, appliedTxLog),
          validators,
          mutators
        )
    }

    /** An emulator on `state`, with these ledger rules, spec [SF-11]. A `Manual` emulator starts
      * from an `EmulatorState` built with [[scalus.cardano.node.emulator.ChainConfig.manual]], or
      * from [[fromEmulator]].
      */
    def fromState(
        state: EmulatorState,
        validators: Iterable[STS.Validator],
        mutators: Iterable[STS.Mutator]
    ): ImmutableEmulator = new ImmutableEmulator(state, validators, mutators)

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
      * Captures a snapshot of the emulator's current state: the chain, the mempool, the preset, and
      * the whole ledger state with the treasury. The state is read once, so a concurrent epoch
      * crossing cannot split the donations between two states.
      */
    def fromEmulator(emulator: EmulatorBase): ImmutableEmulator =
        fromState(EmulatorBase.stateOf(emulator), emulator.validators, emulator.mutators)

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
    ): ImmutableEmulator = withAddresses(addresses, initialValue, ChainConfig.instant)

    /** [[withAddresses]] with a [[scalus.cardano.node.emulator.ChainConfig]], spec [CF-5]. */
    def withAddresses(
        addresses: Seq[Address],
        initialValue: Value,
        chainConfig: ChainConfig
    ): ImmutableEmulator = {
        val utxos = EmulatorBase.createInitialUtxos(addresses, initialValue)
        create(utxos, chainConfig)
    }

    /** Create an ImmutableEmulator from a [[Preconfiguration]].
      *
      * Resolves party names and addresses, then initializes the emulator with the resulting UTxOs.
      */
    def fromPreconfiguration(
        config: Preconfiguration,
        network: Network = Network.Testnet
    ): ImmutableEmulator = fromPreconfiguration(config, network, ChainConfig.instant)

    /** [[fromPreconfiguration]] with a [[scalus.cardano.node.emulator.ChainConfig]], spec [CF-5].
      */
    def fromPreconfiguration(
        config: Preconfiguration,
        network: Network,
        chainConfig: ChainConfig
    ): ImmutableEmulator = {
        val utxos = Preconfiguration.resolveUtxos(config, network)
        create(utxos, chainConfig)
    }

    /** Create an ImmutableEmulator from a JSON string.
      *
      * @see
      *   [[Preconfiguration]] for the JSON format
      */
    def fromJson(
        json: String,
        network: Network = Network.Testnet
    ): ImmutableEmulator = fromJson(json, network, ChainConfig.instant)

    /** [[fromJson]] with a [[scalus.cardano.node.emulator.ChainConfig]], spec [CF-5]. */
    def fromJson(json: String, network: Network, chainConfig: ChainConfig): ImmutableEmulator =
        fromPreconfiguration(Preconfiguration.fromJson(json), network, chainConfig)

    /** An emulator on `utxos` at slot 0 of test mainnet, as the 1.4 factories build it. */
    private def create(utxos: Utxos, chainConfig: ChainConfig): ImmutableEmulator = {
        val context = Context(
          env = UtxoEnv.testMainnet(),
          slotConfig = SlotConfig.mainnet,
          evaluatorMode = EvaluatorMode.Validate
        )
        fromState(
          EmulatorState
              .initial(State(utxos = utxos), context, Map.empty, chainConfig),
          Emulator.defaultValidators,
          Set(PlutusScriptsTransactionMutator)
        )
    }
}
