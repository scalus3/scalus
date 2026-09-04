package scalus.cardano.node

import scalus.uplc.DebugScript
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, STS, State}
import scalus.cardano.ledger.*

import scala.concurrent.{ExecutionContext, Future}

/** Base trait for Emulator implementations containing the whole emulator state machine.
  *
  * Everything an emulator does to its state lives here, expressed against a single abstract cell
  * holding one [[EmulatorState]]. The platform implementations supply only that cell: the JVM one
  * an `AtomicReference` with a compare-and-set retry, the JavaScript one a plain `var`, since
  * JavaScript is single-threaded. Neither carries a copy of the rules, so the two platforms cannot
  * drift apart.
  */
trait EmulatorBase extends BlockchainProvider {

    /** Emulator uses parasitic EC since all operations are synchronous. */
    override def executionContext: ExecutionContext = ExecutionContext.parasitic

    def validators: Iterable[STS.Validator]
    def mutators: Iterable[STS.Mutator]

    /** Read this emulator's state. A single read of a single immutable value, so what comes back is
      * always a coherent state and never a mixture of two.
      *
      * Implement this together with [[modifyState]] and every operation below comes for free. The
      * default exists only so that an implementation written against 1.1.x, which supplied
      * `submitSync`, `setSlot`, `snapshot` and `clearAppliedTxs` itself, still compiles and runs:
      * its own overrides never reach here. Anything that does reach here has no state to read.
      */
    protected def readState: EmulatorState =
        throw UnsupportedOperationException(
          s"${getClass.getName} does not implement EmulatorBase.readState. Implement readState " +
              "and modifyState over a cell holding an EmulatorState, or override the operation " +
              "you called."
        )

    /** Replace this emulator's state with `f`'s result and return the value `f` computed alongside
      * it — the one primitive every mutation below is written in terms of.
      *
      * `f` must be pure: an implementation may run it more than once. The JVM one does exactly that
      * when its compare-and-set loses a race, which is how a submission that read a state another
      * thread has since replaced is re-validated against the state that won instead of being
      * applied on top of it. Returning the state it was given (by reference) means "no change", and
      * writes nothing.
      */
    protected def modifyState[A](f: EmulatorState => (EmulatorState, A)): A =
        throw UnsupportedOperationException(
          s"${getClass.getName} does not implement EmulatorBase.modifyState. Implement readState " +
              "and modifyState over a cell holding an EmulatorState, or override the operation " +
              "you called."
        )

    /** [[modifyState]] for a mutation with nothing to report back. */
    protected final def updateState(f: EmulatorState => EmulatorState): Unit =
        modifyState(state => (f(state), ()))

    def utxos: Utxos = readState.ledger.utxos
    def certState: CertState = readState.ledger.certState
    protected def currentContext: Context = readState.context
    def datums: Map[DataHash, Data] = readState.datums

    /** Index of applied transactions by hash, for O(1) lookup. */
    def appliedTxIndex: Map[TransactionHash, AppliedTx] = readState.appliedTxIndex

    /** Ordered log of transactions applied to this emulator, in submission order (oldest first).
      *
      * Unlike [[appliedTxs]] (hashes only), each entry retains the full transaction together with
      * the slot it was applied at and the inputs it consumed (resolved against the pre-application
      * UTxO set). This lets callers reconstruct chain history — e.g. walk the continuing outputs of
      * an asset — without keeping their own submitted-transaction bookkeeping.
      *
      * Ordering reflects submission order. Under concurrent submission the log entry and the ledger
      * state transition it describes are published together, so a reader never sees a UTxO set that
      * has moved past the log.
      */
    def appliedTxLog: Seq[AppliedTx] = readState.appliedTxLog

    /** Hashes of all applied transactions. Held as a materialized set (derived from
      * [[appliedTxLog]] by [[EmulatorState]], so it cannot drift) rather than a per-call key-set
      * view, so access is a plain O(1) field read.
      */
    def appliedTxs: Set[TransactionHash] = readState.appliedTxs

    def submitSync(transaction: Transaction): Either[SubmitError, TransactionHash] =
        submitWith(transaction, identity)

    def setSlot(slot: SlotNo): Unit = updateState(_.withSlot(slot))

    /** An independent emulator starting from this one's state, taken as a single read — the copy
      * and the original share no mutable cell and evolve separately from here on.
      */
    def snapshot(): Emulator = {
        val state = readState
        Emulator(
          initialUtxos = state.ledger.utxos,
          initialContext = state.context,
          validators = validators,
          mutators = mutators,
          initialCertState = state.ledger.certState,
          initialDatums = state.datums,
          initialAppliedTxLog = state.appliedTxLog
        )
    }

    /** Clear the applied-transaction bookkeeping ([[appliedTxLog]] and [[appliedTxIndex]]), leaving
      * the ledger state (`utxos`, `certState`, `datums`) untouched.
      */
    def clearAppliedTxs(): Unit = updateState(_.withClearedAppliedTxs)

    /** Validate `transaction` against the current state under a context derived by
      * `validationContext`, and apply it if it passes.
      *
      * The derived context is used for validation only; the state keeps the context it had, so a
      * per-submission concern such as debug scripts cannot leak into later submissions.
      */
    private def submitWith(
        transaction: Transaction,
        validationContext: Context => Context
    ): Either[SubmitError, TransactionHash] = modifyState { state =>
        processTransaction(validationContext(state.context), state.ledger, transaction) match
            case Right(newLedger) =>
                val applied = AppliedTx(
                  transaction,
                  state.context.env.slot,
                  EmulatorBase.resolveSpent(state.ledger.utxos, transaction)
                )
                (state.withApplied(newLedger, applied), Right(transaction.id))
            case Left(t: TransactionException) =>
                (state, Left(SubmitError.fromException(t)))
    }

    /** Evaluator mode the emulator runs Plutus scripts in (e.g. `Validate` vs
      * `EvaluateAndComputeCost`). Exposed so snapshots such as `ImmutableEmulator.fromEmulator` can
      * preserve it rather than silently reverting to the `Context` default.
      */
    def evaluatorMode: EvaluatorMode = currentContext.evaluatorMode

    def tick(n: Long): Unit = setSlot(currentContext.env.slot + n)

    def hasTx(txHash: TransactionHash): Boolean = appliedTxIndex.contains(txHash)

    /** Look up a previously applied transaction by hash, or `None` if it was never applied. */
    def getTransaction(txHash: TransactionHash): Option[Transaction] =
        appliedTxIndex.get(txHash).map(_.tx)

    /** Look up the full applied-tx record (transaction + slot + spent inputs) by hash. */
    def getAppliedTx(txHash: TransactionHash): Option[AppliedTx] =
        appliedTxIndex.get(txHash)

    def getDelegation(credential: Credential): DelegationInfo = {
        val st = certState.dstate
        DelegationInfo(
          poolId = st.stakePools.get(credential),
          rewards = st.rewards.getOrElse(credential, Coin.zero)
        )
    }

    /** Live stake per registered stake credential: the lovelace sitting at addresses delegating to
      * it, plus its reward balance, and the pool it delegates to.
      *
      * This is the emulator's answer to "who controls how much stake", the query a governance or
      * delegation test needs. It is not a reward calculation: nothing is paid out.
      *
      * Pointer addresses are ignored — they are deprecated and carry no stake here.
      *
      * Reads the state cell once, the way [[snapshot]] does. Two reads would let a submission land
      * between them on the JVM, producing a row whose `stake` came from the post-transaction UTxO
      * set and whose `rewards` came from the pre-transaction certificate state.
      */
    def stakeDistribution: Seq[StakeDistributionEntry] = {
        val state = readState
        val dstate = state.ledger.certState.dstate
        val stakeByCredential = StakeDistribution.aggregateUtxoStake(state.ledger.utxos)
        val credentials = dstate.rewards.keySet ++ stakeByCredential.keySet
        credentials.toSeq.map { credential =>
            StakeDistributionEntry(
              credential = credential,
              pool = dstate.stakePools.get(credential),
              stake = stakeByCredential.getOrElse(credential, Coin.zero),
              rewards = dstate.rewards.getOrElse(credential, Coin.zero)
            )
        }
    }

    def getDatum(datumHash: DataHash): Future[Option[Data]] =
        Future.successful(datums.get(datumHash))

    override def cardanoInfo: CardanoInfo = {
        val ctx = currentContext
        CardanoInfo(ctx.env.params, ctx.env.network, ctx.slotConfig)
    }

    def currentSlot: Future[SlotNo] = Future.successful(currentSlotSync)

    /** The current slot, without the `Future` wrapper.
      *
      * An emulator's slot is in-memory state, so the effectful accessor above is a formality
      * imposed by the `BlockchainReader` interface. Streaming needs the value synchronously while
      * building a chain point, where handing back an already-completed `Future` and unwrapping it
      * would be pure ceremony.
      */
    def currentSlotSync: SlotNo = currentContext.env.slot

    def fetchLatestParams: Future[ProtocolParams] = {
        val params = currentContext.env.params
        Future.successful(params)
    }

    def submit(transaction: Transaction): Future[Either[SubmitError, TransactionHash]] =
        Future.successful(submitSync(transaction))

    /** Submit a transaction with debug scripts for diagnostic replay.
      *
      * When a release script fails with empty logs, the evaluator replays it using the debug script
      * to produce diagnostic output.
      *
      * @param transaction
      *   the transaction to submit
      * @param debugScripts
      *   map from release script hash to debug script for diagnostic replay
      */
    def submit(
        transaction: Transaction,
        debugScripts: Map[ScriptHash, DebugScript]
    ): Future[Either[SubmitError, TransactionHash]] =
        Future.successful(submitSync(transaction, debugScripts))

    /** Synchronously submit a transaction with debug scripts for diagnostic replay. */
    def submitSync(
        transaction: Transaction,
        debugScripts: Map[ScriptHash, DebugScript]
    ): Either[SubmitError, TransactionHash] =
        submitWith(transaction, _.copy(debugScripts = debugScripts))

    /** Adds a UTxO to the ledger directly, bypassing transaction validation. Overwrites any UTxO
      * already sitting at the same input.
      *
      * An inline datum on `output` joins the datum store, so [[getDatum]] answers for a script UTxO
      * seeded this way exactly as it does for one produced by a transaction.
      */
    def addUtxo(input: TransactionInput, output: TransactionOutput): Unit =
        updateState(_.withUtxo(input, output))

    /** Removes a UTxO from the ledger directly, bypassing transaction validation. A no-op if no
      * UTxO sits at that input.
      *
      * A datum the removed UTxO carried inline stays in the datum store: a node keeps answering for
      * a datum it has seen after the output holding it is spent, and so does this.
      */
    def removeUtxo(input: TransactionInput): Unit =
        updateState(_.withoutUtxo(input))

    /** [[findUtxos]] without the `Future`. The emulator's state is in memory and its query
      * evaluation is pure, so the effect wrapper is an interface formality — JavaScript and Java
      * callers both want the value.
      */
    def findUtxosSync(query: UtxoQuery): Utxos = EmulatorBase.evalQuery(utxos, query)

    override def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] =
        Future.successful(Right(findUtxosSync(query)))

    /** Whether this emulator has applied the transaction.
      *
      * Authoritative, unlike the inherited default, which infers status from the UTxOs a
      * transaction produced: `findUtxos` here answers `Right(empty)` for a transaction it has never
      * seen, and an emulator that has applied a transaction whose outputs are all since spent
      * produces none either. The applied-transaction index knows the answer outright.
      */
    override def checkTransaction(txHash: TransactionHash): Future[TransactionStatus] =
        Future.successful(
          if hasTx(txHash) then TransactionStatus.Confirmed else TransactionStatus.NotFound
        )

    protected def processTransaction(
        context: Context,
        state: State,
        transaction: Transaction
    ): Either[TransactionException, State] = {
        STS.Mutator.transit(validators, mutators, context, state, transaction)
    }
}

/** A record of a transaction applied to the emulated ledger.
  *
  * Captures the parts of chain history that the live UTxO set no longer holds after application:
  * the full transaction, the slot it was applied at, and the resolved inputs it consumed (which are
  * removed from the UTxO set once the transaction is applied).
  *
  * @param tx
  *   the applied transaction
  * @param slot
  *   the emulator slot at the time of application
  * @param spent
  *   the UTxOs consumed by `tx`, resolved against the pre-application UTxO set
  */
case class AppliedTx(tx: Transaction, slot: SlotNo, spent: Utxos) {
    def txHash: TransactionHash = tx.id
}

/** The entire mutable state of an [[EmulatorBase]], as one immutable value.
  *
  * Aggregating it is what lets one state machine serve both platforms, and it is also what makes a
  * transaction land atomically: the ledger state and the bookkeeping that describes it are swapped
  * together, so no reader — and on the JVM no other submitting thread — can observe a UTxO set that
  * has moved on while the applied-transaction log has not caught up.
  *
  * `appliedTxIndex` and `appliedTxs` are derived from `appliedTxLog`, kept for O(1) lookup. Nothing
  * outside this class writes them: the constructor is private, so [[EmulatorState.initial]] and the
  * transitions below are the only way to build one, and no caller can hand back a state whose log
  * and caches disagree (`EmulatorStateInvariantTest` pins it).
  *
  * The log records one entry per *application*, while the two derived views are keyed by hash, so
  * they are not always the same size. Applying one transaction twice needs the direct ledger edits
  * ([[EmulatorBase.addUtxo]]) to put its inputs back — the ledger rules reject the second
  * submission otherwise — but it is reachable, and then the log holds both applications while the
  * index holds the later one and `appliedTxs` holds the hash once. Every hash in the log is in both
  * derived views and vice versa; only the multiplicity differs.
  *
  * @param ledger
  *   the ledger state proper — the UTxO set and the certificate state
  * @param context
  *   the validation context, carrying the current slot, protocol parameters and evaluator mode
  * @param datums
  *   every datum this emulator has seen, by hash
  * @param appliedTxLog
  *   applied transactions in application order, oldest first — one entry per application
  * @param appliedTxIndex
  *   derived: `appliedTxLog` keyed by transaction hash, holding the latest application of each
  * @param appliedTxs
  *   derived: the distinct hashes in `appliedTxLog`
  */
case class EmulatorState private (
    ledger: State,
    context: Context,
    datums: Map[DataHash, Data],
    appliedTxLog: Vector[AppliedTx],
    appliedTxIndex: Map[TransactionHash, AppliedTx],
    appliedTxs: Set[TransactionHash]
) {

    /** The state after `applied` moved the ledger to `newLedger`: the log gains an entry, both
      * derived views follow it, and the transaction's datums join the store.
      *
      * The log entry is appended unconditionally, so re-applying a transaction the emulator has
      * already applied — only reachable by putting its inputs back with [[EmulatorBase.addUtxo]] —
      * records both applications, while the index moves to the later one and `appliedTxs` is
      * unchanged.
      */
    def withApplied(newLedger: State, applied: AppliedTx): EmulatorState = copy(
      ledger = newLedger,
      datums = datums ++ EmulatorBase.extractDatums(applied.tx),
      appliedTxLog = appliedTxLog :+ applied,
      appliedTxIndex = appliedTxIndex + (applied.txHash -> applied),
      appliedTxs = appliedTxs + applied.txHash
    )

    /** The state with the applied-transaction bookkeeping dropped. The ledger, the context and the
      * datum store are deliberately kept: see [[EmulatorBase.clearAppliedTxs]].
      */
    def withClearedAppliedTxs: EmulatorState = copy(
      appliedTxLog = Vector.empty,
      appliedTxIndex = Map.empty,
      appliedTxs = Set.empty
    )

    /** The state at a different slot. A `copy` rather than a fresh `Context`, which would drop the
      * evaluator mode and any debug scripts.
      */
    def withSlot(slot: SlotNo): EmulatorState =
        copy(context = context.copy(env = context.env.copy(slot = slot)))

    /** The state with one UTxO added, or replaced if one already sits at `input`, for the direct
      * ledger edits that bypass validation.
      *
      * An inline datum on `output` joins the datum store, which is how a script UTxO seeded with
      * [[EmulatorBase.addUtxo]] answers [[EmulatorBase.getDatum]] — a real node and Blockfrost both
      * index the datums held in the UTxO set, not only those a transaction carried in its witness
      * set. Only `output` is indexed, never the whole set: every other UTxO was indexed when it
      * arrived, so seeding N UTxOs costs O(N) rather than O(N²). Nothing is removed: the store is
      * everything the emulator has seen.
      */
    def withUtxo(input: TransactionInput, output: TransactionOutput): EmulatorState = copy(
      ledger = ledger.copy(utxos = ledger.utxos + (input -> output)),
      datums = EmulatorBase.inlineDatumOf(output).fold(datums)(datums + _)
    )

    /** The state with the UTxO at `input` dropped, for the direct ledger edits that bypass
      * validation. A no-op if none sits there.
      *
      * The datum store is deliberately left alone: a node keeps answering for a datum it has seen
      * after the output holding it is spent, and so does this.
      */
    def withoutUtxo(input: TransactionInput): EmulatorState =
        copy(ledger = ledger.copy(utxos = ledger.utxos - input))
}

object EmulatorState {

    /** The state an emulator starts from — the only public way to build one.
      *
      * Both derived views and the datum store are computed here rather than passed in, so an
      * emulator restored from a log — a snapshot, say — cannot disagree with the one the log came
      * from. The datum store also picks up the inline datums of the seeded `utxos`, so seeding a
      * script UTxO with its datum inline is enough for `getDatum` to answer.
      */
    def initial(
        utxos: Utxos,
        certState: CertState,
        context: Context,
        datums: Map[DataHash, Data],
        appliedTxLog: Vector[AppliedTx]
    ): EmulatorState = new EmulatorState(
      ledger = State(utxos, certState = certState),
      context = context,
      datums = appliedTxLog.foldLeft(datums ++ EmulatorBase.extractInlineDatums(utxos))((acc, a) =>
          acc ++ EmulatorBase.extractDatums(a.tx)
      ),
      appliedTxLog = appliedTxLog,
      appliedTxIndex = EmulatorBase.indexAppliedTxs(appliedTxLog),
      appliedTxs = appliedTxLog.map(_.txHash).toSet
    )
}

case class DelegationInfo(poolId: Option[PoolKeyHash], rewards: Coin)

/** One row of [[EmulatorBase.stakeDistribution]]. */
case class StakeDistributionEntry(
    credential: Credential,
    pool: Option[PoolKeyHash],
    stake: Coin,
    rewards: Coin
)

case class EmulatorStakeRegistration(
    credential: Credential,
    rewards: Coin = Coin.zero,
    delegatedTo: Option[PoolKeyHash] = None
)

object EmulatorStakeRegistration {
    // Java-friendly factories (no default args, no Option).
    def of(credential: Credential): EmulatorStakeRegistration =
        EmulatorStakeRegistration(credential)
    def of(credential: Credential, rewards: Coin): EmulatorStakeRegistration =
        EmulatorStakeRegistration(credential, rewards)
    def of(
        credential: Credential,
        rewards: Coin,
        delegatedTo: PoolKeyHash
    ): EmulatorStakeRegistration =
        EmulatorStakeRegistration(credential, rewards, Some(delegatedTo))
}

case class EmulatorPoolRegistration(params: Certificate.PoolRegistration)

case class EmulatorDRepRegistration(
    credential: Credential,
    deposit: Coin,
    anchor: Option[Anchor] = None
)

object EmulatorDRepRegistration {
    // Java-friendly factories (no default args, no Option).
    def of(credential: Credential, deposit: Coin): EmulatorDRepRegistration =
        EmulatorDRepRegistration(credential, deposit)
    def of(credential: Credential, deposit: Coin, anchor: Anchor): EmulatorDRepRegistration =
        EmulatorDRepRegistration(credential, deposit, Some(anchor))
}

case class EmulatorInitialState(
    utxos: Utxos = Map.empty,
    stakeRegistrations: Seq[EmulatorStakeRegistration] = Seq.empty,
    poolRegistrations: Seq[EmulatorPoolRegistration] = Seq.empty,
    drepRegistrations: Seq[EmulatorDRepRegistration] = Seq.empty,
    datums: Map[DataHash, Data] = Map.empty
)

object EmulatorInitialState {

    /** Java-friendly builder — the case class keeps its Scala default arguments, which Java cannot
      * use.
      */
    def builder(): Builder = new Builder

    final class Builder private[EmulatorInitialState] () {
        private var _utxos: Utxos = Map.empty
        private var _stakeRegistrations = Vector.empty[EmulatorStakeRegistration]
        private var _poolRegistrations = Vector.empty[EmulatorPoolRegistration]
        private var _drepRegistrations = Vector.empty[EmulatorDRepRegistration]
        private var _datums: Map[DataHash, Data] = Map.empty

        def utxos(utxos: java.util.Map[TransactionInput, TransactionOutput]): Builder = {
            import scala.jdk.CollectionConverters.*
            _utxos = utxos.asScala.toMap
            this
        }

        def putUtxo(input: TransactionInput, output: TransactionOutput): Builder = {
            _utxos = _utxos + (input -> output)
            this
        }

        def datums(datums: java.util.Map[DataHash, Data]): Builder = {
            import scala.jdk.CollectionConverters.*
            _datums = datums.asScala.toMap
            this
        }

        def addStakeRegistration(registration: EmulatorStakeRegistration): Builder = {
            _stakeRegistrations = _stakeRegistrations :+ registration
            this
        }

        def addPoolRegistration(registration: EmulatorPoolRegistration): Builder = {
            _poolRegistrations = _poolRegistrations :+ registration
            this
        }

        def addDRepRegistration(registration: EmulatorDRepRegistration): Builder = {
            _drepRegistrations = _drepRegistrations :+ registration
            this
        }

        def build(): EmulatorInitialState = EmulatorInitialState(
          utxos = _utxos,
          stakeRegistrations = _stakeRegistrations,
          poolRegistrations = _poolRegistrations,
          drepRegistrations = _drepRegistrations,
          datums = _datums
        )
    }
}

object EmulatorBase {

    /** Evaluate a UTxO query against a UTxO set.
      *
      * This is the pure, static query evaluation logic shared by both the mutable [[Emulator]] and
      * the immutable `scalus.testing.ImmutableEmulator` (defined in the `scalus-testkit` module).
      */
    def evalQuery(utxos: Utxos, query: UtxoQuery): Utxos = {
        // Evaluate source to get candidate UTxOs
        def evalSource(source: UtxoSource): Utxos = source match
            case UtxoSource.FromAddress(addr) =>
                utxos.filter { case (_, output) => output.address == addr }
            case UtxoSource.FromAsset(policyId, assetName) =>
                utxos.filter { case (_, output) =>
                    output.value.assets.assets
                        .get(policyId)
                        .exists(_.contains(assetName))
                }
            case UtxoSource.FromInputs(inputs) =>
                utxos.filter { case (input, _) => inputs.contains(input) }
            case UtxoSource.FromTransaction(txId) =>
                utxos.filter { case (input, _) => input.transactionId == txId }
            case UtxoSource.FromPaymentCredential(credential) =>
                utxos.filter { case (_, output) =>
                    output.address.keyHashOption
                        .map(hash => Credential.KeyHash(hash.asInstanceOf[AddrKeyHash]))
                        .contains(credential) ||
                    output.address.scriptHashOption
                        .map(Credential.ScriptHash(_))
                        .contains(credential)
                }
            case UtxoSource.Or(left, right) =>
                evalSource(left) ++ evalSource(right)
            case UtxoSource.And(left, right) =>
                val leftResult = evalSource(left)
                val rightResult = evalSource(right)
                leftResult.filter { case (input, _) => rightResult.contains(input) }

        // Evaluate a simple query
        def evalSimple(q: UtxoQuery.Simple): Utxos = {
            val candidates = evalSource(q.source)
            val filtered = q.filter match
                case Some(f) => candidates.filter(UtxoQuery.evalFilter(f, _))
                case None    => candidates
            UtxoQuery.applyPagination(filtered, q.limit, q.offset, q.minRequiredTotalAmount)
        }

        // Evaluate query recursively
        def evalQueryRec(q: UtxoQuery): Utxos = q match
            case simple: UtxoQuery.Simple => evalSimple(simple)
            case UtxoQuery.Or(left, right, limit, offset, minTotal) =>
                val leftResult = evalQueryRec(UtxoQuery.propagate(left, limit, minTotal))
                val rightResult = evalQueryRec(UtxoQuery.propagate(right, limit, minTotal))
                val combined = leftResult ++ rightResult
                UtxoQuery.applyPagination(combined, limit, offset, minTotal)

        evalQueryRec(query)
    }

    /** Creates initial UTxOs for the given addresses, 10,000 ADA each (like Yaci Devkit). */
    def createInitialUtxos(addresses: Seq[Address]): Utxos =
        createInitialUtxos(addresses, Value.ada(10_000L))

    /** Creates initial UTxOs for the given addresses.
      *
      * @param addresses
      *   The addresses to initialize with funds
      * @param initialValue
      *   Initial value per address
      * @return
      *   A map of transaction inputs to outputs
      */
    def createInitialUtxos(
        addresses: Seq[Address],
        initialValue: Value
    ): Utxos = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        addresses.zipWithIndex.map { case (address, index) =>
            Input(genesisHash, index) -> Output(address, initialValue)
        }.toMap
    }

    /** Resolve the inputs a transaction consumes against a UTxO set.
      *
      * Returns the subset of `utxos` whose inputs appear in `transaction`'s input set — i.e. the
      * UTxOs that applying the transaction will remove. Intended to be evaluated against the
      * pre-application snapshot so the consumed values can be retained in [[AppliedTx]].
      */
    def resolveSpent(utxos: Utxos, transaction: Transaction): Utxos =
        transaction.body.value.inputs.toSeq.view
            .flatMap(input => utxos.get(input).map(input -> _))
            .toMap

    /** Build the by-hash index for an applied-tx log — the single place this derivation lives, so
      * an emulator's index and its log cannot drift apart.
      */
    def indexAppliedTxs(log: Iterable[AppliedTx]): Map[TransactionHash, AppliedTx] =
        log.iterator.map(a => a.txHash -> a).toMap

    /** The inline datums held by a UTxO set, by hash.
      *
      * The datums a node can answer for are not only the ones transactions carried in their witness
      * sets: an output with a datum inline puts that datum on the chain, and both a node and
      * Blockfrost index it. Seeded and directly-added UTxOs go through here so the emulator does
      * the same.
      */
    /** The inline datum `output` carries, keyed by its hash; `None` when it carries none. */
    def inlineDatumOf(output: TransactionOutput): Option[(DataHash, Data)] =
        output.datumOption match
            case Some(DatumOption.Inline(d)) => Some(DataHash.fromByteString(d.dataHash) -> d)
            case _                           => None

    def extractInlineDatums(utxos: Utxos): Map[DataHash, Data] =
        utxos.valuesIterator.flatMap(inlineDatumOf).toMap

    def extractDatums(transaction: Transaction): Map[DataHash, Data] = {
        val fromWitness = transaction.witnessSet.plutusData.value.toMap.map {
            case (hash, keptData) => hash -> keptData.value
        }
        val fromInline = transaction.body.value.outputs.iterator.flatMap { out =>
            out.value.datumOption match
                case Some(DatumOption.Inline(d)) =>
                    Some(DataHash.fromByteString(d.dataHash) -> d)
                case _ => None
        }.toMap
        fromWitness ++ fromInline
    }

    def buildInitialState(
        initState: EmulatorInitialState,
        context: Context
    ): (CertState, Map[DataHash, Data]) = {
        val deposit = Coin(context.env.params.stakeAddressDeposit)
        val poolDeposit = Coin(context.env.params.stakePoolDeposit)

        val dstate = DelegationState(
          deposits = initState.stakeRegistrations.map(s => s.credential -> deposit).toMap,
          rewards = initState.stakeRegistrations.map(s => s.credential -> s.rewards).toMap,
          stakePools =
              initState.stakeRegistrations.flatMap(s => s.delegatedTo.map(s.credential -> _)).toMap
        )

        val pstate = PoolsState(
          stakePools = initState.poolRegistrations.map { p =>
              PoolKeyHash.fromByteString(p.params.operator) -> p.params
          }.toMap,
          deposits = initState.poolRegistrations.map { p =>
              PoolKeyHash.fromByteString(p.params.operator) -> poolDeposit
          }.toMap
        )

        val vstate = VotingState(
          dreps = initState.drepRegistrations.map { d =>
              d.credential -> DRepState(
                expiry = 0L,
                anchor = d.anchor,
                deposit = d.deposit,
                delegates = Set.empty
              )
          }.toMap
        )

        val certState = CertState(dstate = dstate, pstate = pstate, vstate = vstate)
        (certState, initState.datums)
    }

    /** Builds a [[scalus.cardano.ledger.CertState]] with the given stake credentials
      * pre-registered.
      *
      * Each credential is inserted into `deposits` (using the protocol parameter deposit amount)
      * and `rewards` with the provided balance, so the ledger treats them as registered stake
      * addresses without requiring an explicit registration transaction.
      *
      * @param initialStakeRewards
      *   map from stake credential to its initial reward balance
      * @param context
      *   the emulator context (used to read the key deposit amount from protocol params)
      */
    def certStateWithRegisteredCredentials(
        initialStakeRewards: Map[Credential, Coin],
        context: Context
    ): CertState = {
        val deposit = Coin(context.env.params.stakeAddressDeposit)
        val dstate = DelegationState(
          deposits = initialStakeRewards.map { case (cred, _) => cred -> deposit },
          rewards = initialStakeRewards
        )
        CertState(dstate = dstate)
    }
}
