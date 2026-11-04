package scalus.cardano.node

import scalus.uplc.DebugScript
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, STS, State}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.{AccountState, NewEpochState, RatifyState, RewardUpdate, SnapShots}
import scalus.cardano.ledger.epoch.internal.EpochTransition
import scalus.cardano.node.emulator.{AwaitTxPolicy, ChainConfig, ChainView, ChainViewError, ForgeError, RollbackError, TxStatus}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.internal.ChainEvent

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

    /** The gov state: the proposals in the order they were submitted, with their votes, the enacted
      * roots, the committee and the constitution, spec [ES-9]. One proposal is
      * `govState.proposal(id)`; one DRep state is `certState.vstate.dreps(credential)`, D35.
      */
    def govState: GovState = readState.ledger.govState
    protected def currentContext: Context = readState.context

    /** Every datum this emulator has seen, by hash, decoded. [[binaryDatums]] keeps the bytes. */
    def datums: Map[DataHash, Data] = readState.binaryDatums.view.mapValues(_.value).toMap

    /** Every datum this emulator has seen, by hash, with the bytes it arrived in, spec [SC-13k]. A
      * datum seeded as `Data` carries its canonical encoding.
      */
    private[scalus] def binaryDatums: Map[DataHash, KeepRaw[Data]] = readState.binaryDatums

    /** The fee pot: the fees of the applied transactions since the last epoch boundary, plus those
      * of the epoch before it, which the next boundary takes out (`deltaF`, ledger-rules spec
      * [RW-12]).
      */
    def fees: Coin = readState.ledger.fees

    /** The donations of this epoch. The next epoch boundary moves them into [[treasury]]. */
    def donation: Coin = readState.ledger.donation

    /** The treasury as of the last epoch boundary, the value a tx must state as its
      * `currentTreasuryValue`. It lives in the env, spec [SC-6], so it starts as
      * `initialContext.env.treasury`.
      */
    def treasury: Coin = readState.context.env.treasury

    /** The reserves, ledger-rules spec [ES-9]. They start as `EmulatorInitialState.reserves`, the
      * mainnet reserves by default, spec [ES-7].
      */
    def reserves: Coin = readState.accountState.reserves

    /** The stake snapshots `mark`, `set` and `go` and the fee snapshot, as of the last epoch
      * boundary, ledger-rules spec [ES-9a]. The genesis fills all three from the initial state,
      * spec [ES-8a].
      */
    def snapshots: SnapShots = readState.snapshots

    /** The reward update the last epoch boundary computed from `go`, which the next boundary pays,
      * ledger-rules spec [ES-9a], [RW-21]. `None` before the first boundary, spec [ES-8a].
      */
    def pendingRewards: Option[RewardUpdate] = readState.pendingRewards

    /** The ratify result the last epoch boundary computed, which the next boundary enacts,
      * ledger-rules spec [ES-9b], [EB-10a], D94: the enacted ids in order, the expired ids, and the
      * enact state after the run. At genesis it enacts nothing, spec [DM-9c]. What a boundary
      * enacted shows after it in [[govState]], the pparams and the balances.
      *
      * RATIFY never accepts a HardForkInitiation to protocol version 12 or more, spec [EN-4a], D91:
      * such an action delays nothing, and it expires with its deposit refunded.
      */
    def pendingRatify: RatifyState = readState.pendingRatify

    /** The whole ledger state in the shape of the node's `NewEpochState`, read from one state,
      * ledger-rules spec D122: the UTxO and cert state, the gov state, the account state, the stake
      * snapshots, the reward update, the ratify result, the pparams and the pool distribution. Its
      * fields equal the getters above; its treasury is [[treasury]], spec [DM-13].
      */
    def ledgerState: NewEpochState = readState.ledgerState

    /** The epoch of the current slot, ledger-rules spec [ES-9]. */
    def currentEpoch: EpochNo = {
        val context = readState.context
        context.slotConfig.epochOf(context.env.slot)
    }

    /** Index of applied transactions by hash, for O(1) lookup. */
    def appliedTxIndex: Map[TransactionHash, AppliedTx] = readState.appliedTxIndex

    /** Ordered log of transactions applied to this emulator, oldest first: the log the emulator was
      * built with, then the txs of the blocks of the selected chain, spec [TS-5]. The imported txs
      * have no block, so [[txStatus]] and [[checkTransaction]] report them `NotFound`.
      *
      * Unlike [[appliedTxs]] (hashes only), each entry retains the full transaction together with
      * the slot it was applied at and the inputs it consumed (resolved against the pre-application
      * UTxO set). This lets callers reconstruct chain history — e.g. walk the continuing outputs of
      * an asset — without keeping their own submitted-transaction bookkeeping.
      *
      * In the `Instant` preset the order is the submission order. Under concurrent submission the
      * log entry and the ledger state transition it describes are published together, so a reader
      * never sees a UTxO set that has moved past the log.
      */
    def appliedTxLog: Seq[AppliedTx] = readState.appliedTxLog

    /** Hashes of all applied transactions. Held as a materialized set (derived from
      * [[appliedTxLog]] by [[EmulatorState]], so it cannot drift) rather than a per-call key-set
      * view, so access is a plain O(1) field read.
      */
    def appliedTxs: Set[TransactionHash] = readState.appliedTxs

    def submitSync(transaction: Transaction): Either[SubmitError, TransactionHash] =
        submitWith(transaction, identity)

    /** Every failure of `transaction` under the ledger rules of this emulator, as the ledger
      * collects them, in the order of the ledger's interpreter, ledger-rules spec [FL-1], [FL-8].
      * Empty when the rules accept it. A dry run on the state that [[submitSync]] validates
      * against: the state does not change.
      *
      * [[submitSync]] reports one failure, which need not be the first of this list.
      * `TransactionException.ledgerNames` names each failure with its cardano-ledger constructors.
      *
      * It runs [[validators]] and [[mutators]], not [[processTransaction]]: a subclass that
      * overrides [[processTransaction]] to check other rules does not change this list.
      */
    def ledgerFailures(transaction: Transaction): Seq[TransactionException] =
        readState.ledgerFailures(transaction, validators, mutators)

    /** Move the current slot to `slot`, then revalidate the mempool, spec section 10. Each epoch
      * boundary crossed costs one epoch transition, so a move across millions of epochs is slow.
      *
      * @throws IllegalArgumentException
      *   in the `Manual` preset, for a slot below the slot of the tip block, spec [RV-8]
      */
    def setSlot(slot: SlotNo): Unit = edit(_.withSlot(slot))

    /** An independent emulator starting from this one's state, taken as a single read — the copy
      * and the original share no mutable cell and evolve separately from here on. The copy starts
      * from the whole state, so it keeps the fees, the donations and the treasury, spec [SC-7c].
      */
    def snapshot(): Emulator = EmulatorBase.fromState(readState, validators, mutators)

    /** Clear the applied-transaction bookkeeping ([[appliedTxLog]] and [[appliedTxIndex]]), leaving
      * the ledger state (`utxos`, `certState`, `datums`) untouched.
      *
      * The blocks stay: the log drops the imported log and then starts above the tip, spec [DE-4].
      */
    @deprecated(
      "the selected chain is the applied-tx log; start a new Emulator for an empty log",
      "1.4.1"
    )
    def clearAppliedTxs(): Unit = updateState(_.withClearedAppliedTxs)

    /** Adds a datum to the datum store [[getDatum]] reads, with the bytes it is given, so a UTxO
      * added with [[addUtxo]] that holds only the hash of its datum can have the datum found too.
      * Adding a datum the store already holds changes nothing.
      *
      * @return
      *   the hash of the bytes of `datum`, which is the key [[getDatum]] finds it under
      */
    def addDatum(datum: KeepRaw[Data]): DataHash = {
        val hash = datum.dataHash
        edit(_.withSeededDatums(Map(hash -> datum)))
        hash
    }

    /** `addDatum` for a datum without its original bytes: the store keeps its canonical CBOR.
      *
      * @return
      *   the hash of the canonical CBOR of `data`
      */
    def addDatum(data: Data): DataHash = addDatum(KeepRaw(data))

    /** Add `datums` to the datum store with their bytes, for a caller that has them. */
    private[scalus] def seedDatums(datums: Map[DataHash, KeepRaw[Data]]): Unit =
        edit(_.withSeededDatums(datums))

    /** Apply the direct edit or tick `f`, then revalidate the mempool, spec [DE-3], section 10. */
    private def edit(f: EmulatorState => EmulatorState): Unit =
        updateState(state => f(state).revalidated(processTransaction))

    /** Submit `transaction`, validated under a context derived by `validationContext`. See
      * [[EmulatorState]] for the snapshot it is validated on and when it lands in a block.
      *
      * The derived context is used for validation only; the state keeps the context it had, so a
      * per-submission concern such as debug scripts cannot leak into later submissions.
      */
    private def submitWith(
        transaction: Transaction,
        validationContext: Context => Context
    ): Either[SubmitError, TransactionHash] =
        modifyState(_.submit(transaction, validationContext, processTransaction))

    /** Evaluator mode the emulator runs Plutus scripts in (e.g. `Validate` vs
      * `EvaluateAndComputeCost`). Exposed so snapshots such as `ImmutableEmulator.fromEmulator` can
      * preserve it rather than silently reverting to the `Context` default.
      */
    def evaluatorMode: EvaluatorMode = currentContext.evaluatorMode

    def tick(n: Long): Unit = setSlot(currentContext.env.slot + n)

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

    def getDelegation(credential: Credential): DelegationInfo = {
        val account = certState.dstate.accounts.get(credential)
        DelegationInfo(
          poolId = account.flatMap(_.stakePoolDelegation),
          rewards = account.fold(Coin.zero)(_.balance)
        )
    }

    /** Live stake per registered stake credential: the lovelace sitting at addresses delegating to
      * it, plus its reward balance, and the pool it delegates to.
      *
      * This is the emulator's answer to "who controls how much stake", the query a governance or
      * delegation test needs. It is the stake now, not a stake snapshot: [[snapshots]] holds the
      * stake of the last epoch boundaries, which the rewards read. A row here may be for a
      * credential that is not registered or delegates to no pool; a snapshot has no such row.
      *
      * The coin is the ledger's instant stake, the one SNAP reads: pointer, enterprise and Byron
      * addresses carry none.
      *
      * Reads the state cell once, the way [[snapshot]] does. Two reads would let a submission land
      * between them on the JVM, producing a row whose `stake` came from the post-transaction UTxO
      * set and whose `rewards` came from the pre-transaction certificate state.
      */
    def stakeDistribution: Seq[StakeDistributionEntry] = {
        val state = readState
        val accounts = state.ledger.certState.dstate.accounts
        // ledger-rules spec [DM-15] (D80): the instant stake that SNAP reads
        val stakeByCredential = EpochTransition.instantStake(state.ledger.utxos)
        val credentials = accounts.keySet ++ stakeByCredential.keySet
        credentials.toSeq.map { credential =>
            val account = accounts.get(credential)
            StakeDistributionEntry(
              credential = credential,
              pool = account.flatMap(_.stakePoolDelegation),
              stake = stakeByCredential.getOrElse(credential, Coin.zero),
              rewards = account.fold(Coin.zero)(_.balance)
            )
        }
    }

    def getDatum(datumHash: DataHash): Future[Option[Data]] =
        Future.successful(binaryDatums.get(datumHash).map(_.value))

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
        edit(_.withUtxo(input, output))

    /** Pays `amount` into the reward balance of a registered account, as the epoch boundary pays
      * rewards in the Haskell ledger. Nothing else changes: no deposit, no pot, no slot.
      *
      * @throws IllegalArgumentException
      *   if `credential` is not registered, or `amount` is negative
      */
    def addRewards(credential: Credential, amount: Coin): Unit =
        edit(_.withRewards(credential, amount))

    /** Removes a UTxO from the ledger directly, bypassing transaction validation. A no-op if no
      * UTxO sits at that input.
      *
      * A datum the removed UTxO carried inline stays in the datum store: a node keeps answering for
      * a datum it has seen after the output holding it is spent, and so does this.
      */
    def removeUtxo(input: TransactionInput): Unit =
        edit(_.withoutUtxo(input))

    /** Forge a block from the mempool, and return its point, spec section 8.1.
      *
      * In the `Manual` preset this is the only way a block appears: at slot `max(current slot, tip
      * slot + 1)`, with the longest prefix of the mempool that fits the block limits, after the
      * current slot moves there and the mempool is revalidated, spec [FG-1], [FG-3], [FG-4]. An
      * empty mempool forges an empty block, spec [FG-8].
      */
    def produceBlock(): ChainPoint = modifyState(_.produceBlock(processTransaction))

    /** Forge a block of `txs` on `on`, at slot `on.slot + 1`, spec [FG-11]. See the overload with a
      * slot.
      */
    def produceBlock(on: ChainPoint, txs: Seq[Transaction]): Either[ForgeError, ChainPoint] =
        produceBlock(on, on.slot + 1, txs)

    /** Forge a block of `txs` at `slot` on `on`, a block anywhere in the block tree, or
      * [[ChainPoint.origin]] for genesis, and return its point, spec section 8.2. It models a block
      * from another pool: the txs come from the caller, not from the mempool, spec [FG-20].
      *
      * The txs are applied in order to the ledger after `on`, spec [FG-15]. Then chain selection
      * runs, spec [FG-19]: a branch longer than the selected chain becomes the selected chain, and
      * the txs of the blocks it replaces follow the `orphanTxs` policy, spec section 11. On an
      * error nothing changes, spec [FG-17].
      */
    def produceBlock(
        on: ChainPoint,
        slot: SlotNo,
        txs: Seq[Transaction]
    ): Either[ForgeError, ChainPoint] =
        modifyState(_.produceBlock(on, slot, txs, processTransaction))

    /** Remove the last `n` blocks of the selected chain, and every block that descends from them,
      * and return the tip after chain selection, spec section 11.3. The txs of the removed blocks
      * follow the `orphanTxs` policy, and the current slot stays, spec [RB-6], [RB-7].
      */
    def rollback(n: Int): Either[RollbackError, ChainPoint] =
        modifyState(_.rollback(n, processTransaction))

    /** The status of the tx `txHash`, spec [TS-1]. It reads the mempool and the blocks, not the
      * applied-tx log: a tx known only from the log the emulator was built with is `NotFound`.
      */
    def txStatus(txHash: TransactionHash): TxStatus = readState.txStatus(txHash)

    /** The pending txs, oldest first. */
    def mempoolTxs: Seq[Transaction] = readState.mempool.map(_.applied.tx)

    /** The tip of the selected chain, or [[ChainTip.origin]] before the first block. */
    def tip: ChainTip = readState.tip

    /** The ChainSync events that take a follower from `from` to `to`, two tips of the block tree,
      * spec section 12.1: one `RollBackward` to their common ancestor when `from` is not an
      * ancestor of `to`, then one `RollForward` per block up to `to`, in height order.
      * [[ChainTip.origin]] is genesis.
      *
      * Experimental: `ChainEvent`, `AppliedBlock` and `AppliedTransaction` live in
      * `scalus.cardano.node.stream.internal`, outside the binary-compatibility check, and may
      * change in a minor release.
      *
      * @throws IllegalArgumentException
      *   if a tip is not a block of the tree, with the same slot and height
      */
    def diff(from: ChainTip, to: ChainTip): Seq[ChainEvent] = {
        val state = readState
        EmulatorState.diff(state, from, state, to)
    }

    /** The UTxOs that match `query` in the ledger state `view` reads, spec [VW-3]. */
    def findUtxosSync(query: UtxoQuery, view: ChainView): Either[ChainViewError, Utxos] =
        readState.snapshotOf(view).map(s => EmulatorBase.evalQuery(s.ledger.utxos, query))

    /** [[findUtxosSync]] with a view, in a `Future`, spec [VW-3]. */
    def findUtxos(query: UtxoQuery, view: ChainView): Future[Either[ChainViewError, Utxos]] =
        Future.successful(findUtxosSync(query, view))

    /** [[findUtxos]] without the `Future`. The emulator's state is in memory and its query
      * evaluation is pure, so the effect wrapper is an interface formality — JavaScript and Java
      * callers both want the value.
      */
    def findUtxosSync(query: UtxoQuery): Utxos = EmulatorBase.evalQuery(utxos, query)

    override def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] =
        Future.successful(Right(findUtxosSync(query)))

    /** Where the transaction stands, from [[txStatus]]: `Pending` and `Confirmed` map to
      * themselves, every other status to `NotFound`, spec [TS-3], [TS-4]. A tx known only from the
      * applied-tx log the emulator was built with has no block, so it is `NotFound`, as on a node.
      *
      * Authoritative, unlike the inherited default, which infers status from the UTxOs a
      * transaction produced: `findUtxos` here answers `Right(empty)` for a transaction it has never
      * seen, and an emulator that has applied a transaction whose outputs are all since spent
      * produces none either.
      */
    override def checkTransaction(txHash: TransactionHash): Future[TransactionStatus] =
        Future.successful(transactionStatus(txHash))

    /** [[checkTransaction]] without the `Future`. The JavaScript facade calls it, so the bundle
      * does not link `scala.concurrent`.
      */
    private[node] def transactionStatus(txHash: TransactionHash): TransactionStatus = {
        readState.txStatus(txHash) match
            case TxStatus.Pending      => TransactionStatus.Pending
            case _: TxStatus.Confirmed => TransactionStatus.Confirmed
            case _                     => TransactionStatus.NotFound
    }

    /** Wait for the transaction to leave the mempool, without sleeping, spec [SF-2]: with
      * [[AwaitTxPolicy.ProduceBlock]], forge a block while it is `Pending`, at most `maxAttempts`
      * blocks, spec [SF-3]; with [[AwaitTxPolicy.WaitOnly]], report the status as it is, spec
      * [SF-4]. `delayMs` is ignored: the emulator has virtual time.
      */
    override def pollForConfirmation(
        txHash: TransactionHash,
        maxAttempts: Int,
        delayMs: Long
    ): Future[TransactionStatus] = {
        forgeWhilePending(txHash, maxAttempts)
        checkTransaction(txHash)
    }

    /** The forging of [[pollForConfirmation]], without the `Future`, spec [SF-3], [SF-4]. */
    private[node] def forgeWhilePending(txHash: TransactionHash, maxAttempts: Int): Unit =
        if readState.config.awaitTx == AwaitTxPolicy.ProduceBlock then
            var attempts = 0
            while attempts < maxAttempts && txStatus(txHash) == TxStatus.Pending do
                produceBlock()
                attempts += 1

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
  *   the slot at which the ledger last validated `tx`, spec [TS-6]. It can be earlier than the slot
  *   of the block that holds `tx`.
  * @param spent
  *   the UTxOs consumed by `tx`, resolved against the pre-application UTxO set
  */
case class AppliedTx(tx: Transaction, slot: SlotNo, spent: Utxos) {
    def txHash: TransactionHash = tx.id
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

/** What an emulator starts with, beyond its chain parameters.
  *
  * The constructor is private, so a new field later adds a `with` method, not a break. Start from
  * [[EmulatorInitialState.empty]] and set a field with its `with` method, or use the Java
  * [[EmulatorInitialState.builder]], D121.
  *
  * @param committee
  *   the constitutional committee at genesis; `None`, the default, is a gov state with no
  *   committee, spec [ES-13]
  * @param committeeHotKeys
  *   the hot key each committee member has authorized at genesis, cold credential to hot
  *   credential. Every cold credential must be a member of `committee`, spec [ES-6a].
  * @param constitution
  *   the constitution at genesis; `None`, the default, is one with an empty anchor and no
  *   guardrails script, spec [ES-14]
  * @param reserves
  *   the reserves at genesis; the default is the mainnet reserves at epoch 665, spec [ES-7]. No
  *   UTxO holds them, spec [ES-7b].
  */
final case class EmulatorInitialState private (
    utxos: Utxos,
    stakeRegistrations: Seq[EmulatorStakeRegistration],
    poolRegistrations: Seq[EmulatorPoolRegistration],
    drepRegistrations: Seq[EmulatorDRepRegistration],
    datums: Map[DataHash, Data],
    committee: Option[Committee],
    committeeHotKeys: Map[Credential, Credential],
    constitution: Option[Constitution],
    reserves: Coin
) {

    /** The 1.4 constructor, kept for binary compatibility, D121. */
    @deprecated("use EmulatorInitialState.empty and the with methods", "1.4.1")
    def this(
        utxos: Utxos = Map.empty,
        stakeRegistrations: Seq[EmulatorStakeRegistration] = Seq.empty,
        poolRegistrations: Seq[EmulatorPoolRegistration] = Seq.empty,
        drepRegistrations: Seq[EmulatorDRepRegistration] = Seq.empty,
        datums: Map[DataHash, Data] = Map.empty
    ) = this(
      utxos,
      stakeRegistrations,
      poolRegistrations,
      drepRegistrations,
      datums,
      None,
      Map.empty,
      None,
      AccountState.mainnet.reserves
    )

    /** The 1.4 `copy` with its defaults, kept for binary compatibility (1.4 callers call its
      * `copy$default$N`); it keeps the fields 1.4 did not have, D121.
      */
    @deprecated("use the with methods", "1.4.1")
    def copy(
        utxos: Utxos = this.utxos,
        stakeRegistrations: Seq[EmulatorStakeRegistration] = this.stakeRegistrations,
        poolRegistrations: Seq[EmulatorPoolRegistration] = this.poolRegistrations,
        drepRegistrations: Seq[EmulatorDRepRegistration] = this.drepRegistrations,
        datums: Map[DataHash, Data] = this.datums
    ): EmulatorInitialState = withFields(
      utxos = utxos,
      stakeRegistrations = stakeRegistrations,
      poolRegistrations = poolRegistrations,
      drepRegistrations = drepRegistrations,
      datums = datums
    )

    /** Replaces the synthetic `copy`, whose defaults would forbid those of the 1.4 one: only one
      * overload may have defaults.
      */
    private def copy(
        utxos: Utxos,
        stakeRegistrations: Seq[EmulatorStakeRegistration],
        poolRegistrations: Seq[EmulatorPoolRegistration],
        drepRegistrations: Seq[EmulatorDRepRegistration],
        datums: Map[DataHash, Data],
        committee: Option[Committee],
        committeeHotKeys: Map[Credential, Credential],
        constitution: Option[Constitution],
        reserves: Coin
    ): EmulatorInitialState = new EmulatorInitialState(
      utxos,
      stakeRegistrations,
      poolRegistrations,
      drepRegistrations,
      datums,
      committee,
      committeeHotKeys,
      constitution,
      reserves
    )

    private def withFields(
        utxos: Utxos = this.utxos,
        stakeRegistrations: Seq[EmulatorStakeRegistration] = this.stakeRegistrations,
        poolRegistrations: Seq[EmulatorPoolRegistration] = this.poolRegistrations,
        drepRegistrations: Seq[EmulatorDRepRegistration] = this.drepRegistrations,
        datums: Map[DataHash, Data] = this.datums,
        committee: Option[Committee] = this.committee,
        committeeHotKeys: Map[Credential, Credential] = this.committeeHotKeys,
        constitution: Option[Constitution] = this.constitution,
        reserves: Coin = this.reserves
    ): EmulatorInitialState = copy(
      utxos,
      stakeRegistrations,
      poolRegistrations,
      drepRegistrations,
      datums,
      committee,
      committeeHotKeys,
      constitution,
      reserves
    )

    def withUtxos(utxos: Utxos): EmulatorInitialState = withFields(utxos = utxos)

    def withStakeRegistrations(
        stakeRegistrations: Seq[EmulatorStakeRegistration]
    ): EmulatorInitialState = withFields(stakeRegistrations = stakeRegistrations)

    def withPoolRegistrations(
        poolRegistrations: Seq[EmulatorPoolRegistration]
    ): EmulatorInitialState = withFields(poolRegistrations = poolRegistrations)

    def withDRepRegistrations(
        drepRegistrations: Seq[EmulatorDRepRegistration]
    ): EmulatorInitialState = withFields(drepRegistrations = drepRegistrations)

    def withDatums(datums: Map[DataHash, Data]): EmulatorInitialState =
        withFields(datums = datums)

    /** The constitutional committee at genesis, spec [ES-6]. */
    def withCommittee(committee: Committee): EmulatorInitialState =
        withFields(committee = Some(committee))

    /** The hot key of each committee member, cold credential to hot credential, spec [ES-6a]. */
    def withCommitteeHotKeys(committeeHotKeys: Map[Credential, Credential]): EmulatorInitialState =
        withFields(committeeHotKeys = committeeHotKeys)

    /** The constitution at genesis, spec [ES-6]. */
    def withConstitution(constitution: Constitution): EmulatorInitialState =
        withFields(constitution = Some(constitution))

    /** The reserves at genesis, spec [ES-7]. */
    def withReserves(reserves: Coin): EmulatorInitialState = withFields(reserves = reserves)
}

object EmulatorInitialState {

    /** No UTxOs, no registrations, no datums, no committee, no constitution and the mainnet
      * reserves, spec [ES-7], [ES-13], [ES-14].
      */
    val empty: EmulatorInitialState = new EmulatorInitialState(
      Map.empty,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      Map.empty,
      None,
      Map.empty,
      None,
      AccountState.mainnet.reserves
    )

    /** The 1.4 `apply`, kept for source and binary compatibility, D121. */
    @deprecated("use EmulatorInitialState.empty and the with methods", "1.4.1")
    def apply(
        utxos: Utxos = Map.empty,
        stakeRegistrations: Seq[EmulatorStakeRegistration] = Seq.empty,
        poolRegistrations: Seq[EmulatorPoolRegistration] = Seq.empty,
        drepRegistrations: Seq[EmulatorDRepRegistration] = Seq.empty,
        datums: Map[DataHash, Data] = Map.empty
    ): EmulatorInitialState = empty
        .withUtxos(utxos)
        .withStakeRegistrations(stakeRegistrations)
        .withPoolRegistrations(poolRegistrations)
        .withDRepRegistrations(drepRegistrations)
        .withDatums(datums)

    /** The Java form of `empty` and the `with` methods. */
    def builder(): Builder = new Builder

    final class Builder private[EmulatorInitialState] () {
        private var _utxos: Utxos = Map.empty
        private var _stakeRegistrations = Vector.empty[EmulatorStakeRegistration]
        private var _poolRegistrations = Vector.empty[EmulatorPoolRegistration]
        private var _drepRegistrations = Vector.empty[EmulatorDRepRegistration]
        private var _datums: Map[DataHash, Data] = Map.empty
        private var _committee: Option[Committee] = None
        private var _committeeHotKeys: Map[Credential, Credential] = Map.empty
        private var _constitution: Option[Constitution] = None
        private var _reserves: Coin = AccountState.mainnet.reserves

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

        /** The constitutional committee at genesis, spec [ES-6]. */
        def committee(committee: Committee): Builder = {
            _committee = Some(committee)
            this
        }

        /** The hot key `cold`, a member of the committee, has authorized at genesis, spec [ES-6a].
          */
        def addCommitteeHotKey(cold: Credential, hot: Credential): Builder = {
            _committeeHotKeys = _committeeHotKeys + (cold -> hot)
            this
        }

        /** The constitution at genesis, spec [ES-6]. */
        def constitution(constitution: Constitution): Builder = {
            _constitution = Some(constitution)
            this
        }

        /** The reserves at genesis, spec [ES-7]; the mainnet reserves when not given. */
        def reserves(reserves: Coin): Builder = {
            _reserves = reserves
            this
        }

        def build(): EmulatorInitialState = new EmulatorInitialState(
          _utxos,
          _stakeRegistrations,
          _poolRegistrations,
          _drepRegistrations,
          _datums,
          _committee,
          _committeeHotKeys,
          _constitution,
          _reserves
        )
    }
}

object EmulatorBase {

    /** The state of `emulator`, read once, so its fields cannot come from two different states.
      * Used by the testkit's `ImmutableEmulator`.
      */
    private[scalus] def stateOf(emulator: EmulatorBase): EmulatorState = emulator.readState

    /** An emulator that starts from exactly `state`: its fees, donations and env included, which
      * the constructor cannot take. Used by [[EmulatorBase.snapshot]] and the testkit's
      * `ImmutableEmulator`.
      */
    private[scalus] def fromState(
        state: EmulatorState,
        validators: Iterable[STS.Validator],
        mutators: Iterable[STS.Mutator]
    ): Emulator = {
        val emulator = new Emulator(validators = validators, mutators = mutators)
        emulator.updateState(_ => state)
        emulator
    }

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

    /** The inline datum `output` carries, keyed by its hash; `None` when it carries none. */
    def inlineDatumOf(output: TransactionOutput): Option[(DataHash, Data)] =
        inlineBinaryDatumOf(output).map((hash, datum) => hash -> datum.value)

    /** [[inlineDatumOf]] with the datum bytes, spec [SC-13k]. */
    private[scalus] def inlineBinaryDatumOf(
        output: TransactionOutput
    ): Option[(DataHash, KeepRaw[Data])] =
        output.datumOption match
            // keyed by the hash of the original bytes, spec [SC-13j]
            case Some(inline: DatumOption.Inline) => Some(inline.dataHash -> inline.binaryData)
            case _                                => None

    /** The inline datums held by a UTxO set, by hash.
      *
      * The datums a node can answer for are not only the ones transactions carried in their witness
      * sets: an output with a datum inline puts that datum on the chain, and both a node and
      * Blockfrost index it.
      */
    def extractInlineDatums(utxos: Utxos): Map[DataHash, Data] =
        utxos.valuesIterator.flatMap(inlineDatumOf).toMap

    def extractDatums(transaction: Transaction): Map[DataHash, Data] =
        binaryDatumsOf(transaction).view.mapValues(_.value).toMap

    /** The witness and inline datums of `transaction`, with their bytes, spec [SC-13k]. */
    private[scalus] def binaryDatumsOf(transaction: Transaction): Map[DataHash, KeepRaw[Data]] = {
        val fromWitness = transaction.witnessSet.plutusData.value.toMap
        val fromInline = transaction.body.value.outputs.iterator
            .flatMap(out => inlineBinaryDatumOf(out.value))
            .toMap
        fromWitness ++ fromInline
    }

    def buildInitialState(
        initState: EmulatorInitialState,
        context: Context
    ): (CertState, Map[DataHash, Data]) = {
        val deposit = Coin(context.env.params.stakeAddressDeposit)
        val poolDeposit = Coin(context.env.params.stakePoolDeposit)

        val dstate = DelegationState(
          initState.stakeRegistrations.map { s =>
              s.credential -> ConwayAccountState(s.rewards, deposit, s.delegatedTo, None)
          }.toMap
        )

        val stakePools = initState.poolRegistrations.map { p =>
            PoolKeyHash.fromByteString(p.params.operator) -> p.params
        }.toMap
        val pstate = PoolsState(
          stakePools = stakePools,
          futureStakePoolParams = Map.empty,
          retiring = Map.empty,
          deposits = stakePools.view.mapValues(_ => poolDeposit).toMap,
          // The ledger counts the pools per VRF key on the hard fork into PV 11
          // (conway Rules/HardFork.hs:107-125); a genesis sees no hard fork, spec [BK-7]. Below
          // PV 11 the map stays empty, as in the ledger, so that hard fork gives its counts once,
          // spec [BK-7a], [EB-19a]
          vrfKeyHashes =
              if context.env.params.protocolVersion.major < 11 then Map.empty
              else stakePools.values.groupMapReduce(_.vrfKeyHash)(_ => 1L)(_ + _)
        )

        // spec [DR-5]: a seeded DRep is registered at the genesis epoch
        val drepExpiry =
            context.slotConfig.epochOf(context.env.slot) + context.env.params.dRepActivity
        val vstate = VotingState(
          dreps = initState.drepRegistrations.map { d =>
              d.credential -> DRepState(
                expiry = drepExpiry,
                anchor = d.anchor,
                deposit = d.deposit,
                delegates = Set.empty
              )
          }.toMap,
          numDormantEpochs = 0L, // spec [DR-6], C/State/VState.hs:75-76
          committeeState = committeeStateOf(initState)
        )

        val certState = CertState(dstate = dstate, pstate = pstate, vstate = vstate)
        (certState, initState.datums)
    }

    /** The committee state of the seeded hot keys, one `HotCredential` per key, spec [ES-6a].
      *
      * @throws IllegalArgumentException
      *   if a cold credential is not a member of the seeded committee: GOVCERT rejects the same
      *   authorization with `ConwayCommitteeIsUnknown` (conway Rules/GovCert.hs:193-211), spec
      *   [GC-2]
      */
    private def committeeStateOf(
        initState: EmulatorInitialState
    ): Map[Credential, CommitteeAuthorization] = {
        initState.committeeHotKeys.map { (cold, hot) =>
            require(
              initState.committee.exists(_.members.contains(cold)),
              s"committee hot key for $cold: the cold credential is not a member of the seeded " +
                  "committee"
            )
            cold -> CommitteeAuthorization.HotCredential(hot)
        }
    }

    /** The genesis ledger state of `initState`: the cert state of [[buildInitialState]], the
      * deposits it holds, and the gov state with the seeded committee and constitution, spec
      * [ES-6], [ES-13], [ES-14], D34. With none seeded, the gov state is `GovState.empty`.
      */
    private[node] def genesisLedger(initState: EmulatorInitialState, context: Context): State = {
        val (certState, _) = buildInitialState(initState, context)
        val govState = GovState.empty.copy(
          committee = initState.committee,
          constitution = initState.constitution.getOrElse(GovState.empty.constitution)
        )
        State(
          initState.utxos,
          certState = certState,
          govState = govState,
          deposited = EpochTransition.totalObligation(certState, govState) // D52
        )
    }

    /** The emulator `Emulator.withState` builds, on every platform, spec [ES-6], D34. */
    private[node] def fromInitialState(
        initState: EmulatorInitialState,
        context: Context,
        config: ChainConfig,
        validators: Iterable[STS.Validator],
        mutators: Iterable[STS.Mutator]
    ): Emulator = {
        val datums = initState.datums.view.mapValues(KeepRaw(_)).toMap
        val ledger = genesisLedger(initState, context)
        val state =
            EmulatorState.initial(ledger, context, datums, Vector.empty, config, initState.reserves)
        fromState(state, validators, mutators)
    }

    /** Builds a [[scalus.cardano.ledger.CertState]] with the given stake credentials
      * pre-registered.
      *
      * Each credential gets an account with the provided balance and the protocol parameter
      * deposit, so the ledger treats them as registered stake addresses without requiring an
      * explicit registration transaction.
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
          initialStakeRewards.map { (cred, rewards) =>
              cred -> ConwayAccountState(rewards, deposit, None, None)
          }
        )
        CertState(dstate = dstate)
    }
}
