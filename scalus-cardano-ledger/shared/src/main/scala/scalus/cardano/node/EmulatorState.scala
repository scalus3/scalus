package scalus.cardano.node

import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.{AccountState, NewEpochState, RatifyState, RewardUpdate, SnapShots}
import scalus.cardano.ledger.epoch.internal.{EpochBookkeeping, EpochTransition, Ratify}
import scalus.cardano.ledger.rules.{Context, STS, State, TxRefScriptsSizeValidator}
import scalus.cardano.node.emulator.{BlockForge, BlockIssuer, BlockProduction, ChainConfig, ChainView, ChainViewError, DropReason, ForgeError, LedgerSnapshot, OrphanTxPolicy, RollbackError, TxStatus}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.internal.{AppliedBlock, AppliedTransaction, ChainEvent}
import scalus.uplc.builtin.Data

import scala.annotation.tailrec

/** The entire mutable state of an [[EmulatorBase]], as one immutable value: a chain of real blocks,
  * a mempool, and the config, spec section 6.
  *
  * Aggregating it is what lets one state machine serve both platforms, and it is also what makes a
  * transaction land atomically: the ledger state and the bookkeeping that describes it are swapped
  * together, so no reader — and on the JVM no other submitting thread — can observe a UTxO set that
  * has moved on while the applied-transaction log has not caught up. Every transition is a pure
  * function from one state to the next, spec [ST-9].
  *
  * The readers [[ledger]], [[context]] and [[binaryDatums]] return the tip snapshot, spec [ST-8],
  * ticked to the current slot, as the node's ticked ledger state, spec [NL-2]. The stored snapshots
  * stay at the slot their ledger state was produced at, so a block forged on an older block, or
  * after a rollback, sees the treasury and the donations of its own slot. The current slot is its
  * own field: [[withSlot]] moves it, and it can be ahead of the tip block's slot.
  *
  * `appliedTxLog`, `appliedTxIndex` and `appliedTxs` are derived, kept for O(1) lookup: the log is
  * the imported log, then the txs of the selected-chain blocks above the log start, spec [TS-5].
  * The constructor is private, so [[EmulatorState.initial]] and the transitions below are the only
  * way to build one, and no caller can hand back a state whose log and caches disagree
  * (`EmulatorStateInvariantTest` pins it).
  *
  * The log records one entry per *application*, while the two derived views are keyed by hash, so
  * they are not always the same size. Applying one transaction twice needs the direct ledger edits
  * ([[EmulatorBase.addUtxo]]) to put its inputs back — the ledger rules reject the second
  * submission otherwise — but it is reachable, and then the log holds both applications while the
  * index holds the later one and `appliedTxs` holds the hash once.
  *
  * @param config
  *   how this emulator behaves as a chain
  * @param blocks
  *   the block tree, by block id
  * @param chain
  *   the ids of the selected chain, oldest first. Genesis is not in it.
  * @param genesisSnapshot
  *   the snapshot of genesis, until it is deeper than k, spec [ST-4]
  * @param mempool
  *   the pending txs, oldest first
  * @param importedLog
  *   the applied-tx log the state was built with, spec [TS-5]
  * @param logStart
  *   the height above which the txs of the selected chain join the log, spec [DE-4]
  * @param appliedTxLog
  *   derived: the imported log, then the txs of the selected chain above `logStart`
  * @param appliedTxIndex
  *   derived: `appliedTxLog` keyed by transaction hash, holding the latest application of each
  * @param appliedTxs
  *   derived: the distinct hashes in `appliedTxLog`
  * @param statuses
  *   the status of each tx that left the mempool or the selected chain: `Dropped` by revalidation,
  *   spec [RV-3], or `RolledBack` under the `Drop` policy, spec [OR-4]. Kept, since the entry or
  *   the block is gone.
  * @param currentSlot
  *   the current slot, spec [VW-9]
  */
case class EmulatorState private (
    config: ChainConfig,
    private[node] val blocks: Map[BlockHash, BlockNode],
    private[node] val chain: Vector[BlockHash],
    private[node] val genesisSnapshot: Option[LedgerSnapshot],
    private[node] val mempool: Vector[MempoolEntry],
    private[node] val importedLog: Vector[AppliedTx],
    private[node] val logStart: Long,
    appliedTxLog: Vector[AppliedTx],
    appliedTxIndex: Map[TransactionHash, AppliedTx],
    appliedTxs: Set[TransactionHash],
    private[node] val statuses: Map[TransactionHash, TxStatus],
    private[node] val currentSlot: SlotNo
) {

    /** The blocks of the selected chain, oldest first. */
    private[node] def selectedChain: Vector[BlockNode] = chain.map(blocks)

    /** The stored snapshot of the tip, at the slot its ledger state was produced at, with the
      * direct edits on it, spec [DE-1]. [ST-2] keeps it, since k is at least 1.
      */
    private def storedTip: LedgerSnapshot =
        chain.lastOption
            .fold(genesisSnapshot)(blocks(_).snapshot)
            .getOrElse(throw IllegalStateException("the tip has no snapshot"))

    /** The snapshot of the tip ticked to the current slot, spec [NL-2]: the ledger every reader,
      * every submit and every revalidation sees. Computed, not stored, so no stored snapshot ever
      * carries a later slot than its own, ledger-rules spec [EB-3].
      *
      * Computed at most once per state, ledger-rules spec [EB-3a]: the tick runs one epoch
      * transition per boundary crossed, so a `setSlot` over many epochs would otherwise rerun them
      * on every read. The state is immutable and every transition is a `copy`, which drops the
      * memo, so the memo can never be stale (D48).
      */
    private[node] lazy val tipSnapshot: LedgerSnapshot = storedTip.atSlot(currentSlot, config)

    /** The ledger state of the tip snapshot, spec [ST-8]. */
    def ledger: State = tipSnapshot.ledger

    /** The validation context of the tip snapshot, spec [ST-8]. */
    def context: Context = tipSnapshot.context

    /** The datum store of the tip snapshot, spec [ST-8], spec [SC-13k]. */
    def binaryDatums: Map[DataHash, KeepRaw[Data]] = tipSnapshot.binaryDatums

    /** The treasury and the reserves of the tip snapshot, ledger-rules spec [ES-9]. The treasury is
      * `context.env.treasury`, spec [DM-13].
      */
    def accountState: AccountState = tipSnapshot.epoch.accountState

    /** The stake snapshots `mark`, `set`, `go` and the fee snapshot of the tip snapshot,
      * ledger-rules spec [ES-9a].
      */
    def snapshots: SnapShots = tipSnapshot.epoch.snapshots

    /** The reward update the last boundary of the tip snapshot computed and the next one pays, or
      * `None` before the first boundary, ledger-rules spec [ES-9a], [RW-21].
      */
    def pendingRewards: Option[RewardUpdate] = tipSnapshot.epoch.pendingRewards

    /** The ratify result the last boundary of the tip snapshot computed and the next one enacts,
      * ledger-rules spec [ES-9b], [EB-20]. At genesis it enacts nothing, spec [DM-9c].
      */
    def pendingRatify: RatifyState = tipSnapshot.epoch.pendingRatify

    /** The whole ledger state of the tip snapshot in the shape of the node's `NewEpochState`,
      * ledger-rules spec D122.
      */
    def ledgerState: NewEpochState = tipSnapshot.newEpochState

    /** The state after a submit of `tx`, and the submit result, spec section 7.
      *
      * The tx is validated on the snapshot of the last mempool entry, or the tip snapshot if the
      * mempool is empty, at the current slot, spec [MP-1], [MP-2]. A duplicate fails there, because
      * its inputs are spent, spec [MP-5]. A tx over the per-tx reference-script limit is rejected
      * whatever `isValid` says, as the node's mempool measure does. A valid tx that does not fit
      * the mempool capacity is rejected with `MempoolFull`, spec [MP-4]. On success the tx and its
      * snapshot join the mempool, spec [MP-6], and in the `Instant` preset the mempool is forged at
      * once, spec [MP-8]. One transition, so one atomic swap, spec [MP-9].
      *
      * @param validationContext
      *   derives the context the tx is validated under, for a per-submit concern such as debug
      *   scripts. The snapshot keeps the context it had, so the concern cannot leak.
      */
    private[scalus] def submit(
        tx: Transaction,
        validationContext: Context => Context,
        validate: EmulatorState.Validate
    ): (EmulatorState, Either[SubmitError, TransactionHash]) = {
        val base = validationBase
        val validated = for
            newLedger <- validate(validationContext(base.context), base.ledger, tx)
            // LEDGER skips the per-tx reference-script limit when isValid is false; the node's
            // mempool measure applies it to every tx (txMeasureRefScripts), so such a tx never
            // reaches a block. Without it the forge would take it first, over the block limit of
            // spec [BB-1].
            _ <- TxRefScriptsSizeValidator.check(tx, base.ledger.utxos)
        yield newLedger
        validated match
            case Left(error) => (this, Left(SubmitError.fromException(error)))
            case Right(_) if !fitsMempool(tx, base.context.env.params) =>
                (this, Left(NetworkSubmitError.MempoolFull(s"the mempool is full: ${tx.id.toHex}")))
            case Right(newLedger) =>
                (accepted(MempoolEntry.of(base, newLedger, tx, currentSlot)), Right(tx.id))
    }

    /** Every failure of `tx` under `validators` and `mutators`, on the snapshot and at the slot
      * that [[submit]] validates it, ledger-rules spec [FL-8]; empty when the ledger rules accept
      * it. The state does not change.
      */
    private[scalus] def ledgerFailures(
        tx: Transaction,
        validators: Iterable[STS.Validator],
        mutators: Iterable[STS.Mutator]
    ): Seq[TransactionException] = {
        val base = validationBase
        STS.Mutator
            .transitAll(validators, mutators, base.context, base.ledger, tx)
            .left
            .getOrElse(Seq.empty)
    }

    /** Whether the mempool with `tx` stays within the mempool capacity, two times the block limits
      * per dimension, spec [MP-4]. It is measured as one block of the mempool txs, as the block
      * limits are, spec [FG-5]. The check runs after the ledger rules, so an invalid tx still gets
      * the error of the rule it fails, spec [MP-3].
      */
    private def fitsMempool(tx: Transaction, params: ProtocolParams): Boolean =
        BlockForge.fits(
          mempool.map(_.applied.tx) :+ tx,
          2 * params.maxBlockBodySize,
          params.maxBlockExecutionUnits + params.maxBlockExecutionUnits
        )

    /** The snapshot a new tx is validated on: the last mempool entry's, or the tip's if the mempool
      * is empty, at the current slot, spec [MP-1], [MP-2].
      */
    private def validationBase: LedgerSnapshot =
        mempool.lastOption.fold(tipSnapshot)(_.snapshot).atSlot(currentSlot, config)

    /** The state after a valid tx: its `entry` joins the mempool, spec [MP-6], and in the `Instant`
      * preset the mempool is forged at once, spec [MP-8]. `Manual` forges nothing here.
      */
    private def accepted(entry: MempoolEntry): EmulatorState =
        copy(mempool = mempool :+ entry).forgedInInstant

    /** The state after the `Instant` forge of the mempool, spec [FG-9]; the same in `Manual`. */
    private def forgedInInstant: EmulatorState = config.blockProduction match
        case BlockProduction.Instant => forgeMempool
        case BlockProduction.Manual  => this

    /** The state after the `Instant` forge, spec section 8.1: blocks on the tip until the mempool
      * is empty, spec [FG-9], each at `max(current slot, tip slot)`, spec [FG-2]. The current slot
      * stays, spec [FG-3a]. An empty mempool forges nothing, spec [FG-10].
      *
      * The entries after a prefix were validated on top of it at the current slot, so the new tip
      * changes nothing for them, and the forge does not revalidate them.
      */
    private def forgeMempool: EmulatorState =
        if mempool.isEmpty then this
        else forgePrefix(math.max(currentSlot, tip.slot)).forgeMempool

    /** The state after an explicit forge from the mempool, and the point of the new block, spec
      * section 8.1.
      *
      * In the `Manual` preset the forge slot is `max(current slot, tip slot + 1)`, spec [FG-1]. The
      * current slot moves there first, and the mempool is revalidated at it, spec [FG-3]. In the
      * `Instant` preset the mempool is empty here, and the slot is that of [FG-2]. An empty mempool
      * forges an empty block, spec [FG-8].
      */
    private[scalus] def produceBlock(
        validate: EmulatorState.Validate
    ): (EmulatorState, ChainPoint) = {
        val forged = config.blockProduction match
            case BlockProduction.Manual =>
                val slot = math.max(currentSlot, tip.slot + 1)
                withSlot(slot).revalidated(validate).forgePrefix(slot)
            case BlockProduction.Instant => forgePrefix(math.max(currentSlot, tip.slot))
        (forged, forged.tip.point)
    }

    /** The state with a block at `slot` on the tip, of the longest mempool prefix that fits the
      * block limits, spec [FG-4], [FG-5], [BB-1]; the prefix leaves the mempool, spec [FG-7]. The
      * reference scripts resolve on the UTxO set of the tip, the block start, spec [BB-3].
      */
    private def forgePrefix(slot: SlotNo): EmulatorState = {
        val size = BlockForge.prefixThatFits(
          mempool.map(_.applied.tx),
          context.env.params,
          storedTip.ledger.utxos
        )
        val (prefix, rest) = mempool.splitAt(size)
        forge(prefix, slot).copy(mempool = rest)
    }

    /** The state with a block of `entries` at `slot` on the tip. `entries` are already validated in
      * order on the tip snapshot, so the snapshot of the last one is the block snapshot, spec
      * [FG-6]. With no entries, it is the tip snapshot.
      */
    private def forge(entries: Vector[MempoolEntry], slot: SlotNo): EmulatorState = {
        // ledger-rules spec [EB-3a]: at the current slot the ticked tip is computed already
        val before = if slot == currentSlot then tipSnapshot else storedTip.atSlot(slot, config)
        val node =
            BlockNode.of(chain.lastOption.map(blocks), slot, entries, before, config.blockIssuer)
        val txs = node.txs
        copy(
          blocks = blocks.updated(node.id, node),
          chain = chain :+ node.id,
          appliedTxLog = appliedTxLog ++ txs,
          appliedTxIndex = appliedTxIndex ++ txs.map(a => a.txHash -> a),
          appliedTxs = appliedTxs ++ txs.map(_.txHash)
        ).pruned
    }

    /** The state after a forge of `txs` at `slot` on `on`, a block anywhere in the block tree or
      * genesis, and the point of the block, spec section 8.2.
      *
      * The txs are applied in order to the snapshot of `on` at `slot`, spec [FG-15]; the mempool is
      * not read, spec [FG-20]. Chain selection runs after it, spec [FG-19]. On an error the state
      * is this one, spec [FG-17]. A block already in the tree is the same block, spec section 4, so
      * the state is this one too.
      */
    private[scalus] def produceBlock(
        on: ChainPoint,
        slot: SlotNo,
        txs: Seq[Transaction],
        validate: EmulatorState.Validate
    ): (EmulatorState, Either[ForgeError, ChainPoint]) =
        branchBlock(on, slot, txs.toVector, validate) match
            case Left(error) => (this, Left(error))
            case Right(node) =>
                val point = ChainPoint(slot, node.id)
                if blocks.contains(node.id) then (this, Right(point))
                else
                    val grown = copy(blocks = blocks.updated(node.id, node))
                    (grown.reselected(selectedChain, validate), Right(point))

    /** The block of `txs` at `slot` on `on`, or why it cannot be forged, spec [FG-12] .. [FG-18].
      * An unknown `on` comes first: the slot of a point that names no block means nothing.
      */
    private def branchBlock(
        on: ChainPoint,
        slot: SlotNo,
        txs: Vector[Transaction],
        validate: EmulatorState.Validate
    ): Either[ForgeError, BlockNode] = {
        // Some(None) is genesis
        val parent =
            if on == ChainPoint.origin then Some(None)
            else blocks.get(on.blockHash).filter(_.block.slot == on.slot).map(Some(_))
        parent match
            case None => Left(ForgeError.UnknownPoint(on)) // spec [FG-13]
            case Some(_) if slot <= on.slot =>
                Left(ForgeError.SlotNotAfterParent(slot, on.slot)) // spec [FG-12]
            case Some(parent) =>
                val depth = forkDepth(parent.map(_.id))
                parent.fold(genesisSnapshot)(_.snapshot) match
                    case Some(snapshot) if depth <= config.securityParam =>
                        val base = snapshot.atSlot(slot, config)
                        if !BlockForge.fitsBlock(txs, base.context.env.params, base.ledger.utxos)
                        then Left(ForgeError.BlockTooBig) // spec [FG-18], [BB-2]
                        else
                            applied(base, txs, slot, validate)
                                .map(BlockNode.of(parent, slot, _, base, config.blockIssuer))
                    case _ =>
                        Left(ForgeError.TooDeep(depth, config.securityParam)) // spec [FG-14]
    }

    /** The mempool entries of `txs` applied in order on `base`, or the first that fails, spec
      * [FG-15], [FG-16].
      */
    private def applied(
        base: LedgerSnapshot,
        txs: Vector[Transaction],
        slot: SlotNo,
        validate: EmulatorState.Validate
    ): Either[ForgeError, Vector[MempoolEntry]] =
        txs.zipWithIndex.foldLeft[Either[ForgeError, Vector[MempoolEntry]]](Right(Vector.empty)) {
            case (acc, (tx, index)) =>
                acc.flatMap { entries =>
                    val before = entries.lastOption.fold(base)(_.snapshot)
                    validate(before.context, before.ledger, tx) match
                        case Left(error) =>
                            Left(ForgeError.InvalidTx(index, SubmitError.fromException(error)))
                        case Right(ledger) =>
                            Right(entries :+ MempoolEntry.of(before, ledger, tx, slot))
                }
        }

    /** The state after `rollback(n)`, and the tip after chain selection, spec section 11.3.
      *
      * The last n blocks of the selected chain and every block that descends from them leave the
      * block tree, spec [RB-4]. The tip is the block of depth n + 1, or genesis, spec [RB-5]; then
      * chain selection runs, spec [CS-7], [RB-9], with the orphan txs and the revalidation of a
      * switch, spec [RB-7], [RB-8]. The current slot stays, spec [RB-6].
      */
    private[scalus] def rollback(
        n: Int,
        validate: EmulatorState.Validate
    ): (EmulatorState, Either[RollbackError, ChainPoint]) = {
        val k = config.securityParam
        val height = chain.size - n
        if n < 1 then (this, Left(RollbackError.NotPositive(n))) // spec [RB-1]
        else if n > k then (this, Left(RollbackError.TooDeep(n, k))) // spec [RB-2]
        else if height < 0 then (this, Left(RollbackError.PastGenesis(n, chain.size))) // [RB-3]
        else if snapshotAt(height).isEmpty then
            // an earlier rollback went back to the anchor, so no snapshot is left at `height`
            (this, Left(RollbackError.TooDeep(n, k)))
        else
            // a block whose fork point is above `height` is removed or descends from one
            // ceiling: a scan of the whole tree, history too. Index branches if it gets slow.
            val trimmed = copy(
              blocks = blocks.filter((id, _) => forkHeight(Some(id)) <= height),
              chain = chain.take(height)
            )
            val next = trimmed.reselected(selectedChain, validate)
            (next, Right(next.tip.point))
    }

    /** The snapshot of the selected-chain block at `height`, or of genesis at height 0. */
    private def snapshotAt(height: Int): Option[LedgerSnapshot] =
        if height == 0 then genesisSnapshot else blocks(chain(height - 1)).snapshot

    /** Whether `node` is on the selected chain. */
    private def isSelected(node: BlockNode): Boolean =
        chain.lift((node.height - 1).toInt).contains(node.id)

    /** The height of the fork point of the block `id`: the nearest selected-chain block at or above
      * it. Genesis, `None`, is height 0.
      */
    @tailrec
    private def forkHeight(id: Option[BlockHash]): Long = id match
        case None => 0L
        case Some(hash) =>
            val node = blocks(hash)
            if isSelected(node) then node.height else forkHeight(node.parent)

    /** The depth of the fork point of the block `id`, spec [FG-14], [CS-3], [ST-6]. For a block on
      * the selected chain, its own depth; genesis has depth tip height + 1.
      */
    private def forkDepth(id: Option[BlockHash]): Int = (chain.size - forkHeight(id) + 1).toInt

    /** The state after a change of the block tree, spec [CS-7]: the longest chain, spec [CS-1],
      * keeping the current one on a tie, spec [CS-2].
      *
      * @param old
      *   the selected chain before the change
      */
    private def reselected(
        old: Vector[BlockNode],
        validate: EmulatorState.Validate
    ): EmulatorState = {
        val best = bestChain
        if best == old.map(_.id) then this else switchedTo(best, old, validate)
    }

    /** The ids of the chain chain selection picks: the selected chain, or the path to the highest
      * block above it, spec [CS-1], [CS-2]. Among several of the same height, the lowest block id
      * wins. Every branch in the tree forks within k, since pruning removes the others, so none is
      * too deep to switch to, spec [CS-3].
      */
    private def bestChain: Vector[BlockHash] = {
        // no branch: every block in the tree is on the selected chain
        if blocks.size == chain.size then chain
        else
            val longer = blocks.valuesIterator.filter(_.height > chain.size).toVector
            if longer.isEmpty then chain
            else pathTo(longer.minBy(n => (-n.height, n.id.toHex)))
    }

    /** The ids from genesis to `node`, oldest first. */
    private def pathTo(node: BlockNode): Vector[BlockHash] = {
        @tailrec
        def loop(id: Option[BlockHash], acc: List[BlockHash]): Vector[BlockHash] = id match
            case None => acc.toVector
            case Some(hash) =>
                val next = blocks(hash)
                if isSelected(next) then chain.take(next.height.toInt) ++ acc
                else loop(next.parent, hash :: acc)
        loop(Some(node.id), Nil)
    }

    /** The state with `newChain` as the selected chain, after a switch from `old`, spec section 11.
      *
      * The current slot becomes `max(current slot, new tip slot)`, spec [CS-4], so a rollback keeps
      * it, spec [RB-6]. The orphan txs follow the policy, spec [CS-5], [OR-1] .. [OR-4], then the
      * mempool is revalidated, spec [CS-6]. The tree is pruned, spec [ST-4], [ST-6], and `Instant`
      * forges the mempool, spec [OR-5].
      */
    private def switchedTo(
        newChain: Vector[BlockHash],
        old: Vector[BlockNode],
        validate: EmulatorState.Validate
    ): EmulatorState = {
        val fork = newChain.iterator.zip(old.iterator).takeWhile((id, node) => id == node.id).size
        // a tx also on the new chain is confirmed: revalidation removes it quietly, spec [RV-2],
        // and txStatus reads the chain before the recorded status
        val orphans = old.drop(fork).flatMap(_.txs.map(_.tx))
        val switched = EmulatorState
            .withLog(copy(chain = newChain), importedLog, math.min(logStart, fork.toLong))
        val moved = switched.copy(currentSlot = math.max(currentSlot, switched.tip.slot))
        val revalidated = config.orphanTxs match
            case OrphanTxPolicy.ReturnToMempool =>
                moved.withMempoolOf(orphans ++ moved.mempool.map(_.applied.tx), validate)
            case OrphanTxPolicy.Drop =>
                moved
                    .copy(statuses = moved.statuses ++ orphans.map(_.id -> TxStatus.RolledBack))
                    .revalidated(validate)
        revalidated.pruned.forgedInInstant
    }

    /** The block of the tree that `tip` names, or `None` for [[ChainTip.origin]]. */
    private def nodeOf(tip: ChainTip): Option[BlockNode] =
        if tip == ChainTip.origin then None
        else
            val node = blocks.get(tip.blockHash).filter(n => EmulatorState.tipOf(n) == tip)
            require(node.isDefined, s"$tip is not a block of the block tree")
            node

    /** The tip of the selected chain, or [[ChainTip.origin]] at genesis. */
    private[scalus] def tip: ChainTip =
        chain.lastOption.fold(ChainTip.origin)(id => EmulatorState.tipOf(blocks(id)))

    /** The status of the tx `hash`, spec [TS-1]: `Pending` in the mempool; `Confirmed` in a block
      * of the selected chain, with the depth of that block now, spec [TS-2], [TS-7]; else the
      * `Dropped` or `RolledBack` recorded when it left; else `NotFound`.
      */
    private[scalus] def txStatus(hash: TransactionHash): TxStatus =
        if mempool.exists(_.applied.txHash == hash) then TxStatus.Pending
        else
            val i = chainIndexOf(hash)
            if i >= 0 then
                val node = blocks(chain(i))
                TxStatus.Confirmed(chain.size - i, ChainPoint(node.block.slot, node.id))
            else statuses.getOrElse(hash, TxStatus.NotFound)

    /** The index in `chain` of the last selected-chain block that holds the tx `hash`, or -1.
      *
      * The log holds every selected-chain tx above `logStart`, so a hash out of the log can only
      * sit in a block at or below `logStart`, which clearAppliedTxs moved up. A hash in no block
      * costs one lookup in the log, and a scan of the blocks up to `logStart`, none unless
      * clearAppliedTxs ran.
      */
    private def chainIndexOf(hash: TransactionHash): Int = {
        // ceiling: a scan from the tip back for a hash in the log, or from `logStart` back for one
        // out of it. Add a tx-to-block index if long chains make it slow.
        val inBlock = (id: BlockHash) => blocks(id).txs.exists(_.txHash == hash)
        if appliedTxs.contains(hash) then chain.lastIndexWhere(inBlock)
        else chain.lastIndexWhere(inBlock, logStart.toInt - 1)
    }

    /** The snapshot `view` reads, spec section 9. */
    private[node] def snapshotOf(view: ChainView): Either[ChainViewError, LedgerSnapshot] =
        view match
            case ChainView.Tip     => Right(tipSnapshot)
            case ChainView.Mempool => Right(mempool.lastOption.fold(tipSnapshot)(_.snapshot))
            case ChainView.Stable(depth) =>
                if depth < 1 || depth > chain.size + 1 then
                    Left(ChainViewError.DepthOutOfRange(depth, chain.size.toLong)) // spec [VW-6]
                else
                    val snapshot =
                        if depth == chain.size + 1 then genesisSnapshot // spec [VW-8]
                        else blocks(chain(chain.size - depth)).snapshot // spec [VW-5]
                    snapshot.toRight(ChainViewError.SnapshotDropped(depth)) // spec [VW-7]

    /** The state after pruning, spec [CS-7]: without the branches whose fork point is deeper than
      * k, spec [ST-6], and without the snapshots of the selected-chain blocks deeper than k + 1,
      * spec [ST-4]. The blocks themselves stay, spec [ST-5].
      *
      * [CS-7] asks for chain selection after a pruning too. It is not run here, since it would keep
      * the selected chain: it is already the longest when the pruning runs (a forge on the tip
      * extends it, a switch or a rollback has selected it), and the pruning removes only branches
      * and snapshots, never a selected-chain block. The selected chain wins a tie, spec [CS-2].
      */
    private def pruned: EmulatorState = withoutDeepBranches.withoutSnapshotPastAnchor

    /** The state without the branches whose fork point is deeper than k, spec [ST-6]. */
    private def withoutDeepBranches: EmulatorState =
        if blocks.size == chain.size then this
        else
            val deep = blocks.valuesIterator
                .filter(n => !isSelected(n) && forkDepth(Some(n.id)) > config.securityParam)
                .map(_.id)
                .toSet
            if deep.isEmpty then this else copy(blocks = blocks -- deep)

    /** The state without the one snapshot that moved past the anchor, to depth k + 2, spec [ST-4].
      * Depth k + 1 is the anchor, as in the node's LedgerDB, so `rollback(k)` has a snapshot to go
      * back to.
      *
      * One snapshot is enough: chain selection runs after every change of the block tree, so no
      * branch is longer than the selected chain. A forge then makes the selected chain at most one
      * block longer, and a rollback makes it shorter.
      */
    private def withoutSnapshotPastAnchor: EmulatorState = {
        val height = chain.size - config.securityParam - 1
        if height < 0 then this
        else if height == 0 then copy(genesisSnapshot = None)
        else
            val id = chain((height - 1).toInt)
            copy(blocks = blocks.updated(id, blocks(id).copy(snapshot = None)))
    }

    /** The state with `f` applied to the stored tip snapshot in place, for the direct edits, spec
      * [DE-1]. No block is forged, spec [DE-2]. The edit lands before any epoch boundary the
      * current slot has crossed since the tip, and [[tipSnapshot]] ticks the edited snapshot,
      * ledger-rules spec [EB-3b].
      */
    private def withTipSnapshot(f: LedgerSnapshot => LedgerSnapshot): EmulatorState = {
        val snapshot = f(storedTip)
        chain.lastOption match
            case None => copy(genesisSnapshot = Some(snapshot))
            case Some(id) =>
                copy(blocks = blocks.updated(id, blocks(id).copy(snapshot = Some(snapshot))))
    }

    /** The state after revalidation, spec section 10: every mempool tx is applied again, in order,
      * on the tip snapshot at the current slot, spec [RV-1]. A tx on the selected chain leaves
      * quietly, spec [RV-2]. A tx that fails leaves with its drop reason, spec [RV-3] .. [RV-6].
      * Each kept tx gets a new snapshot and its new validation slot, spec [RV-7], [TS-6].
      *
      * With an empty mempool, as always in the `Instant` preset, this is the same state.
      */
    private[scalus] def revalidated(validate: EmulatorState.Validate): EmulatorState =
        if mempool.isEmpty then this else withMempoolOf(mempool.map(_.applied.tx), validate)

    /** The state with the mempool rebuilt from `txs` by the rules of revalidation, spec [RV-1] ..
      * [RV-7]: each tx is applied in order on the tip snapshot at the current slot.
      */
    private def withMempoolOf(
        txs: Vector[Transaction],
        validate: EmulatorState.Validate
    ): EmulatorState = {
        // The txs of the blocks below the clearAppliedTxs cursor, built once per call and only if
        // a tx is out of the log; empty unless clearAppliedTxs ran. A tx in the log can be an
        // imported one, with no block, so `chainIndexOf` scans the blocks for it.
        lazy val belowCursor =
            chain.iterator.take(logStart.toInt).flatMap(blocks(_).txs.map(_.txHash)).toSet
        def onChain(hash: TransactionHash): Boolean =
            if appliedTxs.contains(hash) then chainIndexOf(hash) >= 0 else belowCursor(hash)
        var base = tipSnapshot
        val kept = Vector.newBuilder[MempoolEntry]
        var drops = statuses
        for tx <- txs if !onChain(tx.id) do
            validate(base.context, base.ledger, tx) match
                case Left(error) =>
                    val reason = DropReason.of(SubmitError.fromException(error))
                    drops = drops.updated(tx.id, TxStatus.Dropped(reason))
                case Right(newLedger) =>
                    val entry = MempoolEntry.of(base, newLedger, tx, currentSlot)
                    kept += entry
                    base = entry.snapshot
        copy(mempool = kept.result(), statuses = drops)
    }

    /** The state after `applied` moved the ledger to `newLedger`, as after a submit that passed
      * validation: the tx joins the mempool, and in the `Instant` preset it is forged into a block
      * at once, spec [MP-6], [MP-8]. `newLedger` must follow the snapshot a submit validates on,
      * which in `Instant` is the tip snapshot.
      */
    def withApplied(newLedger: State, applied: AppliedTx): EmulatorState =
        accepted(MempoolEntry(applied, validationBase.after(newLedger, applied.tx)))

    /** The state with an empty applied-tx log: the imported log is dropped and the log starts above
      * the tip, spec [DE-4]. No block is removed, and the ledger, the context and the datum store
      * are kept: see [[EmulatorBase.clearAppliedTxs]].
      */
    def withClearedAppliedTxs: EmulatorState = EmulatorState.withLog(this, Vector.empty, chain.size)

    /** The state at a different slot, spec [NL-2]: only the current slot moves. The tip snapshot
      * readers see is the stored one ticked to it, so crossing epoch boundaries forwards runs their
      * epoch transitions, ledger-rules spec [EB-1]. Moving back before a boundary undoes it only
      * when the tip block is before that boundary too: the tick never runs a boundary the stored
      * tip ran already, ledger-rules spec D66. A tx submitted there is validated at the epoch of
      * the current slot, which can be below the boundaries the tip ran, ledger-rules spec D67: for
      * example a pool retirement for such a boundary is accepted and never reaped.
      *
      * @throws IllegalArgumentException
      *   in the `Manual` preset, for a slot below the slot of the tip block, spec [RV-8]
      */
    def withSlot(slot: SlotNo): EmulatorState = {
        require(
          config.blockProduction == BlockProduction.Instant || slot >= tip.slot,
          s"slot $slot is below the tip slot ${tip.slot}"
        )
        copy(currentSlot = slot)
    }

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
    def withUtxo(input: TransactionInput, output: TransactionOutput): EmulatorState =
        withTipSnapshot { s =>
            s.copy(
              ledger = s.ledger.copy(utxos = s.ledger.utxos + (input -> output)),
              binaryDatums =
                  EmulatorBase.inlineBinaryDatumOf(output).fold(s.binaryDatums)(s.binaryDatums + _)
            )
        }

    /** The state with `seeded` added to the datum store, for a caller that has the datum bytes. A
      * datum already in the store wins, as in [[EmulatorState.initial]].
      */
    private[scalus] def withSeededDatums(seeded: Map[DataHash, KeepRaw[Data]]): EmulatorState =
        withTipSnapshot(s => s.copy(binaryDatums = seeded ++ s.binaryDatums))

    /** The state with `amount` added to the reward balance of the account of `credential`. */
    def withRewards(credential: Credential, amount: Coin): EmulatorState = {
        require(amount.value >= 0, s"rewards must not be negative, got ${amount.value}")
        withTipSnapshot { s =>
            val dstate = s.ledger.certState.dstate
            val account = dstate.accounts.getOrElse(
              credential,
              throw new IllegalArgumentException(s"no registered account for $credential")
            )
            val accounts =
                dstate.accounts.updated(
                  credential,
                  account.copy(balance = account.balance + amount)
                )
            s.copy(ledger =
                s.ledger.copy(certState =
                    s.ledger.certState.copy(dstate = dstate.copy(accounts = accounts))
                )
            )
        }
    }

    /** The state with the UTxO at `input` dropped, for the direct ledger edits that bypass
      * validation. A no-op if none sits there.
      *
      * The datum store is deliberately left alone: a node keeps answering for a datum it has seen
      * after the output holding it is spent, and so does this.
      */
    def withoutUtxo(input: TransactionInput): EmulatorState =
        withTipSnapshot(s => s.copy(ledger = s.ledger.copy(utxos = s.ledger.utxos - input)))
}

object EmulatorState {

    /** The ChainSync events from `from`, a tip of `before`, to `to`, a tip of `after`, spec [SY-1]
      * .. [SY-4]. With two states, `from` can be a block that a rollback or pruning has since
      * removed, as the last tip a stream delivered.
      *
      * A block id is the hash of a header that holds the parent id, so a block with the same id in
      * both trees has the same ancestors. The walk steps the higher tip down to its parent until
      * both tips are the same block: the common ancestor. It costs the distance to that ancestor,
      * so one block forged on the tip costs one step.
      */
    private[node] def diff(
        before: EmulatorState,
        from: ChainTip,
        after: EmulatorState,
        to: ChainTip
    ): Seq[ChainEvent] = {
        val start = before.nodeOf(from)
        type Path = (Option[BlockNode], List[BlockNode])
        // the common ancestor of `a` and `b`, and the blocks above it up to `to`, lowest first
        @tailrec
        def walk(a: Option[BlockNode], b: Option[BlockNode], up: List[BlockNode]): Path =
            if a.map(_.id) == b.map(_.id) then (a, up)
            else if heightOf(b) >= heightOf(a) then
                walk(a, b.get.parent.map(after.blocks), b.get :: up)
            else walk(a.get.parent.map(before.blocks), b, up)
        val (ancestor, forward) = walk(start, after.nodeOf(to), Nil)
        val back =
            if ancestor.map(_.id) == start.map(_.id) then Nil
            else List(ChainEvent.RollBackward(ancestor.fold(ChainTip.origin)(tipOf)))
        back ++ forward.map(n => ChainEvent.RollForward(appliedBlock(n)))
    }

    private def heightOf(node: Option[BlockNode]): Long = node.fold(0L)(_.height)

    private def tipOf(node: BlockNode): ChainTip =
        ChainTip(ChainPoint(node.block.slot, node.id), node.height)

    /** The block as ChainSync delivers it, spec [SY-4]. Each tx carries the outputs it created and
      * the UTxOs it spent, resolved when the ledger applied it.
      */
    private def appliedBlock(node: BlockNode): AppliedBlock = {
        val txs = node.txs.map { applied =>
            val hash = applied.txHash
            val created: Utxos = applied.tx.body.value.outputs.zipWithIndex.map { (out, index) =>
                TransactionInput(hash, index) -> out.value
            }.toMap
            AppliedTransaction(applied.tx, created, applied.spent)
        }
        AppliedBlock(tipOf(node).point, node.height, txs, Some(node.block))
    }

    /** How a transition validates a tx: the overridable `EmulatorBase.processTransaction`. */
    private[scalus] type Validate =
        (Context, State, Transaction) => Either[TransactionException, State]

    /** The state an emulator starts from — the only public way to build one — in the `Instant`
      * preset, spec [CF-4].
      *
      * Both derived views and the datum store are computed here rather than passed in, so an
      * emulator restored from a log — a snapshot, say — cannot disagree with the one the log came
      * from. The datum store also picks up the inline datums of the seeded `utxos`, so seeding a
      * script UTxO with its datum inline is enough for `getDatum` to answer.
      *
      * @param appliedTxLog
      *   Deprecated since 1.4.1: the log has no blocks, so status APIs report `NotFound` for its
      *   txs. Pass an empty log; keep a whole emulator with `snapshot()` or
      *   `ImmutableEmulator.fromEmulator`/`toEmulator`, which keep the blocks.
      */
    def initial(
        utxos: Utxos,
        certState: CertState,
        context: Context,
        datums: Map[DataHash, Data],
        appliedTxLog: Vector[AppliedTx]
    ): EmulatorState =
        initial(utxos, certState, context, datums, appliedTxLog, ChainConfig.instant)

    /** [[initial]] in the preset of `config`, with no imported log, spec [CF-5]. */
    def initial(
        utxos: Utxos,
        certState: CertState,
        context: Context,
        datums: Map[DataHash, Data],
        config: ChainConfig
    ): EmulatorState = initial(utxos, certState, context, datums, Vector.empty, config)

    /** [[initial]] with an imported log and a `config`, for the 1.4 `Emulator` constructor. */
    private[node] def initial(
        utxos: Utxos,
        certState: CertState,
        context: Context,
        datums: Map[DataHash, Data],
        appliedTxLog: Vector[AppliedTx],
        config: ChainConfig
    ): EmulatorState =
        initial(
          // spec [BK-1], [EB-18], D52: a genesis with seeded registrations starts with the
          // deposits it holds, so a refund of a seeded deposit keeps `deposited` consistent
          State(
            utxos,
            certState = certState,
            deposited = EpochTransition.totalObligation(certState, GovState.empty)
          ),
          context,
          datums.view.mapValues(KeepRaw(_)).toMap,
          appliedTxLog,
          config
        )

    /** The state an emulator starts from, with the whole ledger state given: its fees and donations
      * too, not only the UTxOs and the certificate state. `datums` keep their bytes.
      *
      * @param appliedTxLog
      *   Deprecated since 1.4.1, as for the overload above.
      */
    def initial(
        ledger: State,
        context: Context,
        datums: Map[DataHash, KeepRaw[Data]],
        appliedTxLog: Vector[AppliedTx]
    ): EmulatorState = initial(ledger, context, datums, appliedTxLog, ChainConfig.instant)

    /** [[initial]] in the preset of `config`, with no imported log, spec [CF-5]. The given state is
      * the genesis snapshot.
      */
    def initial(
        ledger: State,
        context: Context,
        datums: Map[DataHash, KeepRaw[Data]],
        config: ChainConfig
    ): EmulatorState = initial(ledger, context, datums, Vector.empty, config)

    /** [[initial]] with an imported log and a `config`, for the 1.4 `Emulator` constructor. The
      * reserves are those of mainnet, ledger-rules spec [ES-7].
      */
    private[node] def initial(
        ledger: State,
        context: Context,
        datums: Map[DataHash, KeepRaw[Data]],
        appliedTxLog: Vector[AppliedTx],
        config: ChainConfig
    ): EmulatorState =
        initial(ledger, context, datums, appliedTxLog, config, AccountState.mainnet.reserves)

    /** [[initial]] with `reserves`, ledger-rules spec [ES-7]. The treasury of the account state is
      * `context.env.treasury`, spec [DM-13].
      */
    private[node] def initial(
        ledger: State,
        context: Context,
        datums: Map[DataHash, KeepRaw[Data]],
        appliedTxLog: Vector[AppliedTx],
        config: ChainConfig,
        reserves: Coin
    ): EmulatorState = {
        val binaryDatums = appliedTxLog.foldLeft(
          datums ++ ledger.utxos.valuesIterator.flatMap(EmulatorBase.inlineBinaryDatumOf)
        )((acc, a) => acc ++ EmulatorBase.binaryDatumsOf(a.tx))
        // ledger-rules spec [ES-8], [ES-8a]: mark, set and go are the stake of the genesis ledger,
        // so seeded stake earns from the second boundary on; no fee, no blocks made and no
        // pending rewards. [DM-9c]: the previous pparams are the genesis pparams, and the ratify
        // result enacts and expires nothing on the enact state of the genesis, as the ledger
        // enters Conway (conway Translation.hs:69-83), D85
        val stake = EpochTransition.stakeSnapshot(ledger)
        val params = context.env.params
        val epoch = EpochBookkeeping(
          AccountState(context.env.treasury, reserves),
          context.slotConfig.epochOf(context.env.slot),
          SnapShots(stake, stake, stake, Coin.zero),
          Map.empty,
          Map.empty,
          None,
          params,
          Ratify.nothingRatified(ledger.govState, params)
        )
        val genesis = LedgerSnapshot(ledger, context, binaryDatums, epoch)
        val empty = new EmulatorState(
          config = config,
          blocks = Map.empty,
          chain = Vector.empty,
          genesisSnapshot = Some(genesis),
          mempool = Vector.empty,
          importedLog = Vector.empty,
          logStart = 0L,
          appliedTxLog = Vector.empty,
          appliedTxIndex = Map.empty,
          appliedTxs = Set.empty,
          statuses = Map.empty,
          currentSlot = context.env.slot
        )
        withLog(empty, appliedTxLog, 0L)
    }

    /** `state` with the log defined by `importedLog` and `logStart`, and the derived views
      * recomputed from them: the single place this derivation lives, spec [TS-5].
      */
    private def withLog(
        state: EmulatorState,
        importedLog: Vector[AppliedTx],
        logStart: Long
    ): EmulatorState = {
        val log = importedLog ++ state.chain.drop(logStart.toInt).flatMap(state.blocks(_).txs)
        state.copy(
          importedLog = importedLog,
          logStart = logStart,
          appliedTxLog = log,
          appliedTxIndex = EmulatorBase.indexAppliedTxs(log),
          appliedTxs = log.map(_.txHash).toSet
        )
    }
}

/** A block in the block tree, spec [ST-1]. Its id, parent and height are read from the block, so
  * they cannot disagree with it.
  *
  * @param snapshot
  *   the ledger after the block, until the block is deeper than k, spec [ST-4]
  * @param txs
  *   the txs of the block, in block order, with the slot the ledger validated each at, spec [TS-6]
  */
private[node] final case class BlockNode(
    block: Block,
    snapshot: Option[LedgerSnapshot],
    txs: Vector[AppliedTx]
) {

    /** The block id, spec [BH-2]. */
    def id: BlockHash = block.hash

    /** The id of the parent block, or `None` when the parent is genesis, spec [BH-6]. */
    def parent: Option[BlockHash] = block.header.prevHash

    /** The number of blocks from genesis; the first block has height 1, spec [BH-5]. */
    def height: Long = block.blockNumber
}

private[node] object BlockNode {

    /** The block of `entries` at `slot` on `parent`, or on genesis when `None`, made by `issuer`.
      * `entries` are validated in order on `before`, so the snapshot of the last one is the block
      * snapshot, spec [FG-6]; with no entries, it is `before`.
      *
      * The epoch state of the block snapshot is that of `before` with the block counted for the
      * issuer's pool, registered or not, ledger-rules spec [ES-4], [ES-4a] (`incrBlocks`, alonzo
      * Rules/Bbody.hs:250-257); an `Unattributed` block counts for none, [ES-5]. It is never the
      * epoch state of the last entry: an entry keeps the epoch state of the snapshot it was
      * validated on, which misses the blocks forged since, when a forge left it in the mempool. No
      * tx rule writes the epoch state, so `before`'s is the right one.
      */
    def of(
        parent: Option[BlockNode],
        slot: SlotNo,
        entries: Vector[MempoolEntry],
        before: LedgerSnapshot,
        issuer: BlockIssuer
    ): BlockNode = {
        val epoch = issuer match
            case BlockIssuer.Unattributed => before.epoch
            case BlockIssuer.Pool(pool) =>
                val made = before.epoch.blocksMadeCur
                before.epoch.copy(blocksMadeCur = made.updated(pool, made.getOrElse(pool, 0L) + 1))
        val snapshot = entries.lastOption.fold(before)(_.snapshot).copy(epoch = epoch)
        val block = BlockForge.forge(
          parent = parent.map(_.id),
          height = parent.fold(0L)(_.height) + 1,
          slot = slot,
          txs = entries.map(_.applied.tx),
          protocolVersion = snapshot.context.env.params.protocolVersion
        )
        BlockNode(block, Some(snapshot), entries.map(_.applied))
    }
}

/** A pending tx and the ledger after it, spec [ST-7]. */
private[node] final case class MempoolEntry(applied: AppliedTx, snapshot: LedgerSnapshot)

private[node] object MempoolEntry {

    /** The entry of `tx`, which moved the ledger of `base` to `newLedger` at `slot`. */
    def of(base: LedgerSnapshot, newLedger: State, tx: Transaction, slot: SlotNo): MempoolEntry =
        MempoolEntry(
          AppliedTx(tx, slot, EmulatorBase.resolveSpent(base.ledger.utxos, tx)),
          base.after(newLedger, tx)
        )
}
