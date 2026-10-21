package scalus.cardano.node

import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, State}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.uplc.builtin.Data

/** The entire mutable state of an [[EmulatorBase]], as one immutable value: a chain of real blocks,
  * a mempool, and the config, spec section 6.
  *
  * Aggregating it is what lets one state machine serve both platforms, and it is also what makes a
  * transaction land atomically: the ledger state and the bookkeeping that describes it are swapped
  * together, so no reader — and on the JVM no other submitting thread — can observe a UTxO set that
  * has moved on while the applied-transaction log has not caught up. Every transition is a pure
  * function from one state to the next, spec [ST-9].
  *
  * The readers [[ledger]], [[context]] and [[binaryDatums]] return the tip snapshot, spec [ST-8].
  * The current slot is the slot of the tip snapshot: [[withSlot]] ticks it in place, so it can be
  * ahead of the tip block's slot.
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
  * @param dropped
  *   the txs revalidation removed from the mempool, and why, spec [RV-3]. Kept, since the mempool
  *   entry is gone.
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
    private[node] val dropped: Map[TransactionHash, DropReason]
) {

    /** The blocks of the selected chain, oldest first. */
    private[node] def selectedChain: Vector[BlockNode] = chain.map(blocks)

    /** The snapshot of the tip. [ST-2] keeps it, since k is at least 1. */
    private[node] def tipSnapshot: LedgerSnapshot =
        chain.lastOption
            .fold(genesisSnapshot)(blocks(_).snapshot)
            .getOrElse(throw IllegalStateException("the tip has no snapshot"))

    /** The ledger state of the tip snapshot, spec [ST-8]. */
    def ledger: State = tipSnapshot.ledger

    /** The validation context of the tip snapshot, spec [ST-8]. */
    def context: Context = tipSnapshot.context

    /** The datum store of the tip snapshot, spec [ST-8], spec [SC-13k]. */
    def binaryDatums: Map[DataHash, KeepRaw[Data]] = tipSnapshot.binaryDatums

    /** Every datum this state has seen, decoded. */
    @deprecated("use binaryDatums, which keeps the datum bytes", "1.3.0")
    def datums: Map[DataHash, Data] = binaryDatums.view.mapValues(_.value).toMap

    /** The current slot: the slot of the tip snapshot. */
    private[node] def currentSlot: SlotNo = context.env.slot

    /** The state after a submit of `tx`, and the submit result, spec section 7.
      *
      * The tx is validated on the snapshot of the last mempool entry, or the tip snapshot if the
      * mempool is empty, at the current slot, spec [MP-1], [MP-2]. A duplicate fails there, because
      * its inputs are spent, spec [MP-5]. On success the tx and its snapshot join the mempool, spec
      * [MP-6], and in the `Instant` preset the mempool is forged at once, spec [MP-8]. One
      * transition, so one atomic swap, spec [MP-9].
      *
      * @param validationContext
      *   derives the context the tx is validated under, for a per-submit concern such as debug
      *   scripts. The snapshot keeps the context it had, so the concern cannot leak.
      */
    private[node] def submit(
        tx: Transaction,
        validationContext: Context => Context,
        validate: EmulatorState.Validate
    ): (EmulatorState, Either[SubmitError, TransactionHash]) = {
        val base = validationBase
        validate(validationContext(base.context), base.ledger, tx) match
            case Left(error) => (this, Left(SubmitError.fromException(error)))
            case Right(newLedger) =>
                val spent = EmulatorBase.resolveSpent(base.ledger.utxos, tx)
                (accepted(base, newLedger, AppliedTx(tx, currentSlot, spent)), Right(tx.id))
    }

    /** The snapshot a new tx is validated on: the last mempool entry's, or the tip's if the mempool
      * is empty, at the current slot, spec [MP-1], [MP-2].
      */
    private def validationBase: LedgerSnapshot =
        mempool.lastOption.fold(tipSnapshot)(_.snapshot).atSlot(currentSlot)

    /** The state after `applied` moved the ledger of `base` to `newLedger`: the tx and its snapshot
      * join the mempool, spec [MP-6], and in the `Instant` preset the mempool is forged at once,
      * spec [MP-8]. `Manual` forges nothing here.
      */
    private def accepted(
        base: LedgerSnapshot,
        newLedger: State,
        applied: AppliedTx
    ): EmulatorState = {
        val pending =
            copy(mempool = mempool :+ MempoolEntry(applied, base.after(newLedger, applied.tx)))
        config.blockProduction match
            case BlockProduction.Instant => pending.forgeMempool
            case BlockProduction.Manual  => pending
    }

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
    private[node] def produceBlock(
        validate: EmulatorState.Validate
    ): (EmulatorState, ChainPoint) = {
        val forged = config.blockProduction match
            case BlockProduction.Manual =>
                val slot = math.max(currentSlot, tip.slot + 1)
                // the mempool is valid at the current slot already, spec [TE-2]
                val ready =
                    if slot == currentSlot then this else withSlot(slot).revalidated(validate)
                ready.forgePrefix(slot)
            case BlockProduction.Instant => forgePrefix(math.max(currentSlot, tip.slot))
        (forged, forged.tip.point)
    }

    /** The state with a block of the mempool txs at `slot` on the tip; they leave the mempool, spec
      * [FG-7].
      */
    private def forgePrefix(slot: SlotNo): EmulatorState = {
        val (prefix, rest) = mempool.splitAt(mempool.size)
        forge(prefix, slot).copy(mempool = rest)
    }

    /** The state with a block of `entries` at `slot` on the tip. `entries` are already validated in
      * order on the tip snapshot, so the snapshot of the last one is the block snapshot, spec
      * [FG-6]. With no entries, it is the tip snapshot.
      */
    private def forge(entries: Vector[MempoolEntry], slot: SlotNo): EmulatorState = {
        val snapshot = entries.lastOption.fold(tipSnapshot)(_.snapshot)
        val block = BlockForge.forge(
          parent = chain.lastOption,
          height = chain.size + 1L,
          slot = slot,
          txs = entries.map(_.applied.tx),
          protocolVersion = snapshot.context.env.params.protocolVersion
        )
        val txs = entries.map(_.applied)
        val node = BlockNode(block, Some(snapshot), txs)
        copy(
          blocks = blocks.updated(node.id, node),
          chain = chain :+ node.id,
          appliedTxLog = appliedTxLog ++ txs,
          appliedTxIndex = appliedTxIndex ++ txs.map(a => a.txHash -> a),
          appliedTxs = appliedTxs ++ txs.map(_.txHash)
        ).withoutSnapshotBelowK
    }

    /** The tip of the selected chain, or [[ChainTip.origin]] at genesis. */
    private[node] def tip: ChainTip =
        chain.lastOption.fold(ChainTip.origin) { id =>
            val node = blocks(id)
            ChainTip(ChainPoint(node.block.slot, id), node.height)
        }

    /** The status of the tx `hash`, spec [TS-1]: `Pending` in the mempool; `Confirmed` in a block
      * of the selected chain, with the depth of that block now, spec [TS-2], [TS-7]; else the drop
      * revalidation recorded; else `NotFound`.
      */
    private[node] def txStatus(hash: TransactionHash): TxStatus =
        if mempool.exists(_.applied.txHash == hash) then TxStatus.Pending
        else
            // ceiling: a scan from the tip back. Add a tx index if long chains make it slow.
            chain.indices.reverseIterator.find(i =>
                blocks(chain(i)).txs.exists(_.txHash == hash)
            ) match
                case Some(i) =>
                    val node = blocks(chain(i))
                    TxStatus.Confirmed(chain.size - i, ChainPoint(node.block.slot, node.id))
                case None => dropped.get(hash).fold(TxStatus.NotFound)(TxStatus.Dropped(_))

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

    /** The state without the one snapshot the last forge moved to depth k + 1, spec [ST-4]. The
      * block itself stays, spec [ST-5].
      */
    private def withoutSnapshotBelowK: EmulatorState = {
        val height = chain.size - config.securityParam
        if height < 0 then this
        else if height == 0 then copy(genesisSnapshot = None)
        else
            val id = chain((height - 1).toInt)
            copy(blocks = blocks.updated(id, blocks(id).copy(snapshot = None)))
    }

    /** The state with `f` applied to the tip snapshot in place, for the direct edits and the tick,
      * spec [DE-1]. No block is forged, spec [DE-2].
      */
    private def withTipSnapshot(f: LedgerSnapshot => LedgerSnapshot): EmulatorState = {
        val snapshot = f(tipSnapshot)
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
    private[node] def revalidated(validate: EmulatorState.Validate): EmulatorState =
        if mempool.isEmpty then this
        else
            // ceiling: a scan of the selected chain. Add a tx index if long chains make it slow.
            val onChain = selectedChain.iterator.flatMap(_.txs.map(_.txHash)).toSet
            var base = tipSnapshot
            val kept = Vector.newBuilder[MempoolEntry]
            var drops = dropped
            for entry <- mempool if !onChain.contains(entry.applied.txHash) do
                val tx = entry.applied.tx
                validate(base.context, base.ledger, tx) match
                    case Left(error) =>
                        drops =
                            drops.updated(tx.id, DropReason.of(SubmitError.fromException(error)))
                    case Right(newLedger) =>
                        val spent = EmulatorBase.resolveSpent(base.ledger.utxos, tx)
                        val next = base.after(newLedger, tx)
                        kept += MempoolEntry(AppliedTx(tx, currentSlot, spent), next)
                        base = next
            copy(mempool = kept.result(), dropped = drops)

    /** The state after `applied` moved the ledger to `newLedger`, as after a submit that passed
      * validation: the tx joins the mempool, and in the `Instant` preset it is forged into a block
      * at once, spec [MP-6], [MP-8]. `newLedger` must follow the snapshot a submit validates on,
      * which in `Instant` is the tip snapshot.
      */
    def withApplied(newLedger: State, applied: AppliedTx): EmulatorState =
        accepted(validationBase, newLedger, applied)

    /** The state with an empty applied-tx log: the imported log is dropped and the log starts above
      * the tip, spec [DE-4]. No block is removed, and the ledger, the context and the datum store
      * are kept: see [[EmulatorBase.clearAppliedTxs]].
      */
    def withClearedAppliedTxs: EmulatorState = EmulatorState.withLog(this, Vector.empty, chain.size)

    /** The state at a different slot, the tick of spec [NL-2]: the tip snapshot moves in place, so
      * the slot the tip snapshot carries can be later than the slot of the tip block. Crossing an
      * epoch boundary moves the donations into the treasury, see [[EmulatorBase.atSlot]].
      *
      * @throws IllegalArgumentException
      *   in the `Manual` preset, for a slot below the slot of the tip block, spec [RV-8]
      */
    def withSlot(slot: SlotNo): EmulatorState = {
        require(
          config.blockProduction == BlockProduction.Instant || slot >= tip.slot,
          s"slot $slot is below the tip slot ${tip.slot}"
        )
        withTipSnapshot(_.atSlot(slot))
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

    /** How a transition validates a tx: the overridable `EmulatorBase.processTransaction`. */
    private[node] type Validate =
        (Context, State, Transaction) => Either[TransactionException, State]

    /** The state an emulator starts from — the only public way to build one — in the `Instant`
      * preset, spec [CF-4].
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
    ): EmulatorState =
        initial(utxos, certState, context, datums, appliedTxLog, ChainConfig.instant)

    /** [[initial]] with a [[ChainConfig]], spec [CF-5]. */
    def initial(
        utxos: Utxos,
        certState: CertState,
        context: Context,
        datums: Map[DataHash, Data],
        appliedTxLog: Vector[AppliedTx],
        config: ChainConfig
    ): EmulatorState =
        initial(
          State(utxos, certState = certState),
          context,
          datums.view.mapValues(KeepRaw(_)).toMap,
          appliedTxLog,
          config
        )

    /** The state an emulator starts from, with the whole ledger state given: its fees and donations
      * too, not only the UTxOs and the certificate state. `datums` keep their bytes.
      */
    def initial(
        ledger: State,
        context: Context,
        datums: Map[DataHash, KeepRaw[Data]],
        appliedTxLog: Vector[AppliedTx]
    ): EmulatorState = initial(ledger, context, datums, appliedTxLog, ChainConfig.instant)

    /** [[initial]] with a [[ChainConfig]], spec [CF-5]. The given state is the genesis snapshot. */
    def initial(
        ledger: State,
        context: Context,
        datums: Map[DataHash, KeepRaw[Data]],
        appliedTxLog: Vector[AppliedTx],
        config: ChainConfig
    ): EmulatorState = {
        val binaryDatums = appliedTxLog.foldLeft(
          datums ++ ledger.utxos.valuesIterator.flatMap(EmulatorBase.inlineBinaryDatumOf)
        )((acc, a) => acc ++ EmulatorBase.binaryDatumsOf(a.tx))
        val genesis = LedgerSnapshot(ledger, context, binaryDatums)
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
          dropped = Map.empty
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

/** A pending tx and the ledger after it, spec [ST-7]. */
private[node] final case class MempoolEntry(applied: AppliedTx, snapshot: LedgerSnapshot)
