package scalus.cardano.node

import scalus.InteropApi
import scalus.uplc.DebugScript
import scalus.uplc.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.emulator.{ChainView, ChainViewError, EmulatorResult, ForgeError, RollbackError}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}
import scalus.cardano.node.stream.internal.ChainEvent

import java.util.concurrent.CompletableFuture
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.jdk.FutureConverters.*

/** Java-facing surface of the [[Emulator]], following the cross-language interop style guide.
  *
  * Members return Java collections, use `null` instead of `Option`, wrap `Either` in
  * [[SubmitResult]], and offer `CompletableFuture` variants of the async provider operations. The
  * synchronous accessors never block: every `EmulatorBase` future is created already completed
  * (`ExecutionContext.parasitic` + `Future.successful`).
  *
  * Scala code should use the idiomatic members of [[Emulator]]/[[EmulatorBase]] instead.
  */
private[node] trait EmulatorJavaApi extends InteropApi { self: Emulator =>

    // ─── State accessors ────────────────────────────────────────────────────

    /** The current UTxO set as a Java map. */
    final def getUtxos: java.util.Map[TransactionInput, TransactionOutput] = utxos.asJava

    /** All known datums by hash as a Java map. */
    final def getDatums: java.util.Map[DataHash, Data] = datums.asJava

    /** The applied-transaction log as a Java list (oldest first). */
    final def getAppliedTxLog: java.util.List[AppliedTx] = appliedTxLog.asJava

    /** Hashes of all applied transactions as a Java set. */
    final def getAppliedTxHashes: java.util.Set[TransactionHash] = appliedTxs.asJava

    /** The applied transaction with the given hash, or `null` if unknown. */
    final def getTransactionOrNull(txHash: TransactionHash): Transaction =
        getTransaction(txHash).orNull

    /** The applied-transaction record with the given hash, or `null` if unknown. */
    final def getAppliedTxOrNull(txHash: TransactionHash): AppliedTx =
        getAppliedTx(txHash).orNull

    /** The datum with the given hash, or `null` if unknown. */
    final def getDatumOrNull(datumHash: DataHash): Data =
        completedNow(getDatum(datumHash)).orNull

    /** The current slot number. */
    final def getCurrentSlot: Long = completedNow(currentSlot)

    /** The protocol parameters the emulator validates against. */
    final def getProtocolParams: ProtocolParams = completedNow(fetchLatestParams)

    /** The gov state: proposals in submission order with their votes, the committee and the
      * constitution, spec [ES-9]. One proposal is `getGovState.proposal(id)`, D35.
      */
    final def getGovState: GovState = govState

    /** The reserves, ledger-rules spec [ES-9]. */
    final def getReserves: Coin = reserves

    /** The epoch of the current slot, ledger-rules spec [ES-9]. */
    final def getCurrentEpoch: Long = currentEpoch

    // ─── Submission ─────────────────────────────────────────────────────────

    /** Submits a transaction and reports the outcome as a [[SubmitResult]] (never throws for a
      * rejected transaction).
      */
    final def trySubmit(transaction: Transaction): SubmitResult =
        SubmitResult.fromEither(submitSync(transaction))

    /** Like `trySubmit`, evaluating failing scripts against the given debug scripts for
      * diagnostics.
      */
    final def trySubmit(
        transaction: Transaction,
        debugScripts: java.util.Map[ScriptHash, DebugScript]
    ): SubmitResult =
        SubmitResult.fromEither(submitSync(transaction, debugScripts.asScala.toMap))

    /** `CompletableFuture` variant of [[trySubmit]], for API-shape parity with asynchronous
      * `BlockchainProvider` implementations.
      */
    final def submitAsync(transaction: Transaction): CompletableFuture[SubmitResult] =
        submit(transaction)
            .map(SubmitResult.fromEither)(using executionContext)
            .asJava
            .toCompletableFuture

    // ─── UTxO queries ───────────────────────────────────────────────────────

    /** All UTxOs at the given address as a Java list. */
    final def findUtxosForAddress(address: Address): java.util.List[Utxo] =
        unwrapQuery(findUtxos(address))

    /** The UTxOs among `inputs` that are unspent, as a Java list. */
    final def findUtxosForInputs(inputs: java.util.Set[TransactionInput]): java.util.List[Utxo] =
        unwrapQuery(findUtxos(inputs.asScala.toSet))

    /** `CompletableFuture` variant of [[findUtxosForAddress]]. */
    final def findUtxosForAddressAsync(
        address: Address
    ): CompletableFuture[java.util.List[Utxo]] =
        findUtxos(address)
            .map(result => toJavaUtxoList(queryResultOrThrow(result)))(using executionContext)
            .asJava
            .toCompletableFuture

    // ─── Chain ──────────────────────────────────────────────────────────────
    //
    // `produceBlock()`, `tip` and `txStatus` take and return Java-usable types already, spec
    // [SF-5]. The members below replace a Scala `Seq` or `Either` in the signature.

    /** [[produceBlock]] on a branch with a Java list: forge `txs` on `on`, at slot `on.slot + 1`,
      * spec [FG-11].
      */
    final def tryProduceBlock(
        on: ChainPoint,
        txs: java.util.List[Transaction]
    ): EmulatorResult[ForgeError, ChainPoint] =
        EmulatorResult.fromEither(produceBlock(on, txs.asScala.toSeq))

    /** [[produceBlock]] on a branch at `slot` with a Java list, spec section 8.2. */
    final def tryProduceBlock(
        on: ChainPoint,
        slot: Long,
        txs: java.util.List[Transaction]
    ): EmulatorResult[ForgeError, ChainPoint] =
        EmulatorResult.fromEither(produceBlock(on, slot, txs.asScala.toSeq))

    /** [[rollback]], with the result as an [[EmulatorResult]], spec section 11.3. */
    final def tryRollback(n: Int): EmulatorResult[RollbackError, ChainPoint] =
        EmulatorResult.fromEither(rollback(n))

    /** The pending txs, oldest first, as a Java list. */
    final def getMempoolTxs: java.util.List[Transaction] = mempoolTxs.asJava

    /** [[diff]] as a Java list, spec section 12.1. Experimental, as [[diff]] is: the event types
      * live in `scalus.cardano.node.stream.internal` and may change in a minor release.
      */
    final def getDiff(from: ChainTip, to: ChainTip): java.util.List[ChainEvent] =
        diff(from, to).asJava

    /** All UTxOs at the given address in the ledger state `view` reads, spec [VW-3]. */
    final def findUtxosForAddress(
        address: Address,
        view: ChainView
    ): EmulatorResult[ChainViewError, java.util.List[Utxo]] =
        EmulatorResult.fromEither(
          findUtxosSync(UtxoQuery(UtxoSource.FromAddress(address)), view).map(toJavaUtxoList)
        )

    // ─── Helpers ────────────────────────────────────────────────────────────

    private def completedNow[A](future: Future[A]): A =
        // EmulatorBase futures are always already completed (parasitic EC, Future.successful)
        future.value.get.get

    private def unwrapQuery(result: Future[Either[UtxoQueryError, Utxos]]): java.util.List[Utxo] =
        toJavaUtxoList(queryResultOrThrow(completedNow(result)))

    private def queryResultOrThrow(result: Either[UtxoQueryError, Utxos]): Utxos = result match
        case Right(utxos) => utxos
        case Left(error)  => throw new RuntimeException(s"UTxO query failed: $error")

    private def toJavaUtxoList(utxos: Utxos): java.util.List[Utxo] =
        utxos.iterator.map(Utxo.apply).toSeq.asJava
}
