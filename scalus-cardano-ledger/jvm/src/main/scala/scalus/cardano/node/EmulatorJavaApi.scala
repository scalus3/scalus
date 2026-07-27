package scalus.cardano.node

import scalus.InteropApi
import scalus.uplc.DebugScript
import scalus.uplc.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

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
            .map(SubmitResult.fromEither)(executionContext)
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
            .map(result => toJavaUtxoList(queryResultOrThrow(result)))(executionContext)
            .asJava
            .toCompletableFuture

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
