package scalus.examples.streaming.fs2batcher

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, ProtocolParams, SlotNo, Transaction, TransactionHash, Utxos}
import scalus.cardano.node.{BlockchainProvider, NetworkSubmitError, SubmitError, TransactionStatus, UtxoQuery, UtxoQueryError, UtxoSource}
import scalus.cardano.node.stream.{StartFrom, SubscriptionOptions, SyntheticEventSource, UtxoEvent, UtxoEventQuery}
import scalus.cardano.node.stream.engine.{Engine, EngineTestFixtures}
import scalus.cardano.node.stream.fs2.{Fs2BlockchainStreamProvider, Fs2ScalusAsyncStream}

import Fs2ScalusAsyncStream.IOStream
import EngineTestFixtures.*

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*

/** Verifies that snapshot-method fallbacks route to [[BackupSource.Custom]] when the engine can't
  * answer locally, and that [[BackupSource.NoBackup]] yields typed errors.
  */
class BackupDelegationSuite extends AnyFunSuite {

    private given IORuntime = IORuntime.global
    private val ci: CardanoInfo = CardanoInfo.preview
    private val awaitTimeout: FiniteDuration = 5.seconds

    /** Records every call and returns pre-programmed responses. */
    private final class StubBackup(
        val findUtxosResult: Either[UtxoQueryError, Utxos],
        val submitResult: Either[SubmitError, TransactionHash],
        val checkTxResult: TransactionStatus = TransactionStatus.NotFound
    )(using val ec: ExecutionContext)
        extends BlockchainProvider {

        var submittedTxs: List[Transaction] = Nil
        var findUtxosQueries: List[UtxoQuery] = Nil
        var checkTxCalls: List[TransactionHash] = Nil

        override def executionContext: ExecutionContext = ec
        override def cardanoInfo: CardanoInfo = ci
        override def currentSlot: Future[SlotNo] = Future.successful(100L)
        override def fetchLatestParams: Future[ProtocolParams] =
            Future.successful(ci.protocolParams)
        override def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] = {
            findUtxosQueries = query :: findUtxosQueries
            Future.successful(findUtxosResult)
        }
        override def checkTransaction(
            txHash: TransactionHash
        ): Future[TransactionStatus] = {
            checkTxCalls = txHash :: checkTxCalls
            Future.successful(checkTxResult)
        }
        override def submit(
            transaction: Transaction
        ): Future[Either[SubmitError, TransactionHash]] = {
            submittedTxs = transaction :: submittedTxs
            Future.successful(submitResult)
        }
    }

    private def withProvider[T](backup: Option[BlockchainProvider])(
        f: (
            Fs2BlockchainStreamProvider & SyntheticEventSource[IO, IOStream],
            ExecutionContext
        ) => IO[T]
    ): T =
        Dispatcher
            .parallel[IO]
            .use { d =>
                given Dispatcher[IO] = d
                given ExecutionContext = ExecutionContext.global
                val provider = new Fs2BlockchainStreamProvider(
                  new Engine(ci, backup, Engine.DefaultSecurityParam)
                ) with SyntheticEventSource[IO, IOStream]
                f(provider, summon[ExecutionContext])
            }
            .unsafeRunSync()

    test("findUtxos falls through to backup when no subscription covers the query") {
        val stubUtxos = Map(input(1, 0) -> output(addressA, 42L))
        given ExecutionContext = ExecutionContext.global
        val backup = new StubBackup(
          findUtxosResult = Right(stubUtxos),
          submitResult = Left(NetworkSubmitError.ConnectionError("unused"))
        )
        val result = withProvider(Some(backup)) { (provider, _) =>
            provider.findUtxos(UtxoQuery(UtxoSource.FromAddress(addressA)))
        }
        assert(result == Right(stubUtxos))
        assert(backup.findUtxosQueries.size == 1)
    }

    test("submit delegates to backup and records hash in pendingOwnSubmissions") {
        given ExecutionContext = ExecutionContext.global
        val hash = txHash(500)
        val backup = new StubBackup(
          findUtxosResult = Left(UtxoQueryError.NotFound(UtxoSource.FromAddress(addressA))),
          submitResult = Right(hash)
        )
        val output = withProvider(Some(backup)) { (provider, _) =>
            val dummyTx = Transaction.empty
            for {
                _ <- provider.submit(dummyTx)
                status <- provider.checkTransaction(hash)
            } yield status
        }
        assert(output == TransactionStatus.Pending)
        assert(backup.submittedTxs.size == 1)
    }

    test("NoBackup: findUtxos returns typed NotFound") {
        val result = withProvider(None) { (provider, _) =>
            provider.findUtxos(UtxoQuery(UtxoSource.FromAddress(addressA)))
        }
        result match {
            case Left(UtxoQueryError.NotFound(UtxoSource.FromAddress(addr))) =>
                assert(addr == addressA)
            case other => fail(s"expected NotFound(FromAddress), got $other")
        }
    }

    test("NoBackup: submit returns a typed ConnectionError") {
        val result = withProvider(None) { (provider, _) =>
            provider.submit(Transaction.empty)
        }
        result match {
            case Left(NetworkSubmitError.ConnectionError(msg, _)) =>
                assert(msg.contains("no backup"))
            case other => fail(s"expected ConnectionError, got $other")
        }
    }

    test("subscribeUtxoQuery with includeExistingUtxos=true seeds from backup") {
        given ExecutionContext = ExecutionContext.global
        val seedUtxos = Map(input(9, 0) -> output(addressA, 1000L))
        val backup = new StubBackup(
          findUtxosResult = Right(seedUtxos),
          submitResult = Left(NetworkSubmitError.ConnectionError("unused"))
        )
        val collected = withProvider(Some(backup)) { (provider, _) =>
            val q = UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(addressA)))
            val opts = SubscriptionOptions(
              startFrom = StartFrom.Tip,
              includeExistingUtxos = true
            )
            val stream = provider.subscribeUtxoQuery(q, opts)
            stream.take(1).compile.toList
        }
        assert(collected.size == 1)
        collected.head match {
            case UtxoEvent.Created(utxo, _, _) =>
                assert(utxo.input == input(9, 0))
            case other => fail(s"expected Created seed event, got $other")
        }
    }
}
