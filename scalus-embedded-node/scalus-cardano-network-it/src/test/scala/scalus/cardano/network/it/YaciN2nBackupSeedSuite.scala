package scalus.cardano.network.it

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.address.Network
import scalus.cardano.ledger.{
    CardanoInfo,
    ProtocolParams,
    SlotNo,
    Transaction,
    TransactionHash,
    TransactionInput,
    TransactionOutput,
    Value
}
import scalus.cardano.network.NetworkMagic
import scalus.cardano.node.stream.fs2.Fs2BlockchainStreamProvider
import scalus.cardano.node.stream.{
    BackupSource,
    ChainSyncSource,
    StreamProviderConfig,
    SubscriptionOptions,
    UtxoEvent,
    UtxoEventQuery
}
import scalus.cardano.node.{
    BlockchainProvider,
    NetworkSubmitError,
    SubmitError,
    UtxoQuery,
    UtxoQueryError,
    UtxoSource
}
import scalus.testing.kit.Party
import scalus.testing.yaci.YaciDevKit

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

/** End-to-end yaci IT verifying that [[BackupSource.Custom]] seeding composes with a live N2N
  * chain-sync connection. A UTxO subscription with `includeExistingUtxos = true` asks the backup
  * for matching UTxOs at subscription time; those synthetic `UtxoEvent.Created` events must be
  * delivered before any live delta starts flowing (the engine's
  * [[Engine#registerUtxoSubscription]] contract).
  *
  * The chain-sync source is real (yaci), but the fixture address is one that yaci won't have any
  * history for — so the only matching events the subscriber sees are the ones the backup replayed.
  * This isolates the seed path from live-delta noise while still exercising the full wiring.
  */
class YaciN2nBackupSeedSuite
    extends AnyFunSuite
    with YaciDevKit
    with ScalaFutures
    with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(120, Seconds), interval = Span(500, Millis))

    test("UTxO subscription seed via BackupSource.Custom delivers the fixture UTxO") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        given ExecutionContext = ExecutionContext.global

        // Fixture: a UTxO owned by Party.Alice on the testnet. Using a Party address keeps the
        // fixture independent of yaci's faucet and unambiguously distinct from any UTxO yaci
        // would emit through chain-sync (Alice's key is from the scalus-testkit mnemonic, which
        // yaci doesn't know about).
        val testAddress = Party.Alice.account.baseAddress(Network.Testnet)
        val testTxHash = TransactionHash.fromHex("a" * 64)
        val testInput = TransactionInput(testTxHash, 0)
        val testOutput: TransactionOutput =
            TransactionOutput.Shelley(testAddress, Value.ada(5_000_000L))

        val seedUtxos: Map[TransactionInput, TransactionOutput] = Map(testInput -> testOutput)

        // Minimal custom backup — enough to satisfy the engine's seeding path. `findUtxos` is
        // the only method the engine actually calls during seed; `submit` is never invoked
        // because this test doesn't exercise the submit path, so it fails loudly if called so
        // a future refactor doesn't silently hit a swallowed stub.
        val customBackup: BlockchainProvider = new BlockchainProvider {
            def executionContext: ExecutionContext = ExecutionContext.global
            def cardanoInfo: CardanoInfo = CardanoInfo.preview
            def fetchLatestParams: Future[ProtocolParams] =
                Future.successful(CardanoInfo.preview.protocolParams)
            def currentSlot: Future[SlotNo] = Future.successful(0L)
            def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Map[
              TransactionInput,
              TransactionOutput
            ]]] =
                Future.successful(Right(seedUtxos))
            def submit(
                transaction: Transaction
            ): Future[Either[SubmitError, TransactionHash]] =
                Future.successful(
                  Left(NetworkSubmitError.ConnectionError("submit not supported in seed-IT stub"))
                )
        }

        val config = StreamProviderConfig(
          cardanoInfo = CardanoInfo.preview,
          chainSync = ChainSyncSource.N2N(host, port, NetworkMagic.YaciDevnet.value),
          backup = BackupSource.Custom(customBackup)
        )

        val firstEvent = new AtomicReference[Option[UtxoEvent]](None)

        Dispatcher.parallel[IO].use { d =>
            given Dispatcher[IO] = d

            for {
                provider <- Fs2BlockchainStreamProvider.create(config)
                _ <- provider
                    .subscribeUtxoQuery(
                      UtxoEventQuery(UtxoQuery(UtxoSource.FromAddress(testAddress))),
                      SubscriptionOptions() // includeExistingUtxos=true by default
                    )
                    .evalMap(ev => IO { firstEvent.compareAndSet(None, Some(ev)); () })
                    .take(1) // only the seed event
                    .interruptAfter(15.seconds)
                    .compile
                    .drain
                _ <- provider.close()
            } yield firstEvent.get match {
                case Some(UtxoEvent.Created(utxo, _, _)) =>
                    assert(
                      utxo.input == testInput,
                      s"seed input mismatch: got ${utxo.input}, expected $testInput"
                    )
                    assert(
                      utxo.output.address == testAddress,
                      s"seed address mismatch: got ${utxo.output.address}, expected $testAddress"
                    )
                case Some(other) =>
                    fail(s"expected UtxoEvent.Created seed, got $other")
                case None =>
                    fail("no seed event delivered within 15s — backup seeding may not be wired")
            }
        }.unsafeRunSync()
    }
}
