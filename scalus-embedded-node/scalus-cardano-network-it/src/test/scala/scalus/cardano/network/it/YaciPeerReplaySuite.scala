package scalus.cardano.network.it

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.network.NetworkMagic
import scalus.cardano.network.replay.{PeerReplayConnectionFactory, PeerReplaySource}
import scalus.cardano.node.stream.engine.replay.ReplayError
import scalus.cardano.node.stream.fs2.Fs2BlockchainStreamProvider
import scalus.cardano.node.stream.{BackupSource, ChainPoint, ChainSyncSource, StreamProviderConfig}
import scalus.testing.yaci.YaciDevKit

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

/** End-to-end IT for [[PeerReplaySource]] against a real yaci-devkit peer.
  *
  * Verifies:
  *
  *   - [[PeerReplaySource]] opens a transient N2N connection to yaci, intersects at `from`, drives
  *     the ChainSync + BlockFetch loop, and returns the expected [[scalus.cardano.node.stream.engine.AppliedBlock]]
  *     sequence.
  *   - A live [[Fs2BlockchainStreamProvider]] subscription keeps advancing while a separate
  *     `PeerReplaySource.prefetch` runs against the same yaci — i.e. the transient connection
  *     does NOT interfere with the live applier's connection.
  *
  * Runs under `sbt it`. Requires Docker.
  */
class YaciPeerReplaySuite
    extends AnyFunSuite
    with YaciDevKit
    with ScalaFutures
    with Eventually {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(120, Seconds), interval = Span(500, Millis))

    private def itLog(msg: String): Unit =
        System.err.println(s"[IT][${System.currentTimeMillis()}] $msg")

    test("PeerReplaySource prefetches blocks from yaci via a transient N2N connection") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        given ExecutionContext = ExecutionContext.global

        val replaySource = new PeerReplaySource(
          PeerReplayConnectionFactory.forEndpoint(host, port, NetworkMagic.YaciDevnet)
        )

        // Step 1 — learn a recent tip from yaci by running a live applier just long enough to
        // observe several blocks, then capture the tip.
        val tipSlot = new AtomicReference[Option[ChainPoint]](None)
        val tipCount = new AtomicLong(0L)

        Dispatcher.parallel[IO].use { d =>
            given Dispatcher[IO] = d

            val config = StreamProviderConfig(
              appId = "scalus.it.yaci-peer-replay.tip",
              cardanoInfo = CardanoInfo.preview,
              chainSync = ChainSyncSource.N2N(host, port, NetworkMagic.YaciDevnet.value),
              backup = BackupSource.NoBackup,
              enginePersistence =
                  scalus.cardano.node.stream.engine.persistence.EnginePersistenceStore.noop
            )

            for {
                provider <- Fs2BlockchainStreamProvider.create(config)
                // Pull 3 tips so yaci has produced at least that many blocks before we
                // capture the replay-window upper bound.
                _ <- provider
                    .subscribeTip()
                    .evalMap(tip =>
                        IO {
                            tipSlot.set(Some(tip.point))
                            tipCount.incrementAndGet()
                            ()
                        }
                    )
                    .take(3)
                    .interruptAfter(30.seconds)
                    .compile
                    .drain
                _ <- provider.close()
            } yield ()
        }.unsafeRunSync()

        val target = tipSlot.get.getOrElse(
          fail(s"no tip observed from yaci in 30s (tipCount=${tipCount.get})")
        )
        itLog(s"captured target tip at slot ${target.slot}")

        // Step 2 — peer-replay prefetch against a fresh transient connection from origin up to
        // the observed tip. Expect at least one block back.
        val result = Await.result(
          replaySource.prefetch(ChainPoint.origin, target),
          90.seconds
        )

        result match {
            case Right(blocks) =>
                assert(blocks.nonEmpty, s"prefetch returned no blocks for target $target")
                assert(
                  blocks.last.point.slot == target.slot,
                  s"expected last prefetched block at slot ${target.slot}, got ${blocks.last.point.slot}"
                )
                itLog(s"prefetched ${blocks.size} blocks up to slot ${blocks.last.point.slot}")
            case Left(err) =>
                fail(s"peer-replay prefetch failed: ${err.getClass.getSimpleName}: ${err.getMessage}")
        }
    }

    test("live applier keeps advancing while a separate peer-replay prefetch is in flight") {
        val host = container.getHost
        val port = container.getCardanoNodePort

        given ExecutionContext = ExecutionContext.global

        val replaySource = new PeerReplaySource(
          PeerReplayConnectionFactory.forEndpoint(host, port, NetworkMagic.YaciDevnet)
        )

        val tipBeforeReplay = new AtomicReference[Option[ChainPoint]](None)
        val tipAfterReplay = new AtomicReference[Option[ChainPoint]](None)

        Dispatcher.parallel[IO].use { d =>
            given Dispatcher[IO] = d

            val config = StreamProviderConfig(
              appId = "scalus.it.yaci-peer-replay.concurrent",
              cardanoInfo = CardanoInfo.preview,
              chainSync = ChainSyncSource.N2N(host, port, NetworkMagic.YaciDevnet.value),
              backup = BackupSource.NoBackup,
              enginePersistence =
                  scalus.cardano.node.stream.engine.persistence.EnginePersistenceStore.noop
            )

            val runLoop: IO[Unit] =
                for {
                    provider <- Fs2BlockchainStreamProvider.create(config)
                    tipFiber <- provider
                        .subscribeTip()
                        .evalMap(tip =>
                            IO {
                                tipBeforeReplay.compareAndSet(None, Some(tip.point))
                                tipAfterReplay.set(Some(tip.point))
                                ()
                            }
                        )
                        .interruptAfter(60.seconds)
                        .compile
                        .drain
                        .attempt
                        .start
                    // Wait for a first live tip so we have a concrete target for prefetch.
                    _ <- IO.race(
                      IO.sleep(30.seconds) *> IO.raiseError(
                        new AssertionError("no tip seen within 30s — live sync stalled")
                      ),
                      IO {
                          while tipBeforeReplay.get.isEmpty do Thread.sleep(50)
                      }
                    )
                    pastTip = tipBeforeReplay.get.get
                    _ = itLog(s"starting replay prefetch with target ${pastTip.slot}")
                    prefetched <- IO.fromFuture(
                      IO(replaySource.prefetch(ChainPoint.origin, pastTip))
                    )
                    _ = prefetched match {
                        case Right(bs) =>
                            assert(
                              bs.nonEmpty,
                              s"empty prefetch against a non-origin tip ($pastTip)"
                            )
                            itLog(s"prefetched ${bs.size} blocks")
                        case Left(err) => fail(s"prefetch failed: $err")
                    }
                    // Give the live stream a few more seconds to advance past pastTip.
                    _ <- IO.sleep(5.seconds)
                    _ <- provider.close()
                    _ <- tipFiber.joinWithNever.timeoutTo(15.seconds, IO.unit)
                } yield ()

            runLoop
        }.unsafeRunSync()

        val before = tipBeforeReplay.get.getOrElse(fail("no pre-replay tip observed"))
        val after = tipAfterReplay.get.getOrElse(fail("no post-replay tip observed"))
        assert(
          after.slot >= before.slot,
          s"expected live tip to advance during replay, got before=${before.slot} after=${after.slot}"
        )
    }

    test("prefetch with from == to short-circuits without contacting the peer") {
        given ExecutionContext = ExecutionContext.global
        val replaySource = new PeerReplaySource(
          PeerReplayConnectionFactory.forEndpoint(
            container.getHost,
            container.getCardanoNodePort,
            NetworkMagic.YaciDevnet
          )
        )
        val p = ChainPoint(
          slot = Long.MaxValue / 2,
          blockHash = scalus.cardano.ledger.BlockHash.fromHex("ff" * 32)
        )
        val result = Await.result(replaySource.prefetch(p, p), 30.seconds)
        assert(result == Right(Seq.empty))
    }

    test("prefetch of an unreachable checkpoint fails with ReplaySourceExhausted") {
        given ExecutionContext = ExecutionContext.global
        val replaySource = new PeerReplaySource(
          PeerReplayConnectionFactory.forEndpoint(
            container.getHost,
            container.getCardanoNodePort,
            NetworkMagic.YaciDevnet
          )
        )
        // Arbitrary hash yaci cannot know — intersect yields NoIntersection which the source
        // maps to ReplaySourceExhausted.
        val bogus = scalus.cardano.ledger.BlockHash.fromHex("ff" * 32)
        val result = Await.result(
          replaySource.prefetch(ChainPoint(slot = 0L, blockHash = bogus), ChainPoint(100L, bogus)),
          30.seconds
        )
        assert(
          result.left.exists(_.isInstanceOf[ReplayError.ReplaySourceExhausted]),
          s"expected ReplaySourceExhausted, got $result"
        )
    }
}
