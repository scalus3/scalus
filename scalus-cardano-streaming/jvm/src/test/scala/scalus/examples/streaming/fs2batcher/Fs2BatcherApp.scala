package scalus.examples.streaming.fs2batcher

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Dispatcher
import scalus.cardano.address.Address
import scalus.cardano.ledger.CardanoInfo
import scalus.cardano.node.UtxoSource
import scalus.cardano.node.stream.{BackupSource, BlockfrostNetwork, ChainSyncSource, StreamProviderConfig, SubscriptionOptions, UtxoEvent, UtxoEventQuery}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

/** Minimal runnable demo of the M2 streaming stack wired against a real Blockfrost backup.
  *
  * Requires `BLOCKFROST_API_KEY` and `BATCHER_ADDRESS` (a bech32 address the caller controls) in
  * the environment. Subscribes to UTxO events at that address for 30 seconds, prints each event,
  * then shuts down.
  *
  * *Not* a unit test — the synthetic-backed tests in this package cover M2 behaviour
  * deterministically. This runs against the Blockfrost preview network and therefore needs network
  * access and a Blockfrost API key.
  */
object Fs2BatcherApp extends IOApp {

    def run(args: List[String]): IO[ExitCode] = {
        val apiKey = sys.env.getOrElse("BLOCKFROST_API_KEY", "")
        val addressStr = sys.env.getOrElse("BATCHER_ADDRESS", "")
        if apiKey.isEmpty || addressStr.isEmpty then
            IO.consoleForIO.errorln(
              "Set BLOCKFROST_API_KEY and BATCHER_ADDRESS in the environment."
            ) *> IO.pure(ExitCode.Error)
        else
            Dispatcher.parallel[IO].use { d =>
                given Dispatcher[IO] = d
                given ExecutionContext = ExecutionContext.global

                val config = StreamProviderConfig(
                  cardanoInfo = CardanoInfo.preview,
                  chainSync = ChainSyncSource.Synthetic,
                  backup = BackupSource.Blockfrost(apiKey, BlockfrostNetwork.Preview)
                )
                val query = UtxoEventQuery(
                  scalus.cardano.node
                      .UtxoQuery(UtxoSource.FromAddress(Address.fromBech32(addressStr)))
                )

                for {
                    provider <- Fs2BlockchainStreamProvider.create(config)
                    _ <- IO.consoleForIO.println(s"subscribed to $addressStr, listening for 30s")
                    fiber <- provider
                        .subscribeUtxoQuery(query, SubscriptionOptions())
                        .evalMap {
                            case UtxoEvent.Created(u, tx, at) =>
                                IO.consoleForIO.println(s"Created ${u.input} by $tx at $at")
                            case UtxoEvent.Spent(u, tx, at) =>
                                IO.consoleForIO.println(s"Spent ${u.input} by $tx at $at")
                            case UtxoEvent.RolledBack(to) =>
                                IO.consoleForIO.println(s"RolledBack to $to")
                        }
                        .compile
                        .drain
                        .start
                    _ <- IO.sleep(30.seconds)
                    _ <- provider.close()
                    _ <- fiber.cancel
                } yield ExitCode.Success
            }
    }
}
