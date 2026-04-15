package scalus.cardano.node.stream.fs2

import cats.effect.IO
import cats.effect.std.Dispatcher
import scalus.cardano.node.Emulator
import scalus.cardano.node.stream.StreamingEmulatorOps
import scalus.cardano.node.stream.engine.Engine

import Fs2ScalusAsyncStream.IOStream

import scala.concurrent.ExecutionContext

/** fs2 specialisation: a [[Fs2BlockchainStreamProvider]] backed by an [[Emulator]].
  *
  * Tests can construct `new Fs2StreamingEmulator(emulator)` and subscribe through the usual
  * `subscribeUtxoQuery` / `subscribeTransactionStatus` / etc. Submissions go through the emulator's
  * ledger-validated `submitSync`, and each successful submit drives a one-tx block into the engine.
  */
class Fs2StreamingEmulator(protected val emulator: Emulator)(using
    Dispatcher[IO],
    ExecutionContext
) extends Fs2BlockchainStreamProvider(
      new Engine(emulator.cardanoInfo, Some(emulator), Engine.DefaultSecurityParam)
    )
    with StreamingEmulatorOps[IO, IOStream]
