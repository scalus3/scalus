package scalus.cardano.node.stream.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.Emulator
import scalus.cardano.node.stream.fs2.Fs2StreamAdapter.fs2Adapter
import scalus.cardano.node.stream.{BlockchainStreamProvider, ChainTip, ScalusAsyncSource, StreamingEmulator}

import scala.concurrent.duration.DurationInt

/** The reason the stream type is a per-call choice rather than part of the provider's type.
  *
  * A single provider value serves both at once: application code takes fs2 streams from it while a
  * test — or a library that has no business depending on cats-effect — pulls raw sources from the
  * very same instance. With the stream type baked into the provider, these would have to be two
  * differently-typed providers over the same emulator, and the two views could drift.
  */
// JVM-only: see Fs2StreamAdapterTest.
class OneProviderTwoStreamTypesTest extends AnyFunSuite {

    test("the same provider yields both a raw source and an fs2 stream") {
        val emulator = new Emulator()
        val provider: BlockchainStreamProvider = new StreamingEmulator(emulator)

        // C is inferred from the expected type on each line — same instance, different views.
        val raw: ScalusAsyncSource[ChainTip] = provider.subscribeTip()
        val streamed: Stream[IO, ChainTip] = provider.subscribeTip()

        val fromRaw = raw.pull().value.flatMap(_.toOption).flatten
        val fromStream = streamed.head.compile.last.unsafeRunTimed(5.seconds).flatten

        assert(fromRaw.isDefined, "the raw source should deliver the current tip on subscribe")
        assert(fromStream.isDefined, "so should the fs2 stream")
        assert(fromRaw == fromStream, "both views read the same tip cell")

        raw.cancel()
    }
}
