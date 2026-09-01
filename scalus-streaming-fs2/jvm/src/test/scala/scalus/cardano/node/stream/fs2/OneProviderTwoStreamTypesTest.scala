package scalus.cardano.node.stream.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.Emulator
import scalus.cardano.node.stream.fs2.Fs2StreamAdapter.toStream
import scalus.cardano.node.stream.{BlockchainStreaming, ChainTip, ScalusAsyncSource, StreamingEmulator}

import scala.concurrent.duration.DurationInt

/** The reason the stream type is not part of the streaming view's type.
  *
  * A single view serves both at once: application code takes fs2 streams from it while a test — or
  * a library that has no business depending on cats-effect — pulls raw sources from the very same
  * instance. With the stream type baked into the view, these would have to be two differently-typed
  * views over the same emulator, and the two could drift.
  *
  * Subscriptions are always [[ScalusAsyncSource]] and a library is one `.toStream` away, rather
  * than the view being generic in the stream type. The property this test names survives that; what
  * goes is a higher-kinded parameter on six signatures, return types that depended on which adapter
  * a file happened to import, and an API Java could not call.
  */
// JVM-only: see Fs2StreamAdapterTest.
class OneProviderTwoStreamTypesTest extends AnyFunSuite {

    test("the same provider yields both a raw source and an fs2 stream") {
        val emulator = new Emulator()
        val provider: BlockchainStreaming = emulator.streaming()

        // Same instance, different views.
        val raw: ScalusAsyncSource[ChainTip] = provider.subscribeTip()
        val streamed: Stream[IO, ChainTip] = provider.subscribeTip().toStream

        val fromRaw = raw.pull().value.flatMap(_.toOption).flatten
        val fromStream = streamed.head.compile.last.unsafeRunTimed(5.seconds).flatten

        assert(fromRaw.isDefined, "the raw source should deliver the current tip on subscribe")
        assert(fromStream.isDefined, "so should the fs2 stream")
        assert(fromRaw == fromStream, "both views read the same tip cell")

        raw.cancel()
    }
}
