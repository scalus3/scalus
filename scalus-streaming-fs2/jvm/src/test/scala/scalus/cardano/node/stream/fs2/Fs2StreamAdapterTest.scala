package scalus.cardano.node.stream.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.infra.ScalusBufferOverflowException
import scalus.cardano.node.stream.ScalusAsyncStreamAdapter
import scalus.cardano.node.stream.fs2.Fs2StreamAdapter.fs2Adapter
import scalus.cardano.node.stream.internal.Mailbox

import scala.concurrent.duration.DurationInt

/** The adapter's own responsibilities: bridging pull to `Stream`, terminating, and releasing the
  * subscription. Provider behaviour is the conformance suite's job, not this file's.
  */
// JVM-only: unsafeRunTimed is defined in cats-effect's JVM IOPlatform, and running an fs2 stream
// synchronously is the whole point of these assertions.
class Fs2StreamAdapterTest extends AnyFunSuite {

    private val adapter = summon[ScalusAsyncStreamAdapter[Fs2StreamAdapter.IOStream]]

    test("values reach the stream in order and a close terminates it") {
        val mailbox = Mailbox.delta[Int]()
        mailbox.offer(1)
        mailbox.offer(2)
        mailbox.close()
        val result = adapter.fromSource(mailbox).compile.toList.unsafeRunTimed(5.seconds)
        assert(result.contains(List(1, 2)))
    }

    test("a producer failure surfaces as a stream error, not a silent end") {
        val mailbox = Mailbox.delta[Int](maxSize = 1)
        mailbox.offer(1)
        mailbox.offer(2) // overflows
        val outcome = adapter
            .fromSource(mailbox)
            .compile
            .toList
            .attempt
            .unsafeRunTimed(5.seconds)
            .flatMap(_.left.toOption)
        assert(outcome.exists(_.isInstanceOf[ScalusBufferOverflowException]))
    }

    test("finishing with the stream cancels the subscription") {
        var cancelled = false
        val mailbox = Mailbox.delta[Int](onCancel = () => cancelled = true)
        mailbox.offer(1)
        mailbox.offer(2)
        adapter.fromSource(mailbox).take(1).compile.toList.unsafeRunTimed(5.seconds)
        assert(cancelled, "an unconsumed subscription must not outlive the stream that held it")
    }

    test("the resource form releases even when the stream is never run") {
        var cancelled = false
        val mailbox = Mailbox.delta[Int](onCancel = () => cancelled = true)
        Fs2StreamAdapter
            .subscribe(IO.pure(mailbox))
            .use(_ => IO.unit)
            .unsafeRunTimed(5.seconds)
        assert(
          cancelled,
          "eager registration means the caller owns release; the resource must discharge it"
        )
    }
}
