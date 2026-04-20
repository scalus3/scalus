package scalus.cardano.network.infra

import org.scalatest.RecoverMethods.recoverToExceptionIf
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken, CancelledException}
import scalus.uplc.builtin.ByteString

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/** Contract tests for [[AsyncByteChannel]], exercised against [[PipeAsyncByteChannel]]. The same
  * contract is checked against [[scalus.cardano.network.jvm.JvmAsyncByteChannel]] over loopback TCP in
  * a JVM-only suite.
  */
class AsyncByteChannelPipeSuite extends AnyFunSuite with ScalaFutures {

    // Fast-failing patience — all ops complete instantly on Pipe; only cancel-tests need to
    // wait for async completion of an onCancel listener.
    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(2, Seconds), interval = Span(10, Millis))

    private def bytes(s: String): ByteString = ByteString.fromArray(s.getBytes("UTF-8"))

    test("readExactly returns exact bytes from a single write") {
        val (a, b) = PipeAsyncByteChannel.pair()
        a.write(bytes("hello"), CancelToken.never).futureValue
        val got = b.readExactly(5, CancelToken.never).futureValue
        assert(got.exists(_.bytes sameElements bytes("hello").bytes))
    }

    test("readExactly accumulates across multiple small writes") {
        val (a, b) = PipeAsyncByteChannel.pair()
        val fut = b.readExactly(10, CancelToken.never)
        a.write(bytes("hel"), CancelToken.never).futureValue
        a.write(bytes("lo "), CancelToken.never).futureValue
        a.write(bytes("worl"), CancelToken.never).futureValue
        a.write(bytes("d"), CancelToken.never).futureValue
        val got = fut.futureValue
        assert(got.exists(_.bytes sameElements bytes("hello worl").bytes))
    }

    test("readExactly(0) returns empty bytes without consuming buffer") {
        val (a, b) = PipeAsyncByteChannel.pair()
        a.write(bytes("xyz"), CancelToken.never).futureValue
        val empty = b.readExactly(0, CancelToken.never).futureValue
        assert(empty.exists(_.bytes.length == 0))
        val rest = b.readExactly(3, CancelToken.never).futureValue
        assert(rest.exists(_.bytes sameElements bytes("xyz").bytes))
    }

    test("readExactly splits a large chunk across multiple reads") {
        val (a, b) = PipeAsyncByteChannel.pair()
        a.write(ByteString.fromArray(Array.fill[Byte](100)(42)), CancelToken.never).futureValue
        val first = b.readExactly(30, CancelToken.never).futureValue
        val second = b.readExactly(70, CancelToken.never).futureValue
        assert(first.exists(_.bytes.length == 30))
        assert(second.exists(_.bytes.length == 70))
        assert(first.get.bytes.forall(_ == 42))
        assert(second.get.bytes.forall(_ == 42))
    }

    test("clean EOF after draining returns Some then None") {
        val (a, b) = PipeAsyncByteChannel.pair()
        a.write(bytes("ab"), CancelToken.never).futureValue
        a.close().futureValue
        val got = b.readExactly(2, CancelToken.never).futureValue
        assert(got.exists(_.bytes sameElements bytes("ab").bytes))
        val eof = b.readExactly(1, CancelToken.never).futureValue
        assert(eof.isEmpty)
    }

    test("EOF mid-frame surfaces as UnexpectedEofException") {
        val (a, b) = PipeAsyncByteChannel.pair()
        a.write(bytes("abc"), CancelToken.never).futureValue
        a.close().futureValue
        val ex = recoverToExceptionIf[AsyncByteChannel.UnexpectedEofException](
          b.readExactly(5, CancelToken.never)
        ).futureValue
        assert(ex.wanted == 5 && ex.got == 3)
    }

    test("pre-read cancel fails immediately") {
        val (_, b) = PipeAsyncByteChannel.pair()
        val cancelled = CancelSource()
        cancelled.cancel()
        val _ = recoverToExceptionIf[CancelledException](
          b.readExactly(5, cancelled.token)
        ).futureValue
    }

    test("mid-wait cancel fails the pending read without consuming bytes") {
        val (a, b) = PipeAsyncByteChannel.pair()
        val ct = CancelSource()
        val fut = b.readExactly(10, ct.token)
        ct.cancel()
        val _ = recoverToExceptionIf[CancelledException](fut).futureValue
        // After the cancel, the subsequent write + read still work: nothing was consumed.
        a.write(bytes("1234567890"), CancelToken.never).futureValue
        val got = b.readExactly(10, CancelToken.never).futureValue
        assert(got.exists(_.bytes sameElements bytes("1234567890").bytes))
    }

    test("close() on one end surfaces to pending read on the other") {
        val (a, b) = PipeAsyncByteChannel.pair()
        val fut = b.readExactly(5, CancelToken.never)
        a.close().futureValue
        val got = fut.futureValue
        assert(got.isEmpty)
    }

    test("close() on an endpoint fails subsequent writes on that endpoint") {
        val (a, _) = PipeAsyncByteChannel.pair()
        a.close().futureValue
        val _ = recoverToExceptionIf[AsyncByteChannel.ChannelClosedException](
          a.write(bytes("late"), CancelToken.never)
        ).futureValue
    }

    test("large round-trip (1 MB) works through the pipe") {
        val (a, b) = PipeAsyncByteChannel.pair()
        val payload = Array.tabulate(1024 * 1024)(i => (i & 0xff).toByte)
        a.write(ByteString.unsafeFromArray(payload), CancelToken.never).futureValue
        val got = Await.result(b.readExactly(payload.length, CancelToken.never), 5.seconds)
        assert(got.exists(_.bytes sameElements payload))
    }
}
