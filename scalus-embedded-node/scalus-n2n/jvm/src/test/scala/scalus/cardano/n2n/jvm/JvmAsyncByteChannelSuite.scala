package scalus.cardano.n2n.jvm

import org.scalatest.RecoverMethods.recoverToExceptionIf
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.n2n.{AsyncByteChannel, CancelSource, CancelToken, CancelledException}
import scalus.uplc.builtin.ByteString

import java.net.InetSocketAddress
import java.nio.channels.{AsynchronousServerSocketChannel, AsynchronousSocketChannel}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}

/** Real-socket smoke tests over loopback TCP. Covers the NIO2-specific paths of
  * [[JvmAsyncByteChannel]] — completion-handler wiring, direct-ByteBuffer refill, partial writes —
  * which the pure-Scala pipe tests cannot exercise.
  */
class JvmAsyncByteChannelSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(5, Seconds), interval = Span(10, Millis))

    private def bytes(s: String): ByteString = ByteString.fromArray(s.getBytes("UTF-8"))

    /** Opens a listener on 127.0.0.1:0 and yields `(clientChannel, serverSideChannel)` once the
      * accept completes. Both channels are ready to read/write.
      */
    private def loopbackPair(): (JvmAsyncByteChannel, JvmAsyncByteChannel) = {
        val server =
            AsynchronousServerSocketChannel.open().bind(new InetSocketAddress("127.0.0.1", 0))
        val port = server.getLocalAddress.asInstanceOf[InetSocketAddress].getPort
        val acceptPromise = Promise[AsynchronousSocketChannel]()
        server.accept(
          null,
          JvmAsyncByteChannel.completionHandler[AsynchronousSocketChannel](
            onSuccess = ch => acceptPromise.success(ch),
            onFailure = t => acceptPromise.failure(t)
          )
        )
        val client = Await.result(JvmAsyncByteChannel.connect("127.0.0.1", port), 5.seconds)
        val serverCh = Await.result(acceptPromise.future, 5.seconds)
        server.close()
        (client, JvmAsyncByteChannel.fromChannel(serverCh))
    }

    test("round-trip of a small payload") {
        val (a, b) = loopbackPair()
        try {
            a.write(bytes("hello"), CancelToken.never).futureValue
            val got = b.readExactly(5, CancelToken.never).futureValue
            assert(got.exists(_.bytes sameElements bytes("hello").bytes))
        } finally {
            a.close().futureValue
            b.close().futureValue
        }
    }

    test("readExactly accumulates across multiple OS-level chunks") {
        val (a, b) = loopbackPair()
        try {
            // 4 writes, 3 bytes each — NIO may deliver them as one chunk or several;
            // readExactly(12) must produce the concatenation either way.
            val fut = b.readExactly(12, CancelToken.never)
            a.write(bytes("aaa"), CancelToken.never).futureValue
            a.write(bytes("bbb"), CancelToken.never).futureValue
            a.write(bytes("ccc"), CancelToken.never).futureValue
            a.write(bytes("ddd"), CancelToken.never).futureValue
            val got = fut.futureValue
            assert(got.exists(_.bytes sameElements bytes("aaabbbcccddd").bytes))
        } finally {
            a.close().futureValue
            b.close().futureValue
        }
    }

    test("clean EOF when peer closes after draining") {
        val (a, b) = loopbackPair()
        try {
            a.write(bytes("ab"), CancelToken.never).futureValue
            a.close().futureValue
            val got = b.readExactly(2, CancelToken.never).futureValue
            assert(got.exists(_.bytes sameElements bytes("ab").bytes))
            val eof = b.readExactly(1, CancelToken.never).futureValue
            assert(eof.isEmpty)
        } finally b.close().futureValue
    }

    test("pre-read cancel fails without a syscall") {
        val (a, b) = loopbackPair()
        try {
            val ct = CancelSource()
            ct.cancel()
            val _ = recoverToExceptionIf[CancelledException](b.readExactly(5, ct.token)).futureValue
        } finally {
            a.close().futureValue
            b.close().futureValue
        }
    }

    test("large (1 MB) payload round-trips intact") {
        val (a, b) = loopbackPair()
        try {
            val payload = Array.tabulate(1024 * 1024)(i => (i & 0xff).toByte)
            val writeFut = a.write(ByteString.unsafeFromArray(payload), CancelToken.never)
            val readFut = b.readExactly(payload.length, CancelToken.never)
            Await.result(writeFut, 10.seconds)
            val got = Await.result(readFut, 10.seconds)
            assert(got.exists(_.bytes sameElements payload))
        } finally {
            a.close().futureValue
            b.close().futureValue
        }
    }

    test("operations on a closed channel fail fast") {
        val (a, b) = loopbackPair()
        a.close().futureValue
        val _ = recoverToExceptionIf[AsyncByteChannel.ChannelClosedException](
          a.readExactly(1, CancelToken.never)
        ).futureValue
        val _ = recoverToExceptionIf[AsyncByteChannel.ChannelClosedException](
          a.write(bytes("x"), CancelToken.never)
        ).futureValue
        b.close().futureValue
    }

    test("close is idempotent (no error on second call)") {
        val (a, b) = loopbackPair()
        a.close().futureValue
        a.close().futureValue
        b.close().futureValue
    }
}
