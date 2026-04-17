package scalus.cardano.n2n.jvm

import scalus.cardano.infra.{CancelToken, CancelledException}
import scalus.cardano.n2n.AsyncByteChannel
import scalus.uplc.builtin.ByteString

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousSocketChannel, CompletionHandler}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

/** JVM [[AsyncByteChannel]] over NIO2 `AsynchronousSocketChannel`. Holds a single reusable direct
  * [[ByteBuffer]] for inbound refill — callers get `ByteString`s carved out of that buffer so we
  * avoid allocating a fresh Java array per OS-level chunk. The refill buffer is safe under the
  * single-reader contract: only the `readExactly` code path touches it, and that path is serialised
  * by caller discipline.
  *
  * Constructed via [[JvmAsyncByteChannel.connect]] for live TCP connections or
  * [[JvmAsyncByteChannel.fromChannel]] for server-side accepts / tests.
  */
final class JvmAsyncByteChannel private (
    ch: AsynchronousSocketChannel,
    refillSize: Int
)(using ec: ExecutionContext)
    extends AsyncByteChannel {

    // Reusable inbound buffer. Invariant: on entry/exit of `readExactly` and its helpers, the
    // buffer is in *read mode* — `position..limit` covers buffered-but-undelivered bytes.
    // Initialised as empty (position == limit == 0).
    private val refill = ByteBuffer.allocateDirect(refillSize)
    private val _ = { refill.position(0); refill.limit(0) }

    @volatile private var closedFlag: Boolean = false

    def readExactly(n: Int, cancel: CancelToken): Future[Option[ByteString]] = {
        require(n >= 0, "n must be non-negative")
        if n == 0 then Future.successful(Some(ByteString.empty))
        else if closedFlag then Future.failed(closedExc())
        else if cancel.isCancelled then Future.failed(CancelledException("pre-read"))
        else {
            val out = new Array[Byte](n)
            readLoop(out, 0, cancel)
        }
    }

    private def readLoop(
        out: Array[Byte],
        filled: Int,
        cancel: CancelToken
    ): Future[Option[ByteString]] = {
        if cancel.isCancelled then
            Future.failed(CancelledException(s"after $filled/${out.length} bytes"))
        else if filled == out.length then Future.successful(Some(ByteString.unsafeFromArray(out)))
        else if refill.hasRemaining then {
            val take = math.min(out.length - filled, refill.remaining())
            refill.get(out, filled, take)
            readLoop(out, filled + take, cancel)
        } else
            refillFromSocket(cancel).flatMap {
                case 0 if filled == 0 => Future.successful(None)
                case 0 =>
                    Future.failed(new AsyncByteChannel.UnexpectedEofException(out.length, filled))
                case _ => readLoop(out, filled, cancel)
            }
    }

    private def refillFromSocket(cancel: CancelToken): Future[Int] = {
        val p = Promise[Int]()
        refill.clear()
        ch.read(
          refill,
          null,
          JvmAsyncByteChannel.completionHandler[Integer](
            onSuccess = r => {
                refill.flip()
                if cancel.isCancelled then {
                    // Drop whatever just arrived — the caller has lost interest.
                    refill.position(refill.limit())
                    p.failure(CancelledException("mid-syscall"))
                } else p.success(if r < 0 then 0 else r.intValue)
            },
            onFailure = t => p.failure(if closedFlag then closedExc() else t)
          )
        )
        p.future
    }

    def write(bytes: ByteString, cancel: CancelToken): Future[Unit] = {
        if closedFlag then Future.failed(closedExc())
        else if cancel.isCancelled then Future.failed(CancelledException("pre-write"))
        else if bytes.bytes.length == 0 then Future.unit
        else {
            val buf = ByteBuffer.wrap(bytes.bytes)
            val p = Promise[Unit]()
            writeLoop(buf, p)
            p.future
        }
    }

    private def writeLoop(buf: ByteBuffer, p: Promise[Unit]): Unit = {
        ch.write(
          buf,
          null,
          JvmAsyncByteChannel.completionHandler[Integer](
            onSuccess = _ => {
                if buf.hasRemaining then writeLoop(buf, p) // NIO may deliver a partial write
                else p.success(())
            },
            onFailure = t => p.failure(if closedFlag then closedExc() else t)
          )
        )
    }

    def close(): Future[Unit] = {
        if !closedFlag then {
            closedFlag = true
            try {
                ch.close()
                Future.unit
            } catch {
                // Failure propagates on the returned Future — the caller who invoked
                // close() is the observer.
                case NonFatal(t) => Future.failed(t)
            }
        } else Future.unit
    }

    private def closedExc(): AsyncByteChannel.ChannelClosedException =
        new AsyncByteChannel.ChannelClosedException("channel closed")
}

object JvmAsyncByteChannel {

    /** Default refill-buffer size: 64 KiB. Sized to amortise NIO syscall overhead for a mux reading
      * 8-byte headers interleaved with multi-KiB SDU payloads.
      */
    val DefaultRefillSize: Int = 64 * 1024

    /** Open a TCP connection and wrap it. The returned Future completes once the connect call
      * succeeds; subsequent reads/writes see the live channel.
      */
    def connect(
        host: String,
        port: Int,
        refillSize: Int = DefaultRefillSize
    )(using ec: ExecutionContext): Future[JvmAsyncByteChannel] = {
        val ch = AsynchronousSocketChannel.open()
        val p = Promise[JvmAsyncByteChannel]()
        ch.connect(
          new InetSocketAddress(host, port),
          null,
          completionHandler[Void](
            onSuccess = _ => p.success(new JvmAsyncByteChannel(ch, refillSize)),
            onFailure = t => {
                // Attach any cleanup failure as suppressed so the caller sees both.
                try ch.close()
                catch { case NonFatal(cleanup) => t.addSuppressed(cleanup) }
                p.failure(t)
            }
          )
        )
        p.future
    }

    /** Shared adapter from the NIO `CompletionHandler` contract to callback functions — eliminates
      * the anonymous-class boilerplate that otherwise repeats at every `ch.read`, `ch.write`, and
      * `ch.connect` call site.
      */
    private[jvm] def completionHandler[T](
        onSuccess: T => Unit,
        onFailure: Throwable => Unit
    ): CompletionHandler[T, Null] =
        new CompletionHandler[T, Null] {
            def completed(result: T, a: Null): Unit = onSuccess(result)
            def failed(t: Throwable, a: Null): Unit = onFailure(t)
        }

    /** Wrap an already-connected `AsynchronousSocketChannel`. Used by server-side accepts and by
      * test fixtures that supply their own channel.
      */
    def fromChannel(
        ch: AsynchronousSocketChannel,
        refillSize: Int = DefaultRefillSize
    )(using ExecutionContext): JvmAsyncByteChannel =
        new JvmAsyncByteChannel(ch, refillSize)
}
