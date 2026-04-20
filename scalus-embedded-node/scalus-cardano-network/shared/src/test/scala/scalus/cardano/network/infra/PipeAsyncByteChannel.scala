package scalus.cardano.network.infra

import scalus.cardano.infra.{CancelToken, Cancellable, CancelledException}
import scalus.uplc.builtin.ByteString

import scala.collection.mutable.ArrayDeque
import scala.concurrent.{Future, Promise}

/** Test-only [[AsyncByteChannel]] loopback pair. Bytes written to endpoint A surface as reads on
  * endpoint B and vice versa, without going through a socket. Used across the shared test tree to
  * drive mux / handshake / keep-alive state-machine tests deterministically.
  *
  * Implementation: two single-producer / single-consumer pipes, one per direction. Each pipe
  * buffers bytes in an `ArrayDeque[Array[Byte]]` plus an offset into the head chunk; at most one
  * reader may be pending at a time (the single-reader contract of [[AsyncByteChannel]]).
  */
object PipeAsyncByteChannel {

    def pair(): (AsyncByteChannel, AsyncByteChannel) = {
        val aToB = new PipeBuffer
        val bToA = new PipeBuffer
        val a = new PipeAsyncByteChannel(readFrom = bToA, writeTo = aToB)
        val b = new PipeAsyncByteChannel(readFrom = aToB, writeTo = bToA)
        (a, b)
    }
}

private final class PipeAsyncByteChannel(
    readFrom: PipeBuffer,
    writeTo: PipeBuffer
) extends AsyncByteChannel {

    @volatile private var closedFlag: Boolean = false

    def readExactly(n: Int, cancel: CancelToken): Future[Option[ByteString]] = {
        require(n >= 0, "n must be non-negative")
        if closedFlag then Future.failed(closedExc())
        else readFrom.readExactly(n, cancel)
    }

    def write(bytes: ByteString, cancel: CancelToken): Future[Unit] = {
        if closedFlag then Future.failed(closedExc())
        else if cancel.isCancelled then Future.failed(CancelledException("pre-write"))
        else {
            // Best-effort atomic append: Pipe has no partial-write concept, the whole chunk
            // lands in one step.
            writeTo.append(bytes)
            Future.unit
        }
    }

    def close(): Future[Unit] = {
        closedFlag = true
        // Closing this endpoint signals EOF on both directions: further reads on the peer
        // see None once they drain, further writes on this side fail.
        readFrom.markClosed()
        writeTo.markClosed()
        Future.unit
    }

    private def closedExc(): AsyncByteChannel.ChannelClosedException =
        new AsyncByteChannel.ChannelClosedException("pipe closed")
}

/** Single-direction buffered pipe with Future-based readExactly semantics. */
private final class PipeBuffer {
    private val lock = new AnyRef

    // Buffered bytes: queue of chunks plus an offset into the head chunk.
    private val chunks = ArrayDeque.empty[Array[Byte]]
    private var offsetInHead: Int = 0
    private var totalBuffered: Int = 0

    private var closed: Boolean = false

    private var waiter: Option[PipeBuffer.Waiter] = None

    def append(bytes: ByteString): Unit = {
        val arr = bytes.bytes
        if arr.length == 0 then return
        lock.synchronized {
            if closed then return
            chunks += arr
            totalBuffered += arr.length
            tryCompleteWaiter()
        }
    }

    def markClosed(): Unit = {
        lock.synchronized {
            if closed then return
            closed = true
            tryCompleteWaiter()
        }
    }

    def readExactly(n: Int, cancel: CancelToken): Future[Option[ByteString]] = {
        if n == 0 then return Future.successful(Some(ByteString.empty))
        if cancel.isCancelled then return Future.failed(CancelledException("pre-read"))

        lock.synchronized {
            if totalBuffered >= n then Future.successful(Some(extract(n)))
            else if closed then {
                if totalBuffered == 0 then Future.successful(None)
                else {
                    val got = totalBuffered
                    discardAll()
                    Future.failed(new AsyncByteChannel.UnexpectedEofException(n, got))
                }
            } else {
                require(waiter.isEmpty, "PipeBuffer violates single-reader contract")
                val p = Promise[Option[ByteString]]()
                val cancelReg = cancel.onCancel { () =>
                    lock.synchronized {
                        if waiter.exists(_.promise eq p) then {
                            waiter = None
                            p.failure(CancelledException("mid-read"))
                        }
                    }
                }
                waiter = Some(PipeBuffer.Waiter(n, p, cancelReg))
                p.future
            }
        }
    }

    /** Assumes `lock` is held. */
    private def tryCompleteWaiter(): Unit = {
        waiter.foreach { w =>
            if totalBuffered >= w.n then {
                val bytes = extract(w.n)
                waiter = None
                w.cancelReg.cancel()
                w.promise.success(Some(bytes))
            } else if closed then {
                waiter = None
                w.cancelReg.cancel()
                if totalBuffered == 0 then w.promise.success(None)
                else {
                    val got = totalBuffered
                    discardAll()
                    w.promise.failure(new AsyncByteChannel.UnexpectedEofException(w.n, got))
                }
            }
        }
    }

    /** Assumes `lock` is held. */
    private def extract(n: Int): ByteString = {
        val out = new Array[Byte](n)
        var filled = 0
        while filled < n do {
            val head = chunks.head
            val take = math.min(n - filled, head.length - offsetInHead)
            System.arraycopy(head, offsetInHead, out, filled, take)
            filled += take
            offsetInHead += take
            if offsetInHead >= head.length then {
                val _ = chunks.removeHead()
                offsetInHead = 0
            }
        }
        totalBuffered -= n
        ByteString.unsafeFromArray(out)
    }

    /** Assumes `lock` is held. */
    private def discardAll(): Unit = {
        chunks.clear()
        offsetInHead = 0
        totalBuffered = 0
    }
}

private object PipeBuffer {
    final case class Waiter(n: Int, promise: Promise[Option[ByteString]], cancelReg: Cancellable)
}
