package scalus.cardano.network

import scalus.cardano.infra.CancelToken
import scalus.uplc.builtin.ByteString

import scala.concurrent.Future

/** Platform-neutral async byte channel — the sole transport-layer boundary between the multiplexer
  * and the platform (JVM NIO2, Scala.js Node `net`).
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *The platform boundary — AsyncByteChannel*.
  *
  * Concurrency contract: at most one [[readExactly]] and at most one [[write]] may be in flight at
  * a time. Reads and writes may proceed concurrently with each other (NIO2 supports this and our
  * mux sends/receives on separate logical paths), but two overlapping reads or two overlapping
  * writes on the same channel are undefined behaviour. The multiplexer enforces this by
  * construction (single reader loop, single send queue).
  */
trait AsyncByteChannel {

    /** Completes with exactly `n` bytes, or `None` if a clean EOF arrives before any byte of this
      * read has been consumed. If EOF arrives mid-read (some bytes consumed but `n` not reached),
      * fails with [[AsyncByteChannel.UnexpectedEofException]].
      *
      * The `cancel` token aborts the returned Future with [[CancelledException]] — before the
      * syscall if observed early, or on completion-handler entry if a syscall was in flight. Bytes
      * received mid-cancelled-syscall are discarded rather than delivered; subsequent reads start
      * from wherever the OS's byte stream continues. Cost: at most one implementation-internal
      * refill worth of wasted I/O per cancelled read.
      */
    def readExactly(n: Int, cancel: CancelToken): Future[Option[ByteString]]

    /** Send `bytes` to the peer, completing when every byte has been handed to the OS. `cancel`
      * aborts the call only if the channel has not yet issued its first OS write; a mid-flight
      * cancellation is ignored so we never emit a half-frame that would corrupt the peer's demux.
      * Callers that need strict abort must close the channel instead.
      */
    def write(bytes: ByteString, cancel: CancelToken): Future[Unit]

    /** Idempotent tear-down. Fails any pending read or write with
      * [[AsyncByteChannel.ChannelClosedException]].
      */
    def close(): Future[Unit]
}

object AsyncByteChannel {

    final class ChannelClosedException(message: String) extends RuntimeException(message)

    final class UnexpectedEofException(val wanted: Int, val got: Int)
        extends RuntimeException(s"EOF after $got of $wanted bytes")
}
