package scalus.cardano.network

/** Microsecond monotonic clock — source of the timestamp field on outgoing mux SDUs. Injectable so
  * unit tests can assert on deterministic timestamp values without wall-clock waits.
  *
  * Only the low 32 bits of [[nowMicros]] reach the wire (matching ouroboros-network's
  * `RemoteClockModel`); higher bits are harmless but unused.
  */
trait MonotonicClock {
    def nowMicros(): Long
}

object MonotonicClock {

    /** Default impl. On JVM and Scala.js (Node) `System.nanoTime / 1000` produces microseconds. */
    val system: MonotonicClock = () => System.nanoTime() / 1000L

    /** Test-only fixed clock — `nowMicros()` always returns the supplied constant. */
    def fixed(value: Long): MonotonicClock = () => value

    /** Test-only counter clock — increments by 1 microsecond per read. */
    def counting(start: Long = 0L): MonotonicClock = {
        val box = new java.util.concurrent.atomic.AtomicLong(start)
        () => box.getAndIncrement()
    }
}
