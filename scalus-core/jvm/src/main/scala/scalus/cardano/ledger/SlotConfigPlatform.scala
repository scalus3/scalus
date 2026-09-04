package scalus.cardano.ledger

import java.time.Instant

/** The `java.time.Instant` half of [[SlotConfig]], which only the JVM and Native platforms have.
  *
  * A platform-capability mixin, not an interop surface: these are ordinary Scala members that
  * happen to name a type JavaScript cannot provide, so this trait does *not* extend
  * `scalus.InteropApi` (that marker is for Java/JS-idiom members which would degrade the Scala API,
  * as in `EmulatorJavaApi`). The Scala.js copy of this trait is deliberately empty.
  */
private[ledger] trait SlotConfigPlatform { self: SlotConfig =>

    /** The instant at which the given slot starts. */
    def slotToInstant(slot: Long): Instant = Instant.ofEpochMilli(slotToTime(slot))

    /** The slot containing the given instant, truncating within the slot. */
    def instantToSlot(instant: Instant): Long = timeToSlot(instant.toEpochMilli)
}
