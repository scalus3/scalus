package scalus.cardano.ledger

import scalus.interop.TsIgnore

import java.time.Instant
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

/** Slot and epoch configuration for a Cardano network.
  *
  * Encodes the linear (post-Byron) era as an anchor point: slot `zeroSlot` starts at `zeroTime`
  * (POSIX milliseconds) and falls at the beginning of epoch `zeroEpoch`; slots are `slotLength`
  * milliseconds long and epochs are `epochLength` slots long.
  */
@JSExportTopLevel("SlotConfig")
class SlotConfig(
    /** POSIX time in milliseconds at which slot `zeroSlot` starts. */
    val zeroTime: Double,
    /** The slot this config is anchored at: the first slot of the linear (post-Byron) era. */
    val zeroSlot: Double,
    /** Slot length in milliseconds. */
    val slotLength: Double,
    /** Epoch length in slots. */
    val epochLength: Double = 432000,
    /** Number of the epoch that begins at `zeroSlot`. */
    val zeroEpoch: Double = 0
) extends js.Object {

    /** POSIX time in milliseconds at which the given slot starts. */
    def slotToTime(slot: Double): Double = zeroTime + (slot - zeroSlot) * slotLength

    /** The slot that contains the given POSIX time in milliseconds.
      *
      * The result is fractional whenever the time does not land exactly on a slot boundary, and
      * with one-second slots `timeToSlot(Date.now())` almost never does. Round it yourself before
      * you use it as a slot number, for example with `Math.floor`; `Emulator.setSlot` truncates a
      * fractional value rather than rejecting it.
      */
    def timeToSlot(time: Double): Double = zeroSlot + ((time - zeroTime) / slotLength)
    @TsIgnore
    def slotToInstant(slot: Double): Instant = Instant.ofEpochMilli(slotToTime(slot).toLong)
    @TsIgnore
    def instantToSlot(instant: Instant): Double = timeToSlot(instant.toEpochMilli.toDouble)

    /** Epoch containing the given slot. Slots before `zeroSlot` are clamped to `zeroEpoch`. */
    def epochOf(slot: Double): Double =
        if slot <= zeroSlot then zeroEpoch
        else zeroEpoch + math.floor((slot - zeroSlot) / epochLength)

    /** First slot of the given epoch (for epochs >= `zeroEpoch`). */
    def firstSlotOfEpoch(epoch: Double): Double = zeroSlot + (epoch - zeroEpoch) * epochLength
}

object SlotConfig {
    // taken from https://github.com/spacebudz/lucid/blob/main/src/plutus/time.ts

    /** Mainnet slot configuration starting at Shelley era (slot 4492800 = start of epoch 208) */
    @JSExportStatic
    val mainnet: SlotConfig = SlotConfig(
      zeroTime = 1596059091000L,
      zeroSlot = 4492800,
      slotLength = 1000,
      epochLength = 432000,
      zeroEpoch = 208
    )

    /** Preview testnet slot configuration starting at Shelley era (1-day epochs) */
    @JSExportStatic
    val preview: SlotConfig = SlotConfig(
      zeroTime = 1666656000000L,
      zeroSlot = 0,
      slotLength = 1000,
      epochLength = 86400,
      zeroEpoch = 0
    )

    /** Preprod testnet slot configuration (slot 86400 = start of epoch 4, after 4 Byron epochs) */
    @JSExportStatic
    val preprod: SlotConfig = SlotConfig(
      zeroTime = 1654041600000L + 1728000000L,
      zeroSlot = 86400,
      slotLength = 1000,
      epochLength = 432000,
      zeroEpoch = 4
    )
}
