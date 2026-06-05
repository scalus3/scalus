package scalus.cardano.ledger

import java.time.Instant

/** Slot and epoch configuration for a Cardano network.
  *
  * Encodes the linear (post-Byron) era as an anchor point: slot `zeroSlot` starts at `zeroTime`
  * (POSIX milliseconds) and falls at the beginning of epoch `zeroEpoch`; slots are `slotLength`
  * milliseconds long and epochs are `epochLength` slots long.
  */
case class SlotConfig(
    zeroTime: Long,
    zeroSlot: Long,
    slotLength: Long,
    epochLength: Long = SlotConfig.DefaultEpochLength,
    zeroEpoch: Long = 0
) {
    def this(zeroTime: Long, zeroSlot: Long, slotLength: Long) =
        this(zeroTime, zeroSlot, slotLength, SlotConfig.DefaultEpochLength, 0)

    def slotToTime(slot: Long): Long = zeroTime + (slot - zeroSlot) * slotLength
    def timeToSlot(time: Long): Long = zeroSlot + ((time - zeroTime) / slotLength)
    def slotToInstant(slot: Long): Instant = Instant.ofEpochMilli(slotToTime(slot))
    def instantToSlot(instant: Instant): Long = timeToSlot(instant.toEpochMilli)

    /** Epoch containing the given slot. Slots before `zeroSlot` are clamped to `zeroEpoch`. */
    def epochOf(slot: Long): Long =
        if slot <= zeroSlot then zeroEpoch
        else zeroEpoch + (slot - zeroSlot) / epochLength

    /** First slot of the given epoch (for epochs >= `zeroEpoch`). */
    def firstSlotOfEpoch(epoch: Long): Long = zeroSlot + (epoch - zeroEpoch) * epochLength
}

object SlotConfig {
    // taken from https://github.com/spacebudz/lucid/blob/main/src/plutus/time.ts

    /** Shelley-era epoch length (slots per epoch) on mainnet and preprod. */
    val DefaultEpochLength: Long = 432_000

    def apply(zeroTime: Long, zeroSlot: Long, slotLength: Long): SlotConfig =
        new SlotConfig(zeroTime, zeroSlot, slotLength)

    /** Mainnet slot configuration starting at Shelley era (slot 4492800 = start of epoch 208) */
    val mainnet: SlotConfig = SlotConfig(
      zeroTime = 1596059091000L,
      zeroSlot = 4492800,
      slotLength = 1000,
      epochLength = DefaultEpochLength,
      zeroEpoch = 208
    )

    /** Preview testnet slot configuration starting at Shelley era (1-day epochs) */
    val preview: SlotConfig = SlotConfig(
      zeroTime = 1666656000000L,
      zeroSlot = 0,
      slotLength = 1000,
      epochLength = 86_400,
      zeroEpoch = 0
    )

    /** Preprod testnet slot configuration (slot 86400 = start of epoch 4, after 4 Byron epochs) */
    val preprod: SlotConfig = SlotConfig(
      zeroTime = 1654041600000L + 1728000000L,
      zeroSlot = 86400,
      slotLength = 1000,
      epochLength = DefaultEpochLength,
      zeroEpoch = 4
    )
}
