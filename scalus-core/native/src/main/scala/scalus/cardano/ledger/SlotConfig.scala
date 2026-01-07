package scalus.cardano.ledger

import java.time.Instant

case class SlotConfig(zeroTime: Long, zeroSlot: Long, slotLength: Long) {
    def slotToTime(slot: Long): Long = zeroTime + (slot - zeroSlot) * slotLength
    def timeToSlot(time: Long): Long = zeroSlot + ((time - zeroTime) / slotLength)
    def slotToInstant(slot: Long): Instant = Instant.ofEpochMilli(slotToTime(slot))
    def instantToSlot(instant: Instant): Long = timeToSlot(instant.toEpochMilli)
}

object SlotConfig {
    // taken from https://github.com/spacebudz/lucid/blob/main/src/plutus/time.ts

    /** Mainnet slot configuration starting at Shelley era */
    val mainnet: SlotConfig = SlotConfig(
      zeroTime = 1596059091000L,
      zeroSlot = 4492800,
      slotLength = 1000
    )

    /** Preview testnet slot configuration starting at Shelley era */
    val preview: SlotConfig = SlotConfig(
      zeroTime = 1666656000000L,
      zeroSlot = 0,
      slotLength = 1000
    )

    /** Preprod testnet slot configuration */
    val preprod: SlotConfig = SlotConfig(
      zeroTime = 1654041600000L + 1728000000L,
      zeroSlot = 86400,
      slotLength = 1000
    )

    @deprecated("Use mainnet instead", "0.14.1")
    val Mainnet: SlotConfig = mainnet

    @deprecated("Use preview instead", "0.14.1")
    val Preview: SlotConfig = preview

    @deprecated("Use preprod instead", "0.14.1")
    val Preprod: SlotConfig = preprod
}
