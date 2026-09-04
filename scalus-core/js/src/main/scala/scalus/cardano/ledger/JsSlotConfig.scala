package scalus.cardano.ledger

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

/** Slot and epoch configuration for a Cardano network.
  *
  * Encodes the linear (post-Byron) era as an anchor point: slot `zeroSlot` starts at `zeroTime`
  * (POSIX milliseconds) and falls at the beginning of epoch `zeroEpoch`; slots are `slotLength`
  * milliseconds long and epochs are `epochLength` slots long.
  */
// Implementation notes, deliberately NOT scaladoc: this file's scaladoc is published to npm as
// scalus.d.ts, where `JsSlotConfig` and Scala-side type names mean nothing to the reader.
//
// WHY A HANDLE. `scalus.cardano.ledger.SlotConfig` is a shared `Long`-based case class, the one
// type every platform's Scala code computes with. `Long` has no JavaScript representation
// (Scala.js compiles it to a `RuntimeLong` object, not a `number`), so the exported class is this
// separate handle, taking and returning `number` exactly as the previous JS-only `SlotConfig` did.
//
// WHY `val` FIELDS AND `Double` ARITHMETIC. Both preserve the published contract byte for byte.
// The five fields stay constructor `val`s, so they remain own properties of the instance and
// `JSON.stringify(SlotConfig.mainnet)` keeps working - no `toObject()` is needed here, unlike the
// handles whose state is a ledger object. The four methods keep computing in `Double`, so
// `timeToSlot` keeps returning the fractional slot its doc promises; delegating to the shared
// `Long` method would silently start truncating for every npm consumer.
//
// ONE REPRESENTATION. `config` is derived from the five fields once, at construction, and the two
// cannot disagree: both are immutable and `wrap` builds the fields from the very config it is
// given. Every value a JavaScript caller can express fits a `Double` exactly, so the round trip
// through `wrap`/`underlying` is lossless for anything this API can carry.
@JSExportTopLevel("SlotConfig")
class JsSlotConfig(
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

    private val config: SlotConfig = SlotConfig(
      zeroTime.toLong,
      zeroSlot.toLong,
      slotLength.toLong,
      epochLength.toLong,
      zeroEpoch.toLong
    )

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

    /** Epoch containing the given slot. Slots before `zeroSlot` are clamped to `zeroEpoch`. */
    def epochOf(slot: Double): Double =
        if slot <= zeroSlot then zeroEpoch
        else zeroEpoch + math.floor((slot - zeroSlot) / epochLength)

    /** First slot of the given epoch (for epochs >= `zeroEpoch`). */
    def firstSlotOfEpoch(epoch: Double): Double = zeroSlot + (epoch - zeroEpoch) * epochLength
}

object JsSlotConfig {

    /** Internal bridge: wrap a ledger value. Not exported. */
    private[scalus] def wrap(config: SlotConfig): JsSlotConfig = new JsSlotConfig(
      config.zeroTime.toDouble,
      config.zeroSlot.toDouble,
      config.slotLength.toDouble,
      config.epochLength.toDouble,
      config.zeroEpoch.toDouble
    )

    /** Internal bridge: the wrapped ledger value. Not exported - see `JsValue.underlying` for why
      * this is an extension method in the companion rather than a member of the class.
      */
    extension (self: JsSlotConfig) private[scalus] def underlying: SlotConfig = self.config

    // taken from https://github.com/spacebudz/lucid/blob/main/src/plutus/time.ts

    /** Mainnet slot configuration starting at Shelley era (slot 4492800 = start of epoch 208) */
    @JSExportStatic
    val mainnet: JsSlotConfig = wrap(SlotConfig.mainnet)

    /** Preview testnet slot configuration starting at Shelley era (1-day epochs) */
    @JSExportStatic
    val preview: JsSlotConfig = wrap(SlotConfig.preview)

    /** Preprod testnet slot configuration (slot 86400 = start of epoch 4, after 4 Byron epochs) */
    @JSExportStatic
    val preprod: JsSlotConfig = wrap(SlotConfig.preprod)
}
