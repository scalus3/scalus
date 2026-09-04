package scalus.cardano.ledger

/** The Scala.js half of [[SlotConfig]]: empty, because JavaScript has no `java.time.Instant`.
  *
  * The JVM and Native copies carry `slotToInstant`/`instantToSlot`. This one exists so that the
  * shared `SlotConfig` can name the trait on every platform it compiles to - the same rule the
  * interop style guide states for `<ClassName>Platform` mixins.
  *
  * JavaScript callers reach slot arithmetic through [[JsSlotConfig]], the exported `SlotConfig`
  * handle, not through this type.
  */
private[ledger] trait SlotConfigPlatform { self: SlotConfig => }
