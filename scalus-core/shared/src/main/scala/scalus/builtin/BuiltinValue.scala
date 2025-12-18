package scalus.builtin

/** MaryEraValue (CIP-0153) - multi-asset value representation for Cardano.
  *
  * This is a primitive opaque type in UPLC. The inner representation is hidden - use builtin
  * operations to work with values:
  *   - `insertCoin` to insert/update token amounts
  *   - `lookupCoin` to lookup token amounts
  *   - `unionValue` to merge two values
  *   - `valueContains` to check if one value contains another
  *   - `scaleValue` to multiply all amounts by a scalar
  *   - `valueData` / `unValueData` for Data conversion
  *
  * Invariants maintained by operations:
  *   - No empty inner maps (currency symbols with no tokens are removed)
  *   - No zero quantities (zero-amount tokens are removed)
  *   - Keys max 32 bytes (currency symbols and token names)
  *   - Quantities in signed 128-bit range
  */
final class BuiltinValue private[scalus] (
    private[scalus] val inner: BuiltinValue.InnerType
) {

    override def equals(that: Any): Boolean = BuiltinValue.valueEquals(this, that)

    override def hashCode(): Int = BuiltinValue.valueHashCode(this)

    override def toString: String = BuiltinValue.valueToString(this)
}

object BuiltinValue extends BuiltinValueApi
