package scalus.uplc.builtin

// Stub BuiltinValueApi for the compiler plugin.
// The real implementation in scalus-core uses SortedMap, but the plugin
// only needs the type for compilation - it doesn't need the actual implementation.

private[builtin] trait BuiltinValueApi {

    /** Internal representation type - stub for plugin (Unit placeholder) */
    type InnerType = Unit

    /** The empty value - stub */
    val empty: BuiltinValue = new BuiltinValue(())

    /** Create from inner - stub */
    private[scalus] def unsafeFromInner(inner: InnerType): BuiltinValue =
        new BuiltinValue(inner)

    /** Equality - stub */
    private[builtin] def valueEquals(self: BuiltinValue, that: Any): Boolean = that match {
        case _: BuiltinValue => true // Stub
        case _               => false
    }

    /** Hash code - stub */
    private[builtin] def valueHashCode(self: BuiltinValue): Int = 0

    /** String representation - stub */
    private[builtin] def valueToString(self: BuiltinValue): String = "BuiltinValue(stub)"

    /** Convert to Data - stub, not used in plugin */
    def toData(value: BuiltinValue): Data =
        throw new UnsupportedOperationException("BuiltinValue.toData not available in plugin")

    /** Convert from Data - stub, not used in plugin */
    def fromData(data: Data): BuiltinValue =
        throw new UnsupportedOperationException("BuiltinValue.fromData not available in plugin")

    /** Entry-list view - stub, not used in plugin */
    def toEntryList(value: BuiltinValue): List[(ByteString, List[(ByteString, BigInt)])] =
        throw new UnsupportedOperationException("BuiltinValue.toEntryList not available in plugin")

    /** Rebuild from entry list - stub, not used in plugin */
    def fromEntryList(entries: List[(ByteString, List[(ByteString, BigInt)])]): BuiltinValue =
        throw new UnsupportedOperationException(
          "BuiltinValue.fromEntryList not available in plugin"
        )
}
