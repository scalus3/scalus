package scalus.compiler.intrinsics

/** @deprecated Use [[scalus.compiler.UplcRepresentation]] instead. */
@deprecated("Use scalus.compiler.UplcRepresentation instead", "0.16.0")
type ReprTag = scalus.compiler.UplcRepresentation

/** @deprecated Use [[scalus.compiler.UplcRepresentation]] instead. */
@deprecated("Use scalus.compiler.UplcRepresentation instead", "0.16.0")
val ReprTag = scalus.compiler.UplcRepresentation

object ReprTagConvert {

    import scalus.compiler.UplcRepresentation
    import scalus.compiler.sir.lowering.*

    /** Convert a `UplcRepresentation` to the actual `LoweredValueRepresentation`. */
    def toLoweredValueRepresentation(tag: UplcRepresentation): LoweredValueRepresentation =
        tag match
            case UplcRepresentation.ProdBuiltinPair(fstTag, sndTag) =>
                ProductCaseClassRepresentation.ProdBuiltinPair(
                  toLoweredValueRepresentation(fstTag),
                  toLoweredValueRepresentation(sndTag)
                )
            case UplcRepresentation.SumBuiltinList(elemTag) =>
                SumCaseClassRepresentation.SumBuiltinList(toLoweredValueRepresentation(elemTag))
            case UplcRepresentation.DataData   => SumCaseClassRepresentation.DataData
            case UplcRepresentation.Constant   => PrimitiveRepresentation.Constant
            case UplcRepresentation.PackedData => PrimitiveRepresentation.PackedData
            case UplcRepresentation.DataConstr => SumCaseClassRepresentation.DataConstr
            case UplcRepresentation.PackedSumDataList =>
                SumCaseClassRepresentation.PackedSumDataList
            case _ =>
                throw IllegalArgumentException(
                  s"Cannot convert type-level UplcRepresentation.$tag to LoweredValueRepresentation"
                )
}
