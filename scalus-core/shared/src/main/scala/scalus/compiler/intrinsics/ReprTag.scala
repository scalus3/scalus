package scalus.compiler.intrinsics

import scalus.compiler.sir.lowering.*

/** Lightweight mirror of `LoweredValueRepresentation` for use in `@Compile` intrinsic code.
  *
  * `LoweredValueRepresentation` cannot be used directly in `@Compile` code because its sealed trait
  * hierarchy involves types like `IndexedSeq` that the Scalus compiler plugin cannot compile to
  * SIR. This enum uses `EnumCase` flags so the plugin recognizes its cases as constructors and
  * produces `SIR.Constr` nodes. The lowering interprets these via `interpretReprSIR` to reconstruct
  * the actual `LoweredValueRepresentation`.
  */
enum ReprTag {
    case DataData, Constant, PackedData, DataConstr, PackedSumDataList
    case SumBuiltinList(elemRepr: ReprTag)
    case ProdBuiltinPair(fstRepr: ReprTag, sndRepr: ReprTag)
}

object ReprTagConvert {

    /** Convert a `ReprTag` to the actual `LoweredValueRepresentation`. */
    def toLoweredValueRepresentation(tag: ReprTag): LoweredValueRepresentation = tag match
        case ReprTag.ProdBuiltinPair(fstTag, sndTag) =>
            ProductCaseClassRepresentation.ProdBuiltinPair(
              toLoweredValueRepresentation(fstTag),
              toLoweredValueRepresentation(sndTag)
            )
        case ReprTag.SumBuiltinList(elemTag) =>
            SumCaseClassRepresentation.SumBuiltinList(toLoweredValueRepresentation(elemTag))
        case ReprTag.DataData          => SumCaseClassRepresentation.DataData
        case ReprTag.Constant          => PrimitiveRepresentation.Constant
        case ReprTag.PackedData        => PrimitiveRepresentation.PackedData
        case ReprTag.DataConstr        => SumCaseClassRepresentation.DataConstr
        case ReprTag.PackedSumDataList => SumCaseClassRepresentation.PackedSumDataList
}
