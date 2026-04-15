package scalus.compiler

/** Specification of UPLC type representations.
  *
  * Used with the [[UplcRepr]] annotation to customize how a type or field is represented in UPLC.
  * Also used in `@Compile` intrinsic code via `typeProxyRepr`.
  *
  * This enum uses `EnumCase` flags so the compiler plugin recognizes its cases as constructors and
  * produces `SIR.Constr` nodes. The lowering interprets these via `interpretReprSIR` to reconstruct
  * the actual `LoweredValueRepresentation`.
  */
enum UplcRepresentation {
    // Primitive/data representations (used in typeProxyRepr and field annotations)
    case DataData, Constant, PackedData, DataConstr, PackedSumDataList
    // Parameterized representations
    case SumBuiltinList(elemRepr: UplcRepresentation)
    case ProdBuiltinPair(fstRepr: UplcRepresentation, sndRepr: UplcRepresentation)
    // Type-level representations (used in @UplcRepr annotations on types/fields)
    case ProductCase, SumCase, Map, Data, BuiltinArray, ProductCaseOneElement
    case SumPairDataList
    case UplcConstr
}
