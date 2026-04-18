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
    case ProductCase, SumCase, PackedDataMap, Data, BuiltinArray, ProductCaseOneElement
    case SumPairDataList
    case UplcConstr

    /** TypeVar representation marker for `@UplcRepr(TypeVar(kind))` on type parameters.
      *
      * The plugin reads the annotation and stamps the corresponding `SIRType.TypeVarKind` on the
      * emitted `SIRType.TypeVar` instead of the default `Fixed`.
      */
    case TypeVar(kind: UplcRepresentation.TypeVarKind)
}

object UplcRepresentation {

    /** Mirror of `scalus.compiler.sir.SIRType.TypeVarKind` for the surface
      * `@UplcRepr(TypeVar(...))` annotation. Kept distinct so user-code references don't pull in
      * `SIRType` (which lives in the lowering layer).
      */
    enum TypeVarKind {
        case Transparent, Unwrapped, Fixed
    }
}
