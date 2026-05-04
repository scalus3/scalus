package scalus.compiler.intrinsics

import scalus.compiler.Compile
import scalus.compiler.UplcRepresentation

/** Intrinsic helper functions for type and representation casts.
  *
  * Marker methods intercepted by the compiler plugin. Never evaluated at runtime.
  */
@Compile
object IntrinsicHelpers {

    /** Zero-cost type-only cast. Changes the SIR type without affecting representation.
      */
    def typeProxy[V](x: Any): V =
        throw new RuntimeException("typeProxy: should be eliminated by the Scalus compiler plugin")

    /** Zero-cost type/representation cast.
      *
      * The `repr` parameter specifies the target representation as a `UplcRepresentation`. The
      * plugin compiles it to SIR and stores in an annotation; the lowering interprets it.
      */
    def typeProxyRepr[V](x: Any, repr: UplcRepresentation): V =
        throw new RuntimeException(
          "typeProxyRepr: should be eliminated by the Scalus compiler plugin"
        )

    /** Convert a value to its defaultTypeVarRepresentation.
      *
      * Used in intrinsic bodies to convert native-repr values to the representation expected by
      * pre-compiled higher-order functions (like Eq instances). When the intrinsic is re-lowered at
      * a concrete call site, the lowering resolves A to the concrete type and generates the actual
      * representation conversion.
      */
    def toDefaultTypeVarRepr[A](x: A): A =
        throw new RuntimeException(
          "toDefaultTypeVarRepr: should be eliminated by the Scalus compiler plugin"
        )

    /** Convert a value from its TypeVar representation to the concrete type's native
      * representation.
      *
      * Reverse of `toDefaultTypeVarRepr`. Used in intrinsic HOF wrappers (map, filter, filterMap,
      * etc.) to convert list elements from their UplcConstr-list TypeVar(Transparent) repr to the
      * concrete type's native repr (e.g. ProdUplcConstr) before passing them to user-provided
      * lambdas that were compiled against the concrete type. At lowering time the concrete type A
      * is known, so the lowering calls `gen.defaultRepresentation(tp)` and inserts any needed
      * conversion. For TypeVar(Transparent) elements whose native repr already matches the target
      * (the common case), this is a zero-cost relabel with no UPLC emitted.
      */
    def fromDefaultTypeVarRepr[A](x: A): A =
        throw new RuntimeException(
          "fromDefaultTypeVarRepr: should be eliminated by the Scalus compiler plugin"
        )

    /** Representation-aware structural equality.
      *
      * Marker function intercepted at lowering time. The lowerer knows the concrete type and
      * representation of both arguments and generates optimal comparison:
      *   - Primitive (BigInt): equalsInteger
      *   - Primitive (ByteString): equalsByteString
      *   - Data-compatible: equalsData (after conversion to Data)
      */
    def equalsRepr[A](a: A, b: A): Boolean =
        throw new RuntimeException(
          "equalsRepr: should be eliminated by the Scalus lowering"
        )
}
