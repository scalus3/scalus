package scalus.compiler.intrinsics

import scalus.Compile

/** Intrinsic helper functions for type and representation casts.
  *
  * Marker methods intercepted by the compiler plugin. Never evaluated at runtime.
  */
@Compile
object IntrinsicHelpers {

    /** Zero-cost type/representation cast.
      *
      * The `repr` parameter specifies the target representation as a `ReprTag`. The plugin compiles
      * it to SIR and stores in an annotation; the lowering interprets it.
      */
    /** Zero-cost type-only cast. Changes the SIR type without affecting representation.
      */
    def typeProxy[V](x: Any): V =
        throw new RuntimeException("typeProxy: should be eliminated by the Scalus compiler plugin")

    /** Zero-cost type/representation cast.
      *
      * The `repr` parameter specifies the target representation as a `ReprTag`. The plugin compiles
      * it to SIR and stores in an annotation; the lowering interprets it.
      */
    def typeProxyRepr[V](x: Any, repr: ReprTag): V =
        throw new RuntimeException(
          "typeProxyRepr: should be eliminated by the Scalus compiler plugin"
        )
}
