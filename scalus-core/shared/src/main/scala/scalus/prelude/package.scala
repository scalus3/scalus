package scalus

/** Backward compatibility package for scalus.prelude.
  *
  * All types and values have been moved to [[scalus.cardano.onchain.plutus.prelude]]. This package
  * provides deprecated aliases for migration.
  */
package object prelude {
    private inline val version = "0.14.2"
    private inline val pkg = "scalus.cardano.onchain.plutus.prelude"

    // ============================================================================
    // Deprecated type aliases
    // ============================================================================

    @deprecated(s"Use $pkg.List instead", version)
    type List[+A] = scalus.cardano.onchain.plutus.prelude.List[A]

    @deprecated(s"Use $pkg.Option instead", version)
    type Option[+A] = scalus.cardano.onchain.plutus.prelude.Option[A]

    @deprecated(s"Use $pkg.AssocMap instead", version)
    type AssocMap[A, B] = scalus.cardano.onchain.plutus.prelude.AssocMap[A, B]

    @deprecated(s"Use $pkg.SortedMap instead", version)
    type SortedMap[A, B] = scalus.cardano.onchain.plutus.prelude.SortedMap[A, B]

    @deprecated(s"Use $pkg.Eq instead", version)
    type Eq[A] = scalus.cardano.onchain.plutus.prelude.Eq[A]

    @deprecated(s"Use $pkg.Ord instead", version)
    type Ord[A] = scalus.cardano.onchain.plutus.prelude.Ord[A]

    @deprecated(s"Use $pkg.Order instead", version)
    type Order = scalus.cardano.onchain.plutus.prelude.Order

    @deprecated(s"Use $pkg.Show instead", version)
    type Show[A] = scalus.cardano.onchain.plutus.prelude.Show[A]

    @deprecated(s"Use $pkg.These instead", version)
    type These[+A, +B] = scalus.cardano.onchain.plutus.prelude.These[A, B]

    @deprecated(s"Use $pkg.Validator instead", version)
    type Validator = scalus.cardano.onchain.plutus.prelude.Validator

    @deprecated(s"Use $pkg.ParameterizedValidator instead", version)
    type ParameterizedValidator[A] = scalus.cardano.onchain.plutus.prelude.ParameterizedValidator[A]

    @deprecated(s"Use $pkg.DataParameterizedValidator instead", version)
    type DataParameterizedValidator =
        scalus.cardano.onchain.plutus.prelude.DataParameterizedValidator

    @deprecated(s"Use $pkg.Varargs instead", version)
    type Varargs[A] = scalus.cardano.onchain.plutus.prelude.Varargs[A]

    @deprecated(s"Use $pkg.BuiltinData instead", version)
    type BuiltinData = scalus.cardano.onchain.plutus.prelude.BuiltinData

    @deprecated(s"Use $pkg.Rational instead", version)
    type Rational = scalus.cardano.onchain.plutus.prelude.Rational

    // ============================================================================
    // Deprecated companion objects
    // ============================================================================

    @deprecated(s"Use $pkg.List instead", version)
    val List = scalus.cardano.onchain.plutus.prelude.List

    @deprecated(s"Use $pkg.Option instead", version)
    val Option = scalus.cardano.onchain.plutus.prelude.Option

    @deprecated(s"Use $pkg.AssocMap instead", version)
    val AssocMap = scalus.cardano.onchain.plutus.prelude.AssocMap

    @deprecated(s"Use $pkg.SortedMap instead", version)
    val SortedMap = scalus.cardano.onchain.plutus.prelude.SortedMap

    @deprecated(s"Use $pkg.Eq instead", version)
    val Eq = scalus.cardano.onchain.plutus.prelude.Eq

    @deprecated(s"Use $pkg.Ord instead", version)
    val Ord = scalus.cardano.onchain.plutus.prelude.Ord

    @deprecated(s"Use $pkg.Order instead", version)
    val Order = scalus.cardano.onchain.plutus.prelude.Order

    @deprecated(s"Use $pkg.Show instead", version)
    val Show = scalus.cardano.onchain.plutus.prelude.Show

    @deprecated(s"Use $pkg.These instead", version)
    val These = scalus.cardano.onchain.plutus.prelude.These

    @deprecated(s"Use $pkg.Prelude instead", version)
    val Prelude = scalus.cardano.onchain.plutus.prelude.Prelude

    @deprecated(s"Use $pkg.Math instead", version)
    val Math = scalus.cardano.onchain.plutus.prelude.Math

    @deprecated(s"Use $pkg.Rational instead", version)
    val Rational = scalus.cardano.onchain.plutus.prelude.Rational

    // ============================================================================
    // Deprecated inline functions
    // ============================================================================

    @deprecated(s"Use $pkg.require instead", version)
    inline def require(inline cond: Boolean): Unit =
        scalus.cardano.onchain.plutus.prelude.require(cond)

    @deprecated(s"Use $pkg.require instead", version)
    inline def require(inline cond: Boolean, inline msg: String): Unit =
        scalus.cardano.onchain.plutus.prelude.require(cond, msg)

    @deprecated(s"Use $pkg.fail instead", version)
    inline def fail(inline msg: String): Nothing =
        scalus.cardano.onchain.plutus.prelude.fail(msg)

    @deprecated(s"Use $pkg.fail instead", version)
    inline def fail(): Nothing =
        scalus.cardano.onchain.plutus.prelude.fail()

    @deprecated(s"Use $pkg.impossible instead", version)
    inline def impossible(): Nothing =
        scalus.cardano.onchain.plutus.prelude.impossible()

    @deprecated(s"Use $pkg.log instead", version)
    inline def log(inline msg: String): Unit =
        scalus.cardano.onchain.plutus.prelude.log(msg)

    @deprecated(s"Use $pkg.??? instead", version)
    inline def ??? : Nothing =
        scalus.cardano.onchain.plutus.prelude.???

    @deprecated(s"Use $pkg.identity instead", version)
    inline def identity[A](inline value: A): A =
        scalus.cardano.onchain.plutus.prelude.identity(value)

    // ============================================================================
    // Extension method re-exports
    // ============================================================================

    export scalus.cardano.onchain.plutus.prelude.{`!==`, `<=>`, `===`, `?`, asScalus, list, show}

    // Export BigInt math extension methods
    export scalus.cardano.onchain.plutus.prelude.{absolute, clamp, exp2, gcf, isSqrt, log2, logarithm, maximum, minimum, pow, sqRoot}
}
