package scalus

/** Backward compatibility package for scalus.builtin.
  *
  * All types and values have been moved to [[scalus.uplc.builtin]]. This package provides
  * deprecated aliases for migration.
  */
package object builtin {
    private inline val version = "0.14.2"
    private inline val pkg = "scalus.uplc.builtin"

    // ============================================================================
    // Deprecated type aliases for classes/traits/enums
    // ============================================================================

    @deprecated(s"Use $pkg.Data instead", version)
    type Data = scalus.uplc.builtin.Data

    @deprecated(s"Use $pkg.ByteString instead", version)
    type ByteString = scalus.uplc.builtin.ByteString

    @deprecated(s"Use $pkg.FromData instead", version)
    type FromData[+A] = scalus.uplc.builtin.FromData[A]

    @deprecated(s"Use $pkg.ToData instead", version)
    type ToData[-A] = scalus.uplc.builtin.ToData[A]

    @deprecated(s"Use $pkg.BuiltinList instead", version)
    type BuiltinList[+A] = scalus.uplc.builtin.BuiltinList[A]

    @deprecated(s"Use $pkg.BuiltinPair instead", version)
    type BuiltinPair[A, B] = scalus.uplc.builtin.BuiltinPair[A, B]

    @deprecated(s"Use $pkg.BuiltinArray instead", version)
    type BuiltinArray[A] = scalus.uplc.builtin.BuiltinArray[A]

    @deprecated(s"Use $pkg.BuiltinValue instead", version)
    type BuiltinValue = scalus.uplc.builtin.BuiltinValue

    @deprecated(s"Use $pkg.PlatformSpecific instead", version)
    type PlatformSpecific = scalus.uplc.builtin.PlatformSpecific

    @deprecated(s"Use $pkg.uplcIntrinsic instead", version)
    type uplcIntrinsic = scalus.uplc.builtin.uplcIntrinsic

    @deprecated(s"Use $pkg.Builtins instead", version)
    type Builtins = scalus.uplc.builtin.Builtins

    // BLS12-381 types (these are opaque types, but we alias them for completeness)
    @deprecated(s"Use $pkg.BLS12_381_G1_Element instead", version)
    type BLS12_381_G1_Element = scalus.uplc.builtin.BLS12_381_G1_Element

    @deprecated(s"Use $pkg.BLS12_381_G2_Element instead", version)
    type BLS12_381_G2_Element = scalus.uplc.builtin.BLS12_381_G2_Element

    @deprecated(s"Use $pkg.BLS12_381_MlResult instead", version)
    type BLS12_381_MlResult = scalus.uplc.builtin.BLS12_381_MlResult

    // ============================================================================
    // Deprecated val definitions for companion objects
    // ============================================================================

    @deprecated(s"Use $pkg.Data instead", version)
    val Data = scalus.uplc.builtin.Data

    @deprecated(s"Use $pkg.ByteString instead", version)
    val ByteString = scalus.uplc.builtin.ByteString

    @deprecated(s"Use $pkg.Builtins instead", version)
    val Builtins = scalus.uplc.builtin.Builtins

    @deprecated(s"Use $pkg.FromData instead", version)
    val FromData = scalus.uplc.builtin.FromData

    @deprecated(s"Use $pkg.ToData instead", version)
    val ToData = scalus.uplc.builtin.ToData

    @deprecated(s"Use $pkg.BuiltinList instead", version)
    val BuiltinList = scalus.uplc.builtin.BuiltinList

    @deprecated(s"Use $pkg.BuiltinPair instead", version)
    val BuiltinPair = scalus.uplc.builtin.BuiltinPair

    @deprecated(s"Use $pkg.BuiltinArray instead", version)
    val BuiltinArray = scalus.uplc.builtin.BuiltinArray

    @deprecated(s"Use $pkg.BuiltinValue instead", version)
    val BuiltinValue = scalus.uplc.builtin.BuiltinValue

    @deprecated(s"Use $pkg.PlatformSpecific instead", version)
    val PlatformSpecific = scalus.uplc.builtin.PlatformSpecific

    // BLS12-381 companion objects
    @deprecated(s"Use $pkg.BLS12_381_G1_Element instead", version)
    val BLS12_381_G1_Element = scalus.uplc.builtin.BLS12_381_G1_Element

    @deprecated(s"Use $pkg.BLS12_381_G2_Element instead", version)
    val BLS12_381_G2_Element = scalus.uplc.builtin.BLS12_381_G2_Element

    // Note: BLS12_381_MlResult doesn't have a usable companion object

    // ============================================================================
    // Deprecated inline function for platform access
    // ============================================================================

    @deprecated(s"Use $pkg.platform instead", version)
    inline def platform: scalus.uplc.builtin.PlatformSpecific = scalus.uplc.builtin.platform
}
