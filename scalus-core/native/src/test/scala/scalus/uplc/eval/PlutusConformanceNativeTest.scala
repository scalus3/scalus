package scalus.uplc.eval

/** Native-specific Plutus Conformance tests.
  *
  * BLS12-381 calls blst directly through FFI with the DST as raw bytes, so no case is skipped.
  */
class PlutusConformanceNativeTest extends PlutusConformanceTest
