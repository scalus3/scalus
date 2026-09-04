package scalus.uplc.eval

/** Native-specific Plutus Conformance tests.
  *
  * BLS12-381 is implemented using the blst library via FFI, so it inherits the DST-length skips on
  * the assumption that supranational/blst#232 affects it too. That assumption is untested: the bug
  * was reported against the Java binding, and nobody has run these three cases here with the skips
  * removed. Skipping three cases is the safe direction, so this stays until someone measures it.
  */
class PlutusConformanceNativeTest extends PlutusConformanceTest {
    override protected def ignoredCases: Map[String, String] = blstLargeDstCases
}
