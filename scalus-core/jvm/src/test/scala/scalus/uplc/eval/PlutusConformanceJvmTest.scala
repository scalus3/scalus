package scalus
package uplc
package eval

/** JVM-specific Plutus Conformance tests.
  *
  * BLS12-381 goes through the blst Java binding, which has a bug for DSTs longer than 255 bytes.
  */
class PlutusConformanceJvmTest extends PlutusConformanceTest {
    override protected def ignoredCases: Map[String, String] = blstLargeDstCases
}
