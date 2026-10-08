package scalus
package uplc
package eval

/** JVM-specific Plutus Conformance tests.
  *
  * BLS12-381 goes through scalus-crypto-jni, which passes the DST to blst as raw bytes, so no case
  * is skipped.
  */
class PlutusConformanceJvmTest extends PlutusConformanceTest
