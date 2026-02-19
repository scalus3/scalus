package scalus.crypto.accumulator

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.crypto.bls12_381.{G1, G2}
import scalus.crypto.accumulator.BilinearAccumulatorProver.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}

/** Benchmarks for the bilinear accumulator using the Ethereum KZG ceremony (32768 points).
  *
  * These tests are tagged with `scalus.testing.Benchmark` and excluded from default test runs. Run
  * with:
  * {{{
  * sbtn "scalusJVM/testOnly -- -n scalus.testing.Benchmark"
  * }}}
  */
class BilinearAccumulatorBenchmarkTest extends AnyFunSuite {
    import BilinearAccumulatorBenchmarkTest.*

    private lazy val ceremony = loadCeremony()

    test("load Ethereum ceremony and verify structure", Benchmark) {
        assert(ceremony.g1Monomial.length == 32768, "expected 32768 G1 points")
        assert(ceremony.g2Monomial.length == 65, "expected 65 G2 points")
        // First G1 point should be the generator
        assert(ceremony.g1Monomial.head == G1.generator, "first G1 should be generator")
        // First G2 point should be the generator
        assert(ceremony.g2Monomial.head == G2.generator, "first G2 should be generator")
    }

    test("Poly.product 32000 elements", Benchmark) {
        val rng = new scala.util.Random(42)
        val elements = Vector.fill(32000)(BigInt(256, rng).mod(BilinearAccumulatorProver.p))
        val t0 = System.nanoTime()
        val poly = Poly.product(elements)
        val ms = (System.nanoTime() - t0) / 1_000_000
        info(s"Poly.product(32000) took ${ms} ms, degree = ${poly.degree}")
        assert(poly.degree == 32000)
    }

    test("full G1 accumulator E2E with 32000 elements (Ethereum ceremony)", Benchmark) {
        // G1 accumulator: acc on G1 (needs G1 powers for accumulation/proofs),
        // on-chain verification uses G2 powers (only needs degree of subset).
        // Ethereum ceremony: 32768 G1 points (supports degree 32767), 65 G2 points.
        val n = 32000
        val rng = new scala.util.Random(123)
        val elements = Vector.fill(n)(BigInt(256, rng).mod(BilinearAccumulatorProver.p))
        val setup = Setup.fromPoints(ceremony.g1Monomial.toVector, ceremony.g2Monomial.toVector)

        val t0 = System.nanoTime()
        val acc = accumulateG1(setup, elements)
        val msAcc = (System.nanoTime() - t0) / 1_000_000
        info(s"accumulateG1(32000) took ${msAcc} ms")

        // Membership proof for a single element
        val subset = Vector(elements.head)
        val t1 = System.nanoTime()
        val proof = membershipProofG1(setup, elements, subset)
        val msProof = (System.nanoTime() - t1) / 1_000_000
        info(s"membershipProofG1(32000, 1) took ${msProof} ms")

        val t2 = System.nanoTime()
        val valid = verifyMembershipG1(setup, acc, subset, proof)
        val msVerify = (System.nanoTime() - t2) / 1_000_000
        info(s"verifyMembershipG1 took ${msVerify} ms")
        assert(valid)
    }

    test("Poly.extGcd benchmark (~16000-degree polynomials)", Benchmark) {
        val rng = new scala.util.Random(99)
        val n = 16000
        val elementsA = Vector.fill(n)(BigInt(256, rng).mod(BilinearAccumulatorProver.p))
        val elementsB = Vector.fill(n)(BigInt(256, rng).mod(BilinearAccumulatorProver.p))

        info(s"computing Poly.product for two sets of $n elements...")
        val t0 = System.nanoTime()
        val polyA = Poly.product(elementsA)
        val polyB = Poly.product(elementsB)
        val msProd = (System.nanoTime() - t0) / 1_000_000
        info(s"two Poly.product($n) took ${msProd} ms total")

        info(
          s"computing extGcd of degree-${polyA.degree} and degree-${polyB.degree} polynomials..."
        )
        val t1 = System.nanoTime()
        val (gcd, _, _) = polyA.extGcd(polyB)
        val msGcd = (System.nanoTime() - t1) / 1_000_000
        info(s"extGcd took ${msGcd} ms, gcd degree = ${gcd.degree}")
        // Two random polynomials should be coprime
        assert(gcd == Poly.one, s"expected coprime, got gcd degree ${gcd.degree}")
    }
}

object BilinearAccumulatorBenchmarkTest {

    /** Tag matching scalus.testing.Benchmark for excluding from default runs. */
    private object Benchmark extends Tag("scalus.testing.Benchmark")

    /** JSON structure matching the Ethereum KZG ceremony format. */
    private case class EthereumCeremony(
        g1Monomial: List[G1Element],
        g2Monomial: List[G2Element]
    )

    private given JsonValueCodec[G1Element] = new JsonValueCodec[G1Element] {
        def decodeValue(in: JsonReader, default: G1Element): G1Element = {
            val hex = in.readString("")
            G1Element(ByteString.fromHex(hex.substring(2))) // strip "0x" prefix
        }
        def encodeValue(x: G1Element, out: JsonWriter): Unit = ???
        def nullValue: G1Element = null.asInstanceOf[G1Element]
    }

    private given JsonValueCodec[G2Element] = new JsonValueCodec[G2Element] {
        def decodeValue(in: JsonReader, default: G2Element): G2Element = {
            val hex = in.readString("")
            G2Element(ByteString.fromHex(hex.substring(2))) // strip "0x" prefix
        }
        def encodeValue(x: G2Element, out: JsonWriter): Unit = ???
        def nullValue: G2Element = null.asInstanceOf[G2Element]
    }

    private given JsonValueCodec[EthereumCeremony] = JsonCodecMaker.make(
      CodecMakerConfig
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
          .withSkipUnexpectedFields(false)
    )

    private def loadCeremony(): EthereumCeremony = {
        val input = getClass.getResourceAsStream("/trusted_setup_32768.json")
        require(input != null, "trusted_setup_32768.json not found in test resources")
        try readFromStream[EthereumCeremony](input)
        finally input.close()
    }
}
