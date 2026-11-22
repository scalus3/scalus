package scalus.testing

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.v1.{Credential, Value}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.uplc.builtin.ByteString

/** Thresholds for the shared generators: spec section 6.7 of
  * docs/internal/test-refactor/d5-generators.md (`[gen.quality.*]`).
  *
  * Each generator gets a predicate that every sample must pass (`[gen.quality.predicate]`), written
  * from the ledger specification and not from Scalus code (`[gen.family.spec-check]`), and a
  * minimum share for each edge class (`[gen.quality.edge-shares]`). The samples come from fixed
  * seeds, so the test cannot be flaky: it fails only when a generator changes.
  */
class ArbitraryInstancesQualityTest extends AnyFunSuite {
    import ArbitraryInstancesQualityTest.*

    test("Coin: in [0, max supply], with the edges and the realistic core") {
        import scalus.cardano.ledger.ArbitraryInstances.{genCoin, MaxLovelace}
        val coins = sample(genCoin).map(_.value)
        coins.foreach(c => assert(0 <= c && c <= MaxLovelace, c))
        assertShare(coins, "coin = 0", 0.02)(_ == 0)
        assertShare(coins, "coin = max supply", 0.02)(_ == MaxLovelace)
        assertShare(coins, "1 to 100 ADA", 0.25)(c => 1_000_000L <= c && c <= 100_000_000L)
        assertDiscards(genCoin)
    }

    test("v1.Value: canonical, positive, with the edge classes") {
        import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.genValue
        val values = sample(genValue)
        values.foreach(v =>
            assert(wellFormedValueProblem(v).isEmpty, (wellFormedValueProblem(v), v))
        )
        assertShare(values, "empty value", 0.02)(entries(_).isEmpty)
        assertShare(values, "lovelace only", 0.10)(v => policies(v) == Seq(AdaPolicy))
        assertShare(values, "2 or more policies", 0.10)(policies(_).size >= 2)
        val consecutive = values.zip(values.tail)
        assertShare(consecutive, "two consecutive values share a non-ADA policy", 0.01) { (a, b) =>
            nonAdaPolicies(a).exists(nonAdaPolicies(b).contains)
        }
        assertDiscards(genValue)
    }

    test("v2.TxOut: a well-formed value with lovelace, and every output form") {
        import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.genTxOut
        val outputs = sample(genTxOut)
        outputs.foreach { out =>
            assert(wellFormedValueProblem(out.value).isEmpty, out)
            assert(lovelaceOf(out.value) >= 1, out)
        }
        assertShare(outputs, "a token besides lovelace", 0.25)(out =>
            nonAdaPolicies(out.value).nonEmpty
        )
        // [gen.output.form-shares]: half of the share in the "before" GenStats of pilot step 1
        // (script payment 50.1 %, each datum case 33.1 % to 33.5 %, reference script 90.1 %).
        assertShare(outputs, "script payment credential", 0.25) { out =>
            out.address.credential.isInstanceOf[Credential.ScriptCredential]
        }
        assertShare(outputs, "no datum", 0.16)(_.datum == OutputDatum.NoOutputDatum)
        assertShare(outputs, "datum hash", 0.16)(_.datum.isInstanceOf[OutputDatum.OutputDatumHash])
        assertShare(outputs, "inline datum", 0.16)(_.datum.isInstanceOf[OutputDatum.OutputDatum])
        assertShare(outputs, "reference script", 0.45)(_.referenceScript.isDefined)
        assertDiscards(genTxOut)
    }

    test("genMintValue: canonical, signed, no lovelace") {
        import scalus.cardano.onchain.plutus.v3.ArbitraryInstances.genMintValue
        val mints = sample(genMintValue)
        mints.foreach { mint =>
            assert(canonicalProblem(mint).isEmpty, (canonicalProblem(mint), mint))
            assert(!policies(mint).contains(AdaPolicy), mint)
        }
        assertShare(mints, "a negative amount", 0.25)(entries(_).exists(_._3 < 0))
        assertDiscards(genMintValue)
    }
}

object ArbitraryInstancesQualityTest {

    /** `[gen.quality.sampling]`: 5 000 values at size 100 from `Seed(42)`, with `seed.slide`. */
    val Samples = 5000

    private val AdaPolicy = ByteString.empty
    private val TwoPow64 = BigInt(2).pow(64)

    def sample[A](gen: Gen[A]): Seq[A] = {
        val params = Gen.Parameters.default.withSize(100)
        Iterator
            .iterate(Seed(42L))(_.slide)
            .map(seed => gen(params, seed))
            .collect { case Some(value) => value }
            .take(Samples)
            .toSeq
    }

    /** `[gen.quality.edge-shares]`: fails when fewer than `minimum` of the samples are in the
      * class.
      */
    def assertShare[A](
        samples: Seq[A],
        edgeClass: String,
        minimum: Double
    )(p: A => Boolean): Unit = {
        val share = samples.count(p).toDouble / samples.size
        assert(
          share >= minimum,
          f"$edgeClass: share ${share * 100}%.1f %% is below the minimum ${minimum * 100}%.1f %%"
        )
    }

    /** `[gen.quality.discard-limit]`: at most 5 % discarded samples at each size from 5 to 100. */
    def assertDiscards[A](gen: Gen[A]): Unit =
        for size <- 5 to 100 by 5 do
            val params = Gen.Parameters.default.withSize(size)
            val draws = 200
            val discarded = (0 until draws).count(i => gen(params, Seed(i.toLong)).isEmpty)
            assert(discarded <= draws / 20, s"size $size: $discarded of $draws discarded")

    // ----- Value checks from the ledger specification, not from Scalus code -----
    //
    // A Plutus `Value` is a map from policy id to a map from token name to amount. Its canonical
    // form has both maps in ascending byte order, no empty inner map, and no zero amount. The ADA
    // entry has the empty policy id and the empty token name; any other policy id is a 28-byte
    // script hash, and a token name is at most 32 bytes.

    def entries(v: Value): Seq[(ByteString, ByteString, BigInt)] =
        for
            (policy, tokens) <- v.toSortedMap.toList.asScala
            (token, amount) <- tokens.toList.asScala
        yield (policy, token, amount)

    def policies(v: Value): Seq[ByteString] = v.toSortedMap.toList.asScala.map(_._1)

    def nonAdaPolicies(v: Value): Seq[ByteString] = policies(v).filterNot(_ == AdaPolicy)

    def lovelaceOf(v: Value): BigInt =
        entries(v).collectFirst { case (AdaPolicy, _, amount) => amount }.getOrElse(BigInt(0))

    /** Unsigned lexicographic order of the bytes, the order of the ledger's maps. */
    private def compareBytes(a: ByteString, b: ByteString): Int = {
        val (x, y) = (a.bytes, b.bytes)
        val firstDifference = x.indices.take(y.length).find(i => x(i) != y(i))
        firstDifference match
            case Some(i) => (x(i) & 0xff) - (y(i) & 0xff)
            case None    => x.length - y.length
    }

    private def strictlyAscending(keys: Seq[ByteString]): Boolean =
        keys.zip(keys.drop(1)).forall((a, b) => compareBytes(a, b) < 0)

    /** The first rule of the canonical form that `v` breaks, if any. */
    def canonicalProblem(v: Value): Option[String] = {
        val outer = v.toSortedMap.toList.asScala
        if !strictlyAscending(outer.map(_._1)) then Some("policy ids not strictly ascending")
        else
            outer.collectFirst {
                case (policy, tokens) if tokens.toList.asScala.isEmpty =>
                    s"empty token map under $policy"
                case (policy, tokens) if !strictlyAscending(tokens.toList.asScala.map(_._1)) =>
                    s"token names under $policy not strictly ascending"
                case (policy, tokens) if tokens.toList.asScala.exists(_._2 == 0) =>
                    s"zero amount under $policy"
            }
    }

    /** `canonicalProblem`, plus the key sizes and the amount range of `Arbitrary[Value]`. */
    def wellFormedValueProblem(v: Value): Option[String] =
        canonicalProblem(v).orElse {
            entries(v).collectFirst {
                case (policy, _, _) if policy.size != 0 && policy.size != 28 =>
                    s"policy id of ${policy.size} bytes"
                case (AdaPolicy, token, _) if token.size != 0 => "ADA entry with a token name"
                case (_, token, _) if token.size > 32 => s"token name of ${token.size} bytes"
                case (_, _, amount) if amount < 1 || amount > TwoPow64 => s"amount $amount"
            }
        }
}
