package scalus.cardano.onchain.plutus.v1

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.prelude.{List, Option, Ord, Order}
import scalus.testing.ArbitraryDerivation.autoDerived
import scalus.uplc.test

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends test.ArbitraryInstances {
    given Arbitrary[IntervalBoundType] = Arbitrary {
        Gen.frequency(
          (1, Gen.const(IntervalBoundType.NegInf)),
          (1, Gen.const(IntervalBoundType.PosInf)),
          (8, arbitrary[PosixTime].map(IntervalBoundType.Finite(_)))
        )
    }

    given Arbitrary[IntervalBound] = autoDerived

    given Arbitrary[Interval] = Arbitrary {
        for
            b1 <- arbitrary[IntervalBound]
            b2 <- arbitrary[IntervalBound]
        yield Ord[IntervalBound].compare(b1, b2) match
            case Order.Greater => Interval(b2, b1)
            case _             => Interval(b1, b2)
    }

    given TxIdV1: Arbitrary[TxId] = Arbitrary {
        genByteStringOfN(32).map(TxId.apply)
    }

    given Arbitrary[PubKeyHash] = Arbitrary {
        genByteStringOfN(28).map(PubKeyHash.apply)
    }

    given Arbitrary[Credential] = Arbitrary {
        import scalus.cardano.ledger.ArbitraryInstances.given
        Gen.oneOf(
          arbitrary[PubKeyHash].map(Credential.PubKeyCredential.apply),
          arbitrary[scalus.cardano.ledger.ScriptHash].map(Credential.ScriptCredential.apply)
        )
    }

    given Arbitrary[StakingCredential] = Arbitrary {
        // We don't generate StakingPtr because it's deprecated and can't be used since Conway era.
        arbitrary[Credential].map(StakingCredential.StakingHash.apply)
    }

    given Arbitrary[Address] = autoDerived

    /** Generates a random asset name, which is a ByteString of length between 1 and 32 bytes.
      */
    def genAssetName: Gen[ByteString] = Gen.choose(1, 32).flatMap(genByteStringOfN)
    def genPolicyId: Gen[ByteString] = genByteStringOfN(28)

    /** Token quantities, bounded to what a `Value` may legally hold.
      *
      * Same edge-case shape as `iArb`, but its wide `[-2^128, 2^128]` branch is narrowed to
      * `[-2^64, 2^64]`: real ledger quantities fit Int64, and the CIP-153 builtins that `Value`
      * operations lower to at PV11 reject quantities beyond +-(2^127) and fail when a
      * `unionValue`/`scaleValue` result leaves that range (see
      * `docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md`). The bound stays
      * well below 2^127 so that summing the up-to-11 generated assets cannot overflow either.
      *
      * The `* properties` test in `ValueTest` couples its `+-(2^62)` scaling-factor bound to this
      * `+-(2^64)` amount bound (`2^64 * 2^62 = 2^126`), so widening this bound means narrowing that
      * one in step.
      */
    def genAmount: Gen[BigInt] = Gen.oneOf[BigInt](
      Gen.const[BigInt](0),
      Gen.const[BigInt](-1),
      Gen.const[BigInt](1),
      Gen.const[BigInt](-1000),
      Gen.const[BigInt](1000),
      Gen.const[BigInt](Int.MaxValue),
      Gen.const[BigInt](Int.MinValue),
      Gen.const[BigInt](Long.MaxValue),
      Gen.const[BigInt](Long.MinValue),
      Gen.choose[BigInt](-BigInt(2).pow(64), BigInt(2).pow(64))
    )
    def genToken: Gen[(ByteString, BigInt)] =
        for
            assetName <- genAssetName
            amount <- genAmount
        yield (assetName, amount)

    def genAsset: Gen[Value] = for
        policyId <- genPolicyId
        size <- Gen.frequency(
          (5, Gen.choose(0, 1)), // 0 to 10 policyId
          (3, Gen.choose(1, 10)) // 0 to 10 policyId
        )
        tokens <- Gen.listOfN(size, genToken)
    yield
        val assets = List.from(tokens)
        Value.fromList(List((policyId, assets)))

    def genLovelace: Gen[Value] = genAmount.map(Value.lovelace)

    // ----- D5: Value generators (docs/internal/test-refactor/d5-generators.md, section 6.2) -----
    //
    // The members above stay as they are and keep their signed amounts. The default given now
    // generates positive amounts only (`[gen.value.positive]`); signed values come from the
    // named generators `genAnyValue`, `genMintValue` and `genValueOver`.

    private lazy val TwoPow64 = BigInt(2).pow(64)

    /** A policy id from the pool of 10: `[gen.cover.pool-contents]`. Two values that draw from the
      * pool share policies, so the "same key" branches of `Value.plus` run.
      */
    def genPooledPolicyId: Gen[ByteString] =
        Gen.choose(0, 9).map(i => ByteString.fromArray(Array.fill(28)(i.toByte)))

    /** An asset name from the pool of 10: `[gen.cover.pool-contents]`. */
    def genPooledAssetName: Gen[ByteString] =
        Gen.choose(0, 9).map(i => ByteString.fromString(i.toString))

    /** A positive amount, at most 2^64 (`[gen.value.amount-max]`), in three parts
      * (`[gen.cover.three-parts]`): 50 % 1 to 10^8, 25 % edges, 25 % any amount with a uniform bit
      * length.
      *
      * The edges are the boundaries of the CBOR integer encoding of `Data.I` (23/24, 255/256, 2^16,
      * 2^32), the largest `Long`, and 2^64 − 1 and 2^64.
      */
    def genPositiveAmount: Gen[BigInt] =
        Gen.frequency(
          2 -> Gen.choose(BigInt(1), BigInt(100_000_000)),
          1 -> Gen.oneOf(
            BigInt(1),
            BigInt(23),
            BigInt(24),
            BigInt(255),
            BigInt(256),
            BigInt(65_536),
            BigInt(1L << 32),
            BigInt(Long.MaxValue),
            TwoPow64 - 1,
            TwoPow64
          ),
          1 -> genMagnitude(BigInt(1), TwoPow64)
        )

    /** The generator behind `Arbitrary[Value]`: positive amounts (`[gen.value.positive]`), an
      * optional lovelace entry (`[gen.value.lovelace-optional]`), 30 % pooled keys
      * (`[gen.cover.pooled-keys]`).
      */
    def genValue: Gen[Value] =
        genValueOf(Gen.option(genPositiveAmount), genPositiveAmount)

    /** The value of `Arbitrary[TxOut]`: `genValue` with a lovelace entry of at least 1
      * (`[gen.value.output-lovelace]`, `[gen.value.output-gen]`).
      */
    def genOutputValue: Gen[Value] =
        genValueOf(genPositiveAmount.map(Some(_)), genPositiveAmount)

    /** A mint value: non-zero signed amounts and no lovelace entry (`[gen.value.mint-signed]`). */
    def genMintValue: Gen[Value] = {
        val nonZero =
            Gen.zip(Gen.oneOf(1, -1), genPositiveAmount).map((sign, amount) => amount * sign)
        genValueOf(Gen.const(None), nonZero)
    }

    /** Any value the Scala type can hold, with signed amounts in `[-2^64, 2^64]`
      * (`[gen.value.any-signed]`): the body of `Arbitrary[Value]` before D5, unchanged, for the
      * group laws of `ValueTest`.
      */
    def genAnyValue: Gen[Value] =
        for
            lovelace <- Gen.option(genLovelace)
            size <- Gen.frequency(
              (5, Gen.choose(0, 1)), // 0 to 10 policyId
              (3, Gen.choose(1, 10)) // 0 to 10 policyId
            )
            values <- Gen
                .listOfN(size, genAsset)
                .map(_.foldLeft(Value.zero)(Value.plus))
        yield lovelace match
            case None    => values
            case Some(l) => Value.plus(l, values)

    /** A signed value whose keys come only from `policies` × `tokens` (`[gen.value.over-keys]`).
      * Each key is present with chance 1/2, so two values share keys in most draws: the two-operand
      * laws of `ValueTest` use it (`[gen.value.two-operand-laws]`).
      */
    def genValueOver(policies: Seq[ByteString], tokens: Seq[ByteString]): Gen[Value] = {
        val keys = for policy <- policies; token <- tokens yield (policy, token)
        Gen.sequence[Seq[scala.Option[BigInt]], scala.Option[BigInt]](
          keys.map(_ => Gen.frequency(1 -> Gen.const(None), 1 -> genAmount.map(Some(_))))
        ).map { amounts =>
            val entries = keys.zip(amounts).collect { case ((policy, token), Some(amount)) =>
                (policy, token, amount)
            }
            valueFromEntries(entries)
        }
    }

    /** A value with 0 to 10 policies of 0 to 10 tokens. Policy ids and asset names are 30 % pooled.
      * A key drawn twice keeps its first amount, so an amount is never a sum of two draws.
      */
    private def genValueOf(lovelace: Gen[scala.Option[BigInt]], amount: Gen[BigInt]): Gen[Value] = {
        val count = Gen.frequency((5, Gen.choose(0, 1)), (3, Gen.choose(1, 10)))
        val policyId = Gen.frequency(3 -> genPooledPolicyId, 7 -> genPolicyId)
        val assetName = Gen.frequency(3 -> genPooledAssetName, 7 -> genAssetName)
        val policy =
            for
                id <- policyId
                tokens <- count.flatMap(Gen.listOfN(_, Gen.zip(assetName, amount)))
            yield tokens.map((name, value) => (id, name, value))
        for
            ada <- lovelace
            entries <- count.flatMap(Gen.listOfN(_, policy)).map(_.flatten)
        yield valueFromEntries(
          ada.map(value => (Value.adaPolicyId, Value.adaTokenName, value)).toSeq ++ entries
        )
    }

    /** A canonical value from (policy, token, amount) entries: the first amount of a key wins, zero
      * amounts are dropped, keys are sorted.
      */
    private def valueFromEntries(entries: Seq[(ByteString, ByteString, BigInt)]): Value = {
        val firstPerKey = entries.distinctBy((policy, token, _) => (policy, token))
        val byPolicy = firstPerKey.groupBy(_._1).toSeq.map { (policy, tokens) =>
            (policy, List.from(tokens.map((_, token, amount) => (token, amount))))
        }
        Value.fromList(List.from(byPolicy))
    }

    given Arbitrary[Value] = Arbitrary(genValue)
}
