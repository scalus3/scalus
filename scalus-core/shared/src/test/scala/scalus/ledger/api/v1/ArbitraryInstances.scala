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

    given Arbitrary[Value] = Arbitrary {
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
    }
}
