package scalus.cardano.ledger.value.coin

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen as Gen0}
import scalus.cardano.ledger.value.coin.Coin
import spire.laws.arb.given
import spire.laws.gen
import spire.math.{Rational, SafeLong}

object Gen {
    def genRawWeights(minSize: Int = 1, maxSize: Int = 10): Gen0[NonEmptyList[Rational]] = {
        import spire.compat.fractional
        if minSize < 1 || maxSize < 1 then throw IllegalArgumentException("Bad generator args")
        else
            for {
                size <- Gen0.choose(minSize, maxSize)
                result <- Gen0
                    .containerOfN[List, Rational](size, gen.rational.suchThat(_ >= Rational(0)))
                    .suchThat(_.sum > 0)
            } yield NonEmptyList(result.head, result.tail)
    }

    def genBadRawWeights(minSize: Int = 0, maxSize: Int = 10): Gen0[NonEmptyList[Rational]] = {
        import spire.compat.fractional
        if minSize < 1 || maxSize < 1 then throw IllegalArgumentException("Bad generator args")
        else
            for {
                size <- Gen0.choose(minSize, maxSize)
                result <- Gen0
                    .containerOfN[List, Rational](size, gen.rational.suchThat(_ >= Rational(0)))
                    .suchThat(_.sum > 0)
            } yield NonEmptyList(result.head, result.tail)
    }

    def genNormalizedWeights(
        minSize: Int = 1,
        maxSize: Int = 10
    ): Gen0[Distribution.NormalizedWeights] =
        if minSize < 1 || maxSize < 1 then throw IllegalArgumentException("Bad generator args")
        else
            for {
                rawWeights <- genRawWeights(minSize, maxSize)
            } yield Distribution.unsafeNormalizeWeights(rawWeights)

    object Arb {
        implicit val coinArb: Arbitrary[Coin.Coin] =
            Arbitrary(Gen0.choose(0L, Long.MaxValue).map(Coin.unsafeApply))

        implicit val coinUnboundedArb: Arbitrary[Coin.Unbounded] =
            Arbitrary(Arbitrary.arbitrary[SafeLong].map(Coin.Unbounded.apply))

        implicit val coinFractionalArb: Arbitrary[Coin.Fractional] =
            Arbitrary(gen.rational.map(Coin.Fractional(_)))

        implicit val normalizedWeights: Arbitrary[Distribution.NormalizedWeights] =
            Arbitrary(genNormalizedWeights())
    }
}
