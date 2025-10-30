package scalus.cardano.ledger.value.coin

import org.scalacheck.{Arbitrary, Gen as Gen0}
import scalus.cardano.ledger.value.coin.Coin
import spire.laws.arb.given
import spire.laws.gen
import spire.math.SafeLong

object Gen {
    object Arb {
        implicit val coinArb: Arbitrary[Coin.Coin] =
            Arbitrary(Gen0.choose(0L, Long.MaxValue).map(Coin.unsafeApply))

        implicit val coinUnboundedArb: Arbitrary[Coin.Unbounded] =
            Arbitrary(Arbitrary.arbitrary[SafeLong].map(Coin.Unbounded.apply))

        implicit val coinFractionalArb: Arbitrary[Coin.Fractional] =
            Arbitrary(gen.rational.map(Coin.Fractional(_)))
    }
}
