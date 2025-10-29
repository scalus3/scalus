package scalus.cardano.ledger.value

import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Gen as Gen0, Prop, Properties}
import spire.implicits.*
import spire.laws.arb.given
import spire.math.SafeLong

object CoinProperty extends Properties("Coin") {
    import Arbitrary.arbitrary
    import Gen.Arb.given
    import Prop.forAll

    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(10_000)
    }

    property("∀ (l : Long), l >0 => Coin(l) round-trips") = forAll(Gen0.choose(0L, Long.MaxValue)) {
        l => {
            val c = Coin(l)
            c.isRight && c.map(_.underlying) == Right(l)
        }
    }

    property("∀ (c : Coin) => Coin(c.underlying) == Right(c)") =
        forAll(arbitrary[Coin]){ c => Coin(c.underlying) == Right(c)}


    property("coinA + coinB == coinB + coinA") =
        forAll(arbitrary[Coin], arbitrary[Coin]){ (a, b) => a + b == b + a}

    property("∀ (sl : SafeLong , c: Coin) => c.scaleIntegral(sl) == c.scaleFractional(u.toRational).toUnbounded") =
       forAll(Arbitrary.arbitrary[SafeLong], arbitrary[Coin]){

         (sl, c) => {
           c.scaleIntegral(sl) == c.scaleFractional(sl.toRational).toUnbounded
         }
       }

    property("∀ (u : Unbounded) => u.toFractional.toUnbounded == u)") =
        forAll(Arbitrary.arbitrary[Coin.Unbounded]){
            u => {
              u.toCoinFractional.toUnbounded === u
            }
        }
}
