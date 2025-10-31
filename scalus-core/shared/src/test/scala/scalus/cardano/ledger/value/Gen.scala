package scalus.cardano.ledger.value

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import scalus.cardano.ledger.value.coin.Coin.Unbounded
import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.MultiAsset

import coin.Gen.Arb.given
import multiasset.Gen.Arb.given

object Gen {
    object Arb {
        implicit val valueArb: Arbitrary[Value] = Arbitrary(
          for {
              coin <- arbitrary[Coin]
              ma <- arbitrary[MultiAsset]
          } yield Value(coin, ma)
        )

        implicit val valueUnboundedArb: Arbitrary[Value.Unbounded] = Arbitrary(
          for {
              coin <- arbitrary[Unbounded]
              ma <- arbitrary[MultiAsset.Unbounded]
          } yield Value.Unbounded(coin, ma)
        )

        implicit val valueFractionalArb: Arbitrary[Value.Fractional] = Arbitrary(
          for {
              coin <- arbitrary[Coin.Fractional]
              ma <- arbitrary[MultiAsset.Fractional]
          } yield Value.Fractional(coin, ma)
        )

    }
}
