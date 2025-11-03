package scalus.cardano.ledger.value.multiasset

import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Prop, Properties}
import scalus.cardano.ledger.value.multiasset.MultiAsset
import spire.syntax.all.*
import Arbitrary.arbitrary
import Gen.Arb.given
import scalus.cardano.ledger.value.coin.Gen.genNonZeroSafeLong
import Prop.forAll
import cats.implicits.catsSyntaxEither

/** These tests primarily test functions that mix underlying representations. Functions that test
  * within a single representation are test withing in the Laws module.
  *
  * These tests use the cats.Eq instances when comparing things with `===`.
  */
object Property extends Properties("Coin/MultiAsset/Value") {
    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(10_000)
    }

    property("MultiAsset.Unbounded subtracted from itself equals zero") =
        forAll(arbitrary[MultiAsset.Unbounded]) { ma =>
            ma - ma === MultiAsset.Unbounded.zero
        }

    property("MultiAsset.Fractional subtracted from itself equals zero") =
        forAll(arbitrary[MultiAsset.Fractional]) { ma =>
            ma - ma === MultiAsset.Fractional.zero
        }

    property("Scale bounded multiasset by integral an inverse fractional") =
        forAll(arbitrary[MultiAsset], genNonZeroSafeLong) { (ma, i) =>
            (ma *~ i /~ i).toMultiAsset === Right(ma)
        }
}
