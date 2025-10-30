package scalus.cardano.ledger.value.multiasset

import org.scalacheck.Test.Parameters
import org.scalacheck.{Arbitrary, Prop, Properties}
import scalus.cardano.ledger.value.multiasset.MultiAsset
// import spire.implicits.*
import spire.syntax.all.*
import spire.laws.arb.given
import spire.math.{Rational, SafeLong}

import Arbitrary.arbitrary
import Gen.Arb.given
import Prop.forAll

/** These tests primarily test functions that mix underlying representations. Functions that test
  * within a single representation are test withing in the Laws module.
  */
object Property extends Properties("Coin/MultiAsset/Value") {
    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(10_000)
    }

//    property("MultiAsset.Unbounded subtracted from itself equals zero") =
//        forAll(arbitrary[MultiAsset.Unbounded]) { ma =>
//            ma - ma == MultiAsset.Unbounded.zero
//        }
//
//    property("MultiAsset.Fractional subtracted from itself equals zero") =
//        forAll(arbitrary[MultiAsset.Fractional]) { ma =>
//            ma - ma == MultiAsset.Fractional.zero
//        }
//
//    property("Scale bounded multiasset by integral an inverse fractional") =
//        forAll(arbitrary[MultiAsset], arbitrary[SafeLong].suchThat(sl => sl != 0)) { (ma, i) =>
//            ma.scaleIntegral(i).scaleFractional(Rational(1, i)).toMultiAsset == Right(ma)
//        }
}
