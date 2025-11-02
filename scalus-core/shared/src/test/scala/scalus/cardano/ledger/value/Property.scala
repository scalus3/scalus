package scalus.cardano.ledger.value

import org.scalacheck.Test.Parameters
import org.scalacheck.Properties
//import org.scalacheck.{Arbitrary, Prop, Properties, Gen as Gen0}
//import spire.laws.arb.given
//import spire.math.{Rational, SafeLong}

//    import Arbitrary.arbitrary
//    import Gen.Arb.given
//    import Prop.forAll

/** These tests primarily test functions that mix underlying representations. Functions that test
  * within a single representation are test withing in the Laws module.
  *
  * These tests use the cats.Eq instances when comparing things with `===`.
  */
object Property extends Properties("Coin/MultiAsset/Value") {
    override def overrideParameters(p: Parameters): Parameters = {
        p.withMinSuccessfulTests(10_000)
    }
}
