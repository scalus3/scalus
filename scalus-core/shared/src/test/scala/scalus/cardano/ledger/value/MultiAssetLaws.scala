package scalus.cardano.ledger.value

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import spire.laws.{OrderLaws, PredicateFromMonoid, VectorSpaceLaws}
import spire.math.{Rational, SafeLong}

class MultiAssetLaws extends AnyFunSuite with FunSuiteDiscipline with Checkers {
    import Gen.Arb.given 
    import spire.laws.arb.given 
    
    checkAll("MultiAsset Partial Order", OrderLaws[MultiAsset].partialOrder)
    
    checkAll("MultiAsset Unbounded Partial Order", OrderLaws[MultiAsset.Unbounded].partialOrder)
    
    checkAll("MultiAsset Fractional Partial Order", OrderLaws[MultiAsset.Fractional].partialOrder)
    
    checkAll("MultiAsset Unbounded CModule", VectorSpaceLaws[MultiAsset.Unbounded, SafeLong].cModule)
    
    checkAll("MultiAsset Fractional VectorSpace", VectorSpaceLaws[MultiAsset.Fractional, Rational].vectorSpace)
}
