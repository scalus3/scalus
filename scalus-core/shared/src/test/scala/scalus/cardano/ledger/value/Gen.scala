package scalus.cardano.ledger.value

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen as Gen0}
import scalus.cardano.ledger.ArbitraryInstances.{*, given}
import scalus.cardano.ledger.value.Coin.Unbounded
import scalus.cardano.ledger.value.MultiAsset.Inner
import scalus.cardano.ledger.{AssetName, PolicyId}
import spire.laws.arb.given
import spire.laws.gen
import spire.math.SafeLong

import scala.collection.immutable
import scala.collection.immutable.SortedMap

object Gen {
    import Arb.*

    def genConfigurableMultiAssetFractional(
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen0[MultiAsset.Fractional] = {
        for {
            poly <- genConfigurableMultiAssetPolymorphic[Coin.Fractional](
              minPolicies,
              maxPolicies,
              minAssets,
              maxAssets
            )
            innerDone = poly.map(kv => (kv._1, Inner.Fractional(kv._2)))
        } yield MultiAsset.Fractional(innerDone)
    }

    def genConfigurableMultiAssetUnbounded(
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen0[MultiAsset.Unbounded] = {
        for {
            poly <- genConfigurableMultiAssetPolymorphic[Coin.Unbounded](
              minPolicies,
              maxPolicies,
              minAssets,
              maxAssets
            )
            innerDone = poly.map(kv => (kv._1, Inner.Unbounded(kv._2)))
        } yield MultiAsset.Unbounded(innerDone)
    }

    def genConfigurableMultiAsset(
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen0[MultiAsset] = {
        for {
            poly <- genConfigurableMultiAssetPolymorphic[Coin](
              minPolicies,
              maxPolicies,
              minAssets,
              maxAssets
            )
            innerDone = poly.map(kv => (kv._1, Inner(kv._2)))
        } yield MultiAsset(innerDone)
    }

    def genConfigurableInnerFractional(
                                          minAssets: Int = 1,
                                          maxAssets: Int = 8
                                      ): Gen0[MultiAsset.Inner.Fractional] =
        for {
            poly <- genInnerPolymorphic[Coin.Fractional](minAssets, maxAssets)
        } yield MultiAsset.Inner.Fractional(poly)

    def genConfigurableInnerUnbounded(
                                         minAssets: Int = 1,
                                         maxAssets: Int = 8
                                     ): Gen0[MultiAsset.Inner.Unbounded] =
        for {
            poly <- genInnerPolymorphic[Coin.Unbounded](minAssets, maxAssets)
        } yield MultiAsset.Inner.Unbounded(poly)

    def genConfigurableInner(
                                minAssets: Int = 1,
                                maxAssets: Int = 8
                            ): Gen0[MultiAsset.Inner] =
        for {
            poly <- genInnerPolymorphic[Coin](minAssets, maxAssets)
        } yield MultiAsset.Inner(poly)

    private def genConfigurableMultiAssetPolymorphic[A](
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
    )(using coinArb: Arbitrary[A]): Gen0[SortedMap[PolicyId, SortedMap[AssetName, A]]] = {
        given Arbitrary[immutable.Map[AssetName, A]] = Arbitrary(
          genMapOfSizeFromArbitrary(minAssets, maxAssets)
        )

        for
            policies <- Gen0.choose(minPolicies, maxPolicies)
            result: immutable.Map[PolicyId, SortedMap[AssetName, A]] <- Gen0
                .mapOfN(
                  policies,
                  for
                      policyId <- arbitrary[PolicyId]
                      assets <- arbitrary[immutable.Map[AssetName, A]]
                  yield (policyId, assets.to(immutable.TreeMap))
                )
        yield SortedMap.from(result)
    }

    private def genInnerPolymorphic[A](
        minAssets: Int = 1,
        maxAssets: Int = 8
    )(using coinArb: Arbitrary[A]): Gen0[SortedMap[AssetName, A]] = {

        genMapOfSizeFromArbitrary[AssetName, A](minAssets, maxAssets).map(SortedMap.from)
    }

    object Arb {
        implicit val coinArb: Arbitrary[Coin.Coin] = Arbitrary(
          Gen0.choose(0L, Long.MaxValue).map(Coin.unsafeApply)
        )
        implicit val coinUnboundedArb: Arbitrary[Coin.Unbounded] =
            Arbitrary(Arbitrary.arbitrary[SafeLong].map(Coin.Unbounded.apply))
        implicit val coinFractionalArb: Arbitrary[Coin.Fractional] =
            Arbitrary(gen.rational.map(Coin.Fractional(_)))

        implicit val multiAssetArb: Arbitrary[MultiAsset] =
            Arbitrary(genConfigurableMultiAsset())

        implicit val multiAssetUnboundedArb: Arbitrary[MultiAsset.Unbounded] =
            Arbitrary(genConfigurableMultiAssetUnbounded())

        implicit val multiAssetFractionalArb: Arbitrary[MultiAsset.Fractional] =
            Arbitrary(genConfigurableMultiAssetFractional())

        implicit val multiAssetInnerArb: Arbitrary[MultiAsset.Inner] =
            Arbitrary(genConfigurableInner())

        implicit val multiAssetInnerUnboundedArb: Arbitrary[MultiAsset.Inner.Unbounded] =
            Arbitrary(genConfigurableInnerUnbounded())

        implicit val multiAssetInnerFractionalArb: Arbitrary[MultiAsset.Inner.Fractional] =
            Arbitrary(genConfigurableInnerFractional())
        
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
