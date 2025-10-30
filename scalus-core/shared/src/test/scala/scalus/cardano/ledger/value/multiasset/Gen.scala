package scalus.cardano.ledger.value.multiasset

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen as Gen0}
import scalus.cardano.ledger.ArbitraryInstances.{*, given}
import scalus.cardano.ledger.value.multiasset.MultiAsset.Inner
import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.MultiAsset
import scalus.cardano.ledger.{AssetName, PolicyId}

import scalus.cardano.ledger.value.coin.Gen.Arb.given

import scala.collection.immutable
import scala.collection.immutable.SortedMap

import spire.implicits.MapEq as _
import spire.implicits.{MapMonoid as _, MapCSemiring as _}
import spire.implicits.{MapCRng as _, MapGroup as _}
import spire.implicits.{ MapVectorSpace as _, MapInnerProductSpace as _}

object Gen {
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
                  yield (policyId, assets.to(SortedMap))
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
    }
}
