package scalus.cardano.ledger.value.multiasset

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen as Gen0}
import scalus.cardano.ledger.ArbitraryInstances.{given_Arbitrary_AssetName, given_Arbitrary_Hash}
import scalus.cardano.ledger.value.multiasset.MultiAsset.Inner
import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.MultiAsset
import scalus.cardano.ledger.{AssetName, PolicyId}
import scalus.cardano.ledger.value.coin.Gen.Arb.given
import scalus.cardano.ledger.value.multiasset.lib.Multiset
import spire.algebra.{AdditiveMonoid, Order}

import scala.collection.immutable
import scala.collection.immutable.SortedMap

object Gen {
    def genConfigurableMultiAssetFractional(
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen0[MultiAsset.Fractional] = {
        val innerArb = Arbitrary(genConfigurableInnerFractional(minAssets, maxAssets))
        genConfigurableMultiAssetPolymorphic(minPolicies, maxPolicies)(using innerArb)
            .map(MultiAsset.Fractional.apply)
    }

    def genConfigurableMultiAssetUnbounded(
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen0[MultiAsset.Unbounded] = {
        val innerArb = Arbitrary(genConfigurableInnerUnbounded(minAssets, maxAssets))
        genConfigurableMultiAssetPolymorphic(minPolicies, maxPolicies)(using innerArb)
            .map(MultiAsset.Unbounded.apply)
    }

    def genConfigurableMultiAsset(
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen0[MultiAsset] = {
        val innerArb = Arbitrary(genConfigurableInner(minAssets, maxAssets))
        genConfigurableMultiAssetPolymorphic(minPolicies, maxPolicies)(using
          innerArb = innerArb,
          iMonoid = Inner.AlgebraFull
        ).map(MultiAsset.apply)
    }

    def genConfigurableInnerFractional(
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen0[MultiAsset.Inner.Fractional] =
        genInnerPolymorphic[Coin.Fractional](minAssets, maxAssets)
            .map(MultiAsset.Inner.Fractional.apply)

    def genConfigurableInnerUnbounded(
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen0[MultiAsset.Inner.Unbounded] =
        genInnerPolymorphic[Coin.Unbounded](minAssets, maxAssets)
            .map(MultiAsset.Inner.Unbounded.apply)

    def genConfigurableInner(
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen0[MultiAsset.Inner] =
        genInnerPolymorphic[Coin](minAssets, maxAssets)(using cMonoid = Coin.AlgebraFull)
            .map(MultiAsset.Inner.apply)

    private def genConfigurableMultiAssetPolymorphic[
        I <: Inner | Inner.Unbounded | Inner.Fractional
    ](
        minPolicies: Int = 1,
        maxPolicies: Int = 8
    )(using
        innerArb: Arbitrary[I],
        iMonoid: AdditiveMonoid[I],
        kOrdering: Ordering[PolicyId]
    ): Gen0[Multiset[PolicyId, I, iMonoid.type, Order[PolicyId]]] = for {
        policies <- Gen0.choose(minPolicies, maxPolicies)
        list <- Gen0.containerOfN[List, (PolicyId, I)](
          policies,
          for {
              policy <- arbitrary[PolicyId]
              inner <- innerArb.arbitrary
          } yield policy -> inner
        )
    } yield Multiset(list.to(SortedMap))

    private def genInnerPolymorphic[C <: Coin | Coin.Unbounded | Coin.Fractional](
        minAssets: Int = 1,
        maxAssets: Int = 8
    )(using
        coinArb: Arbitrary[C],
        cMonoid: AdditiveMonoid[C]
    ): Gen0[Multiset[AssetName, C, cMonoid.type, Order[AssetName]]] = for {
        assets <- Gen0.choose(minAssets, maxAssets)
        list <- Gen0.containerOfN[List, (AssetName, C)](
          assets,
          for {
              assetName <- arbitrary[AssetName]
              coin <- coinArb.arbitrary
          } yield assetName -> coin
        )
    } yield Multiset(list.to(SortedMap))

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
