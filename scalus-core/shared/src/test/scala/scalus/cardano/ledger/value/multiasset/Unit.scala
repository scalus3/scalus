package scalus.cardano.ledger.value.multiasset

import algebra.CommutativeGroup
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.AssetName
import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.MultiAsset
import spire.laws.GroupLaws
import spire.math.SafeLong
import spire.syntax.all.*

import scala.collection.immutable.TreeMap

class Unit extends AnyFunSuite {
    val mai: MultiAsset.Inner.Unbounded = MultiAsset.Inner.Unbounded.Unbounded(
        CanonicalSortedMap.fromSortedMap(TreeMap(
            AssetName.fromHex("") -> Coin
                .Unbounded(SafeLong(BigInt(1)))
          )
        )
      )

    test("MultiAssetInner Left Distributivity with 0"){
        assert(0 *: mai + 0 *: mai == 0 *: (mai + mai))
    }

    test("MultiAsset.Unbounded subtracted from itself equals zero") {
        implicit val alg: CommutativeGroup[MultiAsset.Inner.Unbounded] =
            MultiAsset.Inner.Unbounded.algebra.additive
        assert(alg.empty === (alg.inverse(mai) |+| mai))
    }

    test("MultiAsset.Unbounded subtracted from itself equals zero, 2") {
        implicit val alg: CommutativeGroup[MultiAsset.Inner.Unbounded] =
            MultiAsset.Inner.Unbounded.algebra.additive
        assert(alg.empty === mai - mai)
    }

    test("MultiAsset.Unbounded.Inner added to zero is identity") {
        assert(mai + MultiAsset.Inner.Unbounded.zero === mai)
        assert(MultiAsset.Inner.Unbounded.zero + mai === mai)
    }

    test("MultiAsset.Unbounded.Inner minus zero") {
        assert(mai - MultiAsset.Inner.Unbounded.zero === mai)
        assert(MultiAsset.Inner.Unbounded.zero - mai === -mai)
    }

    import Gen.Arb.given

    test("left inverse") {
        GroupLaws[MultiAsset.Inner.Unbounded]
            .group(using MultiAsset.Inner.Unbounded.algebra.additive)
            .props
            .head
            ._2
            .check()
    }
}
