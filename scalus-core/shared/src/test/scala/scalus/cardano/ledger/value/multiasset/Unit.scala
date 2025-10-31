package scalus.cardano.ledger.value.multiasset

import algebra.CommutativeGroup
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.AssetName
import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.MultiAsset
import spire.algebra.AdditiveMonoid
import spire.laws.GroupLaws
import spire.math.SafeLong
import spire.syntax.all.*

import scala.collection.immutable.{SortedMap, TreeMap}

class Unit extends AnyFunSuite {
    val mai: MultiAsset.Inner.Unbounded = MultiAsset.Inner.Unbounded(
      SortedMap(
        AssetName.fromHex("") -> Coin
            .Unbounded(SafeLong(BigInt(1)))
      )
    )

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


    // ===================================
    // Ignored tests, for demonstration purposes
    // ===================================

    // A contrived example showing that different law-abiding additive monoids can lead to
    // different interpretations of the "zero" element
    ignore("canoncial sorted map should not allow different monoids") {
      // map1 uses a semantically nonsense, but lawful (ignoring boundedness) "additive" monoid on Int:
      // 1 as the zero element, multiplication as the monoidal operation
      val map1: CanonicalSortedMap.CanonicalSortedMap[String, Int] = {
        implicit val iMonoid1: AdditiveMonoid[Int] = new AdditiveMonoid[Int] {
          override def zero: Int = 1

          override def plus(x: Int, y: Int): Int = x * y
        }
        CanonicalSortedMap(SortedMap("foo" -> 0, "bar" -> 1))
      }

      // map2 uses the sensible additive monoid, namely addition on integers
      val map2: CanonicalSortedMap.CanonicalSortedMap[String, Int] = {
        implicit val iMonoid2: AdditiveMonoid[Int] = new AdditiveMonoid[Int] {
          override def zero: Int = 0

          override def plus(x: Int, y: Int): Int = x + y
        }
        CanonicalSortedMap(SortedMap("foo" -> 0, "bar" -> 1))
      }

      // Both maps are of the same type and were constructed with the same elements, but had a different implicit
      // AdditiveMonoid in scope at the time of construction; thus they are not equal
      assert(map1 === map2)
    }
    
    // Demonstration that TreeMap may behave strangely if different implicits are given, despite being of the same
    // type.
    ignore("TreeMap ordering is not unique for a type") {
      val defaultOrdering = Ordering[String]

      // The first treemap is constructed using the default lexiographic ordering
      val tm1: TreeMap[String, Int] = {
        TreeMap("a" -> 1, "b" -> 2, "c" -> 3)(using defaultOrdering)
      }

      // The second treemap is constructed using its dual -- reverse lexicographic
      val tm2: TreeMap[String, Int] = {
        implicit val ord2: Ordering[String] = new Ordering[String] {
          override def compare(x: String, y: String): Int = {
            if defaultOrdering.gt(x, y)
            then -1
            else if defaultOrdering.lt(x, y)
            then 1
            else 0
          }
        }
        TreeMap("a" -> 1, "b" -> 2, "c" -> 3)
      }

      // This will fail, even though the TreeMaps are of the same type.
      assert(tm1.keys.toList === tm2.keys.toList)
    }


}
