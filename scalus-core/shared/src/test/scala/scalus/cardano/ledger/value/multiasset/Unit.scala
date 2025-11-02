package scalus.cardano.ledger.value.multiasset

import algebra.CommutativeGroup
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{AssetName, ScriptHash}
import scalus.cardano.ledger.value.coin.Coin
import scalus.cardano.ledger.value.multiasset.MultiAsset
import scalus.cardano.ledger.value.multiasset.Multiset.*
import spire.algebra.{AdditiveMonoid, Order}
import spire.laws.GroupLaws
import spire.math.SafeLong
import spire.syntax.all.*

import scala.collection.immutable.SortedMap

/** These tests use the org.scalactic.Equals instances when comparing things with `===`. */
class Unit extends AnyFunSuite {
    val sm = SortedMap(
      ScriptHash.fromHex("ff".repeat(28)) -> SortedMap(
        AssetName.fromHex("") -> Coin.Unbounded(SafeLong(BigInt(1)))
      )
    )

    val ma: MultiAsset.Unbounded = MultiAsset.Unbounded(sm)

    val mai: MultiAsset.Inner.Unbounded = MultiAsset.Inner.Unbounded(sm.head._2)

    test("MultiAsset.Unbounded subtracted from itself equals zero") {
        implicit val alg: CommutativeGroup[MultiAsset.Unbounded] =
            MultiAsset.Unbounded.Algebra.additive
        assert(alg.empty === (alg.inverse(ma) |+| ma))
    }

    test("MultiAsset.Unbounded subtracted from itself equals zero, 2") {
        implicit val alg: CommutativeGroup[MultiAsset.Unbounded] =
            MultiAsset.Unbounded.Algebra.additive
        assert(alg.empty === ma - ma)
    }

    test("MultiAsset.Unbounded added to zero is identity") {
        assert(ma + MultiAsset.Unbounded.zero === ma)
        assert(MultiAsset.Unbounded.zero + ma === ma)
    }

    test("MultiAsset.Unbounded minus zero") {
        assert(ma - MultiAsset.Unbounded.zero === ma)
        assert(MultiAsset.Unbounded.zero - ma === -ma)
    }

    import Gen.Arb.given

    test("left inverse") {
        GroupLaws[MultiAsset.Inner.Unbounded]
            .group(using MultiAsset.Inner.Unbounded.Algebra.additive)
            .props
            .head
            ._2
            .check()
    }

    // ===================================
    // Multiset
    // ===================================

    test("A multiset only accepts its specific additive monoid.") {
        given goodMonoid: AdditiveMonoid[Int] = new AdditiveMonoid[Int] {
            override def zero: Int = 0
            override def plus(x: Int, y: Int): Int = x + y
        }

        // multiset1 uses a semantically nonsense, but lawful (ignoring boundedness) "additive" monoid on Int:
        // 1 as the zero element, multiplication as the monoidal operation
        val multiset1: Multiset[String, Int, goodMonoid.type, Order[String]] =
            Multiset(SortedMap("foo" -> 0, "bar" -> 1))

        // multiset2 uses the sensible additive monoid, namely addition on integers
        val multiset2: Multiset[String, Int, goodMonoid.type, Order[String]] = {
            given badMonoid: AdditiveMonoid[Int] = new AdditiveMonoid[Int] {
                override def zero: Int = 1
                override def plus(x: Int, y: Int): Int = x * y
            }
            Multiset(SortedMap("foo" -> 0, "bar" -> 1))
            // Trying to force the `badMonoid` into the multiset should
            // give a compiler error:
            // Multiset(SortedMap("foo" -> 0, "bar" -> 1))(using vMonoid = badMonoid)
            // Whereas using `goodMonoid` compiles:
            // Multiset(SortedMap("foo" -> 0, "bar" -> 1))(using vMonoid = goodMonoid)
        }

        // Both maps are of the same type and were constructed with the same elements, but had a different implicit
        // AdditiveMonoid in scope at the time of construction; thus they are not equal
        assert(multiset1 === multiset2)
    }

    // Demonstration that TreeMap may behave strangely if different implicits are given, despite being of the same
    // type.
    test("A multiset only accepts its specific key ordering.") {
        import spire.implicits.IntAlgebra

        given goodOrder: Order[String] = spire.implicits.StringOrder

        // multiset1 uses a semantically nonsense, but lawful (ignoring boundedness) "additive" monoid on Int:
        // 1 as the zero element, multiplication as the monoidal operation
        val multiset1: Multiset[String, Int, AdditiveMonoid[Int], goodOrder.type] =
            Multiset(SortedMap("foo" -> 0, "bar" -> 1))

        // multiset2 uses the sensible additive monoid, namely addition on integers
        val multiset2: Multiset[String, Int, AdditiveMonoid[Int], goodOrder.type] = {
            given badOrder: Ordering[String] = (x: String, y: String) => {
                if goodOrder.gt(x, y)
                then -1
                else if goodOrder.lt(x, y)
                then 1
                else 0
            }
            Multiset(SortedMap("foo" -> 0, "bar" -> 1))
            // Trying to force the `badOrder` into the multiset should
            // give a compiler error:
            // Multiset(SortedMap("foo" -> 0, "bar" -> 1))(using kOrder = badOrder)
            // Whereas using `goodOrder` compiles:
            // Multiset(SortedMap("foo" -> 0, "bar" -> 1))(using kOrder = goodOrder)
        }

        // This will fail, even though the TreeMaps are of the same type.
        assert(multiset1.multiplicityMap.keys.toList === multiset2.multiplicityMap.keys.toList)
    }

}
