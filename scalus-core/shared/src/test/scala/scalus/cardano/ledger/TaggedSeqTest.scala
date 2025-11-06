package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

class TaggedSeqTest extends AnyFunSuite {

    test("TaggedSet") {
        assert(TaggedOrderedSet(2, 1, 4, 3).toSeq === Seq(2, 1, 4, 3))
        assert(TaggedOrderedSet(2, 1, 4, 3, 1).toSeq === Seq(2, 1, 4, 3))
    }

    test("TaggedOrderedSet") {
        assert(TaggedOrderedSet(2, 1, 4, 3).toSeq === Seq(2, 1, 4, 3))
        assertThrows[IllegalArgumentException](
          TaggedOrderedStrictSet(2, 1, 4, 3, 1).toSeq === Seq(2, 1, 4, 3)
        )
    }

    test("TaggedSortedSet") {
        assert(TaggedSortedSet(2, 1, 4, 3, 1).toSet === Set(1, 2, 3, 4))
    }

    test("TaggedSortedMap") {
        given TaggedSortedMap.KeyOf[Int, Int] = identity(_)
        assert(TaggedSortedMap[Int, Int](2, 1, 4, 3, 1).toSet === Set(1, 2, 3, 4))
    }

    test("TaggedSortedStrictMap") {
        given TaggedSortedStrictMap.KeyOf[Int, Int] = identity(_)
        assert(TaggedSortedStrictMap[Int, Int](2, 1, 4, 3).toSet === Set(1, 2, 3, 4))
        assertThrows[IllegalArgumentException](
          TaggedSortedStrictMap[Int, Int](2, 1, 4, 3, 1).toSet === Set(1, 2, 3, 4)
        )
    }

}
