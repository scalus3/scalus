package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

class TaggedSetTest extends AnyFunSuite {

    test("TaggedSet") {
        assert(TaggedSet.from(Seq(1, 2, 3, 4)).toIndexedSeq === IndexedSeq(1, 2, 3, 4))
        assert(TaggedSet.from(Seq(4, 3, 2, 1)).toIndexedSeq === IndexedSeq(4, 3, 2, 1))
        assert(TaggedSet.from(Seq(1, 2, 4, 3, 1)).toIndexedSeq === IndexedSeq(1, 2, 4, 3))
    }

    test("TaggedOrderedSet") {
        assert(TaggedSet.from(Seq(1, 2, 3, 4)).toIndexedSeq === IndexedSeq(1, 2, 3, 4))
        assert(TaggedSet.from(Seq(4, 3, 2, 1)).toIndexedSeq === IndexedSeq(4, 3, 2, 1))
        assertThrows[IllegalArgumentException](
          TaggedOrderedSet.from(Seq(1, 2, 4, 3, 1)).toIndexedSeq === IndexedSeq(1, 2, 4, 3)
        )
    }

    test("TaggedSortedSet") {
        assert(TaggedSortedSet.from(Seq(1, 2, 3, 4)).toSeq === Seq(1, 2, 3, 4))
        assert(TaggedSortedSet.from(Seq(4, 3, 2, 1)).toSeq === Seq(1, 2, 3, 4))
        assertThrows[IllegalArgumentException](
          TaggedSortedSet.from(Seq(1, 2, 4, 3, 1)).toSeq === Seq(1, 2, 4, 3)
        )
    }

}
