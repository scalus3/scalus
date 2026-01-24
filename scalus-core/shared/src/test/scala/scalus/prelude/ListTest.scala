package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.{`???`, asScalus, fail, identity, Eq, List, Option, Ord, Order, SortedMap}
import scalus.testing.kit.EvalTestKit

class ListTest extends AnyFunSuite with EvalTestKit {

    test("empty") {
        assertEvalEq(List.empty[BigInt], Nil)

        assert(scala.List.empty[BigInt].asScalus === List.empty[BigInt])
        assert(List.empty[BigInt].asScala == scala.List.empty[BigInt])
    }

    test("single") {
        check { (value: BigInt) =>
            val scalusResult = List.single(value)
            val scalaResult = scala.List(value)

            scalusResult === Cons(
              value,
              Nil
            ) && scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.single(BigInt(1)), Cons(BigInt(1), Nil))
    }

    test("apply") {
        check { (seq: scala.Seq[BigInt]) =>
            val scalusResult = List(seq*)
            val scalaResult = scala.List(seq*)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(
          List(BigInt(1), BigInt(2), BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("apply list of pairs") {
        assertEvalEq(
          List((BigInt(1), BigInt(2)), (BigInt(3), BigInt(4))),
          Cons((BigInt(1), BigInt(2)), Cons((BigInt(3), BigInt(4)), Nil))
        )
    }

    test("from IterableOnce") {
        check { (seq: scala.Seq[BigInt]) =>
            val scalusResult = List.from(seq)
            val scalaResult = scala.List.from(seq)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("from java.lang.Iterable") {
        check { (seq: scala.Seq[BigInt]) =>
            import scala.jdk.CollectionConverters.*

            val scalusResult = List.from(seq.asJava)
            val scalaResult = scala.List.from(seq)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("range") {
        forAll(bigIntRangeGen) { (start: BigInt, end: BigInt) =>
            val scalusResult = List.range(start, end - 1)
            val scalaResult = scala.List.range(start, end)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalEq(List.range(0, 0), List.single(BigInt(0)))
        assertEvalEq(List.range(1, 3), Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))))
        assertEvalEq(List.range(0, -1), List.empty[BigInt])
    }

    test("rangeUntil") {
        forAll(bigIntRangeGen) { (start: BigInt, end: BigInt) =>
            val scalusResult = List.rangeUntil(start, end)
            val scalaResult = scala.List.range(start, end)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalEq(List.rangeUntil(0, 1), List.single(BigInt(0)))
        assertEvalEq(List.rangeUntil(1, 4), Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))))
        assertEvalEq(List.rangeUntil(0, 0), List.empty[BigInt])
        assertEvalEq(List.rangeUntil(0, -1), List.empty[BigInt])
    }

    test("fill") {
        val generator: Gen[(BigInt, BigInt)] =
            for
                value <- Arbitrary.arbitrary[BigInt]
                times <- bigIntGen
            yield (value, times)

        forAll(generator) { (value: BigInt, times: BigInt) =>
            val scalusResult = List.fill(value, times)
            val scalaResult = scala.List.fill(times.toInt)(value)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalEq(List.fill(BigInt(1), 1), List.single(BigInt(1)))

        assertEvalEq(
          List.fill(BigInt(1), 3),
          Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil)))
        )

        assertEvalEq(List.fill(BigInt(1), 0), List.empty[BigInt])

        assertEvalEq(List.fill(BigInt(1), -1), List.empty[BigInt])
    }

    test("map2") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = List.map2(list1, list2)(_ + _)
            val scalaResult = list1.asScala.zip(list2.asScala).map { case (a, b) => a + b }

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        check { (list: List[BigInt]) =>
            List.map2(list, List.empty[BigInt])(_ + _) === List.empty[BigInt] &&
            List.map2(List.empty[BigInt], list)(_ + _) === List.empty[BigInt]
        }

        assertEvalEq(
          List.map2(Cons(BigInt(1), Cons(BigInt(2), Nil)), List.empty[BigInt])(_ + _),
          List.empty[BigInt]
        )

        assertEvalEq(
          List.map2(List.empty[BigInt], Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _),
          List.empty[BigInt]
        )

        assertEvalEq(
          List.map2(Cons(BigInt(1), Cons(BigInt(2), Nil)), Cons(BigInt(3), Cons(BigInt(4), Nil)))(
            _ + _
          ),
          Cons(BigInt(4), Cons(BigInt(6), Nil))
        )
    }

    test("ToData <-> FromData") {
        check { (list: List[BigInt]) =>
            val data = list.toData
            val fromDataList = fromData[List[BigInt]](data)

            fromDataList === list
        }

        assertEvalEq(
          fromData[List[BigInt]](List.empty[BigInt].toData),
          List.empty[BigInt]
        )

        assertEvalEq(
          fromData[List[BigInt]](List.single(BigInt(1)).toData),
          List.single(BigInt(1))
        )

        assertEvalEq(
          fromData[List[BigInt]](Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))).toData),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("Eq") {
        check { (list: List[BigInt]) => list === list }

        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 === list2
            val scalaResult = list1.asScala == list2.asScala

            scalusResult === scalaResult
        }

        assertEvalEq(List.empty[BigInt], List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)), List.single(BigInt(1)))

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )

        assertEvalNotEq(
          List.empty[BigInt],
          List.single(BigInt(1))
        )

        assertEvalNotEq(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(BigInt(0), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("Ord") {
        check { (list: List[BigInt]) => (list <=> list).isEqual }

        check { (list1: List[BigInt], list2: List[BigInt]) =>
            import scala.Ordering.Implicits.given

            val scalusResult = list1 <=> list2
            val scalaResult =
                summon[Ordering[scala.Seq[BigInt]]].compare(list1.asScala, list2.asScala) match
                    case 0          => Order.Equal
                    case x if x < 0 => Order.Less
                    case _          => Order.Greater

            scalusResult === scalaResult
        }

        assertEval((List.empty[BigInt] <=> List.empty[BigInt]).isEqual)

        assertEval((List.single(BigInt(1)) <=> List.single(BigInt(1))).isEqual)

        assertEval(
          (
            Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))) <=>
                Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          ).isEqual
        )

        assertEval((List.empty[BigInt] <=> List.single(BigInt(1))).isLess)

        assertEval((List.single(BigInt(1)) <=> List.single(BigInt(2))).isLess)

        assertEval(
          (
            Cons(BigInt(1), Cons(BigInt(2), Nil)) <=>
                Cons(BigInt(1), Cons(BigInt(3), Nil))
          ).isLess
        )

        assertEval((List.single(BigInt(1)) <=> List.empty[BigInt]).isGreater)

        assertEval((List.single(BigInt(2)) <=> List.single(BigInt(1))).isGreater)

        assertEval(
          (
            Cons(BigInt(1), Cons(BigInt(3), Nil)) <=>
                Cons(BigInt(1), Cons(BigInt(2), Nil))
          ).isGreater
        )
    }

    test("quicksort") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.quicksort
            val scalaResult = list.asScala.sorted

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].quicksort, List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).quicksort, List.single(BigInt(1)))

        assertEvalEq(
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))).quicksort,
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("flatten") {
        given [T: Arbitrary]: Arbitrary[List[T]] = Arbitrary {
            for {
                size <- Gen.choose(0, 10)
                list <- Gen.listOfN(size, Arbitrary.arbitrary[T])
            } yield list.asScalus
        }

        check { (list: List[List[BigInt]]) =>
            val scalusResult = list.flatten
            val scalaResult = list.asScala.flatMap(_.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[List[BigInt]].flatten, List.empty[BigInt])

        assertEvalEq(
          List.single(Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil)))).flatten,
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil)))
        )

        assertEvalEq(
          Cons[List[BigInt]](
            Cons(BigInt(1), Cons(BigInt(2), Nil)),
            List.single(List.single(BigInt(3)))
          ).flatten,
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("isEmpty") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.isEmpty
            val scalaResult = list.asScala.isEmpty

            scalusResult === scalaResult
        }

        assertEval(List.empty[BigInt].isEmpty)
        assertEval(!List.single(BigInt(1)).isEmpty)
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).isEmpty)
    }

    test("nonEmpty") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.nonEmpty
            val scalaResult = list.asScala.nonEmpty

            scalusResult === scalaResult
        }

        assertEval(!List.empty[BigInt].nonEmpty)
        assertEval(List.single(BigInt(1)).nonEmpty)
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).nonEmpty)
    }

    test("isDefinedAt") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = list.isDefinedAt(index)
            val scalaResult = list.asScala.isDefinedAt(index.toInt)

            assert(scalusResult === scalaResult)
        }

        assertEval(!List.empty[BigInt].isDefinedAt(0))
        assertEval(List.single(BigInt(1)).isDefinedAt(0))
        assertEval(!List.single(BigInt(1)).isDefinedAt(1))
        assertEval(!List.single(BigInt(1)).isDefinedAt(-1))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).isDefinedAt(0))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).isDefinedAt(1))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).isDefinedAt(2))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).isDefinedAt(-1))
    }

    test("get") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = list.get(index)
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalEq(List.empty[BigInt].get(0), None)
        assertEvalEq(List.single(BigInt(1)).get(0), Some(BigInt(1)))
        assertEvalEq(List.single(BigInt(1)).get(1), None)
        assertEvalEq(List.single(BigInt(1)).get(-1), None)
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).get(0), Some(BigInt(1)))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).get(1), Some(BigInt(2)))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).get(2), None)
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).get(-1), None)
    }

    test("at") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = liftThrowableToOption(list.at(index))
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].at(0))
        assertEvalEq(List.single(BigInt(1)).at(0), BigInt(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(-1))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).at(0), BigInt(1))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1), BigInt(2))
        assertEvalFails[NoSuchElementException](Cons(BigInt(1), Cons(BigInt(2), Nil)).at(2))
        assertEvalFails[NoSuchElementException](Cons(BigInt(1), Cons(BigInt(2), Nil)).at(-1))
    }

    test("!!") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = liftThrowableToOption(list.!!(index))
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].!!(0))
        assertEvalEq(List.single(BigInt(1)).!!(0), BigInt(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(-1))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(0), BigInt(1))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(1), BigInt(2))
        assertEvalFails[NoSuchElementException](Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(2))
        assertEvalFails[NoSuchElementException](Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(-1))
    }

    test("contains") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.contains(value)
            val scalaResult = list.asScala.contains(value)

            scalusResult === scalaResult
        }

        assertEval(!List.empty[BigInt].contains(BigInt(1)))
        assertEval(List.single(BigInt(1)).contains(BigInt(1)))
        assertEval(!List.single(BigInt(1)).contains(BigInt(2)))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).contains(BigInt(2)))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).contains(BigInt(3)))
    }

    test("groupBy") {

        check { (list: List[BigInt]) =>
            val scalusResult = list.groupBy(_ % 2)
            val scalaResult = list.asScala.groupBy(_ % 2)

            scalusResult.size == scalaResult.size && scalaResult.forall { case (key, scalaList) =>
                scalusResult.get(key) match {
                    case Some(scalusList) => scalusList.quicksort === scalaList.asScalus.quicksort
                    case None             => fail(s"Key $key not found in scalusResult")
                }
            }
        }

        assertEvalEq(List.empty[BigInt].groupBy(_ % 2), SortedMap.empty[BigInt, List[BigInt]])

        assertEvalEq(
          List.single(BigInt(1)).groupBy(_ % 2),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1)))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).groupBy(_ % 2),
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.single(BigInt(2))),
              Cons((BigInt(1), List.single(BigInt(1))), Nil)
            )
          )
        )

    }

    test("groupMap") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.groupMap(_ % 2)(identity)
            val scalaResult = list.asScala.groupMap(_ % 2)(identity)

            scalusResult.size == scalaResult.size && scalaResult.forall { case (key, scalaList) =>
                scalusResult.get(key) match {
                    case Some(scalusList) => scalusList.quicksort === scalaList.asScalus.quicksort
                    case None             => fail(s"Key $key not found in scalusResult")
                }
            }
        }

        assertEvalEq(
          List.empty[BigInt].groupMap(_ % 2)(identity),
          SortedMap.empty[BigInt, List[BigInt]]
        )

        assertEvalEq(
          List.single(BigInt(1)).groupMap(_ % 2)(identity),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1)))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).groupMap(_ % 2)(identity),
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.single(BigInt(2))),
              Cons((BigInt(1), List.single(BigInt(1))), Nil)
            )
          )
        )
    }

    test("groupMapReduce") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.groupMapReduce(_ % 2)(identity)(_ + _)
            val scalaResult = list.asScala.groupMapReduce(_ % 2)(identity)(_ + _)

            scalusResult.size == scalaResult.size && scalaResult.forall { case (key, scalaValue) =>
                scalusResult.get(key) match {
                    case Some(scalusValue) => scalusValue === scalaValue
                    case None              => fail(s"Key $key not found in scalusResult")
                }
            }
        }

        assertEvalEq(
          List.empty[BigInt].groupMapReduce(_ % 2)(identity)(_ + _),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          List.single(BigInt(1)).groupMapReduce(_ % 2)(identity)(_ + _),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Cons(BigInt(4), Nil))))
              .groupMapReduce(_ % 2)(identity)(_ + _),
          SortedMap.unsafeFromList(
            Cons((BigInt(0), BigInt(6)), Cons((BigInt(1), BigInt(4)), Nil))
          )
        )
    }

    test("zip") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.zip(list2)
            val scalaResult = list1.asScala.zip(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].zip(List.empty[BigInt]), List.empty[(BigInt, BigInt)])

        assertEvalEq(
          List.single(BigInt(1)).zip(List.empty[BigInt]),
          List.empty[(BigInt, BigInt)]
        )

        assertEvalEq(
          List.empty[BigInt].zip(List.single(BigInt(1))),
          List.empty[(BigInt, BigInt)]
        )

        assertEvalEq(
          List.single(BigInt(1)).zip(List.single(BigInt(2))),
          List.single((BigInt(1), BigInt(2)))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).zip(Cons(BigInt(3), Cons(BigInt(4), Nil))),
          Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).zip(List.single(BigInt(3))),
          List.single((BigInt(1), BigInt(3)))
        )
    }

    test("prepended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.prepended(value)
            val scalaResult = list.asScala.prepended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].prepended(BigInt(1)), List.single(BigInt(1)))

        assertEvalEq(
          List.single(BigInt(2)).prepended(BigInt(1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(2), Cons(BigInt(3), Nil)).prepended(BigInt(1)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("+:") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = value +: list
            val scalaResult = value +: list.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(BigInt(1) +: List.empty[BigInt], List.single(BigInt(1)))

        assertEvalEq(
          BigInt(1) +: List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          BigInt(1) +: Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("prependedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.prependedAll(list2)
            val scalaResult = list1.asScala.prependedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].prependedAll(List.empty[BigInt]), List.empty[BigInt])

        assertEvalEq(
          List.empty[BigInt].prependedAll(List.single(BigInt(1))),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)).prependedAll(List.empty[BigInt]),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(2)).prependedAll(List.single(BigInt(1))),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(2), Cons(BigInt(3), Nil)).prependedAll(List.single(BigInt(1))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("++:") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++: list2
            val scalaResult = list1.asScala ++: list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt] ++: List.empty[BigInt], List.empty[BigInt])

        assertEvalEq(
          List.empty[BigInt] ++: List.single(BigInt(1)),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)) ++: List.empty[BigInt],
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)) ++: List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) ++: List.single(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("appended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.appended(value)
            val scalaResult = list.asScala.appended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].appended(BigInt(1)), List.single(BigInt(1)))

        assertEvalEq(
          List.single(BigInt(1)).appended(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).appended(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test(":+") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list :+ value
            val scalaResult = list.asScala :+ value

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt] :+ BigInt(1), List.single(BigInt(1)))

        assertEvalEq(
          List.single(BigInt(1)) :+ BigInt(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) :+ BigInt(3),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("appendedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.appendedAll(list2)
            val scalaResult = list1.asScala.appendedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].appendedAll(List.empty[BigInt]), List.empty[BigInt])

        assertEvalEq(
          List.empty[BigInt].appendedAll(List.single(BigInt(1))),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)).appendedAll(List.empty[BigInt]),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)).appendedAll(List.single(BigInt(2))),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).appendedAll(List.single(BigInt(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test(":++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 :++ list2
            val scalaResult = list1.asScala :++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt] :++ List.empty[BigInt], List.empty[BigInt])

        assertEvalEq(
          List.empty[BigInt] :++ List.single(BigInt(1)),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)) :++ List.empty[BigInt],
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)) :++ List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) :++ List.single(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("concat") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.concat(list2)
            val scalaResult = list1.asScala.concat(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].concat(List.empty[BigInt]), List.empty[BigInt])

        assertEvalEq(
          List.empty[BigInt].concat(List.single(BigInt(1))),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)).concat(List.empty[BigInt]),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)).concat(List.single(BigInt(2))),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).concat(List.single(BigInt(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++ list2
            val scalaResult = list1.asScala ++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt] ++ List.empty[BigInt], List.empty[BigInt])

        assertEvalEq(
          List.empty[BigInt] ++ List.single(BigInt(1)),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)) ++ List.empty[BigInt],
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.single(BigInt(1)) ++ List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) ++ List.single(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        )
    }

    test("map") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.map(_ + value)
            val scalaResult = list.asScala.map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].map(_ + BigInt(1)), List.empty[BigInt])

        assertEvalEq(
          List.single(BigInt(1)).map(_ + BigInt(1)),
          List.single(BigInt(2))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).map(_ + BigInt(1)),
          Cons(BigInt(2), Cons(BigInt(3), Nil))
        )
    }

    test("flatMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.flatMap(x => List.single(x + value))
            val scalaResult = list.asScala.flatMap(x => scala.List(x + value))

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(
          List.empty[BigInt].flatMap(x => List.single(x + BigInt(1))),
          List.empty[BigInt]
        )

        assertEvalEq(
          List.single(BigInt(1)).flatMap(x => List.single(x + BigInt(1))),
          List.single(BigInt(2))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).flatMap(x => List.single(x + BigInt(1))),
          Cons(BigInt(2), Cons(BigInt(3), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).flatMap(x => Nil),
          List.empty[BigInt]
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).flatMap(x =>
              Cons(x + BigInt(10), Cons(x + BigInt(100), Nil))
          ),
          Cons(BigInt(11), Cons(BigInt(101), Cons(BigInt(12), Cons(BigInt(102), Nil))))
        )
    }

    test("filter") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filter(_ > value)
            val scalaResult = list.asScala.filter(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].filter(_ > BigInt(1)), List.empty[BigInt])

        assertEvalEq(
          List.single(BigInt(1)).filter(_ > BigInt(1)),
          List.empty[BigInt]
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).filter(_ > BigInt(1)),
          Cons(BigInt(2), Nil)
        )
    }

    test("filterNot") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterNot(_ > value)
            val scalaResult = list.asScala.filterNot(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].filterNot(_ > BigInt(1)), List.empty[BigInt])

        assertEvalEq(
          List.single(BigInt(1)).filterNot(_ > BigInt(1)),
          List.single(BigInt(1))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).filterNot(_ > BigInt(1)),
          Cons(BigInt(1), Nil)
        )
    }

    test("filterMap") {

        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterMap(x => if x > value then Some(x + value) else None)
            val scalaResult =
                list.asScala.flatMap(x => if x > value then scala.Some(x + value) else scala.None)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(
          List.empty[BigInt].filterMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          List.empty[BigInt]
        )

        assertEvalEq(
          List.single(BigInt(1))
              .filterMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          List.empty[BigInt]
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).filterMap(x =>
              if x > BigInt(1) then Some(x + BigInt(1)) else None
          ),
          Cons(BigInt(3), Nil)
        )

    }

    test("find") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.find(_ > value)
            val scalaResult = list.asScala.find(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].find(_ > BigInt(1)), None)

        assertEvalEq(
          List.single(BigInt(1)).find(_ > BigInt(1)),
          None
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).find(_ > BigInt(1)),
          Some(BigInt(2))
        )
    }

    test("findMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.findMap(x => if x > value then Some(x + value) else None)
            val scalaResult = list.asScala.find(_ > value).map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(
          List.empty[BigInt].findMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          None
        )

        assertEvalEq(
          List.single(BigInt(1))
              .findMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          None
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).findMap(x =>
              if x > BigInt(1) then Some(x + BigInt(1)) else None
          ),
          Some(BigInt(3))
        )
    }

    test("foldLeft") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldLeft(initial)(_ + _)
            val scalaResult = list.asScala.foldLeft(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalEq(List.empty[BigInt].foldLeft(BigInt(0))(_ + _), BigInt(0))

        assertEvalEq(
          List.single(BigInt(1)).foldLeft(BigInt(0))(_ + _),
          BigInt(1)
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).foldLeft(BigInt(0))(_ + _),
          BigInt(3)
        )
    }

    test("foldRight") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldRight(initial)(_ + _)
            val scalaResult = list.asScala.foldRight(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalEq(List.empty[BigInt].foldRight(BigInt(0))(_ + _), BigInt(0))

        assertEvalEq(
          List.single(BigInt(1)).foldRight(BigInt(0))(_ + _),
          BigInt(1)
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).foldRight(BigInt(0))(_ + _),
          BigInt(3)
        )
    }

    test("exists") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.exists(_ > value)
            val scalaResult = list.asScala.exists(_ > value)

            scalusResult === scalaResult
        }

        assertEval(!List.empty[BigInt].exists(_ > BigInt(1)))
        assertEval(List.single(BigInt(1)).exists(_ > BigInt(0)))
        assertEval(!List.single(BigInt(1)).exists(_ > BigInt(1)))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).exists(_ > BigInt(1)))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).exists(_ > BigInt(2)))
    }

    test("forall") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.forall(_ > value)
            val scalaResult = list.asScala.forall(_ > value)

            scalusResult === scalaResult
        }

        assertEval(List.empty[BigInt].forall(_ > BigInt(1)))
        assertEval(List.single(BigInt(1)).forall(_ > BigInt(0)))
        assertEval(!List.single(BigInt(1)).forall(_ > BigInt(1)))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).forall(_ > BigInt(0)))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).forall(_ > BigInt(2)))
    }

    test("count") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.count(_ > value)
            val scalaResult = BigInt(list.asScala.count(_ > value))

            scalusResult === scalaResult
        }

        assertEvalEq(List.empty[BigInt].count(_ > BigInt(1)), BigInt(0))
        assertEvalEq(List.single(BigInt(1)).count(_ > BigInt(0)), BigInt(1))
        assertEvalEq(List.single(BigInt(1)).count(_ > BigInt(1)), BigInt(0))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).count(_ > BigInt(1)), BigInt(1))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).count(_ > BigInt(2)), BigInt(0))
    }

    test("indexOfOption") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOfOption(value)
            val scalaResult = list.asScala.indexOf(value) match {
                case -1    => None
                case index => Some(BigInt(index))
            }

            scalusResult === scalaResult
        }

        assertEvalEq(List.empty[BigInt].indexOfOption(BigInt(1)), None)
        assertEvalEq(List.single(BigInt(1)).indexOfOption(BigInt(1)), Some(BigInt(0)))
        assertEvalEq(List.single(BigInt(1)).indexOfOption(BigInt(2)), None)
        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOfOption(BigInt(2)),
          Some(BigInt(1))
        )
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOfOption(BigInt(3)), None)
    }

    test("indexOf") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOf(value)
            val scalaResult = BigInt(list.asScala.indexOf(value))

            scalusResult === scalaResult
        }

        assertEvalEq(List.empty[BigInt].indexOf(BigInt(1)), BigInt(-1))

        assertEvalEq(List.single(BigInt(1)).indexOf(BigInt(1)), BigInt(0))

        assertEvalEq(List.single(BigInt(1)).indexOf(BigInt(2)), BigInt(-1))

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOf(BigInt(2)),
          BigInt(1)
        )

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOf(BigInt(3)), BigInt(-1))
    }

    test("lastOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.lastOption
            val scalaResult = list.asScala.lastOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].lastOption, None)

        assertEvalEq(List.single(BigInt(1)).lastOption, Some(BigInt(1)))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).lastOption, Some(BigInt(2)))
    }

    test("last") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.last)
            val scalaResult = liftThrowableToOption(list.asScala.last)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].last)

        assertEvalEq(List.single(BigInt(1)).last, BigInt(1))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).last, BigInt(2))
    }

    test("headOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.headOption
            val scalaResult = list.asScala.headOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].headOption, None)

        assertEvalEq(List.single(BigInt(1)).headOption, Some(BigInt(1)))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).headOption, Some(BigInt(1)))
    }

    test("head") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.head)
            val scalaResult = liftThrowableToOption(list.asScala.head)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].head)

        assertEvalEq(List.single(BigInt(1)).head, BigInt(1))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).head, BigInt(1))
    }

    test("length") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.length
            val scalaResult = BigInt(list.asScala.length)

            scalusResult === scalaResult
        }

        assertEvalEq(List.empty[BigInt].length, BigInt(0))

        assertEvalEq(List.single(BigInt(1)).length, BigInt(1))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).length, BigInt(2))
    }

    test("size") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.size
            val scalaResult = BigInt(list.asScala.size)

            scalusResult === scalaResult
        }

        assertEvalEq(List.empty[BigInt].size, BigInt(0))

        assertEvalEq(List.single(BigInt(1)).size, BigInt(1))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).size, BigInt(2))
    }

    test("tail") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.tail)
            val scalaResult = liftThrowableToOption(list.asScala.tail.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].tail)
        assertEvalEq(List.single(BigInt(1)).tail, List.empty[BigInt])
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).tail, Cons(BigInt(2), Nil))
    }

    test("drop") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.drop(number)
            val scalaResult = list.asScala.drop(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalEq(List.empty[BigInt].drop(BigInt(1)), List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).drop(BigInt(1)), List.empty[BigInt])

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(1)), Cons(BigInt(2), Nil))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(2)), List.empty[BigInt])

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(0)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(-1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )
    }

    test("dropRight") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.dropRight(number)
            val scalaResult = list.asScala.dropRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalEq(List.empty[BigInt].dropRight(BigInt(1)), List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).dropRight(BigInt(1)), List.empty[BigInt])

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(1)),
          Cons(BigInt(1), Nil)
        )

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(2)), List.empty[BigInt])

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(0)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(-1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )
    }

    test("dropWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.dropWhile(_ < value)
            val scalaResult = list.asScala.dropWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].dropWhile(_ < BigInt(1)), List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).dropWhile(_ < BigInt(1)), List.single(BigInt(1)))

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropWhile(_ < BigInt(3)),
          List.empty[BigInt]
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropWhile(_ < BigInt(2)),
          Cons(BigInt(2), Nil)
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropWhile(_ < BigInt(1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )
    }

    test("deleteFirst") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.deleteFirst(value)
            val scalaList = list.asScala
            val scalaResult = scalaList.indexOf(value) match {
                case -1    => scalaList
                case index => scalaList.patch(index, scala.Nil, 1)
            }

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].deleteFirst(BigInt(1)), List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).deleteFirst(BigInt(1)), List.empty[BigInt])

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).deleteFirst(BigInt(1)),
          Cons(BigInt(2), Nil)
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).deleteFirst(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).deleteFirst(BigInt(2)),
          List.single(BigInt(1))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(1), Nil)).deleteFirst(BigInt(1)),
          List.single(BigInt(1))
        )
    }

    test("take") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.take(number)
            val scalaResult = list.asScala.take(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalEq(List.empty[BigInt].take(BigInt(1)), List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).take(BigInt(1)), List.single(BigInt(1)))

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(1)),
          Cons(BigInt(1), Nil)
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(0)),
          List.empty[BigInt]
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(-1)),
          List.empty[BigInt]
        )
    }

    test("takeRight") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.takeRight(number)
            val scalaResult = list.asScala.takeRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalEq(List.empty[BigInt].takeRight(BigInt(1)), List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).takeRight(BigInt(1)), List.single(BigInt(1)))

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(1)),
          Cons(BigInt(2), Nil)
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(0)),
          List.empty[BigInt]
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(-1)),
          List.empty[BigInt]
        )
    }

    test("takeWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.takeWhile(_ < value)
            val scalaResult = list.asScala.takeWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].takeWhile(_ < BigInt(1)), List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).takeWhile(_ < BigInt(1)), List.empty[BigInt])

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeWhile(_ < BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeWhile(_ < BigInt(2)),
          List.single(BigInt(1))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeWhile(_ < BigInt(1)),
          List.empty[BigInt]
        )
    }

    test("distinct") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.distinct
            val scalaResult = list.asScala.distinct

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].distinct, List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).distinct, List.single(BigInt(1)))

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).distinct,
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(1), Nil)).distinct,
          List.single(BigInt(1))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(1), Nil))).distinct,
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )
    }

    test("diff") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.diff(list2)
            val scalaResult = list1.asScala.diff(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].diff(List.empty[BigInt]), List.empty[BigInt])

        assertEvalEq(
          List.single(BigInt(1)).diff(List.empty[BigInt]),
          List.single(BigInt(1))
        )

        assertEvalEq(
          List.empty[BigInt].diff(List.single(BigInt(1))),
          List.empty[BigInt]
        )

        assertEvalEq(
          List.single(BigInt(1)).diff(List.single(BigInt(1))),
          List.empty[BigInt]
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).diff(List.single(BigInt(2))),
          Cons(BigInt(1), Nil)
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).diff(List.single(BigInt(3))),
          Cons(BigInt(1), Cons(BigInt(2), Nil))
        )
    }

    test("init") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.init)
            val scalaResult = liftThrowableToOption(list.asScala.init.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].init)

        assertEvalEq(List.single(BigInt(1)).init, List.empty[BigInt])

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).init, Cons(BigInt(1), Nil))
    }

    test("reverse") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.reverse
            val scalaResult = list.asScala.reverse

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt].reverse, List.empty[BigInt])

        assertEvalEq(List.single(BigInt(1)).reverse, List.single(BigInt(1)))

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).reverse,
          Cons(BigInt(2), Cons(BigInt(1), Nil))
        )

        assertEvalEq(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))).reverse,
          Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil)))
        )
    }

    test("foreach") {
        check { (list: List[BigInt], value: BigInt) =>
            var scalaSum = BigInt(0)
            var scalusSum = BigInt(0)

            list.foreach(x => scalusSum += x + value)
            list.asScala.foreach(x => scalaSum += x + value)

            scalusSum === scalaSum
        }

        assertEvalEq(List.empty[BigInt].foreach(_ + BigInt(1)), ())

        assertEvalEq(List.single(BigInt(1)).foreach(_ + BigInt(1)), ())

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil)).foreach(_ + BigInt(1)), ())
    }

    test("asScala/asScalus") {
        check { (scalusList: List[BigInt], scalaList: scala.List[BigInt]) =>
            scalusList === scalusList.asScala.asScalus &&
            scalaList == scalaList.asScalus.asScala
        }
    }

    private val bigIntGen: Gen[BigInt] = Gen.choose(BigInt(-1), BigInt(10))

    private val bigIntRangeGen: Gen[(BigInt, BigInt)] =
        for {
            start <- Gen.choose(BigInt(-5), BigInt(5))
            end <- bigIntGen
        } yield (start, end)

    private val bigIntListAndIndexGen: Gen[(List[BigInt], BigInt)] =
        for {
            list <- Arbitrary.arbitrary[List[BigInt]]
            index <- bigIntGen
        } yield (list, index)
}
