package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.{asScalus, identity, Eq, List, Option, Ord, Order, SortedMap}
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.EvalTestKit

class ListTest extends AnyFunSuite with EvalTestKit {

    test("empty") {
        assertEvalWithBudget(List.empty[BigInt], Nil, ExUnits(200, 16100))

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

        assertEvalWithBudget(
          List.single(BigInt(1)),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 200, steps = 16100)
        )
    }

    test("apply") {
        check { (seq: scala.Seq[BigInt]) =>
            val scalusResult = List(seq*)
            val scalaResult = scala.List(seq*)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List(BigInt(1), BigInt(2), BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 800, steps = 112100)
        )
    }

    test("apply list of pairs") {
        assertEvalWithBudget(
          List((BigInt(1), BigInt(2)), (BigInt(3), BigInt(4))),
          Cons((BigInt(1), BigInt(2)), Cons((BigInt(3), BigInt(4)), Nil)),
          ExUnits(memory = 800, steps = 112100)
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

        assertEvalWithBudget(List.range(0, 0), List.single(BigInt(0)), ExUnits(8702, 1804040))
        assertEvalWithBudget(
          List.range(1, 3),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(16638, 3669550)
        )
        assertEvalWithBudget(List.range(0, -1), List.empty[BigInt], ExUnits(4734, 871285))
    }

    test("rangeUntil") {
        forAll(bigIntRangeGen) { (start: BigInt, end: BigInt) =>
            val scalusResult = List.rangeUntil(start, end)
            val scalaResult = scala.List.range(start, end)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(List.rangeUntil(0, 1), List.single(BigInt(0)), ExUnits(8702, 1806946))
        assertEvalWithBudget(
          List.rangeUntil(1, 4),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(16638, 3675362)
        )
        assertEvalWithBudget(List.rangeUntil(0, 0), List.empty[BigInt], ExUnits(4734, 872738))
        assertEvalWithBudget(List.rangeUntil(0, -1), List.empty[BigInt], ExUnits(4734, 872738))
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

        assertEvalWithBudget(
          List.fill(BigInt(1), 1),
          List.single(BigInt(1)),
          ExUnits(7638, 1616348)
        )

        assertEvalWithBudget(
          List.fill(BigInt(1), 3),
          Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil))),
          ExUnits(14510, 3294166)
        )

        assertEvalWithBudget(List.fill(BigInt(1), 0), List.empty[BigInt], ExUnits(4202, 777439))

        assertEvalWithBudget(List.fill(BigInt(1), -1), List.empty[BigInt], ExUnits(4202, 777439))
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

        assertEvalWithBudget(
          List.map2(Cons(BigInt(1), Cons(BigInt(2), Nil)), List.empty[BigInt])(_ + _),
          List.empty[BigInt],
          ExUnits(memory = 5464, steps = 1_114088)
        )

        assertEvalWithBudget(
          List.map2(List.empty[BigInt], Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _),
          List.empty[BigInt],
          ExUnits(memory = 4732, steps = 869094)
        )

        assertEvalWithBudget(
          List.map2(Cons(BigInt(1), Cons(BigInt(2), Nil)), Cons(BigInt(3), Cons(BigInt(4), Nil)))(
            _ + _
          ),
          Cons(BigInt(4), Cons(BigInt(6), Nil)),
          ExUnits(memory = 20176, steps = 4_889036)
        )
    }

    test("ToData <-> FromData") {
        check { (list: List[BigInt]) =>
            val data = list.toData
            val fromDataList = fromData[List[BigInt]](data)

            fromDataList === list
        }

        assertEvalWithBudget(
          fromData[List[BigInt]](List.empty[BigInt].toData),
          List.empty[BigInt],
          ExUnits(memory = 3132, steps = 506033)
        )

        assertEvalWithBudget(
          fromData[List[BigInt]](List.single(BigInt(1)).toData),
          List.single(BigInt(1)),
          ExUnits(memory = 3132, steps = 506033)
        )

        assertEvalWithBudget(
          fromData[List[BigInt]](Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))).toData),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 3132, steps = 506033)
        )
    }

    test("Eq") {
        check { (list: List[BigInt]) => list === list }

        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 === list2
            val scalaResult = list1.asScala == list2.asScala

            scalusResult === scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt], List.empty[BigInt], ExUnits(200, 16100))

        assertEvalWithBudget(
          List.single(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 200, steps = 16100)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 200, steps = 16100)
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

        assertEvalWithBudget(
          List.empty[BigInt].quicksort,
          List.empty[BigInt],
          ExUnits(7932, 1381094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).quicksort,
          List.single(BigInt(1)),
          ExUnits(memory = 28282, steps = 6_052160)
        )

        assertEvalWithBudget(
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))).quicksort,
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 144599, steps = 33_930539)
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

        assertEvalWithBudget(
          List.empty[List[BigInt]].flatten,
          List.empty[BigInt],
          ExUnits(memory = 7064, steps = 1_263027)
        )

        assertEvalWithBudget(
          List.single(Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil)))).flatten,
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))),
          ExUnits(memory = 16790, steps = 3_451644)
        )

        assertEvalWithBudget(
          Cons[List[BigInt]](
            Cons(BigInt(1), Cons(BigInt(2), Nil)),
            List.single(List.single(BigInt(3)))
          ).flatten,
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 33204, steps = 7_537593)
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

        assertEvalWithBudget(
          List.empty[BigInt].get(0),
          None,
          ExUnits(memory = 6834, steps = 1_326433)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).get(0),
          Some(BigInt(1)),
          ExUnits(memory = 9032, steps = 1_968478)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).get(1),
          None,
          ExUnits(memory = 10802, steps = 2_394680)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).get(-1),
          None,
          ExUnits(memory = 4302, steps = 793439)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).get(0),
          Some(BigInt(1)),
          ExUnits(memory = 9032, steps = 1_968478)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).get(1),
          Some(BigInt(2)),
          ExUnits(memory = 13000, steps = 3_036725)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).get(2),
          None,
          ExUnits(14770, 3462927)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).get(-1),
          None,
          ExUnits(4302, 793439)
        )
    }

    test("at") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = liftThrowableToOption(list.at(index))
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].at(0))
        assertEvalWithBudget(
          List.single(BigInt(1)).at(0),
          BigInt(1),
          ExUnits(memory = 8400, steps = 1_798709)
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(-1))
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(0),
          BigInt(1),
          ExUnits(memory = 8400, steps = 1_798709)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1),
          BigInt(2),
          ExUnits(memory = 12368, steps = 2_866956)
        )
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
        assertEvalWithBudget(
          List.single(BigInt(1)).!!(0),
          BigInt(1),
          ExUnits(memory = 8400, steps = 1_798709)
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(-1))
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(0),
          BigInt(1),
          ExUnits(memory = 8400, steps = 1_798709)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(1),
          BigInt(2),
          ExUnits(memory = 12368, steps = 2_866956)
        )
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

        assertEvalWithBudget(
          List.empty[BigInt].groupBy(_ % 2),
          SortedMap.empty[BigInt, List[BigInt]],
          ExUnits(memory = 16128, steps = 2_902957)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).groupBy(_ % 2),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          ExUnits(memory = 64251, steps = 15_033997)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).groupBy(_ % 2),
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.single(BigInt(2))),
              Cons((BigInt(1), List.single(BigInt(1))), Nil)
            )
          ),
          ExUnits(memory = 123766, steps = 30_290511)
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

        assertEvalWithBudget(
          List.empty[BigInt].groupMap(_ % 2)(identity),
          SortedMap.empty[BigInt, List[BigInt]],
          ExUnits(memory = 15228, steps = 2_758957)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).groupMap(_ % 2)(identity),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          ExUnits(memory = 64415, steps = 15_086040)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).groupMap(_ % 2)(identity),
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.single(BigInt(2))),
              Cons((BigInt(1), List.single(BigInt(1))), Nil)
            )
          ),
          ExUnits(memory = 124994, steps = 30_538597)
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

        assertEvalWithBudget(
          List.empty[BigInt].groupMapReduce(_ % 2)(identity)(_ + _),
          SortedMap.empty[BigInt, BigInt],
          ExUnits(memory = 10432, steps = 1_781094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).groupMapReduce(_ % 2)(identity)(_ + _),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          ExUnits(memory = 29203, steps = 6_545731)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Cons(BigInt(4), Nil))))
              .groupMapReduce(_ % 2)(identity)(_ + _),
          SortedMap.unsafeFromList(
            Cons((BigInt(0), BigInt(6)), Cons((BigInt(1), BigInt(4)), Nil))
          ),
          ExUnits(memory = 158040, steps = 41_755520)
        )
    }

    test("zip") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.zip(list2)
            val scalaResult = list1.asScala.zip(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].zip(List.empty[BigInt]),
          List.empty[(BigInt, BigInt)],
          ExUnits(7732, 1349094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).zip(List.empty[BigInt]),
          List.empty[(BigInt, BigInt)],
          ExUnits(memory = 8464, steps = 1_594088)
        )

        assertEvalWithBudget(
          List.empty[BigInt].zip(List.single(BigInt(1))),
          List.empty[(BigInt, BigInt)],
          ExUnits(memory = 7732, steps = 1_349094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).zip(List.single(BigInt(2))),
          List.single((BigInt(1), BigInt(2))),
          ExUnits(memory = 24660, steps = 6_462396)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).zip(Cons(BigInt(3), Cons(BigInt(4), Nil))),
          Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil)),
          ExUnits(memory = 51524, steps = 14_997767)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).zip(List.single(BigInt(3))),
          List.single((BigInt(1), BigInt(3))),
          ExUnits(memory = 25392, steps = 6_707390)
        )
    }

    test("prepended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.prepended(value)
            val scalaResult = list.asScala.prepended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].prepended(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 200, steps = 16100)
        )

        assertEvalWithBudget(
          List.single(BigInt(2)).prepended(BigInt(1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 200, steps = 16100)
        )

        assertEvalWithBudget(
          Cons(BigInt(2), Cons(BigInt(3), Nil)).prepended(BigInt(1)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 200, steps = 16100)
        )
    }

    test("+:") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = value +: list
            val scalaResult = value +: list.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          BigInt(1) +: List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 200, steps = 16100)
        )

        assertEvalWithBudget(
          BigInt(1) +: List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 200, steps = 16100)
        )

        assertEvalWithBudget(
          BigInt(1) +: Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 200, steps = 16100)
        )
    }

    test("prependedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.prependedAll(list2)
            val scalaResult = list1.asScala.prependedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].prependedAll(List.empty[BigInt]),
          List.empty[BigInt],
          ExUnits(6034, 1229192)
        )

        assertEvalWithBudget(
          List.empty[BigInt].prependedAll(List.single(BigInt(1))),
          List.single(BigInt(1)),
          ExUnits(memory = 6034, steps = 1_229192)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).prependedAll(List.empty[BigInt]),
          List.single(BigInt(1)),
          ExUnits(memory = 7066, steps = 1_522186)
        )

        assertEvalWithBudget(
          List.single(BigInt(2)).prependedAll(List.single(BigInt(1))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 9894, steps = 2_324355)
        )

        assertEvalWithBudget(
          Cons(BigInt(2), Cons(BigInt(3), Nil)).prependedAll(List.single(BigInt(1))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 10194, steps = 2_372355)
        )
    }

    test("++:") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++: list2
            val scalaResult = list1.asScala ++: list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt] ++: List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(6034, 1229192)
        )

        assertEvalWithBudget(
          List.empty[BigInt] ++: List.single(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 7066, steps = 1_522186)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) ++: List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 6034, steps = 1_229192)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) ++: List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 9894, steps = 2_324355)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) ++: List.single(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 13022, steps = 3_174524)
        )
    }

    test("appended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.appended(value)
            val scalaResult = list.asScala.appended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].appended(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 4964, steps = 973456)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).appended(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 8092, steps = 1_823625)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).appended(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 11220, steps = 2_673794)
        )
    }

    test(":+") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list :+ value
            val scalaResult = list.asScala :+ value

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt] :+ BigInt(1),
          List.single(BigInt(1)),
          ExUnits(memory = 4964, steps = 973456)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) :+ BigInt(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 8092, steps = 1_823625)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) :+ BigInt(3),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 11220, steps = 2_673794)
        )
    }

    test("appendedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.appendedAll(list2)
            val scalaResult = list1.asScala.appendedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].appendedAll(List.empty[BigInt]),
          List.empty[BigInt],
          ExUnits(6934, 1373192)
        )

        assertEvalWithBudget(
          List.empty[BigInt].appendedAll(List.single(BigInt(1))),
          List.single(BigInt(1)),
          ExUnits(memory = 7966, steps = 1_666186)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).appendedAll(List.empty[BigInt]),
          List.single(BigInt(1)),
          ExUnits(memory = 6934, steps = 1_373192)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).appendedAll(List.single(BigInt(2))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 10794, steps = 2_468355)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).appendedAll(List.single(BigInt(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 13622, steps = 3_270524)
        )
    }

    test(":++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 :++ list2
            val scalaResult = list1.asScala :++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt] :++ List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(6934, 1373192)
        )

        assertEvalWithBudget(
          List.empty[BigInt] :++ List.single(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 7966, steps = 1_666186)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) :++ List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 6934, steps = 1_373192)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) :++ List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 10794, steps = 2_468355)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) :++ List.single(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 13622, steps = 3_270524)
        )
    }

    test("concat") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.concat(list2)
            val scalaResult = list1.asScala.concat(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].concat(List.empty[BigInt]),
          List.empty[BigInt],
          ExUnits(6934, 1373192)
        )

        assertEvalWithBudget(
          List.empty[BigInt].concat(List.single(BigInt(1))),
          List.single(BigInt(1)),
          ExUnits(memory = 7966, steps = 1_666186)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).concat(List.empty[BigInt]),
          List.single(BigInt(1)),
          ExUnits(memory = 6934, steps = 1_373192)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).concat(List.single(BigInt(2))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 10794, steps = 2_468355)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).concat(List.single(BigInt(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 13622, steps = 3_270524)
        )
    }

    test("++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++ list2
            val scalaResult = list1.asScala ++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt] ++ List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(6934, 1373192)
        )

        assertEvalWithBudget(
          List.empty[BigInt] ++ List.single(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 7966, steps = 1_666186)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) ++ List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 6934, steps = 1_373192)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)) ++ List.single(BigInt(2)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 10794, steps = 2_468355)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)) ++ List.single(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 13622, steps = 3_270524)
        )
    }

    test("map") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.map(_ + value)
            val scalaResult = list.asScala.map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].map(_ + BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 6764, steps = 1_215027)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).map(_ + BigInt(1)),
          List.single(BigInt(2)),
          ExUnits(memory = 13822, steps = 2_870232)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).map(_ + BigInt(1)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          ExUnits(memory = 20880, steps = 4_525437)
        )
    }

    test("flatMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.flatMap(x => List.single(x + value))
            val scalaResult = list.asScala.flatMap(x => scala.List(x + value))

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].flatMap(x => List.single(x + BigInt(1))),
          List.empty[BigInt],
          ExUnits(memory = 7664, steps = 1_359027)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).flatMap(x => List.single(x + BigInt(1))),
          List.single(BigInt(2)),
          ExUnits(memory = 19356, steps = 4_035324)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).flatMap(x => List.single(x + BigInt(1))),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          ExUnits(memory = 34908, steps = 7_806784)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).flatMap(x => Nil),
          List.empty[BigInt],
          ExUnits(memory = 28316, steps = 5_917883)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).flatMap(x =>
              Cons(x + BigInt(10), Cons(x + BigInt(100), Nil))
          ),
          Cons(BigInt(11), Cons(BigInt(101), Cons(BigInt(12), Cons(BigInt(102), Nil)))),
          ExUnits(memory = 39268, steps = 9_210691)
        )
    }

    test("filter") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filter(_ > value)
            val scalaResult = list.asScala.filter(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].filter(_ > BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 7264, steps = 1_295027)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).filter(_ > BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 13858, steps = 2_818702)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).filter(_ > BigInt(1)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 20884, steps = 4_478739)
        )
    }

    test("filterNot") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterNot(_ > value)
            val scalaResult = list.asScala.filterNot(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].filterNot(_ > BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 8464, steps = 1_487027)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).filterNot(_ > BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 16291, steps = 3_351113)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).filterNot(_ > BigInt(1)),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 23686, steps = 5_078837)
        )
    }

    test("filterMap") {

        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterMap(x => if x > value then Some(x + value) else None)
            val scalaResult =
                list.asScala.flatMap(x => if x > value then scala.Some(x + value) else scala.None)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].filterMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          List.empty[BigInt],
          ExUnits(memory = 7864, steps = 1_391027)
        )

        assertEvalWithBudget(
          List.single(BigInt(1))
              .filterMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          List.empty[BigInt],
          ExUnits(memory = 16824, steps = 3_577567)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).filterMap(x =>
              if x > BigInt(1) then Some(x + BigInt(1)) else None
          ),
          Cons(BigInt(3), Nil),
          ExUnits(memory = 28178, steps = 6_624631)
        )

    }

    test("find") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.find(_ > value)
            val scalaResult = list.asScala.find(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].find(_ > BigInt(1)),
          None,
          ExUnits(memory = 5332, steps = 965094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).find(_ > BigInt(1)),
          None,
          ExUnits(memory = 10062, steps = 2_140984)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).find(_ > BigInt(1)),
          Some(BigInt(2)),
          ExUnits(memory = 12824, steps = 2_959880)
        )
    }

    test("findMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.findMap(x => if x > value then Some(x + value) else None)
            val scalaResult = list.asScala.find(_ > value).map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].findMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          None,
          ExUnits(memory = 5832, steps = 1_045094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1))
              .findMap(x => if x > BigInt(1) then Some(x + BigInt(1)) else None),
          None,
          ExUnits(memory = 14230, steps = 3_243947)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).findMap(x =>
              if x > BigInt(1) then Some(x + BigInt(1)) else None
          ),
          Some(BigInt(3)),
          ExUnits(memory = 21062, steps = 5_183163)
        )
    }

    test("foldLeft") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldLeft(initial)(_ + _)
            val scalaResult = list.asScala.foldLeft(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].foldLeft(BigInt(0))(_ + _),
          BigInt(0),
          ExUnits(4964, 921838)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).foldLeft(BigInt(0))(_ + _),
          BigInt(1),
          ExUnits(memory = 12258, steps = 2_513640)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).foldLeft(BigInt(0))(_ + _),
          BigInt(3),
          ExUnits(memory = 19552, steps = 4_105442)
        )
    }

    test("foldRight") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldRight(initial)(_ + _)
            val scalaResult = list.asScala.foldRight(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].foldRight(BigInt(0))(_ + _),
          BigInt(0),
          ExUnits(4964, 921838)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).foldRight(BigInt(0))(_ + _),
          BigInt(1),
          ExUnits(memory = 10458, steps = 2_225640)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).foldRight(BigInt(0))(_ + _),
          BigInt(3),
          ExUnits(memory = 15952, steps = 3_529442)
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

        assertEvalWithBudget(
          List.empty[BigInt].count(_ > BigInt(1)),
          BigInt(0),
          ExUnits(6964, 1241838)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).count(_ > BigInt(0)),
          BigInt(1),
          ExUnits(memory = 15460, steps = 3_146979)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).count(_ > BigInt(1)),
          BigInt(0),
          ExUnits(memory = 15058, steps = 2_981771)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).count(_ > BigInt(1)),
          BigInt(1),
          ExUnits(memory = 23554, steps = 4_886912)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).count(_ > BigInt(2)),
          BigInt(0),
          ExUnits(memory = 23152, steps = 4_721704)
        )
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

        assertEvalWithBudget(
          List.empty[BigInt].indexOfOption(BigInt(1)),
          None,
          ExUnits(memory = 6232, steps = 1_109094)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).indexOfOption(BigInt(1)),
          Some(BigInt(0)),
          ExUnits(memory = 10026, steps = 2_047926)
        )
        assertEvalWithBudget(
          List.single(BigInt(1)).indexOfOption(BigInt(2)),
          None,
          ExUnits(memory = 11796, steps = 2_541979)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOfOption(BigInt(2)),
          Some(BigInt(1)),
          ExUnits(memory = 15590, steps = 3_480811)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOfOption(BigInt(3)),
          None,
          ExUnits(memory = 17360, steps = 3_974864)
        )
    }

    test("indexOf") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOf(value)
            val scalaResult = BigInt(list.asScala.indexOf(value))

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].indexOf(BigInt(1)),
          BigInt(-1),
          ExUnits(memory = 11130, steps = 2_192703)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).indexOf(BigInt(1)),
          BigInt(0),
          ExUnits(memory = 15388, steps = 3_420677)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).indexOf(BigInt(2)),
          BigInt(-1),
          ExUnits(memory = 16694, steps = 3_625588)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOf(BigInt(2)),
          BigInt(1),
          ExUnits(memory = 20952, steps = 4_853562)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).indexOf(BigInt(3)),
          BigInt(-1),
          ExUnits(memory = 22258, steps = 5_058473)
        )
    }

    test("lastOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.lastOption
            val scalaResult = list.asScala.lastOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].lastOption,
          None,
          ExUnits(memory = 4832, steps = 885094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).lastOption,
          Some(BigInt(1)),
          ExUnits(memory = 7893, steps = 1_817463)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).lastOption,
          Some(BigInt(2)),
          ExUnits(memory = 12022, steps = 2_962826)
        )
    }

    test("last") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.last)
            val scalaResult = liftThrowableToOption(list.asScala.last)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].last)

        assertEvalWithBudget(
          List.single(BigInt(1)).last,
          BigInt(1),
          ExUnits(memory = 12455, steps = 3_062214)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).last,
          BigInt(2),
          ExUnits(memory = 16584, steps = 4_207577)
        )
    }

    test("headOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.headOption
            val scalaResult = list.asScala.headOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].headOption,
          None,
          ExUnits(memory = 2732, steps = 549094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).headOption,
          Some(BigInt(1)),
          ExUnits(memory = 3828, steps = 886757)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).headOption,
          Some(BigInt(1)),
          ExUnits(memory = 3828, steps = 886757)
        )
    }

    test("head") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.head)
            val scalaResult = liftThrowableToOption(list.asScala.head)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].head)

        assertEvalWithBudget(
          List.single(BigInt(1)).head,
          BigInt(1),
          ExUnits(memory = 8690, steps = 2_179508)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).head,
          BigInt(1),
          ExUnits(memory = 200, steps = 16100)
        )
    }

    test("length") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.length
            val scalaResult = BigInt(list.asScala.length)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt].length, BigInt(0), ExUnits(5864, 1065838))

        assertEvalWithBudget(
          List.single(BigInt(1)).length,
          BigInt(1),
          ExUnits(memory = 12926, steps = 2_604896)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).length,
          BigInt(2),
          ExUnits(memory = 19988, steps = 4_143954)
        )
    }

    test("size") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.size
            val scalaResult = BigInt(list.asScala.size)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(List.empty[BigInt].size, BigInt(0), ExUnits(5864, 1065838))

        assertEvalWithBudget(
          List.single(BigInt(1)).size,
          BigInt(1),
          ExUnits(memory = 12926, steps = 2_604896)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).size,
          BigInt(2),
          ExUnits(memory = 19988, steps = 4_143954)
        )
    }

    test("tail") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.tail)
            val scalaResult = liftThrowableToOption(list.asScala.tail.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].tail)
        assertEvalWithBudget(
          List.single(BigInt(1)).tail,
          List.empty[BigInt],
          ExUnits(memory = 2964, steps = 662757)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).tail,
          Cons(BigInt(2), Nil),
          ExUnits(memory = 200, steps = 16100)
        )
    }

    test("drop") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.drop(number)
            val scalaResult = list.asScala.drop(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          List.empty[BigInt].drop(BigInt(1)),
          List.empty[BigInt],
          ExUnits(5334, 1084980)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).drop(BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 8570, steps = 1_899737)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(1)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 8570, steps = 1_899737)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(2)),
          List.empty[BigInt],
          ExUnits(memory = 12538, steps = 2_959488)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(0)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 4602, steps = 839986)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).drop(BigInt(-1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 4602, steps = 839986)
        )
    }

    test("dropRight") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.dropRight(number)
            val scalaResult = list.asScala.dropRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          List.empty[BigInt].dropRight(BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 11090, steps = 2_422817)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).dropRight(BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 20674, steps = 5_119039)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(1)),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 30520, steps = 7_925307)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(2)),
          List.empty[BigInt],
          ExUnits(memory = 30258, steps = 7_815261)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(0)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 5902, steps = 1_047986)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropRight(BigInt(-1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 5902, steps = 1_047986)
        )
    }

    test("dropWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.dropWhile(_ < value)
            val scalaResult = list.asScala.dropWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].dropWhile(_ < BigInt(1)),
          List.empty[BigInt],
          ExUnits(4832, 885094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).dropWhile(_ < BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 6998, steps = 1_446327)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropWhile(_ < BigInt(3)),
          List.empty[BigInt],
          ExUnits(memory = 15292, steps = 3_396874)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropWhile(_ < BigInt(2)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 12228, steps = 2_702217)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).dropWhile(_ < BigInt(1)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 6998, steps = 1_446327)
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

        assertEvalWithBudget(
          List.empty[BigInt].deleteFirst(BigInt(1)),
          List.empty[BigInt],
          ExUnits(5932, 1061094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).deleteFirst(BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 8862, steps = 1_843777)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).deleteFirst(BigInt(1)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 8862, steps = 1_843777)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).deleteFirst(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 16984, steps = 4_003472)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).deleteFirst(BigInt(2)),
          List.single(BigInt(1)),
          ExUnits(memory = 14388, steps = 3_314966)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(1), Nil)).deleteFirst(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 8862, steps = 1_843777)
        )
    }

    test("take") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.take(number)
            val scalaResult = list.asScala.take(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          List.empty[BigInt].take(BigInt(1)),
          List.empty[BigInt],
          ExUnits(5934, 1180980)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).take(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 9834, steps = 2_247249)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(1)),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 9834, steps = 2_247249)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 15198, steps = 3_803506)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(0)),
          List.empty[BigInt],
          ExUnits(memory = 5202, steps = 935986)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).take(BigInt(-1)),
          List.empty[BigInt],
          ExUnits(memory = 5202, steps = 935986)
        )
    }

    test("takeRight") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.takeRight(number)
            val scalaResult = list.asScala.takeRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          List.empty[BigInt].takeRight(BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 8962, steps = 1_920643)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).takeRight(BigInt(1)),
          List.single(BigInt(1)),
          ExUnits(memory = 19674, steps = 4_990709)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(1)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 27064, steps = 6_918690)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 30386, steps = 8_060775)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(0)),
          List.empty[BigInt],
          ExUnits(memory = 5902, steps = 1_047986)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeRight(BigInt(-1)),
          List.empty[BigInt],
          ExUnits(memory = 5902, steps = 1_047986)
        )
    }

    test("takeWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.takeWhile(_ < value)
            val scalaResult = list.asScala.takeWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].takeWhile(_ < BigInt(1)),
          List.empty[BigInt],
          ExUnits(5132, 933094)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).takeWhile(_ < BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 7298, steps = 1_494327)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeWhile(_ < BigInt(3)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 16920, steps = 3_947898)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeWhile(_ < BigInt(2)),
          List.single(BigInt(1)),
          ExUnits(memory = 13192, steps = 3_001729)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).takeWhile(_ < BigInt(1)),
          List.empty[BigInt],
          ExUnits(memory = 7298, steps = 1_494327)
        )
    }

    test("distinct") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.distinct
            val scalaResult = list.asScala.distinct

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].distinct,
          List.empty[BigInt],
          ExUnits(memory = 12228, steps = 2_237954)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).distinct,
          List.single(BigInt(1)),
          ExUnits(memory = 31212, steps = 6_637819)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).distinct,
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 56258, steps = 12_449361)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(1), Nil)).distinct,
          List.single(BigInt(1)),
          ExUnits(memory = 47066, steps = 10_470051)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(1), Nil))).distinct,
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 78174, steps = 17_693270)
        )
    }

    test("diff") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.diff(list2)
            val scalaResult = list1.asScala.diff(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].diff(List.empty[BigInt]),
          List.empty[BigInt],
          ExUnits(6933, 1297143)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).diff(List.empty[BigInt]),
          List.single(BigInt(1)),
          ExUnits(memory = 7665, steps = 1_542137)
        )

        assertEvalWithBudget(
          List.empty[BigInt].diff(List.single(BigInt(1))),
          List.empty[BigInt],
          ExUnits(memory = 6933, steps = 1_297143)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).diff(List.single(BigInt(1))),
          List.empty[BigInt],
          ExUnits(memory = 18424, steps = 4_063670)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).diff(List.single(BigInt(2))),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 24682, steps = 5_779853)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).diff(List.single(BigInt(3))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 27278, steps = 6_468359)
        )
    }

    test("init") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.init)
            val scalaResult = liftThrowableToOption(list.asScala.init.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].init)

        assertEvalWithBudget(
          List.single(BigInt(1)).init,
          List.empty[BigInt],
          ExUnits(memory = 23007, steps = 5_696082)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).init,
          Cons(BigInt(1), Nil),
          ExUnits(memory = 33153, steps = 8_550350)
        )
    }

    test("reverse") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.reverse
            val scalaResult = list.asScala.reverse

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          List.empty[BigInt].reverse,
          List.empty[BigInt],
          ExUnits(memory = 6164, steps = 1_119027)
        )

        assertEvalWithBudget(
          List.single(BigInt(1)).reverse,
          List.single(BigInt(1)),
          ExUnits(memory = 12956, steps = 2_604981)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).reverse,
          Cons(BigInt(2), Cons(BigInt(1), Nil)),
          ExUnits(memory = 19748, steps = 4_090935)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))).reverse,
          Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil))),
          ExUnits(memory = 26540, steps = 5_576889)
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

        assertEvalWithBudget(List.empty[BigInt].foreach(_ + BigInt(1)), (), ExUnits(4532, 837094))

        assertEvalWithBudget(
          List.single(BigInt(1)).foreach(_ + BigInt(1)),
          (),
          ExUnits(memory = 9662, steps = 2_056853)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).foreach(_ + BigInt(1)),
          (),
          ExUnits(memory = 14792, steps = 3_276612)
        )
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
