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

        assertEvalWithBudget(
          (n: BigInt) => List.single(n),
          BigInt(1),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 1332, steps = 264462)
        )
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

        assertEvalWithBudget(
          (n: BigInt) => List.range(n, 0),
          BigInt(0),
          List.single(BigInt(0)),
          ExUnits(memory = 8934, steps = 1_856784)
        )
        assertEvalWithBudget(
          (n: BigInt) => List.range(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 16870, steps = 3_722294)
        )
        assertEvalWithBudget(
          (n: BigInt) => List.range(n, -1),
          BigInt(0),
          List.empty[BigInt],
          ExUnits(memory = 4966, steps = 924029)
        )
    }

    test("rangeUntil") {
        forAll(bigIntRangeGen) { (start: BigInt, end: BigInt) =>
            val scalusResult = List.rangeUntil(start, end)
            val scalaResult = scala.List.range(start, end)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudget(
          (n: BigInt) => List.rangeUntil(n, 1),
          BigInt(0),
          List.single(BigInt(0)),
          ExUnits(memory = 8934, steps = 1_859690)
        )
        assertEvalWithBudget(
          (n: BigInt) => List.rangeUntil(n, 4),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 16870, steps = 3_728106)
        )
        assertEvalWithBudget(
          (n: BigInt) => List.rangeUntil(n, 0),
          BigInt(0),
          List.empty[BigInt],
          ExUnits(memory = 4966, steps = 925482)
        )
        assertEvalWithBudget(
          (n: BigInt) => List.rangeUntil(n, -1),
          BigInt(0),
          List.empty[BigInt],
          ExUnits(memory = 4966, steps = 925482)
        )
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
          (n: BigInt) => List.fill(n, 1),
          BigInt(1),
          List.single(BigInt(1)),
          ExUnits(memory = 7638, steps = 1_616348)
        )

        assertEvalWithBudget(
          (n: BigInt) => List.fill(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil))),
          ExUnits(memory = 14510, steps = 3_294166)
        )

        assertEvalWithBudget(
          (n: BigInt) => List.fill(n, 0),
          BigInt(1),
          List.empty[BigInt],
          ExUnits(memory = 4202, steps = 777439)
        )

        assertEvalWithBudget(
          (n: BigInt) => List.fill(n, -1),
          BigInt(1),
          List.empty[BigInt],
          ExUnits(memory = 4202, steps = 777439)
        )
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
          (list: List[BigInt]) => List.map2(list, List.empty[BigInt])(_ + _),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 6496, steps = 1_300021)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => List.map2(List.empty[BigInt], list)(_ + _),
          Cons(3, Cons(4, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 5564, steps = 1_023027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => List.map2(list, Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(4), Cons(BigInt(6), Nil)),
          ExUnits(memory = 21208, steps = 5_074969)
        )
    }

    test("ToData <-> FromData") {
        check { (list: List[BigInt]) =>
            val data = list.toData
            val fromDataList = fromData[List[BigInt]](data)

            fromDataList === list
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 432, steps = 74033)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 432, steps = 74033)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          Cons(1, Cons(2, Cons(3, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 432, steps = 74033)
        )
    }

    test("Eq") {
        check { (list: List[BigInt]) => list === list }

        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 === list2
            val scalaResult = list1.asScala == list2.asScala

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list === List.empty[BigInt],
          List.empty[BigInt],
          true,
          ExUnits(memory = 5696, steps = 1_172021)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list === List.single(BigInt(1)),
          List.single(1),
          true,
          ExUnits(memory = 12254, steps = 2_945505)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(1, Cons(2, Cons(3, Nil))),
          true,
          ExUnits(memory = 25370, steps = 6_492473)
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
          (list: List[BigInt]) => list.quicksort,
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 8164, steps = 1_439027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.quicksort,
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 25514, steps = 5_630093)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.quicksort,
          Cons(3, Cons(1, Cons(2, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 134031, steps = 32_260472)
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
          (list: List[List[BigInt]]) => list.flatten,
          List.empty[List[BigInt]],
          List.empty[BigInt],
          ExUnits(memory = 5796, steps = 1_080960)
        )

        assertEvalWithBudget(
          (list: List[List[BigInt]]) => list.flatten,
          List.single(Cons(3, Cons(1, Cons(2, Nil)))),
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))),
          ExUnits(memory = 14322, steps = 3_077577)
        )

        assertEvalWithBudget(
          (list: List[List[BigInt]]) => list.flatten,
          Cons[List[BigInt]](Cons(1, Cons(2, Nil)), List.single(List.single(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 29536, steps = 6_971526)
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
          (list: List[BigInt]) => list.get(0),
          List.empty[BigInt],
          None,
          ExUnits(memory = 6366, steps = 1_272366)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.get(0),
          List.single(1),
          Some(BigInt(1)),
          ExUnits(memory = 8564, steps = 1_914411)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.get(1),
          List.single(1),
          None,
          ExUnits(memory = 10334, steps = 2_340613)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.get(-1),
          List.single(1),
          None,
          ExUnits(memory = 4234, steps = 803372)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.get(0),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          ExUnits(memory = 8564, steps = 1_914411)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.get(1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          ExUnits(memory = 12532, steps = 2_982658)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.get(2),
          Cons(1, Cons(2, Nil)),
          None,
          ExUnits(memory = 14302, steps = 3_408860)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.get(-1),
          Cons(1, Cons(2, Nil)),
          None,
          ExUnits(memory = 4234, steps = 803372)
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
          ExUnits(memory = 6399, steps = 1_433419)
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(-1))
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(0),
          BigInt(1),
          ExUnits(memory = 6399, steps = 1_433419)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1),
          BigInt(2),
          ExUnits(memory = 10367, steps = 2_501666)
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
          ExUnits(memory = 6399, steps = 1_433419)
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(-1))
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(0),
          BigInt(1),
          ExUnits(memory = 6399, steps = 1_433419)
        )
        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(1),
          BigInt(2),
          ExUnits(memory = 10367, steps = 2_501666)
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
          (list: List[BigInt]) => list.groupBy(_ % 2),
          List.empty[BigInt],
          SortedMap.empty[BigInt, List[BigInt]],
          ExUnits(memory = 11760, steps = 2_224890)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.groupBy(_ % 2),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          ExUnits(memory = 53383, steps = 13_315930)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.groupBy(_ % 2),
          Cons(1, Cons(2, Nil)),
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.single(BigInt(2))),
              Cons((BigInt(1), List.single(BigInt(1))), Nil)
            )
          ),
          ExUnits(memory = 106398, steps = 27_532444)
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
          (list: List[BigInt]) => list.groupMap(_ % 2)(identity),
          List.empty[BigInt],
          SortedMap.empty[BigInt, List[BigInt]],
          ExUnits(memory = 11960, steps = 2_256890)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.groupMap(_ % 2)(identity),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          ExUnits(memory = 54347, steps = 13_495973)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.groupMap(_ % 2)(identity),
          Cons(1, Cons(2, Nil)),
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.single(BigInt(2))),
              Cons((BigInt(1), List.single(BigInt(1))), Nil)
            )
          ),
          ExUnits(memory = 108126, steps = 27_860530)
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
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          List.empty[BigInt],
          SortedMap.empty[BigInt, BigInt],
          ExUnits(memory = 7364, steps = 1_311027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          List.single(1),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          ExUnits(memory = 23235, steps = 5_611664)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          Cons(1, Cons(2, Cons(3, Cons(4, Nil)))),
          SortedMap.unsafeFromList(
            Cons((BigInt(0), BigInt(6)), Cons((BigInt(1), BigInt(4)), Nil))
          ),
          ExUnits(memory = 142772, steps = 39_333453)
        )
    }

    test("zip") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.zip(list2)
            val scalaResult = list1.asScala.zip(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.zip(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[(BigInt, BigInt)],
          ExUnits(memory = 8264, steps = 1_455027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.zip(List.empty[BigInt]),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          ExUnits(memory = 8996, steps = 1_700021)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => List.empty[BigInt].zip(list),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          ExUnits(memory = 8264, steps = 1_455027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.zip(List.single(BigInt(2))),
          List.single(1),
          List.single((BigInt(1), BigInt(2))),
          ExUnits(memory = 25192, steps = 6_568329)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.zip(Cons(BigInt(3), Cons(BigInt(4), Nil))),
          Cons(1, Cons(2, Nil)),
          Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil)),
          ExUnits(memory = 52056, steps = 15_103700)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.zip(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          List.single((BigInt(1), BigInt(3))),
          ExUnits(memory = 25924, steps = 6_813323)
        )
    }

    test("prepended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.prepended(value)
            val scalaResult = list.asScala.prepended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 1564, steps = 322395)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 1564, steps = 322395)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 1564, steps = 322395)
        )
    }

    test("+:") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = value +: list
            val scalaResult = value +: list.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => BigInt(1) +: list,
          List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 1564, steps = 322395)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => BigInt(1) +: list,
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 1564, steps = 322395)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => BigInt(1) +: list,
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 1564, steps = 322395)
        )
    }

    test("prependedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.prependedAll(list2)
            val scalaResult = list1.asScala.prependedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.prependedAll(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6198, steps = 1_297058)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 6198, steps = 1_297058)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.prependedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 7230, steps = 1_590052)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 10590, steps = 2_498154)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 10590, steps = 2_498154)
        )
    }

    test("++:") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++: list2
            val scalaResult = list1.asScala ++: list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => List.empty[BigInt] ++: list,
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6198, steps = 1_297058)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => List.empty[BigInt] ++: list,
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 7230, steps = 1_590052)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list ++: List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 4433, steps = 790082)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list ++: List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 8293, steps = 1_885245)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list ++: List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 11121, steps = 2_687414)
        )
    }

    test("appended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.appended(value)
            val scalaResult = list.asScala.appended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.appended(BigInt(1)),
          List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 5196, steps = 1_031389)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.appended(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 8324, steps = 1_881558)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.appended(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 11452, steps = 2_731727)
        )
    }

    test(":+") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list :+ value
            val scalaResult = list.asScala :+ value

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list :+ BigInt(1),
          List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 5196, steps = 1_031389)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list :+ BigInt(2),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 8324, steps = 1_881558)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list :+ BigInt(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 11452, steps = 2_731727)
        )
    }

    test("appendedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.appendedAll(list2)
            val scalaResult = list1.asScala.appendedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.appendedAll(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6266, steps = 1_287125)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 7298, steps = 1_580119)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.appendedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 6266, steps = 1_287125)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 10126, steps = 2_382288)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 12954, steps = 3_184457)
        )
    }

    test(":++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 :++ list2
            val scalaResult = list1.asScala :++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list :++ List.empty[BigInt],
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6266, steps = 1_287125)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => List.empty[BigInt] :++ list,
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 6998, steps = 1_532119)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list :++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 6266, steps = 1_287125)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list :++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 10126, steps = 2_382288)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list :++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 12954, steps = 3_184457)
        )
    }

    test("concat") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.concat(list2)
            val scalaResult = list1.asScala.concat(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.concat(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6266, steps = 1_287125)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.concat(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          ExUnits(memory = 7298, steps = 1_580119)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.concat(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 6266, steps = 1_287125)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.concat(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 10126, steps = 2_382288)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.concat(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 12954, steps = 3_184457)
        )
    }

    test("++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++ list2
            val scalaResult = list1.asScala ++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list ++ List.empty[BigInt],
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6266, steps = 1_287125)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => List.empty[BigInt] ++ list,
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 6998, steps = 1_532119)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list ++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 6266, steps = 1_287125)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list ++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 10126, steps = 2_382288)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list ++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 12954, steps = 3_184457)
        )
    }

    test("map") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.map(_ + value)
            val scalaResult = list.asScala.map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.map(_ + 1),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 5796, steps = 1_080960)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.map(_ + 1),
          List.single(1),
          List.single(BigInt(2)),
          ExUnits(memory = 11954, steps = 2_592165)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.map(_ + 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          ExUnits(memory = 18112, steps = 4_103370)
        )
    }

    test("flatMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.flatMap(x => List.single(x + value))
            val scalaResult = list.asScala.flatMap(x => scala.List(x + value))

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6396, steps = 1_176960)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          List.single(1),
          List.single(BigInt(2)),
          ExUnits(memory = 16288, steps = 3_565257)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          ExUnits(memory = 30040, steps = 7_048717)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.flatMap(_ => Nil),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 24048, steps = 5_255816)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.flatMap(x => Cons(x + 10, Cons(x + 100, Nil))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(11), Cons(BigInt(101), Cons(BigInt(12), Cons(BigInt(102), Nil)))),
          ExUnits(memory = 35000, steps = 8_548624)
        )
    }

    test("filter") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filter(_ > value)
            val scalaResult = list.asScala.filter(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.filter(_ > 1),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6296, steps = 1_160960)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.filter(_ > 1),
          List.single(1),
          List.empty[BigInt],
          ExUnits(memory = 12290, steps = 2_588635)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.filter(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 18716, steps = 4_152672)
        )
    }

    test("filterNot") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterNot(_ > value)
            val scalaResult = list.asScala.filterNot(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6296, steps = 1_160960)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 13223, steps = 2_881046)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 19718, steps = 4_464770)
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
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6896, steps = 1_256960)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          List.empty[BigInt],
          ExUnits(memory = 15256, steps = 3_347500)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(3), Nil),
          ExUnits(memory = 26010, steps = 6_298564)
        )

    }

    test("find") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.find(_ > value)
            val scalaResult = list.asScala.find(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.find(_ > 1),
          List.empty[BigInt],
          None,
          ExUnits(memory = 5864, steps = 1_071027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.find(_ > 1),
          List.single(1),
          None,
          ExUnits(memory = 10594, steps = 2_246917)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.find(_ > 1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          ExUnits(memory = 13356, steps = 3_065813)
        )
    }

    test("findMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.findMap(x => if x > value then Some(x + value) else None)
            val scalaResult = list.asScala.find(_ > value).map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          List.empty[BigInt],
          None,
          ExUnits(memory = 6064, steps = 1_103027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          None,
          ExUnits(memory = 14162, steps = 3_253880)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(3)),
          ExUnits(memory = 20694, steps = 5_145096)
        )
    }

    test("foldLeft") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldLeft(initial)(_ + _)
            val scalaResult = list.asScala.foldLeft(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          List.empty[BigInt],
          BigInt(0),
          ExUnits(memory = 5496, steps = 1_027771)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          ExUnits(memory = 12190, steps = 2_523573)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          ExUnits(memory = 18884, steps = 4_019375)
        )
    }

    test("foldRight") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldRight(initial)(_ + _)
            val scalaResult = list.asScala.foldRight(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          List.empty[BigInt],
          BigInt(0),
          ExUnits(memory = 5496, steps = 1_027771)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          ExUnits(memory = 10990, steps = 2_331573)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          ExUnits(memory = 16484, steps = 3_635375)
        )
    }

    test("exists") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.exists(_ > value)
            val scalaResult = list.asScala.exists(_ > value)

            scalusResult === scalaResult
        }

        assertEval(!List.empty[BigInt].exists(_ > 1))
        assertEval(List.single(BigInt(1)).exists(_ > 0))
        assertEval(!List.single(BigInt(1)).exists(_ > 1))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).exists(_ > 1))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).exists(_ > 2))
    }

    test("forall") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.forall(_ > value)
            val scalaResult = list.asScala.forall(_ > value)

            scalusResult === scalaResult
        }

        assertEval(List.empty[BigInt].forall(_ > 1))
        assertEval(List.single(BigInt(1)).forall(_ > 0))
        assertEval(!List.single(BigInt(1)).forall(_ > 1))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).forall(_ > 0))
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).forall(_ > 2))
    }

    test("count") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.count(_ > value)
            val scalaResult = BigInt(list.asScala.count(_ > value))

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.count(_ > 1),
          List.empty[BigInt],
          BigInt(0),
          ExUnits(memory = 5996, steps = 1_107771)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.count(_ > 0),
          List.single(1),
          BigInt(1),
          ExUnits(memory = 13592, steps = 2_868912)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.count(_ > 1),
          List.single(1),
          BigInt(0),
          ExUnits(memory = 13190, steps = 2_703704)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.count(_ > 1),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          ExUnits(memory = 20786, steps = 4_464845)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.count(_ > 2),
          Cons(1, Cons(2, Nil)),
          BigInt(0),
          ExUnits(memory = 20384, steps = 4_299637)
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
          (list: List[BigInt]) => list.indexOfOption(BigInt(1)),
          List.empty[BigInt],
          None,
          ExUnits(memory = 5264, steps = 975027)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.indexOfOption(BigInt(1)),
          List.single(1),
          Some(BigInt(0)),
          ExUnits(memory = 8758, steps = 1_865859)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          List.single(1),
          None,
          ExUnits(memory = 10528, steps = 2_359912)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          ExUnits(memory = 14022, steps = 3_250744)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.indexOfOption(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          None,
          ExUnits(memory = 15792, steps = 3_744797)
        )
    }

    test("indexOf") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOf(value)
            val scalaResult = BigInt(list.asScala.indexOf(value))

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.indexOf(BigInt(1)),
          List.empty[BigInt],
          BigInt(-1),
          ExUnits(memory = 8462, steps = 1_786636)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.indexOf(BigInt(1)),
          List.single(1),
          BigInt(0),
          ExUnits(memory = 12420, steps = 2_966610)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          List.single(1),
          BigInt(-1),
          ExUnits(memory = 13726, steps = 3_171521)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          ExUnits(memory = 17684, steps = 4_351495)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.indexOf(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          BigInt(-1),
          ExUnits(memory = 18990, steps = 4_556406)
        )
    }

    test("lastOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.lastOption
            val scalaResult = list.asScala.lastOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.lastOption,
          List.empty[BigInt],
          None,
          ExUnits(memory = 4764, steps = 895027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.lastOption,
          List.single(1),
          Some(BigInt(1)),
          ExUnits(memory = 7525, steps = 1_779396)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.lastOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          ExUnits(memory = 11354, steps = 2_876759)
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
          ExUnits(memory = 4430, steps = 1_188263)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).last,
          BigInt(2),
          ExUnits(memory = 4430, steps = 1_188263)
        )
    }

    test("headOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.headOption
            val scalaResult = list.asScala.headOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.headOption,
          List.empty[BigInt],
          None,
          ExUnits(memory = 2964, steps = 607027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.headOption,
          List.single(1),
          Some(BigInt(1)),
          ExUnits(memory = 4060, steps = 944690)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.headOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          ExUnits(memory = 4060, steps = 944690)
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
          ExUnits(memory = 4430, steps = 1_188263)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.head,
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          ExUnits(memory = 8022, steps = 2_093441)
        )
    }

    test("length") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.length
            val scalaResult = BigInt(list.asScala.length)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.length,
          List.empty[BigInt],
          BigInt(0),
          ExUnits(memory = 4896, steps = 931771)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.length,
          List.single(1),
          BigInt(1),
          ExUnits(memory = 11358, steps = 2_374829)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.length,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          ExUnits(memory = 17820, steps = 3_817887)
        )
    }

    test("size") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.size
            val scalaResult = BigInt(list.asScala.size)

            scalusResult === scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.size,
          List.empty[BigInt],
          BigInt(0),
          ExUnits(memory = 4896, steps = 931771)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.size,
          List.single(1),
          BigInt(1),
          ExUnits(memory = 11358, steps = 2_374829)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.size,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          ExUnits(memory = 17820, steps = 3_817887)
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
          ExUnits(memory = 1832, steps = 405094)
        )
        assertEvalWithBudget(
          (list: List[BigInt]) => list.tail,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 3196, steps = 720690)
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
          (list: List[BigInt]) => list.drop(1),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 5566, steps = 1_142913)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.drop(1),
          List.single(1),
          List.empty[BigInt],
          ExUnits(memory = 8802, steps = 1_957670)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.drop(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 8802, steps = 1_957670)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.drop(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 12770, steps = 3_017421)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.drop(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 4834, steps = 897919)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.drop(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 4834, steps = 897919)
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
          (list: List[BigInt]) => list.dropRight(1),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 11022, steps = 2_432750)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropRight(1),
          List.single(1),
          List.empty[BigInt],
          ExUnits(memory = 20606, steps = 5_128972)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 30452, steps = 7_935240)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropRight(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 30190, steps = 7_825194)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropRight(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 5834, steps = 1_057919)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropRight(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 5834, steps = 1_057919)
        )
    }

    test("dropWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.dropWhile(_ < value)
            val scalaResult = list.asScala.dropWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 5364, steps = 991027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 7530, steps = 1_552260)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 15824, steps = 3_502807)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 12760, steps = 2_808150)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 7530, steps = 1_552260)
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
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 5064, steps = 943027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          List.single(1),
          List.empty[BigInt],
          ExUnits(memory = 7694, steps = 1_677710)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 7694, steps = 1_677710)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.deleteFirst(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 15516, steps = 3_789405)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.deleteFirst(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          ExUnits(memory = 12920, steps = 3_100899)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          ExUnits(memory = 7694, steps = 1_677710)
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
          (list: List[BigInt]) => list.take(1),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6166, steps = 1_238913)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.take(1),
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 10066, steps = 2_305182)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.take(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 10066, steps = 2_305182)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.take(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 15430, steps = 3_861439)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.take(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 5434, steps = 993919)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.take(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 5434, steps = 993919)
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
          (list: List[BigInt]) => list.takeRight(1),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 8294, steps = 1_834576)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeRight(1),
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 18706, steps = 4_856642)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          ExUnits(memory = 25796, steps = 6_736623)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeRight(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 29118, steps = 7_878708)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeRight(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 5834, steps = 1_057919)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeRight(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 5834, steps = 1_057919)
        )
    }

    test("takeWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.takeWhile(_ < value)
            val scalaResult = list.asScala.takeWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 5664, steps = 1_039027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          List.single(1),
          List.empty[BigInt],
          ExUnits(memory = 7830, steps = 1_600260)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 17452, steps = 4_053831)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          ExUnits(memory = 13724, steps = 3_107662)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          ExUnits(memory = 7830, steps = 1_600260)
        )
    }

    test("distinct") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.distinct
            val scalaResult = list.asScala.distinct

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.distinct,
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 9760, steps = 1_863887)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.distinct,
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 26044, steps = 5_831752)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 48390, steps = 11_211294)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          ExUnits(memory = 39798, steps = 9_327984)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Cons(1, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 68206, steps = 16_119203)
        )
    }

    test("diff") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.diff(list2)
            val scalaResult = list1.asScala.diff(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.diff(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6865, steps = 1_307076)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.diff(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 7597, steps = 1_552070)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 6865, steps = 1_307076)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.single(1),
          List.empty[BigInt],
          ExUnits(memory = 16956, steps = 3_849603)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.diff(List.single(BigInt(2))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          ExUnits(memory = 23214, steps = 5_565786)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.diff(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          ExUnits(memory = 25810, steps = 6_254292)
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
          ExUnits(memory = 17046, steps = 4_409077)
        )

        assertEvalWithBudget(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).init,
          Cons(BigInt(1), Nil),
          ExUnits(memory = 27624, steps = 7_460339)
        )
    }

    test("reverse") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.reverse
            val scalaResult = list.asScala.reverse

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudget(
          (list: List[BigInt]) => list.reverse,
          List.empty[BigInt],
          List.empty[BigInt],
          ExUnits(memory = 5196, steps = 984960)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.reverse,
          List.single(1),
          List.single(BigInt(1)),
          ExUnits(memory = 11388, steps = 2_374914)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(1), Nil)),
          ExUnits(memory = 17580, steps = 3_764868)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Cons(3, Nil))),
          Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil))),
          ExUnits(memory = 23772, steps = 5_154822)
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

        assertEvalWithBudget(
          (list: List[BigInt]) => list.foreach(_ + 1),
          List.empty[BigInt],
          (),
          ExUnits(memory = 5064, steps = 943027)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.foreach(_ + 1),
          List.single(1),
          (),
          ExUnits(memory = 9492, steps = 1_949578)
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.foreach(_ + 1),
          Cons(1, Cons(2, Nil)),
          (),
          ExUnits(memory = 13920, steps = 2_956129)
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
