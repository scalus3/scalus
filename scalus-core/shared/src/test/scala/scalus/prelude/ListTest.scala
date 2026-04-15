package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.{asScalus, identity, Eq, List, Option, Ord, Order, SortedMap}
import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion}
import scalus.compiler.Options
import scalus.testing.kit.EvalTestKit
import scalus.uplc.PlutusV3

class ListTest extends AnyFunSuite with EvalTestKit {

    test("empty") {
        assertEvalEq(List.empty[BigInt], Nil)
        assertEval(List.empty[BigInt].isEmpty)

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

        assertEvalWithBudgets(
          (n: BigInt) => List.single(n),
          BigInt(1),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 1032, steps = 216462)
          )
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

        assertEvalWithBudgets(
          (n: BigInt) => List.range(n, 0),
          BigInt(0),
          List.single(BigInt(0)),
          Seq(
            compilerOptions -> ExUnits(memory = 8434, steps = 1_776784)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.range(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 16770, steps = 3_706_294)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.range(n, -1),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4266, steps = 812029)
          )
        )
    }

    test("rangeUntil") {
        forAll(bigIntRangeGen) { (start: BigInt, end: BigInt) =>
            val scalusResult = List.rangeUntil(start, end)
            val scalaResult = scala.List.range(start, end)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, 1),
          BigInt(0),
          List.single(BigInt(0)),
          Seq(
            compilerOptions -> ExUnits(memory = 8434, steps = 1_779690)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, 4),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 16770, steps = 3_712_106)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, 0),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4266, steps = 813482)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, -1),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4266, steps = 813482)
          )
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

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, 1),
          BigInt(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7138, steps = 1_536348)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 14410, steps = 3_278_166)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, 0),
          BigInt(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3502, steps = 665439)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, -1),
          BigInt(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3502, steps = 665439)
          )
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.map2(list, List.empty[BigInt])(_ + _),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5696, steps = 1_172021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.map2(List.empty[BigInt], list)(_ + _),
          Cons(3, Cons(4, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5564, steps = 1_023027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.map2(list, Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(4), Cons(BigInt(6), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 19608, steps = 4_818969)
          )
        )
    }

    test("ToData <-> FromData") {
        check { (list: List[BigInt]) =>
            val data = list.toData
            val fromDataList = fromData[List[BigInt]](data)

            fromDataList === list
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 432, steps = 74033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 432, steps = 74033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          Cons(1, Cons(2, Cons(3, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 432, steps = 74033)
          )
        )
    }

    test("Eq") {
        check { (list: List[BigInt]) => list === list }

        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 === list2
            val scalaResult = list1.asScala == list2.asScala

            scalusResult === scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list === List.empty[BigInt],
          List.empty[BigInt],
          true,
          Seq(
            compilerOptions -> ExUnits(memory = 3601, steps = 1567364)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list === List.single(BigInt(1)),
          List.single(1),
          true,
          Seq(
            compilerOptions -> ExUnits(memory = 3601, steps = 1703759)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(1, Cons(2, Cons(3, Nil))),
          true,
          Seq(
            compilerOptions -> ExUnits(memory = 3601, steps = 1976549)
          )
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.quicksort,
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 7564, steps = 1_343027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.quicksort,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 22218, steps = 4_811_569)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.quicksort,
          Cons(3, Cons(1, Cons(2, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 123543, steps = 29_708_900)
          )
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

        assertEvalWithBudgets(
          (list: List[List[BigInt]]) => list.flatten,
          List.empty[List[BigInt]],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6096, steps = 1_128960)
          )
        )

        assertEvalWithBudgets(
          (list: List[List[BigInt]]) => list.flatten,
          List.single(Cons(3, Cons(1, Cons(2, Nil)))),
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 14122, steps = 2_987016)
          )
        )

        assertEvalWithBudgets(
          (list: List[List[BigInt]]) => list.flatten,
          Cons[List[BigInt]](Cons(1, Cons(2, Nil)), List.single(List.single(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 28836, steps = 6_742404)
          )
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

    test("get - property") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = list.get(index)
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }
    }

    test("get - empty list, idx 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          List.empty[BigInt],
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 5866, steps = 1_192366)
          )
        )
    }

    test("get - single element, idx 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 8164, steps = 1_850_411)
          )
        )
    }

    test("get - single element, idx 1 (out of bounds)") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(1),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 10034, steps = 2_292_613)
          )
        )
    }

    test("get - single element, idx -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(-1),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 3534, steps = 691372)
          )
        )
    }

    test("get - two-element list, idx 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 8164, steps = 1_850_411)
          )
        )
    }

    test("get - two-element list, idx 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 12332, steps = 2_950_658)
          )
        )
    }

    test("get - two-element list, idx 2 (out of bounds)") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(2),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 14202, steps = 3_392_860)
          )
        )
    }

    test("get - two-element list, idx -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(-1),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 3534, steps = 691372)
          )
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
        assertEvalWithBudgets(
          List.single(BigInt(1)).at(0),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6499, steps = 1_449419)
          )
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(-1))
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(0),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6499, steps = 1_449_419)
          )
        )
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 10667, steps = 2_549_666)
          )
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
        assertEvalWithBudgets(
          List.single(BigInt(1)).!!(0),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6499, steps = 1_449_419)
          )
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(-1))
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(0),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6499, steps = 1_449_419)
          )
        )
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(1),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 10667, steps = 2_549_666)
          )
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupBy(_ % 2),
          List.empty[BigInt],
          SortedMap.empty[BigInt, List[BigInt]],
          Seq(
            compilerOptions -> ExUnits(memory = 11360, steps = 2_160890)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupBy(_ % 2),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          Seq(
            compilerOptions -> ExUnits(memory = 43007, steps = 10_418_156)
          )
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
          ExUnits(memory = 85446, steps = 21_704_896)
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMap(_ % 2)(identity),
          List.empty[BigInt],
          SortedMap.empty[BigInt, List[BigInt]],
          Seq(
            compilerOptions -> ExUnits(memory = 11560, steps = 2_192890)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMap(_ % 2)(identity),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          Seq(
            compilerOptions -> ExUnits(memory = 44271, steps = 10_646_199)
          )
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
          ExUnits(memory = 87774, steps = 22_128_982)
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          List.empty[BigInt],
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6964, steps = 1_247027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          List.single(1),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 22607, steps = 5_279_167)
          )
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          Cons(1, Cons(2, Cons(3, Cons(4, Nil)))),
          SortedMap.unsafeFromList(
            Cons((BigInt(0), BigInt(6)), Cons((BigInt(1), BigInt(4)), Nil))
          ),
          ExUnits(memory = 137032, steps = 36_800_819)
        )
    }

    test("zip - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.zip(list2)
            val scalaResult = list1.asScala.zip(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("zip - empty zip empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions -> ExUnits(memory = 4764, steps = 895027)
          )
        )
    }

    test("zip - single zip empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.empty[BigInt]),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions -> ExUnits(memory = 5496, steps = 1_140021)
          )
        )
    }

    test("zip - empty zip single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt].zip(list),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions -> ExUnits(memory = 4764, steps = 895027)
          )
        )
    }

    test("zip - single zip single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.single(BigInt(2))),
          List.single(1),
          List.single((BigInt(1), BigInt(2))),
          Seq(
            compilerOptions -> ExUnits(memory = 10384, steps = 2_577878)
          )
        )
    }

    test("zip - two zip two") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(Cons(BigInt(3), Cons(BigInt(4), Nil))),
          Cons(1, Cons(2, Nil)),
          Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 16004, steps = 4_260729)
          )
        )
    }

    test("zip - two zip single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          List.single((BigInt(1), BigInt(3))),
          Seq(
            compilerOptions -> ExUnits(memory = 11116, steps = 2_822872),
            compilerOptions -> ExUnits(
              memory = 11116,
              steps = 2822872
            )
          )
        )
    }

    test("prepended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.prepended(value)
            val scalaResult = list.asScala.prepended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 1264, steps = 274395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 1264, steps = 274395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 1264, steps = 274395)
          )
        )
    }

    test("+:") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = value +: list
            val scalaResult = value +: list.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => BigInt(1) +: list,
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 1264, steps = 274395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => BigInt(1) +: list,
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 1264, steps = 274395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => BigInt(1) +: list,
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 1264, steps = 274395)
          )
        )
    }

    test("prependedAll - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.prependedAll(list2)
            val scalaResult = list1.asScala.prependedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("prependedAll - empty prepend empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5966, steps = 1_239125)
          )
        )
    }

    test("prependedAll - empty prepend single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5966, steps = 1_239_125)
          )
        )
    }

    test("prependedAll - single prepend empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6998, steps = 1_532_119)
          )
        )
    }

    test("prependedAll - single prepend single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9826, steps = 2_334_288)
          )
        )
    }

    test("prependedAll - two prepend single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 9826, steps = 2_334_288)
          )
        )
    }

    test("++: - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++: list2
            val scalaResult = list1.asScala ++: list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("++: - empty ++: empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++: list,
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5966, steps = 1_239_125)
          )
        )
    }

    test("++: - empty ++: single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++: list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6998, steps = 1_532_119)
          )
        )
    }

    test("++: - single ++: empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 2733, steps = 518082)
          )
        )
    }

    test("++: - single ++: single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 7293, steps = 1_725_245)
          )
        )
    }

    test("++: - two ++: single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 10621, steps = 2_607_414)
          )
        )
    }

    test("appended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.appended(value)
            val scalaResult = list.asScala.appended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appended(BigInt(1)),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 4696, steps = 951389)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appended(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 8124, steps = 1_849_558)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appended(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 11552, steps = 2_747_727)
          )
        )
    }

    test(":+") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list :+ value
            val scalaResult = list.asScala :+ value

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :+ BigInt(1),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 4696, steps = 951389)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :+ BigInt(2),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 8124, steps = 1_849_558)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :+ BigInt(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 11552, steps = 2_747_727)
          )
        )
    }

    test("appendedAll - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.appendedAll(list2)
            val scalaResult = list1.asScala.appendedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("appendedAll - empty append empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5366, steps = 1_084564)
          )
        )
    }

    test("appendedAll - empty append single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6598, steps = 1_409_558)
          )
        )
    }

    test("appendedAll - single append empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5366, steps = 1_084_564)
          )
        )
    }

    test("appendedAll - single append single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9726, steps = 2_259_727)
          )
        )
    }

    test("appendedAll - two append single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 12854, steps = 3_109_896)
          )
        )
    }

    test(":++ - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 :++ list2
            val scalaResult = list1.asScala :++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test(":++ - empty :++ empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.empty[BigInt],
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5366, steps = 1_084_564)
          )
        )
    }

    test(":++ - empty :++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] :++ list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6298, steps = 1_361_558)
          )
        )
    }

    test(":++ - single :++ empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5366, steps = 1_084_564)
          )
        )
    }

    test(":++ - single :++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9726, steps = 2_259_727)
          )
        )
    }

    test(":++ - two :++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 12854, steps = 3_109_896)
          )
        )
    }

    test("concat - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.concat(list2)
            val scalaResult = list1.asScala.concat(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("concat - empty concat empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5366, steps = 1_084_564)
          )
        )
    }

    test("concat - empty concat single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6598, steps = 1_409_558)
          )
        )
    }

    test("concat - single concat empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5366, steps = 1_084_564)
          )
        )
    }

    test("concat - single concat single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9726, steps = 2_259_727)
          )
        )
    }

    test("concat - two concat single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 12854, steps = 3_109_896)
          )
        )
    }

    test("++ - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++ list2
            val scalaResult = list1.asScala ++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("++ - empty ++ empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.empty[BigInt],
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5366, steps = 1_084_564)
          )
        )
    }

    test("++ - empty ++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++ list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6298, steps = 1_361_558)
          )
        )
    }

    test("++ - single ++ empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5366, steps = 1_084_564)
          )
        )
    }

    test("++ - single ++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9726, steps = 2_259_727)
          )
        )
    }

    test("++ - two ++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 12854, steps = 3_109_896)
          )
        )
    }

    test("map") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.map(_ + value)
            val scalaResult = list.asScala.map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.map(_ + 1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4496, steps = 872960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.map(_ + 1),
          List.single(1),
          List.single(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 11154, steps = 2_464_165)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.map(_ + 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 17812, steps = 4_055_370)
          )
        )
    }

    test("flatMap - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.flatMap(x => List.single(x + value))
            val scalaResult = list.asScala.flatMap(x => scala.List(x + value))

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("flatMap - empty list, single mapper") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6696, steps = 1_224960)
          )
        )
    }

    test("flatMap - single, single mapper") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          List.single(1),
          List.single(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 16088, steps = 3_474696)
          )
        )
    }

    test("flatMap - two, single mapper") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 29340, steps = 6_819595)
          )
        )
    }

    test("flatMap - two, empty mapper") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(_ => Nil),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 23948, steps = 5_122694)
          )
        )
    }

    test("flatMap - two, two-element mapper") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => Cons(x + 10, Cons(x + 100, Nil))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(11), Cons(BigInt(101), Cons(BigInt(12), Cons(BigInt(102), Nil)))),
          Seq(
            compilerOptions -> ExUnits(memory = 34900, steps = 8_415502)
          )
        )
    }

    test("filter") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filter(_ > value)
            val scalaResult = list.asScala.filter(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filter(_ > 1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4496, steps = 872960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filter(_ > 1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 11490, steps = 2_460_635)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filter(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 19016, steps = 4_200_672)
          )
        )
    }

    test("filterNot") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterNot(_ > value)
            val scalaResult = list.asScala.filterNot(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5596, steps = 1_048_960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 13323, steps = 2_897_046)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 20618, steps = 4_608_770)
          )
        )
    }

    test("filterMap") {

        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterMap(x => if x > value then Some(x + value) else None)
            val scalaResult =
                list.asScala.flatMap(x => if x > value then scala.Some(x + value) else scala.None)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6296, steps = 1_160_960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 15356, steps = 3_363_500)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(3), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 26810, steps = 6_426_564)
          )
        )

    }

    test("find") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.find(_ > value)
            val scalaResult = list.asScala.find(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.find(_ > 1),
          List.empty[BigInt],
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 4864, steps = 911027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.find(_ > 1),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 9994, steps = 2_150917)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.find(_ > 1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 12956, steps = 3_001813)
          )
        )
    }

    test("findMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.findMap(x => if x > value then Some(x + value) else None)
            val scalaResult = list.asScala.find(_ > value).map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          List.empty[BigInt],
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 4564, steps = 863027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 13162, steps = 3_093_880)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(3)),
          Seq(
            compilerOptions -> ExUnits(memory = 19994, steps = 5_033_096)
          )
        )
    }

    test("foldLeft") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldLeft(initial)(_ + _)
            val scalaResult = list.asScala.foldLeft(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          List.empty[BigInt],
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 3896, steps = 771771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 10990, steps = 2_331_573)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          Seq(
            compilerOptions -> ExUnits(memory = 18084, steps = 3_891_375)
          )
        )
    }

    test("foldRight") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldRight(initial)(_ + _)
            val scalaResult = list.asScala.foldRight(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          List.empty[BigInt],
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 3896, steps = 771771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 9790, steps = 2_139_573)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          Seq(
            compilerOptions -> ExUnits(memory = 15684, steps = 3_507_375)
          )
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

    test("count - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.count(_ > value)
            val scalaResult = BigInt(list.asScala.count(_ > value))

            scalusResult === scalaResult
        }
    }

    test("count - empty list") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          List.empty[BigInt],
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 4496, steps = 867771)
          )
        )
    }

    test("count - single, _ > 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 0),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 12792, steps = 2_740_912)
          )
        )
    }

    test("count - single, _ > 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          List.single(1),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 12390, steps = 2_575_704)
          )
        )
    }

    test("count - two, _ > 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 20686, steps = 4_448_845)
          )
        )
    }

    test("count - two, _ > 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 2),
          Cons(1, Cons(2, Nil)),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 20284, steps = 4_283_637)
          )
        )
    }

    test("indexOfOption - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOfOption(value)
            val scalaResult = list.asScala.indexOf(value) match {
                case -1    => None
                case index => Some(BigInt(index))
            }

            scalusResult === scalaResult
        }
    }

    test("indexOfOption - empty list") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(1)),
          List.empty[BigInt],
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 3864, steps = 751027)
          )
        )
    }

    test("indexOfOption - single, found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(1)),
          List.single(1),
          Some(BigInt(0)),
          Seq(
            compilerOptions -> ExUnits(memory = 6594, steps = 2470581)
          )
        )
    }

    test("indexOfOption - single, not found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 8564, steps = 2996634)
          )
        )
    }

    test("indexOfOption - two, found at 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 11294, steps = 4716188)
          )
        )
    }

    test("indexOfOption - two, not found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 13264, steps = 5242241)
          )
        )
    }

    test("indexOf - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOf(value)
            val scalaResult = BigInt(list.asScala.indexOf(value))

            scalusResult === scalaResult
        }
    }

    test("indexOf - empty list") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(1)),
          List.empty[BigInt],
          BigInt(-1),
          Seq(
            compilerOptions -> ExUnits(memory = 8262, steps = 1_754636)
          )
        )
    }

    test("indexOf - single, found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(1)),
          List.single(1),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 11156, steps = 3715332)
          )
        )
    }

    test("indexOf - single, not found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          List.single(1),
          BigInt(-1),
          Seq(
            compilerOptions -> ExUnits(memory = 12662, steps = 3952243)
          )
        )
    }

    test("indexOf - two, found at 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 15556, steps = 5912939)
          )
        )
    }

    test("indexOf - two, not found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          BigInt(-1),
          Seq(
            compilerOptions -> ExUnits(memory = 17062, steps = 6149850)
          )
        )
    }

    test("lastOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.lastOption
            val scalaResult = list.asScala.lastOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.lastOption,
          List.empty[BigInt],
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 3564, steps = 703027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.lastOption,
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6225, steps = 1_512_835)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.lastOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 9954, steps = 2_535_637)
          )
        )
    }

    test("last") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.last)
            val scalaResult = liftThrowableToOption(list.asScala.last)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].last)

        assertEvalWithBudgets(
          List.single(BigInt(1)).last,
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 3130, steps = 980263)
          )
        )

        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).last,
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 3130, steps = 980263)
          )
        )
    }

    test("headOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.headOption
            val scalaResult = list.asScala.headOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.headOption,
          List.empty[BigInt],
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 1964, steps = 447027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.headOption,
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3260, steps = 816690)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.headOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3260, steps = 816690)
          )
        )
    }

    test("head") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.head)
            val scalaResult = liftThrowableToOption(list.asScala.head)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].head)

        assertEvalWithBudgets(
          List.single(BigInt(1)).head,
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 200, steps = 16100)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.head,
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 7622, steps = 2_029441)
          )
        )
    }

    test("length") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.length
            val scalaResult = BigInt(list.asScala.length)

            scalusResult === scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.length,
          List.empty[BigInt],
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 3896, steps = 771771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.length,
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 10458, steps = 2_230_829)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.length,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 17020, steps = 3_689_887)
          )
        )
    }

    test("size") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.size
            val scalaResult = BigInt(list.asScala.size)

            scalusResult === scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.size,
          List.empty[BigInt],
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 3896, steps = 771771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.size,
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 10458, steps = 2_230_829)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.size,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 17020, steps = 3_689_887)
          )
        )
    }

    test("tail") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.tail)
            val scalaResult = liftThrowableToOption(list.asScala.tail.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].tail)
        assertEvalWithBudgets(
          List.single(BigInt(1)).tail,
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 200, steps = 16100)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.tail,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 2296, steps = 576690)
          )
        )
    }

    test("drop - property") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.drop(number)
            val scalaResult = list.asScala.drop(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }
    }

    test("drop - empty drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4666, steps = 998913)
          )
        )
    }

    test("drop - single drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 8102, steps = 1_845_670)
          )
        )
    }

    test("drop - two drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 8102, steps = 1_845_670)
          )
        )
    }

    test("drop - two drop 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 12470, steps = 2_969_421)
          )
        )
    }

    test("drop - two drop 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 3734, steps = 721919)
          )
        )
    }

    test("drop - two drop -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 3734, steps = 721919)
          )
        )
    }

    test("dropRight - property") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.dropRight(number)
            val scalaResult = list.asScala.dropRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }
    }

    test("dropRight - empty drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 11122, steps = 2_448750)
          )
        )
    }

    test("dropRight - single drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 20706, steps = 5_144_972)
          )
        )
    }

    test("dropRight - two drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 30752, steps = 7_983_240)
          )
        )
    }

    test("dropRight - two drop 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 30290, steps = 7_841_194)
          )
        )
    }

    test("dropRight - two drop 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 5434, steps = 993919)
          )
        )
    }

    test("dropRight - two drop -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 5434, steps = 993919)
          )
        )
    }

    test("dropWhile - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.dropWhile(_ < value)
            val scalaResult = list.asScala.dropWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("dropWhile - empty list") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3464, steps = 687027)
          )
        )
    }

    test("dropWhile - single, _ < 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5530, steps = 1_232_260)
          )
        )
    }

    test("dropWhile - two, _ < 3") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 14324, steps = 3_262_807)
          )
        )
    }

    test("dropWhile - two, _ < 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 10960, steps = 2_520_150)
          )
        )
    }

    test("dropWhile - two, _ < 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 5530, steps = 1_232_260)
          )
        )
    }

    test("deleteFirst - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.deleteFirst(value)
            val scalaList = list.asScala
            val scalaResult = scalaList.indexOf(value) match {
                case -1    => scalaList
                case index => scalaList.patch(index, scala.Nil, 1)
            }

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("deleteFirst - empty list") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5064, steps = 943027)
          )
        )
    }

    test("deleteFirst - single, found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6630, steps = 2458432)
          )
        )
    }

    test("deleteFirst - two, head match") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 6630, steps = 2458432)
          )
        )
    }

    test("deleteFirst - two, no match") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 13788, steps = 5414849)
          )
        )
    }

    test("deleteFirst - two, tail match") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 10992, steps = 4694343)
          )
        )
    }

    test("deleteFirst - two duplicates, head only") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6630, steps = 2458432)
          )
        )
    }

    test("take - property") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.take(number)
            val scalaResult = list.asScala.take(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }
    }

    test("take - empty take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4666, steps = 998913)
          )
        )
    }

    test("take - single take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 8966, steps = 2_129_182)
          )
        )
    }

    test("take - two take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 8966, steps = 2_129_182)
          )
        )
    }

    test("take - two take 3") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 15130, steps = 3_813_439)
          )
        )
    }

    test("take - two take 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3734, steps = 721919)
          )
        )
    }

    test("take - two take -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3734, steps = 721919)
          )
        )
    }

    test("takeRight - property") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.takeRight(number)
            val scalaResult = list.asScala.takeRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }
    }

    test("takeRight - empty take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 8094, steps = 1_802576)
          )
        )
    }

    test("takeRight - single take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 18706, steps = 4_856642)
          )
        )
    }

    test("takeRight - two take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 25996, steps = 6_768_623)
          )
        )
    }

    test("takeRight - two take 3") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 29318, steps = 7_910_708)
          )
        )
    }

    test("takeRight - two take 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5434, steps = 993919)
          )
        )
    }

    test("takeRight - two take -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5434, steps = 993919)
          )
        )
    }

    test("takeWhile - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.takeWhile(_ < value)
            val scalaResult = list.asScala.takeWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("takeWhile - empty list") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4864, steps = 911027)
          )
        )
    }

    test("takeWhile - single, _ < 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6730, steps = 1_424_260)
          )
        )
    }

    test("takeWhile - two, _ < 3") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 16452, steps = 3_893_831)
          )
        )
    }

    test("takeWhile - two, _ < 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 12524, steps = 2_915662)
          )
        )
    }

    test("takeWhile - two, _ < 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6730, steps = 1_424_260)
          )
        )
    }

    test("distinct - property") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.distinct
            val scalaResult = list.asScala.distinct

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("distinct - empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 9760, steps = 1_863887)
          )
        )
    }

    test("distinct - single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 26044, steps = 5_831752)
          )
        )
    }

    test("distinct - two unique") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 47026, steps = 11944016)
          )
        )
    }

    test("distinct - two duplicates") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 38434, steps = 10060706)
          )
        )
    }

    test("distinct - three with one duplicate") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Cons(1, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 64114, steps = 18317369)
          )
        )
    }

    test("diff - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.diff(list2)
            val scalaResult = list1.asScala.diff(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("diff - empty diff empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6065, steps = 1_120515)
          )
        )
    }

    test("diff - single diff empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6797, steps = 1_365509)
          )
        )
    }

    test("diff - empty diff single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6065, steps = 1_120515)
          )
        )
    }

    test("diff - single diff matching single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 14292, steps = 4257203)
          )
        )
    }

    test("diff - two diff matching tail") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(2))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 19186, steps = 6706108)
          )
        )
    }

    test("diff - two diff non-matching") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 21782, steps = 7394614)
          )
        )
    }

    test("init") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.init)
            val scalaResult = liftThrowableToOption(list.asScala.init.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].init)

        assertEvalWithBudgets(
          List.single(BigInt(1)).init,
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 17346, steps = 4_457077)
          )
        )

        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).init,
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 27624, steps = 7_401_778)
          )
        )
    }

    test("reverse") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.reverse
            val scalaResult = list.asScala.reverse

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3896, steps = 776960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 10588, steps = 2_246_914)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(1), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 17280, steps = 3_716_868)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Cons(3, Nil))),
          Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 23972, steps = 5_186_822)
          )
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foreach(_ + 1),
          List.empty[BigInt],
          (),
          Seq(
            compilerOptions -> ExUnits(memory = 3464, steps = 687027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foreach(_ + 1),
          List.single(1),
          (),
          Seq(
            compilerOptions -> ExUnits(memory = 8292, steps = 1_757_578)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foreach(_ + 1),
          Cons(1, Cons(2, Nil)),
          (),
          Seq(
            compilerOptions -> ExUnits(memory = 13120, steps = 2_828_129)
          )
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

    test("at with vanRossemPV uses dropList intrinsic") {
        given opts: Options = compilerOptions.copy(
          targetProtocolVersion = MajorProtocolVersion.vanRossemPV
        )
        assertEvalEq(
          Cons(BigInt(10), Cons(BigInt(20), Cons(BigInt(30), Nil))).at(1),
          BigInt(20)
        )
        // Compare budget: vanRossemPV at(1) should be cheaper than changPV at(1)
        val v11Compiled = PlutusV3.compile(Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1))
        val v11Budget = v11Compiled.program.term.evaluateDebug
            .asInstanceOf[scalus.uplc.eval.Result.Success]
            .budget
        val v9Compiled = {
            given Options = compilerOptions
            PlutusV3.compile(Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1))
        }
        val v9Budget = v9Compiled.program.term.evaluateDebug
            .asInstanceOf[scalus.uplc.eval.Result.Success]
            .budget
        info(s"at(1) budget: changPV=$v9Budget, vanRossemPV=$v11Budget")
        assert(
          v11Budget.steps <= v9Budget.steps,
          s"vanRossemPV should not be more expensive: $v11Budget vs $v9Budget"
        )
    }
}
