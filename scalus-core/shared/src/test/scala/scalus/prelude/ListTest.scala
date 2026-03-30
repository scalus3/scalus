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
        if !compilerOptions.nativeListElements then assertEvalEq(List.empty[BigInt], Nil)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1332, steps = 264462)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8934, steps = 1_856784)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.range(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 16870, steps = 3_722294)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.range(n, -1),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4966, steps = 924029)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8934, steps = 1_859690)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, 4),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 16870, steps = 3_728106)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, 0),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4966, steps = 925482)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, -1),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4966, steps = 925482)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7638, steps = 1_616348)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 14510, steps = 3_294166)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, 0),
          BigInt(1),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4202, steps = 777439)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, -1),
          BigInt(1),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4202, steps = 777439)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6496, steps = 1_300021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.map2(List.empty[BigInt], list)(_ + _),
          Cons(3, Cons(4, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5564, steps = 1_023027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.map2(list, Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(4), Cons(BigInt(6), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 20408, steps = 4_946969)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 432, steps = 74033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 432, steps = 74033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          Cons(1, Cons(2, Cons(3, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 432, steps = 74033)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5696, steps = 1_172021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list === List.single(BigInt(1)),
          List.single(1),
          true,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12254, steps = 2_945505)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(1, Cons(2, Cons(3, Nil))),
          true,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 25370, steps = 6_492473)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8464, steps = 1_487027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.quicksort,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 25314, steps = 5_539532)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.quicksort,
          Cons(3, Cons(1, Cons(2, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 132831, steps = 31_892789)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6096, steps = 1_128960)
          )
        )

        assertEvalWithBudgets(
          (list: List[List[BigInt]]) => list.flatten,
          List.single(Cons(3, Cons(1, Cons(2, Nil)))),
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 14122, steps = 2_987016)
          )
        )

        assertEvalWithBudgets(
          (list: List[List[BigInt]]) => list.flatten,
          Cons[List[BigInt]](Cons(1, Cons(2, Nil)), List.single(List.single(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 28836, steps = 6_742404)
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

    test("get") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = list.get(index)
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          List.empty[BigInt],
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6366, steps = 1_272366)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8564, steps = 1_914411)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(1),
          List.single(1),
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10334, steps = 2_340613)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(-1),
          List.single(1),
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4234, steps = 803372)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8564, steps = 1_914411)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12532, steps = 2_982658)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(2),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 14302, steps = 3_408860)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(-1),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4234, steps = 803372)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6399, steps = 1_433419)
          )
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(-1))
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(0),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6399, steps = 1_433419)
          )
        )
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1),
          BigInt(2),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10367, steps = 2_501666)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6399, steps = 1_433419)
          )
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(-1))
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(0),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6399, steps = 1_433419)
          )
        )
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(1),
          BigInt(2),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10367, steps = 2_501666)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11760, steps = 2_224890)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupBy(_ % 2),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 53383, steps = 13_315930)
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMap(_ % 2)(identity),
          List.empty[BigInt],
          SortedMap.empty[BigInt, List[BigInt]],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11960, steps = 2_256890)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMap(_ % 2)(identity),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 54347, steps = 13_495973)
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          List.empty[BigInt],
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7364, steps = 1_311027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          List.single(1),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 23235, steps = 5_611664)
          )
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          Cons(1, Cons(2, Cons(3, Cons(4, Nil)))),
          SortedMap.unsafeFromList(
            Cons((BigInt(0), BigInt(6)), Cons((BigInt(1), BigInt(4)), Nil))
          ),
          ExUnits(memory = 141572, steps = 39_141453)
        )
    }

    test("zip") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.zip(list2)
            val scalaResult = list1.asScala.zip(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4764, steps = 895027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.empty[BigInt]),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5496, steps = 1_140021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt].zip(list),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4764, steps = 895027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.single(BigInt(2))),
          List.single(1),
          List.single((BigInt(1), BigInt(2))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10384, steps = 2_577878)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(Cons(BigInt(3), Cons(BigInt(4), Nil))),
          Cons(1, Cons(2, Nil)),
          Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 16004, steps = 4_260729)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          List.single((BigInt(1), BigInt(3))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11116, steps = 2_822872),
            compilerOptions.copy(nativeListElements = true) -> ExUnits(
              memory = 15976,
              steps = 4_071_183
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1564, steps = 322395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1564, steps = 322395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1564, steps = 322395)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1564, steps = 322395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => BigInt(1) +: list,
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1564, steps = 322395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => BigInt(1) +: list,
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 1564, steps = 322395)
          )
        )
    }

    test("prependedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.prependedAll(list2)
            val scalaResult = list1.asScala.prependedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6198, steps = 1_297058)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6198, steps = 1_297058)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7230, steps = 1_590052)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10590, steps = 2_498154)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10590, steps = 2_498154)
          )
        )
    }

    test("++:") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++: list2
            val scalaResult = list1.asScala ++: list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++: list,
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6198, steps = 1_297058)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++: list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7230, steps = 1_590052)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4433, steps = 790082)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8293, steps = 1_885245)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11121, steps = 2_687414)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5196, steps = 1_031389)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appended(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8324, steps = 1_881558)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appended(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11452, steps = 2_731727)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5196, steps = 1_031389)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :+ BigInt(2),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8324, steps = 1_881558)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :+ BigInt(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11452, steps = 2_731727)
          )
        )
    }

    test("appendedAll") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.appendedAll(list2)
            val scalaResult = list1.asScala.appendedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6066, steps = 1_196564)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7098, steps = 1_489558)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6066, steps = 1_196564)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 9926, steps = 2_291727)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12754, steps = 3_093896)
          )
        )
    }

    test(":++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 :++ list2
            val scalaResult = list1.asScala :++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.empty[BigInt],
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6066, steps = 1_196564)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] :++ list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6798, steps = 1_441558)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6066, steps = 1_196564)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 9926, steps = 2_291727)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12754, steps = 3_093896)
          )
        )
    }

    test("concat") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.concat(list2)
            val scalaResult = list1.asScala.concat(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6066, steps = 1_196564)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7098, steps = 1_489558)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6066, steps = 1_196564)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 9926, steps = 2_291727)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12754, steps = 3_093896)
          )
        )
    }

    test("++") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++ list2
            val scalaResult = list1.asScala ++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.empty[BigInt],
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6066, steps = 1_196564)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++ list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6798, steps = 1_441558)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6066, steps = 1_196564)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 9926, steps = 2_291727)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12754, steps = 3_093896)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5796, steps = 1_080960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.map(_ + 1),
          List.single(1),
          List.single(BigInt(2)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11954, steps = 2_592165)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.map(_ + 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 18112, steps = 4_103370)
          )
        )
    }

    test("flatMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.flatMap(x => List.single(x + value))
            val scalaResult = list.asScala.flatMap(x => scala.List(x + value))

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6696, steps = 1_224960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          List.single(1),
          List.single(BigInt(2)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 16088, steps = 3_474696)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 29340, steps = 6_819595)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(_ => Nil),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 23348, steps = 5_026694)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => Cons(x + 10, Cons(x + 100, Nil))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(11), Cons(BigInt(101), Cons(BigInt(12), Cons(BigInt(102), Nil)))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 34300, steps = 8_319502)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6296, steps = 1_160960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filter(_ > 1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12290, steps = 2_588635)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filter(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 18716, steps = 4_152672)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6296, steps = 1_160960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 13223, steps = 2_881046)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 19718, steps = 4_464770)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6896, steps = 1_256960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 15256, steps = 3_347500)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(3), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 26010, steps = 6_298564)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5864, steps = 1_071027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.find(_ > 1),
          List.single(1),
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10594, steps = 2_246917)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.find(_ > 1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 13356, steps = 3_065813)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6064, steps = 1_103027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 14162, steps = 3_253880)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(3)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 20694, steps = 5_145096)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5496, steps = 1_027771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12190, steps = 2_523573)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 18884, steps = 4_019375)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5496, steps = 1_027771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10990, steps = 2_331573)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 16484, steps = 3_635375)
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

    test("count") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.count(_ > value)
            val scalaResult = BigInt(list.asScala.count(_ > value))

            scalusResult === scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          List.empty[BigInt],
          BigInt(0),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5996, steps = 1_107771)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 0),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 13292, steps = 2_820912)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          List.single(1),
          BigInt(0),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12890, steps = 2_655704)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 20186, steps = 4_368845)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 2),
          Cons(1, Cons(2, Nil)),
          BigInt(0),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 19784, steps = 4_203637)
          )
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(1)),
          List.empty[BigInt],
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5264, steps = 975027)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(1)),
          List.single(1),
          Some(BigInt(0)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8758, steps = 1_865859)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          List.single(1),
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10528, steps = 2_359912)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 14022, steps = 3_250744)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 15792, steps = 3_744797)
          )
        )
    }

    test("indexOf") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOf(value)
            val scalaResult = BigInt(list.asScala.indexOf(value))

            scalusResult === scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(1)),
          List.empty[BigInt],
          BigInt(-1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8462, steps = 1_786636)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(1)),
          List.single(1),
          BigInt(0),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12420, steps = 2_966610)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          List.single(1),
          BigInt(-1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 13726, steps = 3_171521)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 17684, steps = 4_351495)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          BigInt(-1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 18990, steps = 4_556406)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5064, steps = 943027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.lastOption,
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7325, steps = 1_688835)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.lastOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10654, steps = 2_647637)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4430, steps = 1_188263)
          )
        )

        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).last,
          BigInt(2),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4430, steps = 1_188263)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 2964, steps = 607027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.headOption,
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4060, steps = 944690)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.headOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4060, steps = 944690)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 200, steps = 16100)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.head,
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8022, steps = 2_093441)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4896, steps = 931771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.length,
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11058, steps = 2_326829)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.length,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 17220, steps = 3_721887)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4896, steps = 931771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.size,
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11058, steps = 2_326829)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.size,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 17220, steps = 3_721887)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 200, steps = 16100)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.tail,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 3196, steps = 720690)
          )
        )
    }

    test("drop") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.drop(number)
            val scalaResult = list.asScala.drop(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5566, steps = 1_142913)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8802, steps = 1_957670)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8802, steps = 1_957670)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12770, steps = 3_017421)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4834, steps = 897919)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 4834, steps = 897919)
          )
        )
    }

    test("dropRight") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.dropRight(number)
            val scalaResult = list.asScala.dropRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11022, steps = 2_432750)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 20606, steps = 5_128972)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 30452, steps = 7_935240)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 30190, steps = 7_825194)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5834, steps = 1_057919)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5834, steps = 1_057919)
          )
        )
    }

    test("dropWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.dropWhile(_ < value)
            val scalaResult = list.asScala.dropWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5364, steps = 991027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7530, steps = 1_552260)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 15824, steps = 3_502807)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12760, steps = 2_808150)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7530, steps = 1_552260)
          )
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

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5064, steps = 943027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7694, steps = 1_677710)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7694, steps = 1_677710)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 15516, steps = 3_789405)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 12920, steps = 3_100899)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7694, steps = 1_677710)
          )
        )
    }

    test("take") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.take(number)
            val scalaResult = list.asScala.take(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6166, steps = 1_238913)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10066, steps = 2_305182)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 10066, steps = 2_305182)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 15430, steps = 3_861439)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5434, steps = 993919)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5434, steps = 993919)
          )
        )
    }

    test("takeRight") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], number: BigInt) =>
            val scalusResult = list.takeRight(number)
            val scalaResult = list.asScala.takeRight(number.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 8294, steps = 1_834576)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 18706, steps = 4_856642)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 25796, steps = 6_736623)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 29118, steps = 7_878708)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5834, steps = 1_057919)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5834, steps = 1_057919)
          )
        )
    }

    test("takeWhile") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.takeWhile(_ < value)
            val scalaResult = list.asScala.takeWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5664, steps = 1_039027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7830, steps = 1_600260)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 17452, steps = 4_053831)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 13724, steps = 3_107662)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7830, steps = 1_600260)
          )
        )
    }

    test("distinct") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.distinct
            val scalaResult = list.asScala.distinct

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 9760, steps = 1_863887)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 26044, steps = 5_831752)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 48390, steps = 11_211294)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 39798, steps = 9_327984)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Cons(1, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 68206, steps = 16_119203)
          )
        )
    }

    test("diff") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.diff(list2)
            val scalaResult = list1.asScala.diff(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.empty[BigInt]),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6665, steps = 1_216515)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 7397, steps = 1_461509)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 6665, steps = 1_216515)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 16256, steps = 3_620481)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(2))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 22514, steps = 5_336664)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 25110, steps = 6_025170)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 17046, steps = 4_409077)
          )
        )

        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).init,
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 27424, steps = 7_369778)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5196, steps = 984960)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 11388, steps = 2_374914)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(1), Nil)),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 17580, steps = 3_764868)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Cons(3, Nil))),
          Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil))),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 23772, steps = 5_154822)
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
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 5064, steps = 943027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foreach(_ + 1),
          List.single(1),
          (),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 9492, steps = 1_949578)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foreach(_ + 1),
          Cons(1, Cons(2, Nil)),
          (),
          Seq(
            compilerOptions
                .copy(nativeListElements = false) -> ExUnits(memory = 13920, steps = 2_956129)
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
