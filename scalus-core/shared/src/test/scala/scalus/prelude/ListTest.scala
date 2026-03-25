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
            compilerOptions -> ExUnits(memory = 1332, steps = 264462),
            nativeOpts -> ExUnits(memory = 1564, steps = 317_206)
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
            compilerOptions -> ExUnits(memory = 8934, steps = 1_856784),
            nativeOpts -> ExUnits(memory = 7870, steps = 1_666_186)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.range(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 16870, steps = 3_722294),
            nativeOpts -> ExUnits(memory = 14742, steps = 3_341_098)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.range(n, -1),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4966, steps = 924029),
            nativeOpts -> ExUnits(memory = 4434, steps = 828_730)
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
            compilerOptions -> ExUnits(memory = 8934, steps = 1_859690),
            nativeOpts -> ExUnits(memory = 7870, steps = 1_669_092)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, 4),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 16870, steps = 3_728106),
            nativeOpts -> ExUnits(memory = 14742, steps = 3_346_910)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, 0),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4966, steps = 925482),
            nativeOpts -> ExUnits(memory = 4434, steps = 830_183)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, -1),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4966, steps = 925482),
            nativeOpts -> ExUnits(memory = 4434, steps = 830_183)
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
            compilerOptions -> ExUnits(memory = 7638, steps = 1_616348),
            nativeOpts -> ExUnits(memory = 13930, steps = 3_116_255)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 14510, steps = 3_294166),
            nativeOpts -> ExUnits(memory = 26922, steps = 6_503_899)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, 0),
          BigInt(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4202, steps = 777439),
            nativeOpts -> ExUnits(memory = 7434, steps = 1_422_433)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, -1),
          BigInt(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4202, steps = 777439),
            nativeOpts -> ExUnits(memory = 7434, steps = 1_422_433)
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
            compilerOptions -> ExUnits(memory = 6496, steps = 1_300021),
            nativeOpts -> ExUnits(memory = 10960, steps = 2_270_009)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.map2(List.empty[BigInt], list)(_ + _),
          Cons(3, Cons(4, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5564, steps = 1_023027),
            nativeOpts -> ExUnits(memory = 28132, steps = 6_595_765)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.map2(list, Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(4), Cons(BigInt(6), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 20408, steps = 4_946969),
            nativeOpts -> ExUnits(memory = 37112, steps = 9_325_719)
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
            compilerOptions -> ExUnits(memory = 432, steps = 74033),
            nativeOpts -> ExUnits(memory = 4164, steps = 799_027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 432, steps = 74033),
            nativeOpts -> ExUnits(memory = 7224, steps = 1_653_940)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => fromData[List[BigInt]](list.toData),
          Cons(1, Cons(2, Cons(3, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 432, steps = 74033),
            nativeOpts -> ExUnits(memory = 13344, steps = 3_363_766)
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
            compilerOptions -> ExUnits(memory = 5696, steps = 1_172021),
            nativeOpts -> ExUnits(memory = 12692, steps = 2_675_003)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list === List.single(BigInt(1)),
          List.single(1),
          true,
          Seq(
            compilerOptions -> ExUnits(memory = 12254, steps = 2_945505),
            nativeOpts -> ExUnits(memory = 28430, steps = 7_002_336)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(1, Cons(2, Cons(3, Nil))),
          true,
          Seq(
            compilerOptions -> ExUnits(memory = 25370, steps = 6_492473),
            nativeOpts -> ExUnits(memory = 59906, steps = 15_657_002)
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
            compilerOptions -> ExUnits(memory = 8464, steps = 1_487027),
            nativeOpts -> ExUnits(memory = 10396, steps = 1_924_021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.quicksort,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 25314, steps = 5_539532),
            nativeOpts -> ExUnits(memory = 30306, steps = 6_831_439)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.quicksort,
          Cons(3, Cons(1, Cons(2, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 132831, steps = 31_892789),
            nativeOpts -> ExUnits(memory = 143943, steps = 34_894_522)
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
            compilerOptions -> ExUnits(memory = 6096, steps = 1_128960),
            nativeOpts -> ExUnits(memory = 12792, steps = 2_583_942)
          )
        )

        assertEvalWithBudget(
          (list: List[List[BigInt]]) => list.flatten,
          List.single(Cons(3, Cons(1, Cons(2, Nil)))),
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))),
          ExUnits(memory = 14122, steps = 2_987016)
        )

        assertEvalWithBudget(
          (list: List[List[BigInt]]) => list.flatten,
          Cons[List[BigInt]](Cons(1, Cons(2, Nil)), List.single(List.single(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          ExUnits(memory = 28836, steps = 6_742404)
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
            compilerOptions -> ExUnits(memory = 6366, steps = 1_272366),
            nativeOpts -> ExUnits(memory = 6366, steps = 1_272_366)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 8564, steps = 1_914411),
            nativeOpts -> ExUnits(memory = 8564, steps = 1_914_411)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(1),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 10334, steps = 2_340613),
            nativeOpts -> ExUnits(memory = 10334, steps = 2_340_613)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(-1),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 4234, steps = 803372),
            nativeOpts -> ExUnits(memory = 4234, steps = 803_372)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 8564, steps = 1_914411),
            nativeOpts -> ExUnits(memory = 8564, steps = 1_914_411)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 12532, steps = 2_982658),
            nativeOpts -> ExUnits(memory = 12532, steps = 2_982_658)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(2),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 14302, steps = 3_408860),
            nativeOpts -> ExUnits(memory = 14302, steps = 3_408_860)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(-1),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 4234, steps = 803372),
            nativeOpts -> ExUnits(memory = 4234, steps = 803_372)
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
            compilerOptions -> ExUnits(memory = 6399, steps = 1_433419),
            nativeOpts -> ExUnits(memory = 12992, steps = 3_021_171)
          )
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(-1))
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(0),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6399, steps = 1_433419),
            nativeOpts -> ExUnits(memory = 16052, steps = 3_870_639)
          )
        )
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 10367, steps = 2_501666),
            nativeOpts -> ExUnits(memory = 20020, steps = 4_938_886)
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
            compilerOptions -> ExUnits(memory = 6399, steps = 1_433419),
            nativeOpts -> ExUnits(memory = 12992, steps = 3_021_171)
          )
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(-1))
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(0),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6399, steps = 1_433419),
            nativeOpts -> ExUnits(memory = 16052, steps = 3_870_639)
          )
        )
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(1),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 10367, steps = 2_501666),
            nativeOpts -> ExUnits(memory = 20020, steps = 4_938_886)
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
            compilerOptions -> ExUnits(memory = 11760, steps = 2_224890),
            nativeOpts -> ExUnits(memory = 11760, steps = 2_224_890)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupBy(_ % 2),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          Seq(
            compilerOptions -> ExUnits(memory = 53383, steps = 13_315930),
            nativeOpts -> ExUnits(memory = 53383, steps = 13_315_930)
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
            compilerOptions -> ExUnits(memory = 11960, steps = 2_256890),
            nativeOpts -> ExUnits(memory = 11960, steps = 2_256_890)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMap(_ % 2)(identity),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          Seq(
            compilerOptions -> ExUnits(memory = 54347, steps = 13_495973),
            nativeOpts -> ExUnits(memory = 54347, steps = 13_495_973)
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
            compilerOptions -> ExUnits(memory = 7364, steps = 1_311027),
            nativeOpts -> ExUnits(memory = 7364, steps = 1_311_027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          List.single(1),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 23235, steps = 5_611664),
            nativeOpts -> ExUnits(memory = 23235, steps = 5_611_664)
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
            compilerOptions -> ExUnits(memory = 4764, steps = 895027),
            nativeOpts -> ExUnits(memory = 6996, steps = 1_380_021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.empty[BigInt]),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions -> ExUnits(memory = 5496, steps = 1_140021),
            nativeOpts -> ExUnits(memory = 7728, steps = 1_625_015)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt].zip(list),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions -> ExUnits(memory = 4764, steps = 895027),
            nativeOpts -> ExUnits(memory = 6996, steps = 1_380_021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.single(BigInt(2))),
          List.single(1),
          List.single((BigInt(1), BigInt(2))),
          Seq(
            compilerOptions -> ExUnits(memory = 10384, steps = 2_577878),
            nativeOpts -> ExUnits(memory = 15676, steps = 3_912_340)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(Cons(BigInt(3), Cons(BigInt(4), Nil))),
          Cons(1, Cons(2, Nil)),
          Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 16004, steps = 4_260729),
            nativeOpts -> ExUnits(memory = 24356, steps = 6_444_659)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          List.single((BigInt(1), BigInt(3))),
          Seq(
            compilerOptions -> ExUnits(memory = 11116, steps = 2_822872),
            nativeOpts -> ExUnits(memory = 16408, steps = 4_157_334)
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
            compilerOptions -> ExUnits(memory = 1564, steps = 322395),
            nativeOpts -> ExUnits(memory = 4596, steps = 935_389)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 1564, steps = 322395),
            nativeOpts -> ExUnits(memory = 7656, steps = 1_790_302)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prepended(BigInt(1)),
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 1564, steps = 322395),
            nativeOpts -> ExUnits(memory = 10716, steps = 2_645_215)
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
            compilerOptions -> ExUnits(memory = 1564, steps = 322395),
            nativeOpts -> ExUnits(memory = 4596, steps = 935_389)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => BigInt(1) +: list,
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 1564, steps = 322395),
            nativeOpts -> ExUnits(memory = 7656, steps = 1_790_302)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => BigInt(1) +: list,
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 1564, steps = 322395),
            nativeOpts -> ExUnits(memory = 10716, steps = 2_645_215)
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
            compilerOptions -> ExUnits(memory = 6198, steps = 1_297058),
            nativeOpts -> ExUnits(memory = 6498, steps = 1_345_058)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6198, steps = 1_297058),
            nativeOpts -> ExUnits(memory = 6498, steps = 1_345_058)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7230, steps = 1_590052),
            nativeOpts -> ExUnits(memory = 12522, steps = 2_929_959)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 10590, steps = 2_498154),
            nativeOpts -> ExUnits(memory = 15882, steps = 3_838_061)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 10590, steps = 2_498154),
            nativeOpts -> ExUnits(memory = 18942, steps = 4_692_974)
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
            compilerOptions -> ExUnits(memory = 6198, steps = 1_297058),
            nativeOpts -> ExUnits(memory = 6498, steps = 1_345_058)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++: list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7230, steps = 1_590052),
            nativeOpts -> ExUnits(memory = 12522, steps = 2_929_959)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 4433, steps = 790082),
            nativeOpts -> ExUnits(memory = 9725, steps = 2_129_989)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 8293, steps = 1_885245),
            nativeOpts -> ExUnits(memory = 13585, steps = 3_225_152)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 11121, steps = 2_687414),
            nativeOpts -> ExUnits(memory = 19473, steps = 4_882_234)
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
            compilerOptions -> ExUnits(memory = 5196, steps = 1_031389),
            nativeOpts -> ExUnits(memory = 10788, steps = 2_419_296)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appended(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 8324, steps = 1_881558),
            nativeOpts -> ExUnits(memory = 16976, steps = 4_124_378)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appended(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 11452, steps = 2_731727),
            nativeOpts -> ExUnits(memory = 23164, steps = 5_829_460)
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
            compilerOptions -> ExUnits(memory = 5196, steps = 1_031389),
            nativeOpts -> ExUnits(memory = 10788, steps = 2_419_296)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :+ BigInt(2),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 8324, steps = 1_881558),
            nativeOpts -> ExUnits(memory = 16976, steps = 4_124_378)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :+ BigInt(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 11452, steps = 2_731727),
            nativeOpts -> ExUnits(memory = 23164, steps = 5_829_460)
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
            compilerOptions -> ExUnits(memory = 6066, steps = 1_196564),
            nativeOpts -> ExUnits(memory = 10230, steps = 2_118_552)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7098, steps = 1_489558),
            nativeOpts -> ExUnits(memory = 17382, steps = 4_115_927)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6066, steps = 1_196564),
            nativeOpts -> ExUnits(memory = 13290, steps = 2_973_465)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9926, steps = 2_291727),
            nativeOpts -> ExUnits(memory = 23270, steps = 5_773_009)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 12754, steps = 3_093896),
            nativeOpts -> ExUnits(memory = 29158, steps = 7_430_091)
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
            compilerOptions -> ExUnits(memory = 6066, steps = 1_196564),
            nativeOpts -> ExUnits(memory = 10230, steps = 2_118_552)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] :++ list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6798, steps = 1_441558),
            nativeOpts -> ExUnits(memory = 14322, steps = 3_266_459)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6066, steps = 1_196564),
            nativeOpts -> ExUnits(memory = 13290, steps = 2_973_465)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9926, steps = 2_291727),
            nativeOpts -> ExUnits(memory = 23270, steps = 5_773_009)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 12754, steps = 3_093896),
            nativeOpts -> ExUnits(memory = 29158, steps = 7_430_091)
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
            compilerOptions -> ExUnits(memory = 6066, steps = 1_196564),
            nativeOpts -> ExUnits(memory = 10230, steps = 2_118_552)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7098, steps = 1_489558),
            nativeOpts -> ExUnits(memory = 17382, steps = 4_115_927)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6066, steps = 1_196564),
            nativeOpts -> ExUnits(memory = 13290, steps = 2_973_465)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9926, steps = 2_291727),
            nativeOpts -> ExUnits(memory = 23270, steps = 5_773_009)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 12754, steps = 3_093896),
            nativeOpts -> ExUnits(memory = 29158, steps = 7_430_091)
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
            compilerOptions -> ExUnits(memory = 6066, steps = 1_196564),
            nativeOpts -> ExUnits(memory = 10230, steps = 2_118_552)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++ list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6798, steps = 1_441558),
            nativeOpts -> ExUnits(memory = 14322, steps = 3_266_459)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6066, steps = 1_196564),
            nativeOpts -> ExUnits(memory = 13290, steps = 2_973_465)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9926, steps = 2_291727),
            nativeOpts -> ExUnits(memory = 23270, steps = 5_773_009)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 12754, steps = 3_093896),
            nativeOpts -> ExUnits(memory = 29158, steps = 7_430_091)
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
            compilerOptions -> ExUnits(memory = 5796, steps = 1_080960),
            nativeOpts -> ExUnits(memory = 8328, steps = 1_613_954)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.map(_ + 1),
          List.single(1),
          List.single(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 11954, steps = 2_592165),
            nativeOpts -> ExUnits(memory = 17546, steps = 3_980_072)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.map(_ + 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 18112, steps = 4_103370),
            nativeOpts -> ExUnits(memory = 26764, steps = 6_346_190)
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
            compilerOptions -> ExUnits(memory = 6696, steps = 1_224960),
            nativeOpts -> ExUnits(memory = 8928, steps = 1_709_954)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          List.single(1),
          List.single(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 16088, steps = 3_474696),
            nativeOpts -> ExUnits(memory = 26140, steps = 6_053_766)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 29340, steps = 6_819595),
            nativeOpts -> ExUnits(memory = 47212, steps = 11_492_741)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(_ => Nil),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 23348, steps = 5_026694),
            nativeOpts -> ExUnits(memory = 25580, steps = 5_511_688)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => Cons(x + 10, Cons(x + 100, Nil))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(11), Cons(BigInt(101), Cons(BigInt(12), Cons(BigInt(102), Nil)))),
          Seq(
            compilerOptions -> ExUnits(memory = 34300, steps = 8_319502),
            nativeOpts -> ExUnits(memory = 63948, steps = 16_306_812)
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
            compilerOptions -> ExUnits(memory = 6296, steps = 1_160960),
            nativeOpts -> ExUnits(memory = 8528, steps = 1_645_954)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filter(_ > 1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 12290, steps = 2_588635),
            nativeOpts -> ExUnits(memory = 14522, steps = 3_073_629)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filter(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 18716, steps = 4_152672),
            nativeOpts -> ExUnits(memory = 24008, steps = 5_492_579)
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
            compilerOptions -> ExUnits(memory = 6296, steps = 1_160960),
            nativeOpts -> ExUnits(memory = 8528, steps = 1_645_954)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 13223, steps = 2_881046),
            nativeOpts -> ExUnits(memory = 18515, steps = 4_220_953)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 19718, steps = 4_464770),
            nativeOpts -> ExUnits(memory = 25010, steps = 5_804_677)
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
            compilerOptions -> ExUnits(memory = 6896, steps = 1_256960),
            nativeOpts -> ExUnits(memory = 9428, steps = 1_789_954)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 15256, steps = 3_347500),
            nativeOpts -> ExUnits(memory = 17788, steps = 3_880_494)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(3), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 26010, steps = 6_298564),
            nativeOpts -> ExUnits(memory = 31602, steps = 7_686_471)
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
            compilerOptions -> ExUnits(memory = 5864, steps = 1_071027),
            nativeOpts -> ExUnits(memory = 5864, steps = 1_071_027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.find(_ > 1),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 10594, steps = 2_246917),
            nativeOpts -> ExUnits(memory = 10594, steps = 2_246_917)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.find(_ > 1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 13356, steps = 3_065813),
            nativeOpts -> ExUnits(memory = 13356, steps = 3_065_813)
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
            compilerOptions -> ExUnits(memory = 6064, steps = 1_103027),
            nativeOpts -> ExUnits(memory = 6064, steps = 1_103_027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 14162, steps = 3_253880),
            nativeOpts -> ExUnits(memory = 14162, steps = 3_253_880)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(3)),
          Seq(
            compilerOptions -> ExUnits(memory = 20694, steps = 5_145096),
            nativeOpts -> ExUnits(memory = 20694, steps = 5_145_096)
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
            compilerOptions -> ExUnits(memory = 5496, steps = 1_027771),
            nativeOpts -> ExUnits(memory = 5496, steps = 1_027_771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 12190, steps = 2_523573),
            nativeOpts -> ExUnits(memory = 12190, steps = 2_523_573)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          Seq(
            compilerOptions -> ExUnits(memory = 18884, steps = 4_019375),
            nativeOpts -> ExUnits(memory = 18884, steps = 4_019_375)
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
            compilerOptions -> ExUnits(memory = 5496, steps = 1_027771),
            nativeOpts -> ExUnits(memory = 5496, steps = 1_027_771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 10990, steps = 2_331573),
            nativeOpts -> ExUnits(memory = 10990, steps = 2_331_573)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          Seq(
            compilerOptions -> ExUnits(memory = 16484, steps = 3_635375),
            nativeOpts -> ExUnits(memory = 16484, steps = 3_635_375)
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
            compilerOptions -> ExUnits(memory = 5996, steps = 1_107771),
            nativeOpts -> ExUnits(memory = 5996, steps = 1_107_771)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 0),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 13292, steps = 2_820912),
            nativeOpts -> ExUnits(memory = 13292, steps = 2_820_912)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          List.single(1),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 12890, steps = 2_655704),
            nativeOpts -> ExUnits(memory = 12890, steps = 2_655_704)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 20186, steps = 4_368845),
            nativeOpts -> ExUnits(memory = 20186, steps = 4_368_845)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 2),
          Cons(1, Cons(2, Nil)),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 19784, steps = 4_203637),
            nativeOpts -> ExUnits(memory = 19784, steps = 4_203_637)
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
            compilerOptions -> ExUnits(memory = 5264, steps = 975027),
            nativeOpts -> ExUnits(memory = 5264, steps = 975_027)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(1)),
          List.single(1),
          Some(BigInt(0)),
          Seq(
            compilerOptions -> ExUnits(memory = 8758, steps = 1_865859),
            nativeOpts -> ExUnits(memory = 8758, steps = 1_865_859)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 10528, steps = 2_359912),
            nativeOpts -> ExUnits(memory = 10528, steps = 2_359_912)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 14022, steps = 3_250744),
            nativeOpts -> ExUnits(memory = 14022, steps = 3_250_744)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 15792, steps = 3_744797),
            nativeOpts -> ExUnits(memory = 15792, steps = 3_744_797)
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
            compilerOptions -> ExUnits(memory = 8462, steps = 1_786636),
            nativeOpts -> ExUnits(memory = 8462, steps = 1_786_636)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(1)),
          List.single(1),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 12420, steps = 2_966610),
            nativeOpts -> ExUnits(memory = 12420, steps = 2_966_610)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          List.single(1),
          BigInt(-1),
          Seq(
            compilerOptions -> ExUnits(memory = 13726, steps = 3_171521),
            nativeOpts -> ExUnits(memory = 13726, steps = 3_171_521)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 17684, steps = 4_351495),
            nativeOpts -> ExUnits(memory = 17684, steps = 4_351_495)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          BigInt(-1),
          Seq(
            compilerOptions -> ExUnits(memory = 18990, steps = 4_556406),
            nativeOpts -> ExUnits(memory = 18990, steps = 4_556_406)
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
            compilerOptions -> ExUnits(memory = 5064, steps = 943027),
            nativeOpts -> ExUnits(memory = 5064, steps = 943_027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.lastOption,
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7325, steps = 1_688835),
            nativeOpts -> ExUnits(memory = 7325, steps = 1_688_835)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.lastOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 10654, steps = 2_647637),
            nativeOpts -> ExUnits(memory = 10654, steps = 2_647_637)
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
            compilerOptions -> ExUnits(memory = 4430, steps = 1_188263),
            nativeOpts -> ExUnits(memory = 15447, steps = 3_970_115)
          )
        )

        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).last,
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 4430, steps = 1_188263),
            nativeOpts -> ExUnits(memory = 21836, steps = 5_778_385)
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
            compilerOptions -> ExUnits(memory = 2964, steps = 607027),
            nativeOpts -> ExUnits(memory = 2964, steps = 607_027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.headOption,
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 4060, steps = 944690),
            nativeOpts -> ExUnits(memory = 4060, steps = 944_690)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.headOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 4060, steps = 944690),
            nativeOpts -> ExUnits(memory = 4060, steps = 944_690)
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
            compilerOptions -> ExUnits(memory = 200, steps = 16100),
            nativeOpts -> ExUnits(memory = 200, steps = 16_100)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.head,
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 8022, steps = 2_093441),
            nativeOpts -> ExUnits(memory = 8022, steps = 2_093_441)
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
            compilerOptions -> ExUnits(memory = 4896, steps = 931771),
            nativeOpts -> ExUnits(memory = 4896, steps = 931_771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.length,
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 11058, steps = 2_326829),
            nativeOpts -> ExUnits(memory = 11058, steps = 2_326_829)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.length,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 17220, steps = 3_721887),
            nativeOpts -> ExUnits(memory = 17220, steps = 3_721_887)
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
            compilerOptions -> ExUnits(memory = 4896, steps = 931771),
            nativeOpts -> ExUnits(memory = 4896, steps = 931_771)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.size,
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 11058, steps = 2_326829),
            nativeOpts -> ExUnits(memory = 11058, steps = 2_326_829)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.size,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 17220, steps = 3_721887),
            nativeOpts -> ExUnits(memory = 17220, steps = 3_721_887)
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
            compilerOptions -> ExUnits(memory = 200, steps = 16100),
            nativeOpts -> ExUnits(memory = 200, steps = 16_100)
          )
        )
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.tail,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 3196, steps = 720690),
            nativeOpts -> ExUnits(memory = 8788, steps = 2_108_597)
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
            compilerOptions -> ExUnits(memory = 5566, steps = 1_142913),
            nativeOpts -> ExUnits(memory = 8998, steps = 1_819_907)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 8802, steps = 1_957670),
            nativeOpts -> ExUnits(memory = 12234, steps = 2_634_664)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 8802, steps = 1_957670),
            nativeOpts -> ExUnits(memory = 15294, steps = 3_489_577)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 12770, steps = 3_017421),
            nativeOpts -> ExUnits(memory = 16202, steps = 3_694_415)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 4834, steps = 897919),
            nativeOpts -> ExUnits(memory = 14386, steps = 3_284_739)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 4834, steps = 897919),
            nativeOpts -> ExUnits(memory = 14386, steps = 3_284_739)
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
            compilerOptions -> ExUnits(memory = 11022, steps = 2_432750),
            nativeOpts -> ExUnits(memory = 13254, steps = 2_917_744)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 20606, steps = 5_128972),
            nativeOpts -> ExUnits(memory = 22838, steps = 5_613_966)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 30452, steps = 7_935240),
            nativeOpts -> ExUnits(memory = 35744, steps = 9_275_147)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 30190, steps = 7_825194),
            nativeOpts -> ExUnits(memory = 32422, steps = 8_310_188)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 5834, steps = 1_057919),
            nativeOpts -> ExUnits(memory = 14186, steps = 3_252_739)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 5834, steps = 1_057919),
            nativeOpts -> ExUnits(memory = 14186, steps = 3_252_739)
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
            compilerOptions -> ExUnits(memory = 5364, steps = 991027),
            nativeOpts -> ExUnits(memory = 7896, steps = 1_524_021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7530, steps = 1_552260),
            nativeOpts -> ExUnits(memory = 13122, steps = 2_940_167)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 15824, steps = 3_502807),
            nativeOpts -> ExUnits(memory = 18356, steps = 4_035_801)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 12760, steps = 2_808150),
            nativeOpts -> ExUnits(memory = 18352, steps = 4_196_057)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 7530, steps = 1_552260),
            nativeOpts -> ExUnits(memory = 16182, steps = 3_795_080)
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
            compilerOptions -> ExUnits(memory = 5064, steps = 943027),
            nativeOpts -> ExUnits(memory = 7296, steps = 1_428_021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 7694, steps = 1_677710),
            nativeOpts -> ExUnits(memory = 9926, steps = 2_162_704)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 7694, steps = 1_677710),
            nativeOpts -> ExUnits(memory = 12986, steps = 3_017_617)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 15516, steps = 3_789405),
            nativeOpts -> ExUnits(memory = 23868, steps = 5_984_225)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 12920, steps = 3_100899),
            nativeOpts -> ExUnits(memory = 18212, steps = 4_440_806)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7694, steps = 1_677710),
            nativeOpts -> ExUnits(memory = 12986, steps = 3_017_617)
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
            compilerOptions -> ExUnits(memory = 6166, steps = 1_238913),
            nativeOpts -> ExUnits(memory = 8998, steps = 1_819_907)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 10066, steps = 2_305182),
            nativeOpts -> ExUnits(memory = 15958, steps = 3_741_089)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 10066, steps = 2_305182),
            nativeOpts -> ExUnits(memory = 15958, steps = 3_741_089)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 15430, steps = 3_861439),
            nativeOpts -> ExUnits(memory = 24382, steps = 6_152_259)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5434, steps = 993919),
            nativeOpts -> ExUnits(memory = 8266, steps = 1_574_913)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5434, steps = 993919),
            nativeOpts -> ExUnits(memory = 8266, steps = 1_574_913)
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
            compilerOptions -> ExUnits(memory = 8294, steps = 1_834576),
            nativeOpts -> ExUnits(memory = 10526, steps = 2_319_570)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 18706, steps = 4_856642),
            nativeOpts -> ExUnits(memory = 23998, steps = 6_196_549)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 25796, steps = 6_736623),
            nativeOpts -> ExUnits(memory = 31088, steps = 8_076_530)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 29118, steps = 7_878708),
            nativeOpts -> ExUnits(memory = 37470, steps = 10_073_528)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5834, steps = 1_057919),
            nativeOpts -> ExUnits(memory = 8066, steps = 1_542_913)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5834, steps = 1_057919),
            nativeOpts -> ExUnits(memory = 8066, steps = 1_542_913)
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
            compilerOptions -> ExUnits(memory = 5664, steps = 1_039027),
            nativeOpts -> ExUnits(memory = 7896, steps = 1_524_021)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 7830, steps = 1_600260),
            nativeOpts -> ExUnits(memory = 10062, steps = 2_085_254)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 17452, steps = 4_053831),
            nativeOpts -> ExUnits(memory = 25804, steps = 6_248_651)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 13724, steps = 3_107662),
            nativeOpts -> ExUnits(memory = 19016, steps = 4_447_569)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 7830, steps = 1_600260),
            nativeOpts -> ExUnits(memory = 10062, steps = 2_085_254)
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
            compilerOptions -> ExUnits(memory = 9760, steps = 1_863887),
            nativeOpts -> ExUnits(memory = 11692, steps = 2_300_881)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 26044, steps = 5_831752),
            nativeOpts -> ExUnits(memory = 31036, steps = 7_123_659)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 48390, steps = 11_211294),
            nativeOpts -> ExUnits(memory = 56442, steps = 13_358_114)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 39798, steps = 9_327984),
            nativeOpts -> ExUnits(memory = 44790, steps = 10_619_891)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Cons(1, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 68206, steps = 16_119203),
            nativeOpts -> ExUnits(memory = 76258, steps = 18_266_023)
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
            compilerOptions -> ExUnits(memory = 6665, steps = 1_216515),
            nativeOpts -> ExUnits(memory = 11129, steps = 2_186_503)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7397, steps = 1_461509),
            nativeOpts -> ExUnits(memory = 14921, steps = 3_286_410)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6665, steps = 1_216515),
            nativeOpts -> ExUnits(memory = 14189, steps = 3_035_971)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 16256, steps = 3_620481),
            nativeOpts -> ExUnits(memory = 23780, steps = 5_439_937)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(2))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 22514, steps = 5_336664),
            nativeOpts -> ExUnits(memory = 33098, steps = 8_011_033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 25110, steps = 6_025170),
            nativeOpts -> ExUnits(memory = 38754, steps = 9_554_452)
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
            compilerOptions -> ExUnits(memory = 17046, steps = 4_409077),
            nativeOpts -> ExUnits(memory = 28299, steps = 7_030_544)
          )
        )

        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).init,
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 27424, steps = 7_369778),
            nativeOpts -> ExUnits(memory = 44265, steps = 11_541_193)
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
            compilerOptions -> ExUnits(memory = 5196, steps = 984960),
            nativeOpts -> ExUnits(memory = 7728, steps = 1_517_954)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 11388, steps = 2_374914),
            nativeOpts -> ExUnits(memory = 16980, steps = 3_762_821)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(1), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 17580, steps = 3_764868),
            nativeOpts -> ExUnits(memory = 26232, steps = 6_007_688)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Cons(3, Nil))),
          Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 23772, steps = 5_154822),
            nativeOpts -> ExUnits(memory = 35484, steps = 8_252_555)
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
            compilerOptions -> ExUnits(memory = 5064, steps = 943027),
            nativeOpts -> ExUnits(memory = 5064, steps = 943_027)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foreach(_ + 1),
          List.single(1),
          (),
          Seq(
            compilerOptions -> ExUnits(memory = 9492, steps = 1_949578),
            nativeOpts -> ExUnits(memory = 9492, steps = 1_949_578)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foreach(_ + 1),
          Cons(1, Cons(2, Nil)),
          (),
          Seq(
            compilerOptions -> ExUnits(memory = 13920, steps = 2_956129),
            nativeOpts -> ExUnits(memory = 13920, steps = 2_956_129)
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
