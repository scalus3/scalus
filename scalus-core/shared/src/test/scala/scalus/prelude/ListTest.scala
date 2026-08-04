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
import scalus.uplc.eval.PlutusVM

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
            compilerOptions -> ExUnits(memory = 6232, steps = 1_272686)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.range(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 11966, steps = 2_634098)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.range(n, -1),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3365, steps = 591980)
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
            compilerOptions -> ExUnits(memory = 6232, steps = 1_275592)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, 4),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 11966, steps = 2_639910)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, 0),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3365, steps = 593433)
          )
        )
        assertEvalWithBudgets(
          (n: BigInt) => List.rangeUntil(n, -1),
          BigInt(0),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3365, steps = 593433)
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
            compilerOptions -> ExUnits(memory = 4936, steps = 1_032250)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, 3),
          BigInt(1),
          Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 9606, steps = 2_205970)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, 0),
          BigInt(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 2601, steps = 445390)
          )
        )

        assertEvalWithBudgets(
          (n: BigInt) => List.fill(n, -1),
          BigInt(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 2601, steps = 445390)
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
            compilerOptions -> ExUnits(memory = 3032, steps = 490033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.map2(List.empty[BigInt], list)(_ + _),
          Cons(3, Cons(4, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 2632, steps = 426033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => List.map2(list, Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(4), Cons(BigInt(6), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 11292, steps = 2_230747)
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
            compilerOptions -> ExUnits(memory = 1801, steps = 1_279364)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list === List.single(BigInt(1)),
          List.single(1),
          true,
          Seq(
            compilerOptions -> ExUnits(memory = 1801, steps = 1_415759)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Cons(1, Cons(2, Cons(3, Nil))),
          true,
          Seq(
            compilerOptions -> ExUnits(memory = 1801, steps = 1_688549)
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
            compilerOptions -> ExUnits(memory = 4632, steps = 746033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.quicksort,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 11552, steps = 2_386652)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.quicksort,
          Cons(3, Cons(1, Cons(2, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 43971, steps = 9_626315)
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
            compilerOptions -> ExUnits(memory = 3164, steps = 531966)
          )
        )

        assertEvalWithBudgets(
          (list: List[List[BigInt]]) => list.flatten,
          List.single(Cons(3, Cons(1, Cons(2, Nil)))),
          Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 8092, steps = 1_460117)
          )
        )

        assertEvalWithBudgets(
          (list: List[List[BigInt]]) => list.flatten,
          Cons[List[BigInt]](Cons(1, Cons(2, Nil)), List.single(List.single(3))),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 16584, steps = 3_092992)
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
            compilerOptions -> ExUnits(memory = 3233, steps = 567323)
          )
        )
    }

    test("get - single element, idx 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 4998, steps = 986169)
          )
        )
    }

    test("get - single element, idx 1 (out of bounds)") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(1),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 5436, steps = 1_072864)
          )
        )
    }

    test("get - single element, idx -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(-1),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 1933, steps = 359323)
          )
        )
    }

    test("get - two-element list, idx 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(0),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 4998, steps = 986169)
          )
        )
    }

    test("get - two-element list, idx 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 7201, steps = 1_491710)
          )
        )
    }

    test("get - two-element list, idx 2 (out of bounds)") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(2),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 7639, steps = 1_578405)
          )
        )
    }

    test("get - two-element list, idx -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.get(-1),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 1933, steps = 359323)
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
            compilerOptions -> ExUnits(memory = 1664, steps = 276143)
          )
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).at(-1))
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(0),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 2860, steps = 630470)
          )
        )
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 1664, steps = 276143)
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
            compilerOptions -> ExUnits(memory = 1664, steps = 276143)
          )
        )
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(1))
        assertEvalFails[NoSuchElementException](List.single(BigInt(1)).!!(-1))
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(0),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 2860, steps = 630470)
          )
        )
        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(1),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 1664, steps = 276143)
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
            compilerOptions -> ExUnits(memory = 8696, steps = 1_478902)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupBy(_ % 2),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          Seq(
            compilerOptions -> ExUnits(memory = 28961, steps = 6_252745)
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
          ExUnits(memory = 56284, steps = 12_621110)
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
            compilerOptions -> ExUnits(memory = 8896, steps = 1_510902)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMap(_ % 2)(identity),
          List.single(1),
          SortedMap.singleton(BigInt(1), List.single(BigInt(1))),
          Seq(
            compilerOptions -> ExUnits(memory = 30225, steps = 6_480788)
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
          ExUnits(memory = 58612, steps = 13_045196)
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
            compilerOptions -> ExUnits(memory = 5432, steps = 874033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          List.single(1),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 16745, steps = 3_567030)
          )
        )

        assertEvalWithBudget(
          (list: List[BigInt]) => list.groupMapReduce(_ % 2)(identity)(_ + _),
          Cons(1, Cons(2, Cons(3, Cons(4, Nil)))),
          SortedMap.unsafeFromList(
            Cons((BigInt(0), BigInt(6)), Cons((BigInt(1), BigInt(4)), Nil))
          ),
          ExUnits(memory = 92370, steps = 21_532235)
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
            compilerOptions -> ExUnits(memory = 2832, steps = 458033)
          )
        )
    }

    test("zip - single zip empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.empty[BigInt]),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions -> ExUnits(memory = 3232, steps = 522033)
          )
        )
    }

    test("zip - empty zip single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt].zip(list),
          List.single(1),
          List.empty[(BigInt, BigInt)],
          Seq(
            compilerOptions -> ExUnits(memory = 2832, steps = 458033)
          )
        )
    }

    test("zip - single zip single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.single(BigInt(2))),
          List.single(1),
          List.single((BigInt(1), BigInt(2))),
          Seq(
            compilerOptions -> ExUnits(memory = 6160, steps = 1_209270)
          )
        )
    }

    test("zip - two zip two") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(Cons(BigInt(3), Cons(BigInt(4), Nil))),
          Cons(1, Cons(2, Nil)),
          Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9488, steps = 1_960507)
          )
        )
    }

    test("zip - two zip single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.zip(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          List.single((BigInt(1), BigInt(3))),
          Seq(
            compilerOptions -> ExUnits(memory = 6560, steps = 1_273270)
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
            compilerOptions -> ExUnits(memory = 2232, steps = 362033)
          )
        )
    }

    test("prependedAll - empty prepend single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 2232, steps = 362033)
          )
        )
    }

    test("prependedAll - single prepend empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3164, steps = 531966)
          )
        )
    }

    test("prependedAll - single prepend single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          List.single(2),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 5160, steps = 968113)
          )
        )
    }

    test("prependedAll - two prepend single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.prependedAll(List.single(BigInt(1))),
          Cons(2, Cons(3, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 5160, steps = 968113)
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
            compilerOptions -> ExUnits(memory = 2232, steps = 362033)
          )
        )
    }

    test("++: - empty ++: single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++: list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3164, steps = 531966)
          )
        )
    }

    test("++: - single ++: empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 1832, steps = 298033)
          )
        )
    }

    test("++: - single ++: single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 4328, steps = 814180)
          )
        )
    }

    test("++: - two ++: single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++: List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 5860, steps = 1_126542)
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
            compilerOptions -> ExUnits(memory = 3264, steps = 594395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appended(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 4896, steps = 922757)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appended(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 6528, steps = 1_251119)
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
            compilerOptions -> ExUnits(memory = 3264, steps = 594395)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :+ BigInt(2),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 4896, steps = 922757)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list :+ BigInt(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 6528, steps = 1_251119)
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
            compilerOptions -> ExUnits(memory = 2964, steps = 548466)
          )
        )
    }

    test("appendedAll - empty append single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3464, steps = 628466)
          )
        )
    }

    test("appendedAll - single append empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 2964, steps = 548466)
          )
        )
    }

    test("appendedAll - single append single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 4996, steps = 940828)
          )
        )
    }

    test("appendedAll - two append single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.appendedAll(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 6528, steps = 1_253190)
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
            compilerOptions -> ExUnits(memory = 2964, steps = 548466)
          )
        )
    }

    test(":++ - empty :++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] :++ list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3164, steps = 580466)
          )
        )
    }

    test(":++ - single :++ empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 2964, steps = 548466)
          )
        )
    }

    test(":++ - single :++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 4996, steps = 940828)
          )
        )
    }

    test(":++ - two :++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list :++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 6528, steps = 1_253190)
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
            compilerOptions -> ExUnits(memory = 2964, steps = 548466)
          )
        )
    }

    test("concat - empty concat single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(1))),
          List.empty[BigInt],
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3464, steps = 628466)
          )
        )
    }

    test("concat - single concat empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 2964, steps = 548466)
          )
        )
    }

    test("concat - single concat single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(2))),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 4996, steps = 940828)
          )
        )
    }

    test("concat - two concat single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.concat(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 6528, steps = 1_253190)
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
            compilerOptions -> ExUnits(memory = 2964, steps = 548466)
          )
        )
    }

    test("++ - empty ++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => List.empty[BigInt] ++ list,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3164, steps = 580466)
          )
        )
    }

    test("++ - single ++ empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.empty[BigInt],
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 2964, steps = 548466)
          )
        )
    }

    test("++ - single ++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.single(BigInt(2)),
          List.single(1),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 4996, steps = 940828)
          )
        )
    }

    test("++ - two ++ single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list ++ List.single(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 6528, steps = 1_253190)
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
            compilerOptions -> ExUnits(memory = 3764, steps = 627966)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.map(_ + 1),
          List.single(1),
          List.single(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 7926, steps = 1_537364)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.map(_ + 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 12088, steps = 2_446762)
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
            compilerOptions -> ExUnits(memory = 4564, steps = 755966)
          )
        )
    }

    test("flatMap - single, single mapper") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          List.single(1),
          List.single(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 10758, steps = 2_059797)
          )
        )
    }

    test("flatMap - two, single mapper") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => List.single(x + 1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(3), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 18884, steps = 3_739990)
          )
        )
    }

    test("flatMap - two, empty mapper") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(_ => Nil),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 14820, steps = 2_665890)
          )
        )
    }

    test("flatMap - two, two-element mapper") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.flatMap(x => Cons(x + 10, Cons(x + 100, Nil))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(11), Cons(BigInt(101), Cons(BigInt(12), Cons(BigInt(102), Nil)))),
          Seq(
            compilerOptions -> ExUnits(memory = 23048, steps = 4_830090)
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
            compilerOptions -> ExUnits(memory = 3764, steps = 627966)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filter(_ > 1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 7661, steps = 1_361785)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filter(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 12090, steps = 2_247966)
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
            compilerOptions -> ExUnits(memory = 3764, steps = 627966)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 8393, steps = 1_546147)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterNot(_ > 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 12490, steps = 2_311966)
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
            compilerOptions -> ExUnits(memory = 4664, steps = 771966)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 10025, steps = 1_896268)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.filterMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(3), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 17880, steps = 3_897094)
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
            compilerOptions -> ExUnits(memory = 2432, steps = 394033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.find(_ > 1),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 4765, steps = 828067)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.find(_ > 1),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 7062, steps = 1_340614)
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
            compilerOptions -> ExUnits(memory = 2432, steps = 394033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 6629, steps = 1_282550)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.findMap(x => if x > 1 then Some(x + 1) else None),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(3)),
          Seq(
            compilerOptions -> ExUnits(memory = 11424, steps = 2_462087)
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
            compilerOptions -> ExUnits(memory = 3164, steps = 526777)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6562, steps = 1_212772)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldLeft(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          Seq(
            compilerOptions -> ExUnits(memory = 9960, steps = 1_898767)
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
            compilerOptions -> ExUnits(memory = 3164, steps = 526777)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6562, steps = 1_212772)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foldRight(BigInt(0))(_ + _),
          Cons(1, Cons(2, Nil)),
          BigInt(3),
          Seq(
            compilerOptions -> ExUnits(memory = 9960, steps = 1_898767)
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
            compilerOptions -> ExUnits(memory = 3764, steps = 622777)
          )
        )
    }

    test("count - single, _ > 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 0),
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 8063, steps = 1_498062)
          )
        )
    }

    test("count - single, _ > 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          List.single(1),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 7661, steps = 1_332854)
          )
        )
    }

    test("count - two, _ > 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 1),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 11960, steps = 2_208139)
          )
        )
    }

    test("count - two, _ > 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.count(_ > 2),
          Cons(1, Cons(2, Nil)),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 11558, steps = 2_042931)
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
            compilerOptions -> ExUnits(memory = 2832, steps = 458033)
          )
        )
    }

    test("indexOfOption - single, found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(1)),
          List.single(1),
          Some(BigInt(0)),
          Seq(
            compilerOptions -> ExUnits(memory = 4829, steps = 1_906388)
          )
        )
    }

    test("indexOfOption - single, not found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          List.single(1),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 5035, steps = 1_945784)
          )
        )
    }

    test("indexOfOption - two, found at 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7032, steps = 3_394139)
          )
        )
    }

    test("indexOfOption - two, not found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOfOption(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          None,
          Seq(
            compilerOptions -> ExUnits(memory = 7238, steps = 3_433535)
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
            compilerOptions -> ExUnits(memory = 4528, steps = 901260)
          )
        )
    }

    test("indexOf - single, found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(1)),
          List.single(1),
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 7289, steps = 2_686757)
          )
        )
    }

    test("indexOf - single, not found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          List.single(1),
          BigInt(-1),
          Seq(
            compilerOptions -> ExUnits(memory = 6731, steps = 2_389011)
          )
        )
    }

    test("indexOf - two, found at 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 9492, steps = 4_174508)
          )
        )
    }

    test("indexOf - two, not found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.indexOf(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          BigInt(-1),
          Seq(
            compilerOptions -> ExUnits(memory = 8934, steps = 3_876762)
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
            compilerOptions -> ExUnits(memory = 2132, steps = 346033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.lastOption,
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3828, steps = 770979)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.lastOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(2)),
          Seq(
            compilerOptions -> ExUnits(memory = 5360, steps = 1_085412)
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
            compilerOptions -> ExUnits(memory = 2128, steps = 691881)
          )
        )

        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).last,
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 2128, steps = 691881)
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
            compilerOptions -> ExUnits(memory = 932, steps = 154033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.headOption,
          List.single(1),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 2096, steps = 424546)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.headOption,
          Cons(1, Cons(2, Nil)),
          Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 2096, steps = 424546)
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
            compilerOptions -> ExUnits(memory = 4256, steps = 1_156915)
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
            compilerOptions -> ExUnits(memory = 3164, steps = 526777)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.length,
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6330, steps = 1_160028)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.length,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 9496, steps = 1_793279)
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
            compilerOptions -> ExUnits(memory = 3164, steps = 526777)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.size,
          List.single(1),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6330, steps = 1_160028)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.size,
          Cons(1, Cons(2, Nil)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 9496, steps = 1_793279)
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
            compilerOptions -> ExUnits(memory = 1132, steps = 186033)
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
            compilerOptions -> ExUnits(memory = 3033, steps = 533870)
          )
        )
    }

    test("drop - single drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4139, steps = 927789)
          )
        )
    }

    test("drop - two drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 4139, steps = 927789)
          )
        )
    }

    test("drop - two drop 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4139, steps = 929746)
          )
        )
    }

    test("drop - two drop 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 2833, steps = 501870)
          )
        )
    }

    test("drop - two drop -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.drop(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 2833, steps = 501870)
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
            compilerOptions -> ExUnits(memory = 8921, steps = 1_921559)
          )
        )
    }

    test("dropRight - single drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 15408, steps = 3_763925)
          )
        )
    }

    test("dropRight - two drop 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 22357, steps = 5_748337)
          )
        )
    }

    test("dropRight - two drop 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(2),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 21895, steps = 5_606291)
          )
        )
    }

    test("dropRight - two drop 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(0),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 5397, steps = 961655)
          )
        )
    }

    test("dropRight - two drop -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropRight(-1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 5397, steps = 961655)
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
            compilerOptions -> ExUnits(memory = 2432, steps = 394033)
          )
        )
    }

    test("dropWhile - single, _ < 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3765, steps = 668067)
          )
        )
    }

    test("dropWhile - two, _ < 3") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 7098, steps = 1_262101)
          )
        )
    }

    test("dropWhile - two, _ < 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 6098, steps = 1_102101)
          )
        )
    }

    test("dropWhile - two, _ < 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.dropWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 3765, steps = 668067)
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
            compilerOptions -> ExUnits(memory = 2632, steps = 426033)
          )
        )
    }

    test("deleteFirst - single, found") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3433, steps = 1_588576)
          )
        )
    }

    test("deleteFirst - two, head match") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 3433, steps = 1_588576)
          )
        )
    }

    test("deleteFirst - two, no match") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(3)),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 6898, steps = 3_311843)
          )
        )
    }

    test("deleteFirst - two, tail match") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(2)),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5566, steps = 3_031481)
          )
        )
    }

    test("deleteFirst - two duplicates, head only") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.deleteFirst(BigInt(1)),
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3433, steps = 1_588576)
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
            compilerOptions -> ExUnits(memory = 3033, steps = 533870)
          )
        )
    }

    test("take - single take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5568, steps = 1_151277)
          )
        )
    }

    test("take - two take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 5568, steps = 1_151277)
          )
        )
    }

    test("take - two take 3") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 8503, steps = 1_832684)
          )
        )
    }

    test("take - two take 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 2833, steps = 501870)
          )
        )
    }

    test("take - two take -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.take(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 2833, steps = 501870)
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
            compilerOptions -> ExUnits(memory = 6561, steps = 1_353533)
          )
        )
    }

    test("takeRight - single take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 14376, steps = 3_601743)
          )
        )
    }

    test("takeRight - two take 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(1),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 18869, steps = 4_707868)
          )
        )
    }

    test("takeRight - two take 3") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 22191, steps = 5_849953)
          )
        )
    }

    test("takeRight - two take 0") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(0),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4865, steps = 847803)
          )
        )
    }

    test("takeRight - two take -1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeRight(-1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4865, steps = 847803)
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
            compilerOptions -> ExUnits(memory = 2432, steps = 394033)
          )
        )
    }

    test("takeWhile - single, _ < 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          List.single(1),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3765, steps = 668067)
          )
        )
    }

    test("takeWhile - two, _ < 3") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 3),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 8162, steps = 1_566825)
          )
        )
    }

    test("takeWhile - two, _ < 2") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 2),
          Cons(1, Cons(2, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6630, steps = 1_254463)
          )
        )
    }

    test("takeWhile - two, _ < 1") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.takeWhile(_ < 1),
          Cons(1, Cons(2, Nil)),
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3765, steps = 668067)
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
            compilerOptions -> ExUnits(memory = 6296, steps = 1_053899)
          )
        )
    }

    test("distinct - single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 14752, steps = 2_796676)
          )
        )
    }

    test("distinct - two unique") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 25309, steps = 5_909996)
          )
        )
    }

    test("distinct - two duplicates") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(1, Nil)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 21545, steps = 5_208000)
          )
        )
    }

    test("distinct - three with one duplicate") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.distinct,
          Cons(1, Cons(2, Cons(1, Nil))),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 34203, steps = 9_691863)
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
            compilerOptions -> ExUnits(memory = 3664, steps = 660466)
          )
        )
    }

    test("diff - single diff empty") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.empty[BigInt]),
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 3864, steps = 692466)
          )
        )
    }

    test("diff - empty diff single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.empty[BigInt],
          List.empty[BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 3664, steps = 660466)
          )
        )
    }

    test("diff - single diff matching single") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(1))),
          List.single(1),
          List.empty[BigInt],
          Seq(
            // diff is intrinsified composing the eq-stripped `deleteFirst` → slightly cheaper than
            // the prelude (which threaded a dead `Eq`). See v3-eq-eliminating.md.
            compilerOptions -> ExUnits(memory = 7865, steps = 2_367009)
          )
        )
    }

    test("diff - two diff matching tail") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(2))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 10298, steps = 3_857914)
          )
        )
    }

    test("diff - two diff non-matching") {
        assertEvalWithBudgets(
          (list: List[BigInt]) => list.diff(List.single(BigInt(3))),
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(1), Cons(BigInt(2), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 11530, steps = 4_122276)
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
            compilerOptions -> ExUnits(memory = 11915, steps = 2_950129)
          )
        )

        assertEvalWithBudgets(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).init,
          Cons(BigInt(1), Nil),
          Seq(
            compilerOptions -> ExUnits(memory = 22557, steps = 5_828837)
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
            compilerOptions -> ExUnits(memory = 3164, steps = 531966)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          List.single(1),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 6460, steps = 1_176113)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Nil)),
          Cons(BigInt(2), Cons(BigInt(1), Nil)),
          Seq(
            compilerOptions -> ExUnits(memory = 9756, steps = 1_820260)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.reverse,
          Cons(1, Cons(2, Cons(3, Nil))),
          Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil))),
          Seq(
            compilerOptions -> ExUnits(memory = 13052, steps = 2_464407)
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
            compilerOptions -> ExUnits(memory = 2432, steps = 394033)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foreach(_ + 1),
          List.single(1),
          (),
          Seq(
            compilerOptions -> ExUnits(memory = 4764, steps = 782777)
          )
        )

        assertEvalWithBudgets(
          (list: List[BigInt]) => list.foreach(_ + 1),
          Cons(1, Cons(2, Nil)),
          (),
          Seq(
            compilerOptions -> ExUnits(memory = 7096, steps = 1_171521)
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
        // Compare budget: vanRossemPV at(1) should be cheaper than changPV at(1).
        // The PV11-compiled term uses the dropList builtin, so it must be evaluated on a PV11 VM
        // (which costs dropList via the van Rossem reference cost model) rather than the default
        // Plomin VM, where the PV11 builtin parameters are absent.
        val v11VM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
        val v11Compiled = PlutusV3.compile(Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1))
        val v11Budget = v11Compiled.program.term
            .evaluateDebug(using v11VM)
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
