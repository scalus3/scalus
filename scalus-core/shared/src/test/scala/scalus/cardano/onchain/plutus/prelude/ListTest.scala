package scalus.cardano.onchain.plutus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.OnchainError
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.cardano.onchain.plutus.prelude.Option.{None, Some}
import scalus.cardano.onchain.plutus.prelude.{asScalus, identity, Eq, List, Option, Ord, Order, SortedMap}
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.Options
import scalus.testing.budget.BudgetTable
import scalus.testing.kit.EvalTestKit
import scalus.uplc.PlutusV3
import scalus.uplc.eval.PlutusVM

class ListTest extends AnyFunSuite with EvalTestKit {
    override protected def budgetTable: BudgetTable = ListTestBudgets

    test("empty") {
        assertEvalEq(Nil)(List.empty[BigInt])
        assertEval(List.empty[BigInt].isEmpty)

        assert(scala.List.empty[BigInt].asScalus === List.empty[BigInt])
        assert(List.empty[BigInt].asScala == scala.List.empty[BigInt])
    }

    test("singleton") {
        check { (value: BigInt) =>
            val scalusResult = List.singleton(value)
            val scalaResult = scala.List(value)

            scalusResult === Cons(
              value,
              Nil
            ) && scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(Cons(BigInt(1), Nil))((n: BigInt) => List.singleton(n)).at(BigInt(1))
    }

    test("findUniqueOrFail") {
        assertEvalEq[BigInt](3):
            Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
                .findUniqueOrFail(_ > 2, "expected one")
        assertEvalEq[BigInt](1):
            Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
                .findUniqueOrFail(_ < 2, "expected one")
        assertEvalEq[BigInt](2):
            Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
                .findUniqueOrFail(_ === BigInt(2), "expected one")
        assertEvalFailsWithMessage[OnchainError]("expected one")(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
              .findUniqueOrFail(_ > 1, "expected one")
        )
        assertEvalFailsWithMessage[OnchainError]("expected one")(
          Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
              .findUniqueOrFail(_ > 3, "expected one")
        )
        assertEvalFailsWithMessage[OnchainError]("expected one")(
          List.empty[BigInt].findUniqueOrFail(_ > 0, "expected one")
        )
        // Exact budget pin, ten elements, unique match in the middle.
        assertEvalEq(BigInt(5)): (xs: List[BigInt]) =>
            xs.findUniqueOrFail(_ === BigInt(5), "expected one")
        .at(List.range(1, 10))
        assertEvalEq(BigInt(5)): (xs: List[BigInt]) =>
            xs.findUniqueOrElse(_ === BigInt(5), _ => BigInt(0))
        .at(List.range(1, 10))
        // One pass and no intermediate list: cheaper than filter(p).length === 1 on the same input.
        val fused = PlutusV3.compile { (xs: List[BigInt]) =>
            xs.findUniqueOrFail(_ === BigInt(5), "expected one")
        }
        val twoPass = PlutusV3.compile { (xs: List[BigInt]) =>
            val matches = xs.filter(_ === BigInt(5))
            if matches.length === BigInt(1) then matches.head
            else throw new OnchainError("expected one")
        }
        val input = List.range(1, 10).toData
        val fusedBudget = (fused.program $ input).evaluateDebug.budget
        val twoPassBudget = (twoPass.program $ input).evaluateDebug.budget
        info(s"findUniqueOrFail: $fusedBudget, filter+length+head: $twoPassBudget")
        assert(fusedBudget.steps < twoPassBudget.steps)
        assert(fusedBudget.memory < twoPassBudget.memory)
    }

    test("singleOrFail") {
        assertEvalEq[BigInt](7)(List.singleton(BigInt(7)).singleOrFail("expected one"))
        assertEvalEq(BigInt(7)): (xs: List[BigInt]) =>
            xs.singleOrFail("expected one")
        .at(List.singleton(BigInt(7)))
        assertEvalFailsWithMessage[OnchainError]("expected one")(
          List.empty[BigInt].singleOrFail("expected one")
        )
        assertEvalFailsWithMessage[OnchainError]("expected one")(
          Cons(BigInt(1), Cons(BigInt(2), Nil)).singleOrFail("expected one")
        )
    }

    test("apply") {
        check { (seq: scala.Seq[BigInt]) =>
            val scalusResult = List(seq*)
            val scalaResult = scala.List(seq*)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            List(BigInt(1), BigInt(2), BigInt(3))
    }

    test("apply list of pairs") {
        assertEvalEq(Cons((BigInt(1), BigInt(2)), Cons((BigInt(3), BigInt(4)), Nil))):
            List((BigInt(1), BigInt(2)), (BigInt(3), BigInt(4)))
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

        assertEvalEq(List.singleton(BigInt(0)))((n: BigInt) => List.range(n, 0)).at(BigInt(0))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))): (n: BigInt) =>
            List.range(n, 3)
        .at(BigInt(1))
        assertEvalEq(List.empty[BigInt])((n: BigInt) => List.range(n, -1)).at(BigInt(0))
    }

    test("rangeUntil") {
        forAll(bigIntRangeGen) { (start: BigInt, end: BigInt) =>
            val scalusResult = List.rangeUntil(start, end)
            val scalaResult = scala.List.range(start, end)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalEq(List.singleton(BigInt(0))): (n: BigInt) =>
            List.rangeUntil(n, 1)
        .at(BigInt(0))
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))): (n: BigInt) =>
            List.rangeUntil(n, 4)
        .at(BigInt(1))
        assertEvalEq(List.empty[BigInt])((n: BigInt) => List.rangeUntil(n, 0)).at(BigInt(0))
        assertEvalEq(List.empty[BigInt])((n: BigInt) => List.rangeUntil(n, -1)).at(BigInt(0))
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

        assertEvalEq(List.singleton(BigInt(1)))((n: BigInt) => List.fill(n, 1)).at(BigInt(1))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(1), Cons(BigInt(1), Nil)))): (n: BigInt) =>
            List.fill(n, 3)
        .at(BigInt(1))

        assertEvalEq(List.empty[BigInt])((n: BigInt) => List.fill(n, 0)).at(BigInt(1))

        assertEvalEq(List.empty[BigInt])((n: BigInt) => List.fill(n, -1)).at(BigInt(1))
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

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            List.map2(list, List.empty[BigInt])(_ + _)
        .at(Cons(1, Cons(2, Nil)))

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            List.map2(List.empty[BigInt], list)(_ + _)
        .at(Cons(3, Cons(4, Nil)))

        assertEvalEq(Cons(BigInt(4), Cons(BigInt(6), Nil))): (list: List[BigInt]) =>
            List.map2(list, Cons(BigInt(3), Cons(BigInt(4), Nil)))(_ + _)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("ToData <-> FromData") {
        check { (list: List[BigInt]) =>
            val data = list.toData
            val fromDataList = fromData[List[BigInt]](data)

            fromDataList === list
        }

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            fromData[List[BigInt]](list.toData)
        .at(List.empty[BigInt])

        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            fromData[List[BigInt]](list.toData)
        .at(List.singleton(1))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => fromData[List[BigInt]](list.toData)
        .at(Cons(1, Cons(2, Cons(3, Nil))))
    }

    test("Eq") {
        check { (list: List[BigInt]) => list === list }

        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 === list2
            val scalaResult = list1.asScala == list2.asScala

            scalusResult === scalaResult
        }

        assertEvalEq(true): (list: List[BigInt]) =>
            list === List.empty[BigInt]
        .at(List.empty[BigInt])

        assertEvalEq(true): (list: List[BigInt]) =>
            list === List.singleton(BigInt(1))
        .at(List.singleton(1))

        assertEvalEq(true): (list: List[BigInt]) =>
            list === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
        .at(Cons(1, Cons(2, Cons(3, Nil))))

        assertEvalNotEq(
          List.empty[BigInt],
          List.singleton(BigInt(1))
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

        assertEval((List.singleton(BigInt(1)) <=> List.singleton(BigInt(1))).isEqual)

        assertEval(
          (
            Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil))) <=>
                Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          ).isEqual
        )

        assertEval((List.empty[BigInt] <=> List.singleton(BigInt(1))).isLess)

        assertEval((List.singleton(BigInt(1)) <=> List.singleton(BigInt(2))).isLess)

        assertEval(
          (
            Cons(BigInt(1), Cons(BigInt(2), Nil)) <=>
                Cons(BigInt(1), Cons(BigInt(3), Nil))
          ).isLess
        )

        assertEval((List.singleton(BigInt(1)) <=> List.empty[BigInt]).isGreater)

        assertEval((List.singleton(BigInt(2)) <=> List.singleton(BigInt(1))).isGreater)

        assertEval(
          (
            Cons(BigInt(1), Cons(BigInt(3), Nil)) <=>
                Cons(BigInt(1), Cons(BigInt(2), Nil))
          ).isGreater
        )
    }

    test("insertionSort agrees with sort and with Scala's sorted") {
        check { (list: List[BigInt]) =>
            val byInsertion = list.insertionSort
            val byMerge = list.sort
            byInsertion === byMerge && byInsertion.asScala == list.asScala.sorted
        }
    }

    test("sortWith agrees with sort when given the matching comparator") {
        check { (list: List[BigInt]) =>
            list.sortWith((a, b) => a < b) === list.sort
        }
    }

    /** Stability is not decorative: `SortedMap.fromList` resolves a duplicate key to its FIRST
      * occurrence, which is only well defined on top of a stable sort. Pairs are compared on the
      * first component only, so a reordering of equal keys is visible in the second.
      */
    test("sort, sortWith and insertionSort are all stable") {
        given Ord[(BigInt, BigInt)] = (a, b) => a._1 <=> b._1
        check { (keys: List[BigInt]) =>
            val tagged = List.from(keys.asScala.zipWithIndex.map((k, i) => (k, BigInt(i))))
            val expected =
                List.from(keys.asScala.zipWithIndex.map((k, i) => (k, BigInt(i))).sortBy(_._1))
            tagged.sort === expected
            && tagged.insertionSort === expected
            && tagged.sortWith((a, b) => a._1 < b._1) === expected
        }
    }

    test("isSorted and isStrictlyAscending") {
        check { (list: List[BigInt]) =>
            val xs = list.asScala
            val sortedOk = list.isSorted == xs.zip(xs.drop(1)).forall((a, b) => a <= b)
            val strictOk = list.isStrictlyAscending == xs.zip(xs.drop(1)).forall((a, b) => a < b)
            sortedOk && strictOk
        }
        assertEval(List.empty[BigInt].isSorted)
        assertEval(List.empty[BigInt].isStrictlyAscending)
        assertEval(List.singleton(BigInt(1)).isSorted)
        assertEval(Cons(BigInt(1), Cons(BigInt(1), Nil)).isSorted)
        assertEval(!Cons(BigInt(1), Cons(BigInt(1), Nil)).isStrictlyAscending)
        assertEval(!Cons(BigInt(2), Cons(BigInt(1), Nil)).isSorted)
    }

    /** The `check` blocks above run on the JVM and prove the Scala semantics. These compile to UPLC
      * and evaluate on the Plutus VM, which is the part that can fail independently — a method can
      * be correct in Scala and still fail to lower.
      */
    test("insertionSort, sortWith, isSorted and isStrictlyAscending lower and run on-chain") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))).insertionSort
        assertEvalEq(List.empty[BigInt])(List.empty[BigInt].insertionSort)
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil))).sortWith((a, b) => a < b)
        assertEvalEq(List.empty[BigInt])(List.empty[BigInt].sortWith((a, b) => a < b))
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).isSorted)
        assertEval(!Cons(BigInt(2), Cons(BigInt(1), Nil)).isSorted)
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).isStrictlyAscending)
        assertEval(!Cons(BigInt(1), Cons(BigInt(1), Nil)).isStrictlyAscending)
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).isSortedWith((a, b) => a < b))
        assertEval(!Cons(BigInt(2), Cons(BigInt(1), Nil)).isSortedWith((a, b) => a < b))
    }

    test("sorting anything already sorted is a no-op, and isSorted confirms it") {
        check { (list: List[BigInt]) =>
            val s = list.sort
            s.isSorted && s.sort === s && s.insertionSort === s
        }
    }

    test("quicksort") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.quicksort
            val scalaResult = list.asScala.sorted

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.quicksort
        .at(List.empty[BigInt])

        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.quicksort
        .at(List.singleton(1))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list.quicksort
        .at(Cons(3, Cons(1, Cons(2, Nil))))
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

        assertEvalEq(List.empty[BigInt]): (list: List[List[BigInt]]) =>
            list.flatten
        .at(List.empty[List[BigInt]])

        assertEvalEq(Cons(BigInt(3), Cons(BigInt(1), Cons(BigInt(2), Nil)))):
            (list: List[List[BigInt]]) => list.flatten
        .at(List.singleton(Cons(3, Cons(1, Cons(2, Nil)))))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[List[BigInt]]) => list.flatten
        .at(
          Cons[List[BigInt]](Cons(1, Cons(2, Nil)), List.singleton(List.singleton(3))): List[
            List[BigInt]
          ]
        )
    }

    test("isEmpty") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.isEmpty
            val scalaResult = list.asScala.isEmpty

            scalusResult === scalaResult
        }

        assertEval(List.empty[BigInt].isEmpty)
        assertEval(!List.singleton(BigInt(1)).isEmpty)
        assertEval(!Cons(BigInt(1), Cons(BigInt(2), Nil)).isEmpty)
    }

    test("nonEmpty") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.nonEmpty
            val scalaResult = list.asScala.nonEmpty

            scalusResult === scalaResult
        }

        assertEval(!List.empty[BigInt].nonEmpty)
        assertEval(List.singleton(BigInt(1)).nonEmpty)
        assertEval(Cons(BigInt(1), Cons(BigInt(2), Nil)).nonEmpty)
    }

    test("isDefinedAt") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = list.isDefinedAt(index)
            val scalaResult = list.asScala.isDefinedAt(index.toInt)

            assert(scalusResult === scalaResult)
        }

        assertEval(!List.empty[BigInt].isDefinedAt(0))
        assertEval(List.singleton(BigInt(1)).isDefinedAt(0))
        assertEval(!List.singleton(BigInt(1)).isDefinedAt(1))
        assertEval(!List.singleton(BigInt(1)).isDefinedAt(-1))
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
        assertEvalEq(None)((list: List[BigInt]) => list.get(0)).at(List.empty[BigInt])
    }

    test("get - single element, idx 0") {
        assertEvalEq(Some(BigInt(1))): (list: List[BigInt]) =>
            list.get(0)
        .at(List.singleton(1))
    }

    test("get - single element, idx 1 (out of bounds)") {
        assertEvalEq(None): (list: List[BigInt]) =>
            list.get(1)
        .at(List.singleton(1))
    }

    test("get - single element, idx -1") {
        assertEvalEq(None): (list: List[BigInt]) =>
            list.get(-1)
        .at(List.singleton(1))
    }

    test("get - two-element list, idx 0") {
        assertEvalEq(Some(BigInt(1))): (list: List[BigInt]) =>
            list.get(0)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("get - two-element list, idx 1") {
        assertEvalEq(Some(BigInt(2))): (list: List[BigInt]) =>
            list.get(1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("get - two-element list, idx 2 (out of bounds)") {
        assertEvalEq(None): (list: List[BigInt]) =>
            list.get(2)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("get - two-element list, idx -1") {
        assertEvalEq(None): (list: List[BigInt]) =>
            list.get(-1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("at") {
        forAll(bigIntListAndIndexGen) { (list: List[BigInt], index: BigInt) =>
            val scalusResult = liftThrowableToOption(list.at(index))
            val scalaResult = list.asScala.lift(index.toInt)

            assert(scalaResult.asScalus === scalusResult)
            assert(scalusResult.asScala == scalaResult)
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].at(0))
        assertEvalEq[BigInt](1)(List.singleton(BigInt(1)).at(0))
        assertEvalFails[NoSuchElementException](List.singleton(BigInt(1)).at(1))
        assertEvalFails[NoSuchElementException](List.singleton(BigInt(1)).at(-1))
        assertEvalEq[BigInt](1)(Cons(BigInt(1), Cons(BigInt(2), Nil)).at(0))
        assertEvalEq[BigInt](2)(Cons(BigInt(1), Cons(BigInt(2), Nil)).at(1))
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
        assertEvalEq[BigInt](1)(List.singleton(BigInt(1)).!!(0))
        assertEvalFails[NoSuchElementException](List.singleton(BigInt(1)).!!(1))
        assertEvalFails[NoSuchElementException](List.singleton(BigInt(1)).!!(-1))
        assertEvalEq[BigInt](1)(Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(0))
        assertEvalEq[BigInt](2)(Cons(BigInt(1), Cons(BigInt(2), Nil)).!!(1))
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
        assertEval(List.singleton(BigInt(1)).contains(BigInt(1)))
        assertEval(!List.singleton(BigInt(1)).contains(BigInt(2)))
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

        assertEvalEq(SortedMap.empty[BigInt, List[BigInt]]): (list: List[BigInt]) =>
            list.groupBy(_ % 2)
        .at(List.empty[BigInt])

        assertEvalEq(SortedMap.singleton(BigInt(1), List.singleton(BigInt(1)))):
            (list: List[BigInt]) => list.groupBy(_ % 2)
        .at(List.singleton(1))

        assertEvalEq(
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.singleton(BigInt(2))),
              Cons((BigInt(1), List.singleton(BigInt(1))), Nil)
            )
          )
        ): (list: List[BigInt]) =>
            list.groupBy(_ % 2)
        .at(Cons(1, Cons(2, Nil)))

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

        assertEvalEq(SortedMap.empty[BigInt, List[BigInt]]): (list: List[BigInt]) =>
            list.groupMap(_ % 2)(identity)
        .at(List.empty[BigInt])

        assertEvalEq(SortedMap.singleton(BigInt(1), List.singleton(BigInt(1)))):
            (list: List[BigInt]) => list.groupMap(_ % 2)(identity)
        .at(List.singleton(1))

        assertEvalEq(
          SortedMap.unsafeFromList(
            Cons(
              (BigInt(0), List.singleton(BigInt(2))),
              Cons((BigInt(1), List.singleton(BigInt(1))), Nil)
            )
          )
        ): (list: List[BigInt]) =>
            list.groupMap(_ % 2)(identity)
        .at(Cons(1, Cons(2, Nil)))
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

        assertEvalEq(SortedMap.empty[BigInt, BigInt]): (list: List[BigInt]) =>
            list.groupMapReduce(_ % 2)(identity)(_ + _)
        .at(List.empty[BigInt])

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1))): (list: List[BigInt]) =>
            list.groupMapReduce(_ % 2)(identity)(_ + _)
        .at(List.singleton(1))

        assertEvalEq(
          SortedMap.unsafeFromList(
            Cons((BigInt(0), BigInt(6)), Cons((BigInt(1), BigInt(4)), Nil))
          )
        ): (list: List[BigInt]) =>
            list.groupMapReduce(_ % 2)(identity)(_ + _)
        .at(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
    }

    test("zip - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.zip(list2)
            val scalaResult = list1.asScala.zip(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("zip - empty zip empty") {
        assertEvalEq(List.empty[(BigInt, BigInt)]): (list: List[BigInt]) =>
            list.zip(List.empty[BigInt])
        .at(List.empty[BigInt])
    }

    test("zip - single zip empty") {
        assertEvalEq(List.empty[(BigInt, BigInt)]): (list: List[BigInt]) =>
            list.zip(List.empty[BigInt])
        .at(List.singleton(1))
    }

    test("zip - empty zip single") {
        assertEvalEq(List.empty[(BigInt, BigInt)]): (list: List[BigInt]) =>
            List.empty[BigInt].zip(list)
        .at(List.singleton(1))
    }

    test("zip - single zip single") {
        assertEvalEq(List.singleton((BigInt(1), BigInt(2)))): (list: List[BigInt]) =>
            list.zip(List.singleton(BigInt(2)))
        .at(List.singleton(1))
    }

    test("zip - two zip two") {
        assertEvalEq(Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil))):
            (list: List[BigInt]) => list.zip(Cons(BigInt(3), Cons(BigInt(4), Nil)))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("zip - two zip single") {
        assertEvalEq(List.singleton((BigInt(1), BigInt(3)))): (list: List[BigInt]) =>
            list.zip(List.singleton(BigInt(3)))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("prepended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.prepended(value)
            val scalaResult = list.asScala.prepended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.prepended(BigInt(1))
        .at(List.empty[BigInt])

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.prepended(BigInt(1))
        .at(List.singleton(2))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list.prepended(BigInt(1))
        .at(Cons(2, Cons(3, Nil)))
    }

    test("+:") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = value +: list
            val scalaResult = value +: list.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            BigInt(1) +: list
        .at(List.empty[BigInt])

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            BigInt(1) +: list
        .at(List.singleton(2))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => BigInt(1) +: list
        .at(Cons(2, Cons(3, Nil)))
    }

    test("prependedAll - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.prependedAll(list2)
            val scalaResult = list1.asScala.prependedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("prependedAll - empty prepend empty") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.prependedAll(List.empty[BigInt])
        .at(List.empty[BigInt])
    }

    test("prependedAll - empty prepend single") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.prependedAll(List.singleton(BigInt(1)))
        .at(List.empty[BigInt])
    }

    test("prependedAll - single prepend empty") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.prependedAll(List.empty[BigInt])
        .at(List.singleton(1))
    }

    test("prependedAll - single prepend single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.prependedAll(List.singleton(BigInt(1)))
        .at(List.singleton(2))
    }

    test("prependedAll - two prepend single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list.prependedAll(List.singleton(BigInt(1)))
        .at(Cons(2, Cons(3, Nil)))
    }

    test("++: - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++: list2
            val scalaResult = list1.asScala ++: list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("++: - empty ++: empty") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            List.empty[BigInt] ++: list
        .at(List.empty[BigInt])
    }

    test("++: - empty ++: single") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            List.empty[BigInt] ++: list
        .at(List.singleton(1))
    }

    test("++: - single ++: empty") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list ++: List.empty[BigInt]
        .at(List.singleton(1))
    }

    test("++: - single ++: single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list ++: List.singleton(BigInt(2))
        .at(List.singleton(1))
    }

    test("++: - two ++: single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list ++: List.singleton(BigInt(3))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("appended") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.appended(value)
            val scalaResult = list.asScala.appended(value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.appended(BigInt(1))
        .at(List.empty[BigInt])

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.appended(BigInt(2))
        .at(List.singleton(1))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list.appended(BigInt(3))
        .at(Cons(1, Cons(2, Nil)))
    }

    test(":+") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list :+ value
            val scalaResult = list.asScala :+ value

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list :+ BigInt(1)
        .at(List.empty[BigInt])

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list :+ BigInt(2)
        .at(List.singleton(1))

        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list :+ BigInt(3)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("appendedAll - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.appendedAll(list2)
            val scalaResult = list1.asScala.appendedAll(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("appendedAll - empty append empty") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.appendedAll(List.empty[BigInt])
        .at(List.empty[BigInt])
    }

    test("appendedAll - empty append single") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.appendedAll(List.singleton(BigInt(1)))
        .at(List.empty[BigInt])
    }

    test("appendedAll - single append empty") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.appendedAll(List.empty[BigInt])
        .at(List.singleton(1))
    }

    test("appendedAll - single append single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.appendedAll(List.singleton(BigInt(2)))
        .at(List.singleton(1))
    }

    test("appendedAll - two append single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list.appendedAll(List.singleton(BigInt(3)))
        .at(Cons(1, Cons(2, Nil)))
    }

    test(":++ - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 :++ list2
            val scalaResult = list1.asScala :++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test(":++ - empty :++ empty") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list :++ List.empty[BigInt]
        .at(List.empty[BigInt])
    }

    test(":++ - empty :++ single") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            List.empty[BigInt] :++ list
        .at(List.singleton(1))
    }

    test(":++ - single :++ empty") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list :++ List.empty[BigInt]
        .at(List.singleton(1))
    }

    test(":++ - single :++ single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list :++ List.singleton(BigInt(2))
        .at(List.singleton(1))
    }

    test(":++ - two :++ single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list :++ List.singleton(BigInt(3))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("concat - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.concat(list2)
            val scalaResult = list1.asScala.concat(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("concat - empty concat empty") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.concat(List.empty[BigInt])
        .at(List.empty[BigInt])
    }

    test("concat - empty concat single") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.concat(List.singleton(BigInt(1)))
        .at(List.empty[BigInt])
    }

    test("concat - single concat empty") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.concat(List.empty[BigInt])
        .at(List.singleton(1))
    }

    test("concat - single concat single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.concat(List.singleton(BigInt(2)))
        .at(List.singleton(1))
    }

    test("concat - two concat single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list.concat(List.singleton(BigInt(3)))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("++ - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1 ++ list2
            val scalaResult = list1.asScala ++ list2.asScala

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("++ - empty ++ empty") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list ++ List.empty[BigInt]
        .at(List.empty[BigInt])
    }

    test("++ - empty ++ single") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            List.empty[BigInt] ++ list
        .at(List.singleton(1))
    }

    test("++ - single ++ empty") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list ++ List.empty[BigInt]
        .at(List.singleton(1))
    }

    test("++ - single ++ single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list ++ List.singleton(BigInt(2))
        .at(List.singleton(1))
    }

    test("++ - two ++ single") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))):
            (list: List[BigInt]) => list ++ List.singleton(BigInt(3))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("map") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.map(_ + value)
            val scalaResult = list.asScala.map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.map(_ + 1)
        .at(List.empty[BigInt])

        assertEvalEq(List.singleton(BigInt(2))): (list: List[BigInt]) =>
            list.map(_ + 1)
        .at(List.singleton(1))

        assertEvalEq(Cons(BigInt(2), Cons(BigInt(3), Nil))): (list: List[BigInt]) =>
            list.map(_ + 1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("flatMap - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.flatMap(x => List.singleton(x + value))
            val scalaResult = list.asScala.flatMap(x => scala.List(x + value))

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("flatMap - empty list, single mapper") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.flatMap(x => List.singleton(x + 1))
        .at(List.empty[BigInt])
    }

    test("flatMap - single, single mapper") {
        assertEvalEq(List.singleton(BigInt(2))): (list: List[BigInt]) =>
            list.flatMap(x => List.singleton(x + 1))
        .at(List.singleton(1))
    }

    test("flatMap - two, single mapper") {
        assertEvalEq(Cons(BigInt(2), Cons(BigInt(3), Nil))): (list: List[BigInt]) =>
            list.flatMap(x => List.singleton(x + 1))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("flatMap - two, empty mapper") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.flatMap(_ => Nil)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("flatMap - two, two-element mapper") {
        assertEvalEq(Cons(BigInt(11), Cons(BigInt(101), Cons(BigInt(12), Cons(BigInt(102), Nil))))):
            (list: List[BigInt]) => list.flatMap(x => Cons(x + 10, Cons(x + 100, Nil)))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("filter") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filter(_ > value)
            val scalaResult = list.asScala.filter(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.filter(_ > 1)
        .at(List.empty[BigInt])

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.filter(_ > 1)
        .at(List.singleton(1))

        assertEvalEq(Cons(BigInt(2), Nil)): (list: List[BigInt]) =>
            list.filter(_ > 1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("filterNot") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterNot(_ > value)
            val scalaResult = list.asScala.filterNot(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.filterNot(_ > 1)
        .at(List.empty[BigInt])

        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.filterNot(_ > 1)
        .at(List.singleton(1))

        assertEvalEq(Cons(BigInt(1), Nil)): (list: List[BigInt]) =>
            list.filterNot(_ > 1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("filterMap") {

        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.filterMap(x => if x > value then Some(x + value) else None)
            val scalaResult =
                list.asScala.flatMap(x => if x > value then scala.Some(x + value) else scala.None)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.filterMap(x => if x > 1 then Some(x + 1) else None)
        .at(List.empty[BigInt])

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.filterMap(x => if x > 1 then Some(x + 1) else None)
        .at(List.singleton(1))

        assertEvalEq(Cons(BigInt(3), Nil)): (list: List[BigInt]) =>
            list.filterMap(x => if x > 1 then Some(x + 1) else None)
        .at(Cons(1, Cons(2, Nil)))

    }

    test("find") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.find(_ > value)
            val scalaResult = list.asScala.find(_ > value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(None)((list: List[BigInt]) => list.find(_ > 1)).at(List.empty[BigInt])

        assertEvalEq(None)((list: List[BigInt]) => list.find(_ > 1)).at(List.singleton(1))

        assertEvalEq(Some(BigInt(2))): (list: List[BigInt]) =>
            list.find(_ > 1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("findMap") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.findMap(x => if x > value then Some(x + value) else None)
            val scalaResult = list.asScala.find(_ > value).map(_ + value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(None): (list: List[BigInt]) =>
            list.findMap(x => if x > 1 then Some(x + 1) else None)
        .at(List.empty[BigInt])

        assertEvalEq(None): (list: List[BigInt]) =>
            list.findMap(x => if x > 1 then Some(x + 1) else None)
        .at(List.singleton(1))

        assertEvalEq(Some(BigInt(3))): (list: List[BigInt]) =>
            list.findMap(x => if x > 1 then Some(x + 1) else None)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("foldLeft") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldLeft(initial)(_ + _)
            val scalaResult = list.asScala.foldLeft(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalEq(BigInt(0)): (list: List[BigInt]) =>
            list.foldLeft(BigInt(0))(_ + _)
        .at(List.empty[BigInt])

        assertEvalEq(BigInt(1)): (list: List[BigInt]) =>
            list.foldLeft(BigInt(0))(_ + _)
        .at(List.singleton(1))

        assertEvalEq(BigInt(3)): (list: List[BigInt]) =>
            list.foldLeft(BigInt(0))(_ + _)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("foldRight") {
        check { (list: List[BigInt], initial: BigInt, value: BigInt) =>
            val scalusResult = list.foldRight(initial)(_ + _)
            val scalaResult = list.asScala.foldRight(initial)(_ + _)

            scalusResult === scalaResult
        }

        assertEvalEq(BigInt(0)): (list: List[BigInt]) =>
            list.foldRight(BigInt(0))(_ + _)
        .at(List.empty[BigInt])

        assertEvalEq(BigInt(1)): (list: List[BigInt]) =>
            list.foldRight(BigInt(0))(_ + _)
        .at(List.singleton(1))

        assertEvalEq(BigInt(3)): (list: List[BigInt]) =>
            list.foldRight(BigInt(0))(_ + _)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("exists") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.exists(_ > value)
            val scalaResult = list.asScala.exists(_ > value)

            scalusResult === scalaResult
        }

        assertEval(!List.empty[BigInt].exists(_ > 1))
        assertEval(List.singleton(BigInt(1)).exists(_ > 0))
        assertEval(!List.singleton(BigInt(1)).exists(_ > 1))
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
        assertEval(List.singleton(BigInt(1)).forall(_ > 0))
        assertEval(!List.singleton(BigInt(1)).forall(_ > 1))
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
        assertEvalEq(BigInt(0))((list: List[BigInt]) => list.count(_ > 1)).at(List.empty[BigInt])
    }

    test("count - single, _ > 0") {
        assertEvalEq(BigInt(1))((list: List[BigInt]) => list.count(_ > 0)).at(List.singleton(1))
    }

    test("count - single, _ > 1") {
        assertEvalEq(BigInt(0))((list: List[BigInt]) => list.count(_ > 1)).at(List.singleton(1))
    }

    test("count - two, _ > 1") {
        assertEvalEq(BigInt(1))((list: List[BigInt]) => list.count(_ > 1)).at(Cons(1, Cons(2, Nil)))
    }

    test("count - two, _ > 2") {
        assertEvalEq(BigInt(0))((list: List[BigInt]) => list.count(_ > 2)).at(Cons(1, Cons(2, Nil)))
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
        assertEvalEq(None): (list: List[BigInt]) =>
            list.indexOfOption(BigInt(1))
        .at(List.empty[BigInt])
    }

    test("indexOfOption - single, found") {
        assertEvalEq(Some(BigInt(0))): (list: List[BigInt]) =>
            list.indexOfOption(BigInt(1))
        .at(List.singleton(1))
    }

    test("indexOfOption - single, not found") {
        assertEvalEq(None): (list: List[BigInt]) =>
            list.indexOfOption(BigInt(2))
        .at(List.singleton(1))
    }

    test("indexOfOption - two, found at 1") {
        assertEvalEq(Some(BigInt(1))): (list: List[BigInt]) =>
            list.indexOfOption(BigInt(2))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("indexOfOption - two, not found") {
        assertEvalEq(None): (list: List[BigInt]) =>
            list.indexOfOption(BigInt(3))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("indexOf - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.indexOf(value)
            val scalaResult = BigInt(list.asScala.indexOf(value))

            scalusResult === scalaResult
        }
    }

    test("indexOf - empty list") {
        assertEvalEq(BigInt(-1)): (list: List[BigInt]) =>
            list.indexOf(BigInt(1))
        .at(List.empty[BigInt])
    }

    test("indexOf - single, found") {
        assertEvalEq(BigInt(0)): (list: List[BigInt]) =>
            list.indexOf(BigInt(1))
        .at(List.singleton(1))
    }

    test("indexOf - single, not found") {
        assertEvalEq(BigInt(-1)): (list: List[BigInt]) =>
            list.indexOf(BigInt(2))
        .at(List.singleton(1))
    }

    test("indexOf - two, found at 1") {
        assertEvalEq(BigInt(1)): (list: List[BigInt]) =>
            list.indexOf(BigInt(2))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("indexOf - two, not found") {
        assertEvalEq(BigInt(-1)): (list: List[BigInt]) =>
            list.indexOf(BigInt(3))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("lastOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.lastOption
            val scalaResult = list.asScala.lastOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(None)((list: List[BigInt]) => list.lastOption).at(List.empty[BigInt])

        assertEvalEq(Some(BigInt(1)))((list: List[BigInt]) => list.lastOption).at(List.singleton(1))

        assertEvalEq(Some(BigInt(2))): (list: List[BigInt]) =>
            list.lastOption
        .at(Cons(1, Cons(2, Nil)))
    }

    test("last") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.last)
            val scalaResult = liftThrowableToOption(list.asScala.last)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].last)

        assertEvalEq(BigInt(1))((l: List[BigInt]) => l.last).at(List.singleton(BigInt(1)))

        assertEvalEq(BigInt(2))((l: List[BigInt]) => l.last)
            .at(Cons(BigInt(1), Cons(BigInt(2), Nil)))
    }

    test("headOption") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.headOption
            val scalaResult = list.asScala.headOption

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(None)((list: List[BigInt]) => list.headOption).at(List.empty[BigInt])

        assertEvalEq(Some(BigInt(1)))((list: List[BigInt]) => list.headOption).at(List.singleton(1))

        assertEvalEq(Some(BigInt(1))): (list: List[BigInt]) =>
            list.headOption
        .at(Cons(1, Cons(2, Nil)))
    }

    test("head") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.head)
            val scalaResult = liftThrowableToOption(list.asScala.head)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].head)

        assertEvalEq(BigInt(1))(List.singleton(BigInt(1)).head)

        assertEvalEq(BigInt(1))((list: List[BigInt]) => list.head).at(Cons(1, Cons(2, Nil)))
    }

    test("length") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.length
            val scalaResult = BigInt(list.asScala.length)

            scalusResult === scalaResult
        }

        assertEvalEq(BigInt(0))((list: List[BigInt]) => list.length).at(List.empty[BigInt])

        assertEvalEq(BigInt(1))((list: List[BigInt]) => list.length).at(List.singleton(1))

        assertEvalEq(BigInt(2))((list: List[BigInt]) => list.length).at(Cons(1, Cons(2, Nil)))
    }

    test("size") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.size
            val scalaResult = BigInt(list.asScala.size)

            scalusResult === scalaResult
        }

        assertEvalEq(BigInt(0))((list: List[BigInt]) => list.size).at(List.empty[BigInt])

        assertEvalEq(BigInt(1))((list: List[BigInt]) => list.size).at(List.singleton(1))

        assertEvalEq(BigInt(2))((list: List[BigInt]) => list.size).at(Cons(1, Cons(2, Nil)))
    }

    test("tail") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.tail)
            val scalaResult = liftThrowableToOption(list.asScala.tail.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].tail)
        assertEvalEq(List.empty[BigInt])(List.singleton(BigInt(1)).tail)
        assertEvalEq(Cons(BigInt(2), Nil)): (list: List[BigInt]) =>
            list.tail
        .at(Cons(1, Cons(2, Nil)))
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
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.drop(1)
        .at(List.empty[BigInt])
    }

    test("drop - single drop 1") {
        assertEvalEq(List.empty[BigInt])((list: List[BigInt]) => list.drop(1)).at(List.singleton(1))
    }

    test("drop - two drop 1") {
        assertEvalEq(Cons(BigInt(2), Nil)): (list: List[BigInt]) =>
            list.drop(1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("drop - two drop 2") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.drop(2)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("drop - two drop 0") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.drop(0)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("drop - two drop -1") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.drop(-1)
        .at(Cons(1, Cons(2, Nil)))
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
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.dropRight(1)
        .at(List.empty[BigInt])
    }

    test("dropRight - single drop 1") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.dropRight(1)
        .at(List.singleton(1))
    }

    test("dropRight - two drop 1") {
        assertEvalEq(Cons(BigInt(1), Nil)): (list: List[BigInt]) =>
            list.dropRight(1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("dropRight - two drop 2") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.dropRight(2)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("dropRight - two drop 0") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.dropRight(0)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("dropRight - two drop -1") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.dropRight(-1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("dropWhile - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.dropWhile(_ < value)
            val scalaResult = list.asScala.dropWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("dropWhile - empty list") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.dropWhile(_ < 1)
        .at(List.empty[BigInt])
    }

    test("dropWhile - single, _ < 1") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.dropWhile(_ < 1)
        .at(List.singleton(1))
    }

    test("dropWhile - two, _ < 3") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.dropWhile(_ < 3)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("dropWhile - two, _ < 2") {
        assertEvalEq(Cons(BigInt(2), Nil)): (list: List[BigInt]) =>
            list.dropWhile(_ < 2)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("dropWhile - two, _ < 1") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.dropWhile(_ < 1)
        .at(Cons(1, Cons(2, Nil)))
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
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.deleteFirst(BigInt(1))
        .at(List.empty[BigInt])
    }

    test("deleteFirst - single, found") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.deleteFirst(BigInt(1))
        .at(List.singleton(1))
    }

    test("deleteFirst - two, head match") {
        assertEvalEq(Cons(BigInt(2), Nil)): (list: List[BigInt]) =>
            list.deleteFirst(BigInt(1))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("deleteFirst - two, no match") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.deleteFirst(BigInt(3))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("deleteFirst - two, tail match") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.deleteFirst(BigInt(2))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("deleteFirst - two duplicates, head only") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.deleteFirst(BigInt(1))
        .at(Cons(1, Cons(1, Nil)))
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
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.take(1)
        .at(List.empty[BigInt])
    }

    test("take - single take 1") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.take(1)
        .at(List.singleton(1))
    }

    test("take - two take 1") {
        assertEvalEq(Cons(BigInt(1), Nil)): (list: List[BigInt]) =>
            list.take(1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("take - two take 3") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.take(3)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("take - two take 0") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.take(0)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("take - two take -1") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.take(-1)
        .at(Cons(1, Cons(2, Nil)))
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
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.takeRight(1)
        .at(List.empty[BigInt])
    }

    test("takeRight - single take 1") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.takeRight(1)
        .at(List.singleton(1))
    }

    test("takeRight - two take 1") {
        assertEvalEq(Cons(BigInt(2), Nil)): (list: List[BigInt]) =>
            list.takeRight(1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("takeRight - two take 3") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.takeRight(3)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("takeRight - two take 0") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.takeRight(0)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("takeRight - two take -1") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.takeRight(-1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("takeWhile - property") {
        check { (list: List[BigInt], value: BigInt) =>
            val scalusResult = list.takeWhile(_ < value)
            val scalaResult = list.asScala.takeWhile(_ < value)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("takeWhile - empty list") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.takeWhile(_ < 1)
        .at(List.empty[BigInt])
    }

    test("takeWhile - single, _ < 1") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.takeWhile(_ < 1)
        .at(List.singleton(1))
    }

    test("takeWhile - two, _ < 3") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.takeWhile(_ < 3)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("takeWhile - two, _ < 2") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.takeWhile(_ < 2)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("takeWhile - two, _ < 1") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.takeWhile(_ < 1)
        .at(Cons(1, Cons(2, Nil)))
    }

    test("distinct - property") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.distinct
            val scalaResult = list.asScala.distinct

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("distinct - empty") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.distinct
        .at(List.empty[BigInt])
    }

    test("distinct - single") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.distinct
        .at(List.singleton(1))
    }

    test("distinct - two unique") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.distinct
        .at(Cons(1, Cons(2, Nil)))
    }

    test("distinct - two duplicates") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.distinct
        .at(Cons(1, Cons(1, Nil)))
    }

    test("distinct - three with one duplicate") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.distinct
        .at(Cons(1, Cons(2, Cons(1, Nil))))
    }

    test("diff - property") {
        check { (list1: List[BigInt], list2: List[BigInt]) =>
            val scalusResult = list1.diff(list2)
            val scalaResult = list1.asScala.diff(list2.asScala)

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }
    }

    test("diff - empty diff empty") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.diff(List.empty[BigInt])
        .at(List.empty[BigInt])
    }

    test("diff - single diff empty") {
        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.diff(List.empty[BigInt])
        .at(List.singleton(1))
    }

    test("diff - empty diff single") {
        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.diff(List.singleton(BigInt(1)))
        .at(List.empty[BigInt])
    }

    test("diff - single diff matching single") {
        // diff is intrinsified composing the eq-stripped `deleteFirst` → slightly cheaper than
        // the prelude (which threaded a dead `Eq`). See v3-eq-eliminating.md.
        assertEvalEq(List.empty[BigInt])((list: List[BigInt]) =>
            list.diff(List.singleton(BigInt(1)))
        )
            .at(List.singleton(1))
    }

    test("diff - two diff matching tail") {
        assertEvalEq(Cons(BigInt(1), Nil)): (list: List[BigInt]) =>
            list.diff(List.singleton(BigInt(2)))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("diff - two diff non-matching") {
        assertEvalEq(Cons(BigInt(1), Cons(BigInt(2), Nil))): (list: List[BigInt]) =>
            list.diff(List.singleton(BigInt(3)))
        .at(Cons(1, Cons(2, Nil)))
    }

    test("init") {
        check { (list: List[BigInt]) =>
            val scalusResult = liftThrowableToOption(list.init)
            val scalaResult = liftThrowableToOption(list.asScala.init.asScalus)

            scalusResult === scalaResult
        }

        assertEvalFails[NoSuchElementException](List.empty[BigInt].init)

        assertEvalEq(List.empty[BigInt])((l: List[BigInt]) => l.init).at(List.singleton(BigInt(1)))

        assertEvalEq(Cons(BigInt(1), Nil)): (l: List[BigInt]) =>
            l.init
        .at(Cons(BigInt(1), Cons(BigInt(2), Nil)))
    }

    test("reverse") {
        check { (list: List[BigInt]) =>
            val scalusResult = list.reverse
            val scalaResult = list.asScala.reverse

            scalaResult.asScalus === scalusResult && scalusResult.asScala == scalaResult
        }

        assertEvalEq(List.empty[BigInt]): (list: List[BigInt]) =>
            list.reverse
        .at(List.empty[BigInt])

        assertEvalEq(List.singleton(BigInt(1))): (list: List[BigInt]) =>
            list.reverse
        .at(List.singleton(1))

        assertEvalEq(Cons(BigInt(2), Cons(BigInt(1), Nil))): (list: List[BigInt]) =>
            list.reverse
        .at(Cons(1, Cons(2, Nil)))

        assertEvalEq(Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil)))):
            (list: List[BigInt]) => list.reverse
        .at(Cons(1, Cons(2, Cons(3, Nil))))
    }

    test("foreach") {
        check { (list: List[BigInt], value: BigInt) =>
            var scalaSum = BigInt(0)
            var scalusSum = BigInt(0)

            list.foreach(x => scalusSum += x + value)
            list.asScala.foreach(x => scalaSum += x + value)

            scalusSum === scalaSum
        }

        assertEvalEq(())((list: List[BigInt]) => list.foreach(_ + 1)).at(List.empty[BigInt])

        assertEvalEq(())((list: List[BigInt]) => list.foreach(_ + 1)).at(List.singleton(1))

        assertEvalEq(())((list: List[BigInt]) => list.foreach(_ + 1)).at(Cons(1, Cons(2, Nil)))
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
        assertEvalEq[BigInt](20)(Cons(BigInt(10), Cons(BigInt(20), Cons(BigInt(30), Nil))).at(1))
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
