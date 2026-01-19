package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.RequirementError
import scalus.builtin.Data.{fromData, toData, FromData}
import scalus.testing.kit.EvalTestKit

class SortedMapTest extends AnyFunSuite with EvalTestKit {

    test("empty") {
        assertEvalEq(SortedMap.empty[BigInt, BigInt].toList, List.empty[(BigInt, BigInt)])
    }

    test("singleton") {
        check { (key: BigInt, value: BigInt) =>
            SortedMap.singleton(key, value).toList === List.single((key, value))
        }

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).toList,
          List.single((BigInt(1), BigInt(1)))
        )
    }

    test("unsafeFromList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                list.distinct(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
            SortedMap.unsafeFromList(strictlyAscendingList).toList === strictlyAscendingList
        }

        assertEvalEq(
          SortedMap
              .unsafeFromList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .toList,
          List.Cons(
            (BigInt(1), BigInt(1)),
            List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
          )
        )
    }

    test("fromList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                list.distinct(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
            SortedMap.fromList(list).toList === strictlyAscendingList
        }

        assertEvalEq(
          SortedMap
              .fromList(
                List.Cons(
                  (BigInt(2), BigInt(2)),
                  List.Cons((BigInt(2), BigInt(3)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
                )
              )
              .toList,
          List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
        )
    }

    test("fromStrictlyAscendingList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                list.distinct(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
            SortedMap
                .fromStrictlyAscendingList(strictlyAscendingList)
                .toList === strictlyAscendingList
        }

        assertThrows[RequirementError] {
            SortedMap.fromStrictlyAscendingList(
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
            )
        }

        assertThrows[RequirementError] {
            SortedMap.fromStrictlyAscendingList(
              List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
            )
        }

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .toList,
          List.Cons(
            (BigInt(1), BigInt(1)),
            List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
          )
        )

        assertEvalFails[RequirementError](
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
          )
        )

        assertEvalFails[RequirementError](
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
          )
        )
    }

    test("union") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = SortedMap.union(map, SortedMap.empty[BigInt, BigInt])
            val expected = map.mapValues[These[BigInt, BigInt]](These.This(_))

            result === expected
        }

        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = SortedMap.union(SortedMap.empty[BigInt, BigInt], map)
            val expected = map.mapValues[These[BigInt, BigInt]](These.That(_))

            result === expected
        }

        check { (lhs: SortedMap[BigInt, BigInt], rhs: SortedMap[BigInt, BigInt]) =>
            val result = SortedMap.union(lhs, rhs)
            val keys = (lhs.keys ++ rhs.keys).distinct
            val expected = keys.foldLeft(SortedMap.empty[BigInt, These[BigInt, BigInt]]) {
                (acc, key) =>
                    acc.insert(
                      key,
                      (lhs.get(key), rhs.get(key)) match
                          case (Option.Some(lv), Option.Some(rv)) => These.These(lv, rv)
                          case (Option.Some(lv), Option.None)     => These.This(lv)
                          case (Option.None, Option.Some(rv))     => These.That(rv)
                          case (Option.None, Option.None) =>
                              fail("unreachable: Both values are None")
                    )
            }

            result === expected
        }

        assertEvalEq(
          SortedMap
              .union(
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(2), BigInt(2)),
                          List.Cons((BigInt(3), BigInt(3)), List.Nil)
                        )
                      )
                    ),
                SortedMap.empty[BigInt, BigInt]
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.This(BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.This(BigInt(2))),
                    List.Cons((BigInt(3), These.This(BigInt(3))), List.Nil)
                  )
                )
              )
        )

        assertEvalEq(
          SortedMap
              .union(
                SortedMap.empty[BigInt, BigInt],
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(2), BigInt(2)),
                          List.Cons((BigInt(3), BigInt(3)), List.Nil)
                        )
                      )
                    )
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.That(BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.That(BigInt(2))),
                    List.Cons((BigInt(3), These.That(BigInt(3))), List.Nil)
                  )
                )
              )
        )

        assertEvalEq(
          SortedMap
              .union(
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(2), BigInt(2)),
                          List.Nil
                        )
                      )
                    ),
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(3), BigInt(3)),
                          List.Nil
                        )
                      )
                    )
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.These(BigInt(1), BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.This(BigInt(2))),
                    List.Cons((BigInt(3), These.That(BigInt(3))), List.Nil)
                  )
                )
              )
        )
    }

    test("unionMap") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = SortedMap.unionMap(map, SortedMap.empty[BigInt, BigInt], identity)
            val expected = map.mapValues[These[BigInt, BigInt]](These.This(_))

            result === expected
        }

        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = SortedMap.unionMap(SortedMap.empty[BigInt, BigInt], map, identity)
            val expected = map.mapValues[These[BigInt, BigInt]](These.That(_))

            result === expected
        }

        check { (lhs: SortedMap[BigInt, BigInt], rhs: SortedMap[BigInt, BigInt]) =>
            val result = SortedMap.unionMap(lhs, rhs, identity)
            val keys = (lhs.keys ++ rhs.keys).distinct
            val expected = keys.foldLeft(SortedMap.empty[BigInt, These[BigInt, BigInt]]) {
                (acc, key) =>
                    acc.insert(
                      key,
                      (lhs.get(key), rhs.get(key)) match
                          case (Option.Some(lv), Option.Some(rv)) => These.These(lv, rv)
                          case (Option.Some(lv), Option.None)     => These.This(lv)
                          case (Option.None, Option.Some(rv))     => These.That(rv)
                          case (Option.None, Option.None) =>
                              fail("unreachable: Both values are None")
                    )
            }

            result === expected
        }

        assertEvalEq(
          SortedMap
              .unionMap(
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(2), BigInt(2)),
                          List.Cons((BigInt(3), BigInt(3)), List.Nil)
                        )
                      )
                    ),
                SortedMap.empty[BigInt, BigInt],
                identity
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.This(BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.This(BigInt(2))),
                    List.Cons((BigInt(3), These.This(BigInt(3))), List.Nil)
                  )
                )
              )
        )

        assertEvalEq(
          SortedMap
              .unionMap(
                SortedMap.empty[BigInt, BigInt],
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(2), BigInt(2)),
                          List.Cons((BigInt(3), BigInt(3)), List.Nil)
                        )
                      )
                    ),
                identity
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.That(BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.That(BigInt(2))),
                    List.Cons((BigInt(3), These.That(BigInt(3))), List.Nil)
                  )
                )
              )
        )

        assertEvalEq(
          SortedMap
              .unionMap(
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(2), BigInt(2)),
                          List.Nil
                        )
                      )
                    ),
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(3), BigInt(3)),
                          List.Nil
                        )
                      )
                    ),
                identity
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.These(BigInt(1), BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.This(BigInt(2))),
                    List.Cons((BigInt(3), These.That(BigInt(3))), List.Nil)
                  )
                )
              )
        )
    }

    test("Eq") {
        check { (map: SortedMap[BigInt, BigInt]) => map === map }

        check { (map1: SortedMap[BigInt, BigInt], map2: SortedMap[BigInt, BigInt]) =>
            val result = map1 === map2
            val expected = map1.toList === map2.toList

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt],
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              )
        )

        assertEvalNotEq(
          SortedMap.empty[BigInt, BigInt],
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalNotEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(3), BigInt(3)),
                    List.Nil
                  )
                )
              )
        )
    }

    test("Ord") {
        check { (map: SortedMap[BigInt, BigInt]) => (map <=> map).isEqual }

        assertEval((SortedMap.empty[BigInt, BigInt] <=> SortedMap.empty[BigInt, BigInt]).isEqual)

        assertEval(
          (
            SortedMap.singleton(BigInt(0), BigInt(0)) <=>
                SortedMap.singleton(BigInt(0), BigInt(0))
          ).isEqual
        )

        assertEval(
          (
            SortedMap
                .fromStrictlyAscendingList(
                  List.Cons(
                    (BigInt(1), BigInt(1)),
                    List.Cons(
                      (BigInt(2), BigInt(2)),
                      List.Nil
                    )
                  )
                ) <=> SortedMap
                .fromStrictlyAscendingList(
                  List.Cons(
                    (BigInt(1), BigInt(1)),
                    List.Cons(
                      (BigInt(2), BigInt(2)),
                      List.Nil
                    )
                  )
                )
          ).isEqual
        )

        assertEval(
          (SortedMap.empty[BigInt, BigInt] <=> SortedMap.singleton(BigInt(1), BigInt(1))).isLess
        )

        assertEval(
          (
            SortedMap.singleton(BigInt(0), BigInt(0)) <=>
                SortedMap.singleton(BigInt(1), BigInt(1))
          ).isLess
        )

        assertEval(
          (
            SortedMap
                .fromStrictlyAscendingList(
                  List.Cons(
                    (BigInt(1), BigInt(1)),
                    List.Cons(
                      (BigInt(2), BigInt(2)),
                      List.Nil
                    )
                  )
                ) <=> SortedMap.singleton(BigInt(3), BigInt(3))
          ).isLess
        )

        assertEval(
          (SortedMap.singleton(BigInt(1), BigInt(1)) <=> SortedMap.empty[BigInt, BigInt]).isGreater
        )

        assertEval(
          (
            SortedMap.singleton(BigInt(1), BigInt(1)) <=>
                SortedMap.singleton(BigInt(0), BigInt(0))
          ).isGreater
        )

        assertEval(
          (
            SortedMap
                .fromStrictlyAscendingList(
                  List.Cons(
                    (BigInt(1), BigInt(1)),
                    List.Cons(
                      (BigInt(2), BigInt(2)),
                      List.Nil
                    )
                  )
                ) <=> SortedMap
                .fromStrictlyAscendingList(
                  List.Cons(
                    (BigInt(0), BigInt(0)),
                    List.Cons(
                      (BigInt(2), BigInt(2)),
                      List.Nil
                    )
                  )
                )
          ).isGreater
        )
    }

    test("ToData <-> FromData") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val data = map.toData
            val fromDataMap = fromData[SortedMap[BigInt, BigInt]](data)
            map === fromDataMap
        }

        assertEvalEq(
          fromData[SortedMap[BigInt, BigInt]](SortedMap.empty[BigInt, BigInt].toData),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          fromData[SortedMap[BigInt, BigInt]](SortedMap.singleton(BigInt(1), BigInt(1)).toData),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          fromData[SortedMap[BigInt, BigInt]](
            SortedMap
                .fromStrictlyAscendingList(
                  List.Cons(
                    (BigInt(1), BigInt(1)),
                    List.Cons(
                      (BigInt(2), BigInt(2)),
                      List.Cons((BigInt(3), BigInt(3)), List.Nil)
                    )
                  )
                )
                .toData
          ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Cons((BigInt(3), BigInt(3)), List.Nil)
                  )
                )
              )
        )

    }

    test("sortedMapFromDataWithValidation") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            given [A: FromData: Ord, B: FromData]: FromData[SortedMap[A, B]] =
                SortedMap.sortedMapFromDataWithValidation

            val data = map.toData
            val fromDataMap = fromData[SortedMap[BigInt, BigInt]](data)
            map === fromDataMap
        }

        val sir = scalus.compiler.compile {
            given [A: FromData: Ord, B: FromData]: FromData[SortedMap[A, B]] =
                SortedMap.sortedMapFromDataWithValidation

            val invalidMap = SortedMap
                .unsafeFromList(
                  List((BigInt(2), BigInt(2)), (BigInt(1), BigInt(1)))
                )

            val data = invalidMap.toData
            fromData[SortedMap[BigInt, BigInt]](data)
        }
        import scalus.*
        // val lw = sir.toLoweredValue()
        val uplc = sir.toUplc()

        // TODO:
        //  Evaluation is succesful, because in the currrent codebase implementation,
        //    fromData/toData is not used in the UPLC code, it is NOOP.
        //  We need to find a way, how to specify validation in the UPLC code
        //   disabling optimization.
        // assertEvalFails[RequirementError] {
        //    given [A: FromData: Ord, B: FromData]: FromData[SortedMap[A, B]] =
        //        SortedMap.sortedMapFromDataWithValidation
        //
        //    val invalidMap = SortedMap
        //        .unsafeFromList(
        //          List((BigInt(2), BigInt(2)), (BigInt(1), BigInt(1)))
        //        )
        //
        //    val data = invalidMap.toData
        //    fromData[SortedMap[BigInt, BigInt]](data)
        // }

    }

    test("isEmpty") {
        assertEval(SortedMap.empty[BigInt, BigInt].isEmpty)

        assertEval(!SortedMap.singleton(BigInt(1), BigInt(1)).isEmpty)

        assertEval(
          !SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .isEmpty
        )
    }

    test("nonEmpty") {
        assertEval(!SortedMap.empty[BigInt, BigInt].nonEmpty)

        assertEval(SortedMap.singleton(BigInt(1), BigInt(1)).nonEmpty)

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .nonEmpty
        )
    }

    test("length") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.length
            val expected = map.toList.length

            result === expected
        }

        assertEvalEq(SortedMap.empty[BigInt, BigInt].length, BigInt(0))

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1)).length, BigInt(1))

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .length,
          BigInt(3)
        )
    }

    test("size") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.size
            val expected = map.toList.length

            result === expected
        }

        assertEvalEq(SortedMap.empty[BigInt, BigInt].size, BigInt(0))

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1)).size, BigInt(1))

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .size,
          BigInt(3)
        )
    }

    test("keys") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.keys
            val expected = map.toList.map(_._1)

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].keys,
          List.Nil
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), "1").keys,
          List.single(BigInt(1))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), "1"),
                  List.Cons((BigInt(2), "2"), List.Cons((BigInt(3), "3"), List.Nil))
                )
              )
              .keys,
          List.Cons(BigInt(1), List.Cons(BigInt(2), List.Cons(BigInt(3), List.Nil)))
        )
    }

    test("values") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.values
            val expected = map.toList.map(_._2)

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].values,
          List.Nil
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), "1").values,
          List.single("1")
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), "1"),
                  List.Cons((BigInt(2), "2"), List.Cons((BigInt(3), "3"), List.Nil))
                )
              )
              .values,
          List.Cons("1", List.Cons("2", List.Cons("3", List.Nil)))
        )
    }

    test("forall") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.forall(predicate)
            val expected = map.toList.forall(predicate)

            result === expected
        }

        assertEval(
          SortedMap.empty[BigInt, BigInt].forall(_ => true)
        )

        assertEval(
          SortedMap.singleton(BigInt(1), BigInt(1)).forall(_._1 > 0)
        )

        assertEval(
          !SortedMap.singleton(BigInt(1), BigInt(1)).forall(_._1 < 0)
        )

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .forall(_._1 > 0)
        )

        assertEval(
          !SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .forall(_._1 > 2)
        )
    }

    test("exists") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.exists(predicate)
            val expected = map.toList.exists(predicate)

            result === expected
        }

        assertEval(
          !SortedMap.empty[BigInt, BigInt].exists(_ => true)
        )

        assertEval(
          SortedMap.singleton(BigInt(1), BigInt(1)).exists(_._1 > 0)
        )

        assertEval(
          !SortedMap.singleton(BigInt(1), BigInt(1)).exists(_._1 < 0)
        )

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .exists(_._1 > 2)
        )

        assertEval(
          !SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .exists(_._1 < 0)
        )
    }

    test("mapValues") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.mapValues(_ + 1)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.map { case (k, v) =>
                (k, v + 1)
            })

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].mapValues(_ + 1),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).mapValues(_ + 1),
          SortedMap.fromStrictlyAscendingList(List.single((BigInt(1), BigInt(2))))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .mapValues(_ + 1),
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(2)),
              List.Cons((BigInt(2), BigInt(3)), List.Cons((BigInt(3), BigInt(4)), List.Nil))
            )
          )
        )
    }

    test("filter") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.filter(predicate)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.filter(predicate))

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].filter(_ => true),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).filter(_._1 > 0),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filter(_._1 > 2),
          SortedMap.fromStrictlyAscendingList(List.Cons((BigInt(3), BigInt(3)), List.Nil))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filter(_._1 < 0),
          SortedMap.empty[BigInt, BigInt]
        )
    }

    test("filterNot") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.filterNot(predicate)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.filterNot(predicate))

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].filterNot(_ => true),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).filterNot(_._1 > 0),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filterNot(_._1 > 2),
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
          )
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filterNot(_._1 < 0),
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )
    }

    test("find") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.find(_._1 === key)
            val expected = map.toList.find(_._1 === key)

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].find(_._1 === BigInt(1)),
          Option.None
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).find(_._1 === BigInt(1)),
          Option.Some((BigInt(1), BigInt(1)))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).find(_._1 === BigInt(0)),
          Option.None
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .find(_._1 === BigInt(2)),
          Option.Some((BigInt(2), BigInt(2)))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .find(_._1 === BigInt(4)),
          Option.None
        )
    }

    test("findMap") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.findMap { case (k, v) =>
                if k === key then Option.Some(v) else Option.None
            }
            val expected = map.toList.findMap { case (k, v) =>
                if k === key then Option.Some(v) else Option.None
            }

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].findMap { case (k, v) => Option.Some(v) },
          Option.None
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).findMap { case (k, v) =>
              if k === BigInt(1) then Option.Some(v) else Option.None
          },
          Option.Some(BigInt(1))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).findMap { case (k, v) =>
              if k === BigInt(0) then Option.Some(v) else Option.None
          },
          Option.None
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .findMap { case (k, v) => if k === BigInt(2) then Option.Some(v) else Option.None },
          Option.Some(BigInt(2))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .findMap { case (k, v) => if k === BigInt(4) then Option.Some(v) else Option.None },
          Option.None
        )
    }

    test("foldLeft") {
        check { (map: SortedMap[BigInt, BigInt], initial: BigInt) =>
            val result = map.foldLeft(initial) { case (acc, (k, v)) => acc + k + v }
            val expected = map.toList.foldLeft(initial) { case (acc, (k, v)) => acc + k + v }

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v },
          BigInt(0)
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).foldLeft(BigInt(0)) { case (acc, (k, v)) =>
              acc + k + v
          },
          BigInt(2)
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v },
          BigInt(12)
        )
    }

    test("foldRight") {
        check { (map: SortedMap[BigInt, BigInt], initial: BigInt) =>
            val result = map.foldRight(initial) { case ((k, v), acc) => acc + k + v }
            val expected = map.toList.foldRight(initial) { case ((k, v), acc) => acc + k + v }

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].foldRight(BigInt(0)) { case ((k, v), acc) =>
              acc + k + v
          },
          BigInt(0)
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).foldRight(BigInt(0)) { case ((k, v), acc) =>
              acc + k + v
          },
          BigInt(2)
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .foldRight(BigInt(0)) { case ((k, v), acc) => acc + k + v },
          BigInt(12)
        )
    }

    test("get") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.get(key)
            val expected = map.toList.findMap { case (k, v) =>
                if k === key then Option.Some(v) else Option.None
            }

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].get(BigInt(1)),
          Option.None
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).get(BigInt(1)),
          Option.Some(BigInt(1))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).get(BigInt(0)),
          Option.None
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .get(BigInt(2)),
          Option.Some(BigInt(2))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .get(BigInt(4)),
          Option.None
        )
    }

    test("getOrFail") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = liftThrowableToOption(map.getOrFail(key))

            val expected = map.toList.findMap { case (k, v) =>
                if k === key then Option.Some(v) else Option.None
            }

            result === expected
        }

        assertEvalFails[NoSuchElementException](
          SortedMap.empty[BigInt, BigInt].getOrFail(BigInt(1))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).getOrFail(BigInt(1)),
          BigInt(1)
        )

        assertEvalFails[NoSuchElementException](
          SortedMap.singleton(BigInt(1), BigInt(1)).getOrFail(BigInt(0))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .getOrFail(BigInt(2)),
          BigInt(2)
        )

        assertEvalFails[NoSuchElementException](
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .getOrFail(BigInt(4))
        )
    }

    test("at") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = liftThrowableToOption(map.at(key))

            val expected = map.toList.findMap { case (k, v) =>
                if k === key then Option.Some(v) else Option.None
            }

            result === expected
        }

        assertEvalFails[NoSuchElementException](
          SortedMap.empty[BigInt, BigInt].at(BigInt(1))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).at(BigInt(1)),
          BigInt(1)
        )

        assertEvalFails[NoSuchElementException](
          SortedMap.singleton(BigInt(1), BigInt(1)).at(BigInt(0))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .at(BigInt(2)),
          BigInt(2)
        )

        assertEvalFails[NoSuchElementException](
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .at(BigInt(4))
        )
    }

    test("contains") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.contains(key)
            val expected = map.toList.exists(_._1 === key)

            result === expected
        }

        assertEval(
          !SortedMap.empty[BigInt, BigInt].contains(BigInt(1))
        )

        assertEval(
          SortedMap.singleton(BigInt(1), BigInt(1)).contains(BigInt(1))
        )

        assertEval(
          !SortedMap.singleton(BigInt(1), BigInt(1)).contains(BigInt(0))
        )

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .contains(BigInt(2))
        )

        assertEval(
          !SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .contains(BigInt(4))
        )
    }

    test("insert") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt, value: BigInt) =>
            val result = map.insert(key, value)
            val expected =
                SortedMap.fromList(map.toList.filterNot(_._1 === key) ++ List.single((key, value)))

            result === expected
        }

        check { (map: SortedMap[BigInt, BigInt], key: BigInt, value: BigInt) =>
            map.insert(key, value).get(key) === Option.Some(value)
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].insert(BigInt(1), BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).insert(BigInt(2), BigInt(2)),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
              )
              .insert(BigInt(2), BigInt(2)),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
        )
    }

    test("delete") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.delete(key)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.filterNot(_._1 === key))

            result === expected
        }

        check { (map: SortedMap[BigInt, BigInt], key: BigInt, value: BigInt) =>
            val newMap = map.insert(key, value)
            newMap.contains(key) && !newMap.delete(key).contains(key)
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].delete(BigInt(1)),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).delete(BigInt(1)),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).delete(BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
              .delete(BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
              .delete(BigInt(3)),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
        )
    }

    test("from") {
        import scalus.builtin.ByteString
        import scalus.builtin.ByteString.{hex, given}

        // Test with empty collection
        val emptyMap = SortedMap.from(scala.List.empty[(ByteString, BigInt)])
        assert(emptyMap.toList === List.empty)

        // Test with single element
        val singleMap = SortedMap.from(scala.List((hex"aa", BigInt(10))))
        assert(singleMap.toList === List((hex"aa", BigInt(10))))

        // Test with multiple elements in unsorted order
        val unsortedMap = SortedMap.from(
          scala.List((hex"cc", BigInt(30)), (hex"aa", BigInt(10)), (hex"bb", BigInt(20)))
        )
        assert(
          unsortedMap.toList === List(
            (hex"aa", BigInt(10)),
            (hex"bb", BigInt(20)),
            (hex"cc", BigInt(30))
          )
        )

        // Test with duplicate keys - last inserted value should win
        val duplicateMap = SortedMap.from(
          scala.List((hex"aa", BigInt(10)), (hex"bb", BigInt(20)), (hex"aa", BigInt(100)))
        )
        assert(duplicateMap.toList === List((hex"aa", BigInt(100)), (hex"bb", BigInt(20))))

        // Test with Vector
        val vectorMap =
            SortedMap.from(Vector((hex"bb", BigInt(2)), (hex"aa", BigInt(1)), (hex"cc", BigInt(3))))
        assert(
          vectorMap.toList === List(
            (hex"aa", BigInt(1)),
            (hex"bb", BigInt(2)),
            (hex"cc", BigInt(3))
          )
        )

        // Test that result is properly sorted
        val largeMap = SortedMap.from(
          scala.List(
            (hex"ee", BigInt(5)),
            (hex"bb", BigInt(2)),
            (hex"ff", BigInt(8)),
            (hex"aa", BigInt(1)),
            (hex"dd", BigInt(9))
          )
        )
        assert(
          largeMap.toList === List(
            (hex"aa", BigInt(1)),
            (hex"bb", BigInt(2)),
            (hex"dd", BigInt(9)),
            (hex"ee", BigInt(5)),
            (hex"ff", BigInt(8))
          )
        )
    }

}
