package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.prelude.{identity, Eq, List, Option, Ord, SortedMap, These}
import scalus.uplc.builtin.Data.{fromData, toData, FromData, ToData}
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.EvalTestKit
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.Result

class SortedMapTest extends AnyFunSuite with EvalTestKit {

    test("empty") {
        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].toList,
          List.empty[(BigInt, BigInt)]
        )
    }

    test("singleton") {
        check { (key: BigInt, value: BigInt) =>
            SortedMap.singleton(key, value).toList === List.single((key, value))
        }

        { // Budget-only check: term representation changed from list data to list (pair data data)
            val compiled = PlutusV3.compile((d: scalus.uplc.builtin.Data) => {
                val m = d.to[SortedMap[BigInt, BigInt]]
                m.toList
            })
            val applied =
                compiled.program.term $ toData(SortedMap.singleton(BigInt(1), BigInt(1))).asTerm
            applied.evaluateDebug match
                case Result.Success(_, exunits, _, _) =>
                    assert(
                      exunits == ExUnits(memory = 432, steps = 72723),
                      s"Budget mismatch: got $exunits"
                    )
                case Result.Failure(e, _, _, _) => fail(s"Expected success: $e")
        }
    }

    test("unsafeFromList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                list.distinct(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
            SortedMap.unsafeFromList(strictlyAscendingList).toList === strictlyAscendingList
        }

        { // Budget-only check: term representation changed from list data to list (pair data data)
            val compiled = PlutusV3.compile((d: scalus.uplc.builtin.Data) => {
                val list = d.to[List[(BigInt, BigInt)]]
                SortedMap.unsafeFromList(list).toList
            })
            val arg = List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
            val applied = compiled.program.term $ toData(arg).asTerm
            applied.evaluateDebug match
                case Result.Success(_, exunits, _, _) =>
                    assert(
                      exunits == ExUnits(memory = 19588, steps = 5_584670),
                      s"Budget mismatch: got $exunits"
                    )
                case Result.Failure(e, _, _, _) => fail(s"Expected success: $e")
        }
    }

    test("fromList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                list.distinct(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
            SortedMap.fromList(list).toList === strictlyAscendingList
        }

        { // Budget-only check: term representation changed from list data to list (pair data data)
            val compiled = PlutusV3.compile((d: scalus.uplc.builtin.Data) => {
                val list = d.to[List[(BigInt, BigInt)]]
                SortedMap.fromList(list).toList
            })
            val arg = List.Cons(
              (BigInt(2), BigInt(2)),
              List.Cons((BigInt(2), BigInt(3)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
            )
            val applied = compiled.program.term $ toData(arg).asTerm
            applied.evaluateDebug match
                case Result.Success(_, exunits, _, _) =>
                    val expected =
                        ExUnits(memory = 61364L, steps = 16554940L)
                    assert(
                      exunits == expected,
                      s"Budget mismatch: got $exunits, expected $expected"
                    )
                case Result.Failure(e, _, _, _) => fail(s"Expected success: $e")
        }
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

        assertEvalWithBudget(
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
          ),
          ExUnits(memory = 60117, steps = 17_696430)
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

        assertEvalWithBudget(
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
              ),
          ExUnits(memory = 77805L, steps = 22884961L)
        )

        assertEvalWithBudget(
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
              ),
          ExUnits(memory = 78441, steps = 22_839_344)
        )

        assertEvalWithBudget(
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
              ),
          ExUnits(memory = 105990, steps = 29_964_991)
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

        assertEvalWithBudget(
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
              ),
          ExUnits(memory = 78905L, steps = 23060961L)
        )

        assertEvalWithBudget(
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
              ),
          ExUnits(memory = 79541, steps = 23_015_344)
        )

        assertEvalWithBudget(
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
              ),
          ExUnits(memory = 107290, steps = 30_172_991)
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

        assertEvalWithBudget(
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
              ),
          ExUnits(memory = 29185, steps = 8_136_659)
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

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => fromData[SortedMap[BigInt, BigInt]](m.toData),
          SortedMap.empty[BigInt, BigInt],
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 500, steps = 64100)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => fromData[SortedMap[BigInt, BigInt]](m.toData),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 500, steps = 64100)
          )
        )

        assertEvalWithBudget(
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
              ),
          ExUnits(memory = 43289, steps = 12_532_020)
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

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.length,
          SortedMap.empty[BigInt, BigInt],
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 3164, steps = 637717)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.length,
          SortedMap.singleton(BigInt(1), BigInt(1)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6030, steps = 1_401_582)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .length,
          BigInt(3),
          ExUnits(memory = 53151, steps = 15_173_232)
        )
    }

    test("size") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.size
            val expected = map.toList.length

            result === expected
        }

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.size,
          SortedMap.empty[BigInt, BigInt],
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 3164, steps = 637717)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.size,
          SortedMap.singleton(BigInt(1), BigInt(1)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 6030, steps = 1_401_582)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .size,
          BigInt(3),
          ExUnits(memory = 53151, steps = 15_173_232)
        )
    }

    test("keys") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.keys
            val expected = map.toList.map(_._1)

            result === expected
        }

        assertEvalWithBudget(
          (m: SortedMap[BigInt, BigInt]) => m.keys,
          SortedMap.empty[BigInt, BigInt],
          List.empty[BigInt],
          ExUnits(memory = 5696L, steps = 1063650L)
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.keys,
          SortedMap.singleton(BigInt(1), BigInt(1)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 16468, steps = 4_262_568)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), "1"),
                  List.Cons((BigInt(2), "2"), List.Cons((BigInt(3), "3"), List.Nil))
                )
              )
              .keys,
          List.Cons(BigInt(1), List.Cons(BigInt(2), List.Cons(BigInt(3), List.Nil))),
          ExUnits(memory = 78101, steps = 22_696_324)
        )
    }

    test("values") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.values
            val expected = map.toList.map(_._2)

            result === expected
        }

        assertEvalWithBudget(
          (m: SortedMap[BigInt, BigInt]) => m.values,
          SortedMap.empty[BigInt, BigInt],
          List.empty[BigInt],
          ExUnits(memory = 5696L, steps = 1063650L)
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.values,
          SortedMap.singleton(BigInt(1), BigInt(1)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 16700, steps = 4_376_231)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), "1"),
                  List.Cons((BigInt(2), "2"), List.Cons((BigInt(3), "3"), List.Nil))
                )
              )
              .values,
          List.Cons("1", List.Cons("2", List.Cons("3", List.Nil))),
          ExUnits(memory = 78797, steps = 23_037_313)
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

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.mapValues(_ + 1),
          SortedMap.empty[BigInt, BigInt],
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5196, steps = 1_025963)
          )
        )

        assertEvalWithBudget(
          (m: SortedMap[BigInt, BigInt]) => m.mapValues(_ + 1),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.fromStrictlyAscendingList(List.single((BigInt(1), BigInt(2)))),
          ExUnits(memory = 12946L, steps = 3389503L)
        )

        assertEvalWithBudget(
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
          ),
          ExUnits(memory = 70735, steps = 20_504_503)
        )
    }

    test("filter") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.filter(predicate)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.filter(predicate))

            result === expected
        }

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.filter(_ => true),
          SortedMap.empty[BigInt, BigInt],
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 5496, steps = 1_073963)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.filter(_._1 > 0),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 12682L, steps = 3352276L)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filter(_._1 > 2),
          SortedMap.fromStrictlyAscendingList(List.Cons((BigInt(3), BigInt(3)), List.Nil)),
          ExUnits(memory = 67715, steps = 19_745_798)
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filter(_._1 < 0),
          SortedMap.empty[BigInt, BigInt],
          ExUnits(memory = 67051, steps = 19_494_286)
        )
    }

    test("filterNot") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.filterNot(predicate)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.filterNot(predicate))

            result === expected
        }

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.filterNot(_ => true),
          SortedMap.empty[BigInt, BigInt],
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6096, steps = 1_169_963)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.filterNot(_._1 > 0),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 13419L, steps = 3400813L)
          )
        )

        assertEvalWithBudget(
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
          ),
          ExUnits(memory = 71382, steps = 20_705_457)
        )

        assertEvalWithBudget(
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
          ),
          ExUnits(memory = 72046, steps = 20_956_969)
        )
    }

    test("find") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.find(_._1 === key)
            val expected = map.toList.find(_._1 === key)

            result === expected
        }

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.find(_._1 === BigInt(1)),
          SortedMap.empty[BigInt, BigInt],
          Option.None,
          Seq(
            compilerOptions -> ExUnits(memory = 5664, steps = 1_037717)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.find(_._1 === BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.Some((BigInt(1), BigInt(1))),
          Seq(
            compilerOptions -> ExUnits(memory = 12410, steps = 3_568_479)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.find(_._1 === BigInt(0)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.None,
          Seq(
            compilerOptions -> ExUnits(memory = 11986L, steps = 3039561L)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .find(_._1 === BigInt(2)),
          Option.Some((BigInt(2), BigInt(2))),
          ExUnits(memory = 60121, steps = 17_814_243)
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .find(_._1 === BigInt(4)),
          Option.None,
          ExUnits(memory = 66819, steps = 19_415_169)
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

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.findMap { case (k, v) => Option.Some(v) },
          SortedMap.empty[BigInt, BigInt],
          Option.None,
          Seq(
            compilerOptions -> ExUnits(memory = 5664, steps = 1_037_717)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) =>
              m.findMap { case (k, v) =>
                  if k === BigInt(1) then Option.Some(v) else Option.None
              },
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 13912, steps = 3_887_600)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) =>
              m.findMap { case (k, v) =>
                  if k === BigInt(0) then Option.Some(v) else Option.None
              },
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.None,
          Seq(
            compilerOptions -> ExUnits(memory = 13920L, steps = 3555276L)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .findMap { case (k, v) => if k === BigInt(2) then Option.Some(v) else Option.None },
          Option.Some(BigInt(2)),
          ExUnits(memory = 63557, steps = 18_649_079)
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .findMap { case (k, v) => if k === BigInt(4) then Option.Some(v) else Option.None },
          Option.None,
          ExUnits(memory = 72621, steps = 20_962_314)
        )
    }

    test("foldLeft") {
        check { (map: SortedMap[BigInt, BigInt], initial: BigInt) =>
            val result = map.foldLeft(initial) { case (acc, (k, v)) => acc + k + v }
            val expected = map.toList.foldLeft(initial) { case (acc, (k, v)) => acc + k + v }

            result === expected
        }

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) =>
              m.foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v },
          SortedMap.empty[BigInt, BigInt],
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 5696L, steps = 1058461L)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) =>
              m.foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v },
          SortedMap.singleton(BigInt(1), BigInt(1)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 19268, steps = 4_978_250)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v },
          BigInt(12),
          ExUnits(memory = 87701, steps = 25_045_748)
        )
    }

    test("foldRight") {
        check { (map: SortedMap[BigInt, BigInt], initial: BigInt) =>
            val result = map.foldRight(initial) { case ((k, v), acc) => acc + k + v }
            val expected = map.toList.foldRight(initial) { case ((k, v), acc) => acc + k + v }

            result === expected
        }

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) =>
              m.foldRight(BigInt(0)) { case ((k, v), acc) => acc + k + v },
          SortedMap.empty[BigInt, BigInt],
          BigInt(0),
          Seq(
            compilerOptions -> ExUnits(memory = 5696L, steps = 1058461L)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) =>
              m.foldRight(BigInt(0)) { case ((k, v), acc) => acc + k + v },
          SortedMap.singleton(BigInt(1), BigInt(1)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 18068, steps = 4_786_250)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .foldRight(BigInt(0)) { case ((k, v), acc) => acc + k + v },
          BigInt(12),
          ExUnits(memory = 84101, steps = 24_469_748)
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

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.get(BigInt(1)),
          SortedMap.empty[BigInt, BigInt],
          Option.None,
          Seq(
            compilerOptions -> ExUnits(memory = 5064, steps = 941717)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.get(BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 12228, steps = 3_193_936)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.get(BigInt(0)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.None,
          Seq(
            compilerOptions -> ExUnits(memory = 9328, steps = 2_259_710)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .get(BigInt(2)),
          Option.Some(BigInt(2)),
          ExUnits(memory = 65413, steps = 18_445_715)
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .get(BigInt(4)),
          Option.None,
          ExUnits(memory = 76145, steps = 21_250_982)
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

        assertEvalWithBudgets(
          SortedMap.singleton(BigInt(1), BigInt(1)).getOrFail(BigInt(1)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 3130, steps = 980263)
          )
        )

        assertEvalFails[NoSuchElementException](
          SortedMap.singleton(BigInt(1), BigInt(1)).getOrFail(BigInt(0))
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .getOrFail(BigInt(2)),
          BigInt(2),
          ExUnits(memory = 67975, steps = 19_370_466)
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

        assertEvalWithBudgets(
          SortedMap.singleton(BigInt(1), BigInt(1)).at(BigInt(1)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 3130, steps = 980263)
          )
        )

        assertEvalFails[NoSuchElementException](
          SortedMap.singleton(BigInt(1), BigInt(1)).at(BigInt(0))
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .at(BigInt(2)),
          BigInt(2),
          ExUnits(memory = 67975, steps = 19_370_466)
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

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.insert(BigInt(1), BigInt(1)),
          SortedMap.empty[BigInt, BigInt],
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 4896, steps = 977963)
          )
        )

        assertEvalWithBudget(
          (m: SortedMap[BigInt, BigInt]) => m.insert(BigInt(2), BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              ),
          ExUnits(memory = 14260, steps = 3_632_696)
        )

        assertEvalWithBudget(
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
              ),
          ExUnits(memory = 52861, steps = 14_534_055)
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

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.delete(BigInt(1)),
          SortedMap.empty[BigInt, BigInt],
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4896, steps = 977963)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.delete(BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 11196, steps = 2_947_340)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.delete(BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 13760, steps = 3_552_696)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
              .delete(BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          ExUnits(memory = 50809, steps = 13_956_120)
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
              .delete(BigInt(3)),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              ),
          ExUnits(memory = 53173, steps = 14_529_476)
        )
    }

    test("from") {
        import scalus.uplc.builtin.ByteString
        import scalus.uplc.builtin.ByteString.{hex, given}

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
