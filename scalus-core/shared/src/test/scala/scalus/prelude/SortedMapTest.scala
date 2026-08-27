package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.prelude.{identity, Eq, List, Option, Ord, SortedMap, These}
import scalus.uplc.builtin.Data.{fromData, toData, FromData}
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.EvalTestKit
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.Result

class SortedMapTest extends AnyFunSuite with EvalTestKit {

    /** Keep the first occurrence per key, preserving input order. Used to construct reference lists
      * for `SortedMap` constructor tests without going through the deprecated `Eq.keyPairEq` (which
      * intentionally violates the `Eq` contract).
      */
    private def distinctByKey[A: Eq, B](lst: List[(A, B)]): List[(A, B)] =
        lst.foldLeft(List.empty[(A, B)]) { (acc, p) =>
            if acc.exists(_._1 === p._1) then acc else List.Cons(p, acc)
        }.reverse

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

        { // Lowering check: `singleton(k, v).toData` must compile to the direct Data construction
            // (one mapData/mkCons/mkPairData chain), not a list build followed by a conversion walk.
            // This is what makes e.g. `equalsData(innerMap.toData, SortedMap.singleton(tn, 1).toData)`
            // a cheap exact-token check on `tx.mint`.
            import scalus.toUplcOptimized
            import scalus.uplc.DeBruijn
            import scalus.uplc.builtin.{Builtins, ByteString}

            val preludeUplc = scalus.compiler
                .compile { (tn: ByteString) =>
                    SortedMap.singleton(tn, BigInt(1)).toData
                }
                .toUplcOptimized()
            val rawUplc = scalus.compiler
                .compile { (tn: ByteString) =>
                    Builtins.mapData(
                      Builtins.mkCons(
                        Builtins.mkPairData(Builtins.bData(tn), Builtins.iData(BigInt(1))),
                        Builtins.mkNilPairData()
                      )
                    )
                }
                .toUplcOptimized()
            assert(
              DeBruijn.deBruijnTerm(preludeUplc) α_== DeBruijn.deBruijnTerm(rawUplc),
              s"singleton lowering changed:\n${preludeUplc.show}\nexpected:\n${rawUplc.show}"
            )

            val applied = preludeUplc $ ByteString.fromString("BEACON").asTerm
            applied.evaluateDebug match
                case Result.Success(_, exunits, _, _) =>
                    assert(
                      exunits == ExUnits(memory = 1928, steps = 435437),
                      s"Budget mismatch: got $exunits"
                    )
                case Result.Failure(e, _, _, _) => fail(s"Expected success: $e")
        }
    }

    test("unsafeFromList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                distinctByKey(list).quicksort(using Ord.keyPairOrd)
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
                      exunits == ExUnits(memory = 13768, steps = 3_678255),
                      s"Budget mismatch: got $exunits"
                    )
                case Result.Failure(e, _, _, _) => fail(s"Expected success: $e")
        }
    }

    test("fromList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                distinctByKey(list).quicksort(using Ord.keyPairOrd)
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
                        ExUnits(memory = 42023, steps = 10_328376)
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
                distinctByKey(list).quicksort(using Ord.keyPairOrd)
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
          ExUnits(memory = 37134, steps = 9_142509)
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
          ExUnits(memory = 53058, steps = 13_793052)
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
          ExUnits(memory = 52226, steps = 13_640429)
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
          ExUnits(memory = 67289, steps = 16_193945)
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
          ExUnits(memory = 54158, steps = 13_969052)
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
          ExUnits(memory = 53326, steps = 13_816429)
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
          ExUnits(memory = 68389, steps = 16_369945)
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
          ExUnits(memory = 17837, steps = 4_371486)
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
          ExUnits(memory = 26418, steps = 6_752175)
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
            compilerOptions -> ExUnits(memory = 2132, steps = 344723)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.length,
          SortedMap.singleton(BigInt(1), BigInt(1)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 3534, steps = 669931)
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
          ExUnits(memory = 31956, steps = 7_960422)
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
            compilerOptions -> ExUnits(memory = 2132, steps = 344723)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.size,
          SortedMap.singleton(BigInt(1), BigInt(1)),
          BigInt(1),
          Seq(
            compilerOptions -> ExUnits(memory = 3534, steps = 669931)
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
          ExUnits(memory = 31956, steps = 7_960422)
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
          ExUnits(memory = 4364, steps = 722656)
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.keys,
          SortedMap.singleton(BigInt(1), BigInt(1)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 12176, steps = 2_891880)
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
          ExUnits(memory = 52218, steps = 13_678403)
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
          ExUnits(memory = 4364, steps = 722656)
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.values,
          SortedMap.singleton(BigInt(1), BigInt(1)),
          List.single(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 12408, steps = 3_005543)
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
          ExUnits(memory = 52914, steps = 14_019392)
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
            compilerOptions -> ExUnits(memory = 3764, steps = 668969)
          )
        )

        assertEvalWithBudget(
          (m: SortedMap[BigInt, BigInt]) => m.mapValues(_ + 1),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.fromStrictlyAscendingList(List.single((BigInt(1), BigInt(2)))),
          ExUnits(memory = 9054, steps = 2_082815)
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
          ExUnits(memory = 45052, steps = 11_518582)
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
            compilerOptions -> ExUnits(memory = 3064, steps = 556969)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.filter(_._1 > 0),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 7225, steps = 1_563239)
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
          ExUnits(memory = 40501, steps = 9_975130)
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
          ExUnits(memory = 40069, steps = 9_838768)
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
            compilerOptions -> ExUnits(memory = 3064, steps = 556969)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.filterNot(_._1 > 0),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 6993, steps = 1_458877)
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
          ExUnits(memory = 41533, steps = 10_207492)
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
          ExUnits(memory = 41965, steps = 10_343854)
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
            compilerOptions -> ExUnits(memory = 2832, steps = 456723)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.find(_._1 === BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.Some((BigInt(1), BigInt(1))),
          Seq(
            compilerOptions -> ExUnits(memory = 7357, steps = 1_562338)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.find(_._1 === BigInt(0)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.None,
          Seq(
            compilerOptions -> ExUnits(memory = 6861, steps = 1_381674)
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
          ExUnits(memory = 37304, steps = 9_175364)
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
          ExUnits(memory = 40737, steps = 9_903651)
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
            compilerOptions -> ExUnits(memory = 3932, steps = 632723)
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
            compilerOptions -> ExUnits(memory = 10581, steps = 2_671138)
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
            compilerOptions -> ExUnits(memory = 8725, steps = 1_836157)
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
          ExUnits(memory = 40392, steps = 10_418647)
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
          ExUnits(memory = 43329, steps = 10_787100)
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
            compilerOptions -> ExUnits(memory = 4564, steps = 749467)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) =>
              m.foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v },
          SortedMap.singleton(BigInt(1), BigInt(1)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 14476, steps = 3_527562)
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
          ExUnits(memory = 59518, steps = 15_659827)
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
            compilerOptions -> ExUnits(memory = 4364, steps = 717467)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) =>
              m.foldRight(BigInt(0)) { case ((k, v), acc) => acc + k + v },
          SortedMap.singleton(BigInt(1), BigInt(1)),
          BigInt(2),
          Seq(
            compilerOptions -> ExUnits(memory = 13776, steps = 3_415562)
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
          ExUnits(memory = 57818, steps = 15_387827)
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
            compilerOptions -> ExUnits(memory = 1832, steps = 296723)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.get(BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.Some(BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5330, steps = 1_046560)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.get(BigInt(0)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Option.None,
          Seq(
            compilerOptions -> ExUnits(memory = 3765, steps = 666757)
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
          ExUnits(memory = 39046, steps = 9_119447)
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
          ExUnits(memory = 43748, steps = 9_909002)
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
            compilerOptions -> ExUnits(memory = 2128, steps = 691881)
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
          ExUnits(memory = 40906, steps = 9_803816)
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
            compilerOptions -> ExUnits(memory = 2128, steps = 691881)
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
          ExUnits(memory = 40906, steps = 9_803816)
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
            compilerOptions -> ExUnits(memory = 2464, steps = 460969)
          )
        )

        assertEvalWithBudget(
          (m: SortedMap[BigInt, BigInt]) => m.insert(BigInt(2), BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              ),
          ExUnits(memory = 6730, steps = 1_316655)
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
          ExUnits(memory = 33452, steps = 7_792882)
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
            compilerOptions -> ExUnits(memory = 2064, steps = 396969)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.delete(BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.empty[BigInt, BigInt],
          Seq(
            compilerOptions -> ExUnits(memory = 4598, steps = 908293)
          )
        )

        assertEvalWithBudgets(
          (m: SortedMap[BigInt, BigInt]) => m.delete(BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          Seq(
            compilerOptions -> ExUnits(memory = 5930, steps = 1_188655)
          )
        )

        assertEvalWithBudget(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
              .delete(BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1)),
          ExUnits(memory = 29965, steps = 6_704853)
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
          ExUnits(memory = 31197, steps = 6_969215)
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
