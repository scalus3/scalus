package scalus.cardano.onchain.plutus.prelude

import scalus.testing.budget.BudgetTable
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.{OnchainError, RequirementError}
import scalus.cardano.onchain.plutus.prelude.{identity, Eq, List, Option, Ord, SortedMap, These}
import scalus.uplc.builtin.Data.{fromData, toData, FromData}
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.EvalTestKit
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.Result

class SortedMapTest extends AnyFunSuite with EvalTestKit {
    override protected def budgetTable: BudgetTable = SortedMapTestBudgets

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
            SortedMap.singleton(key, value).toList === List.singleton((key, value))
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

    test("singleOrFail") {
        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(2)).singleOrFail("expected one"),
          (BigInt(1), BigInt(2))
        )
        assertEvalEq(BigInt(2)): (m: SortedMap[BigInt, BigInt]) =>
            m.singleOrFail("expected one")._2
        .at(SortedMap.singleton(BigInt(1), BigInt(2)))
        assertEvalFailsWithMessage[OnchainError]("expected one")(
          SortedMap.empty[BigInt, BigInt].singleOrFail("expected one")
        )
        assertEvalFailsWithMessage[OnchainError]("expected one")(
          SortedMap
              .fromList(
                List.Cons((BigInt(1), BigInt(2)), List.Cons((BigInt(3), BigInt(4)), List.Nil))
              )
              .singleOrFail("expected one")
        )
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
                      exunits == ExUnits(memory = 13768, steps = 3_678_255),
                      s"Budget mismatch: got $exunits"
                    )
                case Result.Failure(e, _, _, _) => fail(s"Expected success: $e")
        }
    }

    test("fromLargeList agrees with fromList on every input") {
        check { (list: List[(BigInt, BigInt)]) =>
            SortedMap.fromLargeList(list).toList === SortedMap.fromList(list).toList
        }
        assertEvalEq(
          SortedMap
              .fromLargeList(
                List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
              )
              .toList,
          List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
        )
    }

    /** Both constructors document that the FIRST occurrence of a duplicate key prevails. They reach
      * that by opposite routes — `fromList` inserts later-elements-first and lets the incoming pair
      * replace an equal key, `fromLargeList` relies on the sort being stable and keeps the head of
      * each run — so the property is worth checking on both rather than assumed.
      */
    test("first occurrence of a duplicate key wins, in both constructors") {
        check { (list: List[(BigInt, BigInt)]) =>
            val firstWins = list.asScala
                .foldLeft(Vector.empty[(BigInt, BigInt)]) { (acc, p) =>
                    if acc.exists(_._1 == p._1) then acc else acc :+ p
                }
                .sortBy(_._1)
            SortedMap.fromList(list).toList.asScala == firstWins
            && SortedMap.fromLargeList(list).toList.asScala == firstWins
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
                        ExUnits(memory = 41638, steps = 9_866_953)
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

        // Compared inside the script: toList returns native pairs, while a list literal compiles to
        // Data constructors, so the two UPLC terms differ structurally for the same value.
        assertEvalEq(true): (l: List[(BigInt, BigInt)]) =>
            SortedMap.fromStrictlyAscendingList(l).toList === List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
        .at(
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
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.This(BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.This(BigInt(2))),
                    List.Cons((BigInt(3), These.This(BigInt(3))), List.Nil)
                  )
                )
              )
        ): (a: SortedMap[BigInt, BigInt], b: SortedMap[BigInt, BigInt]) =>
            SortedMap.union(a, b)
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          ),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
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
        ): (a: SortedMap[BigInt, BigInt], b: SortedMap[BigInt, BigInt]) =>
            SortedMap.union(a, b)
        .at(
          SortedMap.empty[BigInt, BigInt],
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )

        assertEvalEq(
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
        ): (a: SortedMap[BigInt, BigInt], b: SortedMap[BigInt, BigInt]) =>
            SortedMap.union(a, b)
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
          ),
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
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
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.This(BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.This(BigInt(2))),
                    List.Cons((BigInt(3), These.This(BigInt(3))), List.Nil)
                  )
                )
              )
        ): (a: SortedMap[BigInt, BigInt], b: SortedMap[BigInt, BigInt]) =>
            SortedMap.unionMap(a, b, identity)
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          ),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
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
        ): (a: SortedMap[BigInt, BigInt], b: SortedMap[BigInt, BigInt]) =>
            SortedMap.unionMap(a, b, identity)
        .at(
          SortedMap.empty[BigInt, BigInt],
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )

        assertEvalEq(
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
        ): (a: SortedMap[BigInt, BigInt], b: SortedMap[BigInt, BigInt]) =>
            SortedMap.unionMap(a, b, identity)
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
          ),
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
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
              )
        ): (l: List[(BigInt, BigInt)]) =>
            SortedMap.fromStrictlyAscendingList(l)
        .at(
          List.Cons(
            (BigInt(1), BigInt(1)),
            List.Cons(
              (BigInt(2), BigInt(2)),
              List.Nil
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

        assertEvalEq(SortedMap.empty[BigInt, BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            fromData[SortedMap[BigInt, BigInt]](m.toData)
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1))): (m: SortedMap[BigInt, BigInt]) =>
            fromData[SortedMap[BigInt, BigInt]](m.toData)
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(
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
        ):
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

        assertEvalEq(BigInt(0)): (m: SortedMap[BigInt, BigInt]) =>
            m.length
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(BigInt(1)): (m: SortedMap[BigInt, BigInt]) =>
            m.length
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(BigInt(3)): (m: SortedMap[BigInt, BigInt]) =>
            m.length
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )
    }

    test("size") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.size
            val expected = map.toList.length

            result === expected
        }

        assertEvalEq(BigInt(0)): (m: SortedMap[BigInt, BigInt]) =>
            m.size
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(BigInt(1)): (m: SortedMap[BigInt, BigInt]) =>
            m.size
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(BigInt(3)): (m: SortedMap[BigInt, BigInt]) =>
            m.size
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )
    }

    test("keys") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.keys
            val expected = map.toList.map(_._1)

            result === expected
        }

        assertEvalEq(List.empty[BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            m.keys
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(List.singleton(BigInt(1))): (m: SortedMap[BigInt, BigInt]) =>
            m.keys
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(List.Cons(BigInt(1), List.Cons(BigInt(2), List.Cons(BigInt(3), List.Nil)))):
            (m: SortedMap[BigInt, String]) => m.keys
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), "1"),
              List.Cons((BigInt(2), "2"), List.Cons((BigInt(3), "3"), List.Nil))
            )
          )
        )
    }

    test("values") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.values
            val expected = map.toList.map(_._2)

            result === expected
        }

        assertEvalEq(List.empty[BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            m.values
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(List.singleton(BigInt(1))): (m: SortedMap[BigInt, BigInt]) =>
            m.values
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(List.Cons("1", List.Cons("2", List.Cons("3", List.Nil)))):
            (m: SortedMap[BigInt, String]) => m.values
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), "1"),
              List.Cons((BigInt(2), "2"), List.Cons((BigInt(3), "3"), List.Nil))
            )
          )
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

        assertEvalEq(SortedMap.empty[BigInt, BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            m.mapValues(_ + 1)
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(SortedMap.fromStrictlyAscendingList(List.singleton((BigInt(1), BigInt(2))))):
            (m: SortedMap[BigInt, BigInt]) => m.mapValues(_ + 1)
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(2)),
              List.Cons((BigInt(2), BigInt(3)), List.Cons((BigInt(3), BigInt(4)), List.Nil))
            )
          )
        ): (m: SortedMap[BigInt, BigInt]) =>
            m.mapValues(_ + 1)
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
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

        assertEvalEq(SortedMap.empty[BigInt, BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            m.filter(_ => true)
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1))): (m: SortedMap[BigInt, BigInt]) =>
            m.filter(_._1 > 0)
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(
          SortedMap.fromStrictlyAscendingList(List.Cons((BigInt(3), BigInt(3)), List.Nil))
        ): (m: SortedMap[BigInt, BigInt]) =>
            m.filter(_._1 > 2)
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )

        assertEvalEq(SortedMap.empty[BigInt, BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            m.filter(_._1 < 0)
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )
    }

    test("filterNot") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.filterNot(predicate)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.filterNot(predicate))

            result === expected
        }

        assertEvalEq(SortedMap.empty[BigInt, BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            m.filterNot(_ => true)
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(SortedMap.empty[BigInt, BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            m.filterNot(_._1 > 0)
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
          )
        ): (m: SortedMap[BigInt, BigInt]) =>
            m.filterNot(_._1 > 2)
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )

        assertEvalEq(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        ): (m: SortedMap[BigInt, BigInt]) =>
            m.filterNot(_._1 < 0)
        .at(
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

        assertEvalEq(Option.None): (m: SortedMap[BigInt, BigInt]) =>
            m.find(_._1 === BigInt(1))
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(Option.Some((BigInt(1), BigInt(1)))): (m: SortedMap[BigInt, BigInt]) =>
            m.find(_._1 === BigInt(1))
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(Option.None): (m: SortedMap[BigInt, BigInt]) =>
            m.find(_._1 === BigInt(0))
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(Option.Some((BigInt(2), BigInt(2)))): (m: SortedMap[BigInt, BigInt]) =>
            m.find(_._1 === BigInt(2))
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )

        assertEvalEq(Option.None): (m: SortedMap[BigInt, BigInt]) =>
            m.find(_._1 === BigInt(4))
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
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

        assertEvalEq(Option.None): (m: SortedMap[BigInt, BigInt]) =>
            m.findMap { case (k, v) => Option.Some(v) }
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(Option.Some(BigInt(1))): (m: SortedMap[BigInt, BigInt]) =>
            m.findMap { case (k, v) =>
                if k === BigInt(1) then Option.Some(v) else Option.None
            }
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(Option.None): (m: SortedMap[BigInt, BigInt]) =>
            m.findMap { case (k, v) =>
                if k === BigInt(0) then Option.Some(v) else Option.None
            }
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(Option.Some(BigInt(2))): (m: SortedMap[BigInt, BigInt]) =>
            m.findMap { case (k, v) => if k === BigInt(2) then Option.Some(v) else Option.None }
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )

        assertEvalEq(Option.None): (m: SortedMap[BigInt, BigInt]) =>
            m.findMap { case (k, v) => if k === BigInt(4) then Option.Some(v) else Option.None }
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )
    }

    test("foldLeft") {
        check { (map: SortedMap[BigInt, BigInt], initial: BigInt) =>
            val result = map.foldLeft(initial) { case (acc, (k, v)) => acc + k + v }
            val expected = map.toList.foldLeft(initial) { case (acc, (k, v)) => acc + k + v }

            result === expected
        }

        assertEvalEq(BigInt(0)): (m: SortedMap[BigInt, BigInt]) =>
            m.foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v }
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(BigInt(2)): (m: SortedMap[BigInt, BigInt]) =>
            m.foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v }
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(BigInt(12)): (m: SortedMap[BigInt, BigInt]) =>
            m.foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v }
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )
    }

    test("foldRight") {
        check { (map: SortedMap[BigInt, BigInt], initial: BigInt) =>
            val result = map.foldRight(initial) { case ((k, v), acc) => acc + k + v }
            val expected = map.toList.foldRight(initial) { case ((k, v), acc) => acc + k + v }

            result === expected
        }

        assertEvalEq(BigInt(0)): (m: SortedMap[BigInt, BigInt]) =>
            m.foldRight(BigInt(0)) { case ((k, v), acc) => acc + k + v }
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(BigInt(2)): (m: SortedMap[BigInt, BigInt]) =>
            m.foldRight(BigInt(0)) { case ((k, v), acc) => acc + k + v }
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(BigInt(12)): (m: SortedMap[BigInt, BigInt]) =>
            m.foldRight(BigInt(0)) { case ((k, v), acc) => acc + k + v }
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
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

        assertEvalEq(Option.None): (m: SortedMap[BigInt, BigInt]) =>
            m.get(BigInt(1))
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(Option.Some(BigInt(1))): (m: SortedMap[BigInt, BigInt]) =>
            m.get(BigInt(1))
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(Option.None): (m: SortedMap[BigInt, BigInt]) =>
            m.get(BigInt(0))
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(Option.Some(BigInt(2))): (m: SortedMap[BigInt, BigInt]) =>
            m.get(BigInt(2))
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )

        assertEvalEq(Option.None): (m: SortedMap[BigInt, BigInt]) =>
            m.get(BigInt(4))
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
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

        assertEvalEq(BigInt(1)): (m: SortedMap[BigInt, BigInt]) =>
            m.getOrFail(BigInt(1))
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalFails[NoSuchElementException](
          SortedMap.singleton(BigInt(1), BigInt(1)).getOrFail(BigInt(0))
        )

        assertEvalEq(BigInt(2)): (m: SortedMap[BigInt, BigInt]) =>
            m.getOrFail(BigInt(2))
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
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

        assertEvalEq(BigInt(1)): (m: SortedMap[BigInt, BigInt]) =>
            m.at(BigInt(1))
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalFails[NoSuchElementException](
          SortedMap.singleton(BigInt(1), BigInt(1)).at(BigInt(0))
        )

        assertEvalEq(BigInt(2)): (m: SortedMap[BigInt, BigInt]) =>
            m.at(BigInt(2))
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
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
                SortedMap.fromList(
                  map.toList.filterNot(_._1 === key) ++ List.singleton((key, value))
                )

            result === expected
        }

        check { (map: SortedMap[BigInt, BigInt], key: BigInt, value: BigInt) =>
            map.insert(key, value).get(key) === Option.Some(value)
        }

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1))): (m: SortedMap[BigInt, BigInt]) =>
            m.insert(BigInt(1), BigInt(1))
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
        ): (m: SortedMap[BigInt, BigInt]) =>
            m.insert(BigInt(2), BigInt(2))
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
        ): (m: SortedMap[BigInt, BigInt]) =>
            m.insert(BigInt(2), BigInt(2))
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
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

        assertEvalEq(SortedMap.empty[BigInt, BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            m.delete(BigInt(1))
        .at(SortedMap.empty[BigInt, BigInt])

        assertEvalEq(SortedMap.empty[BigInt, BigInt]): (m: SortedMap[BigInt, BigInt]) =>
            m.delete(BigInt(1))
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1))): (m: SortedMap[BigInt, BigInt]) =>
            m.delete(BigInt(2))
        .at(SortedMap.singleton(BigInt(1), BigInt(1)))

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1))): (m: SortedMap[BigInt, BigInt]) =>
            m.delete(BigInt(2))
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
          )
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
        ): (m: SortedMap[BigInt, BigInt]) =>
            m.delete(BigInt(3))
        .at(
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
          )
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
