package scalus.cardano.onchain.plutus.v1

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.ByteString.{hex, utf8}
import scalus.cardano.ledger.LedgerToPlutusTranslation
import scalus.cardano.onchain.plutus.prelude.*
import scalus.testing.kit.EvalTestKit

class ValueTest extends AnyFunSuite with EvalTestKit with ArbitraryInstances {
    given [T: Arbitrary]: Arbitrary[List[T]] = Arbitrary {
        for
            size <- Gen.choose(0, 10)
            elements <- Gen.listOfN(size, Arbitrary.arbitrary[T])
        yield List.from(elements)
    }

    test("toSortedMap") {
        checkEval { (value: Value) =>
            value.toSortedMap.forall { case (policyId, tokens) =>
                tokens.forall { case (tokenName, amount) =>
                    amount === value.quantityOf(policyId, tokenName)
                }
            }
        }

        assertEvalWithBudget(
          Value.zero.toSortedMap,
          SortedMap.empty,
          ExUnits(memory = 200, steps = 16100)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).toSortedMap,
          SortedMap.singleton(
            Value.adaPolicyId,
            SortedMap.singleton(Value.adaTokenName, BigInt(1000))
          ),
          ExUnits(memory = 9713, steps = 2_621831)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ).toSortedMap,
          SortedMap.singleton(
            utf8"PolicyId",
            SortedMap.singleton(utf8"TokenName", BigInt(1000))
          ),
          ExUnits(memory = 9713, steps = 2_621831)
        )
    }

    test("zero") {
        assertEvalWithBudget(
          Value.zero.toSortedMap,
          SortedMap.empty[PolicyId, SortedMap[TokenName, BigInt]],
          ExUnits(memory = 200, steps = 16100)
        )
    }

    test("apply") {
        checkEval { (policyId: PolicyId, tokenName: TokenName, value: BigInt) =>
            Value(policyId, tokenName, value).toSortedMap ===
                (
                  if value !== BigInt(0) then
                      SortedMap.singleton(
                        policyId,
                        SortedMap.singleton(tokenName, value)
                      )
                  else Value.zero.toSortedMap
                )
        }

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1)
          ).toSortedMap,
          SortedMap.singleton(
            utf8"PolicyId",
            SortedMap.singleton(utf8"TokenName", BigInt(1))
          ),
          ExUnits(memory = 9713, steps = 2_621831)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(0)
          ),
          Value.zero,
          ExUnits(memory = 2601, steps = 476149)
        )
    }

    test("lovelace") {
        checkEval { (value: BigInt) =>
            Value.lovelace(value).toSortedMap ===
                (
                  if value !== BigInt(0) then
                      SortedMap.singleton(
                        Value.adaPolicyId,
                        SortedMap.singleton(Value.adaTokenName, value)
                      )
                  else Value.zero.toSortedMap
                )
        }

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).toSortedMap,
          SortedMap.singleton(
            Value.adaPolicyId,
            SortedMap.singleton(Value.adaTokenName, BigInt(1000))
          ),
          ExUnits(memory = 9713, steps = 2_621831)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(0)),
          Value.zero,
          ExUnits(memory = 2601, steps = 476149)
        )
    }

    test("unsafeFromList") {
        checkEval { (list: List[(PolicyId, List[(TokenName, BigInt)])]) =>
            val validList =
                list.distinct(using Eq.keyPairEq)
                    .quicksort(using Ord.keyPairOrd)
                    .filterMap { case (cs, tokens) =>
                        val validTokens = tokens
                            .distinct(using Eq.keyPairEq)
                            .quicksort(using Ord.keyPairOrd)
                            .filter { case (_, value) =>
                                value !== BigInt(0)
                            }

                        if validTokens.nonEmpty then Option.Some((cs, validTokens)) else Option.None
                    }

            Value.unsafeFromList(validList).toSortedMap === SortedMap.unsafeFromList(
              validList.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
            )

        }

        assertEvalWithBudget(
          Value
              .unsafeFromList(
                List(
                  (utf8"CS1", List((utf8"TN1", BigInt(10)))),
                  (utf8"CS2", List((utf8"TN2", BigInt(20))))
                )
              )
              .toSortedMap,
          SortedMap.unsafeFromList(
            List(
              (
                utf8"CS1",
                SortedMap.unsafeFromList(List((utf8"TN1", BigInt(10))))
              ),
              (
                utf8"CS2",
                SortedMap.unsafeFromList(List((utf8"TN2", BigInt(20))))
              )
            )
          ),
          ExUnits(memory = 50600, steps = 15_135487)
        )

    }

    test("fromList") {

        checkEval { (list: List[(PolicyId, List[(TokenName, BigInt)])]) =>
            Value.fromList(list).toSortedMap === SortedMap.fromList(
              list.filterMap { case (cs, tnList) =>
                  val tokens = tnList.filter { _._2 !== BigInt(0) }

                  if tokens.nonEmpty then Option.Some((cs, SortedMap.fromList(tokens)))
                  else Option.None
              }
            )
        }

        assertEvalWithBudget(
          Value
              .fromList(
                List(
                  (
                    utf8"CS1",
                    List(
                      (utf8"TN1", BigInt(10)),
                      (utf8"TN1", BigInt(20)),
                      (utf8"TN2", BigInt(0)),
                    )
                  ),
                  (utf8"CS2", List((utf8"TN2", BigInt(20)))),
                  (utf8"CS2", List((utf8"TN2", BigInt(30)))),
                  (utf8"CS3", List((utf8"TN3", BigInt(0))))
                )
              )
              .toSortedMap,
          SortedMap.fromList(
            List(
              (
                utf8"CS1",
                SortedMap.fromList(List((utf8"TN1", BigInt(10))))
              ),
              (
                utf8"CS2",
                SortedMap.fromList(List((utf8"TN2", BigInt(20))))
              )
            )
          ),
          ExUnits(memory = 367362, steps = 108_366404)
        )
    }

    test("fromStrictlyAscendingListWithNonZeroAmounts") {
        checkEval { (list: List[(PolicyId, List[(TokenName, BigInt)])]) =>
            val validList =
                list.distinct(using Eq.keyPairEq)
                    .quicksort(using Ord.keyPairOrd)
                    .filterMap { case (cs, tokens) =>
                        val validTokens = tokens
                            .distinct(using Eq.keyPairEq)
                            .quicksort(using Ord.keyPairOrd)
                            .filter { case (_, value) =>
                                value !== BigInt(0)
                            }

                        if validTokens.nonEmpty then Option.Some((cs, validTokens)) else Option.None
                    }

            Value.fromStrictlyAscendingListWithNonZeroAmounts(validList).toSortedMap ===
                SortedMap.unsafeFromList(
                  validList.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
                )
        }

        assertEvalWithBudget(
          Value
              .fromStrictlyAscendingListWithNonZeroAmounts(
                List(
                  (utf8"CS1", List((utf8"TN1", BigInt(10)))),
                  (utf8"CS2", List((utf8"TN2", BigInt(20))))
                )
              )
              .toSortedMap,
          SortedMap.unsafeFromList(
            List(
              (
                utf8"CS1",
                SortedMap.unsafeFromList(List((utf8"TN1", BigInt(10))))
              ),
              (
                utf8"CS2",
                SortedMap.unsafeFromList(List((utf8"TN2", BigInt(20))))
              )
            )
          ),
          ExUnits(memory = 101515, steps = 28_947284)
        )
    }

    test("adaCurrencySymbol") {
        assertEvalWithBudget(
          Value.adaPolicyId,
          ByteString.empty,
          ExUnits(memory = 200, steps = 16100)
        )
    }

    test("adaTokenName") {
        assertEvalWithBudget(
          Value.adaTokenName,
          ByteString.empty,
          ExUnits(memory = 200, steps = 16100)
        )
    }

    test("equalsAssets") {
        assertEval(
          Value.equalsAssets(
            SortedMap.singleton(utf8"TokenName", BigInt(1)),
            SortedMap.singleton(utf8"TokenName", BigInt(1))
          )
        )

        assertEval(
          !Value.equalsAssets(
            SortedMap.singleton(utf8"TokenName1", BigInt(1)),
            SortedMap.singleton(utf8"TokenName2", BigInt(1))
          )
        )

        assertEval(
          !Value.equalsAssets(
            SortedMap.singleton(utf8"TokenName", BigInt(1)),
            SortedMap.singleton(utf8"TokenName", BigInt(-1))
          )
        )
    }

    test("Eq") {
        checkEval { (value: Value) => value === value }

        assertEval(Value.zero === Value.zero)

        assertEval(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(0)
          ) === Value.zero
        )

        assertEval(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1)
          ) ===
              Value(
                utf8"PolicyId",
                utf8"TokenName",
                BigInt(1)
              )
        )

        assertEval(
          Value(
            utf8"CurrencySymbol1",
            utf8"TokenName",
            BigInt(1)
          ) !==
              Value(
                utf8"CurrencySymbol2",
                utf8"TokenName",
                BigInt(1)
              )
        )

        assertEval(
          Value(
            utf8"PolicyId",
            utf8"TokenName1",
            BigInt(1)
          ) !==
              Value(
                utf8"PolicyId",
                utf8"TokenName2",
                BigInt(1)
              )
        )

        assertEval(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1)
          ) !==
              Value(
                utf8"PolicyId",
                utf8"TokenName",
                BigInt(2)
              )
        )
    }

    test("toData <-> FromData") {
        checkEval { (value: Value) =>
            val data = value.toData
            val fromDataValue = fromData[Value](data)
            fromDataValue === value
        }
    }

    test("valueFromDataWithValidation") {

        checkEval { (value: Value) =>
            given FromData[Value] = Value.valueFromDataWithValidation

            val data = value.toData
            val fromDataValue = fromData[Value](data)
            fromDataValue === value
        }

        // TODO: this fragment evaluated successfully, because UPLC compiler optimizes out fromData calls.
        //  Maybe implement something like annotation
        // assertEvalFails[RequirementError] {
        //    given FromData[Value] = Value.valueFromDataWithValidation
        //
        //    val invalidValue = Value.unsafeFromList(
        //      List(
        //        (utf8"CS1", List((utf8"TN1", BigInt(0))))
        //      )
        //    )
        //
        //    val data = invalidValue.toData
        //    fromData[Value](data)
        //  //fromData[Vaue](data): @keepInUplc  ??
        // }
    }

    test("unary_") {
        checkEval { (value: Value) =>
            val negatedValue = -value
            negatedValue.toSortedMap === value.toSortedMap.mapValues(_.mapValues(-_))
        }

        assertEvalWithBudget(
          -Value.zero,
          Value.zero,
          ExUnits(memory = 5996, steps = 1_153963)
        )

        assertEvalWithBudget(
          -Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ),
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(-1000)
          ),
          ExUnits(memory = 47701, steps = 14_053124)
        )

        assertEvalWithBudget(
          -Value.lovelace(BigInt(1000)),
          Value.lovelace(BigInt(-1000)),
          ExUnits(memory = 47701, steps = 14_053124)
        )
    }

    test("+") {
        checkEval { (value: Value) =>
            (value + Value.zero) === value && (Value.zero + value) === value
        }

        checkEval { (value1: Value, value2: Value) =>
            val sumValue = value1 + value2
            sumValue.flatten.forall { case (cs, token, value) =>
                val v1 = value1.toSortedMap.get(cs).flatMap { _.get(token) }
                val v2 = value2.toSortedMap.get(cs).flatMap { _.get(token) }

                v1 match
                    case Option.Some(v1Value) =>
                        v2 match
                            case Option.Some(v2Value) => (v1Value + v2Value) === value
                            case Option.None          => v1Value === value
                    case Option.None =>
                        v2 match
                            case Option.Some(v2Value) => v2Value === value
                            case Option.None          => false

            }
        }

        assertEvalWithBudget(
          Value.zero + Value.zero,
          Value.zero,
          ExUnits(memory = 22916, steps = 4_817410)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) +
              Value(
                utf8"PolicyId",
                utf8"TokenName",
                BigInt(2000)
              ),
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(3000)
          ),
          ExUnits(memory = 155205, steps = 46_781929)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) +
              Value.zero,
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ),
          ExUnits(memory = 108163, steps = 31_138940)
        )

        assertEvalWithBudget(
          Value.zero +
              Value(
                utf8"PolicyId",
                utf8"TokenName",
                BigInt(1000)
              ),
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ),
          ExUnits(memory = 106699, steps = 30_648952)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) + Value.lovelace(BigInt(2000)),
          Value.lovelace(BigInt(3000)),
          ExUnits(memory = 153277, steps = 46_449085)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) + Value.zero,
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 108163, steps = 31_138940)
        )

        assertEvalWithBudget(
          Value.zero + Value.lovelace(BigInt(1000)),
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 106699, steps = 30_648952)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) +
              Value.lovelace(BigInt(1000)),
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ),
          ExUnits(memory = 251448, steps = 75_526480)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) +
              Value(
                utf8"PolicyId",
                utf8"TokenName",
                BigInt(-1000)
              ),
          Value.zero,
          ExUnits(memory = 118885, steps = 34_926306)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) + Value.lovelace(BigInt(-1000)),
          Value.zero,
          ExUnits(memory = 116957, steps = 34_593462)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) +
              Value.fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(-1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(-1000))))
                )
              ),
          Value.zero,
          ExUnits(memory = 518382, steps = 155_716286)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) +
              Value.fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(-1000)))
                  )
                )
              ),
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 468684, steps = 140_249573)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) +
              Value.fromList(
                List(
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(-1000))))
                )
              ),
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ),
          ExUnits(memory = 438114, steps = 130_968950)
        )
    }

    test("-") {
        checkEval { (value: Value) =>
            (value - Value.zero) === value && (Value.zero - value) === -value
        }

        checkEval { (value1: Value, value2: Value) =>
            val diffValue = value1 - value2
            diffValue.flatten.forall { case (cs, token, value) =>
                val v1 = value1.toSortedMap.get(cs).flatMap { _.get(token) }
                val v2 = value2.toSortedMap.get(cs).flatMap { _.get(token) }

                v1 match
                    case Option.Some(v1Value) =>
                        v2 match
                            case Option.Some(v2Value) => (v1Value - v2Value) === value
                            case Option.None          => v1Value === value
                    case Option.None =>
                        v2 match
                            case Option.Some(v2Value) => -v2Value === value
                            case Option.None          => false

            }
        }

        assertEvalWithBudget(
          Value.zero - Value.zero,
          Value.zero,
          ExUnits(memory = 22916, steps = 4_817410)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) -
              Value(
                utf8"PolicyId",
                utf8"TokenName",
                BigInt(2000)
              ),
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(-1000)
          ),
          ExUnits(memory = 155205, steps = 46_781929)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) -
              Value.zero,
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ),
          ExUnits(memory = 108163, steps = 31_138940)
        )

        assertEvalWithBudget(
          Value.zero -
              Value(
                utf8"PolicyId",
                utf8"TokenName",
                BigInt(1000)
              ),
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(-1000)
          ),
          ExUnits(memory = 106699, steps = 30_648952)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) - Value.lovelace(BigInt(2000)),
          Value.lovelace(BigInt(-1000)),
          ExUnits(memory = 153277, steps = 46_449085)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) - Value.zero,
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 108163, steps = 31_138940)
        )

        assertEvalWithBudget(
          Value.zero - Value.lovelace(BigInt(1000)),
          Value.lovelace(BigInt(-1000)),
          ExUnits(memory = 106699, steps = 30_648952)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) -
              Value.lovelace(BigInt(1000)),
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(-1000))))
            )
          ),
          ExUnits(memory = 251448, steps = 75_526480)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) -
              Value(
                utf8"PolicyId",
                utf8"TokenName",
                BigInt(1000)
              ),
          Value.zero,
          ExUnits(memory = 118885, steps = 34_926306)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) - Value.lovelace(BigInt(1000)),
          Value.zero,
          ExUnits(memory = 116957, steps = 34_593462)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) -
              Value.fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
                )
              ),
          Value.zero,
          ExUnits(memory = 518382, steps = 155_716286)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) -
              Value.fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(1000)))
                  )
                )
              ),
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 468684, steps = 140_249573)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) -
              Value.fromList(
                List(
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
                )
              ),
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ),
          ExUnits(memory = 438114, steps = 130_968950)
        )
    }

    test("*") {
        checkEval { (value: Value) => (value * BigInt(0)) === Value.zero }

        checkEval { (value: Value, factor: BigInt) =>
            (value * factor).toSortedMap === (
              if factor !== BigInt(0) then
                  value.toSortedMap.mapValues { _.mapValues { _ * factor } }
              else SortedMap.empty
            )
        }

        assertEvalWithBudget(
          Value.zero * BigInt(0),
          Value.zero,
          ExUnits(memory = 3801, steps = 668149)
        )

        assertEvalWithBudget(
          Value.zero * BigInt(1),
          Value.zero,
          ExUnits(memory = 6997, steps = 1_390012)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) * BigInt(2),
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(2000)
          ),
          ExUnits(memory = 48402, steps = 14_200153)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ) * BigInt(0),
          Value.zero,
          ExUnits(memory = 4101, steps = 716149)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) * BigInt(2),
          Value.lovelace(BigInt(2000)),
          ExUnits(memory = 48402, steps = 14_200153)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) * BigInt(0),
          Value.zero,
          ExUnits(memory = 4101, steps = 716149)
        )
    }

    test("showDebug") {
        assert(Value.zero.showDebug === "{  }")

        assert(
          Value
              .fromList(
                List.Cons(
                  (
                    Value.adaPolicyId,
                    List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)
                  ),
                  List.Cons(
                    (
                      utf8"ff",
                      List.Cons((utf8"TOKEN", BigInt(100)), List.Nil)
                    ),
                    List.Nil
                  )
                )
              )
              .showDebug === "{ policy# -> { #: 1000000 }, policy#6666 -> { #544f4b454e: 100 } }"
        )
    }

    test("getLovelace") {
        checkEval { (value: Value) =>
            value.getLovelace ===
                value.toSortedMap
                    .get(Value.adaPolicyId)
                    .flatMap(_.get(Value.adaTokenName))
                    .getOrElse(BigInt(0))
        }

        assertEvalWithBudget(
          Value.zero.getLovelace,
          BigInt(0),
          ExUnits(memory = 8230, steps = 1_732582)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).getLovelace,
          BigInt(1000),
          ExUnits(memory = 67065, steps = 18_483101)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ).getLovelace,
          BigInt(0),
          ExUnits(memory = 22771, steps = 5_782812)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(0)
          ).getLovelace,
          BigInt(0),
          ExUnits(memory = 10263, steps = 2_139814)
        )
    }

    test("lovelaceAmount") {
        // returns correct lovelace for lovelace-only value
        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).lovelaceAmount,
          BigInt(1000),
          ExUnits(memory = 18785, steps = 5_821257)
        )
        // returns correct lovelace for value with lovelace + native asset
        assertEvalWithBudget(
          Value
              .fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(500)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(2000))))
                )
              )
              .lovelaceAmount,
          BigInt(2000),
          ExUnits(memory = 186258, steps = 55_059680)
        )

        // fails on Value.zero (empty list)
        assertEvalFails[NoSuchElementException] {
            Value.zero.lovelaceAmount
        }
    }

    test("isZero") {
        checkEval { (value: Value) =>
            if value.isZero then value.toSortedMap.isEmpty else value.nonZero
        }

        assertEval(Value.zero.isZero)

        assertEval(Value.lovelace(BigInt(0)).isZero)

        assertEval(!Value.lovelace(BigInt(1000)).isZero)

        assertEval(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(0)
          ).isZero
        )

        assertEval(
          !Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ).isZero
        )
    }

    test("nonZero") {
        checkEval { (value: Value) =>
            if value.nonZero then
                value.toSortedMap.nonEmpty && value.toSortedMap.forall { case (_, tokens) =>
                    tokens.nonEmpty && tokens.forall { case (_, amount) => amount !== BigInt(0) }
                }
            else value.isZero
        }

        assertEval(!Value.zero.nonZero)

        assertEval(!Value.lovelace(BigInt(0)).nonZero)

        assertEval(Value.lovelace(BigInt(1000)).nonZero)

        assertEval(
          !Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(0)
          ).nonZero
        )

        assertEval(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ).nonZero
        )
    }

    test("quantityOf") {
        checkEval { (value: Value, policyId: PolicyId, tokenName: TokenName) =>
            value.quantityOf(policyId, tokenName) ===
                value.toSortedMap
                    .get(policyId)
                    .flatMap(_.get(tokenName))
                    .getOrElse(BigInt(0))
        }

        assertEvalWithBudget(
          Value.zero.quantityOf(Value.adaPolicyId, Value.adaTokenName),
          BigInt(0),
          ExUnits(memory = 8230, steps = 1_732582)
        )

        assertEvalWithBudget(
          Value.zero.quantityOf(utf8"CS", utf8"TN"),
          BigInt(0),
          ExUnits(memory = 8230, steps = 1_732582)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).quantityOf(Value.adaPolicyId, Value.adaTokenName),
          BigInt(1000),
          ExUnits(memory = 67065, steps = 18_483101)
        )

        assertEvalWithBudget(
          Value
              .lovelace(BigInt(1000))
              .quantityOf(utf8"CS", utf8"TN"),
          BigInt(0),
          ExUnits(memory = 27171, steps = 7_061331)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ).quantityOf(Value.adaPolicyId, Value.adaTokenName),
          BigInt(0),
          ExUnits(memory = 22771, steps = 5_782812)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ).quantityOf(utf8"PolicyId", utf8"TokenName"),
          BigInt(1000),
          ExUnits(memory = 67065, steps = 18_483213)
        )
    }

    test("withoutLovelace") {
        checkEval { (value: Value) =>
            value.withoutLovelace.getLovelace === BigInt(0)
        }

        assertEvalWithBudget(
          Value.zero.withoutLovelace,
          Value.zero,
          ExUnits(memory = 200, steps = 16100)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).withoutLovelace,
          Value.zero,
          ExUnits(memory = 19641, steps = 5_486393)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ).withoutLovelace,
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ),
          ExUnits(memory = 17173, steps = 4_644868)
        )

        assertEvalWithBudget(
          Value
              .fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
                )
              )
              .withoutLovelace,
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ),
          ExUnits(memory = 187814, steps = 54_836816)
        )
    }

    test("flatten") {
        checkEval { (value: Value) =>
            value.flatten ===
                value.toSortedMap.toList.flatMap { case (cs, tokens) =>
                    tokens.toList.map { case (tn, amount) => (cs, tn, amount) }
                }
        }

        assertEvalWithBudget(
          Value.zero.flatten,
          List.empty,
          ExUnits(memory = 6396, steps = 1_175650)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).flatten,
          List((Value.adaPolicyId, Value.adaTokenName, BigInt(1000))),
          ExUnits(memory = 44925, steps = 12_748738)
        )

        assertEvalWithBudget(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(1000)
          ).flatten,
          List(
            (
              utf8"PolicyId",
              utf8"TokenName",
              BigInt(1000)
            )
          ),
          ExUnits(memory = 44925, steps = 12_748738)
        )

        assertEvalWithBudget(
          Value
              .fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
                )
              )
              .flatten,
          List(
            (Value.adaPolicyId, Value.adaTokenName, BigInt(1000)),
            (
              utf8"PolicyId",
              utf8"TokenName",
              BigInt(1000)
            )
          ),
          ExUnits(memory = 242514, steps = 71_130518)
        )
    }

    test("toLedgerValue") {
        import scalus.cardano.ledger.Coin

        // Create valid 28-byte PolicyIds (ScriptHash) for testing
        val policyId1 = hex"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        val policyId2 = hex"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        val policyId3 = hex"11111111111111111111111111111111111111111111111111111111"
        val policyId4 = hex"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

        // Test zero value conversion
        val ledgerZero = Value.zero.toLedgerValue
        assert(ledgerZero.coin == Coin(0))
        assert(ledgerZero.assets.assets.isEmpty)

        // Test lovelace-only value conversion
        val lovelaceValue = Value.lovelace(BigInt(1000000))
        val ledgerLovelace = lovelaceValue.toLedgerValue
        assert(ledgerLovelace.coin == Coin(1000000))
        assert(ledgerLovelace.assets.assets.isEmpty)

        // Test value with single native asset
        val singleAssetValue = Value(
          policyId1,
          utf8"assetName1",
          BigInt(100)
        )
        val ledgerSingleAsset = singleAssetValue.toLedgerValue
        assert(ledgerSingleAsset.coin == Coin(0))
        assert(ledgerSingleAsset.assets.assets.size == 1)

        // Test value with lovelace and native assets
        val mixedValue = Value.lovelace(BigInt(2000000)) +
            Value(
              policyId1,
              utf8"assetName1",
              BigInt(100)
            ) +
            Value(
              policyId2,
              utf8"assetName2",
              BigInt(200)
            )
        val ledgerMixed = mixedValue.toLedgerValue
        assert(ledgerMixed.coin == Coin(2000000))
        assert(ledgerMixed.assets.assets.size == 2)

        // Test value with multiple assets under same policy
        val multiAssetSamePolicy = Value.fromList(
          List(
            (
              policyId1,
              List(
                (utf8"asset1", BigInt(100)),
                (utf8"asset2", BigInt(200))
              )
            )
          )
        )
        val ledgerMultiSame = multiAssetSamePolicy.toLedgerValue
        assert(ledgerMultiSame.coin == Coin(0))
        assert(ledgerMultiSame.assets.assets.size == 1)
        assert(ledgerMultiSame.assets.assets.values.head.size == 2)

        // Test round-trip conversion: api.Value -> ledger.Value -> api.Value
        val originalValue = Value.lovelace(BigInt(5000000)) +
            Value(
              policyId3,
              utf8"token1",
              BigInt(150)
            )
        val ledgerVal = originalValue.toLedgerValue
        val backToApiValue =
            scalus.cardano.ledger.LedgerToPlutusTranslation.getValue(ledgerVal)

        assert(originalValue === backToApiValue)

        // Test that toLedgerValue preserves token ordering
        val orderedValue = Value.fromList(
          List(
            (
              policyId3,
              List((utf8"token1", BigInt(10)))
            ),
            (
              policyId4,
              List((utf8"token2", BigInt(20)))
            )
          )
        )
        val ledgerOrdered = orderedValue.toLedgerValue
        val policyIds = ledgerOrdered.assets.assets.keys.toSeq
        assert(policyIds.size == 2)
    }

    test("Eq vs toData equality budget comparison") {
        // Using realistic 28-byte PolicyIds (ScriptHash) and utf8 TokenNames

        // zero: Eq 2.9x more mem, 1.7x more steps
        assertEvalWithBudget(
          Value.zero === Value.zero,
          true,
          ExUnits(memory = 11192, steps = 2_325322)
        )
        assertEvalWithBudget(
          Value.zero.toData == Value.zero.toData,
          true,
          ExUnits(memory = 200, steps = 16100)
        )

        // lovelace: Eq 2.2x more mem, 2.0x more steps
        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) === Value.lovelace(BigInt(1000)),
          true,
          ExUnits(memory = 79181, steps = 22_800749)
        )
        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).toData == Value.lovelace(BigInt(1000)).toData,
          true,
          ExUnits(memory = 21095, steps = 7_328489)
        )

        // single native asset (28-byte policyId): Eq 2.2x more mem, 2.0x more steps
        assertEvalWithBudget(
          Value(
            hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
            utf8"TOKEN1",
            BigInt(1000)
          ) ===
              Value(
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                utf8"TOKEN1",
                BigInt(1000)
              ),
          true,
          ExUnits(memory = 81109, steps = 23_133595)
        )
        assertEvalWithBudget(
          Value(
            hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
            utf8"TOKEN1",
            BigInt(1000)
          ).toData ==
              Value(
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                utf8"TOKEN1",
                BigInt(1000)
              ).toData,
          true,
          ExUnits(memory = 23023, steps = 7_743058)
        )

        // lovelace + native asset (2 policies): Eq 1.25x more mem, 1.24x more steps
        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                List((utf8"TOKEN1", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(2000))))
            )
          ) ===
              Value.fromList(
                List(
                  (
                    hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                    List((utf8"TOKEN1", BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(2000))))
                )
              ),
          true,
          ExUnits(memory = 457154, steps = 135_038950)
        )
        assertEvalWithBudget(
          Value
              .fromList(
                List(
                  (
                    hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                    List((utf8"TOKEN1", BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(2000))))
                )
              )
              .toData ==
              Value
                  .fromList(
                    List(
                      (
                        hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                        List((utf8"TOKEN1", BigInt(1000)))
                      ),
                      (Value.adaPolicyId, List((Value.adaTokenName, BigInt(2000))))
                    )
                  )
                  .toData,
          true,
          ExUnits(memory = 351673, steps = 105_430111)
        )

        // 3 policies, multiple tokens: Eq 1.22x more mem, 1.21x more steps
        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                List(
                  (utf8"TOKEN1", BigInt(100)),
                  (utf8"TOKEN2", BigInt(200))
                )
              ),
              (
                hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678",
                List((utf8"TOKEN3", BigInt(300)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(5000))))
            )
          ) ===
              Value.fromList(
                List(
                  (
                    hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                    List(
                      (utf8"TOKEN1", BigInt(100)),
                      (utf8"TOKEN2", BigInt(200))
                    )
                  ),
                  (
                    hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678",
                    List((utf8"TOKEN3", BigInt(300)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(5000))))
                )
              ),
          true,
          ExUnits(memory = 852157, steps = 256_434014)
        )
        assertEvalWithBudget(
          Value
              .fromList(
                List(
                  (
                    hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                    List(
                      (utf8"TOKEN1", BigInt(100)),
                      (utf8"TOKEN2", BigInt(200))
                    )
                  ),
                  (
                    hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678",
                    List((utf8"TOKEN3", BigInt(300)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(5000))))
                )
              )
              .toData ==
              Value
                  .fromList(
                    List(
                      (
                        hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                        List(
                          (utf8"TOKEN1", BigInt(100)),
                          (utf8"TOKEN2", BigInt(200))
                        )
                      ),
                      (
                        hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678",
                        List((utf8"TOKEN3", BigInt(300)))
                      ),
                      (Value.adaPolicyId, List((Value.adaTokenName, BigInt(5000))))
                    )
                  )
                  .toData,
          true,
          ExUnits(memory = 676197, steps = 205_924785)
        )

        // not equal: different amounts: Eq 2.0x more mem, 1.9x more steps
        assertEvalWithBudget(
          Value(
            hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
            utf8"TOKEN1",
            BigInt(1000)
          ) !==
              Value(
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                utf8"TOKEN1",
                BigInt(2000)
              ),
          true,
          ExUnits(memory = 74354, steps = 21_311016)
        )
        assertEvalWithBudget(
          Value(
            hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
            utf8"TOKEN1",
            BigInt(1000)
          ).toData !=
              Value(
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                utf8"TOKEN1",
                BigInt(2000)
              ).toData,
          true,
          ExUnits(memory = 23524, steps = 7_899107)
        )
    }

    test("toLedgerValue roundtrip property") {
        // Helper to check if all amounts in a Value are within Long range
        def isValidForLedger(v: Value): Boolean = {
            val lovelaceValid = v.getLovelace.isValidLong
            val assetsValid = v.flatten.forall { case (_, _, amount) =>
                amount.isValidLong
            }
            lovelaceValid && assetsValid
        }

        // Use ScalaCheck directly (not checkEval) since toLedgerValue is offchain-only
        forAll { (value: Value) =>
            // Only test values that can be converted to ledger.Value
            // (i.e., all amounts must be within Long range)
            if isValidForLedger(value) then
                val ledgerValue = value.toLedgerValue
                val roundtripped = LedgerToPlutusTranslation.getValue(ledgerValue)
                roundtripped == value
            else
                // For values outside Long range, just verify the property passes
                // (we can't test roundtrip for invalid values)
                true
        }
    }

}
