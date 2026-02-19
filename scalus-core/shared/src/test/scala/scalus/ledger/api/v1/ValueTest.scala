package scalus.cardano.onchain.plutus.v1

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
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
          ExUnits(memory = 4464, steps = 889340)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).toSortedMap,
          SortedMap.singleton(
            Value.adaPolicyId,
            SortedMap.singleton(Value.adaTokenName, BigInt(1000))
          ),
          ExUnits(memory = 35979, steps = 8936366)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).toSortedMap,
          SortedMap.singleton(
            ByteString.fromString("PolicyId"),
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1000))
          ),
          ExUnits(memory = 34779, steps = 8744366)
        )
    }

    test("zero") {
        assertEvalWithBudget(
          Value.zero.toSortedMap,
          SortedMap.empty[PolicyId, SortedMap[TokenName, BigInt]],
          ExUnits(memory = 4464, steps = 889340)
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
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ).toSortedMap,
          SortedMap.singleton(
            ByteString.fromString("PolicyId"),
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1))
          ),
          ExUnits(memory = 34779, steps = 8744366)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ),
          Value.zero,
          ExUnits(memory = 11931, steps = 2300137)
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
          ExUnits(memory = 35979, steps = 8936366)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(0)),
          Value.zero,
          ExUnits(memory = 13131, steps = 2492137)
        )
    }

    test("unsafeFromList") {

        val sir = scalus.compiler.compile {
            // (list: List[(PolicyId, List[(TokenName, BigInt)])]) =>
            (d: Data) =>
                import scalus.cardano.onchain.plutus.prelude.*
                val list = Data.fromData[
                  scalus.cardano.onchain.plutus.prelude.List[
                    (PolicyId, scalus.cardano.onchain.plutus.prelude.List[(TokenName, BigInt)])
                  ]
                ](d)
                // val validList = list
                // list.distinct(using Eq.keyPairEq)
                /* .quicksort(using Ord.keyPairOrd)
                        .filterMap { case (cs, tokens) =>
                            val validTokens = tokens
                                .distinct(using Eq.keyPairEq)
                                .quicksort(using Ord.keyPairOrd)
                                .filter { case (_, value) =>
                                    value !== BigInt(0)
                                }

                            if validTokens.nonEmpty then Option.Some((cs, validTokens))
                            else Option.None
                        }
                 */
                // list.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
                // val (cs, tnList) = list.head
                list.headOption match {
                    case Option.Some(v) =>
                        log("negora assign")
                        // val (cs, tnList) = v
                        v match
                            case (cs, tnList) =>
                                log("parsed pair")
                                val q = (cs, tnList)
                                log("after assign")
                            case _ =>
                                log("not a pair")
                        log("after assign")
                    case Option.None =>
                        throw new RuntimeException("List is empty, cannot extract head")
                }
                // SortedMap.unsafeFromList(tnList)
                /*
                Value.unsafeFromList(validList).toSortedMap === SortedMap.unsafeFromList(
                  validList.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
                )

                 */

        }
        /*
        import scalus.*
        import scalus.cardano.onchain.plutus.prelude.*
        import scalus.uplc.*

        println(s"sir=${sir.pretty.render(100)}")

        val lw = sir.toLoweredValue()
        println(s"lwe=${lw.pretty.render(100)}")

        val uplc = sir.toUplc()
        println(s"uplc=${uplc.pretty.render(100)}")

        val listFromDataSir = scalus.compiler.compile { (x: Data) =>
            Data.fromData[
              scalus.cardano.onchain.plutus.prelude.List[(PolicyId, scalus.cardano.onchain.plutus.prelude.List[(TokenName, BigInt)])]
            ](x)
        }

        val arg0 =
            List.Cons(
              (
                "fc7f1f7f1380d97aff4eff207fabdcffbe7f80f4012300ec807a7fbf9701017fffb4347f7fc401",
                List.Cons(
                  ("000e01628001", BigInt(104929086447450L)),
                  List.Cons(("fbff7f011b530100ff8a01a3", BigInt(-173061477122556L)), List.Nil)
                )
              ),
              List.Nil
            )
        val arg0Data = arg0.toData
        println(s"arg0Data=${arg0Data}")

        val arg0DataUplc = listFromDataSir.toUplc() $ Term.Const(Constant.Data(arg0Data))

        val arg0DataTerm = arg0DataUplc.evaluateDebug match {
            case Success(term, budget, costs, logs) =>
                term
            case Failure(ex, budget, cost, logs) =>
                println(s"UPLC evaluation failed (1): $ex")
                println(s"logs: $logs")
                assert(false, "UPLC evaluation failed")
                throw ex
        }

        // val uplcWithArg = uplc $ arg0DataTerm
        val uplcWithArg = uplc $ Term.Const(Constant.Data(arg0Data))
        // println(s"uplcWithArg= ${uplcWithArg.pretty.render(100)}")

        val result = uplcWithArg.evaluateDebug match {
            case Success(term, budget, costs, logs) =>
                term
            case Failure(ex, budget, cost, logs) =>
                println(s"UPLC evaluation failed (2): $ex")
                println(s"logs: $logs")
                assert(false, "UPLC evaluation failed")
        }


         */

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
                  (ByteString.fromString("CS1"), List((ByteString.fromString("TN1"), BigInt(10)))),
                  (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(20))))
                )
              )
              .toSortedMap,
          SortedMap.unsafeFromList(
            List(
              (
                ByteString.fromString("CS1"),
                SortedMap.unsafeFromList(List((ByteString.fromString("TN1"), BigInt(10))))
              ),
              (
                ByteString.fromString("CS2"),
                SortedMap.unsafeFromList(List((ByteString.fromString("TN2"), BigInt(20))))
              )
            )
          ),
          ExUnits(memory = 72388, steps = 19087813)
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
                    ByteString.fromString("CS1"),
                    List(
                      (ByteString.fromString("TN1"), BigInt(10)),
                      (ByteString.fromString("TN1"), BigInt(20)),
                      (ByteString.fromString("TN2"), BigInt(0)),
                    )
                  ),
                  (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(20)))),
                  (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(30)))),
                  (ByteString.fromString("CS3"), List((ByteString.fromString("TN3"), BigInt(0))))
                )
              )
              .toSortedMap,
          SortedMap.fromList(
            List(
              (
                ByteString.fromString("CS1"),
                SortedMap.fromList(List((ByteString.fromString("TN1"), BigInt(10))))
              ),
              (
                ByteString.fromString("CS2"),
                SortedMap.fromList(List((ByteString.fromString("TN2"), BigInt(20))))
              )
            )
          ),
          ExUnits(memory = 616106, steps = 160296255)
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
                  (ByteString.fromString("CS1"), List((ByteString.fromString("TN1"), BigInt(10)))),
                  (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(20))))
                )
              )
              .toSortedMap,
          SortedMap.unsafeFromList(
            List(
              (
                ByteString.fromString("CS1"),
                SortedMap.unsafeFromList(List((ByteString.fromString("TN1"), BigInt(10))))
              ),
              (
                ByteString.fromString("CS2"),
                SortedMap.unsafeFromList(List((ByteString.fromString("TN2"), BigInt(20))))
              )
            )
          ),
          ExUnits(memory = 143635, steps = 36169761)
        )
    }

    test("adaCurrencySymbol") {
        assertEvalWithBudget(
          Value.adaPolicyId,
          ByteString.empty,
          ExUnits(memory = 500, steps = 64100)
        )
    }

    test("adaTokenName") {
        assertEvalWithBudget(
          Value.adaTokenName,
          ByteString.empty,
          ExUnits(memory = 500, steps = 64100)
        )
    }

    test("equalsAssets") {
        assertEval(
          Value.equalsAssets(
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1)),
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1))
          )
        )

        assertEval(
          !Value.equalsAssets(
            SortedMap.singleton(ByteString.fromString("TokenName1"), BigInt(1)),
            SortedMap.singleton(ByteString.fromString("TokenName2"), BigInt(1))
          )
        )

        assertEval(
          !Value.equalsAssets(
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1)),
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(-1))
          )
        )
    }

    test("Eq") {
        checkEval { (value: Value) => value === value }

        assertEval(Value.zero === Value.zero)

        assertEval(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ) === Value.zero
        )

        assertEval(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ) ===
              Value(
                ByteString.fromString("PolicyId"),
                ByteString.fromString("TokenName"),
                BigInt(1)
              )
        )

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol1"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ) !==
              Value(
                ByteString.fromString("CurrencySymbol2"),
                ByteString.fromString("TokenName"),
                BigInt(1)
              )
        )

        assertEval(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName1"),
            BigInt(1)
          ) !==
              Value(
                ByteString.fromString("PolicyId"),
                ByteString.fromString("TokenName2"),
                BigInt(1)
              )
        )

        assertEval(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ) !==
              Value(
                ByteString.fromString("PolicyId"),
                ByteString.fromString("TokenName"),
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
        //        (ByteString.fromString("CS1"), List((ByteString.fromString("TN1"), BigInt(0))))
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
          ExUnits(memory = 18388, steps = 3632976)
        )

        assertEvalWithBudget(
          -Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ),
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(-1000)
          ),
          ExUnits(memory = 108861, steps = 27299771)
        )

        assertEvalWithBudget(
          -Value.lovelace(BigInt(1000)),
          Value.lovelace(BigInt(-1000)),
          ExUnits(memory = 110061, steps = 27491771)
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
          ExUnits(memory = 33244, steps = 6718354)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) +
              Value(
                ByteString.fromString("PolicyId"),
                ByteString.fromString("TokenName"),
                BigInt(2000)
              ),
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(3000)
          ),
          ExUnits(memory = 240637, steps = 64303523)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) +
              Value.zero,
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ),
          ExUnits(memory = 169289, steps = 43308271)
        )

        assertEvalWithBudget(
          Value.zero +
              Value(
                ByteString.fromString("PolicyId"),
                ByteString.fromString("TokenName"),
                BigInt(1000)
              ),
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ),
          ExUnits(memory = 166625, steps = 42626283)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) + Value.lovelace(BigInt(2000)),
          Value.lovelace(BigInt(3000)),
          ExUnits(memory = 242137, steps = 64543411)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) + Value.zero,
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 170489, steps = 43500271)
        )

        assertEvalWithBudget(
          Value.zero + Value.lovelace(BigInt(1000)),
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 167825, steps = 42818283)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) +
              Value.lovelace(BigInt(1000)),
          Value.fromList(
            List(
              (
                ByteString.fromString("PolicyId"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ),
          ExUnits(memory = 378004, steps = 100241347)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) +
              Value(
                ByteString.fromString("PolicyId"),
                ByteString.fromString("TokenName"),
                BigInt(-1000)
              ),
          Value.zero,
          ExUnits(memory = 193217, steps = 50671900)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) + Value.lovelace(BigInt(-1000)),
          Value.zero,
          ExUnits(memory = 194717, steps = 50911788)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                ByteString.fromString("PolicyId"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) +
              Value.fromList(
                List(
                  (
                    ByteString.fromString("PolicyId"),
                    List((ByteString.fromString("TokenName"), BigInt(-1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(-1000))))
                )
              ),
          Value.zero,
          ExUnits(memory = 795630, steps = 207616212)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                ByteString.fromString("PolicyId"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) +
              Value.fromList(
                List(
                  (
                    ByteString.fromString("PolicyId"),
                    List((ByteString.fromString("TokenName"), BigInt(-1000)))
                  )
                )
              ),
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 718484, steps = 187067334)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                ByteString.fromString("PolicyId"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
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
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ),
          ExUnits(memory = 672854, steps = 174918104)
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
          ExUnits(memory = 33244, steps = 6718354)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) -
              Value(
                ByteString.fromString("PolicyId"),
                ByteString.fromString("TokenName"),
                BigInt(2000)
              ),
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(-1000)
          ),
          ExUnits(memory = 240637, steps = 64303523)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) -
              Value.zero,
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ),
          ExUnits(memory = 169289, steps = 43308271)
        )

        assertEvalWithBudget(
          Value.zero -
              Value(
                ByteString.fromString("PolicyId"),
                ByteString.fromString("TokenName"),
                BigInt(1000)
              ),
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(-1000)
          ),
          ExUnits(memory = 166625, steps = 42626283)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) - Value.lovelace(BigInt(2000)),
          Value.lovelace(BigInt(-1000)),
          ExUnits(memory = 242137, steps = 64543411)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) - Value.zero,
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 170489, steps = 43500271)
        )

        assertEvalWithBudget(
          Value.zero - Value.lovelace(BigInt(1000)),
          Value.lovelace(BigInt(-1000)),
          ExUnits(memory = 167825, steps = 42818283)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) -
              Value.lovelace(BigInt(1000)),
          Value.fromList(
            List(
              (
                ByteString.fromString("PolicyId"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(-1000))))
            )
          ),
          ExUnits(memory = 378004, steps = 100241347)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) -
              Value(
                ByteString.fromString("PolicyId"),
                ByteString.fromString("TokenName"),
                BigInt(1000)
              ),
          Value.zero,
          ExUnits(memory = 193217, steps = 50671900)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) - Value.lovelace(BigInt(1000)),
          Value.zero,
          ExUnits(memory = 194717, steps = 50911788)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                ByteString.fromString("PolicyId"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) -
              Value.fromList(
                List(
                  (
                    ByteString.fromString("PolicyId"),
                    List((ByteString.fromString("TokenName"), BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
                )
              ),
          Value.zero,
          ExUnits(memory = 795630, steps = 207616212)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                ByteString.fromString("PolicyId"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ) -
              Value.fromList(
                List(
                  (
                    ByteString.fromString("PolicyId"),
                    List((ByteString.fromString("TokenName"), BigInt(1000)))
                  )
                )
              ),
          Value.lovelace(BigInt(1000)),
          ExUnits(memory = 718484, steps = 187067334)
        )

        assertEvalWithBudget(
          Value.fromList(
            List(
              (
                ByteString.fromString("PolicyId"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
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
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ),
          ExUnits(memory = 672854, steps = 174918104)
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
          ExUnits(memory = 13567, steps = 2549771)
        )

        assertEvalWithBudget(
          Value.zero * BigInt(1),
          Value.zero,
          ExUnits(memory = 22091, steps = 4429407)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) * BigInt(2),
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(2000)
          ),
          ExUnits(memory = 112264, steps = 28037947)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) * BigInt(0),
          Value.zero,
          ExUnits(memory = 43882, steps = 10404797)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) * BigInt(2),
          Value.lovelace(BigInt(2000)),
          ExUnits(memory = 113464, steps = 28229947)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)) * BigInt(0),
          Value.zero,
          ExUnits(memory = 45082, steps = 10596797)
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
                      ByteString.fromString("ff"),
                      List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)
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
          ExUnits(memory = 18390, steps = 3700150)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).getLovelace,
          BigInt(1000),
          ExUnits(memory = 153635, steps = 39836084)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).getLovelace,
          BigInt(0),
          ExUnits(memory = 63713, steps = 15792289)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ).getLovelace,
          BigInt(0),
          ExUnits(memory = 25857, steps = 5110947)
        )
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
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ).isZero
        )

        assertEval(
          !Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
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
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ).nonZero
        )

        assertEval(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
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
          ExUnits(memory = 17790, steps = 3604150)
        )

        assertEvalWithBudget(
          Value.zero.quantityOf(ByteString.fromString("CS"), ByteString.fromString("TN")),
          BigInt(0),
          ExUnits(memory = 17190, steps = 3508150)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).quantityOf(Value.adaPolicyId, Value.adaTokenName),
          BigInt(1000),
          ExUnits(memory = 153035, steps = 39740084)
        )

        assertEvalWithBudget(
          Value
              .lovelace(BigInt(1000))
              .quantityOf(ByteString.fromString("CS"), ByteString.fromString("TN")),
          BigInt(0),
          ExUnits(memory = 69613, steps = 17310808)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).quantityOf(Value.adaPolicyId, Value.adaTokenName),
          BigInt(0),
          ExUnits(memory = 63113, steps = 15696289)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).quantityOf(ByteString.fromString("PolicyId"), ByteString.fromString("TokenName")),
          BigInt(1000),
          ExUnits(memory = 151835, steps = 39548196)
        )
    }

    test("withoutLovelace") {
        checkEval { (value: Value) =>
            value.withoutLovelace.getLovelace === BigInt(0)
        }

        assertEvalWithBudget(
          Value.zero.withoutLovelace,
          Value.zero,
          ExUnits(memory = 13892, steps = 2679134)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).withoutLovelace,
          Value.zero,
          ExUnits(memory = 63547, steps = 16102038)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).withoutLovelace,
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ),
          ExUnits(memory = 57715, steps = 14531273)
        )

        assertEvalWithBudget(
          Value
              .fromList(
                List(
                  (
                    ByteString.fromString("PolicyId"),
                    List((ByteString.fromString("TokenName"), BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
                )
              )
              .withoutLovelace,
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ),
          ExUnits(memory = 325286, steps = 83821952)
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
          ExUnits(memory = 15024, steps = 2903736)
        )

        assertEvalWithBudget(
          Value.lovelace(BigInt(1000)).flatten,
          List((Value.adaPolicyId, Value.adaTokenName, BigInt(1000))),
          ExUnits(memory = 99935, steps = 24992913)
        )

        assertEvalWithBudget(
          Value(
            ByteString.fromString("PolicyId"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).flatten,
          List(
            (
              ByteString.fromString("PolicyId"),
              ByteString.fromString("TokenName"),
              BigInt(1000)
            )
          ),
          ExUnits(memory = 98735, steps = 24800913)
        )

        assertEvalWithBudget(
          Value
              .fromList(
                List(
                  (
                    ByteString.fromString("PolicyId"),
                    List((ByteString.fromString("TokenName"), BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
                )
              )
              .flatten,
          List(
            (Value.adaPolicyId, Value.adaTokenName, BigInt(1000)),
            (
              ByteString.fromString("PolicyId"),
              ByteString.fromString("TokenName"),
              BigInt(1000)
            )
          ),
          ExUnits(memory = 401534, steps = 102756909)
        )
    }

    test("toLedgerValue") {
        import scalus.cardano.ledger.Coin

        // Create valid 28-byte PolicyIds (ScriptHash) for testing
        val policyId1 = ByteString.fromHex("a" * 56) // 28 bytes
        val policyId2 = ByteString.fromHex("b" * 56) // 28 bytes
        val policyId3 = ByteString.fromHex("1" * 56) // 28 bytes
        val policyId4 = ByteString.fromHex("f" * 56) // 28 bytes

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
          ByteString.fromString("assetName1"),
          BigInt(100)
        )
        val ledgerSingleAsset = singleAssetValue.toLedgerValue
        assert(ledgerSingleAsset.coin == Coin(0))
        assert(ledgerSingleAsset.assets.assets.size == 1)

        // Test value with lovelace and native assets
        val mixedValue = Value.lovelace(BigInt(2000000)) +
            Value(
              policyId1,
              ByteString.fromString("assetName1"),
              BigInt(100)
            ) +
            Value(
              policyId2,
              ByteString.fromString("assetName2"),
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
                (ByteString.fromString("asset1"), BigInt(100)),
                (ByteString.fromString("asset2"), BigInt(200))
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
              ByteString.fromString("token1"),
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
              List((ByteString.fromString("token1"), BigInt(10)))
            ),
            (
              policyId4,
              List((ByteString.fromString("token2"), BigInt(20)))
            )
          )
        )
        val ledgerOrdered = orderedValue.toLedgerValue
        val policyIds = ledgerOrdered.assets.assets.keys.toSeq
        assert(policyIds.size == 2)
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
