package scalus.cardano.onchain.plutus.v1

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Coin, ExUnits, MajorProtocolVersion}
import scalus.uplc.PlutusV3
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.ByteString.{hex, utf8}
import scalus.cardano.ledger.LedgerToPlutusTranslation
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.prelude.*
import scalus.testing.kit.EvalTestKit

class ValueTest extends AnyFunSuite with EvalTestKit with ArbitraryInstances {
    // The macro-spliced `===` calls trip the not-provably-default Eq heuristic at the
    // prelude's extension method site; the warning is informational and not actionable
    // for this test suite, so silence it here.
    override protected def compilerOptions: scalus.compiler.Options =
        super.compilerOptions.copy(noWarn = true)

    given [T: Arbitrary]: Arbitrary[List[T]] = Arbitrary {
        for
            size <- Gen.choose(0, 10)
            elements <- Gen.listOfN(size, Arbitrary.arbitrary[T])
        yield List.from(elements)
    }

    test("toSortedMap properties") {
        checkEval { (value: Value) =>
            value.toSortedMap.forall { case (policyId, tokens) =>
                tokens.forall { case (tokenName, amount) =>
                    amount === value.quantityOf(policyId, tokenName)
                }
            }
        }

        assertEvalEq(
          Value.zero.toSortedMap,
          SortedMap.empty
        )
    }

    test("toSortedMap lovelace") {
        assertEvalWithBudget(
          (v: Value) => v.toSortedMap,
          Value.lovelace(1000),
          SortedMap.singleton(
            Value.adaPolicyId,
            SortedMap.singleton(Value.adaTokenName, BigInt(1000))
          ),
          ExUnits(memory = 500, steps = 64100)
        )
    }

    test("toSortedMap token") {
        assertEvalWithBudget(
          (v: Value) => v.toSortedMap,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          SortedMap.singleton(
            utf8"PolicyId",
            SortedMap.singleton(utf8"TokenName", BigInt(1000))
          ),
          ExUnits(memory = 500, steps = 64100)
        )
    }

    test("zero") {
        assertEvalEq(
          Value.zero.toSortedMap,
          SortedMap.empty[PolicyId, SortedMap[TokenName, BigInt]]
        )
    }

    test("apply properties") {
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

        assertEvalEq(
          Value(
            utf8"PolicyId",
            utf8"TokenName",
            BigInt(0)
          ),
          Value.zero
        )
    }

    test("apply token") {
        assertEvalWithBudget(
          (v: Value) => v.toSortedMap,
          Value(utf8"PolicyId", utf8"TokenName", 1),
          SortedMap.singleton(
            utf8"PolicyId",
            SortedMap.singleton(utf8"TokenName", BigInt(1))
          ),
          ExUnits(memory = 500, steps = 64100)
        )
    }

    test("lovelace properties") {
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

        assertEvalEq(
          Value.lovelace(0),
          Value.zero
        )
    }

    test("lovelace toSortedMap") {
        assertEvalWithBudget(
          (v: Value) => v.toSortedMap,
          Value.lovelace(1000),
          SortedMap.singleton(
            Value.adaPolicyId,
            SortedMap.singleton(Value.adaTokenName, BigInt(1000))
          ),
          ExUnits(memory = 500, steps = 64100)
        )
    }

    test("unsafeFromList properties") {
        checkEval { (list: List[(PolicyId, List[(TokenName, BigInt)])]) =>
            // Build a strictly-ascending, key-unique reference list by routing through
            // `SortedMap.fromList(...).toList` rather than `distinct(keyPairEq)+quicksort`.
            // The latter is broken under the LoweringEq optimization: every `Eq[(A,B)]` is
            // rewritten to a structural (both-fields) compare, so `distinct` no longer
            // dedupes by key alone and pairs with the same key but different values survive.
            val validList =
                SortedMap
                    .fromList(list)
                    .toList
                    .filterMap { case (cs, tokens) =>
                        val validTokens = SortedMap
                            .fromList(tokens)
                            .toList
                            .filter { case (_, value) => value !== BigInt(0) }

                        if validTokens.nonEmpty then Option.Some((cs, validTokens)) else Option.None
                    }

            Value.unsafeFromList(validList).toSortedMap === SortedMap.unsafeFromList(
              validList.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
            )

        }
    }

    test("unsafeFromList two policies") {
        assertEvalWithBudget(
          (v: Value) => v.toSortedMap,
          Value.unsafeFromList(
            List(
              (utf8"CS1", List((utf8"TN1", BigInt(10)))),
              (utf8"CS2", List((utf8"TN2", BigInt(20))))
            )
          ),
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
          ExUnits(memory = 500, steps = 64100)
        )
    }

    test("fromList properties") {
        checkEval { (list: List[(PolicyId, List[(TokenName, BigInt)])]) =>
            Value.fromList(list).toSortedMap === SortedMap.fromList(
              list.filterMap { case (cs, tnList) =>
                  val tokens = tnList.filter { _._2 !== BigInt(0) }

                  if tokens.nonEmpty then Option.Some((cs, SortedMap.fromList(tokens)))
                  else Option.None
              }
            )
        }
    }

    test("fromList with duplicates and zeros") {
        assertEvalWithBudget(
          (v: Value) => v.toSortedMap,
          Value.fromList(
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
          ),
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
          ExUnits(memory = 500, steps = 64100)
        )
    }

    test("fromStrictlyAscendingListWithNonZeroAmounts properties") {
        checkEval { (list: List[(PolicyId, List[(TokenName, BigInt)])]) =>
            // Same dedup-via-SortedMap.fromList approach as the unsafeFromList property —
            // see that test for the rationale.
            val validList =
                SortedMap
                    .fromList(list)
                    .toList
                    .filterMap { case (cs, tokens) =>
                        val validTokens = SortedMap
                            .fromList(tokens)
                            .toList
                            .filter { case (_, value) => value !== BigInt(0) }

                        if validTokens.nonEmpty then Option.Some((cs, validTokens)) else Option.None
                    }

            Value.fromStrictlyAscendingListWithNonZeroAmounts(validList).toSortedMap ===
                SortedMap.unsafeFromList(
                  validList.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
                )
        }
    }

    test("fromStrictlyAscendingListWithNonZeroAmounts two policies") {
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
          ExUnits(memory = 65647, steps = 16_079838)
        )
    }

    test("adaCurrencySymbol") {
        assertEvalEq(
          Value.adaPolicyId,
          ByteString.empty
        )
    }

    test("adaTokenName") {
        assertEvalEq(
          Value.adaTokenName,
          ByteString.empty
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

    test("unary_ properties") {
        checkEval { (value: Value) =>
            val negatedValue = -value
            negatedValue.toSortedMap === value.toSortedMap.mapValues(_.mapValues(-_))
        }

        assertEvalEq(
          -Value.zero,
          Value.zero
        )
    }

    test("unary_ token") {
        assertEvalWithBudget(
          (v: Value) => -v,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", -1000),
          ExUnits(memory = 1413, steps = 990526)
        )
    }

    test("unary_ lovelace") {
        assertEvalWithBudget(
          (v: Value) => -v,
          Value.lovelace(1000),
          Value.lovelace(-1000),
          ExUnits(memory = 1413, steps = 990526)
        )
    }

    test("+ properties") {
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

        assertEvalEq(
          Value.zero + Value.zero,
          Value.zero
        )
    }

    test("+ token + token") {
        assertEvalWithBudget(
          (v: Value) => v + Value(utf8"PolicyId", utf8"TokenName", 2000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", 3000),
          ExUnits(memory = 1446, steps = 1_068221)
        )
    }

    test("+ token + zero") {
        assertEvalWithBudget(
          (v: Value) => v + Value.zero,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          ExUnits(memory = 1425, steps = 885065)
        )
    }

    test("+ zero + token") {
        assertEvalWithBudget(
          (v: Value) => Value.zero + v,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          ExUnits(memory = 1425, steps = 896099)
        )
    }

    test("+ lovelace + lovelace") {
        assertEvalWithBudget(
          (v: Value) => v + Value.lovelace(2000),
          Value.lovelace(1000),
          Value.lovelace(3000),
          ExUnits(memory = 1446, steps = 1_068221)
        )
    }

    test("+ lovelace + zero") {
        assertEvalWithBudget(
          (v: Value) => v + Value.zero,
          Value.lovelace(1000),
          Value.lovelace(1000),
          ExUnits(memory = 1425, steps = 885065)
        )
    }

    test("+ zero + lovelace") {
        assertEvalWithBudget(
          (v: Value) => Value.zero + v,
          Value.lovelace(1000),
          Value.lovelace(1000),
          ExUnits(memory = 1425, steps = 896099)
        )
    }

    test("+ token + lovelace") {
        assertEvalWithBudget(
          (v: Value) => v + Value.lovelace(1000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ),
          ExUnits(memory = 1468, steps = 1_106380)
        )
    }

    test("+ token cancel") {
        assertEvalWithBudget(
          (v: Value) => v + Value(utf8"PolicyId", utf8"TokenName", -1000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value.zero,
          ExUnits(memory = 1424, steps = 1_030062)
        )
    }

    test("+ lovelace cancel") {
        assertEvalWithBudget(
          (v: Value) => v + Value.lovelace(-1000),
          Value.lovelace(1000),
          Value.zero,
          ExUnits(memory = 1424, steps = 1_030062)
        )
    }

    test("+ multi-asset cancel") {
        assertEvalWithBudget(
          (v: Value) =>
              v + Value.fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(-1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(-1000))))
                )
              ),
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
          ExUnits(memory = 104613, steps = 27_995649)
        )
    }

    test("+ multi-asset partial cancel token") {
        assertEvalWithBudget(
          (v: Value) =>
              v + Value.fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(-1000)))
                  )
                )
              ),
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ),
          Value.lovelace(1000),
          ExUnits(memory = 55936, steps = 15_150352)
        )
    }

    test("+ multi-asset partial cancel lovelace") {
        assertEvalWithBudget(
          (v: Value) =>
              v + Value.fromList(
                List(
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(-1000))))
                )
              ),
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          ExUnits(memory = 55936, steps = 15_150352)
        )
    }

    test("- properties") {
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

        assertEvalEq(
          Value.zero - Value.zero,
          Value.zero
        )
    }

    test("- token - token") {
        assertEvalWithBudget(
          (v: Value) => v - Value(utf8"PolicyId", utf8"TokenName", 2000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", -1000),
          ExUnits(memory = 1446, steps = 1_068221)
        )
    }

    test("- token - zero") {
        assertEvalWithBudget(
          (v: Value) => v - Value.zero,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          ExUnits(memory = 1425, steps = 885065)
        )
    }

    test("- zero - token") {
        assertEvalWithBudget(
          (v: Value) => Value.zero - v,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", -1000),
          ExUnits(memory = 1858, steps = 1_238676)
        )
    }

    test("- lovelace - lovelace") {
        assertEvalWithBudget(
          (v: Value) => v - Value.lovelace(2000),
          Value.lovelace(1000),
          Value.lovelace(-1000),
          ExUnits(memory = 1446, steps = 1_068221)
        )
    }

    test("- lovelace - zero") {
        assertEvalWithBudget(
          (v: Value) => v - Value.zero,
          Value.lovelace(1000),
          Value.lovelace(1000),
          ExUnits(memory = 1425, steps = 885065)
        )
    }

    test("- zero - lovelace") {
        assertEvalWithBudget(
          (v: Value) => Value.zero - v,
          Value.lovelace(1000),
          Value.lovelace(-1000),
          ExUnits(memory = 1858, steps = 1_238676)
        )
    }

    test("- token - lovelace") {
        assertEvalWithBudget(
          (v: Value) => v - Value.lovelace(1000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(-1000))))
            )
          ),
          ExUnits(memory = 1468, steps = 1_106380)
        )
    }

    test("- token cancel") {
        assertEvalWithBudget(
          (v: Value) => v - Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value.zero,
          ExUnits(memory = 1424, steps = 1_030062)
        )
    }

    test("- lovelace cancel") {
        assertEvalWithBudget(
          (v: Value) => v - Value.lovelace(1000),
          Value.lovelace(1000),
          Value.zero,
          ExUnits(memory = 1424, steps = 1_030062)
        )
    }

    test("- multi-asset cancel") {
        assertEvalWithBudget(
          (v: Value) =>
              v - Value.fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
                )
              ),
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
          ExUnits(memory = 105067, steps = 28_615803)
        )
    }

    test("- multi-asset partial cancel token") {
        assertEvalWithBudget(
          (v: Value) =>
              v - Value.fromList(
                List(
                  (
                    utf8"PolicyId",
                    List((utf8"TokenName", BigInt(1000)))
                  )
                )
              ),
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ),
          Value.lovelace(1000),
          ExUnits(memory = 56369, steps = 15_492929)
        )
    }

    test("- multi-asset partial cancel lovelace") {
        assertEvalWithBudget(
          (v: Value) =>
              v - Value.fromList(
                List(
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
                )
              ),
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          ExUnits(memory = 56369, steps = 15_492929)
        )
    }

    test("* properties") {

        /** Scaling factors, bounded so that the product stays a legal quantity.
          *
          * Shadows the ambient wide `iArb` for this test only. `genAmount` caps a generated
          * quantity at `2^64`, and the CIP-153 `scaleValue` builtin that `Value.multiply` lowers to
          * at PV11 fails when a scaled quantity leaves the `+-(2^127)` range (see
          * `docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md`). `2^62` is the
          * largest bound that provably composes with that cap - `2^64 * 2^62 = 2^126`, one bit of
          * headroom - so it exercises the guard's neighbourhood without tripping it. Widening
          * `genAmount` means narrowing this in step. The overflow behaviour itself is pinned by the
          * `*` PV11/PV10 divergence test below.
          */
        given Arbitrary[BigInt] =
            Arbitrary(Gen.choose(-BigInt(2).pow(62), BigInt(2).pow(62)))

        checkEval { (value: Value) => (value * 0) === Value.zero }

        checkEval { (value: Value, factor: BigInt) =>
            (value * factor).toSortedMap === (
              if factor !== BigInt(0) then
                  value.toSortedMap.mapValues { _.mapValues { _ * factor } }
              else SortedMap.empty
            )
        }

        assertEvalEq(
          Value.zero * 0,
          Value.zero
        )

        assertEvalEq(
          Value.zero * 1,
          Value.zero
        )
    }

    test("* token by 2") {
        assertEvalWithBudget(
          (v: Value) => v * 2,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", 2000),
          ExUnits(memory = 1413, steps = 990526)
        )
    }

    test("* token by 0") {
        assertEvalWithBudget(
          (v: Value) => v * 0,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value.zero,
          ExUnits(memory = 1391, steps = 952367)
        )
    }

    test("* lovelace by 2") {
        assertEvalWithBudget(
          (v: Value) => v * 2,
          Value.lovelace(1000),
          Value.lovelace(2000),
          ExUnits(memory = 1413, steps = 990526)
        )
    }

    test("* lovelace by 0") {
        assertEvalWithBudget(
          (v: Value) => v * 0,
          Value.lovelace(1000),
          Value.zero,
          ExUnits(memory = 1391, steps = 952367)
        )
    }

    /** Pins the documented PV11 strictness of `Value.multiply`.
      *
      * At PV11 `multiply` lowers to the CIP-153 `scaleValue` builtin, which fails when a scaled
      * quantity leaves the `+-(2^127)` range; the portable PV10 lowering computes on unbounded
      * `BigInt` and has no such bound. `* properties` above deliberately stays inside the safe
      * range, so this is the one place the divergence is asserted. See
      * `docs/superpowers/specs/2026-08-18-t7-value-builtins-lowering-design.md`.
      */
    test("* product outside the 128-bit quantity range fails at PV11, succeeds at PV10") {
        // 1073741824 is 2^30; the input quantity is 2^100 (itself a legal quantity), so the
        // product is 2^130 - out of range. The argument is applied after lowering, so the
        // optimizer never sees it as a constant.
        val compiled = PlutusV3.compile { (d: Data) =>
            (fromData[Value](d) * 1073741824).toData
        }
        val pv10 = compiled.withOptions(
          compilerOptions.copy(targetProtocolVersion = MajorProtocolVersion.plominPV)
        )
        val input: Data = Value(utf8"PolicyId", utf8"TokenName", BigInt(2).pow(100)).toData

        val pv11Result = (compiled.program.term $ input.asTerm).evaluateDebug
        assert(pv11Result.isFailure, s"expected a PV11 failure, got: $pv11Result")
        // Pin the reason, so the test cannot pass on an unrelated failure.
        assert(
          pv11Result.toString.contains("128-bit"),
          s"expected the scaleValue range check to fail, got: $pv11Result"
        )
        assert(
          (pv10.program.term $ input.asTerm).evaluateDebug.isSuccess,
          "expected the portable PV10 lowering to accept the overflowing product"
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

    test("getLovelace properties") {
        checkEval { (value: Value) =>
            value.getLovelace ===
                value.toSortedMap
                    .get(Value.adaPolicyId)
                    .flatMap(_.get(Value.adaTokenName))
                    .getOrElse(BigInt(0))
        }
    }

    test("getLovelace zero") {
        assertEvalWithBudget(
          (v: Value) => v.getLovelace,
          Value.zero,
          BigInt(0),
          ExUnits(memory = 1213, steps = 492985)
        )
    }

    test("getLovelace lovelace") {
        assertEvalWithBudget(
          (v: Value) => v.getLovelace,
          Value.lovelace(1000),
          BigInt(1000),
          ExUnits(memory = 1257, steps = 895629)
        )
    }

    test("getLovelace token returns zero") {
        assertEvalWithBudget(
          (v: Value) => v.getLovelace,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          BigInt(0),
          ExUnits(memory = 1257, steps = 895629)
        )
    }

    test("getLovelace zero-amount token returns zero") {
        assertEvalWithBudget(
          (v: Value) => v.getLovelace,
          Value(utf8"PolicyId", utf8"TokenName", 0),
          BigInt(0),
          ExUnits(memory = 1213, steps = 492985)
        )
    }

    test("lovelaceAmount lovelace-only") {
        assertEvalWithBudget(
          Value.lovelace(1000).lovelaceAmount,
          BigInt(1000),
          ExUnits(memory = 200, steps = 16_100)
        )
    }

    test("lovelaceAmount lovelace + native asset") {
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
          ExUnits(memory = 200, steps = 16_100)
        )
    }

    test("lovelaceAmount fails on zero") {
        assertEvalFails[NoSuchElementException] {
            Value.zero.lovelaceAmount
        }
    }

    test("isZero") {
        checkEval { (value: Value) =>
            if value.isZero then value.toSortedMap.isEmpty else value.nonZero
        }

        assertEval(Value.zero.isZero)

        assertEval(Value.lovelace(0).isZero)

        assertEval(!Value.lovelace(1000).isZero)

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

        assertEval(!Value.lovelace(0).nonZero)

        assertEval(Value.lovelace(1000).nonZero)

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

    test("quantityOf properties") {
        checkEval { (value: Value, policyId: PolicyId, tokenName: TokenName) =>
            value.quantityOf(policyId, tokenName) ===
                value.toSortedMap
                    .get(policyId)
                    .flatMap(_.get(tokenName))
                    .getOrElse(BigInt(0))
        }
    }

    test("quantityOf ada in zero") {
        assertEvalWithBudget(
          (v: Value) => v.quantityOf(Value.adaPolicyId, Value.adaTokenName),
          Value.zero,
          BigInt(0),
          ExUnits(memory = 1213, steps = 492985)
        )
    }

    test("quantityOf token in zero") {
        assertEvalWithBudget(
          (v: Value) => v.quantityOf(utf8"CS", utf8"TN"),
          Value.zero,
          BigInt(0),
          ExUnits(memory = 1213, steps = 492985)
        )
    }

    test("quantityOf ada in lovelace") {
        assertEvalWithBudget(
          (v: Value) => v.quantityOf(Value.adaPolicyId, Value.adaTokenName),
          Value.lovelace(1000),
          BigInt(1000),
          ExUnits(memory = 1257, steps = 895629)
        )
    }

    test("quantityOf missing token in lovelace") {
        assertEvalWithBudget(
          (v: Value) => v.quantityOf(utf8"CS", utf8"TN"),
          Value.lovelace(1000),
          BigInt(0),
          ExUnits(memory = 1257, steps = 895629)
        )
    }

    test("quantityOf ada in token value") {
        assertEvalWithBudget(
          (v: Value) => v.quantityOf(Value.adaPolicyId, Value.adaTokenName),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          BigInt(0),
          ExUnits(memory = 1257, steps = 895629)
        )
    }

    test("quantityOf matching token") {
        assertEvalWithBudget(
          (v: Value) => v.quantityOf(utf8"PolicyId", utf8"TokenName"),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          BigInt(1000),
          ExUnits(memory = 1257, steps = 895629)
        )
    }

    test("containsAtLeast: superset with larger amounts contains subset") {
        val a = Value.unsafeFromList(
          List(
            (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000)))),
            (hex"aa", List((hex"01", BigInt(5)), (hex"02", BigInt(7))))
          )
        )
        val b = Value.unsafeFromList(
          List((hex"aa", List((hex"01", BigInt(5)))))
        )
        assert(a.containsAtLeast(b))
        assert(!b.containsAtLeast(a))
        assert(a.containsAtLeast(a))
        assert(a.containsAtLeast(Value.zero))
        assert(Value.zero.containsAtLeast(Value.zero))
    }

    test("containsAtLeast: missing token or smaller amount is not contained") {
        val a = Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(5))))))
        val more = Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(6))))))
        val other = Value.unsafeFromList(List((hex"bb", List((hex"01", BigInt(1))))))
        assert(!a.containsAtLeast(more))
        assert(!a.containsAtLeast(other))
    }

    test("containsAtLeast: negative amounts on either side throw") {
        val neg = Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(-1))))))
        val pos = Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(1))))))
        assertThrows[RequirementError](pos.containsAtLeast(neg))
        assertThrows[RequirementError](neg.containsAtLeast(pos))
    }

    test("containsAtLeast: evaluates on-chain") {
        assertEvalEq(
          Value
              .unsafeFromList(List((hex"aa", List((hex"01", BigInt(5))))))
              .containsAtLeast(Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(4))))))),
          true
        )
        assertEvalEq(
          Value
              .unsafeFromList(List((hex"aa", List((hex"01", BigInt(3))))))
              .containsAtLeast(Value.unsafeFromList(List((hex"aa", List((hex"01", BigInt(4))))))),
          false
        )
    }

    test("insertCoin properties") {
        // The inserted keys must respect the CIP-153 32-byte key bound: the value produced
        // here flows into the PV11 `unValueData` guard inside `quantityOf`. Shadows the
        // ambient unbounded ByteString arbitrary for this test only.
        given Arbitrary[ByteString] = Arbitrary(genAssetName)
        checkEval { (value: Value, cs: PolicyId, tn: TokenName) =>
            value.insertCoin(cs, tn, BigInt(7)).quantityOf(cs, tn) === BigInt(7) &&
            value.insertCoin(cs, tn, BigInt(-3)).quantityOf(cs, tn) === BigInt(-3) &&
            value.insertCoin(cs, tn, BigInt(0)).quantityOf(cs, tn) === BigInt(0)
        }
    }

    test("insertCoin inserts a new token") {
        assertEvalEq(
          Value(hex"aa", utf8"tokenX", 5).insertCoin(hex"bb", utf8"tokenY", BigInt(7)),
          Value.unsafeFromList(
            List(
              (hex"aa", List((utf8"tokenX", BigInt(5)))),
              (hex"bb", List((utf8"tokenY", BigInt(7))))
            )
          )
        )
    }

    test("insertCoin replaces an existing amount instead of adding") {
        assertEvalEq(
          Value(hex"aa", utf8"tokenX", 5).insertCoin(hex"aa", utf8"tokenX", BigInt(7)),
          Value(hex"aa", utf8"tokenX", 7)
        )
    }

    test("insertCoin zero amount deletes the token and drops an emptied policy") {
        assertEvalEq(
          Value(hex"aa", utf8"tokenX", 5).insertCoin(hex"aa", utf8"tokenX", BigInt(0)),
          Value.zero
        )
        assertEvalEq(
          Value
              .unsafeFromList(
                List((hex"aa", List((utf8"tokenX", BigInt(5)), (utf8"tokenY", BigInt(7)))))
              )
              .insertCoin(hex"aa", utf8"tokenX", BigInt(0)),
          Value(hex"aa", utf8"tokenY", 7)
        )
    }

    test("insertCoin budget") {
        assertEvalWithBudget(
          (v: Value) => v.insertCoin(utf8"PolicyId", utf8"Other", BigInt(7)),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value.unsafeFromList(
            List((utf8"PolicyId", List((utf8"Other", BigInt(7)), (utf8"TokenName", BigInt(1000)))))
          ),
          ExUnits(memory = 1689, steps = 1_175858)
        )
    }

    test("insertCoin zero amount on an absent token is a no-op") {
        assertEvalEq(
          Value(hex"aa", utf8"tokenX", 5).insertCoin(hex"bb", utf8"tokenY", BigInt(0)),
          Value(hex"aa", utf8"tokenX", 5)
        )
    }

    test("hasNft") {
        checkEval { (value: Value, cs: PolicyId, tn: TokenName) =>
            value.hasNft(cs, tn) === (value.quantityOf(cs, tn) === BigInt(1))
        }
        assertEval(Value(hex"aa", utf8"BEACON", 1).hasNft(hex"aa", utf8"BEACON"))
        assertEval(!Value(hex"aa", utf8"BEACON", 2).hasNft(hex"aa", utf8"BEACON"))
        assertEval(!Value.lovelace(1).hasNft(hex"aa", utf8"BEACON"))
        // Other assets are tolerated, under the same policy or another.
        assertEval(
          Value
              .unsafeFromList(
                List(
                  (hex"aa", List((utf8"BEACON", BigInt(1)), (utf8"OTHER", BigInt(9)))),
                  (hex"ff", List((utf8"tokenY", BigInt(2))))
                )
              )
              .hasNft(hex"aa", utf8"BEACON")
        )
    }

    test("budget: hasNft") {
        assertEvalWithBudgetAndFee(
          (v: Value) => v.hasNft(hex"aa", utf8"BEACON"),
          Value.lovelace(2_000_000) + Value(hex"aa", utf8"BEACON", 1) + Value(hex"ff", utf8"X", 3),
          true,
          ExUnits(memory = 1746, steps = 1_789014),
          Coin(230)
        )
    }

    test("budget: hasSameTokensAndAtLeastAda") {
        assertEvalWithBudgetAndFee(
          (v: Value) =>
              v.hasSameTokensAndAtLeastAda(
                Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 5)
              ),
          Value.lovelace(2_500_000) + Value(hex"aa", utf8"TOKEN", 5),
          true,
          ExUnits(memory = 20170, steps = 11_586206),
          Coin(2000)
        )
    }

    test("hasSameTokensAndAtLeastAda") {
        // Reference: tokens exact, ADA >=.
        checkEval { (value: Value, expected: Value) =>
            value.hasSameTokensAndAtLeastAda(expected) ===
                (value.withoutLovelace === expected.withoutLovelace &&
                    value.getLovelace >= expected.getLovelace)
        }
        assertEval(
          (Value.lovelace(2_500_000) + Value(hex"aa", utf8"TOKEN", 5))
              .hasSameTokensAndAtLeastAda(
                Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 5)
              )
        )
        assertEval(
          (Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 5))
              .hasSameTokensAndAtLeastAda(
                Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 5)
              )
        )
        // ADA below.
        assertEval(
          !(Value.lovelace(1_999_999) + Value(hex"aa", utf8"TOKEN", 5))
              .hasSameTokensAndAtLeastAda(
                Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 5)
              )
        )
        // Token delta in either direction.
        assertEval(
          !(Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 6))
              .hasSameTokensAndAtLeastAda(
                Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 5)
              )
        )
        assertEval(
          !(Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 5) + Value(hex"bb", utf8"X", 1))
              .hasSameTokensAndAtLeastAda(
                Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 5)
              )
        )
        assertEval(
          !Value
              .lovelace(2_000_000)
              .hasSameTokensAndAtLeastAda(
                Value.lovelace(2_000_000) + Value(hex"aa", utf8"TOKEN", 5)
              )
        )
    }

    test("hasOnly properties") {
        checkEval { (value: Value, cs: PolicyId, tn: TokenName) =>
            val amount = value.quantityOf(cs, tn)
            (value.hasOnly(cs, tn, amount) ===
                (value.tokens(cs) === SortedMap.singleton(tn, amount))) &&
            (value.hasOnly(cs, tn, BigInt(1)) ===
                (value.tokens(cs) === SortedMap.singleton(tn, BigInt(1))))
        }

        checkEval { (cs: PolicyId, tn: TokenName, amount: BigInt) =>
            Value(cs, tn, amount).hasOnly(cs, tn, amount) === (amount !== BigInt(0))
        }
    }

    test("hasOnly: exactly one token under the policy") {
        val solo = Value(hex"aa", utf8"BEACON", 1)
        val multi = Value.unsafeFromList(
          List(
            (hex"11", List((utf8"tokenX", BigInt(5)))),
            (hex"aa", List((utf8"BEACON", BigInt(1)))),
            (hex"ff", List((utf8"tokenY", BigInt(2))))
          )
        )
        assert(solo.hasOnly(hex"aa", utf8"BEACON", 1))
        assert(multi.hasOnly(hex"aa", utf8"BEACON", 1))

        assertEvalEq(Value(hex"aa", utf8"BEACON", 1).hasOnly(hex"aa", utf8"BEACON", 1), true)
        assertEvalEq(
          Value
              .unsafeFromList(
                List(
                  (hex"11", List((utf8"tokenX", BigInt(5)))),
                  (hex"aa", List((utf8"BEACON", BigInt(1)))),
                  (hex"ff", List((utf8"tokenY", BigInt(2))))
                )
              )
              .hasOnly(hex"aa", utf8"BEACON", 1),
          true
        )
    }

    test("hasOnly: extra token, wrong amount, absent policy, zero amount") {
        val extraToken = Value.unsafeFromList(
          List((hex"aa", List((utf8"BEACON", BigInt(1)), (utf8"BEACON1", BigInt(1)))))
        )
        val wrongAmount = Value(hex"aa", utf8"BEACON", 2)
        assert(!extraToken.hasOnly(hex"aa", utf8"BEACON", 1))
        assert(!wrongAmount.hasOnly(hex"aa", utf8"BEACON", 1))
        assert(!wrongAmount.hasOnly(hex"bb", utf8"BEACON", 2))
        assert(!Value.zero.hasOnly(hex"aa", utf8"BEACON", 0))

        assertEvalEq(
          Value
              .unsafeFromList(
                List((hex"aa", List((utf8"BEACON", BigInt(1)), (utf8"BEACON1", BigInt(1)))))
              )
              .hasOnly(hex"aa", utf8"BEACON", 1),
          false
        )
        assertEvalEq(Value(hex"aa", utf8"BEACON", 2).hasOnly(hex"aa", utf8"BEACON", 1), false)
    }

    test("hasOnly: exact burn") {
        val burn = Value(hex"aa", utf8"BEACON", -1)
        assert(burn.hasOnly(hex"aa", utf8"BEACON", -1))
        assert(!burn.hasOnly(hex"aa", utf8"BEACON", 1))

        assertEvalEq(Value(hex"aa", utf8"BEACON", -1).hasOnly(hex"aa", utf8"BEACON", -1), true)
    }

    test("hasOnly: budget") {
        val multi = Value.unsafeFromList(
          List(
            (hex"11", List((utf8"tokenX", BigInt(5)))),
            (hex"aa", List((utf8"BEACON", BigInt(1)))),
            (hex"ff", List((utf8"tokenY", BigInt(2))))
          )
        )
        assertEvalWithBudget(
          (v: Value) => v.hasOnly(hex"aa", utf8"BEACON", 1),
          multi,
          true,
          ExUnits(memory = 8687, steps = 3_620482)
        )
    }

    test("withoutLovelace properties") {
        checkEval { (value: Value) =>
            value.withoutLovelace.getLovelace === BigInt(0)
        }
    }

    test("withoutLovelace keeps other tokens under the empty policy") {
        // Pins the delete-the-ada-token semantics shared with the CIP-153 `insertCoin`
        // builtin: only the (adaPolicyId, adaTokenName) coin goes; a non-ada token that
        // sits under the empty policy survives. Ledger-shaped values never hit this case.
        assertEvalEq(
          Value
              .unsafeFromList(
                List(
                  (
                    Value.adaPolicyId,
                    List((Value.adaTokenName, BigInt(1000)), (utf8"weird", BigInt(2)))
                  )
                )
              )
              .withoutLovelace,
          Value(Value.adaPolicyId, utf8"weird", 2)
        )
    }

    test("withoutLovelace zero") {
        assertEvalWithBudget(
          (v: Value) => v.withoutLovelace,
          Value.zero,
          Value.zero,
          ExUnits(memory = 1559, steps = 678958)
        )
    }

    test("withoutLovelace lovelace") {
        assertEvalWithBudget(
          (v: Value) => v.withoutLovelace,
          Value.lovelace(1000),
          Value.zero,
          ExUnits(memory = 1645, steps = 1_099540)
        )
    }

    test("withoutLovelace token") {
        assertEvalWithBudget(
          (v: Value) => v.withoutLovelace,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          ExUnits(memory = 1667, steps = 1_137699)
        )
    }

    test("withoutLovelace multi-asset") {
        assertEvalWithBudget(
          (v: Value) => v.withoutLovelace,
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ),
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          ExUnits(memory = 1732, steps = 1_539900)
        )
    }

    test("flatten properties") {
        checkEval { (value: Value) =>
            value.flatten ===
                value.toSortedMap.toList.flatMap { case (cs, tokens) =>
                    tokens.toList.map { case (tn, amount) => (cs, tn, amount) }
                }
        }
    }

    test("flatten zero") {
        assertEvalWithBudget(
          (v: Value) => v.flatten,
          Value.zero,
          List.empty,
          ExUnits(memory = 5764, steps = 946656)
        )
    }

    test("flatten lovelace") {
        assertEvalWithBudget(
          (v: Value) => v.flatten,
          Value.lovelace(1000),
          List((Value.adaPolicyId, Value.adaTokenName, BigInt(1000))),
          ExUnits(memory = 26680, steps = 6_562443)
        )
    }

    test("flatten token") {
        assertEvalWithBudget(
          (v: Value) => v.flatten,
          Value(utf8"PolicyId", utf8"TokenName", 1000),
          List(
            (
              utf8"PolicyId",
              utf8"TokenName",
              BigInt(1000)
            )
          ),
          ExUnits(memory = 26680, steps = 6_562443)
        )
    }

    test("flatten multi-asset") {
        assertEvalWithBudget(
          (v: Value) => v.flatten,
          Value.fromList(
            List(
              (
                utf8"PolicyId",
                List((utf8"TokenName", BigInt(1000)))
              ),
              (Value.adaPolicyId, List((Value.adaTokenName, BigInt(1000))))
            )
          ),
          List(
            (Value.adaPolicyId, Value.adaTokenName, BigInt(1000)),
            (
              utf8"PolicyId",
              utf8"TokenName",
              BigInt(1000)
            )
          ),
          ExUnits(memory = 47596, steps = 12_178230)
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
        val lovelaceValue = Value.lovelace(1000000)
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
        val mixedValue = Value.lovelace(2000000) +
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
        val originalValue = Value.lovelace(5000000) +
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

    test("Eq vs toData: zero equal via Eq") {
        assertEvalEq(
          Value.zero === Value.zero,
          true
        )
    }

    test("Eq vs toData: zero equal via toData") {
        assertEvalEq(
          Value.zero.toData == Value.zero.toData,
          true
        )
    }

    test("Eq vs toData: lovelace equal via Eq") {
        assertEvalWithBudget(
          (v: Value) => v === Value.lovelace(1000),
          Value.lovelace(1000),
          true,
          ExUnits(memory = 901, steps = 1_653665)
        )
    }

    test("Eq vs toData: lovelace equal via toData") {
        assertEvalWithBudget(
          (v: Value) => v.toData == Value.lovelace(1000).toData,
          Value.lovelace(1000),
          true,
          ExUnits(memory = 901, steps = 1_653665)
        )
    }

    test("Eq vs toData: single token equal via Eq") {
        assertEvalWithBudget(
          (v: Value) =>
              v === Value(
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                utf8"TOKEN1",
                BigInt(1000)
              ),
          Value(
            hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
            utf8"TOKEN1",
            BigInt(1000)
          ),
          true,
          ExUnits(memory = 901, steps = 1_735502)
        )
    }

    test("Eq vs toData: single token equal via toData") {
        assertEvalWithBudget(
          (v: Value) =>
              v.toData == Value(
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                utf8"TOKEN1",
                BigInt(1000)
              ).toData,
          Value(
            hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
            utf8"TOKEN1",
            BigInt(1000)
          ),
          true,
          ExUnits(memory = 901, steps = 1_735502)
        )
    }

    test("Eq vs toData: two policies equal via Eq") {
        assertEvalWithBudget(
          (v: Value) =>
              v === Value.fromList(
                List(
                  (
                    hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                    List((utf8"TOKEN1", BigInt(1000)))
                  ),
                  (Value.adaPolicyId, List((Value.adaTokenName, BigInt(2000))))
                )
              ),
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
          ExUnits(memory = 103704, steps = 27_583840)
        )
    }

    test("Eq vs toData: two policies equal via toData") {
        assertEvalWithBudget(
          (v: Value) =>
              v.toData == Value
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
          ExUnits(memory = 103704, steps = 27_583840)
        )
    }

    test("Eq vs toData: three policies equal via Eq") {
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
          ExUnits(memory = 200, steps = 16100)
        )
    }

    test("Eq vs toData: three policies equal via toData") {
        assertEvalWithBudget(
          (v: Value) =>
              v.toData == Value
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
          ExUnits(memory = 175461, steps = 46_629283)
        )
    }

    test("Eq vs toData: single token not equal via Eq") {
        assertEvalWithBudget(
          (v: Value) =>
              v !== Value(
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                utf8"TOKEN1",
                BigInt(2000)
              ),
          Value(
            hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
            utf8"TOKEN1",
            BigInt(1000)
          ),
          true,
          ExUnits(memory = 1101, steps = 1_767502)
        )
    }

    test("Eq vs toData: single token not equal via toData") {
        assertEvalWithBudget(
          (v: Value) =>
              v.toData != Value(
                hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
                utf8"TOKEN1",
                BigInt(2000)
              ).toData,
          Value(
            hex"a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8",
            utf8"TOKEN1",
            BigInt(1000)
          ),
          true,
          ExUnits(memory = 1101, steps = 1_767502)
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
