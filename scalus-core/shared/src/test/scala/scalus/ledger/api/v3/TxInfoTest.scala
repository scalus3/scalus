package scalus.cardano.onchain.plutus.v3

import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.budget.BudgetTable
import scalus.cardano.onchain.OnchainError
import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{List, Option, SortedMap}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Builtins
import scalus.uplc.builtin.Builtins.{constrData, mkNilData}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.testing.kit.EvalTestKit

@Compile
private object TxInfoTestFixtures {
    // Two addresses sharing a payment credential but not a staking part, and a third one.
    val payment: Credential = Credential.ScriptCredential(hex"aa")
    val stakeA: Option[StakingCredential] =
        Option.Some(StakingCredential.StakingHash(Credential.PubKeyCredential(PubKeyHash(hex"01"))))
    val stakeB: Option[StakingCredential] =
        Option.Some(StakingCredential.StakingHash(Credential.PubKeyCredential(PubKeyHash(hex"02"))))
    val addrA: Address = Address(payment, stakeA)
    val addrB: Address = Address(payment, stakeB)
    val other: Address = Address(Credential.PubKeyCredential(PubKeyHash(hex"bb")), Option.None)
    val ref0: TxOutRef = TxOutRef(TxInfo.placeholder.id, BigInt(0))
    val ref1: TxOutRef = TxOutRef(TxInfo.placeholder.id, BigInt(1))
    val token: Value = Value(hex"cc", utf8"TOKEN", 5)

    def input(ref: TxOutRef, addr: Address, value: Value): TxInInfo =
        TxInInfo(ref, TxOut(addr, value))

    val ref0TokenName: ByteString = Builtins.blake2b_256(Builtins.serialiseData(ref0.toData))

    // A three-input, three-output transaction used by the budget pins.
    val budgetTx: TxInfo = TxInfo.placeholder.copy(
      inputs = List(
        input(ref1, other, Value.lovelace(100)),
        input(ref0, addrA, Value.lovelace(1) + token),
        input(ref1, addrB, Value.lovelace(2))
      ),
      outputs = List(
        TxOut(other, Value.lovelace(2)),
        TxOut(addrB, Value.lovelace(4)),
        TxOut(addrA, Value.lovelace(5) + token, OutputDatum.OutputDatum(BigInt(7).toData))
      ),
      mint = Value(hex"cc", utf8"A", -1) + Value(hex"cc", utf8"B", -3),
      signatories = List(PubKeyHash(hex"01"), PubKeyHash(hex"02")),
      validRange = Interval.between(10, 20)
    )
}

class TxInfoTest extends AnyFunSuite with EvalTestKit with ArbitraryInstances {
    override protected def budgetTable: BudgetTable = TxInfoTestBudgets
    import TxInfoTestFixtures.*

    test("findInputOrFail") {
        val tx = TxInfo.placeholder.copy(inputs = List(input(ref0, addrA, Value.lovelace(1))))
        assert(tx.findInputOrFail(ref0, "missing") == input(ref0, addrA, Value.lovelace(1)))
        assertThrows[OnchainError](tx.findInputOrFail(ref1, "missing"))

        assertEvalEq(
          TxInfo.placeholder
              .copy(inputs = List(input(ref0, addrA, Value.lovelace(1))))
              .findInputOrFail(ref0, "missing"),
          input(ref0, addrA, Value.lovelace(1))
        )
        assertEvalFailsWithMessage[OnchainError]("missing")(
          TxInfo.placeholder
              .copy(inputs = List(input(ref0, addrA, Value.lovelace(1))))
              .findInputOrFail(ref1, "missing")
        )
    }

    test("findContinuingOutputOrFail compares the whole address") {
        val own = input(ref0, addrA, Value.lovelace(1))
        // Same payment credential, different staking part: not a continuing output.
        val redirected = TxInfo.placeholder.copy(
          inputs = List(own),
          outputs = List(TxOut(addrB, Value.lovelace(1)), TxOut(other, Value.lovelace(2)))
        )
        assertThrows[OnchainError](redirected.findContinuingOutputOrFail(own, "no continuing"))
        assert(
          redirected.findOutputsByCredential(payment).length == BigInt(1)
        ) // the loose finder accepts it

        val ok = TxInfo.placeholder.copy(
          inputs = List(own),
          outputs = List(TxOut(other, Value.lovelace(2)), TxOut(addrA, Value.lovelace(1)))
        )
        assert(
          ok.findContinuingOutputOrFail(own, "no continuing") == TxOut(addrA, Value.lovelace(1))
        )

        val two = ok.copy(outputs = TxOut(addrA, Value.lovelace(3)) +: ok.outputs)
        assertThrows[OnchainError](two.findContinuingOutputOrFail(own, "no continuing"))

        assertEvalEq(
          TxInfo.placeholder
              .copy(
                inputs = List(input(ref0, addrA, Value.lovelace(1))),
                outputs = List(TxOut(other, Value.lovelace(2)), TxOut(addrA, Value.lovelace(1)))
              )
              .findContinuingOutputOrFail(input(ref0, addrA, Value.lovelace(1)), "no continuing"),
          TxOut(addrA, Value.lovelace(1))
        )
        assertEvalFailsWithMessage[OnchainError]("no continuing")(
          TxInfo.placeholder
              .copy(
                inputs = List(input(ref0, addrA, Value.lovelace(1))),
                outputs = List(TxOut(addrB, Value.lovelace(1)))
              )
              .findContinuingOutputOrFail(input(ref0, addrA, Value.lovelace(1)), "no continuing")
        )
    }

    test("valuePaidTo and valueSpentFrom sum whole values") {
        val tx = TxInfo.placeholder.copy(
          inputs = List(
            input(ref0, addrA, Value.lovelace(1) + token),
            input(ref1, addrA, Value.lovelace(2)),
            input(ref1, other, Value.lovelace(100))
          ),
          outputs = List(
            TxOut(addrA, Value.lovelace(3) + token),
            TxOut(addrB, Value.lovelace(4)),
            TxOut(addrA, Value.lovelace(5))
          )
        )
        assert(tx.valuePaidTo(addrA) == Value.lovelace(8) + token)
        assert(tx.valuePaidTo(other) == Value.zero)
        assert(tx.valueSpentFrom(addrA) == Value.lovelace(3) + token)
        assert(tx.valueSpentFrom(addrB) == Value.zero)

        assertEvalEq(
          TxInfo.placeholder
              .copy(outputs =
                  List(
                    TxOut(addrA, Value.lovelace(3) + token),
                    TxOut(addrB, Value.lovelace(4)),
                    TxOut(addrA, Value.lovelace(5))
                  )
              )
              .valuePaidTo(addrA),
          Value.lovelace(8) + token
        )
        assertEvalEq(
          TxInfo.placeholder
              .copy(inputs =
                  List(
                    input(ref0, addrA, Value.lovelace(1) + token),
                    input(ref1, other, Value.lovelace(100))
                  )
              )
              .valueSpentFrom(addrA),
          Value.lovelace(1) + token
        )
    }

    test("isSignedByAny") {
        val tx =
            TxInfo.placeholder.copy(signatories = List(PubKeyHash(hex"01"), PubKeyHash(hex"02")))
        assert(tx.isSignedByAny(List(PubKeyHash(hex"09"), PubKeyHash(hex"02"))))
        assert(!tx.isSignedByAny(List(PubKeyHash(hex"09"))))
        assert(!tx.isSignedByAny(List.empty))

        assertEval(
          TxInfo.placeholder
              .copy(signatories = List(PubKeyHash(hex"01"), PubKeyHash(hex"02")))
              .isSignedByAny(List(PubKeyHash(hex"09"), PubKeyHash(hex"02")))
        )
        assertEval(
          !TxInfo.placeholder
              .copy(signatories = List(PubKeyHash(hex"01"), PubKeyHash(hex"02")))
              .isSignedByAny(List(PubKeyHash(hex"09")))
        )
    }

    test("validFromOrFail and validToOrFail") {
        val bounded = TxInfo.placeholder.copy(validRange = Interval.between(10, 20))
        assert(bounded.validFromOrFail("from") == BigInt(10))
        assert(bounded.validToOrFail("to") == BigInt(20))
        val unbounded = TxInfo.placeholder // Interval.always
        assertThrows[OnchainError](unbounded.validFromOrFail("from"))
        assertThrows[OnchainError](unbounded.validToOrFail("to"))

        assertEvalEq(
          TxInfo.placeholder.copy(validRange = Interval.between(10, 20)).validFromOrFail("from"),
          BigInt(10)
        )
        assertEvalEq(
          TxInfo.placeholder.copy(validRange = Interval.between(10, 20)).validToOrFail("to"),
          BigInt(20)
        )
        assertEvalFailsWithMessage[OnchainError]("from")(TxInfo.placeholder.validFromOrFail("from"))
        assertEvalFailsWithMessage[OnchainError]("to")(TxInfo.placeholder.validToOrFail("to"))
    }

    test("onlyBurnsUnder is false on an empty sub-map") {
        val policy = hex"cc"
        assert(
          !TxInfo.placeholder.onlyBurnsUnder(policy)
        ) // mints nothing: vacuous forall would pass
        assert(TxInfo.placeholder.copy(mint = Value(policy, utf8"A", -1)).onlyBurnsUnder(policy))
        assert(
          TxInfo.placeholder
              .copy(mint = Value(policy, utf8"A", -1) + Value(policy, utf8"B", -3))
              .onlyBurnsUnder(policy)
        )
        assert(
          !TxInfo.placeholder
              .copy(mint = Value(policy, utf8"A", -1) + Value(policy, utf8"B", 1))
              .onlyBurnsUnder(policy)
        )
        // Another policy's mint does not count.
        assert(!TxInfo.placeholder.copy(mint = Value(hex"dd", utf8"A", -1)).onlyBurnsUnder(policy))

        assertEval(!TxInfo.placeholder.onlyBurnsUnder(hex"cc"))
        assertEval(
          TxInfo.placeholder.copy(mint = Value(hex"cc", utf8"A", -1)).onlyBurnsUnder(hex"cc")
        )
        assertEval(
          !TxInfo.placeholder
              .copy(mint = Value(hex"cc", utf8"A", -1) + Value(hex"cc", utf8"B", 1))
              .onlyBurnsUnder(hex"cc")
        )
    }

    test("hasPaidTagged is exact on address, value and tag") {
        val tag = OutputDatum.OutputDatum(ref0.deriveTokenName.toData)
        val tx = TxInfo.placeholder.copy(outputs = List(TxOut(addrA, Value.lovelace(5), tag)))
        assert(tx.hasPaidTagged(addrA, Value.lovelace(5), tag))
        assert(!tx.hasPaidTagged(addrA, Value.lovelace(4), tag)) // >= must NOT pass
        assert(!tx.hasPaidTagged(addrB, Value.lovelace(5), tag))
        assert(
          !tx.hasPaidTagged(
            addrA,
            Value.lovelace(5),
            OutputDatum.OutputDatum(ref1.deriveTokenName.toData)
          )
        )
        assert(!tx.hasPaidTagged(addrA, Value.lovelace(5), OutputDatum.NoOutputDatum))

        assertEval(
          TxInfo.placeholder
              .copy(outputs =
                  List(TxOut(addrA, Value.lovelace(5), OutputDatum.OutputDatum(BigInt(7).toData)))
              )
              .hasPaidTagged(addrA, Value.lovelace(5), OutputDatum.OutputDatum(BigInt(7).toData))
        )
        assertEval(
          !TxInfo.placeholder
              .copy(outputs =
                  List(TxOut(addrA, Value.lovelace(5), OutputDatum.OutputDatum(BigInt(7).toData)))
              )
              .hasPaidTagged(addrA, Value.lovelace(4), OutputDatum.OutputDatum(BigInt(7).toData))
        )
    }

    test("deriveTokenName matches the off-chain digest") {
        assert(ref0.deriveTokenName == ref0TokenName)
        assert(ref0.deriveTokenName != ref1.deriveTokenName)
        assertEvalEq(TxOutRef(TxInfo.placeholder.id, BigInt(0)).deriveTokenName, ref0TokenName)
    }
    test("findInput") {
        checkEval { (txOutRef: TxOutRef) =>
            TxInfo.placeholder.findInput(txOutRef).isEmpty
        }

        checkEval { (txInfo: TxInfo, txInInfo: TxInInfo) =>
            val newTxInfo = txInfo.copy(inputs = txInInfo +: txInfo.inputs)
            newTxInfo.findInput(txInInfo.outRef) === Option.Some(txInInfo)
        }

        assertEval(
          TxInfo.placeholder
              .findInput(
                TxOutRef(
                  TxInfo.placeholder.id,
                  BigInt(0)
                )
              )
              .isEmpty
        )

        assertEvalEq(
          TxInfo.placeholder
              .copy(
                inputs = List(
                  TxInInfo(
                    TxOutRef(TxInfo.placeholder.id, BigInt(0)),
                    TxOut(
                      Address(
                        Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
                        Option.None
                      ),
                      Value.zero
                    )
                  )
                )
              )
              .findInput(
                TxOutRef(TxInfo.placeholder.id, BigInt(0))
              ),
          Option.Some(
            TxInInfo(
              TxOutRef(TxInfo.placeholder.id, BigInt(0)),
              TxOut(
                Address(
                  Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
                  Option.None
                ),
                Value.zero
              )
            )
          )
        )
    }

    test("findDatum") {
        checkEval { (datumHash: DatumHash) =>
            TxInfo.placeholder.findDatum(datumHash).isEmpty
        }

        checkEval { (txInfo: TxInfo, datum: Datum) =>
            val newTxInfo = txInfo.copy(
              data = SortedMap.singleton(datum.dataHash, datum),
              outputs = List.empty
            )

            newTxInfo.findDatum(datum.dataHash) === Option.Some(datum)
        }

        checkEval { (txInfo: TxInfo, datum: Datum) =>
            val newTxInfo = txInfo.copy(
              data = SortedMap.empty,
              outputs = List(
                TxOut(
                  Address(
                    Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
                    Option.None
                  ),
                  Value.zero,
                  OutputDatum.OutputDatum(datum)
                )
              )
            )

            newTxInfo.findDatum(datum.dataHash) === Option.Some(datum)
        }

        assertEval(
          TxInfo.placeholder.findDatum(constrData(BigInt(0), mkNilData()).dataHash).isEmpty
        )

        assertEvalEq(
          TxInfo.placeholder
              .copy(
                data = SortedMap.singleton(
                  constrData(BigInt(0), mkNilData()).dataHash,
                  constrData(BigInt(0), mkNilData())
                ),
                outputs = List.empty
              )
              .findDatum(constrData(BigInt(0), mkNilData()).dataHash),
          Option.Some(constrData(BigInt(0), mkNilData()))
        )

        assertEvalEq(
          TxInfo.placeholder
              .copy(
                data = SortedMap.empty,
                outputs = List(
                  TxOut(
                    Address(
                      Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
                      Option.None
                    ),
                    Value.zero,
                    OutputDatum.OutputDatum(constrData(BigInt(0), mkNilData()))
                  )
                )
              )
              .findDatum(constrData(BigInt(0), mkNilData()).dataHash),
          Option.Some(constrData(BigInt(0), mkNilData()))
        )
    }

    test("findOutputsByScriptHash") {
        checkEval { (validatorHash: ValidatorHash) =>
            TxInfo.placeholder.findOutputsByScriptHash(validatorHash).isEmpty
        }

        checkEval { (txInfo: TxInfo, validatorHash: ValidatorHash) =>
            val newTxInfo = txInfo.copy(
              outputs = List(
                TxOut(
                  Address(
                    Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
                    Option.None
                  ),
                  Value.zero
                ),
                TxOut(
                  Address(
                    Credential.ScriptCredential(validatorHash),
                    Option.None
                  ),
                  Value.zero
                )
              )
            )

            newTxInfo.findOutputsByScriptHash(validatorHash) === List(
              TxOut(
                Address(
                  Credential.ScriptCredential(validatorHash),
                  Option.None
                ),
                Value.zero
              )
            )
        }

        assertEval(
          TxInfo.placeholder
              .findOutputsByScriptHash(ByteString.empty)
              .isEmpty
        )

        assertEvalEq(
          TxInfo.placeholder
              .copy(
                outputs = List(
                  TxOut(
                    Address(
                      Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
                      Option.None
                    ),
                    Value.zero
                  ),
                  TxOut(
                    Address(
                      Credential.ScriptCredential(ByteString.empty),
                      Option.None
                    ),
                    Value.zero
                  )
                )
              )
              .findOutputsByScriptHash(ByteString.empty),
          List(
            TxOut(
              Address(
                Credential.ScriptCredential(ByteString.empty),
                Option.None
              ),
              Value.zero
            )
          )
        )
    }

    test("budget: findInputOrFail") {
        assertEvalEq(BigInt(1)): (tx: TxInfo) =>
            tx.findInputOrFail(ref0, "missing").resolved.value.getLovelace
        .at(budgetTx)
    }

    test("budget: findContinuingOutputOrFail") {
        assertEvalEq(BigInt(5)): (tx: TxInfo) =>
            tx.findContinuingOutputOrFail(tx.findInputOrFail(ref0, "missing"), "no continuing")
                .value
                .getLovelace
        .at(budgetTx)
    }

    test("budget: valuePaidTo") {
        assertEvalEq(BigInt(5))((tx: TxInfo) => tx.valuePaidTo(addrA).getLovelace).at(budgetTx)
    }

    test("budget: valueSpentFrom") {
        assertEvalEq(BigInt(1))((tx: TxInfo) => tx.valueSpentFrom(addrA).getLovelace).at(budgetTx)
    }

    test("budget: isSignedByAny") {
        assertEvalEq(true): (tx: TxInfo) =>
            tx.isSignedByAny(List(PubKeyHash(hex"09"), PubKeyHash(hex"02")))
        .at(budgetTx)
    }

    test("budget: validFromOrFail and validToOrFail") {
        assertEvalEq(BigInt(10)): (tx: TxInfo) =>
            tx.validToOrFail("to") - tx.validFromOrFail("from")
        .at(budgetTx)
    }

    test("budget: onlyBurnsUnder") {
        assertEvalEq(true)((tx: TxInfo) => tx.onlyBurnsUnder(hex"cc")).at(budgetTx)
    }

    test("budget: hasPaidTagged") {
        assertEvalEq(true): (tx: TxInfo) =>
            tx.hasPaidTagged(
              addrA,
              Value.lovelace(5) + token,
              OutputDatum.OutputDatum(BigInt(7).toData)
            )
        .at(budgetTx)
    }

    test("budget: deriveTokenName") {
        assertEvalEq(ref0TokenName)((ref: TxOutRef) => ref.deriveTokenName).at(ref0)
    }
}
