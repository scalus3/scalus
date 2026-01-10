package scalus.ledger.api.v3

import org.scalatest.funsuite.AnyFunSuite
import scalus.prelude.{List, Option, SortedMap}
import scalus.builtin.ByteString
import scalus.builtin.Builtins.{constrData, mkNilData}
import scalus.ledger.api.v2.OutputDatum
import scalus.testing.kit.EvalTestKit

class TxInfoTest extends AnyFunSuite with EvalTestKit with ArbitraryInstances {
    test("findOwnInput") {
        checkEval { (txOutRef: TxOutRef) =>
            TxInfo.placeholder.findOwnInput(txOutRef).isEmpty
        }

        checkEval { (txInfo: TxInfo, txInInfo: TxInInfo) =>
            val newTxInfo = txInfo.copy(inputs = txInInfo +: txInfo.inputs)
            newTxInfo.findOwnInput(txInInfo.outRef) === Option.Some(txInInfo)
        }

        assertEval(
          TxInfo.placeholder
              .findOwnInput(
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
              .findOwnInput(
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

    test("findOwnDatum") {
        checkEval { (datumHash: DatumHash) =>
            TxInfo.placeholder.findOwnDatum(datumHash).isEmpty
        }

        checkEval { (txInfo: TxInfo, datum: Datum) =>
            val newTxInfo = txInfo.copy(
              data = SortedMap.singleton(datum.dataHash, datum),
              outputs = List.empty
            )

            newTxInfo.findOwnDatum(datum.dataHash) === Option.Some(datum)
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

            newTxInfo.findOwnDatum(datum.dataHash) === Option.Some(datum)
        }

        assertEval(
          TxInfo.placeholder.findOwnDatum(constrData(BigInt(0), mkNilData()).dataHash).isEmpty
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
              .findOwnDatum(constrData(BigInt(0), mkNilData()).dataHash),
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
              .findOwnDatum(constrData(BigInt(0), mkNilData()).dataHash),
          Option.Some(constrData(BigInt(0), mkNilData()))
        )
    }

    test("findOwnScriptOutputs") {
        checkEval { (validatorHash: ValidatorHash) =>
            TxInfo.placeholder.findOwnScriptOutputs(validatorHash).isEmpty
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

            newTxInfo.findOwnScriptOutputs(validatorHash) === List(
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
              .findOwnScriptOutputs(ByteString.empty)
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
              .findOwnScriptOutputs(ByteString.empty),
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
}
