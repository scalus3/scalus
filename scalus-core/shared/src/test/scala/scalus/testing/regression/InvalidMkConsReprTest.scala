package scalus.testing.regression

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.*
import scalus.prelude.*
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.toData
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.{Credential, PubKeyHash, Value}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.uplc.eval.*

class InvalidMkConsReprTest extends AnyFunSuite {

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    given PlutusVM = PlutusVM.makePlutusV3VM()

    val pkhA = PubKeyHash(ByteString.fromString("A" * 32))
    val pkhB = PubKeyHash(ByteString.fromString("B" * 32))
    val scriptHash = ByteString.fromString("scriptHash")

    test("readingbytes string should compile and run") {
        // pending
        val sir = compile { (d: Data) =>

            val payes = d
                .to[List[ByteString]]
                .map(payee => payee)

        }

        // println(sir.pretty.render(100))
        val uplc = sir.toUplc(generateErrorTraces = true)

        // println(uplc.pretty.render(100))

        val payeesData = List(pkhA, pkhB).toData

        val applied = uplc.plutusV3 $ payeesData

        // val programWithContext = applied $ context.toData

        val result = applied.evaluateDebug
        // println(s"result=$result")
        assert(result.isSuccess)
    }

    test("Unconstr service context in fold should compile and run") {

        val sir = compile { (txInputsData: Data) =>
            val txInputs = txInputsData.to[List[TxInInfo]]

            val (optPayeeInputWithChange, sumContractInputs) = txInputs
                .foldLeft(Option.empty[TxOut], BigInt(0)) { case (acc, input) =>
                    (acc._1, acc._2 + input.resolved.value.getLovelace)
                }

        }

        val inTxId = TxId(ByteString.fromString("inTxId"))
        val lockTxId = TxId(ByteString.fromString("lockTxId"))
        val currentTxId = TxId(ByteString.fromString("currentTxId"))

        // val txCert = TxCert.RegStaking(Credential.PubKeyCredential(pkhA), Option.None)
        val txOutRef = TxOutRef(lockTxId, 0)

        val inputs = scalus.prelude.List(
          TxInInfo(
            outRef = TxOutRef(inTxId, 0),
            resolved = TxOut(
              address = Address(PubKeyCredential(pkhA), Option.None),
              value = Value.lovelace(10)
            )
          ),
          TxInInfo(
            outRef = TxOutRef(lockTxId, 0),
            resolved = TxOut(
              address = Address(ScriptCredential(scriptHash), Option.None),
              value = Value.lovelace(30)
            )
          )
        )

        val pl = List(pkhA).toData

        val uplc = sir.toUplc(generateErrorTraces = true)
        // println(uplc.pretty.render(100))

        val applied = uplc.plutusV3 $ inputs.toData

        val result = applied.evaluateDebug

        assert(result.isSuccess, s"Result: $result")

    }

    test("reading of input value") {

        val sir = compile { (txInInfoData: Data) =>
            val txInInfo = txInInfoData.to[TxInInfo]

            val lv = txInInfo.resolved.value.getLovelace

            require(lv == BigInt(10), "Expected input value to be 10")

        }

        val inTxId = TxId(ByteString.fromString("inTxId"))
        val lockTxId = TxId(ByteString.fromString("lockTxId"))
        val currentTxId = TxId(ByteString.fromString("currentTxId"))

        val txCert = TxCert.RegStaking(Credential.PubKeyCredential(pkhA), Option.None)
        val txOutRef = TxOutRef(lockTxId, 0)

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = scalus.prelude.List(
              TxInInfo(
                outRef = TxOutRef(inTxId, 0),
                resolved = TxOut(
                  address = Address(PubKeyCredential(pkhA), Option.None),
                  value = Value.lovelace(10)
                )
              ),
              TxInInfo(
                outRef = TxOutRef(lockTxId, 0),
                resolved = TxOut(
                  address = Address(ScriptCredential(scriptHash), Option.None),
                  value = Value.lovelace(30)
                )
              )
            ),
            outputs = List(
              TxOut(
                address = Address(
                  PubKeyCredential(pkhA),
                  Option.None
                ),
                value = Value.lovelace(38)
              )
            ),
            fee = 2,
            certificates = scalus.prelude.List(txCert),
            signatories = scalus.prelude.List(pkhA),
            redeemers = SortedMap.fromList(
              scalus.prelude.List((ScriptPurpose.Spending(txOutRef), Data.unit))
            ),
            id = currentTxId
          ),
          redeemer = Data.unit,
          scriptInfo = ScriptInfo.SpendingScript(txOutRef = txOutRef),
        )

        val pl = List(pkhA).toData

        // val lw = sir.toLoweredValue(generateErrorTraces = true)
        // println(lw.pretty.render(100))

        val uplc = sir.toUplc(generateErrorTraces = true)
        // println(uplc.pretty.render(100))

        val input = context.txInfo.inputs.head
        // println(s"input=${input}")
        // println(s"lovelace: ${input.resolved.value.getLovelace}")

        val applied = uplc.plutusV3 $ context.txInfo.inputs.head.toData

        val result = applied.evaluateDebug

        // println(s"evaluation result: ${result}")

        assert(result.isSuccess, s"Result: $result")

    }

    test("check foldLeft over SortedMap[ByteString, BigInt]") {

        val sir = compile {

            val aPayee: ByteString = ByteString.fromString("AAAA")

            val sumsPerPayee =
                SortedMap.empty[ByteString, BigInt].insert(aPayee, BigInt(38))

        }

        // val lw = sir.toLoweredValue(generateErrorTraces = true)
        // println(lw.pretty.render(100))

        val uplc = sir.toUplc(generateErrorTraces = true)
        // println(uplc.pretty.render(100))

        val result = uplc.evaluateDebug

        assert(result.isSuccess, s"Result: $result")

    }

    test("check sericalization with unB ") {

        val sir = compile {

            val aPayee: ByteString = ByteString.fromString("AAAA")

            val sumsPerPayee =
                SortedMap.empty[ByteString, BigInt].insert(aPayee, BigInt(38))

            val (optSplit, optPayeeSumWithChange, nPayed) =
                sumsPerPayee.toList.foldLeft(
                  (Option.empty[BigInt], Option.empty[BigInt], BigInt(0))
                ) { case ((optSplit, optPayeeSumWithChange, nPayed), (payee, value)) =>
                    (Option.Some(value), optPayeeSumWithChange, nPayed + 1)
                }

        }

        val inTxId = TxId(ByteString.fromString("inTxId"))
        val lockTxId = TxId(ByteString.fromString("lockTxId"))
        val currentTxId = TxId(ByteString.fromString("currentTxId"))

        val txCert = TxCert.RegStaking(Credential.PubKeyCredential(pkhA), Option.None)
        val txOutRef = TxOutRef(lockTxId, 0)

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = scalus.prelude.List(
              TxInInfo(
                outRef = TxOutRef(inTxId, 0),
                resolved = TxOut(
                  address = Address(PubKeyCredential(pkhA), Option.None),
                  value = Value.lovelace(10)
                )
              ),
              TxInInfo(
                outRef = TxOutRef(lockTxId, 0),
                resolved = TxOut(
                  address = Address(ScriptCredential(scriptHash), Option.None),
                  value = Value.lovelace(30)
                )
              )
            ),
            outputs = List(
              TxOut(
                address = Address(
                  PubKeyCredential(pkhA),
                  Option.None
                ),
                value = Value.lovelace(38)
              )
            ),
            fee = 2,
            certificates = scalus.prelude.List(txCert),
            signatories = scalus.prelude.List(pkhA),
            redeemers = SortedMap.fromList(
              scalus.prelude.List((ScriptPurpose.Spending(txOutRef), Data.unit))
            ),
            id = currentTxId
          ),
          redeemer = Data.unit,
          scriptInfo = ScriptInfo.SpendingScript(txOutRef = txOutRef),
        )

        val paramsData = List(pkhA).toData

        // val lw = sir.toLoweredValue(generateErrorTraces = true)
        // println(lw.pretty.render(100))

        val uplc = sir.toUplc(generateErrorTraces = true)
        // println(uplc.pretty.render(100))

        val input = context.txInfo.inputs.head

        val applied = uplc.plutusV3

        val result = applied.evaluateDebug

        assert(result.isSuccess, s"Result: $result")

    }

}
