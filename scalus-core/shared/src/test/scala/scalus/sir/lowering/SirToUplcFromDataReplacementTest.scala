package scalus.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.*
import scalus.builtin.ByteString.hex
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ScriptInfo.SpendingScript
import scalus.builtin.Data.toData
import scalus.sir.*
import scalus.uplc.*
import scalus.uplc.eval.PlutusVM
import scalus.uplc.eval.Result

class SirToUplcFromDataReplacementTest extends AnyFunSuite {

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("Import ScriptContext from Data and look on tx") {
        val sir = compile { (scData: Data) =>
            val sc = Data.fromData[ScriptContext](scData)
            val tx = sc.txInfo
            tx.inputs.isEmpty
        }
        // println(sir.show)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        // println(term.show)

        val ownerPkh = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val scriptContext = makeSpendingScriptContext(
          datum = ownerPkh.toData,
          redeemer = "Hello, World!".toData,
          signatories = scalus.prelude.List(ownerPkh)
        )

        val termWithSc = term $ Term.Const(Constant.Data(Data.toData(scriptContext)))

        val l = termWithSc.evaluateDebug

        // println(l)
        l match {
            case Result.Success(term, budget, costs, logs) =>
                assert(term == Term.Const(Constant.Bool(false)), "Expected term to be false")
            case Result.Failure(_, _, _, _) =>
                fail(s"Lowered code failed with result: $l")
        }
        // pending
    }

    test("Import ScriptInfp from Data and look on datum") {
        val sir = compile { (spData: Data) =>
            val scriptInfo = Data.fromData[ScriptInfo](spData)
            scriptInfo match {
                case SpendingScript(txOutRef, datum) =>
                    if datum.isEmpty then scalus.prelude.fail("Expected datum to be present")
                    else
                        scalus.prelude.fail("datum is present")
                        //
                        // val signed = sc.txInfo.signatories.contains(owner)
                        // require(signed, "Must be signed")
                        // val saysHello = sc.redeemer.to[String] == "Hello, World!"
                        // require(saysHello, "Invalid redeemer")
                case _ =>
                    scalus.prelude.fail("Expected SpendingScript")
            }
        }
        val lowering = SirToUplcV3Lowering(sir, generateErrorTraces = true)
        val term = lowering.lower()

        val ownerPkh = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val scriptContext = makeSpendingScriptContext(
          datum = ownerPkh.toData,
          redeemer = "Hello, World!".toData,
          signatories = scalus.prelude.List(ownerPkh)
        )

        val termWithSc = term $ Term.Const(Constant.Data(Data.toData(scriptContext.scriptInfo)))

        val l = termWithSc.evaluateDebug

        assert(l.isFailure, s"Lowered code should be failed but it's success: $l")

        assert(
          l.logs.find(_.contains("datum is present")).isDefined,
          "Expected log message 'datum is present' not found"
        )

        // pending
    }

    test("Import Option[PubkeyHash] from Data and look on it") {
        import scalus.prelude.*
        val sir = compile { (data: Data) =>
            val optDatum = Data.fromData[scalus.prelude.Option[Datum]](data)
            optDatum match {
                case Option.Some(datum) =>
                case _                  =>
                    scalus.prelude.fail("Expected non-empty PubKeyHash")
            }
        }
        val lowering = SirToUplcV3Lowering(sir, generateErrorTraces = true)
        val term = lowering.lower()

        val ownerPkh = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val scriptContext = makeSpendingScriptContext(
          datum = ownerPkh.toData,
          redeemer = "Hello, World!".toData,
          signatories = scalus.prelude.List(ownerPkh)
        )

        val optDatum: scalus.prelude.Option[Datum] = scriptContext.scriptInfo match {
            case SpendingScript(_, x) => x
            case _                    => scalus.prelude.Option.None
        }

        val termWithSc = term $ Term.Const(Constant.Data(Data.toData(optDatum)))

        val l = termWithSc.evaluateDebug

        assert(l.isSuccess, s"Lowered code failed with result: $l")

        // println(l)
        // pending
    }

    test("run Hello World") {
        import scalus.prelude.*
        val sir = compile { (scData: Data) =>
            val sc = Data.fromData[ScriptContext](scData)
            val tx = sc.txInfo
            sc.scriptInfo match {
                case ScriptInfo.SpendingScript(txOutRef, datum) =>
                    val Option.Some(ownerData) = datum: @unchecked
                    val owner = ownerData.to[PubKeyHash]
                    val signed = tx.signatories.contains(owner)
                    require(signed, "Must be signed")
                    val saysHello = sc.redeemer.to[String] == "Hello, World!"
                    require(saysHello, "Invalid redeemer")
                case _ =>
                    scalus.prelude.fail("Expected SpendingScript")
            }
        }
        // println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir, generateErrorTraces = true)
        val term = lowering.lower()
        // println(term.showHighlighted)
        val scriptContext = makeSpendingScriptContext(
          datum = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678").toData,
          redeemer = "Hello, World!".toData,
          signatories = scalus.prelude.List(
            PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
          )
        )
        val scData = Data.toData(scriptContext)
        val scD1 = Data.fromData[ScriptContext](scData)
        assert(scD1 == scriptContext, "ScriptContext deserialization failed")
        val termWithSc = term $ Term.Const(Constant.Data(Data.toData(scriptContext)))
        val res = termWithSc.evaluateDebug
        assert(res.isSuccess, s"Lowered code failed with result: $res")
    }

    private def makeSpendingScriptContext(
        datum: Data,
        redeemer: Redeemer,
        signatories: scalus.prelude.List[PubKeyHash]
    ): ScriptContext = {
        import scalus.prelude.*
        val ownInput =
            TxInInfo(
              outRef = TxOutRef(
                id = TxId(randomByteString(32, 20)),
                idx = 0
              ),
              resolved = TxOut(
                address = Address(
                  Credential.ScriptCredential(randomByteString(28)),
                  Option.None
                ),
                value = Value.zero
              )
            )
        ScriptContext(
          txInfo = TxInfo(
            inputs = List(ownInput),
            fee = 188021,
            signatories = signatories,
            id = TxId(randomByteString(32, 20))
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = ownInput.outRef,
            datum = Option.Some(datum)
          )
        )
    }

    def randomByteString(n: Int, seed: Int = 10): ByteString = {
        import scalus.builtin.ByteString
        val random = scala.util.Random(seed)
        ByteString(random.nextBytes(n)*)
    }

}
