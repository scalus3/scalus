package scalus.examples.paymentsplitter

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data, ToData}
import scalus.cardano.ledger.ExUnits
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.{Address, Credential, PubKeyHash, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ScriptInfo.SpendingScript
import scalus.prelude.{List, Option, *}
import scalus.testing.kit.ScalusTest
import scalus.uplc.Program

import scala.util.control.NonFatal

class PaymentSplitterValidatorTest extends AnyFunSuite, ScalusTest {
    import Payee.*

    test("success when payments are correctly split for a single payee") {
        TestCase(
          payees = List(A),
          amount = 30,
          fee = 2,
          inputs = List(A gives 10),
          outputs = List(A gets 38),
          expected = success
        ).runWithDebug()
    }

    test("success when payments are correctly split between 2 payees") {
        TestCase(
          payees = List(A, B),
          amount = 30,
          fee = 2,
          inputs = List(A gives 10),
          outputs = List(A gets 23, B gets 15),
          expected = success
        ).runWithDebug()
    }

    test("success when payments are correctly split between 3 payees") {
        val scalusBudget = ExUnits(memory = 928805, steps = 201_475488L)
        TestCase(
          payees = List(A, B, C),
          amount = 30,
          fee = 2,
          inputs = List(A gives 10),
          outputs = List(A gets 18, B gets 10, C gets 10),
          // expected = success(scalusBudget)
          expected = success
        ).runWithDebug()

        compareBudgetWithReferenceValue(
          testName =
              "PaymentSplitterTest.success when payments are correctly split between 3 payees",
          scalusBudget = scalusBudget,
          refBudget = ExUnits(memory = 742305L, steps = 290456791L),
          isPrintComparison = true
        )
    }

    test("failure when a payee is not present in the inputs") {
        TestCase(
          payees = List(A, B),
          amount = 30,
          fee = 2,
          inputs = List.empty,
          outputs = List(A gets 14, B gets 14),
          expected = failure(
            "One of the payees must have an input to pay the fee and trigger the payout"
          )
        ).runWithDebug()
    }

    test("failure when a payee is not payed out (1 payee)") {
        TestCase(
          payees = List(A),
          amount = 30,
          fee = 2,
          inputs = List(A gives 10),
          outputs = List.empty,
          expected = failure("Not all payees were paid")
        ).runWithDebug()
    }

    test("failure when a one of the payee is not payed out") {
        val scalusBudget = ExUnits(memory = 597991, steps = 135_983994)
        TestCase(
          payees = List(A, B),
          amount = 30,
          fee = 2,
          inputs = List(A gives 10),
          outputs = List(A gets 38),
          // expected = failure("Not all payees were paid", scalusBudget)
          expected = failure("Not all payees were paid")
        ).runWithDebug()

        compareBudgetWithReferenceValue(
          testName = "PaymentSplitterTest.failure when a one of the payee is not payed out",
          scalusBudget = scalusBudget,
          refBudget = ExUnits(memory = 476022L, steps = 177206757L),
          isPrintComparison = true
        )
    }

    test("failure when payee not present in contact to be payed") {
        TestCase(
          payees = List(A, B),
          amount = 30,
          fee = 2,
          inputs = List(A gives 10),
          outputs = List(A gets 18, B gets 10, C gets 10),
          expected = failure("Must pay to a payee")
        ).runWithDebug()
    }

    test("success when split equally and remainder compensates fee - o1") {
        TestCase(
          payees = List(A, B, C),
          amount = 31,
          fee = 2,
          inputs = List(A gives 3),
          outputs = List(A gets 12, B gets 10, C gets 10),
          expected = success
        ).runWithDebug()
    }

    test("success when split equally and remainder compensates fee - o2") {
        TestCase(
          payees = List(A, B, C),
          amount = 31,
          fee = 3,
          inputs = List(A gives 3),
          outputs = List(A gets 11, B gets 10, C gets 10),
          expected = success
        ).runWithDebug()
    }

    test("success when split equally and remainder compensates fee - o3") {
        TestCase(
          payees = List(A, B, C),
          amount = 31,
          fee = 4,
          inputs = List(A gives 3),
          outputs = List(A gets 10, B gets 10, C gets 10),
          expected = success
        ).runWithDebug()
    }

    test("failure when inflated fee reduces the split payout") {
        TestCase(
          payees = List(A, B, C),
          amount = 31,
          fee = 10,
          inputs = List(A gives 3),
          outputs = List(A gets 8, B gets 8, C gets 8),
          expected = failure("value to be payed to payees is too low")
        ).runWithDebug()
    }

    test("failure when multiple payees are present in the inputs") {
        val payees = List(A.pkh, B.pkh)
        assertCase(
          payees,
          inputs = List(
            makeScriptInput(100),
            makePayeeInput(pkh = A.pkh, idx = 0, value = 100),
            makePayeeInput(pkh = B.pkh, idx = 1, value = 100)
          ),
          outputs = List.empty,
          fee = 10,
          expected = failure("Already found a fee payer")
        )
    }

    test("success when multiple payees are correctly split") {
        val payees = List(A.pkh, B.pkh)
        assertCase(
          payees,
          inputs = List(
            makeScriptInput(100),
            makePayeeInput(pkh = A.pkh, idx = 0, value = 100)
          ),
          outputs = List((A.pkh, 50 + 100 - 10), (B.pkh, 50)),
          fee = 10,
          expected = success
        )
    }

    test("success between 5 splitters with merged outputs") {
        val payees = List(A.pkh, B.pkh, C.pkh, D.pkh, E.pkh)
        assertCase(
          payees,
          inputs = List(
            makePayeeInput(A.pkh, idx = 0, value = 41961442),
            makeScriptInput(15000000),
          ),
          outputs = List(
            (A.pkh, 3000000 + 41115417),
            (B.pkh, 3000000),
            (C.pkh, 3000000),
            (D.pkh, 3000000),
            (E.pkh, 3000000)
          ),
          fee = 846025,
          expected = success
        )
    }

    test(
      "success between 5 splitters were first splitter have separate outputs for fee and payout"
    ) {
        val payees = List(A.pkh, B.pkh, C.pkh, D.pkh, E.pkh)
        assertCase(
          payees,
          inputs = List(
            makePayeeInput(A.pkh, idx = 0, value = 41961442),
            makeScriptInput(15000000),
          ),
          outputs = List(
            (A.pkh, 3000000),
            (A.pkh, 41115417),
            (B.pkh, 3000000),
            (C.pkh, 3000000),
            (D.pkh, 3000000),
            (E.pkh, 3000000)
          ),
          fee = 846025,
          expected = success
        )
    }

    private val contract = PaymentSplitterContract.withErrorTraces
    private val lockTxId = random[TxId]
    private val payeesTxId = random[TxId]
    private val txId = random[TxId]
    private val scriptHash = contract.script.scriptHash

    private val runScalaVersion = true

    private def assertCase(
        payees: List[ByteString],
        inputs: List[TxInInfo],
        outputs: List[(ByteString, BigInt)],
        fee: BigInt,
        expected: (String | Unit, Option[ExUnits])
    ): Unit = assertCase(contract.program)(payees, inputs, outputs, fee, expected)

    private def assertCase(script: Program)(
        payees: List[ByteString],
        inputs: List[TxInInfo],
        outputs: List[(ByteString, BigInt)],
        fee: BigInt,
        expected: (String | Unit, Option[ExUnits])
    ): Unit = {
        // Create contract with payees parameter

        val payeesData = payees.toData

        val applied = script $ payeesData

        // Build transaction outputs from provided parameters
        val txOutputs = outputs.map { case (pkh, amount) =>
            TxOut(
              address = Address(
                PubKeyCredential(PubKeyHash(pkh)),
                Option.None
              ),
              value = Value.lovelace(amount)
            )
        }

        // Create contract context with given inputs, outputs and fee
        val pkh = PubKeyHash(
          ByteString.fromHex("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        )
        val txCert = TxCert.RegStaking(Credential.PubKeyCredential(pkh), Option.None)
        val txOutRef = TxOutRef(lockTxId, 0)
        val redeemer = Data.unit // PaymentSplitterRedeemer("qqq").toData
        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = inputs,
            outputs = txOutputs,
            fee = fee,
            certificates = scalus.prelude.List(txCert),
            signatories = scalus.prelude.List(pkh),
            redeemers = SortedMap.fromList(
              scalus.prelude.List((ScriptPurpose.Spending(txOutRef), redeemer))
            ),
            id = txId
          ),
          redeemer = redeemer,
          scriptInfo = SpendingScript(txOutRef = txOutRef),
        )

        assert(context.scriptInfo == SpendingScript(txOutRef, Option.None))

        // Apply contract to the context
        val programWithContext = applied $ context.toData

        if runScalaVersion then
            try contract.code(payees.toData)(context.toData)
            catch
                case NonFatal(ex) =>
                    if expected._1.isInstanceOf[Unit] then throw ex

        // Evaluate the program
        val result = programWithContext.evaluateDebug

        // Assert the result matches the expected outcome
        checkResult(expected, result)
    }

    private def makePayeeInput(pkh: ByteString, idx: Int, value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(payeesTxId, idx),
          resolved = TxOut(
            address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    private def makeScriptInput(value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(lockTxId, 0),
          resolved = TxOut(
            address = Address(ScriptCredential(scriptHash), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    enum Payee(val pkh: ByteString):
        case A extends Payee(genByteStringOfN(28).sample.get)
        case B extends Payee(genByteStringOfN(28).sample.get)
        case C extends Payee(genByteStringOfN(28).sample.get)
        case D extends Payee(genByteStringOfN(28).sample.get)
        case E extends Payee(genByteStringOfN(28).sample.get)
        case F extends Payee(genByteStringOfN(28).sample.get)
        case G extends Payee(genByteStringOfN(28).sample.get)
        case H extends Payee(genByteStringOfN(28).sample.get)

    extension (payee: Payee)
        inline infix def gives(amount: BigInt): Input = Input(payee, amount)
        inline infix def gets(amount: BigInt): Output = Output(payee, amount)

    case class Input(payee: Payee, amount: BigInt)

    case class Output(payee: Payee, amount: BigInt)

    case class TestCase(
        payees: List[Payee],
        amount: BigInt,
        fee: BigInt,
        inputs: List[Input],
        outputs: List[Output],
        expected: (String | Unit, Option[ExUnits])
    ) {
        def runWithDebug(): Unit = {
            assertCase(
              payees.map(_.pkh),
              inputs = List.from(
                inputs.asScala.zipWithIndex
                    .map { case (Input(payee, amount), index) =>
                        makePayeeInput(payee.pkh, index, amount)
                    }
                    .prepended(makeScriptInput(amount))
              ),
              outputs = outputs.map { case Output(payee, amount) =>
                  (payee.pkh, amount)
              },
              fee = fee,
              expected = expected
            )
        }
    }
}
