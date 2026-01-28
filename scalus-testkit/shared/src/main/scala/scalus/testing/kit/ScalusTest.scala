package scalus.testing.kit

import org.scalacheck.Arbitrary
import org.scalatest.Assertions
import scalus.*
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.cardano.ledger.Transaction
import scalus.cardano.ledger.TransactionInput
import scalus.cardano.txbuilder.TxBuilderException
import scalus.cardano.onchain.plutus.v1.Credential.PubKeyCredential
import scalus.cardano.onchain.plutus.v1.Credential.ScriptCredential
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.compiler.Options
import scalus.compiler.sir.SIR
import scalus.uplc.*
import scalus.uplc.eval.*

@deprecated("Use TestUtil instead", "0.14.2")
object Mock:
    def rootKeyHash: ByteString = TestUtil.rootKeyHash
    def rootTxHash: ByteString = TestUtil.rootTxHash
    def mockPubKeyHash(variation: BigInt): PubKeyHash = TestUtil.mockPubKeyHash(variation)
    def mockScriptHash(variation: BigInt): ValidatorHash = TestUtil.mockScriptHash(variation)
    def mockTxOutRef(variation: BigInt, idx: BigInt): TxOutRef =
        TestUtil.mockTxOutRef(variation, idx)
    def mockTxInput(variation: BigInt, idx: BigInt): TransactionInput =
        TestUtil.mockTxInput(variation, idx)

trait ScalusTest extends ArbitraryInstances, Assertions {
    protected def plutusVM: PlutusVM = PlutusVM.makePlutusV3VM()
    protected given PlutusVM = plutusVM

    /** Verifies that transaction building fails with the expected error.
      *
      * @param expectedError
      *   Substring expected in error message or script logs
      * @param buildTx
      *   The transaction building code that should fail
      */
    protected def assertScriptFail(expectedError: String)(buildTx: => Transaction): Unit = {
        try
            val tx = buildTx
            fail(s"Transaction building should have failed but succeeded: $tx")
        catch
            case e: TxBuilderException.BalancingException =>
                val logs = e.scriptLogs.getOrElse(Seq.empty)
                if !logs.exists(_.contains(expectedError)) then
                    fail(
                      s"Expected error containing '$expectedError' but got logs: ${logs.mkString("\n")}"
                    )
            case e: Exception =>
                val message = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
                if !message.contains(expectedError) then
                    fail(s"Expected error containing '$expectedError' but got: $message")
    }

    extension (self: SIR)
        def runScript(using
            scalusOptions: Options = Options.default
        )(
            scriptContext: ScriptContext,
            param: Option[Data] = None
        ): Result =
            // UPLC program: (ScriptContext as Data) -> ()
            val script = self.toUplc().plutusV3
            // println(s"uplc: ${script.pretty.render(100)}")
            val appliedScript = param.map(script $ _).getOrElse(script) $ scriptContext.toData
            appliedScript.evaluateDebug

        def scriptV3(using
            scalusOptions: Options = Options.default
        )(errorTraces: Boolean = true): Program =
            self.toUplc(generateErrorTraces = errorTraces).plutusV3

    extension (self: Program)
        def runWithDebug(scriptContext: ScriptContext): Result =
            val appliedScript = self $ scriptContext.toData
            appliedScript.evaluateDebug

        @deprecated("will be removed", "0.14.2")
        def hash: ValidatorHash = blake2b_224(ByteString.fromArray(3 +: self.cborEncoded))

    protected def random[A: Arbitrary]: A = {
        Arbitrary.arbitrary[A].sample.get
    }

    protected def makeSpendingScriptContext(
        datum: Data,
        redeemer: Redeemer,
        signatories: List[PubKeyHash]
    ): ScriptContext = {
        val ownInput =
            TxInInfo(
              outRef = random[TxOutRef],
              resolved = TxOut(
                address = Address(
                  Credential.ScriptCredential(genByteStringOfN(28).sample.get),
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
            id = random[TxId]
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = ownInput.outRef,
            datum = Option.Some(datum)
          )
        )
    }

    protected def makePubKeyHashInput(pkh: Hash, value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(random[TxId], 0),
          resolved = TxOut(
            address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    protected def makeScriptHashInput(scriptHash: ValidatorHash, value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(random[TxId], 0),
          resolved = TxOut(
            address = Address(ScriptCredential(scriptHash), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    protected def makePubKeyHashOutput(
        pkh: Hash,
        value: BigInt,
        datum: OutputDatum = OutputDatum.NoOutputDatum
    ): TxOut = {
        TxOut(
          address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
          value = Value.lovelace(value),
          datum = datum
        )
    }

    protected def makeScriptHashOutput(
        scriptHash: ValidatorHash,
        value: BigInt,
        datum: OutputDatum = OutputDatum.NoOutputDatum
    ): TxOut = {
        TxOut(
          address = Address(ScriptCredential(scriptHash), Option.None),
          value = Value.lovelace(value),
          datum = datum
        )
    }

    final protected def failure(message: String): (String, Option[ExUnits]) =
        (message, Option.None)
    final protected def failure(message: String, budget: ExUnits): (String, Option[ExUnits]) =
        (message, Option.Some(budget))
    protected val success: (Unit, Option[ExUnits]) = ((), Option.None)
    final protected def success(budget: ExUnits): (Unit, Option[ExUnits]) =
        ((), Option.Some(budget))

    protected def checkResult(
        expected: (String | Unit, Option[ExUnits]),
        actual: Result
    ): Unit = {
        expected._1 match
            case errorMsg: String =>
                assert(
                  actual.isFailure,
                  s"Expected failure with: $errorMsg, but got success"
                )
                // If a specific error message is provided, check it matches
                assert(
                  actual.logs.exists(_.contains(errorMsg)),
                  s"Expected error containing: $errorMsg, but got: ${actual.logs.mkString(", ")}"
                )
            case () =>
                actual match
                    case Result.Failure(ex, budget, cost, logs) =>
                        ex match
                            case be: scalus.uplc.eval.BuiltinError =>
                                be.cause.printStackTrace()
                            case _ =>
                    case _ =>
                assert(
                  actual.isSuccess,
                  s"Expected success, but got: ${actual.toString}, logs0: ${actual.logs.mkString(", ")}"
                )

        expected._2 match
            case Option.Some(budget) if budget != ExUnits(0, 0) =>
                assert(
                  actual.budget == budget,
                  s"Expected budget: $budget, but got: ${actual.budget}"
                )
            case _ =>
    }

    def compareBudgetWithReferenceValue(
        testName: String,
        scalusBudget: ExUnits,
        refBudget: ExUnits,
        isPrintComparison: Boolean = false
    ): Unit = {
        import ScalusTest.BenchmarkConfig
        extension (scalus: Long)
            def comparisonAsJsonString(ref: Long): String = {
                val comparison = f"${scalus.toDouble / ref.toDouble * 100}%.2f"
                s"{scalus: $scalus, ref: $ref, comparison: $comparison%}"
            }

        end extension

        if isPrintComparison || BenchmarkConfig.isPrintAllComparisonsOfBudgetWithReferenceValue then
            println(
              s"${BenchmarkConfig.logPrefix}[$testName]: {" +
                  s"cpu: ${scalusBudget.steps.comparisonAsJsonString(refBudget.steps)}, " +
                  s"memory: ${scalusBudget.memory.comparisonAsJsonString(refBudget.memory)}" +
                  "}"
            )
    }

    protected def generateKeyPair(): (ByteString, ByteString) =
        KeyPairGenerator.generateKeyPair()
}

object ScalusTest {
    private object BenchmarkConfig {
        inline val logPrefix = "BenchmarkComparison"
        val isPrintAllComparisonsOfBudgetWithReferenceValue: Boolean = false
    }
}
