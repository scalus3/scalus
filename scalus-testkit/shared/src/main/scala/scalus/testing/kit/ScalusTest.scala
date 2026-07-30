package scalus.testing.kit

import org.scalacheck.Arbitrary
import org.scalatest.Assertions
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.{EvaluatorReportConfig, ExUnits, Language, ProfileLevel, RedeemerTag}
import scalus.cardano.ledger.Script
import scalus.cardano.ledger.Transaction
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

        /** Like [[runWithDebug]] but also collects profiling data (`result.profile`). */
        def runWithProfile(scriptContext: ScriptContext)(using vm: PlutusVM): Result =
            vm.evaluateScriptProfile((self $ scriptContext.toData).deBruijnedProgram)

        /** Like [[runWithProfile]], and additionally writes the reports the Scalus VS Code
          * extension reads: the HTML/JSON/CSV renderings plus a `profile-manifest.json`, into
          * `SCALUS_DUMP_DIR` (default `target/scalus`).
          *
          * Suites that evaluate UPLC directly bypass `PlutusScriptEvaluator`, which is what
          * normally emits those files, so `SCALUS_PROFILE=full` alone produces nothing for them.
          * File names and the manifest schema copy the evaluator's, so the extension consumes
          * profiles from either source identically.
          *
          * Which renderings are produced follows [[scalus.cardano.ledger.EvaluatorReportConfig]]:
          * calling this asks for the full set, and `SCALUS_PROFILE` / `SCALUS_PROFILE_OUT` /
          * `SCALUS_DUMP_DIR` override it as they do for the ledger. Manifest entries are merged by
          * (scriptHash, tag, index), so several profiled tests accumulate rather than overwrite.
          */
        def runWithProfileReport(scriptContext: ScriptContext)(using vm: PlutusVM): Result = {
            val result = runWithProfile(scriptContext)
            result.profile.foreach { data =>
                ProfileReportWriter.write(
                  data,
                  EvaluatorReportConfig.fromEnv(profileReportDefaults),
                  Script.PlutusV3(self.cborByteString).scriptHash.toHex,
                  Language.PlutusV3.toString,
                  redeemerTag(scriptContext.scriptInfo),
                  0,
                  println
                )
            }
            result
        }

    /** The redeemer tag [[scalus.cardano.ledger.PlutusScriptEvaluator]] would record for this
      * script purpose, so test-side reports key their files the same way ledger-side ones do.
      */
    private def redeemerTag(scriptInfo: ScriptInfo): String = scriptInfo match
        case _: ScriptInfo.MintingScript    => RedeemerTag.Mint.toString
        case _: ScriptInfo.SpendingScript   => RedeemerTag.Spend.toString
        case _: ScriptInfo.RewardingScript  => RedeemerTag.Reward.toString
        case _: ScriptInfo.CertifyingScript => RedeemerTag.Cert.toString
        case _: ScriptInfo.VotingScript     => RedeemerTag.Voting.toString
        case _: ScriptInfo.ProposingScript  => RedeemerTag.Proposing.toString

    /** Base report config for [[runWithProfileReport]]: full renderings, overridable by env. */
    private def profileReportDefaults: EvaluatorReportConfig =
        EvaluatorReportConfig(enabled = true, profile = ProfileLevel.Full)

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
