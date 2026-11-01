package scalus.testing.kit

import org.scalacheck.Arbitrary
import org.scalactic.source
import org.scalatest.Assertions
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.{CardanoInfo, EvaluatorReportConfig, ExUnits, Language, ProfileLevel, RedeemerTag}
import scalus.cardano.ledger.Script
import scalus.cardano.ledger.{Transaction, TransactionHash}
import scalus.cardano.node.{EmulatorBase, NodeSubmitError}
import scalus.testing.budget.BudgetPins
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
import scalus.uplc.internal.ProfileReportWriter

import scala.util.control.NonFatal

// `prelude.Option.*` above shadows the standard Some; alias it for the off-chain APIs that take one.
import scala.{Option as ScalaOption, Some as ScalaSome}

trait ScalusTest extends ArbitraryInstances, Assertions, BudgetPins {
    protected def plutusVM: PlutusVM = PlutusVM.makePlutusV3VM()
    protected given PlutusVM = plutusVM

    /** Asserts that building a transaction fails, and that `expectedError` is in the error message
      * or in the script logs.
      * {{{
      * buildFails(HtlcValidator.InvalidReceiverPreimage) {
      *     txCreator.reveal(...)
      * }
      * }}}
      */
    protected def buildFails(expectedError: String)(buildTx: => Transaction)(using
        pos: source.Position
    ): Unit = {
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

    /** Verifies that transaction building fails with the expected error. */
    @deprecated("use buildFails", "1.5.0")
    protected def assertScriptFail(expectedError: String)(buildTx: => Transaction): Unit =
        buildFails(expectedError)(buildTx)

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
          *
          * A `<key>.uplc.json` source map is written alongside them when this program still carries
          * the compiler's source annotations, i.e. it was compiled in this process rather than
          * decoded from CBOR.
          */
        def runWithProfileReport(scriptContext: ScriptContext)(using vm: PlutusVM): Result = {
            val result = runWithProfile(scriptContext)
            result.profile.foreach { data =>
                // Profiling is auxiliary output: a failure to render or write the report must not
                // fail a test whose evaluation succeeded. Warn and keep the result, exactly like
                // the guarded renderProfile call in PlutusScriptEvaluator.
                try
                    ProfileReportWriter.write(
                      // Mainnet execution-unit prices so every report carries the derived fee
                      // columns; all public Cardano networks currently share the same prices.
                      data.withPrices(CardanoInfo.mainnet.protocolParams.executionUnitPrices),
                      EvaluatorReportConfig.fromEnv(profileReportDefaults),
                      Script.PlutusV3(self.cborByteString).scriptHash.toHex,
                      Language.PlutusV3.toString,
                      redeemerTag(scriptContext.scriptInfo),
                      0,
                      println,
                      ScalaSome(self.term)
                    )
                catch case NonFatal(e) => println(s"Failed to write profile report: $e")
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

    /** Asserts that an evaluation succeeded. Pins nothing: use the overload with the program when
      * the evaluation is the operation under test.
      */
    protected def assertSuccess(result: Result)(using pos: source.Position): Unit =
        if !result.isSuccess then
            fail(s"Expected success, but got: $result, logs: ${result.logs.mkString(", ")}")

    /** Asserts that an evaluation succeeded, and pins it (`[BP-1]`): memory, steps, the size of
      * `program` and the combined fee. The pin is a no-op until the suite sets its budget table and
      * mixes in `BudgetPinsSuite`.
      * {{{
      * val result = contract.program.runWithDebug(scriptContext)
      * assertSuccess(result, contract.program)
      * }}}
      */
    protected def assertSuccess(result: Result, program: Program)(using
        pos: source.Position
    ): Unit = {
        assertSuccess(result)
        pinEval(result.budget, Script.PlutusV3(program).script.size)
    }

    /** Asserts that an evaluation failed with `expectedError` in its logs. A failure is never
      * pinned (`[BP-1c]`).
      */
    protected def assertFailure(result: Result, expectedError: String)(using
        pos: source.Position
    ): Unit = {
        if !result.isFailure then fail(s"Expected failure with: $expectedError, but got success")
        if !result.logs.exists(_.contains(expectedError)) then
            fail(
              s"Expected error containing: $expectedError, but got: ${result.logs.mkString(", ")}"
            )
    }

    /** Asserts that an evaluation failed, with any error. Prefer the overload with the expected
      * error: this one also passes when the script fails for an unrelated reason.
      */
    protected def assertFailure(result: Result)(using pos: source.Position): Unit =
        if !result.isFailure then fail(s"Expected failure, but got success: $result")

    /** [[assertSuccess]] when `expectedError` is `None`, else [[assertFailure]]. For tests that
      * take the expected outcome as a parameter.
      */
    protected def assertOutcome(result: Result, expectedError: ScalaOption[String])(using
        pos: source.Position
    ): Unit =
        expectedError match
            case ScalaSome(error) => assertFailure(result, error)
            case _                => assertSuccess(result)

    @deprecated("use assertFailure", "1.5.0")
    final protected def failure(message: String): (String, Option[ExUnits]) =
        (message, Option.None)
    @deprecated("use assertFailure; budgets are pinned in the budget table", "1.5.0")
    final protected def failure(message: String, budget: ExUnits): (String, Option[ExUnits]) =
        (message, Option.Some(budget))
    @deprecated("use assertSuccess", "1.5.0")
    protected val success: (Unit, Option[ExUnits]) = ((), Option.None)
    @deprecated("use assertSuccess; budgets are pinned in the budget table", "1.5.0")
    final protected def success(budget: ExUnits): (Unit, Option[ExUnits]) =
        ((), Option.Some(budget))

    @deprecated("use assertSuccess or assertFailure", "1.5.0")
    protected def checkResult(
        expected: (String | Unit, Option[ExUnits]),
        actual: Result
    ): Unit = {
        expected._1 match
            case errorMsg: String => assertFailure(actual, errorMsg)
            case ()               => assertSuccess(actual)
        expected._2 match
            case Option.Some(budget) if budget != ExUnits(0, 0) =>
                assert(
                  actual.budget == budget,
                  s"Expected budget: $budget, but got: ${actual.budget}"
                )
            case _ =>
    }

    extension (emulator: EmulatorBase)

        /** Submits the transaction under test, asserts it is accepted, and pins it (`[BP-4]`): the
          * sum of its redeemer budgets, its size and its fee.
          * {{{
          * provider.submitOk(depositTx)
          * }}}
          */
        def submitOk(tx: Transaction)(using pos: source.Position): TransactionHash =
            val hash = emulator.submitSetup(tx)
            pinTx(tx)
            hash

        /** Submits a setup transaction and asserts it is accepted. Not pinned: it is not the
          * transaction under test (`[BP-4a]`).
          */
        def submitSetup(tx: Transaction)(using pos: source.Position): TransactionHash =
            emulator.submitSync(tx) match
                case Right(hash) => hash
                case Left(error) => fail(s"Transaction rejected: $error")

        /** Submits a transaction, asserts it is rejected, and that `expectedError` is in the script
          * logs or in the error message. A rejection is never pinned (`[BP-1c]`).
          * {{{
          * provider.submitFails(tx, "Deadline not reached")
          * }}}
          */
        def submitFails(tx: Transaction, expectedError: String)(using
            pos: source.Position
        ): Unit =
            emulator.submitSync(tx) match
                case Right(_) => fail("Transaction accepted, but a rejection was expected")
                case Left(sf: NodeSubmitError.ScriptFailure) =>
                    if !sf.logs.exists(_.contains(expectedError)) &&
                        !sf.message.contains(expectedError)
                    then
                        fail(
                          s"Expected error containing '$expectedError' but got: ${sf.message}, " +
                              s"logs: ${sf.logs.mkString(", ")}"
                        )
                case Left(error) =>
                    if !error.message.contains(expectedError) then
                        fail(s"Expected error containing '$expectedError' but got: $error")

    @deprecated("pin the budget with assertSuccess(result, program) instead", "1.5.0")
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
