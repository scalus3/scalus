package scalus.uplc.eval

import io.bullet.borer.Cbor
import scalus.interop.TsType
import scalus.uplc.builtin.Data
import scalus.cardano.ledger.*
import scalus.uplc.{Constant, DeBruijnedProgram, Term}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.typedarray.Uint8Array

/** Main API exported by Scalus.
  *
  * @deprecated
  *   Use the top-level functions (`evaluateScript`, `evaluateScriptProfile`,
  *   `applyDataArgToScript`, `evalPlutusScripts`) instead; this namespace object remains for
  *   backwards compatibility.
  */
@JSExportTopLevel("Scalus")
object JScalus {

    extension (self: ExUnits)
        /** Converts ExUnits to a JavaScript BigInt representation. */
        def toJSExUnits: JSExUnits = new JSExUnits(
          steps = js.BigInt(self.steps.toString),
          memory = js.BigInt(self.memory.toString)
        )

    extension (self: Result)
        /** Converts Result to JSResult. When the result carries profiling data (i.e. it was
          * produced with profiling enabled), the `profileJson` field is set to the profile rendered
          * as JSON; otherwise it is `undefined`.
          *
          * Only the lightweight JSON rendering is exposed here on purpose: the HTML report and its
          * source-annotation machinery live in `ProfileFormatter.toHtml` / `loadSources`, which are
          * deliberately not referenced from the JS facade so they stay dead-code-eliminated out of
          * the (transaction-builder) `scalus.js` bundle.
          */
        def toJSResult: JSResult =
            val profileJson: js.UndefOr[String] = self.profile match
                case Some(p) => ProfileFormatter.toJson(p)
                case None    => js.undefined
            self match
                case s: Result.Success =>
                    JSResult(
                      isSuccess = true,
                      budget = s.budget.toJSExUnits,
                      logs = js.Array(s.logs*),
                      profileJson = profileJson
                    )
                case f: Result.Failure =>
                    JSResult(
                      isSuccess = false,
                      budget = f.budget.toJSExUnits,
                      logs = js.Array(f.exception.getMessage +: f.logs*),
                      profileJson = profileJson
                    )

    /** Execution units: what a script costs to run, in abstract machine memory and steps. A
      * transaction pays a fee for the units its scripts declare, and the ledger rejects it if a
      * script goes over what it declared.
      */
    @JSExportTopLevel("ExUnits")
    class JSExUnits(val memory: js.BigInt, val steps: js.BigInt) extends js.Object

    /** Outcome of evaluating one Plutus script. Read `isSuccess` first: the two outcomes differ in
      * what `budget` and `logs` mean.
      */
    @JSExportTopLevel("EvaluationResult")
    @JSExportTopLevel("Result")
    class JSResult(
        val isSuccess: Boolean,
        /** Units the machine spent. On failure this is what was spent before the script failed, and
          * zero when the script could not be decoded at all.
          */
        val budget: JSExUnits,
        /** Trace output the script emitted, oldest first. On failure the failure message is
          * prepended, so `logs[0]` is the error and the traces follow it.
          */
        val logs: js.Array[String],
        /** Profiling data as JSON; `undefined` unless the script was evaluated with profiling (see
          * [[evaluateScriptProfile]]).
          */
        val profileJson: js.UndefOr[String]
    ) extends js.Object

    /** One redeemer of a transaction, together with the execution budget its script really used.
      * `tag` and `index` together say which script this is, and match the redeemer in the
      * transaction.
      *
      * `tag` is why the script ran: `"Spend"` for a script input, `"Mint"` for a minting policy,
      * `"Cert"` for a certificate, `"Reward"` for a withdrawal, `"Voting"` for a vote, and
      * `"Proposing"` for a governance proposal.
      */
    @JSExportTopLevel("RedeemerBudget")
    @JSExportTopLevel("Redeemer")
    class Redeemer(
        @TsType("\"Spend\" | \"Mint\" | \"Cert\" | \"Reward\" | \"Voting\" | \"Proposing\"")
        val tag: String,
        /** Position within the group named by `tag`, counting from 0: for `"Spend"` it indexes the
          * transaction's inputs in ledger order, for `"Mint"` its minting policies, and so on.
          */
        val index: Int,
        val budget: JSExUnits
    ) extends js.Object

    /** Thrown by `evalPlutusScripts` when a Plutus script fails to evaluate. Carries the failure
      * message and the script's trace logs. Note: this is a plain object (not a subclass of
      * `Error`), so check it by shape or name rather than `instanceof Error`.
      */
    @JSExportTopLevel("PlutusScriptEvaluationError")
    class JSPlutusScriptEvaluationError(
        val message: String,
        val logs: js.Array[String]
    ) extends js.Object

    /** Applies one argument to a Plutus script and returns the applied script. Use it to bake a
      * parameter into a parameterized validator before you compute its script hash.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @param data
      *   The argument in the standard Plutus Data JSON encoding, for example `{"int":42}` or
      *   `{"constructor":0,"fields":[{"bytes":"deadbeef"}]}`.
      * @return
      *   The double-CBOR-encoded hex representation of the script with the argument applied.
      */
    @JSExport
    @JSExportTopLevel("applyDataArgToScript")
    def applyDataArgToScript(doubleCborHex: String, data: String): String = {
        // Parse script and data from hex
        val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
        val arg = Data.fromJson(data)
        val applied = program $ Term.Const(Constant.Data(arg))
        applied.doubleCborHex
    }

    /** Evaluates a Plutus script on its own, with no transaction around it. The script runs on a
      * Plutus V3 machine with the mainnet cost models of the current mainnet protocol major version
      * (11, van Rossem).
      *
      * This never throws. A script that fails, and a script whose hex cannot even be decoded, both
      * come back as a result with `isSuccess: false` and the message in `logs[0]`.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @return
      *   The outcome, with the units spent and the trace logs.
      */
    @JSExport
    @JSExportTopLevel("evaluateScript")
    def evaluateScript(doubleCborHex: String): JSResult = {
        try
            // Parse script from hex
            val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
            // Create appropriate VM based on version
            val vm = PlutusVM.makePlutusV3VM()
            // Evaluate script
            vm.evaluateScriptDebug(program).toJSResult
        catch
            case exception: Exception =>
                JSResult(
                  isSuccess = false,
                  budget = ExUnits.zero.toJSExUnits,
                  logs = js.Array(exception.getMessage),
                  profileJson = js.undefined
                )
    }

    /** Evaluates a Plutus script with profiling enabled.
      *
      * Same evaluation and same never-throws contract as [[evaluateScript]], but the result also
      * carries the machine's profiling data as JSON in `profileJson`: cost per source location,
      * cost per builtin, and the transition edges between them.
      *
      * The renderer that turns that JSON into the interactive HTML report is a Scala-side tool
      * (`ProfileFormatter`, in the Scalus library for the JVM). It is deliberately left out of this
      * package to keep the bundle small, so from JavaScript you get the data, not the report.
      *
      * @param doubleCborHex
      *   The double-CBOR-encoded hex representation of the Plutus script.
      * @return
      *   The outcome, with `profileJson` populated.
      */
    @JSExport
    @JSExportTopLevel("evaluateScriptProfile")
    def evaluateScriptProfile(doubleCborHex: String): JSResult = {
        try
            val program = DeBruijnedProgram.fromDoubleCborHex(doubleCborHex)
            val vm = PlutusVM.makePlutusV3VM()
            vm.evaluateScriptProfile(program).toJSResult
        catch
            case exception: Exception =>
                JSResult(
                  isSuccess = false,
                  budget = ExUnits.zero.toJSExUnits,
                  logs = js.Array(exception.getMessage),
                  profileJson = js.undefined
                )
    }

    /** Evaluates every Plutus script a transaction runs, and reports what each one costs. Use it to
      * fill in a transaction's execution units before you balance and submit it.
      *
      * @param txCborBytes
      *   CBOR bytes of the transaction whose scripts should run.
      * @param utxoCborBytes
      *   CBOR bytes of the UTxO set the scripts see: a CBOR map whose keys are transaction inputs
      *   (a `[transactionHash, outputIndex]` pair) and whose values are transaction outputs, as in
      *   the Cardano ledger CDDL. It must resolve every input and reference input of the
      *   transaction.
      * @param slotConfig
      *   Slot arithmetic for the target network, used to turn the transaction's validity interval
      *   into the POSIX times the scripts observe.
      * @param costModels
      *   One cost model per Plutus language version, indexed by position: `costModels[0]` is Plutus
      *   V1, `[1]` is V2, `[2]` is V3. Each inner array holds that version's cost parameters in
      *   protocol-parameter order. Give a model for every version the transaction uses; since the
      *   position is the version, an earlier version cannot be skipped.
      * @param protocolMajorVersion
      *   Cardano protocol major version, which picks the builtin semantics and the costing rules.
      *   Defaults to the current mainnet version, 11 (van Rossem).
      * @return
      *   One entry per redeemer of the transaction, carrying the units that redeemer's script
      *   spent.
      * @throws PlutusScriptEvaluationError
      *   if a script fails; it carries the failure message and that script's trace logs. Only
      *   script failures are reported this way: malformed transaction or UTxO CBOR surfaces as an
      *   ordinary error instead.
      */
    @JSExport
    @JSExportTopLevel("evalPlutusScripts")
    def evalPlutusScripts(
        txCborBytes: Uint8Array,
        utxoCborBytes: Uint8Array,
        slotConfig: SlotConfig,
        costModels: js.Array[js.Array[Double]],
        protocolMajorVersion: Int = CardanoInfo.mainnet.majorProtocolVersion.version
    ): js.Array[Redeemer] = {
        try
            val tx = Transaction.fromCbor(txCborBytes.toArray.map(_.toByte))
            val utxo =
                Cbor.decode(utxoCborBytes.toArray.map(_.toByte))
                    .to[Map[TransactionInput, TransactionOutput]]
                    .value
            val cms = CostModels(costModels.zipWithIndex.map { case (cm, lang) =>
                lang -> cm.toIndexedSeq.map(_.toLong)
            }.toMap)
            val evaluator = PlutusScriptEvaluator(
              slotConfig = slotConfig,
              initialBudget = ExUnits(Long.MaxValue, Long.MaxValue),
              protocolMajorVersion = MajorProtocolVersion(protocolMajorVersion),
              costModels = cms,
              mode = EvaluatorMode.EvaluateAndComputeCost
            )
            val results =
                for r <- evaluator.evalPlutusScripts(tx, utxo)
                yield new Redeemer(
                  tag = r.tag.toString,
                  index = r.index,
                  budget = JSExUnits(
                    steps = js.BigInt(r.exUnits.steps.toString),
                    memory = js.BigInt(r.exUnits.memory.toString)
                  )
                )
            results.toJSArray
        catch
            case e: PlutusScriptEvaluationException =>
                throw js.JavaScriptException(
                  JSPlutusScriptEvaluationError(e.getMessage, js.Array(e.logs*))
                )
    }

}
