package scalus.uplc.eval

import scalus.cardano.ledger.PlutusScriptEvaluationException
import scalus.interop.{TsIgnore, TsType}
import scalus.uplc.builtin.Data.toCbor
import scalus.utils.Hex

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

/** One redeemer of a transaction, together with the execution budget its script really used. `tag`
  * and `index` together say which script this is, and match the redeemer in the transaction.
  *
  * `tag` is why the script ran: `"Spend"` for a script input, `"Mint"` for a minting policy,
  * `"Cert"` for a certificate, `"Reward"` for a withdrawal, `"Voting"` for a vote, and
  * `"Proposing"` for a governance proposal.
  *
  * @param tag
  *   Why the script ran, and so which group `index` counts within.
  * @param index
  *   Position within the group named by `tag`, counting from 0: for `"Spend"` it indexes the
  *   transaction's inputs in ledger order, for `"Mint"` its minting policies, and so on.
  * @param budget
  *   What this redeemer's script actually spent, to put in the transaction.
  */
@JSExportTopLevel("RedeemerBudget")
@JSExportTopLevel("Redeemer")
class JRedeemerBudget(
    @TsType("\"Spend\" | \"Mint\" | \"Cert\" | \"Reward\" | \"Voting\" | \"Proposing\"")
    val tag: String,
    val index: Int,
    val budget: JExUnits
) extends js.Object

/** Thrown by `evaluator.evaluateTx`, `evalPlutusScripts` and `Emulator.evaluateTx` when a Plutus
  * script fails. Extends `Error`, so `instanceof Error`, `.stack` and unhandled-rejection output
  * all behave normally.
  *
  * `args` is an own property but not an enumerable one: it holds the script context, and printing
  * kilobytes of hex by default would bury the fields that say what failed.
  *
  * From JavaScript, construct with `(message, logs)`.
  *
  * @param logs
  *   The failing script's own `trace` output, oldest first. Empty if it emitted none.
  * @param redeemer
  *   Which redeemer failed. Its `budget` is what the script spent before it failed.
  * @param scriptHash
  *   Hash of the failing script, as lowercase hex.
  * @param code
  *   Why it failed, classified the same way `evaluator.evaluateScript` classifies.
  * @param args
  *   The `Data` arguments the script was applied to, each as CBOR hex, in order: for a `PlutusV3`
  *   script the script context alone, for a `PlutusV1` or `PlutusV2` spend the datum, the redeemer
  *   and the script context.
  */
@JSExportTopLevel("PlutusScriptEvaluationError")
class JPlutusScriptEvaluationError @TsIgnore() (
    message: String,
    val logs: js.Array[String],
    @TsType("RedeemerBudget") val redeemer: js.UndefOr[JRedeemerBudget],
    @TsType("string") val scriptHash: js.UndefOr[String],
    @TsType(
      "\"SCRIPT_FAILURE\" | \"BUILTIN_FAILURE\" | \"INVALID_RETURN_VALUE\" | \"OUT_OF_BUDGET\" | \"INTERNAL_ERROR\""
    ) val code: js.UndefOr[String],
    @TsType("readonly string[]") val args: js.UndefOr[js.Array[String]]
) extends js.Error(message) {

    js.Object.defineProperty(
      this,
      "args",
      js.Dynamic.literal(enumerable = false).asInstanceOf[js.PropertyDescriptor]
    )

    override val name: String = "PlutusScriptEvaluationError"

    /** The constructor of releases before 1.3. An error built through it has no redeemer, hash,
      * code or arguments.
      */
    def this(message: String, logs: js.Array[String]) =
        this(message, logs, js.undefined, js.undefined, js.undefined, js.undefined)
}

object JPlutusScriptEvaluationError {

    /** One classification for both entry points: the single-script result and this error. */
    private[scalus] def failureCode(cause: Throwable): String = cause match
        case _: InvalidReturnValue                 => "INVALID_RETURN_VALUE"
        case _: OutOfExBudgetError                 => "OUT_OF_BUDGET"
        case _: BuiltinError | _: BuiltinException => "BUILTIN_FAILURE"
        case _: MachineError                       => "SCRIPT_FAILURE"
        case _                                     => "INTERNAL_ERROR"

    private[scalus] def fromException(
        e: PlutusScriptEvaluationException
    ): JPlutusScriptEvaluationError = {
        val redeemer = e.redeemer
            .map(r => new JRedeemerBudget(r.tag.toString, r.index, JExUnits(e.spentBudget)))
            .orUndefined
        new JPlutusScriptEvaluationError(
          e.getMessage,
          js.Array(e.logs*),
          redeemer,
          e.failedScriptHash.toHex,
          failureCode(e.getCause),
          if e.args.isEmpty then js.undefined
          else js.Array(e.args.map(arg => Hex.bytesToHex(arg.toCbor))*)
        )
    }
}
