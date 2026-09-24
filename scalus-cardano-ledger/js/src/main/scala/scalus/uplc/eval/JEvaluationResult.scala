package scalus.uplc.eval

import scalus.cardano.ledger.ExUnits
import scalus.interop.{TsName, TsType}
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Execution units: what a script costs to run, in abstract machine memory and steps. A transaction
  * pays a fee for the units its scripts declare, and the ledger rejects it if a script goes over
  * what it declared.
  */
@JSExportTopLevel("ExUnits")
class JExUnits(val memory: js.BigInt, val steps: js.BigInt) extends js.Object {

    /** The units as decimal strings, so `JSON.stringify` works: it throws on a `bigint`. */
    @TsType("{ memory: string; steps: string }")
    def toJSON(): js.Object = js.Dynamic.literal(memory = memory.toString, steps = steps.toString)
}

object JExUnits {
    private[scalus] def apply(units: ExUnits): JExUnits =
        new JExUnits(units.memory.toJsBigInt, units.steps.toJsBigInt)

    /** The units of an [[JExUnitsLike]] argument, or `None` for `undefined` or `null`.
      *
      * @throws TypeError
      *   if `memory` or `steps` is not a safe integer or a `bigint` that fits 64 bits
      */
    private[scalus] def exUnitsOf(value: js.Any, name: String): Option[ExUnits] =
        if js.isUndefined(value) || value == null then None
        else
            val units = value.asInstanceOf[js.Dynamic]
            Some(
              ExUnits(
                memory = longOf(units.memory, s"$name.memory"),
                steps = longOf(units.steps, s"$name.steps")
              )
            )
}

/** Execution units as a plain record: `memory` and `steps`, each a safe-integer `number` or a
  * `bigint`. An `ExUnits` fits, and so does any `{ memory, steps }` object.
  */
@TsName("ExUnitsLike")
trait JExUnitsLike extends js.Object {
    @TsType("number | bigint") val memory: js.Any
    @TsType("number | bigint") val steps: js.Any
}

/** Why a script did not succeed.
  *
  * Branch on [[code]] rather than on [[message]], whose wording may improve. `INTERNAL_ERROR` means
  * a defect in Scalus, not a problem with the script: report it.
  */
@TsName("EvaluationError")
trait JEvaluationError extends js.Object {

    /** `SCRIPT_FAILURE`: the script failed, for example by evaluating `error`. `BUILTIN_FAILURE`: a
      * builtin rejected its arguments. `INVALID_RETURN_VALUE`: a Plutus V3 script returned
      * something other than unit. `OUT_OF_BUDGET`: the script spent more than `maxBudget`.
      * `INTERNAL_ERROR`: a defect in Scalus.
      */
    @TsType(
      "\"SCRIPT_FAILURE\" | \"BUILTIN_FAILURE\" | \"INVALID_RETURN_VALUE\" | \"OUT_OF_BUDGET\" | \"INTERNAL_ERROR\""
    )
    val code: String

    /** What went wrong, in prose. The wording may change between releases. */
    val message: String
}

/** Outcome of evaluating one Plutus script. Read `isSuccess` first.
  *
  * @param isSuccess
  *   Whether the script ran to completion and returned a value the ledger accepts.
  * @param budget
  *   Units the machine spent. On failure, what was spent before the script failed.
  * @param logs
  *   Trace output the script emitted, oldest first. From the deprecated `evaluateScript` and
  *   `evaluateScriptProfile`, a failure's message comes first.
  * @param profileJson
  *   Profiling data as JSON; set only by the deprecated `evaluateScriptProfile`.
  */
@JSExportTopLevel("EvaluationResult")
@JSExportTopLevel("Result")
class JEvaluationResult(
    val isSuccess: Boolean,
    val budget: JExUnits,
    val logs: js.Array[String],
    val profileJson: js.UndefOr[String]
) extends js.Object {

    /** Why the script did not succeed. Set by `evaluator.evaluateScript` on failure. */
    def error: js.UndefOr[JEvaluationError] = js.undefined
}

object JEvaluationResult {

    /** The result `evaluator.evaluateScript` returns: `logs` holds the traces only. */
    private[eval] def of(result: Result): JEvaluationResult = result match
        case s: Result.Success =>
            new JEvaluationResult(true, JExUnits(s.budget), js.Array(s.logs*), js.undefined)
        case f: Result.Failure =>
            val r =
                new JEvaluationResult(false, JExUnits(f.budget), js.Array(f.logs*), js.undefined)
            val error = js.Dynamic.literal(
              code = JPlutusScriptEvaluationError.failureCode(f.exception),
              message = f.exception.getMessage
            )
            // An own enumerable property, shadowing the getter, so `JSON.stringify` and
            // `Object.keys` see it.
            js.Object.defineProperty(
              r,
              "error",
              js.Dynamic
                  .literal(value = error, enumerable = true)
                  .asInstanceOf[js.PropertyDescriptor]
            )
            r

    /** The result of the deprecated entry points: a failure's message is `logs[0]`. */
    private[eval] def legacy(result: Result): JEvaluationResult = {
        val profileJson: js.UndefOr[String] = result.profile match
            case Some(p) => ProfileFormatter.toJson(p)
            case None    => js.undefined
        result match
            case s: Result.Success =>
                new JEvaluationResult(true, JExUnits(s.budget), js.Array(s.logs*), profileJson)
            case f: Result.Failure =>
                new JEvaluationResult(
                  false,
                  JExUnits(f.budget),
                  js.Array(f.exception.getMessage +: f.logs*),
                  profileJson
                )
    }

    /** The deprecated entry points' result for a script that could not be read at all. */
    private[eval] def unreadable(message: String): JEvaluationResult =
        new JEvaluationResult(false, JExUnits(ExUnits.zero), js.Array(message), js.undefined)
}
