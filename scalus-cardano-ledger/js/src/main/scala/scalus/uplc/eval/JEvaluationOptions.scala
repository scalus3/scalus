package scalus.uplc.eval

import scalus.cardano.ledger.{CardanoInfo, CostModels, ExUnits, JsProtocolParams, Language, MajorProtocolVersion}
import scalus.interop.{TsName, TsType}
import scalus.uplc.BuiltinSemanticsVariant
import scalus.uplc.builtin.platform
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** How to run one script: the language, the protocol version, the cost model and, optionally, a
  * budget limit.
  */
@TsName("EvaluationOptions")
trait JEvaluationOptions extends js.Object {

    /** Which Plutus language the script is written in. The script bytes do not carry it, and the
      * same bytes cost differently under different languages.
      */
    @TsType("\"PlutusV1\" | \"PlutusV2\" | \"PlutusV3\"")
    val plutusVersion: String

    /** The Cardano major protocol version to evaluate under. It selects the builtin semantics and
      * the costing rules.
      */
    val protocolMajorVersion: Double

    /** That language's cost parameters, in protocol-parameter order, each a safe-integer `number`
      * or a `bigint`. The factories fill it with numbers.
      *
      * Any length is accepted. A builtin whose parameters the array does not reach is priced beyond
      * any budget, as in Plutus.
      */
    @TsType("readonly (number | bigint)[]")
    val costModel: js.Array[Double]

    /** The most the script may spend, for example the ex-units its redeemer declares. A script that
      * exceeds it stops with `OUT_OF_BUDGET`. Without it, execution is not bounded.
      */
    val maxBudget: js.UndefOr[JExUnitsLike] = js.undefined
}

/** Factories for `EvaluationOptions`. A plain object literal with the same fields works too. */
@JSExportTopLevel("EvaluationOptions")
object JEvaluationOptions {

    /** Options from a `ProtocolParams` handle, as returned by `CardanoInfo`, an emulator's
      * `getProtocolParameters()`, or `ProtocolParams.fromBlockfrostJson`.
      *
      * @throws TypeError
      *   if the parameters have no cost model for `plutusVersion`
      */
    @JSExport
    def fromProtocolParams(
        @TsType("\"PlutusV1\" | \"PlutusV2\" | \"PlutusV3\"") plutusVersion: String,
        params: JsProtocolParams
    ): JEvaluationOptions = {
        val language = this.language(plutusVersion)
        val underlying = params.underlying
        val costs = underlying.costModels.models.getOrElse(
          language.ordinal,
          typeError(s"no $language cost model")
        )
        js.Dynamic
            .literal(
              plutusVersion = plutusVersion,
              protocolMajorVersion = underlying.protocolVersion.major.toDouble,
              costModel = js.Array(costs.map(_.toDouble)*)
            )
            .asInstanceOf[JEvaluationOptions]
    }

    /** Options from Scalus's bundled mainnet snapshot at protocol version 11. It makes no network
      * request, so it does not follow later parameter changes.
      */
    @JSExport
    def mainnet(
        @TsType("\"PlutusV1\" | \"PlutusV2\" | \"PlutusV3\"") plutusVersion: String
    ): JEvaluationOptions =
        fromProtocolParams(plutusVersion, JsProtocolParams.wrap(CardanoInfo.mainnet.protocolParams))

    /** The machine an options record describes. Throws a `TypeError` for a record it cannot read.
      */
    private[eval] def machine(options: JEvaluationOptions): PlutusVM = {
        // Read untyped: a typed read of a wrong-typed field is undefined behaviour in Scala.js.
        val record = options.asInstanceOf[js.Dynamic]
        val language = this.language(record.plutusVersion)
        val protocol = MajorProtocolVersion(
          intOf(record.protocolMajorVersion, "protocolMajorVersion")
        )
        val costs = arrayOf(record.costModel, "costModel").map(longOf)
        val params =
            try
                MachineParams.fromCostModels(
                  CostModels(Map(language.ordinal -> costs)),
                  language,
                  protocol
                )
            catch case e: IllegalArgumentException => typeError(e.getMessage)
        new PlutusVM(
          language,
          params,
          BuiltinSemanticsVariant.fromProtocolAndPlutusVersion(protocol, language),
          platform,
          protocol
        )
    }

    /** The budget limit of an options record, if it has one. */
    private[eval] def maxBudget(options: JEvaluationOptions): Option[ExUnits] =
        JExUnits.exUnitsOf(options.asInstanceOf[js.Dynamic].maxBudget, "maxBudget")

    /** Compares rather than casts, so a value of any JS type is safe to pass. */
    private def language(value: js.Any): Language =
        Language.values
            .find(l => (l.toString: js.Any) == value)
            .getOrElse(typeError(s"unsupported plutusVersion: $value"))

}
