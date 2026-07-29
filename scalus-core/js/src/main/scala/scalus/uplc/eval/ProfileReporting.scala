package scalus.uplc.eval

import scalus.cardano.ledger.{ProfileFormat, ProfileLevel}

/** JS stub for [[scalus.cardano.ledger.PlutusScriptEvaluator]]'s built-in profile report.
  *
  * The report's text/CSV/HTML/JSON rendering (and its file output) is a JVM/Node debugging feature.
  * It is intentionally NOT wired into the JS build so that [[ProfileFormatter]] and its HTML/CSS/JS
  * templates are dead-code-eliminated from the published `scalus.js` bundle, which transaction
  * builders depend on and which must stay small. For JS-side profiling, use
  * `Scalus.evaluateScriptProfile`, which returns the raw profile as JSON.
  */
private[scalus] object ProfileReporting {

    /** Always `None`: the built-in evaluator report is unavailable in the JS build. */
    def render(
        data: ProfilingData,
        format: ProfileFormat,
        level: ProfileLevel,
        maxRows: Int
    ): Option[String] = None
}
