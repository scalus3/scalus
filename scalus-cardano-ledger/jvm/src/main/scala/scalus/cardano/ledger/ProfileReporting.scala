package scalus.cardano.ledger

import scalus.uplc.eval.{ProfileFormatter, ProfilingData}

/** JVM renderer for [[PlutusScriptEvaluator]]'s built-in profile report.
  *
  * Lives in the platform layer (rather than inline in the shared evaluator) so the JS build can
  * substitute a no-op and keep [[scalus.uplc.eval.ProfileFormatter]] — including its HTML/CSS/JS
  * templates and the Tarjan inclusive-cost pass — dead-code-eliminated from the published
  * `scalus.js` bundle, which transaction builders depend on and which must stay small.
  */
private[ledger] object ProfileReporting {

    /** Render `data` in `format` (always `Some` on the JVM). For HTML, profiled source files
      * readable from the working directory are annotated with per-line cost.
      *
      * @note
      *   Called once per configured output, so a (rare) config with several HTML outputs re-reads
      *   the annotated source files per output — negligible next to the extra profiling evaluation
      *   pass that precedes it.
      */
    def render(
        data: ProfilingData,
        format: ProfileFormat,
        level: ProfileLevel,
        maxRows: Int
    ): Option[String] = Some(format match
        case ProfileFormat.Text =>
            if level == ProfileLevel.Full then ProfileFormatter.toText(data, maxRows)
            else ProfileFormatter.summary(data)
        case ProfileFormat.Csv  => ProfileFormatter.toCsv(data)
        case ProfileFormat.Html => ProfileFormatter.toHtml(data, ProfileFormatter.loadSources(data))
        case ProfileFormat.Json => ProfileFormatter.toJson(data))
}
