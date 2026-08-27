package scalus.uplc.internal

import scalus.cardano.ledger.EvaluatorReportConfig
import scalus.uplc.Term
import scalus.uplc.eval.ProfilingData

/** JS no-op stub for the profile report writer.
  *
  * Profile reports on disk (text/CSV/HTML/JSON files, `profile-manifest.json`, the `.uplc.json`
  * source map) are a JVM debugging feature. Stubbing the writer — rather than compiling the shared
  * implementation against a no-op renderer — keeps `ProfileFormatter` (HTML/CSS/JS templates,
  * Tarjan pass), the jsoniter manifest codec and the UPLC source-map renderer dead-code-eliminated
  * from the published `scalus.js` bundle, which transaction builders depend on and which must stay
  * small. For JS-side profiling, use `Scalus.evaluateScriptProfile`, which returns the raw profile
  * as JSON.
  */
object ProfileReportWriter {

    /** Does nothing on JS; see the object note. The signature matches the JVM implementation so
      * shared callers ([[scalus.cardano.ledger.PlutusScriptEvaluator]], `ScalusTest`) compile
      * unchanged.
      */
    def write(
        data: ProfilingData,
        report: EvaluatorReportConfig,
        scriptHash: String,
        language: String,
        redeemerTag: String,
        redeemerIndex: Int,
        onConsole: String => Unit,
        uplcTerm: Option[Term] = None
    ): Unit = ()
}
