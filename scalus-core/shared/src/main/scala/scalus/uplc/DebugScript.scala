package scalus.uplc

import scalus.cardano.ledger.PlutusScript

/** A debug-compiled Plutus script for diagnostic replay.
  *
  * When a release script (compiled without error traces) fails during evaluation with empty logs,
  * the evaluator can use a DebugScript to replay the failing evaluation with traces enabled,
  * producing diagnostic logs.
  *
  * DebugScript wraps either:
  *   - A pre-compiled debug [[PlutusScript]] (for external builders like meshJS that have CBOR)
  *   - A lazy computation from [[CompiledPlutus]] (deferred SIR recompilation until failure)
  *
  * @param compute
  *   lazy computation that produces the debug PlutusScript
  */
class DebugScript(compute: () => PlutusScript) {
    lazy val plutusScript: PlutusScript = compute()
}

object DebugScript {

    /** Creates a DebugScript from a pre-compiled debug PlutusScript.
      *
      * Use this when you already have a debug-compiled script in CBOR form (e.g., from an external
      * transaction builder).
      *
      * @param script
      *   the debug-compiled PlutusScript with traces enabled
      */
    def apply(script: PlutusScript): DebugScript =
        new DebugScript(() => script)

    /** Creates a DebugScript from a CompiledPlutus, deferring recompilation with error traces.
      *
      * The SIR recompilation to enable traces is lazy — it only happens if/when the release script
      * fails and diagnostic replay is triggered.
      *
      * @param compiled
      *   the compiled Plutus script to recompile with error traces on failure
      */
    def fromCompiled(compiled: CompiledPlutus[?]): DebugScript =
        new DebugScript(() => compiled.withErrorTraces.script)
}
