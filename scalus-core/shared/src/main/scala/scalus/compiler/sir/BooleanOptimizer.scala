package scalus.compiler.sir

/** Moved to [[scalus.compiler.sir.transform.BooleanOptimizer]], alongside the other SIR-to-SIR
  * passes. This alias forwards to the new location so existing callers keep compiling and linking.
  *
  * Note that the pass itself changed when it moved: several rules that could drop an operand the
  * original expression evaluates - and so delete an `Error` or a `trace` - were removed. See
  * `docs/internal/BOOLEAN_OPTIMIZER_FINDINGS.md`.
  */
@deprecated("use scalus.compiler.sir.transform.BooleanOptimizer instead", "1.1.1")
object BooleanOptimizer {

    /** Forwards to [[scalus.compiler.sir.transform.BooleanOptimizer.optimize]]. */
    def optimize(sir: SIR): SIR = transform.BooleanOptimizer.optimize(sir)
}
