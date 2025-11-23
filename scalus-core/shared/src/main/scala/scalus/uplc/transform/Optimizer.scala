package scalus.uplc.transform

import scalus.uplc.Term

/** Base trait for UPLC term optimizers.
  *
  * An optimizer transforms a UPLC (Untyped Plutus Core) term into an equivalent but more efficient
  * term. Optimizers can perform various transformations such as:
  *   - Dead code elimination
  *   - Function inlining and beta-reduction
  *   - Eta-reduction (removing redundant lambda abstractions)
  *   - Constant folding
  *   - Conversion of lazy evaluation to strict evaluation when safe
  *
  * ==Usage==
  *
  * Optimizers are typically used in optimization pipelines (see [[V1V2Optimizer]] and
  * [[V3Optimizer]]) where multiple optimization passes are applied sequentially:
  *
  * {{{
  * val optimizer = new StrictIf()
  * val optimizedTerm = optimizer(term)
  * println(s"Optimizations applied: \${optimizer.logs.mkString(", ")}")
  * }}}
  *
  * ==Implementation Notes==
  *
  * Optimizer implementations should:
  *   - Accept a [[scalus.uplc.eval.Logger]] as constructor parameter for logging optimizations
  *   - Be deterministic: applying the same optimizer to the same term should always produce the
  *     same result
  *   - Preserve semantics: the optimized term must be equivalent to the original term
  *   - Log all applied optimizations for debugging and analysis
  *
  * @see
  *   [[StrictIf]] for lazy-to-strict if-then-else conversion
  * @see
  *   [[EtaReduce]] for eta-reduction optimization
  * @see
  *   [[Inliner]] for function inlining and dead code elimination
  * @see
  *   [[ForcedBuiltinsExtractor]] for extracting forced builtins to top level
  * @see
  *   [[CaseConstrApply]] for optimizing nested Apply with Case/Constr (Plutus V3)
  */
trait Optimizer {

    /** Applies the optimization to a UPLC term.
      *
      * @param term
      *   The UPLC term to optimize
      * @return
      *   The optimized UPLC term, semantically equivalent to the input
      */
    def apply(term: Term): Term

    /** Returns the log messages from optimization operations.
      *
      * Each log entry describes an optimization that was applied, useful for debugging and
      * understanding the optimization process.
      *
      * @return
      *   Sequence of log messages describing applied optimizations
      */
    def logs: Seq[String]
}

/** No-op optimizer that returns the term unchanged without any optimizations.
  *
  * This is useful as a default when optimization is disabled or as a placeholder in testing.
  */
object NoopOptimizer extends Optimizer {
    def apply(term: Term): Term = term
    def logs: Seq[String] = Seq.empty
}
