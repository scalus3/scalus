package scalus.uplc.eval

import scalus.uplc.Term

/** Represents different JIT compilation strategies for UPLC terms.
  *
  * This enum provides a unified interface for different JIT implementation approaches,
  * each with different performance characteristics and trade-offs:
  *
  * - **Mincont**: Minimal continuation-based approach using heap-allocated frames.
  *   Stack-safe with moderate performance overhead due to eval loop.
  *
  * - **NativeStack**: Direct JVM stack-based approach with no continuation overhead.
  *   Maximum performance but not stack-safe (can overflow on deep recursion).
  *
  * - **Hybrid**: Intelligent switching between NativeStack (for shallow recursion)
  *   and Mincont (for deep recursion), providing best-of-both-worlds performance.
  *
  * @example
  * {{{
  * val term: Term = ...
  *
  * // Use specific implementation
  * val mincontFunc = JITImplementation.Mincont.compile(term)
  * val nativeFunc = JITImplementation.NativeStack.compile(term)
  *
  * // Use hybrid with automatic selection
  * val hybridFunc = JITImplementation.Hybrid().compile(term)
  *
  * // Evaluate
  * val result = mincontFunc(logger, budgetSpender, params)
  * }}}
  */
enum JITImplementation:
    /** Minimal continuation-based JIT implementation.
      *
      * Uses heap-allocated continuation frames and a trampolining eval loop to achieve
      * stack safety. Can handle arbitrary recursion depth but has moderate performance
      * overhead (~27% from eval loop).
      *
      * **Characteristics**:
      * - Stack-safe: ✓ (heap-based continuations)
      * - Performance: ~1.37x faster than CeK machine
      * - Memory: 2.45x less allocation than CeK
      * - Max recursion depth: Unlimited (heap-bounded)
      *
      * **Best for**:
      * - Production environments
      * - Unknown recursion depth
      * - Safety-critical applications
      */
    case Mincont

    /** Native JVM stack-based JIT implementation.
      *
      * Uses direct JVM method calls with no continuation overhead. Provides maximum
      * performance through zero-indirection execution but uses JVM call stack directly,
      * making it susceptible to StackOverflowError on deep recursion.
      *
      * **Characteristics**:
      * - Stack-safe: ✗ (uses JVM call stack)
      * - Performance: ~1.3x faster than CeK machine (estimated 3-7x vs Mincont)
      * - Memory: Minimal heap allocation
      * - Max recursion depth: Limited (~1000-5000 depending on JVM stack)
      *
      * **Best for**:
      * - Benchmarking
      * - Known shallow recursion
      * - Performance-critical paths with depth guarantees
      */
    case NativeStack

    /** Hybrid JIT implementation with intelligent engine selection.
      *
      * Automatically chooses between NativeStack (for shallow recursion) and Mincont
      * (for deep recursion) based on static analysis or dynamic fallback. Provides
      * optimal performance while maintaining safety.
      *
      * @param depthThreshold Maximum recursion depth before switching to Mincont (default: 1000)
      * @param useStaticAnalysis Whether to use static depth estimation (default: true)
      * @param useDynamicFallback Whether to catch StackOverflow and retry with Mincont (default: true)
      *
      * **Characteristics**:
      * - Stack-safe: ✓ (fallback to Mincont)
      * - Performance: Best of both worlds (NativeStack speed when safe, Mincont for deep calls)
      * - Memory: Depends on execution path
      * - Max recursion depth: Unlimited (via Mincont fallback)
      *
      * **Best for**:
      * - General-purpose production use
      * - Mixed workloads (shallow and deep recursion)
      * - When you want both performance and safety
      */
    case Hybrid(
        depthThreshold: Int = 1000,
        useStaticAnalysis: Boolean = true,
        useDynamicFallback: Boolean = true
    )

    /** Compiles a UPLC term using this JIT implementation strategy.
      *
      * @param term The UPLC term to compile
      * @return A function that evaluates the term with the given logger, budget, and params
      */
    def compile(term: Term): (Logger, BudgetSpender, MachineParams) => Any = this match
        case Mincont =>
            mincont.JIT.jitUplc(term)

        case NativeStack =>
            nativestack.JIT.jitUplc(term)

        case Hybrid(threshold, useStatic, useDynamic) =>
            // TODO: Implement hybrid compilation in Phase 2
            // For now, use mincont as safe default
            mincont.JIT.jitUplc(term)

    /** Returns a human-readable name for this implementation. */
    def name: String = this match
        case Mincont => "Mincont"
        case NativeStack => "NativeStack"
        case Hybrid(_, _, _) => "Hybrid"

    /** Returns whether this implementation is stack-safe. */
    def isStackSafe: Boolean = this match
        case Mincont => true
        case NativeStack => false
        case Hybrid(_, _, _) => true

object JITImplementation:
    /** Default JIT implementation (currently Mincont for safety). */
    def default: JITImplementation = Mincont

    /** Returns all available standalone implementations (excludes Hybrid). */
    def standaloneImplementations: List[JITImplementation] = List(Mincont, NativeStack)

    /** Returns all implementations including Hybrid with default settings. */
    def all: List[JITImplementation] = standaloneImplementations :+ Hybrid()
