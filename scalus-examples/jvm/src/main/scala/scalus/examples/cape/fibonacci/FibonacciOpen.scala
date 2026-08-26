package scalus.examples.cape.fibonacci

import scalus.uplc.DefaultFun.{AddInteger, LessThanEqualsInteger, SubtractInteger}
import scalus.uplc.Term
import scalus.uplc.Term.{asTerm, λ}

import scala.language.implicitConversions

/** CAPE fibonacci open mode implementation.
  *
  * `fibonacci(n) = if n < 0 then n else if n <= 25 then table(n) else go(n - 25, fib(24), fib(25))`:
  * the `fib(0)..fib(25)` table is a memoized base case, not the whole implementation -- for
  * `n >= 26` the program falls back to a linear accumulator loop (`go`), so it is correct for every
  * integer input, not just the benchmark's `0..25`/`-1` fixture range (see the
  * `fib26`/`fib30`/`fib40`/`fib60`/negative correctness-gate tests in `FibonacciCapeTest`).
  *
  * `go(k, a, b) = if k <= 0 then b else go(k - 1, b, a + b)`, invoked as
  * `go(n - 25, fib(24), fib(25))`. Correctness: with `a = fib(m - 1)`, `b = fib(m)`, one step
  * advances `m` by one (`a' = b = fib(m)`, `b' = a + b = fib(m - 1) + fib(m) = fib(m + 1)`) while
  * decrementing `k`, and `k = 0` returns `b` unchanged. So `go(n - 25, fib(24), fib(25)) = fib(n)`
  * for every `n >= 25`. This is genuine self-application recursion (`pfix`, the same idiom
  * `FactorialOpen` uses for its `x >= 13` fallback), but *linear* in `n`, not exponential like
  * naive double recursion -- the "iterative approach with accumulators" the scenario's open mode
  * explicitly permits.
  *
  * For `0 <= n <= 25`, `fib(n)` is looked up directly via 26 UPLC 1.1.0 "case-on-builtins"
  * (PV11/vanRossem) branches, cased on `n` itself -- the CEK machine picks branch `i` when the
  * scrutinee is the raw integer `i`, so the selected branch is returned with zero further builtin
  * calls (no decode/slice step at all). For `n < 0` it returns `n` itself, matching the scenario's
  * documented "current implementation" semantics.
  *
  * Unlike `sliceByteString`, case-on-builtin-integer *errors* (`CaseIndexOutOfBounds`) on a
  * scrutinee outside the case list, so a negative `n` cannot share the `0 <= n <= 25` case -- this
  * needs an explicit outer `n < 0` guard (cased on the `Bool` from `lessThanEqualsInteger`, to
  * avoid an `ifThenElse` builtin call), then an inner `n <= 25` guard routing to the
  * case-on-integer table vs. the `go` fallback for `n >= 26`.
  *
  * Requires `min_plutus_version = 1.60.0.0` (set on the `"fibonacci"` entry in
  * `CapeScenarios.scala`) since case-on-builtins (both the outer case-on-`Bool` and the inner
  * case-on-`Integer`) is a PV11-only feature.
  */
object FibonacciOpen {

    /** Largest input the table covers; above it the program falls back to the `go` accumulator
      * loop. Chosen as the benchmark's largest fixture input -- every value up to it is memoized,
      * everything past it is computed.
      */
    private val MaxTableInput: Int = 25

    /** `fib(0)..fib(MaxTableInput)` -- the memoized values, one constant per case-on-integer
      * branch, and the source of the seed pair (`fibValues(MaxTableInput - 1)`,
      * `fibValues(MaxTableInput)`) the `go` fallback starts from.
      *
      * Computed here by [[FibonacciBase.fibonacci]] -- the same plain recursive Scala function that
      * compiles to the `fibonacci_naive_recursion` submission -- running on the JVM. One definition
      * therefore does three jobs: it ships as its own CAPE submission, it generates this
      * submission's table, and it defines the semantics both must agree on (asserted in
      * `FibonacciCapeTest`).
      */
    private val fibValues: scala.List[BigInt] =
        (0 to MaxTableInput).map(FibonacciBase.fibonacci(_)).toList

    private def pfix(f: Term => Term): Term =
        λ { r => r $ r } $ λ { r => f(r $ r) }

    /** `go(k, a, b) = if k <= 0 then b else go(k - 1, b, a + b)` as a self-application fixpoint --
      * see the module scaladoc for the correctness argument.
      */
    private def go: Term = {
        import scalus.uplc.TermDSL.given

        pfix: r =>
            λ: k =>
                λ: a =>
                    λ: b =>
                        // Bool is a 2-constructor value under case-on-builtins: False=tag 0, True=tag 1.
                        Term.Case(
                          LessThanEqualsInteger $ k $ 0,
                          scala.List(
                            // branch 0 (`k <= 0` is False, i.e. k >= 1): one more Fibonacci step.
                            r $ (SubtractInteger $ k $ 1) $ b $ (AddInteger $ a $ b),
                            // branch 1 (`k <= 0` is True, i.e. k == 0): base case, b already = fib(n).
                            b
                          )
                        )
    }

    def term: Term = {
        import scalus.uplc.TermDSL.given

        // Bool is a 2-constructor value under case-on-builtins: False=tag 0, True=tag 1.
        λ: n =>
            Term.Case(
              LessThanEqualsInteger $ n $ -1,
              scala.List(
                // branch 0 (`n <= -1` is False, i.e. n >= 0): range-check against the table.
                Term.Case(
                  LessThanEqualsInteger $ n $ MaxTableInput,
                  scala.List(
                    // branch 0 (`n <= 25` is False, i.e. n >= 26): the linear accumulator fallback.
                    go $ (SubtractInteger $ n $ MaxTableInput) $
                        fibValues(MaxTableInput - 1).asTerm $ fibValues(MaxTableInput).asTerm,
                    // branch 1 (`n <= 25` is True, i.e. 0 <= n <= 25): O(1) table lookup.
                    Term.Case(n, fibValues.map(_.asTerm))
                  )
                ),
                // branch 1 (`n <= -1` is True, i.e. n < 0): matches fibonacci(n) = n.
                n
              )
            )
    }
}
