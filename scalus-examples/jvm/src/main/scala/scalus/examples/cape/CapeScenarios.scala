package scalus.examples.cape

import scalus.examples.cape.ecd.EcdContract
import scalus.examples.cape.factorial.FactorialContract
import scalus.examples.cape.fibonacci.FibonacciContract
import scalus.examples.cape.htlc.HtlcContract
import scalus.examples.cape.linearvesting.LinearVestingContract
import scalus.examples.cape.twopartyescrow.TwoPartyEscrowContract
import scalus.uplc.Program

/** One CAPE submission scenario: a scenario name (matches the UPLC-CAPE `submissions/<name>/`
  * directory), a lazily-compiled program, and the metadata/README text describing it.
  *
  * @param minPlutusVersion
  *   Minimum plutus-core version required to evaluate this submission (CAPE
  *   `metadata.schema.json`'s `compilation_config.min_plutus_version`). Set for scenarios using
  *   PV11/vanRossem-only features -- either `@Compile`'d via `Options.release` (flexible
  *   case-on-builtins, batch-6 builtins) or hand-crafted UPLC that directly emits a `case` on a raw
  *   builtin value (e.g. `fibonacci`'s O(1) lookup, cased directly on the input `Integer` itself,
  *   after an outer `case` on the `Bool` from `lessThanEqualsInteger` to route negative inputs) --
  *   CAPE's production evaluator is still pinned to plutus-core 1.45.0.0 (pre-vanRossem), so these
  *   only evaluate against CAPE's `_preview` evaluator/report track. `None` for scenarios that
  *   don't need PV11 semantics.
  */
case class CapeScenario(
    name: String,
    program: () => Program,
    implementationNotes: String,
    readmeApproach: String,
    sourceSubdir: String,
    minPlutusVersion: Option[String] = None
)

/** Registry of all Scalus CAPE benchmark submissions. */
object CapeScenarios {
    private val compiledNote =
        "Compiled from Scala 3 with the Scalus compiler plugin, Options.release " +
            "(PV11/vanRossem target: flexible case, batch-6 builtins), no traces."

    val all: List[CapeScenario] = List(
      CapeScenario(
        "factorial",
        () => FactorialContract.openProgram,
        "Hand-crafted UPLC: memoized base case + computed fallback, correct for every integer " +
            "input. x < 0 -> 1; 0 <= x <= 12 -> O(1) lookup via PV11 case-on-builtin-integer " +
            "(0!..12! baked in as direct case branches, no bytestring decode step); x >= 13 -> " +
            "genuine self-application recursion (x * factorial(x - 1)) bottoming out on the " +
            "table. A PV9-compatible sliceByteString/byteStringToInteger encoding of the table " +
            "was also measured and lost decisively (109 vs 91 bytes; 18.4M vs 4.5M summed steps " +
            "across the 10 fixture cases) -- see FactorialOpen.scala.",
        "Hand-written UPLC term: memoized 0!..12! base case (PV11 case-on-builtin-integer) plus " +
            "self-application recursion for x >= 13, so it's correct for any integer input, not " +
            "just the benchmark's fixture range. Optimized by the Scalus UPLC pipeline.",
        "factorial",
        minPlutusVersion = Some("1.60.0.0")
      ),
      CapeScenario(
        "factorial_naive_recursion",
        () => FactorialContract.baseProgram,
        compiledNote,
        "Direct @Compile of the prescribed naive recursion.",
        "factorial",
        minPlutusVersion = Some("1.60.0.0")
      ),
      CapeScenario(
        "fibonacci",
        () => FibonacciContract.openProgram,
        "Hand-crafted UPLC: memoized base case + computed fallback, correct for every integer " +
            "input. n < 0 -> n; 0 <= n <= 25 -> O(1) lookup via PV11 case-on-builtin-integer " +
            "(fib(0)..fib(25) baked in as direct case branches, no bytestring decode step); " +
            "n >= 26 -> a linear accumulator loop (go(k, a, b) = if k <= 0 then b else " +
            "go(k - 1, b, a + b), seeded from fib(24)/fib(25)) bottoming out on the table -- " +
            "O(n), not the O(2^n) naive double recursion the scenario warns against.",
        "Hand-written UPLC term: memoized fib(0)..fib(25) base case (PV11 " +
            "case-on-builtin-integer) plus a linear accumulator loop for n >= 26, so it's " +
            "correct for any integer input, not just the benchmark's fixture range. Optimized " +
            "by the Scalus UPLC pipeline.",
        "fibonacci",
        minPlutusVersion = Some("1.60.0.0")
      ),
      CapeScenario(
        "fibonacci_naive_recursion",
        () => FibonacciContract.baseProgram,
        compiledNote,
        "Direct @Compile of the prescribed naive recursion.",
        "fibonacci",
        minPlutusVersion = Some("1.60.0.0")
      ),
      CapeScenario(
        "ecd",
        () => EcdContract.program,
        compiledNote + " Direct translation of the prescribed Euclidean algorithm; " +
            "the only compiler-automatic transforms are recursion encoding and inlining.",
        "Direct @Compile of the prescribed naive recursive GCD.",
        "ecd",
        minPlutusVersion = Some("1.60.0.0")
      ),
      CapeScenario(
        "htlc",
        () => HtlcContract.program,
        compiledNote,
        "Scala 3 validator: SHA-256 preimage claim before timeout, payer refund after.",
        "htlc",
        minPlutusVersion = Some("1.60.0.0")
      ),
      CapeScenario(
        "linear_vesting",
        () => LinearVestingContract.program,
        compiledNote + " Value.quantityOf lowers to the CIP-0153 lookupCoin/unValueData " +
            "builtins (Options.valueBuiltins, the PV11 default).",
        "Scala 3 validator implementing the ceiling-division vesting schedule.",
        "linearvesting",
        minPlutusVersion = Some("1.60.0.0")
      ),
      CapeScenario(
        "two_party_escrow",
        () => TwoPartyEscrowContract.compiled.program,
        compiledNote,
        "Scala 3 validator: Deposited -> Accepted | Refunded state machine.",
        "twopartyescrow",
        minPlutusVersion = Some("1.60.0.0")
      )
    )
}
