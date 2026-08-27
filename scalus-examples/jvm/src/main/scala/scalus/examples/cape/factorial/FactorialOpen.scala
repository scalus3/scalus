package scalus.examples.cape.factorial

import scalus.uplc.DefaultFun.{ByteStringToInteger, IfThenElse, LessThanEqualsInteger, MultiplyInteger, SliceByteString, SubtractInteger}
import scalus.uplc.Term
import scalus.uplc.Term.{asTerm, λ}
import scalus.uplc.builtin.ByteString

import scala.language.implicitConversions

/** CAPE factorial open mode implementation.
  *
  * `factorial(x) = if x < 0 then 1 else if x <= 12 then table(x) else x * factorial(x - 1)`: the
  * `0!..12!` table is a memoized base case, not the whole implementation -- for `x >= 13` the
  * program falls back to genuine self-application recursion (`x * factorial(x - 1)`, bottoming out
  * on the table), so it is correct for every integer input, not just the benchmark's `-5..12`
  * fixture range (see the `factorial13`/`factorial15`/`factorial20`/`factorial25`/negative
  * correctness-gate tests in `FactorialCapeTest`).
  *
  * Two independent encodings of the table were implemented and measured across all 10 open-mode
  * fixture cases (`FactorialVariantSpike`, since deleted -- numbers preserved here and in
  * `docs/internal/CAPE_COMPETITIVE_ANALYSIS.md`):
  *
  *   - `termA` (PV9-compatible, `sliceByteString`/`byteStringToInteger` decode): 109-byte script,
  *     18,413,280 summed steps / 36,090 summed mem across the 10 cases.
  *   - `termB` (PV11 case-on-builtins, adopted as `term`): 91-byte script, 4,545,903 summed steps
  *     (-75.3%) / 24,219 summed mem (-32.9%) across the 10 cases.
  *
  * `termB` wins on every axis (script size, steps, mem), so it's `term`. The cost: `termB` needs
  * PV11/vanRossem case-on-builtins, so adopting it means `factorial` (open) now needs
  * `min_plutus_version = 1.60.0.0` (see `CapeScenarios.scala`) -- it was previously CAPE's *only*
  * ungated Scalus submission (evaluated on CAPE's production track); it now joins the other 7 on
  * CAPE's preview track, like every other Scalus CAPE submission, pending upstream promoting its
  * production evaluator past vanRossem.
  */
object FactorialOpen {

    /** Largest input the table covers; above it both variants fall back to recursion. Chosen as the
      * benchmark's largest fixture input -- every value up to it is memoized, everything past it is
      * computed.
      */
    private val MaxTableInput: Int = 12

    /** `0!..MaxTableInput!` -- the memoized values, and the single source of truth for both
      * encodings: one constant per case-on-integer branch in `termB`, and the bytes packed into
      * `table` for `termA`.
      *
      * Computed here by [[FactorialBase.factorial]] -- the same plain recursive Scala function that
      * compiles to the `factorial_naive_recursion` submission -- running on the JVM. One definition
      * therefore does three jobs: it ships as its own CAPE submission, it generates this
      * submission's table, and it defines the semantics both must agree on (asserted in
      * `FactorialCapeTest`).
      */
    private val factorials: scala.List[BigInt] =
        (0 to MaxTableInput).map(FactorialBase.factorial(_)).toList

    /** Width in bytes of one `table` entry: the smallest that still holds the largest entry (`12! =
      * 479001600` needs 4), so the packing follows `MaxTableInput` rather than pinning a width that
      * a wider table would silently overflow.
      */
    private val EntryBytes: Int = (factorials.last.bitLength + 7) / 8

    /** `factorials` packed as fixed-width big-endian entries, used by `termA`'s
      * `sliceByteString`/`byteStringToInteger` decode. Derived rather than hand-written so the two
      * encodings cannot drift apart; `FactorialCapeTest` evaluates `termA` against Scala's `BigInt`
      * factorial, which fails on a wrong entry, width, or endianness.
      */
    private val table: ByteString =
        factorials.map(n => ByteString.fromBigIntBigEndian(n, EntryBytes)).reduce(_.concat(_))

    private def pfix(f: Term => Term): Term =
        λ { r => r $ r } $ λ { r => f(r $ r) }

    /** Variant A (PV9-compatible): the `0!..12!` table is a `ByteString` decoded via
      * `sliceByteString`/`byteStringToInteger`/`multiplyInteger` (the original lookup technique),
      * gated by a single `x <= 12` check.
      *
      * Only one guard is needed, not two: `sliceByteString`'s `from` argument clamps rather than
      * erroring on a negative start (it is `drop`/`take` internally, and Scala's/Haskell's `drop`
      * on a negative count is a no-op) -- verified by evaluation in `FactorialCapeTest` (the
      * `-5`/`0`/`1` fixture and correctness-gate cases). So `x <= 0` (including every negative
      * input) already reads `table` entry `0` (`= 1`) through the *same* `x <= 12` branch as
      * `0 <= x <= 12`, with no separate `x <= 0` guard required. `x >= 13` falls through to
      * self-application recursion (`pfix`) bottoming out on this table.
      *
      * Uses only PV9-compatible builtins (`sliceByteString`, `byteStringToInteger`,
      * `multiplyInteger`, `subtractInteger`, a `force`d `ifThenElse`) -- no PV11 "case-on-builtins"
      * is required, so this variant needs no `min_plutus_version` gate.
      *
      * Measured (all 10 open-mode fixture cases): 109-byte script, 18,413,280 summed steps, 36,090
      * summed mem. Lost to `termB` on every axis -- kept here for reference, not adopted as `term`.
      *
      * Package-private rather than private so `FactorialCapeTest` can evaluate it: it is not on any
      * shipped path, so its correctness (and that of the derived `table` it decodes) would
      * otherwise go unchecked.
      */
    private[factorial] def termA: Term = {
        import scalus.uplc.TermDSL.given

        pfix: r =>
            λ: x =>
                !(!IfThenElse $ (LessThanEqualsInteger $ x $ MaxTableInput) $
                    ~(ByteStringToInteger $ true.asTerm $
                        (SliceByteString $ (MultiplyInteger $ x $ EntryBytes) $
                            EntryBytes.asTerm $ table.asTerm)) $
                    ~(MultiplyInteger $ x $ (r $ (SubtractInteger $ x $ 1))))
    }

    /** Variant B (PV11/vanRossem): the `0!..12!` table is 13 UPLC 1.1.0 "case-on-builtins" branches
      * cased directly on `x` itself (the technique `FibonacciOpen` uses) -- the CEK machine picks
      * branch `i` when the scrutinee is the raw integer `i`, so the selected branch is returned
      * with zero decode step (no `sliceByteString`/`byteStringToInteger`/`multiplyInteger` chain).
      *
      * Unlike `sliceByteString`, case-on-builtin-integer *errors* (`CaseIndexOutOfBounds`) on a
      * scrutinee outside the case list, so a negative `x` cannot share the `0 <= x <= 12` case the
      * way it does in `termA` -- this variant needs an explicit outer `x < 0` guard (cased on the
      * `Bool` from `lessThanEqualsInteger`, matching `FibonacciOpen`'s style, to avoid an
      * `ifThenElse` builtin call), then an inner `x <= 12` guard routing to the case-on-integer
      * table vs. self-application recursion for `x >= 13`.
      *
      * Requires `min_plutus_version = 1.60.0.0` (case-on-builtins is PV11-only) -- set on the
      * `"factorial"` entry in `CapeScenarios.scala`.
      *
      * Measured (all 10 open-mode fixture cases): 91-byte script, 4,545,903 summed steps, 24,219
      * summed mem. Won on every axis vs. `termA` -- adopted as `term`.
      */
    private def termB: Term = {
        import scalus.uplc.TermDSL.given

        pfix: r =>
            λ: x =>
                // Bool is a 2-constructor value under case-on-builtins: False=tag 0, True=tag 1.
                Term.Case(
                  LessThanEqualsInteger $ x $ -1,
                  scala.List(
                    // branch 0 (`x <= -1` is False, i.e. x >= 0): range-check against the table.
                    Term.Case(
                      LessThanEqualsInteger $ x $ MaxTableInput,
                      scala.List(
                        // branch 0 (`x <= 12` is False, i.e. x >= 13): recurse.
                        MultiplyInteger $ x $ (r $ (SubtractInteger $ x $ 1)),
                        // branch 1 (`x <= 12` is True, i.e. 0 <= x <= 12): table lookup.
                        Term.Case(x, factorials.map(_.asTerm))
                      )
                    ),
                    // branch 1 (`x <= -1` is True, i.e. x < 0): matches `factorial(x) = 1`.
                    1.asTerm
                  )
                )
    }

    /** Adopted implementation (`termB`) -- see the module scaladoc and
      * `docs/internal/CAPE_COMPETITIVE_ANALYSIS.md` for the `termA` vs `termB` measurement that
      * decided this.
      */
    def term: Term = termB
}
