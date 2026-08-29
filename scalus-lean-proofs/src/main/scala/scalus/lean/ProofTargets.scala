package scalus.lean

import scalus.*
// Wildcard, not a selective import: `List.foldLeft` and friends are extension methods that a
// selective import would leave out of scope. This shadows scala.List and scala.Option inside
// this file, which is fine because the file uses Seq everywhere else.
import scalus.cardano.onchain.plutus.prelude.*
import scalus.compiler.Options
import scalus.uplc.builtin.Data
import scalus.uplc.PlutusV3

/** The catalogue of programs the Lean proof suite reasons about. */
object ProofTargets {

    /** The single pinned compile configuration for every exported target.
      *
      * `valueBuiltins = false` is mandatory: the Lean model of UPLC has no CIP-153 `Value` builtins
      * and no CIP-138 array builtins, so a program using them cannot be decoded. Everything else is
      * the normal release lowering, so PV11 `case` is exercised.
      */
    val options: Options = Options.releaseUntagged.copy(valueBuiltins = false)

    private given Options = options

    /** Programs with no integer samples, used only by the hand-written Sanity.lean. */
    val sanity: Seq[ProofTarget] = Seq(
      // NB: compiled here rather than reusing `PlutusV3.alwaysOk`, which is a library constant
      // built with `valueBuiltins = true` and so would violate the module's pinned Options.
      ProofTarget("always_ok", PlutusV3.compile((_: Data) => ()).program, 1, Seq.empty),
      ProofTarget("always_fail", PlutusV3.compile((_: Data) => fail("nope")).program, 1, Seq.empty)
    )

    val math: Seq[ProofTarget] = Seq(
      ProofTarget(
        "math_abs",
        PlutusV3.compile((x: BigInt) => Math.abs(x)).program,
        1,
        Seq(Seq(BigInt(-7)) -> BigInt(7), Seq(BigInt(0)) -> BigInt(0), Seq(BigInt(9)) -> BigInt(9))
      ),
      ProofTarget(
        "math_min",
        PlutusV3.compile((x: BigInt) => (y: BigInt) => Math.min(x, y)).program,
        2,
        Seq(Seq(BigInt(3), BigInt(5)) -> BigInt(3), Seq(BigInt(5), BigInt(3)) -> BigInt(3))
      ),
      ProofTarget(
        "math_max",
        PlutusV3.compile((x: BigInt) => (y: BigInt) => Math.max(x, y)).program,
        2,
        Seq(Seq(BigInt(3), BigInt(5)) -> BigInt(5), Seq(BigInt(5), BigInt(3)) -> BigInt(5))
      ),
      ProofTarget(
        "math_clamp",
        PlutusV3
            .compile((x: BigInt) => (lo: BigInt) => (hi: BigInt) => Math.clamp(x, lo, hi))
            .program,
        3,
        Seq(
          Seq(BigInt(9), BigInt(1), BigInt(5)) -> BigInt(5),
          Seq(BigInt(-9), BigInt(1), BigInt(5)) -> BigInt(1),
          Seq(BigInt(3), BigInt(1), BigInt(5)) -> BigInt(3)
        )
      ),
      ProofTarget(
        "math_gcd",
        PlutusV3.compile((x: BigInt) => (y: BigInt) => Math.gcd(x, y)).program,
        2,
        Seq(
          Seq(BigInt(12), BigInt(18)) -> BigInt(6),
          Seq(BigInt(-19), BigInt(14)) -> BigInt(1),
          Seq(BigInt(0), BigInt(5)) -> BigInt(5)
        )
      ),
      ProofTarget(
        "math_exp2",
        PlutusV3.compile((e: BigInt) => Math.exp2(e)).program,
        1,
        Seq(
          Seq(BigInt(10)) -> BigInt(1024),
          Seq(BigInt(0)) -> BigInt(1),
          Seq(BigInt(-1)) -> BigInt(0)
        )
      ),
      ProofTarget(
        "math_sqrt",
        PlutusV3.compile((x: BigInt) => Math.sqrt(x)).program,
        1,
        Seq(Seq(BigInt(10000)) -> BigInt(100), Seq(BigInt(0)) -> BigInt(0))
      )
    )

    /** Prelude data structures. At PV11 these lower to real UPLC `constr` and `case`. */
    val data: Seq[ProofTarget] = Seq(
      ProofTarget(
        "opt_double_or_default",
        PlutusV3
            .compile((x: BigInt) =>
                val o = if x > 0 then Option.Some(x) else Option.None
                o match
                    case Option.Some(v) => v * 2
                    case Option.None    => BigInt(-1)
            )
            .program,
        1,
        Seq(Seq(BigInt(5)) -> BigInt(10), Seq(BigInt(-5)) -> BigInt(-1))
      ),
      ProofTarget(
        "list_sum2",
        PlutusV3
            .compile((a: BigInt) =>
                (b: BigInt) =>
                    List
                        .Cons(a, List.Cons(b, List.Nil))
                        .foldLeft(BigInt(0))((acc, x) => acc + x)
            )
            .program,
        2,
        Seq(Seq(BigInt(3), BigInt(4)) -> BigInt(7), Seq(BigInt(-1), BigInt(1)) -> BigInt(0))
      )
    )

    /** Second compilations of sources already in `math`, for codegen-equivalence proofs. */
    val equivalence: Seq[ProofTarget] = Seq(
      ProofTarget(
        "math_gcd_unopt",
        PlutusV3
            .compile((x: BigInt) => (y: BigInt) => Math.gcd(x, y))(using
              options.copy(optimizeUplc = false, uplcOptimizers = Seq.empty)
            )
            .program,
        2,
        Seq(Seq(BigInt(12), BigInt(18)) -> BigInt(6), Seq(BigInt(-19), BigInt(14)) -> BigInt(1))
      )
    )

    val all: Seq[ProofTarget] = sanity ++ math ++ data ++ equivalence
}
