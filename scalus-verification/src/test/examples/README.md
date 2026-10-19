# BigInt verification example

[`BigIntVerificationTest.scala`](BigIntVerificationTest.scala) is a compact example for
reports of verifying a Scala function after compilation to UPLC with `UplcBlaster`.

The function computes `half(x) = ceil(|x| / 2)` using a sign branch and integer division.
For example, `half(-5) = 3`, `half(0) = 0`, and `half(4) = 2`. The properties call
`half(x)` directly. The property macro captures these expressions, and UplcBlaster
links the `@Compile` function into the UPLC predicates it verifies.

| Case | Statement | Expected result |
| --- | --- | --- |
| [Proof](BigIntVerificationTest.scala#L29) | For every integer `x`, `abs(x) <= 2 * half(x) <= abs(x) + 1` | Proven |
| [Counterexample](BigIntVerificationTest.scala#L40) | For every nonnegative integer `x`, `2 * half(x) == x` | Refuted; any nonnegative odd input, such as `1`, violates it |
| [No solutions](BigIntVerificationTest.scala#L52) | There is no integer `x` for which `half(x) < 0` | Proven by showing `half(x) >= 0` for every input |
| [Inconclusive](BigIntVerificationTest.scala#L57) | The same nonnegativity property, with only 3 Lean evaluation steps | Inconclusive; the candidate counterexample is spurious on CEK replay |

The counterexample test checks the input returned by the solver and prints it; the solver
may choose a different odd integer on another run. `UplcBlaster` replays a counterexample
on the Scalus CEK before reporting a refutation. An inconclusive result establishes
neither truth nor falsity; here the deliberately small budget prevents a proof.

In a run of this direct-call example, all four tests passed with Lean: the counterexample was `x = 1`,
`half(x) = 1`. The inconclusive check produced the spurious candidate `x = 0`; Lean
reported that the property holds there from 42 steps onward, exceeding the budget of 3.

The step budgets bound symbolic evaluation on Lean's CEK model. They are not Cardano
execution units. Successful checks use Blaster and its SMT solver, whose verdict Lean
admits through `blasterProven`; they are not proofs checked solely by Lean's kernel.

## Prerequisites

The easiest setup is the repository's Nix development shell: install Nix with flakes
enabled, then run `nix develop` from the repository root (see
[CONTRIBUTING.md](../../../../CONTRIBUTING.md#env-setup-with-nix)). The shell supplies
the JDK, sbt, elan, and Z3, as well as the repository's other build dependencies.

For an existing environment without Nix, install these tools and put them on `PATH`:

- Git, for Lake to fetch the Lean dependencies.
- JDK 11 or newer and the sbt launcher; `project/build.properties` selects sbt 1.13.0.
- Elan, the Lean toolchain manager, which provides `lean` and `lake` and installs the
  Lean 4.24.0 version selected by `src/main/lean/lean-toolchain`.
- The Z3 SMT solver; the pinned Blaster dependency documents Z3 4.15.2 as its tested version.

The first build needs network access to download Scala and Lean dependencies. `lake build`
fetches and builds Blaster and PlutusCore from the committed Lake manifest; no separate
Blaster installation or editor plugin is needed.

## Run the example

From the repository root, enter the development shell if using Nix:

```sh
nix develop
```

The Lean workspace is already included in this repository at
`scalus-verification/src/main/lean`; you do not need to create or initialize it.
Before running the example for the first time, manually run `lake build` to download
its dependencies and compile its Lean library. The tests do not run this build for you.
Repeat it after changes to the Lean sources or dependencies:

```sh
(cd scalus-verification/src/main/lean && lake build)
```

Once built, the tests start and close the Lean server automatically.

Run all four tests, requiring Lean so a missing workspace cannot silently cancel them:

```sh
# One-time setup from the repository root (also repeat after changing Scalus).
sbt scalusVerification/prepareVerificationExamples

# Independent build: compiles and runs only this directory's examples and the ScalaTest helper.
cd scalus-verification/src/test/examples
sbt test
```

The standalone build reuses the compiled Scalus classes, dependency jars, compiler plugin,
Scala version, and compiler options exported by the setup task. It has no project dependency
on the main build, so subsequent `sbt test` runs cannot recompile Scalus. Editing an example
recompiles only the example. Run setup again after changing Scalus or its build settings,
switching Scala versions, or cleaning the main build. Keep the compiled artifacts in place.
The standalone build sets `SCALUS_REQUIRE_LEAN=1` for its tests.

To run a single case, filter by its test name, for example:

```sh
sbt 'testOnly *BigIntVerificationTest -- -z "counterexample:"'
```

Successful execution reports four passed tests, including the expected refutation and
inconclusive result. You can also run the example through the main build from the repository
root with `SCALUS_REQUIRE_LEAN=1 sbt 'scalusVerification/testOnly *BigIntVerificationTest'`,
which uses the usual incremental compilation of Scalus. Without `SCALUS_REQUIRE_LEAN=1`
in the main build, a missing Lean installation or unbuilt workspace cancels the tests
instead of failing them.
