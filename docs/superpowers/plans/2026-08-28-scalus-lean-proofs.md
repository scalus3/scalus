# Scalus Lean Proofs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a `scalus-lean-proofs` module that compiles chosen Scalus prelude functions to UPLC, exports the bytes, and proves properties about them in Lean 4 with the Blaster SMT backend.

**Architecture:** Two halves with a committed generated interface. A JVM-only sbt project compiles a catalogue of targets and writes `.flat` hex files plus a generated Lean file of imports and differential `#guard` checks. A Lean project alongside it states and proves the properties. The generated directory is committed, so the Lean half builds with no JVM present.

**Tech Stack:** Scala 3 + sbt (existing build), Lean 4 v4.24.0, Lake, Z3 4.15.4, nix devShell, GitHub Actions.

**Spec:** `docs/superpowers/specs/2026-08-27-lean-blaster-uplc-proofs-design.md`

**Pre-verified before hand-off:** the `ProofTarget`/`ProofTargets` code in Task 2 and the
`ProofTargetsTest` assertions were compiled and run against this working tree; all four tests
pass and every declared sample value is correct. The Lean `Prelude.lean` helper signatures and
the generated-file shape in Tasks 3 and 4 were elaborated successfully (1.8 seconds, including
`native_decide` guards). The budgets in Tasks 5 to 7 come from measured step counts. What is
NOT pre-verified: the sbt project wiring, the exporter itself, and the proof files as written.

## Global Constraints

- Lean toolchain is exactly `leanprover/lean4:v4.24.0`. Blaster and PlutusCoreBlaster both require it.
- Lean dependencies are pinned as: `PlutusCore` from `https://github.com/nau/PlutusCoreBlaster` @ `fix/case-scrutinee-smt-blowup`, and `Blaster` from `https://github.com/input-output-hk/Lean-blaster` @ `main`. Do not use Blaster's `beta-lambda-cache-optimization` branch. Commit `lake-manifest.json`.
- Every exported program MUST be compiled with `Options.releaseUntagged.copy(valueBuiltins = false)`. `valueBuiltins = false` is mandatory: the Lean model has no CIP-153 `Value` or CIP-138 array builtins.
- Budgets for `#prep_uplc` are calibrated with the Lean helper `steps`, never derived from Scalus's own step count (Scalus counts about 1.85x fewer steps).
- **Proof cost grows steeply and superlinearly in the budget.** Measured on `gcd_nonneg`, whose worst sample needs 203 steps: budget 250 proved in 2 seconds, budget 350 in 52 seconds, budget 500 did not finish in 500 seconds. So the budget rule is **the smallest value that covers your samples, times about 1.25** - never "a generous multiple". If a proof is slow, lower the budget toward the measured maximum; do not raise it.
- Every property group MUST include at least one negative control, i.e. a deliberately false statement checked with `#blaster (gen-cex: 0) (solve-result: 1) [...]`. Without it a vacuous proof is indistinguishable from a real one.
- All Lean paths inside `#import_uplc` are relative to the Lake workspace root, which is `scalus-lean-proofs/lean/`.
- Run `sbtn scalafmtAll` before every commit that touches Scala. `ci-jvm` runs `scalafmtCheckAll` and one unformatted file fails the whole job.
- Never add a `Co-Authored-By: Claude` trailer or a "Generated with" footer to any commit message.
- Commit directly to `master`. No feature branch, no PR. Rebase before pushing.

---

### Task 1: Nix devShell for the Lean toolchain

**Files:**
- Modify: `flake.nix:81` (inside `devShells = {`, add a `lean` shell after the closing of `default`, before `bench =` at line 170)

**Interfaces:**
- Consumes: nothing.
- Produces: `nix develop .#lean` provides `elan`, `z3`, `git`, `bashInteractive` on `PATH`.

- [ ] **Step 1: Add the devShell**

In `flake.nix`, immediately before the line `        bench =`, insert:

```nix
        # Lean 4 + Z3 for the scalus-lean-proofs module. `elan` fetches the exact
        # toolchain named in scalus-lean-proofs/lean/lean-toolchain (v4.24.0).
        # Blaster documents Z3 4.15.2; nixpkgs 25.11 ships 4.15.4, which works.
        lean = pkgs.mkShell {
          buildInputs = [ pkgs.bashInteractive ];
          packages = with pkgs; [
            elan
            z3
            git
          ];
        };
```

- [ ] **Step 2: Verify the shell provides the tools**

Run:

```bash
nix develop .#lean --accept-flake-config --command bash -c 'elan --version && z3 --version && git --version'
```

Expected: three version lines, including `Z3 version 4.15.4`.

- [ ] **Step 3: Commit**

```bash
git add flake.nix
git commit -m "build: add a lean devShell with elan and z3

For the scalus-lean-proofs module. elan fetches Lean 4.24.0 as named in
lean-toolchain; nixpkgs 25.11 ships Z3 4.15.4, which Blaster accepts."
```

---

### Task 2: Proof target catalogue

**Files:**
- Create: `scalus-lean-proofs/src/main/scala/scalus/lean/ProofTarget.scala`
- Create: `scalus-lean-proofs/src/main/scala/scalus/lean/ProofTargets.scala`
- Create: `scalus-lean-proofs/src/test/scala/scalus/lean/ProofTargetsTest.scala`
- Modify: `build.sbt` (add project after `lazy val generateLlmsApi` block ending at line 813; add to `jvm` aggregate at line 262)

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `final case class ProofTarget(name: String, program: Program, arity: Int, samples: Seq[(Seq[BigInt], BigInt)])`
  - `ProofTarget.leanName: String` - `math_gcd` becomes `mathGcd`
  - `ProofTargets.all: Seq[ProofTarget]`
  - `ProofTargets.options: Options` - the single pinned compile configuration
  - sbt project `scalusLeanProofs`

- [ ] **Step 1: Add the sbt project**

In `build.sbt`, immediately after the `generateLlmsApi := Def.taskDyn { ... }.value` block (which ends at line 813 with `}.value`), insert:

```scala
// Compiles selected prelude functions to UPLC and exports them for the Lean/Blaster
// proof suite in scalus-lean-proofs/lean. See
// docs/superpowers/specs/2026-08-27-lean-blaster-uplc-proofs-design.md
lazy val scalusLeanProofs = project
    .in(file("scalus-lean-proofs"))
    .dependsOn(scalus.jvm)
    .disablePlugins(MimaPlugin)
    .settings(
      name := "scalus-lean-proofs",
      publish / skip := true,
      run / fork := true,
      libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
      PluginDependency
    )
```

Then add `scalusLeanProofs,` to the `jvm` aggregate list, immediately after the `llmApiGen,` line (line 262).

- [ ] **Step 2: Write the failing test**

Create `scalus-lean-proofs/src/test/scala/scalus/lean/ProofTargetsTest.scala`:

```scala
package scalus.lean

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.{PlutusVM, Result}

/** Every declared sample must actually hold when the compiled UPLC is run on the JVM. This
  * catches a mis-declared expectation here rather than in Lean, where it would surface as a
  * confusing proof failure.
  */
class ProofTargetsTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("target names are unique and file-name safe") {
        val names = ProofTargets.all.map(_.name)
        assert(names.distinct == names, s"duplicate target names: ${names.diff(names.distinct)}")
        names.foreach { n =>
            assert(n.matches("[a-z0-9_]+"), s"target name '$n' must be lower_snake_case")
        }
    }

    test("declared arity matches the number of arguments in every sample") {
        ProofTargets.all.foreach { t =>
            t.samples.foreach { case (args, _) =>
                assert(args.length == t.arity, s"${t.name}: sample ${args} does not match arity ${t.arity}")
            }
        }
    }

    test("every sample evaluates to its declared expected value") {
        ProofTargets.all.foreach { t =>
            t.samples.foreach { case (args, expected) =>
                val applied = args.foldLeft(t.program)((acc, a) => acc $ a.asTerm)
                applied.term.evaluateDebug match
                    case s: Result.Success =>
                        assert(s.term == expected.asTerm, s"${t.name}(${args.mkString(",")})")
                    case f: Result.Failure =>
                        fail(s"${t.name}(${args.mkString(",")}) failed: ${f.exception}")
            }
        }
    }

    test("leanName converts snake_case to camelCase") {
        assert(ProofTarget.leanNameOf("math_gcd") == "mathGcd")
        assert(ProofTarget.leanNameOf("math_is_sqrt") == "mathIsSqrt")
        assert(ProofTarget.leanNameOf("always_ok") == "alwaysOk")
    }
}
```

- [ ] **Step 3: Run the test to verify it fails**

Run: `sbtn "scalusLeanProofs/testOnly scalus.lean.ProofTargetsTest"`
Expected: FAIL to compile, `Not found: ProofTargets`.

- [ ] **Step 4: Write ProofTarget**

Create `scalus-lean-proofs/src/main/scala/scalus/lean/ProofTarget.scala`:

```scala
package scalus.lean

import scalus.uplc.Program

/** One compiled UPLC program that the Lean proof suite reasons about.
  *
  * @param name
  *   lower_snake_case; becomes `<name>.flat` on disk and a camelCase Lean identifier
  * @param program
  *   the compiled program, always built with [[ProofTargets.options]]
  * @param arity
  *   how many integer arguments the program takes
  * @param samples
  *   argument lists paired with the value the program must produce, used both as a JVM-side
  *   test and as the generated Lean differential checks
  */
final case class ProofTarget(
    name: String,
    program: Program,
    arity: Int,
    samples: Seq[(Seq[BigInt], BigInt)]
) {

    /** The Lean identifier for this target, e.g. `math_gcd` becomes `mathGcd`. */
    def leanName: String = ProofTarget.leanNameOf(name)
}

object ProofTarget {

    /** Converts a lower_snake_case target name to a camelCase Lean identifier. */
    def leanNameOf(name: String): String = {
        val parts = name.split('_').filter(_.nonEmpty)
        (parts.head +: parts.tail.map(p => p.head.toUpper +: p.tail)).mkString
    }
}
```

- [ ] **Step 5: Write ProofTargets**

Create `scalus-lean-proofs/src/main/scala/scalus/lean/ProofTargets.scala`:

```scala
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
      * `valueBuiltins = false` is mandatory: the Lean model of UPLC has no CIP-153 `Value`
      * builtins and no CIP-138 array builtins, so a program using them cannot be decoded.
      * Everything else is the normal release lowering, so PV11 `case` is exercised.
      */
    val options: Options = Options.releaseUntagged.copy(valueBuiltins = false)

    private given Options = options

    /** Programs with no integer samples, used only by the hand-written Sanity.lean. */
    val sanity: Seq[ProofTarget] = Seq(
      ProofTarget("always_ok", PlutusV3.alwaysOk.program, 1, Seq.empty),
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
        Seq(Seq(BigInt(10)) -> BigInt(1024), Seq(BigInt(0)) -> BigInt(1), Seq(BigInt(-1)) -> BigInt(0))
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
```

- [ ] **Step 6: Run the tests to verify they pass**

Run: `sbtn "scalusLeanProofs/testOnly scalus.lean.ProofTargetsTest"`
Expected: 4 tests PASS.

If a sample fails, the declared expected value is wrong. Fix the expectation, not the test.

- [ ] **Step 7: Format and commit**

```bash
sbtn scalafmtAll
git add build.sbt scalus-lean-proofs/src
git commit -m "feat(lean-proofs): add the proof target catalogue

A ProofTarget pairs a compiled UPLC program with sample argument lists and
the values it must produce. The test runs every sample through Scalus's own
CEK, so a mis-declared expectation is caught here rather than surfacing as a
confusing proof failure in Lean."
```

---

### Task 3: UPLC exporter

**Files:**
- Create: `scalus-lean-proofs/src/main/scala/scalus/lean/ExportUplc.scala`
- Create: `scalus-lean-proofs/src/test/scala/scalus/lean/ExportUplcTest.scala`
- Modify: `build.sbt` (add the `exportLeanUplc` task after the `scalusLeanProofs` project)

**Interfaces:**
- Consumes: `ProofTargets.all`, `ProofTarget.leanName`.
- Produces:
  - `ExportUplc.write(dir: Path): Seq[Path]` - writes one `<name>.flat` per target plus `Targets.lean`, returns what it wrote
  - `ExportUplc.main(args: Array[String])` - `args(0)` is the output directory
  - sbt task `exportLeanUplc`
  - On-disk format: `<name>.flat` holds the single-CBOR-wrapped flat program as lowercase hex, no trailing newline
  - `Targets.lean` in namespace `ScalusProofs.Generated`, one `#import_uplc <leanName> PlutusV3 single_cbor_hex "ScalusProofs/Generated/<name>.flat"` per target, plus one `example ... := by native_decide` per sample

- [ ] **Step 1: Write the failing test**

Create `scalus-lean-proofs/src/test/scala/scalus/lean/ExportUplcTest.scala`:

```scala
package scalus.lean

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex

import java.nio.file.{Files, Path}

class ExportUplcTest extends AnyFunSuite {

    private def withTempDir[A](f: Path => A): A = {
        val dir = Files.createTempDirectory("lean-export-test")
        try f(dir)
        finally {
            Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
        }
    }

    test("writes one flat file per target plus Targets.lean") {
        withTempDir { dir =>
            ExportUplc.write(dir)
            ProofTargets.all.foreach { t =>
                assert(Files.exists(dir.resolve(s"${t.name}.flat")), s"missing ${t.name}.flat")
            }
            assert(Files.exists(dir.resolve("Targets.lean")))
        }
    }

    test("flat files are lowercase hex that round-trips to the original program") {
        withTempDir { dir =>
            ExportUplc.write(dir)
            ProofTargets.all.foreach { t =>
                val hex = Files.readString(dir.resolve(s"${t.name}.flat"))
                assert(hex.matches("[0-9a-f]+"), s"${t.name}.flat is not lowercase hex")
                assert(Hex.hexToBytes(hex).sameElements(t.program.cborEncoded), s"${t.name} mismatch")
            }
        }
    }

    test("Targets.lean has an import and a native_decide check per sample") {
        withTempDir { dir =>
            ExportUplc.write(dir)
            val lean = Files.readString(dir.resolve("Targets.lean"))
            ProofTargets.all.foreach { t =>
                assert(
                  lean.contains(
                    s"""#import_uplc ${t.leanName} PlutusV3 single_cbor_hex "ScalusProofs/Generated/${t.name}.flat""""
                  ),
                  s"missing import for ${t.name}"
                )
            }
            val expectedChecks = ProofTargets.all.map(_.samples.size).sum
            assert(lean.split("native_decide").length - 1 == expectedChecks)
            assert(lean.contains("namespace ScalusProofs.Generated"))
            assert(lean.contains("Generated by `sbt exportLeanUplc`"))
        }
    }

    test("export is deterministic") {
        withTempDir { a =>
            withTempDir { b =>
                ExportUplc.write(a)
                ExportUplc.write(b)
                ProofTargets.all.foreach { t =>
                    val fa = Files.readString(a.resolve(s"${t.name}.flat"))
                    val fb = Files.readString(b.resolve(s"${t.name}.flat"))
                    assert(fa == fb, s"${t.name}.flat is not deterministic")
                }
                assert(Files.readString(a.resolve("Targets.lean")) == Files.readString(b.resolve("Targets.lean")))
            }
        }
    }
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `sbtn "scalusLeanProofs/testOnly scalus.lean.ExportUplcTest"`
Expected: FAIL to compile, `Not found: ExportUplc`.

- [ ] **Step 3: Write the exporter**

Create `scalus-lean-proofs/src/main/scala/scalus/lean/ExportUplc.scala`:

```scala
package scalus.lean

import scalus.utils.Hex

import java.nio.file.{Files, Path}

/** Writes the compiled UPLC of every [[ProofTargets.all]] entry in the form the Lean proof
  * suite consumes, plus a generated Lean file of imports and differential checks.
  *
  * The generated output is committed to git, so the Lean half of the module builds without a
  * JVM. `exportLeanUplc` regenerates it; CI fails if the result differs from what is committed.
  */
object ExportUplc {

    /** Budget used only by the generated differential checks. Large enough for every target;
      * `runSteps` short-circuits once the machine halts, so an oversized value costs nothing.
      * Proof budgets are NOT this value: they are calibrated per property file.
      */
    private val guardBudget = 20000

    private def header: String =
        s"""-- Generated by `sbt exportLeanUplc`. Do not edit by hand.
           |--
           |-- Each `example` below asserts that the Lean CEK machine produces the value Scalus's
           |-- own JVM CEK produced for the same arguments, so the two implementations are checked
           |-- against each other on every build. A failing check most often means the compiled
           |-- program changed, not that the Lean model is wrong.
           |import ScalusProofs.Prelude
           |
           |namespace ScalusProofs.Generated
           |open PlutusCore.Integer (Integer)
           |open ScalusProofs.Prelude
           |
           |set_option warn.sorry false
           |""".stripMargin

    private def renderArgs(args: Seq[BigInt]): String =
        args.map(a => if a < 0 then s"($a)" else a.toString).mkString("[", ", ", "]")

    private def renderExpected(v: BigInt): String = if v < 0 then s"($v)" else v.toString

    private def renderTarget(t: ProofTarget): String = {
        val imp =
            s"""#import_uplc ${t.leanName} PlutusV3 single_cbor_hex "ScalusProofs/Generated/${t.name}.flat""""
        val checks = t.samples.zipWithIndex.map { case ((args, expected), i) =>
            s"""example : runInts ${t.leanName} ${renderArgs(args)} $guardBudget """ +
                s"""= some ${renderExpected(expected)} := by native_decide"""
        }
        (imp +: checks).mkString("\n") + "\n"
    }

    /** Writes every target into `dir`. Returns the paths written. */
    def write(dir: Path): Seq[Path] = {
        Files.createDirectories(dir)
        val flats = ProofTargets.all.map { t =>
            val p = dir.resolve(s"${t.name}.flat")
            Files.writeString(p, Hex.bytesToHex(t.program.cborEncoded).toLowerCase)
            p
        }
        val body = ProofTargets.all.map(renderTarget).mkString("\n")
        val lean = dir.resolve("Targets.lean")
        Files.writeString(lean, s"$header\n$body\nend ScalusProofs.Generated\n")
        flats :+ lean
    }

    def main(args: Array[String]): Unit = {
        require(args.nonEmpty, "usage: ExportUplc <outputDir>")
        val written = write(Path.of(args(0)))
        written.foreach(p => println(s"wrote $p"))
        println(s"${written.size} files, ${ProofTargets.all.size} targets")
    }
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `sbtn "scalusLeanProofs/testOnly scalus.lean.ExportUplcTest"`
Expected: 4 tests PASS.

If the "lowercase hex" assertion fails, check that `Hex.bytesToHex` output was lowercased.

- [ ] **Step 5: Add the sbt task**

In `build.sbt`, immediately after the `scalusLeanProofs` project definition, insert:

```scala
lazy val exportLeanUplc = taskKey[Unit]("Regenerate scalus-lean-proofs/lean/ScalusProofs/Generated")
exportLeanUplc := Def.taskDyn {
    val outDir = ((ThisBuild / baseDirectory).value / "scalus-lean-proofs" / "lean" /
        "ScalusProofs" / "Generated").getAbsolutePath
    (scalusLeanProofs / Compile / runMain).toTask(s" scalus.lean.ExportUplc $outDir")
}.value
```

- [ ] **Step 6: Run the export and inspect the output**

```bash
sbtn exportLeanUplc
ls scalus-lean-proofs/lean/ScalusProofs/Generated/
head -30 scalus-lean-proofs/lean/ScalusProofs/Generated/Targets.lean
```

Expected: one `.flat` per target plus `Targets.lean`, whose first `#import_uplc` line is for `alwaysOk`.

- [ ] **Step 7: Format and commit**

```bash
sbtn scalafmtAll
git add build.sbt scalus-lean-proofs/src scalus-lean-proofs/lean/ScalusProofs/Generated
git commit -m "feat(lean-proofs): export compiled UPLC for the Lean suite

Writes one single-CBOR hex file per target plus a generated Targets.lean of
imports and differential checks. Each check asserts the Lean CEK agrees with
the value Scalus's JVM CEK produced, so the two implementations test each
other on every build."
```

---

### Task 4: Lean project skeleton and sanity proofs

**Files:**
- Create: `scalus-lean-proofs/lean/lean-toolchain`
- Create: `scalus-lean-proofs/lean/lakefile.lean`
- Create: `scalus-lean-proofs/lean/ScalusProofs.lean`
- Create: `scalus-lean-proofs/lean/ScalusProofs/Prelude.lean`
- Create: `scalus-lean-proofs/lean/ScalusProofs/Sanity.lean`
- Create: `scalus-lean-proofs/.gitignore`

**Interfaces:**
- Consumes: `ScalusProofs/Generated/*.flat` and `Targets.lean` from Task 3.
- Produces:
  - `ScalusProofs.Prelude.ints : List Integer -> List Term`
  - `ScalusProofs.Prelude.runInts : PlutusScript -> List Integer -> Nat -> Option Integer`
  - `ScalusProofs.Prelude.steps : PlutusScript -> List Integer -> Nat -> Option Nat`
  - a Lake workspace at `scalus-lean-proofs/lean` whose default target builds everything

- [ ] **Step 1: Write the toolchain and lakefile**

Create `scalus-lean-proofs/lean/lean-toolchain` containing exactly:

```
leanprover/lean4:v4.24.0
```

Create `scalus-lean-proofs/lean/lakefile.lean`:

```lean
import Lake
open Lake DSL

package «ScalusProofs» where
  moreGlobalServerArgs := #["--threads=4"]
  moreLeanArgs := #["--threads=4"]
  -- PlutusCore is pinned to a fork until input-output-hk/PlutusCoreBlaster#40 merges.
  -- That PR fixes Frame.CaseScrutinee, without which `blaster` does not terminate on any
  -- program using UPLC `case`, which is every PV11 program Scalus emits.
  require PlutusCore from git
    "https://github.com/nau/PlutusCoreBlaster" @ "fix/case-scrutinee-smt-blowup"
  -- Blaster `main`, NOT beta-lambda-cache-optimization: the branches have diverged and
  -- neither is a superset. `main` carries the Int.ediv/Int.emod fix our integer targets
  -- need and measured about 4x faster on gcd equivalence.
  require Blaster from git
    "https://github.com/input-output-hk/Lean-blaster" @ "main"

@[default_target]
lean_lib «ScalusProofs» where
```

Create `scalus-lean-proofs/.gitignore`:

```
lean/.lake/
```

- [ ] **Step 2: Write the Prelude helpers**

Create `scalus-lean-proofs/lean/ScalusProofs/Prelude.lean`:

```lean
import PlutusCore.UPLC
import Blaster

/-! Shared helpers for the Scalus proof suite. -/

namespace ScalusProofs.Prelude

open PlutusCore.Integer (Integer)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open PlutusCore.UPLC.PlutusScript (PlutusScript)
open PlutusCore.UPLC.CekMachine (cekExecuteProgram)

/-- Integer arguments as UPLC constant terms. -/
def ints (xs : List Integer) : List Term :=
  xs.map (fun x => Term.Const $ Const.Integer x)

/-- Run `p` on integer arguments `xs` for at most `n` CEK steps. `none` means the machine did
    not halt with an integer within the budget. -/
def runInts (p : PlutusScript) (xs : List Integer) (n : Nat) : Option Integer :=
  fromFrameToInt (cekExecuteProgram p.script (ints xs) n)

/-- The smallest step count at which `p` halts with an integer on `xs`, searching up to `hi`.

    Use this to choose a `#prep_uplc` budget. Do NOT derive budgets from Scalus's own step
    count: Plutus charges per `Eval` transition while this machine counts `Eval` and `Return`,
    so the Lean figure is roughly 1.85x larger.

    A budget below the true step count makes the machine return `State.Error`, which silently
    makes conditional theorems vacuous and falsifies equations. A falsification is not a bug
    until it is reproduced at twice the budget. -/
def steps (p : PlutusScript) (xs : List Integer) (hi : Nat) : Option Nat :=
  (List.range hi).find? (fun n => (runInts p xs n).isSome)

end ScalusProofs.Prelude
```

- [ ] **Step 3: Write the root module and the sanity proofs**

Create `scalus-lean-proofs/lean/ScalusProofs.lean`:

```lean
import ScalusProofs.Prelude
import ScalusProofs.Generated.Targets
import ScalusProofs.Sanity
```

Create `scalus-lean-proofs/lean/ScalusProofs/Sanity.lean`:

```lean
import ScalusProofs.Generated.Targets

/-! The two trivial scripts, as an end-to-end check that the whole pipeline is wired up. -/

namespace ScalusProofs.Sanity

open PlutusCore.Data (Data)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open ScalusProofs.Generated

set_option warn.sorry false

def dataArg (d : Data) : List Term := [Term.Const $ Const.Data d]

#prep_uplc pAlwaysOk   alwaysOk   dataArg 100
#prep_uplc pAlwaysFail alwaysFail dataArg 100

theorem always_ok_succeeds : ∀ (d : Data), isSuccessful (pAlwaysOk.prop d) := by blaster

theorem always_fail_never_succeeds :
    ∀ (d : Data), ¬ isSuccessful (pAlwaysFail.prop d) := by blaster

/-- Negative control: this is false, and Blaster must say so rather than prove it. -/
def bogus_always_fail_succeeds : Prop := ∀ (d : Data), isSuccessful (pAlwaysFail.prop d)
#blaster (gen-cex: 0) (solve-result: 1) [bogus_always_fail_succeeds]

end ScalusProofs.Sanity
```

- [ ] **Step 4: Build and run**

```bash
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'lake update && lake build'
```

Expected: `Build completed successfully`. The first run downloads Lean 4.24.0 and builds Blaster and PlutusCore; allow 15 minutes.

If it fails with `external command 'git' exited with code 128`, a pinned upstream rev was force-pushed away. Re-run `lake update` and note the new revs in the commit message.

- [ ] **Step 5: Verify the proofs actually run**

```bash
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'lake env lean ScalusProofs/Sanity.lean'
```

Expected: exactly `✅ Valid`, `✅ Valid`, `✅ Expected Falsified`. Anything else, including `Undetermined`, is a failure.

- [ ] **Step 6: Commit**

```bash
git add scalus-lean-proofs/lean/lean-toolchain scalus-lean-proofs/lean/lakefile.lean \
        scalus-lean-proofs/lean/lake-manifest.json scalus-lean-proofs/lean/ScalusProofs.lean \
        scalus-lean-proofs/lean/ScalusProofs/Prelude.lean \
        scalus-lean-proofs/lean/ScalusProofs/Sanity.lean scalus-lean-proofs/.gitignore
git commit -m "feat(lean-proofs): Lean project skeleton and sanity proofs

Lake workspace pinned to Lean 4.24.0, PlutusCoreBlaster (fork, pending
input-output-hk/PlutusCoreBlaster#40) and Lean-blaster main. Prelude carries
the argument helpers and the step-count calibration tool; Sanity proves
alwaysOk always succeeds and alwaysFail never does, with a negative control."
```

---

### Task 5: Math properties

**Files:**
- Create: `scalus-lean-proofs/lean/ScalusProofs/Math.lean`
- Modify: `scalus-lean-proofs/lean/ScalusProofs.lean` (add `import ScalusProofs.Math`)

**Interfaces:**
- Consumes: `ScalusProofs.Generated.{mathAbs, mathMin, mathMax, mathClamp, mathGcd, mathExp2, mathSqrt}`, `ScalusProofs.Prelude.{runInts, steps}`.
- Produces: nothing other tasks depend on.

- [ ] **Step 1: Calibrate the budgets**

Create a scratch file `scalus-lean-proofs/lean/Calibrate.lean` (do NOT commit it):

```lean
import ScalusProofs.Generated.Targets
open ScalusProofs.Prelude ScalusProofs.Generated
#eval steps mathAbs   [(-7)]        2000
#eval steps mathMin   [3, 5]        2000
#eval steps mathMax   [3, 5]        2000
#eval steps mathClamp [9, 1, 5]     2000
#eval steps mathGcd   [(-19), 14]   8000
#eval steps mathExp2  [10]          8000
#eval steps mathSqrt  [10000]       8000
```

Run:

```bash
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'lake env lean Calibrate.lean'
```

Record each printed number. Measured during the spike: abs 26, min 23, max 23, clamp 39, gcd 203 (at `(-19, 14)`), exp2 84.

The budgets written into Step 2 below are already set from those measurements at about 1.25x. Confirm your numbers are not larger than the spike's; if one is, raise only that budget, by as little as possible. Do **not** round budgets up generously: at budget 350 the `gcd` proof takes 52 seconds and at 500 it does not finish, while at 250 it takes 2 seconds.

Delete `Calibrate.lean` when done.

- [ ] **Step 2: Write the property file**

Create `scalus-lean-proofs/lean/ScalusProofs/Math.lean`. The budgets below are already set; adjust one only if Step 1 printed a larger number for that target:

```lean
import ScalusProofs.Generated.Targets

/-! Properties of `scalus.cardano.onchain.plutus.prelude.Math`, proved against the compiled
    UPLC rather than against the Scala source.

    Budgets are deliberately tight. Proof cost grows superlinearly in the budget: for
    `gcd_nonneg`, whose worst sample needs 203 steps, budget 250 proves in 2s, 350 in 52s and
    500 does not finish. Lower a budget if a proof is slow; never raise it "to be safe". -/

namespace ScalusProofs.Math

open PlutusCore.Integer (Integer)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open ScalusProofs.Prelude
open ScalusProofs.Generated

set_option warn.sorry false

def i1 (x : Integer) : List Term := ints [x]
def i2 (x y : Integer) : List Term := ints [x, y]
def i3 (x y z : Integer) : List Term := ints [x, y, z]

#prep_uplc pAbs   mathAbs   i1 40
#prep_uplc pMin   mathMin   i2 40
#prep_uplc pMax   mathMax   i2 40
#prep_uplc pClamp mathClamp i3 60
#prep_uplc pExp2  mathExp2  i1 150

/-! ### abs -/

theorem abs_total : ∀ (x : Integer), isSuccessful (pAbs.prop x) := by blaster

theorem abs_nonneg : ∀ (x r : Integer),
    (fromFrameToInt $ pAbs.prop x) = some r → r ≥ 0 := by blaster

theorem abs_magnitude : ∀ (x r : Integer),
    (fromFrameToInt $ pAbs.prop x) = some r → r = x ∨ r = -x := by blaster

/-! ### min and max -/

theorem min_lower_bound : ∀ (x y r : Integer),
    (fromFrameToInt $ pMin.prop x y) = some r → r ≤ x ∧ r ≤ y := by blaster

theorem min_is_one_of : ∀ (x y r : Integer),
    (fromFrameToInt $ pMin.prop x y) = some r → r = x ∨ r = y := by blaster

theorem max_upper_bound : ∀ (x y r : Integer),
    (fromFrameToInt $ pMax.prop x y) = some r → r ≥ x ∧ r ≥ y := by blaster

theorem min_max_sum : ∀ (x y a b : Integer),
    (fromFrameToInt $ pMin.prop x y) = some a →
    (fromFrameToInt $ pMax.prop x y) = some b →
    a + b = x + y := by blaster

/-! ### clamp -/

theorem clamp_in_range : ∀ (x lo hi r : Integer), lo ≤ hi →
    (fromFrameToInt $ pClamp.prop x lo hi) = some r → lo ≤ r ∧ r ≤ hi := by blaster

theorem clamp_identity : ∀ (x lo hi r : Integer), lo ≤ x → x ≤ hi →
    (fromFrameToInt $ pClamp.prop x lo hi) = some r → r = x := by blaster

/-! ### exp2 -/

theorem exp2_negative_is_zero : ∀ (e r : Integer), e < 0 →
    (fromFrameToInt $ pExp2.prop e) = some r → r = 0 := by blaster

theorem exp2_nonneg : ∀ (e r : Integer),
    (fromFrameToInt $ pExp2.prop e) = some r → r ≥ 0 := by blaster

/-! ### negative control -/

def bogus_min_is_upper_bound : Prop := ∀ (x y r : Integer),
    (fromFrameToInt $ pMin.prop x y) = some r → r ≥ x ∧ r ≥ y
#blaster (gen-cex: 0) (solve-result: 1) [bogus_min_is_upper_bound]

end ScalusProofs.Math
```

- [ ] **Step 3: Run the proofs**

```bash
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'time lake env lean ScalusProofs/Math.lean'
```

Expected: eleven `✅ Valid` and one `✅ Expected Falsified`. The nine abs/min/max/clamp results took about 3 seconds in the spike; `exp2` is the slowest here.

If any theorem is falsified, do NOT treat it as a stdlib bug yet. Double that target's budget and re-run. Only a falsification that survives at twice the budget is real. If doubling makes the proof hang instead, the property needs a narrower domain, not a bigger budget.

If `exp2` is slow, lower its budget toward the number `steps` printed for it.

- [ ] **Step 4: Wire it into the root module**

Add `import ScalusProofs.Math` to `scalus-lean-proofs/lean/ScalusProofs.lean`, then:

```bash
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'lake build'
rm -f Calibrate.lean
```

Expected: `Build completed successfully`.

- [ ] **Step 5: Commit**

```bash
git add scalus-lean-proofs/lean/ScalusProofs.lean scalus-lean-proofs/lean/ScalusProofs/Math.lean
git commit -m "feat(lean-proofs): prove prelude Math properties against compiled UPLC

Eleven properties of abs, min, max, clamp and exp2, plus a negative control
that must be falsified so a vacuous proof cannot pass unnoticed."
```

---

### Task 6: Prelude data-structure properties

**Files:**
- Create: `scalus-lean-proofs/lean/ScalusProofs/Data.lean`
- Modify: `scalus-lean-proofs/lean/ScalusProofs.lean` (add `import ScalusProofs.Data`)

**Interfaces:**
- Consumes: `ScalusProofs.Generated.{optDoubleOrDefault, listSum2}`, `ScalusProofs.Prelude.ints`.
- Produces: nothing other tasks depend on.

These two targets are the ones that exercise real UPLC `constr` and `case` at PV11, so this file is the regression test for the upstream `Frame.CaseScrutinee` fix.

- [ ] **Step 1: Calibrate the budgets**

Recreate the scratch `scalus-lean-proofs/lean/Calibrate.lean`:

```lean
import ScalusProofs.Generated.Targets
open ScalusProofs.Prelude ScalusProofs.Generated
#eval steps optDoubleOrDefault [5]     4000
#eval steps optDoubleOrDefault [(-5)]  4000
#eval steps listSum2           [3, 4]  4000
```

Run it as in Task 5 Step 1. In the spike these were about 86, 38 and 209. The budgets in Step 2 are set from those at about 1.25x. Confirm yours are not larger.

- [ ] **Step 2: Write the property file**

Create `scalus-lean-proofs/lean/ScalusProofs/Data.lean`. Adjust a budget only if Step 1 printed a larger number for that target:

```lean
import ScalusProofs.Generated.Targets

/-! Properties of prelude `Option` and `List`. At PV11 these lower to real UPLC `constr` and
    `case`, so this file is also the regression test for the upstream Frame.CaseScrutinee fix
    (input-output-hk/PlutusCoreBlaster#40): without it, none of these terminate. -/

namespace ScalusProofs.Data

open PlutusCore.Integer (Integer)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open ScalusProofs.Prelude
open ScalusProofs.Generated

set_option warn.sorry false

def i1 (x : Integer) : List Term := ints [x]
def i2 (x y : Integer) : List Term := ints [x, y]

#prep_uplc pOpt optDoubleOrDefault i1 120
#prep_uplc pSum listSum2           i2 260

/-! ### Option match

    Source: `val o = if x > 0 then Some(x) else None; o match { case Some(v) => v * 2; case None => -1 }` -/

theorem opt_positive : ∀ (x r : Integer), x > 0 →
    (fromFrameToInt $ pOpt.prop x) = some r → r = 2 * x := by blaster

theorem opt_nonpositive : ∀ (x r : Integer), x ≤ 0 →
    (fromFrameToInt $ pOpt.prop x) = some r → r = -1 := by blaster

theorem opt_total : ∀ (x : Integer), isSuccessful (pOpt.prop x) := by blaster

/-! ### List fold

    Source: `List.Cons(a, List.Cons(b, List.Nil)).foldLeft(0)(_ + _)` -/

theorem list_fold_is_sum : ∀ (a b r : Integer),
    (fromFrameToInt $ pSum.prop a b) = some r → r = a + b := by blaster

theorem list_fold_total : ∀ (a b : Integer), isSuccessful (pSum.prop a b) := by blaster

/-! ### negative control -/

def bogus_fold_is_product : Prop := ∀ (a b r : Integer),
    (fromFrameToInt $ pSum.prop a b) = some r → r = a * b
#blaster (gen-cex: 0) (solve-result: 1) [bogus_fold_is_product]

end ScalusProofs.Data
```

- [ ] **Step 3: Run the proofs**

```bash
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'time lake env lean ScalusProofs/Data.lean'
```

Expected: five `✅ Valid` and one `✅ Expected Falsified`, in a few seconds.

If this hangs for minutes, the `PlutusCore` dependency is resolving to upstream `main` rather than the fork. Check `lake-manifest.json` names `nau/PlutusCoreBlaster`.

- [ ] **Step 4: Wire in, clean up, commit**

```bash
cd scalus-lean-proofs/lean && rm -f Calibrate.lean && cd ../..
# add `import ScalusProofs.Data` to scalus-lean-proofs/lean/ScalusProofs.lean first
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'lake build'
cd ../..
git add scalus-lean-proofs/lean/ScalusProofs.lean scalus-lean-proofs/lean/ScalusProofs/Data.lean
git commit -m "feat(lean-proofs): prove prelude Option and List properties

These targets lower to real UPLC constr and case at PV11, so they double as
the regression test for the upstream Frame.CaseScrutinee fix."
```

---

### Task 7: Codegen equivalence

**Files:**
- Create: `scalus-lean-proofs/lean/ScalusProofs/Equivalence.lean`
- Modify: `scalus-lean-proofs/lean/ScalusProofs.lean` (add `import ScalusProofs.Equivalence`)

**Interfaces:**
- Consumes: `ScalusProofs.Generated.{mathGcd, mathGcdUnopt}`.
- Produces: nothing other tasks depend on.

This is the group no Scala test can express: the same source compiled two ways must compute the same function.

- [ ] **Step 1: Calibrate both programs together**

Recreate the scratch `Calibrate.lean`:

```lean
import ScalusProofs.Generated.Targets
open ScalusProofs.Prelude ScalusProofs.Generated
#eval steps mathGcd      [(-19), 14] 8000
#eval steps mathGcdUnopt [(-19), 14] 8000
#eval steps mathGcd      [12, 18]    8000
#eval steps mathGcdUnopt [12, 18]    8000
```

Both programs MUST use the same budget, and it must cover the **slower** one. In the spike the optimized program halted at 282 steps and the unoptimized at 307 for `(-19, 14)`; a budget of 300 produced a false counterexample.

Set the budget to about 1.25x the largest number printed, and no more. This is the file where the tension bites hardest: the budget must clear the slower program, but `gcd` proofs become unusable well before budget 500.

- [ ] **Step 2: Write the property file**

Create `scalus-lean-proofs/lean/ScalusProofs/Equivalence.lean`. Both targets share one budget; adjust it only per Step 1:

```lean
import ScalusProofs.Generated.Targets

/-! Codegen equivalence: the same Scalus source compiled with the UPLC optimizer on and off
    must compute the same function. This is the property no Scala-level test can express,
    because both sides here are compiled UPLC. -/

namespace ScalusProofs.Equivalence

open PlutusCore.Integer (Integer)
open PlutusCore.UPLC.Term
open PlutusCore.UPLC.Utils
open ScalusProofs.Prelude
open ScalusProofs.Generated

set_option warn.sorry false

def i2 (x y : Integer) : List Term := ints [x, y]

-- Both must use the same budget, large enough for the SLOWER program. An under-sized budget
-- makes the faster program return `some r` while the slower returns `none`, which falsifies
-- the equality for reasons that have nothing to do with codegen.
#prep_uplc pGcd      mathGcd      i2 400
#prep_uplc pGcdUnopt mathGcdUnopt i2 400

theorem gcd_optimizer_equivalence : ∀ (x y : Integer),
    (fromFrameToInt $ pGcd.prop x y) = (fromFrameToInt $ pGcdUnopt.prop x y) := by blaster

theorem gcd_nonneg : ∀ (x y r : Integer),
    (fromFrameToInt $ pGcd.prop x y) = some r → r ≥ 0 := by blaster

/-! ### negative control

    `gcd` is not the product, so this must be falsified. Without a control here, an
    equivalence proof that held only because both sides ran out of budget would look
    identical to a real one. -/
def bogus_gcd_is_product : Prop := ∀ (x y r : Integer),
    (fromFrameToInt $ pGcd.prop x y) = some r → r = x * y
#blaster (gen-cex: 0) (solve-result: 1) [bogus_gcd_is_product]

end ScalusProofs.Equivalence
```

- [ ] **Step 3: Run the proofs**

```bash
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'time lake env lean ScalusProofs/Equivalence.lean'
```

Expected: two `✅ Valid` and one `✅ Expected Falsified`. This file is by far the slowest; the spike measured 53 seconds for the equivalence pair. Allow up to 15 minutes before treating a hang as a failure.

If it hangs, lower both budgets in step of 25 toward the largest number `steps` printed, re-running each time. The budget must stay above that number.

If `gcd_optimizer_equivalence` is falsified, read the counterexample, then check both programs at that input with `#eval runInts pGcd [a, b] 20000` and `#eval runInts pGcdUnopt [a, b] 20000`. If both return the same value, the budget was too small.

- [ ] **Step 4: Wire in, clean up, commit**

```bash
cd scalus-lean-proofs/lean && rm -f Calibrate.lean && cd ../..
# add `import ScalusProofs.Equivalence` to scalus-lean-proofs/lean/ScalusProofs.lean first
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'lake build'
cd ../..
git add scalus-lean-proofs/lean/ScalusProofs.lean scalus-lean-proofs/lean/ScalusProofs/Equivalence.lean
git commit -m "feat(lean-proofs): prove gcd is unchanged by the UPLC optimizer

Same source, optimizeUplc on and off, proved to compute the same function.
Both sides are compiled UPLC, so no Scala-level test can express this."
```

---

### Task 8: Freshness gate, CI workflow and README

**Files:**
- Create: `.github/workflows/lean-proofs.yml`
- Create: `scalus-lean-proofs/README.md`
- Modify: `.github/workflows/ci-jvm.yml` (add a freshness step after the build step)

**Interfaces:**
- Consumes: `exportLeanUplc` from Task 3, the Lean files from Tasks 4 to 7.
- Produces: nightly CI, and a PR-time gate that the committed generated files match a fresh export.

- [ ] **Step 1: Add the freshness gate to JVM CI**

The existing `release.yml` uses this pattern for `llms-api.txt`; follow it. In `.github/workflows/ci-jvm.yml`, after the `Build and test in Nix devshell` step, insert:

```yaml
      - name: Verify exported Lean UPLC is fresh
        if: matrix.name == 'lts'
        run: |
          nix develop .#ci --accept-flake-config --command bash -c "sbt --batch exportLeanUplc"
          git diff --exit-code scalus-lean-proofs/lean/ScalusProofs/Generated \
            || (echo "Exported UPLC is stale - run 'sbt exportLeanUplc' and commit" && exit 1)
```

The `if:` guard keeps this on one matrix leg only; running it three times would be wasted work.

- [ ] **Step 2: Verify the gate catches staleness**

```bash
printf 'deadbeef' > scalus-lean-proofs/lean/ScalusProofs/Generated/math_min.flat
sbtn exportLeanUplc
git diff --exit-code scalus-lean-proofs/lean/ScalusProofs/Generated; echo "exit=$?"
```

Expected: `exit=0`, because the export overwrote the corruption. Now test the real failure mode:

```bash
printf 'deadbeef' > scalus-lean-proofs/lean/ScalusProofs/Generated/math_min.flat
git diff --exit-code scalus-lean-proofs/lean/ScalusProofs/Generated; echo "exit=$?"
git checkout scalus-lean-proofs/lean/ScalusProofs/Generated
```

Expected: `exit=1` with a diff shown. That is what CI sees when someone changes codegen without re-exporting.

- [ ] **Step 3: Add the nightly proof workflow**

Create `.github/workflows/lean-proofs.yml`:

```yaml
name: Lean-Proofs
on:
  workflow_dispatch:
  schedule:
    # 03:17 UTC daily. Off the hour to avoid the GitHub Actions rush.
    - cron: '17 3 * * *'
jobs:
  proofs:
    name: blaster
    runs-on: ubuntu-latest
    timeout-minutes: 60
    permissions:
      contents: read
      id-token: write
    steps:
      - name: Checkout
        uses: actions/checkout@v4
      - uses: nixbuild/nix-quick-install-action@v34
      - uses: DeterminateSystems/magic-nix-cache-action@main
        with:
          use-flakehub: false
      - name: Cache elan and lake
        uses: actions/cache@v4
        with:
          path: |
            ~/.elan
            scalus-lean-proofs/lean/.lake
          key: lean-${{ hashFiles('scalus-lean-proofs/lean/lean-toolchain', 'scalus-lean-proofs/lean/lake-manifest.json') }}
      - name: Build the proof suite
        working-directory: scalus-lean-proofs/lean
        run: |
          nix develop ../..#lean --accept-flake-config --command bash -c "lake build"
      - name: Run the proofs
        working-directory: scalus-lean-proofs/lean
        run: |
          nix develop ../..#lean --accept-flake-config --command bash -c '
            set -e
            fail=0
            for f in ScalusProofs/Sanity.lean ScalusProofs/Math.lean \
                     ScalusProofs/Data.lean ScalusProofs/Equivalence.lean; do
              echo "=== $f ==="
              out=$(lake env lean "$f" 2>&1) || true
              echo "$out"
              # Any outcome other than Valid or the expected falsification is a failure.
              if echo "$out" | grep -qE "Falsified|Undetermined|error"; then
                if echo "$out" | grep -q "Expected Falsified" \
                   && ! echo "$out" | grep -qE "^.*❌|Undetermined|error:"; then
                  :
                else
                  echo "FAILED: $f"
                  fail=1
                fi
              fi
            done
            exit $fail
          '
      - name: Report results
        uses: sarisia/actions-status-discord@v1
        if: always()
        with:
          webhook: ${{ secrets.DISCORD_WEBHOOK }}
          title: "[${{ github.event.repository.name }}] Lean-Proofs ${{ job.status == 'success' && '✅' || '❌' }}"
          url: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}
          username: "GitHub Actions"
          nodetail: true
          notimestamp: true
```

- [ ] **Step 4: Write the README**

Create `scalus-lean-proofs/README.md`:

```markdown
# scalus-lean-proofs

Proves properties of Scalus prelude functions against the **compiled UPLC**, not against the
Scala source, using IOG's [Blaster](https://github.com/input-output-hk/Lean-blaster) SMT
backend for Lean 4 and their [Lean model of UPLC](https://github.com/input-output-hk/PlutusCoreBlaster).

This tests the standard library and the code generator at the same time. A Scala unit test
exercises the JVM interpretation of `Math.gcd`; these proofs exercise the flat-encoded program
a validator actually runs on chain.

## Layout

- `src/main/scala/scalus/lean/` - the target catalogue and the exporter.
- `lean/ScalusProofs/Generated/` - **generated and committed**. One `.flat` hex file per
  target plus `Targets.lean`. Never edit by hand; run `sbt exportLeanUplc`.
- `lean/ScalusProofs/*.lean` - the hand-written properties.

## Running

```bash
sbt exportLeanUplc                      # regenerate lean/ScalusProofs/Generated
cd scalus-lean-proofs/lean
nix develop ../..#lean --command bash -c 'lake build'
nix develop ../..#lean --command bash -c 'lake env lean ScalusProofs/Math.lean'
```

Expect `✅ Valid` per theorem and `✅ Expected Falsified` per negative control. Anything
else, including `Undetermined`, is a failure.

## Adding a target

1. Add a `ProofTarget` to `ProofTargets.scala` with sample inputs and expected values. The
   Scala test runs those samples through Scalus's own CEK, so a wrong expectation is caught
   before it reaches Lean.
2. `sbt exportLeanUplc`, and commit the generated files.
3. Calibrate a budget (see below), then write the properties in a `lean/ScalusProofs/*.lean`
   file. Every group needs a negative control.

## The budget rule

`#prep_uplc target args N` symbolically unrolls the CEK machine `N` steps. Proofs are
therefore bounded: "for all inputs, up to N steps". Past the budget the machine returns
`State.Error`, so `fromFrameToInt` gives `none`, conditional theorems hold vacuously and
equations get falsified.

> **A falsification is not a bug until it is reproduced at twice the budget.**

This is not hypothetical. A `gcd` optimizer-equivalence proof once reported a counterexample
at `x = -19, y = 14`. It was false: the optimized program halts at step 282, the unoptimized
at 307, and the budget was 300. Both return 1.

The opposite mistake is just as bad. Proof cost grows superlinearly in the budget: for
`gcd_nonneg`, whose worst sample needs 203 steps, budget 250 proves in 2 seconds, 350 takes
52 seconds, and 500 does not finish. **Set the budget to about 1.25x the measured maximum.**
If a proof is slow, lower the budget toward that maximum; do not raise it.

Calibrate with `ScalusProofs.Prelude.steps`, which finds the exact minimum for the Lean
machine:

```lean
#eval steps mathGcd [(-19), 14] 8000
```

Do **not** derive budgets from Scalus's own step count. Plutus charges per `Eval` transition
while this machine counts `Eval` and `Return`, so the Lean figure is about 1.85x larger.

## Pinned dependencies

- `PlutusCore` from `nau/PlutusCoreBlaster` @ `fix/case-scrutinee-smt-blowup`. **Temporary
  fork.** Switch to upstream `main` once
  [input-output-hk/PlutusCoreBlaster#40](https://github.com/input-output-hk/PlutusCoreBlaster/pull/40)
  merges. Without that fix, `blaster` does not terminate on any program using UPLC `case`,
  which is every PV11 program Scalus emits.
- `Blaster` from `input-output-hk/Lean-blaster` @ `main`. Not the
  `beta-lambda-cache-optimization` branch: the two have diverged and neither is a superset.
  `main` carries the `Int.ediv`/`Int.emod` fix these integer targets need.

Upstream force-pushes. If `lake build` fails with `git exited with code 128`, a pinned rev is
gone; re-run `lake update` and commit the new `lake-manifest.json`.

## Limitations

1. **Blaster does not reconstruct proofs.** On `Valid` it uses `admit`. This is strong
   differential testing, not a kernel-checked guarantee. The trusted base includes Blaster's
   translation and Z3.
2. **Proofs are bounded** by the CEK step budget.
3. **We prove against the Lean model of UPLC**, not the Plutus reference implementation. The
   evidence for that model is that it passes the plutus-conformance corpus.
4. **`Value` and array builtins are out of scope.** Targets must compile with
   `valueBuiltins = false`; the model has no CIP-153 or CIP-138 builtins.
```

- [ ] **Step 5: Validate the workflow YAML**

```bash
python3 -c "import yaml,sys; yaml.safe_load(open('.github/workflows/lean-proofs.yml')); yaml.safe_load(open('.github/workflows/ci-jvm.yml')); print('yaml ok')"
```

Expected: `yaml ok`.

- [ ] **Step 6: Run the whole suite once more, end to end**

```bash
sbtn "scalusLeanProofs/test"
sbtn exportLeanUplc
git diff --exit-code scalus-lean-proofs/lean/ScalusProofs/Generated && echo "generated files fresh"
cd scalus-lean-proofs/lean
nix develop ../..#lean --accept-flake-config --command bash -c 'lake build'
```

Expected: tests pass, no diff, build succeeds.

- [ ] **Step 7: Commit**

```bash
git add .github/workflows/lean-proofs.yml .github/workflows/ci-jvm.yml scalus-lean-proofs/README.md
git commit -m "ci(lean-proofs): nightly proof run and a freshness gate

The proofs run nightly rather than per-PR: they take minutes and Z3 has
enough nondeterminism to make a blocking gate a nuisance. JVM CI instead
checks that the committed generated UPLC matches a fresh export, so a codegen
change that would invalidate the proofs is caught at PR time."
```

---

## Post-implementation follow-ups

Not tasks in this plan, but record them:

- **Watch [PlutusCoreBlaster#40](https://github.com/input-output-hk/PlutusCoreBlaster/pull/40).** When it merges, repoint `lakefile.lean` at upstream `main`, run `lake update`, commit the new manifest, and delete the fork note from the README.
- **Revisit `valueBuiltins`** when the upstream `value-builtins` and `array-builtins` branches merge; then `Value` operations become provable.
- **Validators** via `CardanoLedgerApiBlaster` are the natural next project. That needs a different argument encoding (`SpendingInput`, `validSpendingContext`) and much larger budgets.
- The spike files that produced these measurements have already been deleted from
  `scalus-core/jvm/src/test/scala/scalus/lean/`; nothing to clean up.

### Deliberate deviations from the spec

Recorded so they are not mistaken for oversights.

- The spec names an sbt task `checkLeanUplcUpToDate`. Task 8 implements the gate as
  `exportLeanUplc` followed by `git diff --exit-code` in CI instead, because that is the
  pattern the repo already uses for `llms-api.txt` in `release.yml`. Same guarantee, one less
  task key.
- The spec's property catalogue lists `pow`, `log2`, `isSqrt`, `sqrt`, `gcd divides both`, and
  `fromData(toData(x)) = x` round-trips. This plan does not prove them:
  - `sqrt` needs 405 steps, which lands in the region where proof cost explodes (budget 500
    did not finish in 500 seconds for a much simpler theorem). `math_sqrt` is still exported,
    so its generated `native_decide` checks still guard codegen, but it gets no `blaster`
    properties.
  - `pow`, `log2` and `isSqrt` are not in the catalogue at all, for the same cost reason.
  - `gcd divides both` needs divisibility reasoning, which is a poor fit for the bounded
    encoding.
  - `Data` round-trips need `Data`-typed arguments; the `ints` helper only covers integer
    arguments, so this needs a `datas` helper and a way to quantify over symbolic `Data`.
    Worth its own task once the integer pipeline is running.
  Add them in a follow-up, most likely by restricting input domains so the step count drops.
