# Explicit nulls migration

Status: step 1 (scalus-plugin) done on branch `worktree-explicit-nulls`. Run the next module
from this file.

## Goal

Every Scalus module compiles with Scala 3 `-Yexplicit-nulls`, main and test sources, on every
compiler the build supports. One module migrates at a time.

## Definitions

- **Flag**: the scalac option `-Yexplicit-nulls`. It is experimental. The name is the same on
  3.3.x, 3.8.x and 3.9.x.
- **Migrated module**: an sbt project whose `scalacOptions` contain the flag, for Compile and Test.
- **Copied core file**: a scalus-core file that `copySharedFiles` copies into scalus-plugin. It
  compiles in the plugin with the flag and in scalus-core without it.
- **Java result**: a value returned by a Java method or field, e.g. `String.split`,
  `Base64.getEncoder`, `IdentityHashMap.get`.

## Background

- Scala 3.3 types a Java result as `T | Null`. Scala 3.5+ types it as the flexible type `T?`,
  which is more lenient. The artifacts publish from 3.3.8, so 3.3 is the binding compiler.
- `T | Null` erases to `T`. MiMa does not see the change. A consumer without the flag sees `T`.
- On 3.8+, the compiler API is itself typed with `| Null`, e.g. `AbstractFile.file: JFile | Null`.
  On 3.3 the same member is `JFile`.

Probe on 3.3.8 (main sources, before any fix): core 314 errors, bloxbean 271, verification 134,
plugin 79, design-patterns 62 (all from one plugin bug), ledger 60, ledger-it 43 (tests),
examples 25, ts-exporter 22, testkit 21, utxo-cell 20 (tests), bench 18, others under 10.
Test-source counts are unknown for every module whose main sources failed.

## Module order

Migrate a module only after every module it depends on. `[EN-1]`

1. scalus-plugin (done)
2. scalus-core: shared, jvm, js, native
3. scalus-cardano-ledger: shared, jvm, js
4. scalus-testkit
5. Leaf modules, any order: examples, design-patterns, utxo-cell, streaming-fs2, bloxbean,
   uplc-jit-compiler, verification, verification-scalatest, ts-exporter, ethereum-kzg-ceremony,
   bench, llm-api-gen, cardano-ledger-it

The scalus-sbt-plugin MUST NOT get the flag. `[EN-2]`

**Why:** `[EN-1]`: an upstream module can change a public type from `T` to `T | Null`. A
downstream module that migrated first would break again. `[EN-2]`: the sbt plugin also builds on
Scala 2.12, which rejects the option.

When every module has migrated, the build SHOULD move the flag into `commonScalacOptions` and
delete the per-module lines. `[EN-3]`

The scalus-core migration MUST NOT fix `SIRHashCodeInRec`. `[EN-4]`

The scalus-core migration MAY delete `SIRHashCodeInRec` only after a release that contains the
deprecation has shipped. `[EN-5]` Until then, it MUST suppress the errors in that file with
`import scala.language.unsafeNulls`, with a comment that points to `[EN-4]`. `[EN-6]`

**Why:** `[EN-4]`: nothing uses the typeclass, and it never computed a hash. A fix would need its
own failing test for code nobody calls. `[EN-5]`: the annotation says `"1.4.1"` because AGENTS.md
takes the latest tag, but 1.4.1 shipped without it. The first release after 1.4.1 is the first
that carries the deprecation. `[EN-6]` is the one allowed exception to `[EN-15]`.

*Example, illustrative,* for `[EN-5]`:

| Latest released version | Delete? |
|---|---|
| 1.4.1 | no |
| the next release after 1.4.1, built from a master that contains the deprecation | yes |

## Per-module recipe

1. Add `scalacOptions += "-Yexplicit-nulls"` to the project in `build.sbt`.
2. Compile main and test sources on 3.3.8 and fix every error with the rules below.
3. Run the gates.
4. Commit one module per commit, with `scalafmtAll` first.

## Fix rules

- Code that tests a value for null first MUST copy it into a local `val` and test the `val`.
  `[EN-10]`
- Code that assumes a value is set MUST use `.nn` at the use. `[EN-11]`
- A field that holds null on purpose, e.g. a lazy cache or a `cause = null` default, MUST have
  the type `T | Null`. `[EN-12]`
- A fix MUST NOT change the result of any input, except an input that already is a bug state.
  `[EN-13]`
- A public API MUST NOT change `T` to `Option[T]`. `[EN-14]`
- New code MUST NOT use `import scala.language.unsafeNulls`. `[EN-15]`
- A test that checks null handling MAY use `unsafeNulls` locally, with a comment. `[EN-16]`

**Why:** `[EN-10]`: flow typing narrows a local `val`, not a `def` or a `var` field. `[EN-11]`:
`.nn` fails at the place the old code got its NPE, so the failure mode stays. `[EN-13]`: the
migration is a type change, and a behaviour change hidden in it would not get its own review.
`[EN-14]`: `Option` breaks binary compatibility. `[EN-15]`: the import switches the check off for
the whole file.

*Example, illustrative,* for `[EN-10]` and `[EN-11]`:

```scala
// [EN-10]: tested first
val ref = proxy.ref
if ref == null then tp else unrollTypeProxy(ref)

// [EN-11]: assumed set
val hexString = sc.s(args*).replace(" ", "").nn
```

*Example, illustrative,* for `[EN-13]`. A walker that matched a null `ref` into `case _` today
keeps that path with a guard: `case TypeProxy(ref) if ref != null =>`. A walker with no default
case threw `MatchError` on a null `ref`, so it gets `.nn` and still fails.

## Gates

A module is done only when every gate passes:

- The module compiles with the flag on 3.3.8 with 0 errors and 0 warnings, after a clean.
  `[EN-20]`
- The module compiles with the flag on every other supported compiler with 0 warnings. For the
  plugin that is 3.3.7, 3.8.4 and 3.9.0. `[EN-21]`
- Every module that is not migrated yet and compiles the changed files still compiles with 0
  warnings on 3.3.8, 3.8.4 and 3.9.0, on JVM, JS and Native. `[EN-22]`
- `sbtn quick` passes. `[EN-23]`
- `mimaReportBinaryIssues` passes for every changed published module. `[EN-24]`
- `scalusExamplesJVM/clean` then `scalusExamplesJVM/test` passes, so script hashes and ExUnits
  stay the same. `[EN-25]`

## Gotchas

- `set p/scalacOptions += ...` fails for a cross project. Write
  `set LocalProject("scalusJVM")/Compile/scalacOptions += "-Yexplicit-nulls"`.
- sbt prints at most 100 errors (`maxErrors`). Read the compiler's `N errors found` line.
- A `java.util.Map[K, Int]`: without the flag, `get` of a missing key unboxes to `0`, and
  `found != null` does not compile. On 3.9 with the flag, `found != null` compiles but `found` is
  still `0`. Use `if m.containsKey(k) then m.get(k).nn`.
- `x eq null` does not narrow on 3.3. Use `x == null`.
- `.nn` on a Java result gives no warning on 3.9, with or without the flag. `.nn` on a Scala value
  that is not nullable gives warning E216 on 3.9 with the flag.
- Typer errors stop the build before the Scalus plugin phase runs. Plugin-phase errors appear only
  after the typer errors are fixed.

## Done so far

- Plugin bug fixed: the plugin built a throwing body for each `Validator` endpoint that was not
  overridden. It looked up `RuntimeException(String)` with `=:= String`. Under the flag the
  parameter is `String | Null`, so a user project with the flag could not compile a `Validator`.
  Test: `ExplicitNullsTest`.
- `SIRHashCodeInRec` deprecated since 1.4.1 and no longer copied into the plugin. Nothing calls it,
  and it never computed a hash, see Gotchas and `[EN-4]`.
- scalus-plugin migrated, with 11 copied core files.
