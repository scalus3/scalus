# Claude Code Guidelines for Scalus Project

## Project Overview

Scalus is a platform for developing decentralized applications (DApps) on Cardano.
It compiles a subset of Scala 3 to Scalus Intermediate Representation (SIR) and then
lowers to Untyped Plutus Core (UPLC), the language of Cardano smart contracts.

**For smart contract development:** Use `/contract` skill
**For smart contract testing:** Use `/contract-test` skill

## Commands

Prefer `sbtn` over `sbt`, when `sbtn` is aviable and not hangs. 

| Command          | Purpose                                              |
|------------------|------------------------------------------------------|
| `sbtn quick`     | Format, compile, jvm/testQuick (fast iteration)      |
| `sbtn testQuick` | Run only tests affected by recent changes            |
| `sbtn ci`        | Full CI: format check, all platforms, docs, mima     |
| `sbtn it`        | Integration tests (requires Docker)                  |

**Compilation:**

- `sbtn jvm/Test/compile` - all JVM projects with tests
- `sbtn scalusJVM/Test/compile` - scalus-core JVM with tests
- `sbtn scalusCardanoLedgerJVM/compile` - cardano-ledger JVM without tests
- `sbtn "clean; Test/compile"` - full clean compilation, useful for checking for warnings. Otherwise
  incremental compilation could not show warnings in already compiled files

**Testing:**

- `sbtn jvm/test` - all JVM tests
- `sbtn jvm/testQuick` - run only tests affected by recent changes (incremental)
- `sbtn scalusJVM/test` - core tests
- `sbtn scalusExamplesJVM/test` - example tests
- `sbtn scalusCardanoLedgerIt/test` - integration tests

**Note:** sbt uses incremental compilation - only changed files and their dependents are recompiled.
Use `sbtn clean` to force full recompilation if you encounter stale class issues.

### Working in a git worktree with sbt

When the repo is checked out in a **git worktree** (`git worktree add ...`, e.g. `.claude/worktrees/<name>`),
sbt fails to load out of the box. Two one-time fixes are needed inside the worktree:

1. **`sbt-git` / JGit cannot read the worktree's `.git` file.** Loading the build throws
   `org.eclipse.jgit.errors.NoWorkTreeException: Bare Repository has neither a working tree, nor an index`
   (the bundled JGit 5.13 does not understand a worktree's `gitdir:`-pointer `.git`). Work around it by
   adding a local, **untracked** `.sbt` file at the worktree root that short-circuits the eager git
   settings with constants (do NOT commit it):

   ```scala
   // zz-worktree-git-override.sbt  — local worktree workaround, do not commit
   import com.github.sbt.git.SbtGit.git
   ThisBuild / git.gitUncommittedChanges := false
   ThisBuild / git.gitHeadCommit := Some("0000000000000000000000000000000000000000")
   ThisBuild / git.gitCurrentTags := Nil
   ThisBuild / git.gitCurrentBranch := "<your-branch>"
   ThisBuild / git.gitHeadMessage := None
   ```

   Add `zz-worktree-git-override.sbt` to your worktree's `.git/info/exclude` (or just leave it unstaged)
   so it never lands in a commit.

2. **The `plutus-conformance` corpus is not present in the worktree.** `scalusJVM/Test/compile` fails with
   `Plutus conformance corpus not found at .../plutus-conformance/test-cases/uplc/evaluation` (it is a
   symlink into the nix store that only exists in the primary checkout). Recreate the symlink in the
   worktree, pointing at the same target as the primary checkout:

   ```bash
   ln -s "$(readlink /path/to/primary/checkout/plutus-conformance)" <worktree>/plutus-conformance
   ```

   It is git-ignored, so it won't show up in `git status`.

## Architecture

### Compilation Pipeline

Scala Source → SIR (via compiler plugin) → UPLC (via SIR compiler) → Plutus Script (V1/V2/V3)

### Module Structure

**Cross-platform modules (JVM/JS, some with Native):**

| Module                         | Purpose                           | sbt Project                                       |
|--------------------------------|-----------------------------------|---------------------------------------------------|
| `scalus-core/`                 | Plutus VM, UPLC, standard library | `scalusJVM`, `scalusJS`, `scalusNative`           |
| `scalus-cardano-ledger/`       | Transaction building, ledger      | `scalusCardanoLedgerJVM`, `scalusCardanoLedgerJS` |
| `scalus-testkit/`              | Testing utilities                 | `scalusTestkitJVM`, `scalusTestkitJS`             |
| `scalus-examples/`             | Smart contract examples           | `scalusExamplesJVM`, `scalusExamplesJS`           |

**JVM-only modules:**

| Module                         | Purpose                           | sbt Project                         |
|--------------------------------|-----------------------------------|-------------------------------------|
| `scalus-plugin/`               | Scala 3 compiler plugin           | `scalusPlugin`                      |
| `scalus-plugin-tests/`         | Plugin test suite                 | `scalusPluginTests`                 |
| `scalus-uplc-jit-compiler/`    | Experimental UPLC JIT compiler    | `scalusUplcJitCompiler`             |
| `scalus-design-patterns/`      | Design pattern examples           | `scalusDesignPatterns`              |
| `bloxbean-cardano-client-lib/` | Bloxbean CCL integration          | `scalus-bloxbean-cardano-client-lib`|
| `scalus-cardano-ledger-it/`    | Integration tests                 | `scalusCardanoLedgerIt`             |
| `bench/`                       | JMH benchmarks                    | `bench`                             |
| `scalus-docs/`                 | API documentation (Unidoc)        | `docs`                              |

### Key Source Locations

- SIR compiler: `scalus-core/shared/src/main/scala/scalus/compiler/sir/`
- UPLC implementation: `scalus-core/shared/src/main/scala/scalus/uplc/`
- Plutus VM (CEK): `scalus-core/shared/src/main/scala/scalus/uplc/eval/`
- Standard library: `scalus-core/shared/src/main/scala/scalus/prelude/`
- Compiler plugin: `scalus-plugin/src/main/scala/scalus/plugin/`
- Transaction builder: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/`
- Examples: `scalus-examples/shared/src/main/scala/scalus/examples/`

## Development Guidelines

### Before Completing Tasks

Run `sbtn quick` to ensure formatting, compilation, and tests pass.
If issues occur, try `sbtn clean` first.

### Version control

When creating files, make sure to run `git add` to add them to the version control system. 

### Multi-Platform Architecture

- Shared code goes in `shared/src/main/scala/`
- Platform-specific code goes in `jvm/`, `js/`, `native/`
- Tests mirror source structure in `shared/src/test/scala/`

### Scala 3 Code Style

Use `{}` for top-level definitions and multi-line function bodies.
Use indentation-based syntax for `if`/`match`/`try`/`for`, unless it's too big (more than 20 lines).
Use braces otherwise.
Use `then` in `if` expressions and `do` in `while` loops.

```scala
object Example {
    def exampleFunction(x: Int): Int = {
        if x > 0 then x * 2
        else
            val y = -x
            y * 2
    }

    def describe(x: Any): String = x match
        case 1 => "one"
        case _ => "other"
}
```

## Cross-Language Interop Style

New public code and refactorings in the **designated interop packages** (the
ledger domain objects — `Transaction`, `TransactionOutput`, `Value`, … — plus
`Data`/`ToData`/`FromData`, `TxBuilder`, `Emulator`, `PlutusVM`) must follow the
cross-language interop guide so the API stays usable from Java/Kotlin/JavaScript.
Full design: `docs/superpowers/specs/2026-07-11-cross-language-interop-style-guide-design.md`.

**Decision rule:** if the interop-friendly form is equally good Scala, make it the
only form (Tier 0). If it degrades Scala ergonomics, put it in a per-platform
`<ClassName>Platform` trait instead.

**Tier 0 — author directly into the shared public type (no Scala cost):**

- No default parameters on public APIs — provide explicit overloads instead
  (Java/Kotlin can't use `$default$`; Scala.js default-param export is buggy).
- `@scala.annotation.varargs` on public vararg methods.
- Keep symbolic operators (`+`, `unary_-`) but add named aliases (`def plus`,
  `def negate`).
- Public factories return concrete types (no `Product`/path-dependent/`IterableOnce`).
- For every public `given` codec (`ToData`/`FromData`/`Encoder`/`Decoder`), also
  expose a non-implicit entry (`Foo.toData(x)`, `Foo.fromData(d)`).
- Public callback types are `@FunctionalInterface` single-method interfaces.
- No `using`/context params on public entry points — take them as ordinary
  parameters, with a Scala overload that supplies the `given` (see `fromCbor`).

**Tier 1 — per-platform `<ClassName>Platform` mixin (only for divergent members):**

`shared/` classes can't gain instance methods from platform-only files, so
divergent Java/JS members go in a trait of the same FQN with a different body per
platform (same idiom as `scalus.uplc.builtin.platform`):

```scala
case class Transaction(...) extends TransactionPlatform          // shared/
// jvm/: full Java idiom (orNull getters, java.util.List, builders)
// js/:  full JS idiom (@JSExport, js.Array, Long/BigInt -> String)
// native/: empty
private[ledger] trait TransactionPlatform extends InteropApi { self: Transaction => ... }
```

Every platform trait extends the shared marker `InteropApi` and must exist on
every platform the class compiles to (empty is fine).

**Stability:** interop packages are the MiMa-stable surface; route churn-prone
additions into `*.internal` subpackages. `InteropSurfaceTest` mechanically
enforces the rules. Internal/compiler/prelude code is out of scope — stays fully
idiomatic Scala.

## Commit Guidelines

- Use conventional commit style: `feat:`, `fix:`, `docs:`, `refactor:`, `test:`, `chore:`
- Keep messages short: 1-2 paragraphs
- Mention key changes
- Never add "Co-authored by Claude Code" or similar

## Libraries in Tests

- Use Scalus data types when appropriate
- Use jsoniter-scala for JSON parsing
- Use com.lihaoyi.pprint to display complex data types

## Reference Projects

- [Cardano Ledger](https://github.com/IntersectMBO/cardano-ledger)
- [Plutus](https://github.com/IntersectMBO/plutus)
- [Amaru](https://github.com/pragma-org/amaru)

These can be obtained in sibling directories (`../`) or directly on GitHub.

## Backwards Compatibility

- Maintain backwards compatibility for public APIs
- Use `sbtn mima` to verify compatibility
- Use `@deprecated("use XYZ instead", "$version")` on deprecated APIs
- Use latest git tag for deprecated $version (e.g., if tag is "v0.14.2", write `@deprecated("use XYZ instead", "0.14.2")`)

## Important Files

- `build.sbt` - Build configuration
- `.scalafmt.conf` - Code formatting
- `CONTRIBUTING.md` - Contribution guidelines
- `scalus-site/content/` - Documentation
