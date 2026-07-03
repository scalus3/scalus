# Blueprint generation: committed pin artifacts (Aiken-style), caching, per-contract + aggregate

Date: 2026-06-28 (revised 2026-07-03)
Status: Design — decisions resolved, pending user review of this spec
Scope: `scalus-sbt-plugin` + a small additive `scalus-core` helper

## Problem

The `blueprint` task today:

1. **Regenerates on every build** (`Def.uncached`) — each build reflectively loads every contract and
   re-runs `blueprintJson()` (SIR→UPLC per contract). Slow.
2. **Writes flat, throwaway output** to `target/.../resourceManaged/META-INF/scalus/blueprints/<SimpleClassName>.json` —
   git-ignored, so it can't be committed; two same-simple-named validators in different packages
   silently overwrite each other.

## What we actually want (clarified during design)

Blueprints should become **committed pin artifacts**, adopting Aiken's model where it fits:

- **Pin the UPLC in git.** A blueprint's compiled code / script hash is a function of
  `(source, contract version, Scalus version, Scala version)`. Committing the blueprint locks that
  output; git history is the version history (`git show <rev>:plutus.json`), and a clean content diff
  shows exactly what a toolchain bump or source edit did to the on-chain script.
- **Always shipped in the JAR.**
- **Ecosystem interop.** Aiken emits a single stable `plutus.json` (never versioned filenames; version
  lives in `preamble.compiler`), consumed by off-chain tooling by that known path. We adopt this
  *philosophy*: **stable names, version-in-preamble, committed, regenerated every build**.

### Why not copy Aiken verbatim (single aggregate file only)

- **Cross-compilation.** Scalus cross-builds Scala 3.3.7 and 3.8.4, which emit *different* UPLC (the
  dual-ExUnits baselines). One `plutus.json` cannot hold two Scala baselines.
- **Multi-contract repos.** `scalus-examples` has ~21 unrelated contracts; one 21-contract file means
  noisy diffs and merge conflicts. Per-contract files diff cleanly and scale.
- **Per-contract preamble.** Each Scalus `Contract` carries its own title/version/description; a single
  aggregate must hoist one project-level preamble, losing per-contract version granularity.

**Decision (confirmed): emit BOTH** — per-contract files *and* a merged aggregate.

## Confirmed facts

- Filename/layout decided in `writeBlueprints` (`ScalusSbtPlugin.scala:156-170`); the plugin has the full
  class name and each contract's JSON string.
- **No in-repo consumer reads these files by name** — renaming/relayout is non-breaking.
- `scalus.cardano.blueprint.Blueprint` = `preamble: Preamble` + `validators: Seq[Validator]`, with
  `addValidator` and a jsoniter codec (`Blueprint.scala:23-45`). It already supports multiple validators
  per document, so aggregation is a natural fit. Per-contract `title`/`description` already live on each
  `Validator`, so flattening loses little.
- `Preamble` (`Blueprint.scala:285`): `title`, `description?`, `version?`, `compiler?`, `plutusVersion?`,
  `license?`. `compiler = CompilerInfo(name, version)` — currently `{name:"scalus", version:<BuildInfo>}`.
- Aiken (`../aiken`): hard default `plutus.json` at project root; all validators in one file; compiler
  name+version (incl. git hash) recorded **inside** the file; filename always stable.
- Scala version is available to the sbt plugin as `scalaVersion`; it is **not** currently in the JSON.

## Design

### Outputs (both stable-named, committed, regenerated-when-changed, cached)

1. **Per-contract files** — one per `Contract`, nested by package, keeping its own preamble:
   ```
   src/main/resources/META-INF/scalus/blueprints/<packagePath>/<SimpleClassName>.json
   e.g. src/main/resources/META-INF/scalus/blueprints/scalus/examples/auction/AuctionContract.json
   ```
   - `src/main/resources` is committed by the user AND packaged into the JAR by sbt's default resource
     handling → satisfies both "committed" and "in JAR" from one location.
   - Package path from the full class name; simple class name for the file (filesystem-safe, no title
     spaces). Nesting prevents same-name clashes.

2. **Aggregate `plutus.json`** — one merged CIP-57 document with every contract's validators:
   - Location: **project root `plutus.json`** (Aiken drop-in for external tooling) **and** embedded in
     the JAR at resource root `/plutus.json` (so JVM consumers can load the aggregate by a known path).
     Both committed; same content. *(Decision A.)*
   - Project-level preamble: `title` = sbt `name`, `version` = sbt `version`,
     `compiler = {name:"scalus", version:<scalusVersion>}`, `scalaVersion` = sbt `scalaVersion`,
     `plutusVersion` from the contracts.
   - `validators` = concatenation of every contract's validators (each retains its own title/description).

### Generation, caching, pruning

A single task (run as part of the build so JAR/commit stay current), wrapped in `FileFunction.cached`:

- **Inputs (cache key):** every regular file under `classDirectory`, plus a marker file holding
  `scheme=<N>;scala=<scalaVersion>;projectVersion=<version>` (rewritten only when its content changes, so
  no spurious invalidation). `<N>` is a naming/format-scheme constant bumped when this layout changes.
- **On cache hit:** skip entirely (no class loading, no SIR→UPLC) → no git churn.
- **On cache miss (run):**
  1. Load contracts (existing reflection path), get each `blueprintJson()`.
  2. Stamp each with `scalaVersion` and write the per-contract files to the committed resource dir.
  3. Build the aggregate via `BlueprintTool.aggregate` (reflectively) and write it to **both** repo-root
     `plutus.json` and `src/main/resources/plutus.json` (Decision A).
  4. **Prune** per-contract files under the blueprints root that are no longer produced (renamed/deleted
     contracts), leaving exactly the current set.
- Because content is deterministic, an unchanged rebuild rewrites identical bytes → git stays clean; a
  fresh clone that already has committed files regenerates matching content → clean `git status`.

**Cross-build enforcement (Decision C).** A `blueprintScalaVersion` setting names the one Scala version
whose output is committed (defaults to the project's primary/LTS — e.g. the first of `crossScalaVersions`).
When `scalaVersion.value == blueprintScalaVersion`, generation writes to the committed
`src/main/resources` (and repo-root `plutus.json`). For any other cross-build, generation instead writes
to `resourceManaged`/`target` — so that build's JAR still embeds blueprints ("always in JAR" holds for
every cross-build), but git is never touched by a non-primary build.
- **Freshness gate (recommended):** a `blueprintCheck` mode for CI that fails if regeneration would change
  committed files — catches stale commits, the standard price of committed-generated artifacts.

### scalus-core additions (additive, backwards-compatible)

Robust JSON merging in a Scala-2.13 sbt plugin without a JSON lib is fragile; instead add a helper to
scalus-core (already on the reflection classpath) and call it reflectively with Java-friendly types:

```
object BlueprintTool:
  // returns merged CIP-57 JSON; java types for safe cross-classloader reflection
  def aggregate(jsons: java.util.List[String], title: String, version: String,
                scalusVersion: String, scalaVersion: String): String
  // returns the same per-contract blueprint JSON with preamble.scalaVersion stamped in
  def stampScalaVersion(json: String, scalaVersion: String): String
```

Also add an optional `Preamble.scalaVersion: Option[String]` field (*Decision B*). Per-contract files are
stamped via `stampScalaVersion`; the aggregate gets it from `aggregate`. So every blueprint records its
full toolchain provenance (Scalus in `compiler.version`, Scala in `scalaVersion`).

### Touched code

- `scalus-core`: new `BlueprintTool.aggregate` (+ optional `Preamble.scalaVersion`).
- `scalus-sbt-plugin` `ScalusSbtPlugin.scala`: write to the committed resource dir; add the aggregate;
  thread `scalaVersion` / `name` / `version`; wrap in `FileFunction.cached`; add prune + `blueprintCheck`.
- Existing `blueprint / skip` and `SCALUS_SKIP_BLUEPRINT` opt-outs preserved.

## Resolved decisions

- **A. Aggregate location = project root + in JAR.** Write `plutus.json` to the repo root (Aiken drop-in,
  committed) and to `src/main/resources/plutus.json` (committed + embedded at `/plutus.json` in the JAR).
  Same content, two locations.
- **B. Record Scala version = yes.** Add optional `Preamble.scalaVersion`, stamped at generation. Extra
  optional key; verify the CIP-57 roundtrip still holds.
- **C. Cross-Scala-build = primary version only.** Committed generation targets the project's primary/LTS
  Scala version; secondary cross-builds write to `target` only (not committed). `scalus-examples` commits
  its LTS baseline. (A `scala-<ver>/` path segment can be added later if a project must commit multiple
  baselines, but that is out of scope here.)

## Risks / to verify in implementation

- **Build mutates tracked files** (the lockfile pattern). Mitigated by caching (rewrite only on real
  change) + the `blueprintCheck` CI gate.
- **sbt 1 vs sbt 2**: confirm `FileFunction.cached` / `PathFinder#allPaths` compile under both via the
  `sbtcompat` shim; fall back to a manual fingerprint-file cache if sbt 2 diverges.
- **Cross-classloader reflection** into `BlueprintTool.aggregate` — keep the signature Java-typed.
- **CIP-57 strictness** if adding `Preamble.scalaVersion` (decision B) — extra optional key; verify the
  `Blueprint(show) == blueprint` roundtrip still holds.
- **Aggregate forces one project version** — per-contract files retain their own versions; the aggregate
  is a project-level view.

## Verification plan

- `scalusExamplesJVM/clean` + build → per-contract files land at nested paths and a merged `plutus.json`
  is produced; committed content matches (clean `git status`).
- Rebuild with no changes → generation skipped (cache hit), no "Wrote…" logs, clean git.
- Edit one contract → only its per-contract file and the aggregate change; no stale files remain.
- Rename/delete a contract → its per-contract file is pruned; aggregate drops it.
- `blueprintCheck` fails when committed files are stale, passes when current.
