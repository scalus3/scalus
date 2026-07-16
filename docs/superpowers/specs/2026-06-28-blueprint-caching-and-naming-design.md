# Blueprint generation: cached fresh-in-JAR output + explicit pin (Aiken-informed)

Date: 2026-06-28 (revised 2026-07-16)
Status: Design – all decisions resolved, pending user review
Scope: `scalus-sbt-plugin` + small additive `scalus-core` changes

## Problem

The `blueprint` task today:

1. **Regenerates on every build** (`Def.uncached`) – each build reflectively loads every contract and
   re-runs `blueprintJson()` (SIR→UPLC per contract). Slow.
2. **Writes flat, throwaway output** to `target/.../resourceManaged/META-INF/scalus/blueprints/<SimpleClassName>.json` –
   git-ignored, so nothing can be committed/pinned; two same-simple-named validators in different
   packages silently overwrite each other.

## Goals (clarified during design)

- **Pin the UPLC in git** when there is a usable version: a blueprint's compiled code / script hash is a
  function of `(source, contract version, Scalus version, Scala version)`; a committed blueprint locks
  that output, and git history is the version history.
- **Always ship fresh blueprints in the JAR** – an artifact must describe the code it actually contains.
- **Cache generation** – no SIR→UPLC work when nothing changed.
- **No same-name clashes** – layout keyed by the validator's fully-qualified name.
- **Ecosystem interop** – Aiken-style stable names with versions *inside* the file; a root `plutus.json`
  at the path external tooling expects.

## User-model analysis (drove the final shape)

- **Aiken migrants** expect `aiken build` semantics: stable root `plutus.json`, committed; version info
  inside the file, never in filenames.
- **Scala/Java developers** (the plugin's main audience) expect the opposite invariant: **the build never
  mutates tracked files**. Generated-and-committed artifacts on the JVM are produced by an *explicit*
  command plus a CI freshness check, not as a side effect of `compile`. Concretely:
  - IDE (Metals/bloop) imports trigger resource generation; auto-writing tracked files means the repo
    changes while browsing code.
  - sbt-dynver/sbt-git derive the version from git *dirty state*; a build that dirties the worktree can
    flip its own version to `-dirty`/`SNAPSHOT` mid-build. Scalus itself uses a git-derived version.
- **Snapshot churn:** `preamble.compiler.version` is git-derived for snapshot builds
  (`0.18.1+66-aa9cc792+…`), changing every commit. Auto-committed blueprints would churn perpetually in
  this repo and for downstream snapshot users; only released-version downstream would be stable.

Hence: automatic output goes to `target` (fresh, in JAR); committing is a deliberate act (`blueprintPin`).

## Confirmed facts

- Filename/layout decided in `writeBlueprints` (`ScalusSbtPlugin.scala:156-170`); the plugin has each
  contract's fully-qualified class name and its JSON string.
- **No in-repo consumer reads these files by name** – renaming/relayout is non-breaking.
- `scalus.cardano.blueprint.Blueprint` = `preamble: Preamble` + `validators: Seq[Validator]` with a
  jsoniter codec (`Blueprint.scala:23-45`); multi-validator documents are natively supported, and
  per-contract `title`/`description` already live on each `Validator`, so aggregation loses little.
- The Scalus version is already baked into each per-contract preamble's `compiler.version`
  (`CompilerInfo.currentScalus`, set at contract-compile time). Nothing needs to parse or recompute it.
- The downstream *Scala* version (the toolchain compiling the contracts, which sets the UPLC baseline)
  is `scalaVersion.value` in the plugin; it appears nowhere in the JSON today.
- **CIP-57 official schema** (`CIPs/CIP-0057/schemas/plutus-blueprint.json`): `preamble` and `compiler`
  set `additionalProperties: false` – extra keys there are *invalid*. The document **root** does not –
  extra top-level keys are valid CIP-57. (Aiken's parser also ignores unknown fields: serde default, no
  `deny_unknown_fields`.)
- **sbt resource collision:** the same relative path in both `src/main/resources` and `resourceManaged`
  makes JAR content ambiguous. Committed copies must therefore live *outside* resource directories.
- Aiken (`../aiken`): hard default `plutus.json` at project root; all validators in one file; compiler
  name+version recorded inside the file; filename always stable.

## Design

### Layer 1 – automatic, every build, cached (JAR contents)

Generated into `resourceManaged` (as today) via the resource generator, so every built JAR embeds
blueprints describing exactly its own code. Git is never touched.

1. **Per-contract files**, nested by package to prevent clashes:
   ```
   resourceManaged/main/META-INF/scalus/blueprints/<packagePath>/<SimpleClassName>.json
   e.g. .../META-INF/scalus/blueprints/scalus/examples/auction/AuctionContract.json
   ```
   Stable names – no versions in filenames; provenance lives inside the JSON (Aiken model).
2. **Aggregate** at resource root `/plutus.json`: one CIP-57 document merging every contract's
   validators. Project-level preamble: `title` = sbt `name`, `version` = sbt `version`, `compiler`
   carried over from the merged blueprints, `plutusVersion` from the contracts.

**Caching** via `FileFunction.cached` (store: `streams.value.cacheDirectory / "scalus-blueprints"`):

- Inputs: every regular file under `classDirectory`, plus a marker file holding
  `scheme=<N>;scala=<scalaVersion>;projectVersion=<version>` (rewritten only when its content differs,
  so no spurious invalidation; `<N>` bumped when the layout/format changes).
- Cache hit → skip entirely (no classloading, no SIR→UPLC).
- Cache miss → regenerate all, then **prune** any file under the blueprints output that is no longer
  produced (renamed/deleted contracts), leaving exactly the current set.
- Cross-building is naturally safe: each Scala version has its own `target/scala-<v>/resource_managed`.

### Layer 2 – explicit `blueprintPin` task (committed pins)

A deliberate task, run "whenever there is a usable version":

- Copies the current generated set to committed, non-resource locations at the project base directory:
  ```
  <projectBase>/plutus.json                              (aggregate – Aiken drop-in path)
  <projectBase>/blueprints/<packagePath>/<SimpleClassName>.json
  ```
- Replaces the `blueprints/` directory content (prunes stale files) – safe because it is an explicit act.
- The user commits the result; git history is the pin history.
- **`blueprintCheck`** task: fails if pinned files differ from freshly generated ones. For release
  pipelines – not regular CI, which cannot stay green under git-derived snapshot versions.
- **Cross-build guard:** optional `blueprintScalaVersion` setting; when set (e.g. to the LTS version in
  cross-built projects), `blueprintPin` refuses to run under any other `scalaVersion`. Single-version
  projects need not set it.

### Scala-version provenance – top-level extension key

CIP-57 forbids extra preamble keys but allows extra root keys, so record downstream toolchain info in a
namespaced top-level object:

```json
{
  "preamble": { ... },
  "validators": [ ... ],
  "scalus": { "scalaVersion": "3.3.7" }
}
```

Valid CIP-57, ignored by Aiken/JS consumers, extensible (room for e.g. plugin/SIR versions later).
Stamped into both per-contract files and the aggregate at generation time. Full provenance is then:
Scalus version in `preamble.compiler.version`, Scala version in `scalus.scalaVersion`.

### scalus-core additions (additive, backwards-compatible)

Robust JSON manipulation in a Scala-2.13 sbt plugin without a JSON lib is fragile; add helpers to
scalus-core (already on the reflection classpath), called reflectively with Java-friendly signatures:

```
case class Blueprint(preamble, validators, scalus: Option[ScalusInfo] = None)  // new optional field
case class ScalusInfo(scalaVersion: Option[String] = None)

object BlueprintTool:
  // merges validators into one CIP-57 doc; compiler info carried over from the inputs
  def aggregate(jsons: java.util.List[String], title: String, version: String,
                scalaVersion: String): String
  // returns the per-contract blueprint JSON with the top-level scalus.scalaVersion stamped in
  def stampScalaVersion(json: String, scalaVersion: String): String
```

Verify the `Blueprint(show) == blueprint` roundtrip still holds with the new optional field, and that
documents *without* the `scalus` key still deserialize (back-compat with existing files).

### Touched code

- `scalus-core`: `Blueprint.scalus` optional field + `ScalusInfo` + `BlueprintTool`.
- `scalus-sbt-plugin` `ScalusSbtPlugin.scala`: nested layout, aggregate, `FileFunction.cached` + marker +
  prune, new `blueprintPin` / `blueprintCheck` tasks, optional `blueprintScalaVersion` setting.
- Existing `blueprint / skip` and `SCALUS_SKIP_BLUEPRINT` opt-outs preserved.

## Resolved decisions

- **Write model = explicit pin.** Automatic generation stays in `resourceManaged` (fresh in every JAR,
  git untouched); committing is the deliberate `blueprintPin`. Reverses the earlier lockfile choice for
  the reasons in the user-model analysis. (Aiken-style auto-commit could later be offered as opt-in
  `blueprint / autoPin := true`; out of scope now.)
- **Granularity = both.** Per-contract files (nested by package) *and* a merged aggregate `plutus.json`.
- **Filenames = stable.** No versions in names; provenance inside the JSON; git history = pin history.
- **Aggregate locations.** In-JAR at `/plutus.json` (always fresh) + pinned at project root (Aiken
  drop-in). They intentionally differ between pins: the JAR describes itself, the pin is the blessing.
- **Scala version = top-level `scalus.scalaVersion` key.** CIP-57-valid (root allows extra keys;
  preamble does not).
- **Cross-build = primary version only for pins**, guarded by optional `blueprintScalaVersion`; the
  automatic layer runs for every cross-build into its own per-version target.

## Risks / to verify in implementation

- **sbt 1 vs sbt 2:** confirm `FileFunction.cached` / `PathFinder#allPaths` compile under both via the
  `sbtcompat` shim; fall back to a manual fingerprint-file cache if sbt 2 diverges.
- **Cross-classloader reflection** into `BlueprintTool` – keep signatures Java-typed.
- **Jsoniter roundtrip / back-compat** with the new optional `scalus` field (old files must still parse).
- **Aggregate forces one project version** – per-contract files retain their own preamble versions; the
  aggregate is a project-level view.
- **Pin staleness between releases** is by design; `blueprintCheck` exists for the moments it matters.

## Verification plan

- `scalusExamplesJVM/clean` + build → per-contract files at nested paths under `resourceManaged`, plus
  `/plutus.json` aggregate; packaged JAR contains both; `git status` untouched.
- Rebuild with no changes → cache hit, no regeneration logs, git untouched.
- Edit one contract → regeneration; changed contract's file and the aggregate updated; no stale files.
- Rename/delete a contract → its file pruned; aggregate drops it.
- `blueprintPin` → root `plutus.json` + `blueprints/<pkg>/...` written; re-running after a contract
  change updates them; `blueprintCheck` fails when pins are stale, passes right after a pin.
- With `blueprintScalaVersion` set, `blueprintPin` refuses under a non-primary Scala version.
- A blueprint with the `scalus` top-level key validates against the official CIP-57 schema and loads in
  existing consumers (jsoniter roundtrip + an Aiken-fixture-style parse).
