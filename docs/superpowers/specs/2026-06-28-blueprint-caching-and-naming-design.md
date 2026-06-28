# Blueprint generation: caching, versioned filenames, package subfolders

Date: 2026-06-28
Status: Design — pending implementation
Scope: `scalus-sbt-plugin/src/main/scala/scalus/sbt/ScalusSbtPlugin.scala` (self-contained; no scalus-core changes)

## Problem

The `blueprint` task (and its `resourceGenerator`) currently:

1. **Regenerates on every build.** It is wrapped in `Def.uncached`, so each build reflectively
   loads every contract and re-runs `blueprintJson()` (SIR→UPLC compilation per contract) even
   when nothing changed. This is slow.
2. **Writes flat, version-less filenames.** Output is
   `resourceManaged/META-INF/scalus/blueprints/<SimpleClassName>.json`. Two validators with the
   same simple class name in different packages would clash, and a JSON gives no hint of which
   Scalus/Scala build produced it.

## Goals

- Cache blueprint generation: regenerate only when compiled classes or the toolchain versions change.
- Encode contract version, Scalus version, and Scala version in the filename.
- Nest output under the validator's package path to prevent same-name clashes.
- Leave exactly one current set of files in `resourceManaged` (and therefore in the JAR) — no
  orphaned files from renames, deletions, version bumps, or plugin upgrades.

## Non-goals

- No changes to scalus-core public API (no new `Contract` accessor). All work stays in the plugin.
- No change to the `deploy` task.
- No change to *what* the blueprint JSON contains — only where/how it is named and when it is written.

## Confirmed facts (from code investigation)

- Filename/layout is decided in `writeBlueprints` (`ScalusSbtPlugin.scala:156-170`).
- **No consumers.** Nothing in the repo reads `META-INF/scalus/blueprints/*.json` by name or by
  scan — files are only produced. Renaming/relayout is non-breaking.
- At generation time the plugin has: the full class name (e.g. `scalus.examples.auction.AuctionContract$`,
  yielding both package path and simple name) and the generated JSON string. The JSON `preamble`
  carries `title`, optional `version` (contract version), and `compiler.version` (Scalus version).
- `preamble.version` is `Option[String]` (`Blueprint.scala:285`), default `None`.
- Scala version is available as the sbt `scalaVersion` setting (e.g. `3.3.7`).

## Design

### 1. Filename and layout

```
META-INF/scalus/blueprints/<packagePath>/<ClassName>-<contractVer>-<scalusVer>-<scalaVer>.json
```

Example:
```
META-INF/scalus/blueprints/scalus/examples/auction/AuctionContract-1.0.0-0.18.2-3.3.7.json
```

Components, in order:

| Segment       | Source                                                        | Notes |
|---------------|---------------------------------------------------------------|-------|
| `packagePath` | full class name minus the simple name, dots → `/`             | rooted under the existing `META-INF/scalus/blueprints/` |
| `ClassName`   | `simpleName(className)` (existing helper)                     | filesystem-safe by construction |
| `contractVer` | `preamble.version` from the JSON, else placeholder `0.0.0`    | fixed position always present |
| `scalusVer`   | `preamble.compiler.version` from the JSON, **verbatim**, then filesystem-sanitized | snapshots appear in full |
| `scalaVer`    | `scalaVersion.value`                                          | full version, not binary |

**Sanitization.** Apply to every segment that comes from free-form data (`contractVer`, `scalusVer`):
replace any character outside `[A-Za-z0-9._]` with `_`. This keeps `0.18.2` intact and turns a
snapshot like `0.18.1+66-aa9cc792+20260619-1457-SNAPSHOT` into
`0.18.1_66-aa9cc792_20260619-1457-SNAPSHOT` (note: `-` is allowed, so it survives; `+` becomes `_`).
`ClassName` and `scalaVer` are already safe but pass through the same function defensively.

**Extracting `contractVer` and `scalusVer` from the JSON.** The plugin is Scala 2.13 with no JSON
dependency. Use a small targeted extractor on the (machine-generated, stable) JSON string:

- `scalusVer`: order-independent match of the compiler block —
  `"compiler"\s*:\s*\{[^}]*?"version"\s*:\s*"([^"]+)"`. Falls back to `"unknown"` if absent.
- `contractVer`: remove the matched compiler block from the string first, then take the first
  `"version"\s*:\s*"([^"]+)"`; if none, use `0.0.0`.

(The plugin already holds the JSON string returned by `blueprintJson()`, so no extra work to obtain it.)

### 2. Caching

Replace the per-build regeneration with `sbt`'s `FileFunction.cached` driving the *decision to run*:

- **Cache store:** `streams.value.cacheDirectory / "scalus-blueprints"` (stable location — do **not**
  vary it by version; FileFunction needs the prior run's recorded outputs to prune correctly).
- **Inputs (cache key):**
  1. Every regular file under `classDirectory` (`classesDir.allPaths` filtered to files) — covers any
     code change that could affect a validator. Coarse but always correct; incremental compile already
     gates how often this changes.
  2. A small **marker file** (written under the cache store) whose content is
     `scheme=<N>;scala=<scalaVersion>`, where `<N>` is a naming-scheme constant bumped whenever this
     plugin changes the filename format. The marker is rewritten **only when its content differs** from
     what is on disk (read-compare-write), so it does not spuriously invalidate the cache every build.
     Its purpose: force regeneration (and orphan cleanup) on a Scala-version change or a plugin upgrade
     that changes the scheme, even when `.class` files are byte-identical.
- **On cache hit:** skip entirely — no class loading, no SIR→UPLC.
- **On cache miss (run):**
  1. Generate all blueprints (existing `loadContracts` path).
  2. Write each to its new versioned, nested path.
  3. **Sweep orphans:** delete any `*.json` under the `META-INF/scalus/blueprints/` root that is not in
     the freshly produced set. This guarantees exactly one current set in `resourceManaged`/JAR and
     cleans up (a) renamed/deleted contracts, (b) previous-version filenames, and (c) flat files left by
     the pre-this-change plugin layout, which `FileFunction` alone would not know about.
  4. Return the produced files.

The existing `Def.uncached` wrapper stays (the task still returns `Seq[File]` and performs file I/O,
which is not meaningfully cacheable at sbt 2's task-result layer); the real work-skipping now comes from
`FileFunction.cached` inside the task. The `blueprint / skip` and `SCALUS_SKIP_BLUEPRINT` opt-outs are
unchanged.

### 3. Touched code

All within `ScalusSbtPlugin.scala`:

- `blueprintTask` / `writeBlueprints`: thread in `scalaVersion`, `streams.cacheDirectory`; wrap the
  generation body in `FileFunction.cached`; compute the new path per contract; add the orphan sweep.
- New private helpers: `sanitizeSegment`, `extractScalusVersion`, `extractContractVersion`,
  `packagePath`, and the marker read-compare-write. `simpleName` is reused as-is.

## Risks / things to verify during implementation

- **sbt 1 vs sbt 2 compatibility.** This source cross-compiles against both via the `sbtcompat`
  PluginCompat shim. Confirm `FileFunction.cached` (and `PathFinder#allPaths`) compile and behave under
  both. If sbt 2 diverges, fall back to a manual fingerprint cache (hash the input set + marker into a
  file under the cache store and compare) — same semantics, no `FileFunction` dependency.
- **JSON key-order assumption** for `contractVer` is sidestepped by removing the compiler block before
  matching; verify on a contract that sets `version` and one that does not (placeholder path).
- **Coarse cache key.** Any change under `classDirectory` regenerates *all* blueprints. Acceptable:
  generation is only expensive relative to a no-op build, and correctness beats fine-grained
  per-contract keys here.

## Verification plan

- `scalusExamplesJVM/clean` then build; assert files land at the nested versioned paths with the
  Scala version matching the active cross-build (`3.3.7` / `3.8.4`).
- Touch nothing and rebuild → generation is skipped (cache hit); assert no "Wrote ..." log lines.
- Edit one contract → rebuild → blueprints regenerate; the changed contract's file is rewritten and no
  stale-named file for it remains (sweep).
- Bump the scheme marker (or Scala version) → old-named files are removed, new ones written.
- Confirm a contract without `preamble.version` produces the `0.0.0` placeholder segment.
