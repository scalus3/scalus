# UPLC Source View – Design

Date: 2026-07-31
Status: Approved design, pending implementation plan
Repos: `scalus` (producer), `scalus-vscode-extension` (consumer)

## Goal

Show the compiled UPLC for a given Scala source line, definition, or function in the
Scalus Profiler VS Code extension. A side-by-side, bidirectionally synced view:
moving the cursor in the Scala file highlights the matching UPLC text, and moving
the cursor in the UPLC view highlights the originating Scala range.

## Feasibility summary (verified)

- Every UPLC `Term` node carries `UplcAnnotation(pos: ScalusSourcePos, functionName: String)`
  (`scalus-core/shared/src/main/scala/scalus/uplc/UplcAnnotation.scala`).
- `ScalusSourcePos` has file, 0-based start/end line and column, and an `inlinedFrom` chain.
- Positions survive V3 lowering (~40 stamp sites in `LoweredValue.scala`), all UPLC
  optimizer passes, `DeBruijn`, and `TermSanitizer`. Two fill passes in
  `CompiledPlutus.toUplc` (`Compiled.scala`) give near-total coverage on the final term.
- Positions are erased at flat/CBOR encoding. The mapping must be extracted from the
  in-memory `Term` before serialization.
- `UplcAnnotation.functionName` is currently never populated (dead field).
- The `Pretty[Term]` printer (`Term.scala`, paiges) discards annotations today.

Ecosystem check: neither Aiken (shipped) nor Plutus emits a source-map file; both use
custom in-band/JSON approaches, not JS Source Map v3. Aiken's unmerged
`pi/source-maps` branch uses a custom JSON keyed by post-order node index; we adopt
that index as a forward-compatible join key.

## Decisions

| Decision | Choice |
|---|---|
| Primary UX | Side-by-side synced view (Compiler Explorer style) |
| Artifact timing | Written with profile reports (evaluation time) |
| Sync direction | Bidirectional |
| Function granularity | Populate `functionName` during lowering; spans carry it |
| Renderer | Existing paiges printer + decorator hook + zero-width markers |
| Format | Custom JSON (no ecosystem standard exists); includes post-order node index |

## Architecture

### 1. Scalus: function names in annotations

- Add a `currentFunction: String` field to the V3 lowering context.
  Set it when lowering a top-level binding and when lowering a `Let`-bound lambda.
- Annotation construction sites in `LoweredValue.scala` go through one helper that
  builds `UplcAnnotation(pos, ctx.currentFunction)`.
- Extend `fillEmptyPosBottomUp` / `fillEmptyPosTopDown` to back-fill the whole
  annotation (pos and functionName), not only pos. Method signatures do not change,
  so MiMa is unaffected.

### 2. Scalus: span-aware rendering (hook + markers)

Why not record offsets in `Pretty[Term]` directly: the printer builds a paiges `Doc`,
a layout tree. Text offsets exist only after `doc.render(width)`, and paiges provides
no render callback and no annotation channel (unlike Haskell's `prettyprinter`).
The printer already depends on paiges zero-width output for ANSI styling
(`d.style(...)` in `Term.scala`), so zero-width markers use a supported mechanism and
provably do not change layout.

Design:

- Refactor the `Pretty[Term]` printer to accept a decorator hook
  `(Term, Doc) => Doc`, default identity. Default output stays byte-identical.
- New `UplcSourceMapRenderer` (in `scalus.uplc.internal`, together with
  `ProfileReportWriter`: public utilitarian tooling with no binary-compatibility
  guarantees — the `.uplc.json`/manifest formats are the contract, and the
  package is wildcard-exempt from MiMa. Both are JVM-only; JS gets a no-op
  `ProfileReportWriter` stub so the formatter, manifest codec and renderer stay
  dead-code-eliminated from the `scalus.js` bundle):
  - The hook wraps each node that has a non-empty effective position in
    `Doc.zeroWidth` markers: `<id>` before, `/<id>` after.
    `id` indexes an array of collected annotations.
  - Render at the same width the plain `show` uses.
  - One post-render scan strips markers and records `(startOffset, endOffset)` per id
    in the clean text. Spans nest; nesting is expected and used for innermost-match.
  - During the same traversal, assign each node its post-order index (children
    visited in declaration order, then the parent). Post-order keeps existing
    indices stable when a program is later wrapped in `Apply` nodes for parameter
    application, matching Aiken's convention.
- Invariant (tested): marker-stripped output equals the plain pretty output.

The renderer runs on the same in-memory `Term` the CEK machine evaluated. That term's
positions already feed the profiler, so it is available at report time by construction.

### 3. Artifact and manifest

New file per run, next to the profile files (default `target/scalus/`):
`<scriptHash>-<tag>-<index>.uplc.json`

```json
{
  "schemaVersion": 1,
  "uplc": "(program 1.1.0 ...)",
  "files": ["/abs/path/Validator.scala"],
  "functions": ["validate", "checkSig"],
  "spans": [
    { "s": 120, "e": 245, "n": 17,
      "file": 0, "sl": 16, "sc": 4, "el": 18, "ec": 20, "fn": 0 }
  ]
}
```

- `s`/`e`: character offsets into `uplc` (start inclusive, end exclusive).
- `n`: post-order node index of the term node. Forward-compatible join key for
  future consumers that work on decoded on-chain scripts (debuggers, coverage).
- `file`/`fn`: indices into the `files`/`functions` string tables. `fn` optional.
- `sl`/`sc`/`el`/`ec`: 0-based source lines and columns (raw `ScalusSourcePos`).
  Note: `profile.json` uses 1-based lines; this artifact is 0-based and documents it.
- `inlinedFrom` is omitted in v1.
- Spans are emitted only for nodes with a non-empty effective position.

Wiring:

- `ProfileReportWriter.write` takes the evaluated term as an optional parameter and
  writes the artifact when the profile level is `Full`, the term carries source info,
  and the run rendered at least one profile file (a console-only report stays off
  disk, and never replaces a manifest run that indexes profile files). No new
  `ProfileFormat` case: those are rendered from `ProfilingData`, which has no `Term`.
- The file registers in the existing `profile-manifest.json` run as
  `{ "format": "uplc", "file": "..." }`. Manifest `schemaVersion` stays 1; the
  extension's `parseManifest` ignores unknown formats, so old extension versions are
  unaffected.
- Writer plumbing follows `ProfileReportWriter` (jsoniter-scala codec, existing
  locked read-merge-write for the manifest).
- Producers: `PlutusScriptEvaluator.renderProfile` and
  `ScalusTest.runWithProfileReport`, i.e. both existing manifest writers.

### 4. Extension: side-by-side synced view

- New pure module `src/uplcMap.ts`: parse the artifact, offset-to-position helpers,
  span queries (spans intersecting a source range; innermost span containing a UPLC
  offset; all spans of a function). No `vscode` imports, testable in `test/smoke.ts`.
- New `UplcContentProvider` (`TextDocumentContentProvider`, scheme `scalus-uplc`).
  Document content is the `uplc` text of the active run. Read-only by construction.
- Commands:
  - `scalusProfile.showUplc`: opens the UPLC document beside the active editor
    (`ViewColumn.Beside`) for the run selected in `ProfileStore`.
  - `scalusProfile.showUplcForFunction`: highlights every span whose `fn` matches the
    function under the cursor.
- Cursor sync via `window.onDidChangeTextEditorSelection`:
  - Scala to UPLC: match the file with the existing `bestMatchingFile`; find spans
    whose source range contains the cursor; decorate those UPLC ranges
    (`TextDocument.positionAt(offset)`); reveal the first.
  - UPLC to Scala: find the innermost span containing the cursor offset; decorate
    and reveal its source range in the matching Scala editor.
- Highlight decoration uses a theme color (e.g. `editor.findMatchHighlightBackground`).
- The UPLC document follows the selected run; `ProfileStore.onDidChange` refreshes it.
- Nice-to-have: contribute a minimal TextMate grammar for language id `uplc` so the
  view is not plain text.

### 5. Error handling

- Run has no `uplc` file: info message "No UPLC map found. Re-run a profiled test
  with a Scalus version that emits it."
- Source drift after edits: highlights may be off until the profile is regenerated.
  Same limitation as the existing cost decorations; accepted for v1.
- Path mismatch (artifact produced on CI or another checkout): handled by the
  existing trailing-segment `bestMatchingFile` matching.
- Malformed or wrong `schemaVersion` artifact: treated as absent, log to the output
  channel.

### 6. Testing

Scalus:
- Invariant test: marker-stripped render equals plain pretty render for a corpus of
  compiled programs.
- Span correctness: compile a small validator, assert selected spans map to expected
  source lines and function names.
- Manifest test: `uplc` entry merges into an existing manifest without dropping runs.

Extension:
- Unit tests in the vscode-free smoke harness: artifact parsing, innermost-span
  query, source-range intersection, function-span query.

### Out of scope (future work)

- Cost overlay inside the UPLC document (per-span cpu/mem from `profile.json`).
- `inlinedFrom` chains in the artifact and hover.
- Build-time sbt task emitting the artifact without running tests.
- Standard Source Map v3 emission for external tooling.

## Delivery

Two-repo rollout, producer first:

1. `scalus`: annotations, renderer, artifact writer, tests. Ships in the next release.
2. `scalus-vscode-extension`: consumer feature; degrades gracefully (info message)
   when the artifact is absent.
