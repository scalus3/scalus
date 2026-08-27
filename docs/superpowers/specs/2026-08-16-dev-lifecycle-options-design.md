# Dev lifecycle: Options presets, compile default, debug-twin registration

Date: 2026-08-16
Status: approved design, pending implementation

## The model

A contract is ONE compiled artifact (SIR). The **release** lowering is its
identity: the script hash, the blueprint entry, the budgets. The **debug
twin** - same SIR, traces on - is a diagnostic *view* derived from it, never
deployed, consulted only when something fails. `Options` select a view; they
are not a project-wide mode.

Lifecycle mapping:

| Stage | Runs against | Mechanism |
|---|---|---|
| Negative tests (`assertScriptFail`), log debugging | debug twin | diagnostic replay via `debugScripts` |
| Budget/perf tests | release artifact | pins match production |
| Testnet deploy | release blueprint | same hash you ship to mainnet |
| Production deploy | pinned blueprint | `blueprintPin` + `blueprintCheck` in CI |
| Live tx failure diagnostics | release in tx, twin on silent failure | `PlutusScriptEvaluator.replayWithDiagnostics` |

What already exists (verified 2026-08-16):

- `DebugScript.fromCompiled(compiled)` = lazy `compiled.withErrorTraces.script`
  (`scalus-core/.../uplc/DebugScript.scala:44`).
- `PlutusScriptEvaluator` replays a failing script against
  `debugScripts: Map[ScriptHash, DebugScript]` when logs are empty, keyed by
  the RELEASE hash (`PlutusScriptEvaluator.scala:733-802`).
- `TxBuilder.withDebugScript(compiled)` plus AUTO-registration in every
  `CompiledPlutus`-typed overload (`spend`/`references`/`payTo`/`mint`,
  `TxBuilder.scala:1995-1999`).
- `CompiledPlutus.withErrorTraces` flips exactly
  `generateErrorTraces = true, removeTraces = false` (all three versions).
- sbt plugin `blueprint` / `blueprintPin` / `blueprintCheck` tasks
  (`ScalusSbtPlugin.scala:44-52`).

## Changes

### 1. Redefine `Options.debug` = release + traces

`scalus-core/shared/src/main/scala/scalus/compiler/compiler.scala:46-51`:

```scala
/** Same codegen as [[release]]; only error traces differ. What you debug is
  * what you ship: identical optimization, backend, and tag - so failure
  * points and term structure match the deployable, and only trace output is
  * added. (Budgets differ slightly: traces cost budget and inhibit
  * PartialEvaluator folding of the traced subterms.)
  */
val debug: Options = release.copy(generateErrorTraces = true, removeTraces = false)
```

Consequences (all intended):

- `optimizeUplc = true` (was false) - debug builds stop diverging from
  release in codegen.
- `addScalusTag = true` (was false) - **supersedes the scalus-tag design
  decision** (`docs/superpowers/specs/2026-04-11-scalus-tag-design.md:87`)
  that kept debug untagged. Rationale: the debug twin derived via
  `withErrorTraces` from a release build keeps the tag, and "only traces
  differ" is the whole point of the preset. Update `ScalusTagTest.scala:32-35`
  (asserts debug is untagged) and add a supersession note to the old spec.
- `CaptureNodeErrorsTest` (`Options.debug` for `log()` lines) keeps working:
  `removeTraces = false` preserves logs; the optimizer never strips traces.
- The 10 utxo-cell `Options.debug` compilation objects get optimized+traced
  scripts; they pin no literal hashes or budgets (verified), only
  `info(...)` sizes.
- Changelog entry; lands in 1.1.0.

Also: scaladoc on the `debug`, `debugLevel`, `warnListConversions`, `noWarn`
FIELDS of `Options` marking them "Scalus compiler development diagnostics" -
distinct from the `debug` preset. No rename (case-class/MiMa pain; next
major).

### 2. Default `Options` for `PlutusVX.compile`

`scalus-core/shared/src/main/scala/scalus/uplc/Compiled.scala:170,292,428`:

```scala
inline def compile[A](inline code: A)(using opts: Options = Options.release): PlutusVX[A]
```

- `PlutusV3.compile { ... }` with no given in scope compiles the deployable.
  Any user `given Options` still wins. Diagnostics come from the derived
  twin, not from switching the compile mode.
- NOT a companion `given` - that would be found by implicit search
  everywhere and hijack every `using options: Options = Options()` default
  in the API (897 bare `sir.toUplc()` test calls included).
- `compile` is `inline def`, so no bytecode change; source-compatible.

### 3. Debug-twin helpers on `CompiledPlutus`

`scalus-core/shared/src/main/scala/scalus/uplc/Compiled.scala` (base class):

```scala
/** The traced diagnostic twin of this script, for failure replay. Lazy:
  * the twin is only lowered if a failure actually consults it. */
lazy val debugScript: DebugScript = DebugScript.fromCompiled(this)

/** Registration entry for diagnostic replay, keyed by THIS (release)
  * script's hash: `evaluator.evalPlutusScripts(tx, utxos, Map(contract.debugScriptEntry))`. */
def debugScriptEntry: (ScriptHash, DebugScript) = script.scriptHash -> debugScript
```

`TxBuilder.registerDebugScript` switches to `compiled.debugScriptEntry`
(behavior identical; removes the private duplicate).

### 4. Emulator-level debug-script registration

Today only per-submit: `EmulatorBase.submit(tx, debugScripts)`; the 1-arg
`submit` and the testkit path (`EmulatorTestContext.submit` ->
`provider.submit(tx)`) pass nothing, and `ImmutableEmulator`'s `Context`
carries no debug scripts. Change: register once at the provider, apply on
every submit.

- `EmulatorBase`: add
  `def registerDebugScripts(entries: (ScriptHash, DebugScript)*): this.type`
  (mutable accumulation mirroring the emulator's existing style) and a
  `protected def registeredDebugScripts: Map[ScriptHash, DebugScript]`.
  `submit(tx)` and `submitSync(tx, perSubmit)` merge:
  `registeredDebugScripts ++ perSubmit` (per-submit wins on key clash).
- JVM + JS `Emulator` and `ImmutableEmulator` honor it; `EmulatorJavaApi`
  gets a `java.util.Map`-taking overload.
- Convenience: `registerDebugScripts(contract.debugScriptEntry)` -
  one line per contract in a test suite's provider setup.

### 5. Docs

- New page `scalus-site/content/dapp-development/dev-lifecycle.mdx` telling
  the model above: one artifact, release identity, derived twin, replay on
  failure, blueprint pinning (`blueprintPin` + `blueprintCheck` in the
  user's CI), budget tests against release.
- `working-with-contract.mdx:104-108`: update the presets table - debug is
  now "traces yes, optimized yes"; add a row note that `default` is
  unoptimized and intended for compiler-level inspection.
- `testing/unit-testing.mdx:161`: replace "compile your validator with
  error traces enabled" guidance with the twin-registration story
  (`assertScriptFail` works against release scripts once the twin is
  registered).
- `testing/debugging.mdx`: fix the dangling anchor at line 168
  (`#debugscript-api-for-external-builders` does not exist); document
  `debugScriptEntry` and emulator-level registration.

## Explicitly out of scope

- Migrating the ~50 example-test sites that hold `.withErrorTraces`-at-the-
  field contracts to the release+twin pattern. That is the eventual payoff
  (tests exercise the deployable; budgets match production) but is a large
  re-pin, staged separately after this infrastructure lands.
- Flipping `SIRDefaultOptions.optimizeUplc` (the `Options()` default) -
  still a separate decision with its own blast radius (897 bare `toUplc()`
  calls; shape-test audit).
- Renaming the `debug`/`debugLevel` Options fields (next major).

## Testing

- `ScalusTagTest`: update to the new expectation (debug tagged).
- New `OptionsPresetTest`: `Options.debug` equals
  `Options.release.copy(generateErrorTraces = true, removeTraces = false)`;
  a program compiled with debug differs from release only in trace content
  (evaluates to the same result; release logs empty, debug logs present on
  failure).
- New test: `compile` without a given uses `Options.release`
  (tagged + optimized + trace-free output).
- `debugScriptEntry`: key equals the release hash; twin script differs;
  `DiagnosticReplayTest` extended to go through the new helper.
- Emulator registration: a failing submit on a provider with a registered
  twin yields the trace message in the returned logs, with no per-submit
  map; per-submit map overrides the registered one.
- Budget pins: utxo-cell suites re-run (no literal pins - expect green);
  full `sbtn quick` + re-pin anything that moves; MiMa.
