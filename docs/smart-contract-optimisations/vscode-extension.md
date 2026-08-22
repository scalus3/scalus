Source: https://scalus.org/docs/smart-contract-optimisations/vscode-extension

# Scalus Profiler VS Code Extension

The [Scalus Profile](https://marketplace.visualstudio.com/items?itemName=Lantr.scalus-profile)
extension brings the [profiler](/docs/smart-contract-optimisations/profiling) into the editor:
every profiled line of your validator shows its CEK cost inline, hot spots are tinted, and the
compiled Untyped Plutus Core of your code opens side by side with bidirectional cursor sync.

![Per-line CEK cost annotations and heat backgrounds on a Scalus auction validator](/scalus-profile-annotations.png)

## Install

Install from the [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=Lantr.scalus-profile),
or from the command line:

```sh copy
code --install-extension Lantr.scalus-profile
```

## Quick Start

1. Run your tests with profiling enabled:

   ```sh copy
   SCALUS_PROFILE=full sbt test
   ```

2. Open your Scalus project folder in VS Code. The extension discovers the reports under
   `target/scalus` automatically (including git-ignored build directories) via
   `profile-manifest.json`.
3. Open a profiled `.scala` file – cost annotations appear on the costed lines.
4. Run **Scalus Profile: Open Report** from the Command Palette for the full interactive report.

## Features

- **Inline cost annotations** – each profiled line gets `⏱ <cpu> · <mem> · <count>× · <fee> lov`
  at the end of the line.
- **Heat background** – lines are tinted by their share of the selected metric (cpu or fee), so
  hot spots jump out.
- **Hover breakdown** – totals, the builtins invoked on that line, and the hottest control-flow
  edges leaving it.
- **Status bar** – total CEK budget and on-chain fee; click to open the report.
- **Open Report** – the self-contained interactive HTML report in a webview.
- **Compiled UPLC view** – the compiled Untyped Plutus Core beside your code, with the cursor
  synced in both directions (see below).
- **Auto-refresh** – re-reads the profile when your tests regenerate it.

When several scripts or redeemers were profiled, **Scalus Profile: Select Profile File** picks
among the manifest's runs, labelled by script hash, redeemer and Plutus version.

## Compiled UPLC View

Scalus compiles your Scala to Untyped Plutus Core, and that is what the CEK machine actually
runs. With the [UPLC source map](/docs/smart-contract-optimisations/profiling#uplc-source-map-uplcjson)
written next to the profile, the extension shows you that program and maps it back to your code:

- **Scalus Profile: Show Compiled UPLC** opens the compiled UPLC of the selected run in a
  read-only, syntax-highlighted editor beside the active one.
- Move the cursor in a `.scala` file – every UPLC node compiled from that position is
  highlighted, and the innermost one is revealed.
- Move the cursor in the UPLC editor – the Scala range that produced the node under the cursor
  is highlighted and revealed.
- **Scalus Profile: Show Compiled UPLC for Function** highlights every UPLC region compiled from
  the function at the cursor, merged into contiguous regions, scoped to the current file. It
  works from either editor.

The UPLC text is registered as its own `uplc` language with a syntax grammar; `.uplc` files get
the same highlighting.

![Compiled UPLC view: the profiled validator on the left, its compiled UPLC on the right, with the code compiled from the function under the cursor highlighted](/scalus-uplc-view.png)

  The Compiled UPLC view needs the `.uplc.json` source map, which recent Scalus versions write
  with full profile reports (the run's manifest entry lists a file of format `"uplc"`). With an
  older Scalus the cost annotations still work, and the command reports that no UPLC map is
  available. Console-only profiling writes no files, so no source map either.

## Settings

| Setting | Default | Description |
| --- | --- | --- |
| `scalusProfile.metric` | `cpu` | Primary metric: `cpu` or `fee` (fee falls back to cpu when unpriced) |
| `scalusProfile.showInline` | `true` | Inline per-line annotations |
| `scalusProfile.showHeat` | `true` | Line heat backgrounds |
| `scalusProfile.minPercent` | `0` | Only annotate lines at or above this % of the file's hottest line |
| `scalusProfile.heatColor` | `220,40,40` | Base RGB for the heat tint |
| `scalusProfile.manifestGlob` | `**/profile-manifest.json` | Where to find the profile manifest |
| `scalusProfile.jsonGlob` | `**/*profile*.json` | Fallback profile discovery (newest wins) |

## See Also

- **[Profiling](/docs/smart-contract-optimisations/profiling)** – Producing the reports and the file formats
- **[Measuring Performance](/docs/smart-contract-optimisations/measuring-performance)** – Budgets, fees and how to measure them
- **[Project Commands](/docs/get-started/project-commands)** – The `SCALUS_*` environment switches
