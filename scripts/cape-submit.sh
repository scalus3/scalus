#!/usr/bin/env bash
# Generate, verify, and measure Scalus UPLC-CAPE submissions, then rank them.
# Usage: scripts/cape-submit.sh <path-to-UPLC-CAPE-clone> [version]
set -euo pipefail

CAPE_DIR=${1:?usage: cape-submit.sh <uplc-cape-dir> [version]}
VERSION=${2:-}

# Resolve to an absolute path before changing directories, so a relative
# CAPE_DIR (the common case, e.g. "../UPLC-CAPE") still works no matter where
# this script is invoked from. Also fails fast if the clone doesn't exist.
CAPE_DIR=$(cd "$CAPE_DIR" && pwd)

cd "$(dirname "$0")/.."

GEN_LOG=$(mktemp)
trap 'rm -f "$GEN_LOG"' EXIT

echo "==> Generating submissions"
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.GenerateSubmissions $CAPE_DIR $VERSION" | tee "$GEN_LOG"

# Derive the exact "Scalus_<version>_nau" submission id the generator just
# used for this run (VERSION defaults to BuildInfo.version inside
# GenerateSubmissions when not given, so this is the only reliable way to
# learn it from the shell). Scoping the glob below to this id -- instead of a
# bare `submissions/*/Scalus_*_nau` -- keeps leftover dirs from older runs
# (e.g. `Scalus_0.17.0_Unisay`, `Scalus_1.1.0+2-415f64c1-SNAPSHOT_nau`) from
# being counted as part of this run's 8 submissions.
SUB_NAME=$(sed -E 's/\x1b\[[0-9;]*[a-zA-Z]//g' "$GEN_LOG" | grep -oE 'Scalus_[^[:space:]]+_nau$' | sort -u)
SUB_NAME_COUNT=$(printf '%s\n' "$SUB_NAME" | grep -c . || true)
if [[ "$SUB_NAME_COUNT" -ne 1 ]]; then
  echo "expected exactly one Scalus_<version>_nau submission id in generator output, got $SUB_NAME_COUNT: $SUB_NAME" >&2
  exit 1
fi

cd "$CAPE_DIR"
shopt -s nullglob
DIRS=(submissions/*/"$SUB_NAME")
[[ ${#DIRS[@]} -eq 8 ]] || { echo "expected 8 submission dirs matching $SUB_NAME, found ${#DIRS[@]}: ${DIRS[*]}"; exit 1; }

# Split into "current" (evaluates against CAPE's production plutus-core,
# 1.45.0.0) and "preview" (metadata.json sets compilation_config.min_plutus_
# version, e.g. for PV11/vanRossem scenarios -- needs the newer plutus-core
# pinned by cabal.project.preview) submissions. `min_plutus_version` is only
# ever emitted by our own generator (CapeMetadata.render) when a scenario
# actually needs it (CapeScenarios.scala), so a plain grep for the key is a
# reliable, dependency-free way to split them here (no jq needed on the host;
# jq only exists inside `nix develop`).
CURRENT_DIRS=()
PREVIEW_DIRS=()
for d in "${DIRS[@]}"; do
  if grep -q '"min_plutus_version"' "$d/metadata.json"; then
    PREVIEW_DIRS+=("$d")
  else
    CURRENT_DIRS+=("$d")
  fi
done

for d in "${CURRENT_DIRS[@]}"; do
  echo "==> verify $d"
  nix develop --command ./scripts/cape.sh submission verify "$d"
  echo "==> measure $d"
  nix develop --command ./scripts/cape.sh submission measure "$d"
done

# `cape submission verify`/`measure` refuse a bare PATH for a preview
# submission outright ("Submission requires plutus-core >= ...; Use 'cape
# submission measure --preview'"), and `measure --preview` itself has no
# path-scoping: it re-measures EVERY preview submission under submissions/,
# which would rewrite metrics.json for every other contributor's already-
# committed `_preview` submission too. So preview-gated dirs get a narrowly
# scoped equivalent here instead: call the measure-preview binary directly
# (same tool `cape submission measure --preview` uses under the hood, per
# scripts/lib/cape_common.sh's cape_measure_preview_binary +
# scripts/cape-subcommands/submission/measure.sh's measure_preview_
# submissions), then schema-validate exactly like `cape submission verify`
# does for the production track (verify.sh doesn't do this for preview
# submissions at all -- CI's own pr-ci.yml never schema-checks the preview
# track either, so this is stricter than upstream, deliberately).
if [[ ${#PREVIEW_DIRS[@]} -gt 0 ]]; then
  echo "==> Building measure-preview (cabal.project.preview: plutus-core ^>=1.65, +preview flag)"
  nix develop --command cabal --project-file=cabal.project.preview build measure-preview
  PREVIEW_BIN=$(nix develop --command cabal --project-file=cabal.project.preview list-bin measure-preview | tail -n1)

  for d in "${PREVIEW_DIRS[@]}"; do
    scenario=${d#submissions/}
    scenario=${scenario%%/*}
    uplc_file=$(compgen -G "$d"/*.uplc | head -n1)
    tests_file="scenarios/$scenario/cape-tests.json"
    tmp_metrics=$(mktemp)

    echo "==> measure --preview $d"
    nix develop --command "$PREVIEW_BIN" -i "$uplc_file" -t "$tests_file" -o "$tmp_metrics"
    # `nix develop --command CMD > file` also redirects the dev shell's own
    # shellHook banner ("Synchronizing Cabal package index...", the welcome
    # message) into `file`, ahead of CMD's real output -- nix prints that
    # banner to the shell's stdout during activation, before CMD ever runs,
    # and this whole line's `>` redirect captures the entire subprocess's
    # stdout, banner included. That silently corrupts metrics.json with
    # leading non-JSON text (caught downstream by check-jsonschema's "failed
    # to parse"). Filter to just the JSON object: jq pretty-prints with a
    # standalone `{` as the first character of its first line, so drop
    # everything before that line.
    nix develop --command jq --arg s "$scenario" '.scenario = $s' "$tmp_metrics" \
      | sed -n '/^{/,$p' > "$d/metrics.json"
    rm -f "$tmp_metrics"

    echo "==> schema-verify $d"
    nix develop --command check-jsonschema --schemafile submissions/TEMPLATE/metrics.schema.json "$d/metrics.json"
    nix develop --command check-jsonschema --schemafile submissions/TEMPLATE/metadata.schema.json "$d/metadata.json"
  done
fi

cd - >/dev/null
echo "==> Leaderboard"
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.CompareWithLeaderboard $CAPE_DIR"

echo "Done. Review $CAPE_DIR, then commit and open the PR manually."
