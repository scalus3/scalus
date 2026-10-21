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

# The clone's pre-commit hook rejects any .uplc file that is not in its
# canonical pretty-uplc layout, and the generator does not emit that layout.
# Formatting changes only whitespace, never the program or its measurements.
UPLC_FILES=()
for d in "${DIRS[@]}"; do UPLC_FILES+=("$d"/*.uplc); done
echo "==> Formatting ${#UPLC_FILES[@]} .uplc files with treefmt"
nix develop --command treefmt "${UPLC_FILES[@]}"

for d in "${DIRS[@]}"; do
  echo "==> verify $d"
  nix develop --command ./scripts/cape.sh submission verify "$d"
  echo "==> measure $d"
  nix develop --command ./scripts/cape.sh submission measure "$d"
done

cd - >/dev/null
echo "==> Leaderboard"
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.CompareWithLeaderboard $CAPE_DIR --ours $SUB_NAME"

echo "Done. Review $CAPE_DIR, then commit and open the PR manually."
