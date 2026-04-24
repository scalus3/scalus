#!/usr/bin/env bash
# Download the full latest testing-preview Cardano Database V2 snapshot
# through MithrilClient and print size/count/time stats.
#
# This is the manual probe behind MithrilFullPreviewProbe — gated on
# SCALUS_MITHRIL_FULL_PREVIEW=1 so `sbt test` / CI never invoke it.
#
# Usage:
#   ./scripts/download_preview.sh                   # temp dir, auto-cleanup up to you
#   SCALUS_MITHRIL_DEST=/data/preview \
#     ./scripts/download_preview.sh                 # custom destination
#
# Rerunning against the same destination is idempotent: the per-archive
# `.extracted` marker files make already-extracted archives skip.
set -euo pipefail

# Resolve repo root (this script lives in scalus-embedded-node/scripts/).
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

export SCALUS_MITHRIL_FULL_PREVIEW=1
export SCALUS_MITHRIL_DEST="${SCALUS_MITHRIL_DEST:-/tmp/mithrill-preview}"

cd "$REPO_ROOT"
exec sbt 'scalusChainStoreMithril/testOnly scalus.cardano.node.stream.engine.snapshot.mithril.MithrilFullPreviewProbe'
