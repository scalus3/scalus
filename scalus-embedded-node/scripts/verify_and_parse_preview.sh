#!/usr/bin/env bash
# Verify + parse an already-downloaded Mithril Cardano Database V2 snapshot. Pairs with
# download_preview.sh — point it at the same destination.
#
# Usage:
#   SCALUS_IMMUTABLEDB_SRC=/tmp/mithrill-preview ./scripts/verify_and_parse_preview.sh
#
# Runs two passes: SHA-256 verification against digests.json, then a full block walk that
# tallies counts / bytes / slot range and reports throughput.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

if [ -z "${SCALUS_IMMUTABLEDB_SRC:-}" ]; then
    export SCALUS_IMMUTABLEDB_SRC=/tmp/mithrill-preview
    echo "SCALUS_IMMUTABLEDB_SRC unset, defaulting to $SCALUS_IMMUTABLEDB_SRC"
fi

cd "$REPO_ROOT"
exec sbt 'scalusChainStoreMithril/testOnly scalus.cardano.node.stream.engine.snapshot.immutabledb.ImmutableDbReadProbe'
