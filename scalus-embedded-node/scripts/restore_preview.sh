#!/usr/bin/env bash
# Restore an already-extracted Mithril Cardano DB V2 snapshot into an in-memory
# `KvChainStore` — exercises the full ImmutableDbReader → HfcDiskBlockDecoder →
# AppliedBlock → ChainStore.appendBlock pipeline at preview scale.
#
# Memory: expect multi-GB RSS on preview (~2.37 M blocks). Prefer a machine
# with >= 16 GB free. Override SCALUS_IMMUTABLEDB_SRC to a smaller fixture if
# needed.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

if [ -z "${SCALUS_IMMUTABLEDB_SRC:-}" ]; then
    export SCALUS_IMMUTABLEDB_SRC=/tmp/mithrill-preview
    echo "SCALUS_IMMUTABLEDB_SRC unset, defaulting to $SCALUS_IMMUTABLEDB_SRC"
fi

cd "$REPO_ROOT"
exec sbt 'scalusChainStoreMithril/testOnly scalus.cardano.node.stream.engine.snapshot.immutabledb.ImmutableDbRestoreProbe'
