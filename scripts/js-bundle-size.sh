#!/usr/bin/env bash
#
# Tools for working on the size of the published scalus.js npm bundle.
# See docs/internal/JS_BUNDLE_SIZE.md for what to do with the output.
#
#   scripts/js-bundle-size.sh measure [label]
#       Links and bundles the npm package, then prints the linker, minified and gzip sizes.
#       This is the ground truth: source-map attribution over-counts, deltas do not.
#
#   scripts/js-bundle-size.sh packages [--depth N] [--top N]
#       Size per package and per library, attributed through the source map that fullLinkJS
#       already wrote. Closest to shipped bytes, but it OVER-COUNTS: output with no mapping is
#       credited to whichever source precedes it. Use it to rank, never to claim a number.
#       Needs a previous `measure` (or any fullLinkJS) run.
#
#   scripts/js-bundle-size.sh modules [--depth N] [--top N]
#       Size per package, summed over the per-class ES modules of a `graph` link. Every number
#       is a real file size, so the split between packages is trustworthy, but the totals are
#       fastLink, roughly 2-3x the optimised output. Needs a previous `graph` run.
#
#   scripts/js-bundle-size.sh attribute
#       The raw source-map-explorer JSON behind `packages`, for ad-hoc queries.
#
#   scripts/js-bundle-size.sh graph
#       Relinks with one ES module per class so that reachability can be inspected: parse each
#       emitted module's `from "./X.js"` imports and BFS from main.js to find out why something
#       is in the bundle. Diagnostic only, never ship this split style.
#
set -o pipefail
cd "$(dirname "$0")/.." || exit 1

OPT_DIR=scalus-cardano-ledger/js/target/scala-3.3.8/scalus-cardano-ledger-opt
FASTOPT_DIR=scalus-cardano-ledger/js/target/scala-3.3.8/scalus-cardano-ledger-fastopt
BUNDLE=scalus-cardano-ledger/js/src/main/npm/scalus.js
REPORT=scripts/js-bundle-size-report.mjs
mkdir -p logs

require_map() {
    if [ ! -f "$OPT_DIR/main.js.map" ]; then
        echo "No $OPT_DIR/main.js.map. Run '$0 measure' first." >&2
        exit 1
    fi
}

case "${1:-measure}" in
measure)
    LABEL="${2:-run}"
    LOG="logs/bundle-$LABEL.log"
    nix develop .#ci --command sbt \
        -Dsbt.supershell=false -Dsbt.log.noformat=true \
        scalusCardanoLedgerJS/prepareNpmPackage >"$LOG" 2>&1
    RC=$?
    echo "=== $LABEL (sbt rc=$RC, log: $LOG) ==="
    [ -f "$OPT_DIR/main.js" ] && echo "linker   : $(wc -c <"$OPT_DIR/main.js")"
    [ -f "$BUNDLE" ] && echo "minified : $(wc -c <"$BUNDLE")"
    [ -f "$BUNDLE" ] && echo "gzip     : $(gzip -c "$BUNDLE" | wc -c)"
    exit $RC
    ;;
attribute)
    require_map
    npx --yes source-map-explorer "$OPT_DIR/main.js" "$OPT_DIR/main.js.map" \
        --json --no-border-checks
    ;;
packages)
    require_map
    shift
    TMP=$(mktemp -t js-bundle-size.XXXXXX)
    trap 'rm -f "$TMP"' EXIT
    npx --yes source-map-explorer "$OPT_DIR/main.js" "$OPT_DIR/main.js.map" \
        --json --no-border-checks >"$TMP" || exit 1
    node "$REPORT" --sources "$TMP" "$@"
    ;;
modules)
    shift
    node "$REPORT" --modules "$FASTOPT_DIR" "$@"
    ;;
graph)
    nix develop .#ci --command sbt -Dsbt.supershell=false -Dsbt.log.noformat=true \
        'set LocalProject("scalusCardanoLedgerJS") / scalaJSLinkerConfig ~= (_.withModuleSplitStyle(org.scalajs.linker.interface.ModuleSplitStyle.SmallModulesFor(List("scalus","upickle","ujson","upack","io","scribe","org","com","cats"))))' \
        scalusCardanoLedgerJS/fastLinkJS >logs/graph.log 2>&1
    RC=$?
    # Record what was linked. File mtimes cannot answer this: the linker leaves modules it did
    # not have to rewrite untouched, so they keep timestamps from an older link.
    if [ $RC -eq 0 ]; then
        {
            date '+%Y-%m-%d %H:%M'
            git rev-parse --short HEAD 2>/dev/null || echo "unknown"
            if [ -n "$(git status --porcelain 2>/dev/null)" ]; then echo dirty; else echo clean; fi
        } | paste -sd' ' - >"$FASTOPT_DIR/.graph-stamp"
    fi
    echo "modules written to $FASTOPT_DIR: $(ls "$FASTOPT_DIR" 2>/dev/null | grep -c '\.js$')"
    exit $RC
    ;;
*)
    sed -n '2,29p' "$0"
    exit 2
    ;;
esac
