#!/usr/bin/env bash
# Completes the site output after `pnpm build`, from the version folders that build-version.sh
# wrote (and the deploy restored from gh-pages):
#   api/            copy of the latest version's Scaladoc          [VW-5]
#   api/versions.json  the Scaladoc version dropdown              [VW-7]
#   llms-*.txt      the latest version's llms files               [VW-6] [VW-12]
#   llms.txt        the committed index plus a Versions section   [VW-8] [VW-13]
# Spec: docs/superpowers/specs/2026-10-22-versioned-api-docs-design.md
#
# Usage: scalus-site/scripts/finalize-versions.sh scalus-site/out
set -euo pipefail

DIR=${1:?usage: finalize-versions.sh <site output dir>}
SITE=https://scalus.org
MAX_MB=900

# Version folders, newest first [VD-7].
VERSIONS=$(find "$DIR/api" -mindepth 1 -maxdepth 1 -type d -name '[0-9]*' -exec basename {} \; | sort -rV)
[ -n "$VERSIONS" ] || { echo "finalize-versions: no version folder in $DIR/api" >&2; exit 1; }
LATEST=$(echo "$VERSIONS" | head -1)

# Root API = the latest version [VD-2] [VW-5]. Clear everything else first, so no stale page of an
# older layout survives [VW-2].
find "$DIR/api" -mindepth 1 -maxdepth 1 ! -name '[0-9]*' -exec rm -rf {} +
cp -R "$DIR/api/$LATEST/." "$DIR/api/"

# versions.json [VD-3] [VD-4] [VW-7].
{
    echo '{"versions": {'
    echo "$VERSIONS" | sed "s|.*|  \"&\": \"$SITE/api/&/\"|" | sed '$!s/$/,/'
    echo '}}'
} > "$DIR/api/versions.json"

# Root llms files from the latest version, without the version header line [VW-6] [VW-12].
for name in llms-api.txt llms-examples.txt llms-full.txt; do
    if [ -f "$DIR/llms/$LATEST/$name" ]; then
        tail -n +2 "$DIR/llms/$LATEST/$name" > "$DIR/$name"
    else
        echo "finalize-versions: WARNING: $LATEST has no $name, keeping the root file as built" >&2
    fi
done

# Versions section of the root llms.txt [VW-8].
LLMS_VERSIONS=$(find "$DIR/llms" -mindepth 2 -maxdepth 2 -name llms.txt 2>/dev/null \
    | sed "s|^$DIR/llms/||; s|/llms.txt$||" | sort -rV)
if [ -n "$LLMS_VERSIONS" ]; then
    {
        # The committed llms.txt has no final newline.
        [ -z "$(tail -c 1 "$DIR/llms.txt")" ] || echo
        echo
        echo "## Versions"
        echo
        echo "The files above describe the latest release, $LATEST."
        echo "If your project depends on an older Scalus, fetch the index of that version:"
        echo
        echo "$LLMS_VERSIONS" | sed 's|.*|- /llms/&/llms.txt|'
    } >> "$DIR/llms.txt"
fi

# GitHub Pages rejects sites above 1 GB [VW-9].
SIZE_MB=$(du -sm "$DIR" | cut -f1)
echo "finalize-versions: latest $LATEST, versions $(echo $VERSIONS), site $SIZE_MB MB"
if [ "$SIZE_MB" -gt "$MAX_MB" ]; then
    echo "finalize-versions: site is $SIZE_MB MB, above the $MAX_MB MB limit" >&2
    exit 1
fi
