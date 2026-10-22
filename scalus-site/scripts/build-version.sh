#!/usr/bin/env bash
# Builds the Scaladoc and the llms files of one release tag:
#   $OUT/api/<version>/   Scaladoc with the version dropdown
#   $OUT/llms/<version>/  llms.txt, llms-api.txt, llms-examples.txt, llms-full.txt (as the tag has them)
# OUT defaults to scalus-site/public. Spec: docs/superpowers/specs/2026-10-22-versioned-api-docs-design.md
#
# Usage: scalus-site/scripts/build-version.sh v1.3.0
set -euo pipefail

TAG=${1:?usage: build-version.sh <tag>}
VERSION=${TAG#v}
SITE=https://scalus.org
REPO=$(git rev-parse --show-toplevel)
OUT=${OUT:-$REPO/scalus-site/public}
mkdir -p "$OUT"
OUT=$(cd "$OUT" && pwd)

warn() { echo "build-version: WARNING: $*" >&2; }

# Build from a worktree of the tag, never from the current checkout [VB-1]. Build output goes to
# $WORK, outside the worktree's tracked files [VB-2] [VB-4]. The worktree is removed on any exit [VB-14].
WORK=$(mktemp -d)
SRC=$WORK/src
cleanup() {
    git -C "$REPO" worktree remove --force "$SRC" 2>/dev/null || true
    rm -rf "$WORK"
}
trap cleanup EXIT
git -C "$REPO" worktree add --detach "$SRC" "$TAG"

# Scaladoc [VB-3]. -project-version is explicit: a tag's scalusStableVersion is its MiMa baseline,
# so v1.4.0 would label itself 1.3.0. -project replaces the sbt project name "docs" in the header.
# -dynamic-side-menu cuts one version from 827 MB to 66 MB.
SBT_CMDS=(
    "set docs / ScalaUnidoc / unidoc / scalacOptions ++= Seq(\"-dynamic-side-menu\", \"-versions-dictionary-url\", \"$SITE/api/versions.json\", \"-project\", \"Scalus\", \"-project-version\", \"$VERSION\")"
    "set docs / ScalaUnidoc / unidoc / target := file(\"$WORK/api\")"
    "docs/unidoc"
)
# llms-api.txt [VB-5]. The task exists since v1.1.0; it writes scalus-site/public/llms-api.txt,
# which git ignores.
if grep -q '^generateLlmsApi :=' "$SRC/build.sbt"; then
    SBT_CMDS+=("generateLlmsApi")
else
    warn "$TAG has no generateLlmsApi task, skipping llms-api.txt" # [VB-12]
fi
# A failed unidoc fails the script through set -e [VB-13].
(cd "$SRC" && sbt --batch -Dsbt.supershell=false "${SBT_CMDS[@]}")
[ -f "$WORK/api/index.html" ] || { echo "build-version: unidoc wrote no index.html" >&2; exit 1; }

LLMS=$WORK/llms
mkdir -p "$LLMS"
if [ -f "$SRC/scalus-site/public/llms-api.txt" ]; then
    cp "$SRC/scalus-site/public/llms-api.txt" "$LLMS/"
fi

# The tag's own generators [VB-6]: their example list matches the tag's examples. They read and
# write paths relative to scalus-site/ and need no pnpm install. Keep only the two files [VB-7];
# the per-page .md copies they also write are for the unversioned prose docs.
for gen in generate-llms.mjs:llms-full.txt generate-llms-examples.mjs:llms-examples.txt; do
    script=${gen%%:*}
    file=${gen##*:}
    if [ -f "$SRC/scalus-site/scripts/$script" ]; then
        (cd "$SRC/scalus-site" && node "scripts/$script")
        cp "$SRC/scalus-site/out/$file" "$LLMS/"
    else
        warn "$TAG has no scalus-site/scripts/$script, skipping $file" # [VB-12]
    fi
done

# The versioned index [VB-8], with links to this version's files [VB-9] and API [VB-10].
# Links to /docs/ pages stay: the prose docs are not versioned.
if [ -f "$SRC/scalus-site/public/llms.txt" ]; then
    perl -pe '
        s{(?<![\w/.-])((?:https://scalus\.org)?)/(llms(?:-api|-examples|-full)?\.txt)}{$1/llms/'"$VERSION"'/$2}g;
        s{https://scalus\.org/api(?:/index\.html)?(?=$|[\s)\]"'"'"'])}{https://scalus.org/api/'"$VERSION"'/}g;
        s{https://scalus\.org/api/(?![0-9])}{https://scalus.org/api/'"$VERSION"'/}g;
    ' "$SRC/scalus-site/public/llms.txt" > "$LLMS/llms.txt"
else
    warn "$TAG has no scalus-site/public/llms.txt, skipping llms.txt" # [VB-12]
fi

# A first line naming the version and the latest URL [VB-11]. The deploy strips it again when it
# copies the latest version's files to the site root.
for f in "$LLMS"/*.txt; do
    [ -e "$f" ] || continue
    name=$(basename "$f")
    { echo "# Scalus $VERSION. Latest version: $SITE/$name"; cat "$f"; } > "$f.tmp"
    mv "$f.tmp" "$f"
done

# A rebuilt version replaces its folders completely [VW-4].
rm -rf "$OUT/api/$VERSION" "$OUT/llms/$VERSION"
mkdir -p "$OUT/api" "$OUT/llms"
mv "$WORK/api" "$OUT/api/$VERSION"
mv "$LLMS" "$OUT/llms/$VERSION"
echo "build-version: $TAG -> $OUT/api/$VERSION ($(du -sh "$OUT/api/$VERSION" | cut -f1)), $OUT/llms/$VERSION ($(ls "$OUT/llms/$VERSION" | tr '\n' ' '))"
