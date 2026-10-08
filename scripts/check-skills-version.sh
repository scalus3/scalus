#!/usr/bin/env bash
# Fails when scalus-skills/ changed after the last plugin version bump.
#
# Installed Claude Code plugins update only when `version` in
# scalus-skills/.claude-plugin/plugin.json changes. A skill edit without a bump
# never reaches users. Needs full history (actions/checkout fetch-depth: 0).

set -euo pipefail

MANIFEST="scalus-skills/.claude-plugin/plugin.json"
REF="${1:-HEAD}"

bump=$(git log -1 --format=%H -G'"version"' "$REF" -- "$MANIFEST")
if [ -z "$bump" ]; then
    echo "No version bump found in $MANIFEST history; nothing to check."
    exit 0
fi

changed=$(git diff --name-only "$bump" "$REF" -- scalus-skills \
    ":(exclude)$MANIFEST" ":(exclude)scalus-skills/README.md")

if [ -n "$changed" ]; then
    version=$(git show "$REF:$MANIFEST" | grep -o '"version": *"[^"]*"')
    echo "scalus-skills changed after the last plugin version bump ($bump, $version):"
    echo "$changed" | sed 's/^/  /'
    echo "Bump \"version\" in $MANIFEST so installed plugins pick up the change."
    echo "See \"Agent artifacts\" in CONTRIBUTING.md."
    exit 1
fi

echo "scalus-skills is unchanged since the last plugin version bump ($bump)."
