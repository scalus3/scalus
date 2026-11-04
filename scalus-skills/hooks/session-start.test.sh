#!/usr/bin/env bash
# Tests for the session-start hook: which projects get the using-scalus routing table.
# Run: bash scalus-skills/hooks/session-start.test.sh
# Spec: docs/superpowers/specs/2026-11-04-llm-docs-typescript-java-design.md [TS-33]
set -euo pipefail

HOOK="$(cd "$(dirname "$0")" && pwd)/session-start"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
FAILED=0

# check <name> <expected injections: 0 or 1> ; the project is in $TMP/<name>
check() {
    local name=$1 expected=$2 out count
    out=$(CLAUDE_PROJECT_DIR="$TMP/$name" bash "$HOOK")
    count=$({ printf '%s' "$out" | grep -o "Below is its 'scalus:using-scalus' skill" || true; } | wc -l | tr -d ' ')
    if [ "$count" != "$expected" ]; then
        echo "FAIL $name: expected $expected injection(s), got $count"
        FAILED=1
        return
    fi
    if [ "$expected" = 1 ] && ! printf '%s' "$out" | grep -q 'scalus:typescript'; then
        echo "FAIL $name: the routing table has no scalus:typescript row"
        FAILED=1
        return
    fi
    echo "ok   $name"
}

project() {
    mkdir -p "$TMP/$1"
}

project sbt
echo 'libraryDependencies += "org.scalus" %% "scalus" % "1.5.0"' > "$TMP/sbt/build.sbt"
check sbt 1

project npm-dev
printf '{\n  "name": "app",\n  "devDependencies": {\n    "scalus": "^1.5.0"\n  }\n}\n' > "$TMP/npm-dev/package.json"
check npm-dev 1

project npm-other
printf '{\n  "name": "app",\n  "dependencies": {\n    "scalus-helper": "1.0.0"\n  }\n}\n' > "$TMP/npm-other/package.json"
check npm-other 0

project npm-none
printf '{\n  "name": "app",\n  "dependencies": {\n    "lucid": "1.0.0"\n  }\n}\n' > "$TMP/npm-none/package.json"
check npm-none 0

project both
cp "$TMP/sbt/build.sbt" "$TMP/npm-dev/package.json" "$TMP/both/"
check both 1

project empty
check empty 0

exit $FAILED
