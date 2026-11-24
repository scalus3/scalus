#!/usr/bin/env bash
# Counts the compiler warnings in an sbt log. Writes the count to the job summary and to the step
# output `warnings` ([ci.alias.warnings-push], [ci.nightly.warnings] in
# docs/superpowers/specs/2026-11-21-ci-speed-design.md).
#
# Usage: scripts/ci/count-warnings.sh <sbt log> <job label>
#
# A push job compiles only what the push changed, so its count covers only those files. The
# nightly full run compiles everything, so its count is the count of the whole build.
set -euo pipefail

log=$1
label=$2
summary=${GITHUB_STEP_SUMMARY:-/dev/stdout}

if [[ ! -f "$log" ]]; then
    echo "### Compiler warnings, $label: no log" >> "$summary"
    exit 0
fi

# Scala 3 starts each warning with a header, for example
#   [warn] -- Warning: /src/Foo.scala:3:7
#   [warn] -- [E198] Unused Symbol Warning: /src/Foo.scala:3:7
#   [warn] -- Deprecation Warning: /src/Foo.scala:3:7
# The sed removes ANSI colours: sbt 2.1 prints them in GitHub Actions logs too, inside the
# header ("[\e[33mwarn\e[0m] \e[33m-- Warning: ..."), and they break the pattern.
pattern='-- (\[E[0-9]+\] )?[A-Za-z ]*Warning: '
warnings=$(sed 's/\x1b\[[0-9;]*m//g' "$log" | grep -E -- "$pattern" || true)
count=$(printf '%s' "$warnings" | grep -c . || true)

echo "warnings=$count" >> "${GITHUB_OUTPUT:-/dev/null}"
{
    echo "### Compiler warnings, $label: $count"
    if [[ $count -gt 0 ]]; then
        echo
        echo '```'
        # Not `printf | head`: under pipefail a long list ends the script with SIGPIPE.
        head -n 50 <<< "$warnings"
        echo '```'
    fi
} >> "$summary"
