#!/usr/bin/env bash
# Profiles the tests of one sbt project with Java Flight Recorder (spec [PR-4] to [PR-8]).
#
#   profile.sh <sbt-project> <name> [<suite-glob>]
#
# Examples:
#   profile.sh scalusJVM core
#   profile.sh scalusExamplesJVM linkedlist scalus.examples.linkedlist.LinkedListTest
#
# The script forces Test / fork := true and adds the recording option to Test / javaOptions
# through `set`, so build.sbt stays unchanged ([PR-5]). Projects that do not fork by default
# (scalusCardanoLedgerJVM) then run in a fresh JVM: same tests, slightly different start-up.
# Set ROOT_CWD=1 for a project whose tests read files relative to the repo root and do not fork by
# default (scalusCardanoLedgerJVM): the forked JVM then starts in the repo root, as in sbt.
# Writes docs/internal/test-refactor/profiles/<name>.txt. The .jfr file stays in /tmp ([PR-8]).
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

project=$1
name=$2
suite=${3:-}
jfr_file=/tmp/trp-profiles/$name.jfr
out=docs/internal/test-refactor/profiles/$name.txt
mkdir -p /tmp/trp-profiles docs/internal/test-refactor/profiles
rm -f "$jfr_file"

if [[ -n $suite ]]; then task="$project/testOnly $suite"; else task="$project/test"; fi

start=$(date +%s)
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true -Dsbt.server.forcestart=false \
    "set LocalProject(\"$project\") / Test / fork := true" \
    ${ROOT_CWD:+"set LocalProject(\"$project\") / Test / baseDirectory := (LocalRootProject / baseDirectory).value"} \
    "set LocalProject(\"$project\") / Test / javaOptions += \"-XX:FlightRecorderOptions=stackdepth=1024\"" \
    "set LocalProject(\"$project\") / Test / javaOptions += \"-XX:StartFlightRecording=filename=$jfr_file,settings=profile,dumponexit=true\"" \
    "$task" > "/tmp/trp-profiles/$name.sbt.log" 2>&1 && status=0 || status=$?
wall=$(( $(date +%s) - start ))

{
    echo "# Profile: $name"
    echo
    echo "project: $project; task: $task; sbt wall time (incl. startup): ${wall}s; exit: $status"
    echo "commit: $(git rev-parse --short HEAD); date: $(date -u +%Y-%m-%dT%H:%MZ)"
    grep -E 'Run completed|Tests: succeeded' "/tmp/trp-profiles/$name.sbt.log" || true
    for view in hot-methods allocation-by-class allocation-by-site gc-pauses gc-configuration cpu-load-samples; do
        echo
        echo "## jfr view $view"
        echo
        jfr view --width 200 "$view" "$jfr_file" 2>&1 | head -60 || true
    done
} > "$out"
echo "wrote $out (wall ${wall}s, sbt exit $status)"
