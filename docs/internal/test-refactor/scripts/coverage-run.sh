#!/usr/bin/env bash
# Runs all JVM tests under coverage and writes the uncovered-statement set to <out-dir>
# (spec [P0-8], [CG-1]). See docs/internal/test-refactor/coverage.md.
#
#   coverage-run.sh <out-dir> [<sbt-test-task> ...]     default task: jvm/test
#
# Always compiles from clean: the plugin and the macros record coverage while scalac compiles the
# test sources, so an incremental build would drop that coverage (finding F-12).
#
# Uses a one-shot sbt: never run it while an sbtn server is live in the same checkout.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
out=$1
shift
tasks=("${@:-jvm/test}")

docs/internal/test-refactor/scripts/coverage-exclude.sh
# Measurements accumulate across runs; without this, the report is the union of runs.
find . -path ./.claude -prune -o -name 'scoverage.measurements.*' -print0 | xargs -0 rm -f
rm -rf -- */target/scala-*/scoverage-report */*/target/scala-*/scoverage-report

sbt -Dsbt.supershell=false -Dsbt.log.noformat=true -Dsbt.server.forcestart=false \
    clean coverage "${tasks[@]}" coverageReport > "/tmp/coverage-run.log" 2>&1 && status=0 || status=$?
grep -E 'Tests: succeeded|\*\*\* FAILED' /tmp/coverage-run.log || true
python3 docs/internal/test-refactor/scripts/coverage.py baseline . "$out"
echo "sbt exit $status; wrote $out"
exit $status
