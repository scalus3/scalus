#!/usr/bin/env bash
# Runs the JIT bench (spec 2026-10-19-uplc-jit-simplification-design.md, section 4.3): each run in
# its own JVM, one after the other, then the report.
#
# Usage, from the repository root:
#   scalus-uplc-jit-compiler/scripts/jit-bench.sh [--quick] [--report-only] [jit|cek[:label]...]
#
# With no arguments it runs the CEK machine once and the JIT three times (labels jit-1 to 3).
# `jit:label` names the result files; --quick runs the given ones in quick mode ([MS-13]). The
# reference branch is measured with a copy of the bench in its worktree, as label `reference`.
#
# Results go to target/jit-bench (OUT to change): <label>.csv, <label>.summary, <label>.log and
# report.md.
set -euo pipefail

SBT=${SBT:-sbt}  # one-shot sbt by default: the module is often built in a git worktree
OUT=${OUT:-target/jit-bench}
QUICK_FLAGS="-XX:Tier3InvocationThreshold=100 -XX:Tier4InvocationThreshold=1000 -XX:Tier4CompileThreshold=2000"
# Each script warms up and is timed alone (-Djit.bench.isolated=true): round-robin over the bench
# scripts does not settle (spec, implementation status). For the round-robin of [MS-9]:
# JVM_OPTS=-Djit.bench.minWarmupS=240
JVM_OPTS=${JVM_OPTS:--Djit.bench.isolated=true}

quick=""
report_only=""
variants=()
for a in "$@"; do
  case "$a" in
    --quick) quick=1 ;;
    --report-only) report_only=1 ;;
    *) variants+=("$a") ;;
  esac
done
mkdir -p "$OUT"

$SBT "scalusUplcJitCompiler/Test/compile" >/dev/null
CP=$($SBT "export scalusUplcJitCompiler/Test/fullClasspath" | grep -v '^\[' | grep '\.jar\|classes' | tail -1)

run() { # run <variant> <label> <extra jvm flags>
  local variant=$1 label=$2 extra=$3
  echo "== $label ($variant) $(date +%T)"
  # shellcheck disable=SC2086
  java -Xss64m -Xmx6g -XX:CICompilerCount=12 $JVM_OPTS $extra \
    -Dscalus.jit.outDir="$OUT/classes" -Djit.bench.out="$OUT" \
    -cp "$CP" scalus.uplc.jit.JitBench "$variant" --label "$label" >"$OUT/$label.log" 2>&1 ||
    echo "   FAILED, see $OUT/$label.log"
  grep -h '^\[bench\] \(geomean\|compiled \([0-9]\)\|warmup_s\|variant .* BROKEN\)' "$OUT/$label.log" || true
}

if [ -z "$report_only" ]; then
  if [ ${#variants[@]} -eq 0 ]; then
    variants=(cek jit:jit-1 jit:jit-2 jit:jit-3)
    for v in "${variants[@]}"; do run "${v%%:*}" "${v##*:}" ""; done
  else
    for v in "${variants[@]}"; do
      label=${v##*:}
      [ -n "$quick" ] && label="$label-quick"
      run "${v%%:*}" "$label" "${quick:+$QUICK_FLAGS}"
    done
  fi
fi

java -cp "$CP" scalus.uplc.jit.JitBenchReport "$OUT"
