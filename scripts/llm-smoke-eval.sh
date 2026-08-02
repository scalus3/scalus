#!/usr/bin/env bash
# LLM smoke eval: scaffold a fresh project from the validator.g8 template,
# drive a headless Claude Code agent through canonical tasks, and require the
# result to compile and pass tests after every task.
#
# Usage:
#   TEMPLATE_DIR=/path/to/validator.g8 [EVAL_TASKS=N] scripts/llm-smoke-eval.sh
#
# Requires: sbt, scala-cli, claude (Claude Code CLI), ANTHROPIC_API_KEY (in CI).
# Exit code 0 = all tasks passed.
set -uo pipefail

TEMPLATE_DIR="${TEMPLATE_DIR:-$(cd "$(dirname "$0")/../../validator.g8" 2>/dev/null && pwd)}"
if [[ -z "$TEMPLATE_DIR" || ! -f "$TEMPLATE_DIR/default.properties" ]]; then
    echo "llm-smoke-eval: validator.g8 template not found (set TEMPLATE_DIR)" >&2
    exit 2
fi

# Fixed canonical tasks - do not edit casually; runs stop being comparable.
TASKS=(
    "Read AGENTS.md. Rename nothing. Add a deadline: PosixTime field to the datum and reject spends after it. Update the tests so all pass."
    "Read AGENTS.md. Write a new minting policy GiftMint in a new file that only allows minting when the transaction is signed by a fixed owner key. Add an Emulator test for the success and failure case."
    "Read AGENTS.md. Add a budget assertion test that pins the validator's spend budget with assertBudgetWithin."
)
N="${EVAL_TASKS:-${#TASKS[@]}}"

WORKDIR=$(mktemp -d "${TMPDIR:-/tmp}/llm-smoke-eval.XXXXXX")
trap 'rm -rf "$WORKDIR"' EXIT
echo "llm-smoke-eval: workdir $WORKDIR, template $TEMPLATE_DIR, tasks 1..$N"

cd "$WORKDIR"
sbt --allow-empty --batch -Dsbt.supershell=false \
    "new file://$TEMPLATE_DIR --name=smoke-eval" || {
    echo "llm-smoke-eval: template render failed" >&2
    exit 2
}
cd smoke-eval

echo "llm-smoke-eval: baseline check"
scala-cli compile . >/dev/null && scala-cli test . >/dev/null || {
    echo "llm-smoke-eval: FAIL baseline (template broken before any agent ran)" >&2
    exit 2
}

declare -a RESULTS
failures=0
for ((i = 0; i < N; i++)); do
    task="${TASKS[$i]}"
    echo
    echo "=== Task $((i + 1)): $task"
    claude -p "$task" --dangerously-skip-permissions
    agent_exit=$?
    if [[ $agent_exit -ne 0 ]]; then
        RESULTS[$i]="FAIL (agent exit $agent_exit)"
        failures=$((failures + 1))
        continue
    fi
    if scala-cli compile . >/dev/null 2>&1 && scala-cli test .; then
        RESULTS[$i]="PASS"
    else
        RESULTS[$i]="FAIL (compile/test)"
        failures=$((failures + 1))
    fi
done

echo
echo "=== llm-smoke-eval summary"
for ((i = 0; i < N; i++)); do
    echo "  Task $((i + 1)): ${RESULTS[$i]}"
done
[[ $failures -eq 0 ]] || {
    echo "llm-smoke-eval: $failures task(s) failed" >&2
    exit 1
}
echo "llm-smoke-eval: all $N task(s) passed"
