# LLM Developer Experience (Plan 3: smoke eval) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A scripted eval that proves an LLM agent with only the shipped context (AGENTS.md, skills, scalus.org artifacts) produces Scalus code that compiles and passes tests.

**Architecture:** One bash script scaffolds a fresh project from `validator.g8`, runs `claude -p` for each canonical task, and after each task requires `scala-cli compile .` and `scala-cli test .` to pass.

**Amendment 2026-08-02:** no `ANTHROPIC_API_KEY` will be available, so Task 2 (GitHub workflow) is dropped. The script is a local release-checklist tool; the local Claude Code login authenticates it.

**Tech Stack:** bash, giter8 via `sbt new`, Claude Code CLI (headless `-p`), scala-cli.

**Spec:** `docs/superpowers/specs/2026-08-02-llm-developer-experience-design.md`, layer "Smoke eval".

## Global Constraints

- Script lives in the scalus repo: `scripts/llm-smoke-eval.sh`; workflow: `.github/workflows/llm-smoke-eval.yml`.
- Eval failure must not block release publishing; the workflow is `workflow_dispatch` only and is a release-checklist item.
- The agent runs headless with `--dangerously-skip-permissions` inside a throwaway directory; the script must never run it inside the scalus repo itself.
- Canonical tasks are fixed strings in the script - do not generate them dynamically, or runs stop being comparable.

---

### Task 1: `scripts/llm-smoke-eval.sh`

- [ ] **Step 1: Write the script** - scaffold from `TEMPLATE_DIR` (default `../validator.g8`, CI passes a checkout path), then for each canonical task run `claude -p "$task" --dangerously-skip-permissions`, then `scala-cli compile .` and `scala-cli test .`; any failure exits non-zero with a per-task PASS/FAIL summary. Canonical tasks:
  1. "Read AGENTS.md. Rename nothing. Add a `deadline: PosixTime` field to the datum and reject spends after it. Update the tests so all pass."
  2. "Read AGENTS.md. Write a new minting policy `GiftMint` in a new file that only allows minting when the transaction is signed by a fixed owner key. Add an Emulator test for the success and failure case."
  3. "Read AGENTS.md. Add a budget assertion test that pins the validator's spend budget with assertBudgetWithin."
- [ ] **Step 2: Local dry run** with `EVAL_TASKS=1` (first task only) to validate plumbing end to end.
- [ ] **Step 3: Commit.**

### Task 2: `.github/workflows/llm-smoke-eval.yml`

- [ ] **Step 1: Write workflow** - `workflow_dispatch`; checkout scalus + `scalus3/validator.g8` (side path); setup JDK 17, sbt, scala-cli, `npm install -g @anthropic-ai/claude-code`; run the script with `ANTHROPIC_API_KEY` secret and `TEMPLATE_DIR` pointing at the template checkout.
- [ ] **Step 2: Commit.** Trigger manually from the Actions tab after the templates' next release.
