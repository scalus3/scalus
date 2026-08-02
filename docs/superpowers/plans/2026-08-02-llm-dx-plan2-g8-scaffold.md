# LLM Developer Experience (Plan 2: g8 scaffold) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every project scaffolded from `hello.g8` and `validator.g8` AI-ready: `AGENTS.md`, `CLAUDE.md` pointer, and the five Scalus skills in `.claude/skills/`.

**Architecture:** Both templates are root-layout g8 repos (template files at repo root). Files are copied from `scalus-skills/` in the scalus repo at template-release time. g8 substitutes `$token$` patterns in file bodies, so all new markdown files are declared `verbatim` in `default.properties`.

**Tech Stack:** giter8 templates, sbt (`sbt new file://…` for render verification).

**Spec:** `docs/superpowers/specs/2026-08-02-llm-developer-experience-design.md`, layer "Scaffold".

## Global Constraints

- Repos: `/Users/nau/projects/lantr/hello.g8` and `/Users/nau/projects/lantr/validator.g8` (siblings of the scalus repo). Commit in each repo on its default branch.
- Conventional commits; never a `Co-Authored-By: Claude` trailer; en dashes only in prose.
- `AGENTS.md`, `CLAUDE.md`, and `SKILL.md` files must contain no g8 `$token$` patterns and be listed in `verbatim` (the local-development skill contains a literal `$value`).
- Skill copies come from `scalus-skills/` in the scalus repo - do not edit them in the templates.
- `validator.g8`'s `Readme.md` uses `$name$` tokens - it must NOT become verbatim.

---

### Task 1: hello.g8 - AGENTS.md, CLAUDE.md, skills, verbatim

**Files (in `/Users/nau/projects/lantr/hello.g8`):**
- Create: `AGENTS.md` (content below)
- Create: `CLAUDE.md` (one-line pointer)
- Create: `.claude/skills/<name>/` - copies of the five skills from `scalus-skills/`
- Modify: `default.properties` (add `verbatim`)

**Interfaces:**
- Consumes: `scalus-skills/{contract,contract-test,local-development,optimize-contract,smart-contract-security-review}` from the scalus repo.
- Produces: the AGENTS.md content Task 2 reuses verbatim.

- [ ] **Step 1: Write `AGENTS.md`** with: what the project is, fetch-first context URLs (llms.txt, llms-api.txt, llms-examples.txt, llms-full.txt, per-page `.md`), build/test commands (scala-cli and sbt), on-chain subset rules (@Compile, prelude-only, toData comparison, require/fail), common pitfalls (param Data encoding must match exactly, script hash changes with any validator edit, measure budgets), and a skills listing.

- [ ] **Step 2: Write `CLAUDE.md`**: `See AGENTS.md for all project guidance for AI coding agents.`

- [ ] **Step 3: Copy skills**

```bash
mkdir -p /Users/nau/projects/lantr/hello.g8/.claude/skills
for s in contract contract-test local-development optimize-contract smart-contract-security-review; do
  cp -R /Users/nau/projects/lantr/scalus/scalus-skills/$s /Users/nau/projects/lantr/hello.g8/.claude/skills/$s
done
```

- [ ] **Step 4: Add verbatim to `default.properties`**

```properties
name=hello-cardano
verbatim=AGENTS.md CLAUDE.md SKILL.md *.mdx
```

- [ ] **Step 5: Render-verify**

```bash
cd "$CLAUDE_JOB_DIR/tmp" && rm -rf g8test && mkdir g8test && cd g8test
sbt --batch "new file:///Users/nau/projects/lantr/hello.g8 --name=probe"
grep -q "llms-api" probe/AGENTS.md && ls probe/.claude/skills/contract/SKILL.md && grep -q 'Transaction failed: \$value' probe/.claude/skills/local-development/SKILL.md
```

Expected: renders without substitution errors; `$value` survives verbatim.

- [ ] **Step 6: Commit** (`feat: ship AGENTS.md and Scalus skills for AI coding agents`)

---

### Task 2: validator.g8 - same files

Same steps as Task 1 in `/Users/nau/projects/lantr/validator.g8`:

- [ ] **Step 1: Copy `AGENTS.md` and `CLAUDE.md` unchanged from hello.g8** (content is template-independent).
- [ ] **Step 2: Copy the five skills** (same `cp -R` loop, target validator.g8).
- [ ] **Step 3: `default.properties`**: keep `name=My Validator`, add the same `verbatim` line. `Readme.md` stays substitutable.
- [ ] **Step 4: Render-verify** with `sbt --batch "new file:///Users/nau/projects/lantr/validator.g8 --name=Probe"`; check `ProbeValidator.scala`-style names still substitute AND `AGENTS.md` is verbatim.
- [ ] **Step 5: Commit.**
