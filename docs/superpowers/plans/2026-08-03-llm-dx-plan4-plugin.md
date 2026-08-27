# LLM Developer Experience (Plan 4: Claude Code plugin) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `/plugin marketplace add scalus3/scalus` + `/plugin install scalus@scalus` installs the five Scalus skills into any project.

**Architecture:** The scalus repo root becomes a plugin marketplace (`.claude-plugin/marketplace.json`). `scalus-skills/` becomes the plugin: skills move into `scalus-skills/skills/<name>/` (the auto-discovery layout) and `scalus-skills/.claude-plugin/plugin.json` names the plugin. No symlinks inside the plugin; `.claude/skills/` dogfood symlinks are repointed.

**Spec:** `docs/superpowers/specs/2026-08-02-llm-developer-experience-design.md`, layer "Plugin (later)". Manifest schemas confirmed against code.claude.com/docs/en/plugin-marketplaces.md and plugins-reference.md.

## Global Constraints

- Only `name` is required in plugin.json; marketplace.json needs `name`, `owner.name`, `plugins[]`.
- Relative `source` paths resolve against the marketplace root (the repo root).
- Skill copies in the g8 templates now sync from `scalus-skills/skills/<name>`.

---

### Task 1: restructure + manifests + verify

- [ ] **Step 1:** `git mv scalus-skills/<name> scalus-skills/skills/<name>` for the five skills.
- [ ] **Step 2:** `scalus-skills/.claude-plugin/plugin.json`:

```json
{
  "name": "scalus",
  "description": "Scalus smart contract development skills: write, test, optimize, and security-review Cardano validators in Scala 3.",
  "version": "1.0.0",
  "author": { "name": "Scalus" }
}
```

- [ ] **Step 3:** `.claude-plugin/marketplace.json` at repo root:

```json
{
  "name": "scalus",
  "owner": { "name": "Scalus", "url": "https://scalus.org" },
  "plugins": [
    {
      "name": "scalus",
      "source": "./scalus-skills",
      "description": "Skills for Scalus Cardano smart contract development"
    }
  ]
}
```

- [ ] **Step 4:** Repoint the five `.claude/skills/<name>` symlinks to `../../scalus-skills/skills/<name>`; verify they resolve.
- [ ] **Step 5:** Verify install non-interactively in a throwaway `CLAUDE_CONFIG_DIR`: `claude plugin marketplace add <repo-path>` then `claude plugin install scalus@scalus`; check the five skills land in the installed plugin cache.
- [ ] **Step 6:** Mention the install commands in `scalus-skills/README.md` (new, short) and in `llms.txt`. Commit.
