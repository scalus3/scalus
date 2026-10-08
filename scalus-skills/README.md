# Scalus skills

Task guides for AI coding agents working on Scalus smart contracts:

- `skills/contract` - writing validators
- `skills/contract-test` - testing validators
- `skills/local-development` - Emulator + TxBuilder development loop
- `skills/optimize-contract` - execution-budget optimization review
- `skills/smart-contract-security-review` - pre-deploy security audit
- `skills/using-scalus` - routing table from task to skill, injected at session start

## Install as a Claude Code plugin

```
/plugin marketplace add scalus3/scalus
/plugin install scalus@scalus
```

Projects scaffolded from the `scalus3/hello.g8` and `scalus3/validator.g8` templates
enable this plugin in `.claude/settings.json`.

In a Scalus project (a build file that mentions `scalus`), the `hooks/session-start` hook
injects `skills/using-scalus` at session start: a routing table from task to skill.

Other agents can read the `SKILL.md` files directly when doing the matching task.
This directory doubles as the plugin source (`.claude-plugin/plugin.json`); the
marketplace manifest lives at the repository root.

## Changing a skill

Bump `version` in `.claude-plugin/plugin.json` in the same commit. Installed plugins
update only when the version changes. Contributors to this repository use the plugin
too; there is no second copy under `.claude/skills/`.
