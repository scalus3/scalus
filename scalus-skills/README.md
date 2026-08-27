# Scalus skills

Task guides for AI coding agents working on Scalus smart contracts:

- `skills/contract` - writing validators
- `skills/contract-test` - testing validators
- `skills/local-development` - Emulator + TxBuilder development loop
- `skills/optimize-contract` - execution-budget optimization review
- `skills/smart-contract-security-review` - pre-deploy security audit

## Install as a Claude Code plugin

```
/plugin marketplace add scalus3/scalus
/plugin install scalus@scalus
```

Projects scaffolded from the `scalus3/hello.g8` and `scalus3/validator.g8` templates
already ship these skills in `.claude/skills/`.

Other agents can read the `SKILL.md` files directly when doing the matching task.
This directory doubles as the plugin source (`.claude-plugin/plugin.json`); the
marketplace manifest lives at the repository root.
