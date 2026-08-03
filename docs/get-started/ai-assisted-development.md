Source: https://scalus.org/docs/get-started/ai-assisted-development

# AI-Assisted Development

Scalus ships first-class support for LLM coding agents (Claude Code, Cursor, Codex, Copilot, and
friends). Three layers work together: fetchable context artifacts on this site, AI-ready project
templates, and a Claude Code plugin with task skills.

## Context artifacts (llms.txt)

LLMs trained before Scalus 1.0 hallucinate outdated APIs. These plain-text artifacts give any
agent the current ground truth:

| URL | Content | When an agent should fetch it |
|---|---|---|
| [/llms.txt](https://scalus.org/llms.txt) | Index of everything below | First contact |
| [/llms-api.txt](https://scalus.org/llms-api.txt) | Version-pinned public API signatures: prelude, builtins, validator traits, ledger, TxBuilder, Emulator, testkit | Before writing any Scalus code |
| [/llms-examples.txt](https://scalus.org/llms-examples.txt) | 21 complete example validators with their tests | When writing a new validator or test |
| [/llms-full.txt](https://scalus.org/llms-full.txt) | All documentation pages as one markdown file | Deep dives |

Every documentation page is also available as plain markdown: append `.md` to its URL, e.g.
[/docs/smart-contracts/validators.md](https://scalus.org/docs/smart-contracts/validators.md).

All artifacts are generated from the source of truth on every release – the API cheatsheet comes
from the compiled code itself, so it cannot drift from the published library.

## AI-ready project templates

Projects scaffolded from the [hello.g8 or validator.g8 templates](/docs/get-started/project-commands)
are AI-ready out of the box:

- **`AGENTS.md`** – the cross-tool agent instruction file: build/test commands, the on-chain
  subset rules (`@Compile` restrictions, `toData` comparisons, prelude-only imports), common
  pitfalls, and the artifact URLs above. Claude Code, Cursor, and most agents read it
  automatically. `CLAUDE.md` points at it.
- **`.claude/skills/`** – the five Scalus task skills (below), pre-installed.

## Claude Code plugin

For existing projects, install the skills with the Scalus plugin:

```
/plugin marketplace add scalus3/scalus
/plugin install scalus@scalus
```

The plugin ships five skills that load on demand when the task matches:

| Skill | Use |
|---|---|
| `contract` | Writing validators |
| `contract-test` | Testing validators |
| `local-development` | Emulator + TxBuilder development loop |
| `optimize-contract` | Execution-budget optimization review |
| `smart-contract-security-review` | Pre-deploy security audit |

Skills are plain markdown in
[`scalus-skills/`](https://github.com/scalus3/scalus/tree/master/scalus-skills) – agents other
than Claude Code can read the `SKILL.md` files directly.

## Tips for best results

- Tell your agent to **fetch `/llms-api.txt` before writing code** and check every signature it
  plans to use. This is the single highest-impact instruction.
- Point it at `/llms-examples.txt` and ask it to imitate the HTLC example – agents follow working
  code better than prose.
- Have it **measure execution budgets** with the testkit assertions (`assertBudgetWithin`)
  instead of guessing.
- Ask for negative tests: every validator test suite should prove the failure cases fail.
