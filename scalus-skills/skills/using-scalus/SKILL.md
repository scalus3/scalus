---
name: using-scalus
description: Use at the start of any task in a Scalus project, to choose the Scalus skill for smart contract work.
---

# Using the Scalus skills

This project uses Scalus: Cardano smart contracts written in Scala 3 and compiled to
Plutus Core. The `scalus` plugin has one skill per contract task. Load it with the
`Skill` tool.

## Routing table

| Task | Skill |
|------|-------|
| Write or change on-chain code (`@Compile`, `extends Validator`, `DataParameterizedValidator`) | `scalus:contract` |
| Write or change a test of on-chain code | `scalus:contract-test` |
| Build transactions, use the Emulator or TxBuilder | `scalus:local-development` |
| Reduce execution units or script size | `scalus:optimize-contract` |
| Review on-chain code before you call it done | `scalus:smart-contract-security-review` |

## Rule

Invoke the matching skill before the first line of on-chain code, and before the first
line of its test.

The rule also applies inside a process skill (brainstorming, planning, TDD, debugging).
The process skill decides the steps. The `scalus:` skill decides the code. A process
skill that says "invoke no other skill" does not cancel this rule.

Before you call on-chain work done, run `scalus:smart-contract-security-review` on it.
