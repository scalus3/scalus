---
name: using-scalus
description: Use at the start of any task in a Scalus project, to choose the Scalus skill for smart contract work or for TypeScript and JavaScript code that uses the npm package scalus.
---

# Using the Scalus skills

This project uses Scalus. In a Scala project, Scalus compiles Cardano smart contracts written
in Scala 3 to Plutus Core. In a TypeScript or JavaScript project, the npm package `scalus` gives
a Cardano Emulator, a Plutus script evaluator and transaction tools. The `scalus` plugin has one
skill per task. Load it with the `Skill` tool.

## Routing table

| Task | Skill |
|------|-------|
| Write or change on-chain code (`@Compile`, `extends Validator`, `DataParameterizedValidator`) | `scalus:contract` |
| Write or change a test of on-chain code | `scalus:contract-test` |
| Build transactions, use the Emulator or TxBuilder from Scala | `scalus:local-development` |
| Reduce execution units or script size | `scalus:optimize-contract` |
| Review on-chain code before you call it done | `scalus:smart-contract-security-review` |
| Use Scalus from TypeScript or JavaScript (Emulator, script evaluation, parameters, transaction evaluation) | `scalus:typescript` |

## Rule

The rule in this section applies to Scala on-chain code only. TypeScript and JavaScript code
does not write validators.

Invoke the matching skill before the first line of on-chain code, and before the first
line of its test.

The rule also applies inside a process skill (brainstorming, planning, TDD, debugging).
The process skill decides the steps. The `scalus:` skill decides the code. A process
skill that says "invoke no other skill" does not cancel this rule.

Before you call on-chain work done, run `scalus:smart-contract-security-review` on it.
