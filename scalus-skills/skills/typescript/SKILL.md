---
name: typescript
description: Use when TypeScript or JavaScript code calls the npm package scalus - the Cardano Emulator, Plutus script evaluation and budgets, applying parameters to a script, evaluating or balancing a transaction, or a Lucid, Mesh or Evolution SDK provider backed by Scalus.
---

# Scalus from TypeScript and JavaScript

The npm package `scalus` is Scalus compiled to JavaScript. It gives you a Cardano node Emulator,
a Plutus script evaluator with the exact budgets of a node, parameter application, and transaction
evaluation and balancing. It does not write validators: validators are Scala code. For that work,
use the `scalus:contract` skill in the Scala project.

## Read the installed API first

Before you write code, read these two files. They ship in the package and match the version the
project uses:

- `node_modules/scalus/scalus.d.ts` - every exported class, function and option, with doc comments.
- `node_modules/scalus/README.md` - the guide, with examples and upgrade notes.

If the package is not installed yet, read `https://scalus.org/llms-ts.txt`. It holds the same two
files for the latest release. Do not use `https://scalus.org/llms-api.txt`: it is the Scala API.

Do not rely on remembered names. The 1.2.0 entry points `Scalus.evaluateScript`,
`Scalus.applyDataArgToScript` and `Scalus.evalPlutusScripts` are deprecated.

## Entry points

Each example is a starting point. Check every signature in `scalus.d.ts` before you use it.

**Emulator** - an in-process Cardano ledger for tests:

```typescript
import { CardanoInfo, Emulator, Utxo, Value } from "scalus";

const emulator = Emulator.create(CardanoInfo.preview(), {
  utxos: [new Utxo("00".repeat(32), 0, aliceAddress, Value.ada(1000n))],
});
const result = emulator.submitTx(signedTxBytes); // never throws
if (!result.isSuccess) console.log(result.errorRule, result.error, result.logs);
```

**Script evaluation** - run one script with arguments and read its budget:

```typescript
import { evaluator, EvaluationOptions } from "scalus";

const options = EvaluationOptions.mainnet("PlutusV3");
const r = evaluator.evaluateScript(scriptHex, [datumCbor, redeemerCbor, contextCbor], options);
r.isSuccess; r.budget.memory; r.budget.steps; r.error?.code; r.logs;
```

To evaluate one script many times, build `new PlutusVM(options)` once and call its
`evaluateScript`.

**Parameter application** - the same contract as the Lucid and Mesh function of the same name:

```typescript
import { uplc } from "scalus";

const applied = uplc.applyParamsToScript(blueprintCompiledCode, [paramCbor]); // double-CBOR hex
```

**Transaction evaluation** - the execution units of every redeemer:

```typescript
for (const r of emulator.evaluateTx(txBytes)) {
  console.log(`${r.tag}[${r.index}]: ${r.budget.memory} mem, ${r.budget.steps} steps`);
}
```

Without an Emulator, `evaluator.evaluateTx(tx, utxos, slotConfig, costModels, protocolMajorVersion)`
takes the resolved inputs and the network settings explicitly. `balancer.balanceTx` sets the
execution units, the fee and the change; it is experimental.

**SDK provider** - the Emulator answers what Lucid's `Provider` and Mesh's
`IFetcher`/`ISubmitter`/`IEvaluator` ask. The npm README section "As a Transaction-Builder
Backend" and `https://scalus.org/docs/testing/js-emulator` show complete adapters.

## Pitfalls

- **ES module.** `require("scalus")` works only on Node 22.12+ and 20.19+. On older Node, use
  `const scalus = await import("scalus")`.
- **PV11 snapshot.** `EvaluationOptions.mainnet(...)` uses a bundled mainnet snapshot at protocol
  version 11. It makes no network request and does not follow later parameter changes. To match
  an Emulator or a live network, use `EvaluationOptions.fromProtocolParams(version, params)`.
- **The V3 return rule.** A Plutus V3 script that does not return unit fails with
  `error.code === "INVALID_RETURN_VALUE"`. The budget is still reported, so you can cost a pure
  function. Under V1 and V2 any result is accepted.
- **A failed script is a result, not an exception.** `evaluateScript` returns `isSuccess: false`
  with an `error.code`. Branch on `code`, not on the message text. `evaluateTx` and
  `balanceTx` throw `PlutusScriptEvaluationError` instead.
- **Accessor objects.** `Utxo`, `Value`, `Asset`, `ProtocolParams` and `CardanoInfo` expose
  fields through prototype accessors. `JSON.stringify` and `toEqual` see an empty object. Call
  `toObject()` before you compare or print them.
- **bigint.** Lovelace, token quantities and budgets are `bigint`. Write `1000n`, not `1000`.
- **CBOR round trips keep the value, not the bytes.** Do not rebuild a datum to reproduce its hash.
