# Scalus - Cardano Toolkit for JavaScript

Scalus brings a near-complete Cardano node emulator, Plutus script evaluator, and transaction
tooling to JavaScript and TypeScript. It is compiled from the JVM Scalus codebase via Scala.js.

The core features include a **Node Emulator** -- a local, in-process implementation of a Cardano node, complete with
most of the ledger rules to validate incoming transactions, as well ledger state transitions.

## Installation

```bash
npm install scalus
```

## Upgrading from 0.18.x

The npm package now carries the same version number as the Scalus JVM libraries, so 0.18.1 is
followed by 1.1.1. Six changes affect existing code:

- **Protocol version 11 (van Rossem) is the default.** `evaluateScript` and
  `evalPlutusScripts` use the mainnet PV11 cost models. Version 0.18.1 used PV10
  (Plomin). Execution budgets therefore differ. Pass `protocolMajorVersion` to
  `evalPlutusScripts` to select another version.
- **`scalus.js` is an ES module.** Import it with `import` or with
  `<script type="module">`. `require("scalus")` now fails with
  `ERR_PACKAGE_PATH_NOT_EXPORTED`; from CommonJS use `const scalus = await import("scalus")`.
  The old CommonJS shim for browsers no longer works. See [Browser Usage](#browser-usage).
- **`ExUnits`, `Result` and `Redeemer` are top-level exports**, not members of `Scalus`. The
  runtime behaviour did not change, but the type declarations were wrong before. Import them
  from the package root: `import { ExUnits } from "scalus"`.
- **The evaluation functions are top-level exports too**, and two classes were renamed:
  `Result` is now `EvaluationResult` and `Redeemer` is now `RedeemerBudget`. Prefer
  `import { evaluateScript } from "scalus"` over `Scalus.evaluateScript`. The `Scalus`
  object and the old class names still work as deprecated **value** aliases and will be
  removed in a later release.
- **`Scalus.Result` and `Scalus.Redeemer` in a type position no longer compile.** Version
  0.18.1 declared `Scalus` as a TypeScript `namespace` that also re-exported the class
  *types*; the generated declarations export it as a `const`, so
  `const r: Scalus.Result = ...` now fails with
  `'Scalus' refers to a value, but is being used as a type`. Calls through the object
  (`Scalus.evaluateScript(...)`) are unaffected. Import the types from the package root
  instead: `import { EvaluationResult, ExUnits, RedeemerBudget } from "scalus"`.
- **`Emulator.withAddresses` funds 10 000 ada per address** when you omit
  `lovelacePerAddress`, that is `10_000_000_000n` lovelace. It funded 10 000 lovelace before,
  which is below min-ada, so seeded outputs were unusable.

## Emulator

The emulator implements Cardano ledger validation locally. Transactions go through the same
rule checks as a real node: phase 1 & 2 validation, and a near-complete suite of ledger rules. This makes it suitable
for automated tests, local development, and any application that needs a self-contained Cardano environment.

### Quick Start

```typescript
import { Emulator, SlotConfig } from "scalus";

// Fund two addresses with 10 000 ADA each
const emulator = Emulator.withAddresses(
  [aliceAddress, bobAddress],
  SlotConfig.preview,
  10_000_000_000n   // lovelace (optional, defaults to 10 000 ADA)
);

// Query UTxOs
const aliceUtxos = emulator.getUtxosForAddress(aliceAddress); // Uint8Array[]
const allUtxos   = emulator.getAllUtxos();                     // Uint8Array[]
const utxoMap    = emulator.getUtxosCbor();                    // single CBOR map

// Build & sign a transaction with your favourite CBOR library, then submit:
const result = emulator.submitTx(txCborBytes);
// { isSuccess: true, txHash: "ab12…" }
// or { isSuccess: false, error: "…", logs: ["…"] }
```

`getAllUtxos` and `getUtxosForAddress` return one CBOR map per UTxO, each holding a single
entry; `getUtxosCbor` returns the whole set in one map. In both cases the map keys are
transaction inputs (a `[transactionHash, outputIndex]` pair) and the values are transaction
outputs, exactly as in the Cardano ledger CDDL. Decode them with your favourite CBOR codec
library.

### Time Control

```typescript
emulator.setSlot(500);   // jump to an absolute slot
emulator.tick(10);       // advance by 10 slots
```

Use this to test validity-interval logic, time-locked scripts, and epoch transitions.

### Transaction Lookup

```typescript
emulator.hasTx(txHashBytes);  // true if the tx was accepted
```

### Staking and Delegation

Query delegation state and reward balances:

```typescript
emulator.getDelegation(stakeCredentialCbor);
// { poolId: Uint8Array | null, rewards: bigint }

emulator.getStakeReward(scriptHashHex);
// bigint | null
```

### Datum Store

```typescript
emulator.getDatum(datumHashBytes);  // Uint8Array | null
```

Datums observed in submitted transactions are indexed automatically. You can also pre-seed
them via `withState` (see below).

### Snapshots

```typescript
const snap = emulator.snapshot();  // independent copy of the current state
```

Useful for branching test scenarios from a shared setup without re-submitting transactions.

### Full Initial State

`Emulator.withState` lets you seed not just UTxOs but also stake credentials, pool
registrations, DRep registrations, and a datum store:

```typescript
const emulator = Emulator.withState(
  {
    utxos: utxoMapCbor,           // Uint8Array: CBOR map, input -> output
    stakeRegistrations: [
      { credentialType: "key", credentialHash: "abcd…", rewards: 42_000_000n },
      { credentialType: "key", credentialHash: "1234…", rewards: 0n, delegatedTo: poolIdHex },
    ],
    poolRegistrations: [
      { params: poolRegCertCbor },
    ],
    drepRegistrations: [
      { credentialType: "key", credentialHash: "ef01…", deposit: 500_000_000n },
    ],
    datums: [
      { hash: datumHashHex, datum: datumCborHex },
    ],
  },
  SlotConfig.preview
);
```

All fields except `utxos` are optional.

## Plutus Script Evaluation

### Evaluate a Single Script

```typescript
import { applyDataArgToScript, evaluateScript } from "scalus";

// Scripts are represented as double-CBOR-encoded hex strings
const script = "545301010023357389210753756363657373004981";

// Apply a data argument (Plutus Data JSON format)
const applied = applyDataArgToScript(script, JSON.stringify({ int: 42 }));

const result = evaluateScript(applied);
// { isSuccess: true, budget: { memory: 1032n, steps: 203598n }, logs: [] }
```

### Profile a Script

`evaluateScriptProfile` evaluates like `evaluateScript` and also returns the CEK machine
profiling data as JSON in `profileJson`: cost per source location, cost per builtin, and the
transition edges.

```typescript
const result = evaluateScriptProfile(applied);
const profile = JSON.parse(result.profileJson!);
```

This package gives you the data, not the report. The renderer that turns the JSON into the
interactive HTML report (`ProfileFormatter`) is a Scala-side tool, shipped with the Scalus
library for the JVM; it is left out here so the bundle stays small.

### Evaluate All Scripts in a Transaction

```typescript
import { evalPlutusScripts, SlotConfig } from "scalus";

// One cost model per Plutus version, indexed by position: [0] is V1, [1] is V2, [2] is V3.
// Give a model for every version the transaction uses; an earlier version cannot be skipped.
const costModels = [plutusV1Costs, plutusV2Costs, plutusV3Costs]; // number[][]

const redeemers = evalPlutusScripts(
  txCborBytes,        // Uint8Array
  utxoCborBytes,      // Uint8Array: CBOR map, input -> output
  SlotConfig.mainnet,
  costModels
);

for (const r of redeemers) {
  console.log(`${r.tag}[${r.index}]: ${r.budget.memory} mem, ${r.budget.steps} steps`);
}
```

Each `r.tag` is one of `"Spend"`, `"Mint"`, `"Cert"`, `"Reward"`, `"Voting"` or
`"Proposing"`, and `r.index` is the position within that group, counting from 0.

On failure, `evalPlutusScripts` throws a `PlutusScriptEvaluationError` with
`.message` and `.logs` (the script's trace output). Only script failures arrive that way;
malformed transaction or UTxO CBOR throws an ordinary error. `evaluateScript` and
`evaluateScriptProfile` never throw at all: they report a failure as a result with
`isSuccess: false` and the message in `logs[0]`.

### Plutus Data JSON Format

Data arguments passed to `applyDataArgToScript` use the standard Plutus Data JSON encoding:

```jsonc
{ "int": 42 }
{ "bytes": "deadbeef" }
{ "list": [{ "int": 1 }, { "int": 2 }] }
{ "map": [{ "k": { "int": 1 }, "v": { "bytes": "aa" } }] }
{ "constructor": 0, "fields": [{ "int": 42 }] }
```

## Slot Configuration

Built-in configs for mainnet, preview, and preprod, or construct your own:

```typescript
import { SlotConfig } from "scalus";

const cfg = SlotConfig.mainnet; // or .preview, .preprod
const time = cfg.slotToTime(100_000); // POSIX ms
const slot = cfg.timeToSlot(time);    // fractional unless `time` is on a slot boundary

// Custom config
const custom = new SlotConfig(zeroTime, zeroSlot, slotLength);
```

`timeToSlot` does not round. With one-second slots, `cfg.timeToSlot(Date.now())` almost
always has a fraction, so round it yourself (for example with `Math.floor`) before you use it
as a slot number.

## Browser Usage

`scalus.js` is a self-contained ES module. It has no runtime dependencies, so a browser can
load it directly, with no bundler and no import map:

```html
<script type="module">
    import { evaluateScript, SlotConfig, Emulator } from "./scalus.js";

    const result = evaluateScript(scriptDoubleCborHex);
    console.log(result.isSuccess, result.budget.steps);
</script>
```

## TypeScript Definitions

`scalus.d.ts` is generated from the Scala sources by `scalus-ts-exporter`
(`sbt scalusCardanoLedgerJS/generateDts`). Do not edit it by hand. CI fails
if it drifts from the Scala facades.

## License

Apache-2.0

## Links

- [Repository](https://github.com/scalus3/scalus)
- [Documentation](https://scalus.org)
