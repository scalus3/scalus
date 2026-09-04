# Scalus - Cardano Toolkit for JavaScript

Scalus brings a near-complete Cardano node emulator, Plutus script evaluator, and transaction
tooling to JavaScript and TypeScript. It is compiled from the JVM Scalus codebase via Scala.js.

The core features include a **Node Emulator** -- a local, in-process implementation of a Cardano node, complete with
most of the ledger rules to validate incoming transactions, as well ledger state transitions.

## Conformance

The JavaScript build is checked against the
[Plutus conformance test suite](https://github.com/IntersectMBO/plutus/tree/master/plutus-conformance)
on every CI run, not only the JVM build. It passes **999 of 999** UPLC evaluation cases with none
skipped.

724 of those cases are programs the reference evaluates successfully, and each of the 724 asserts
two things: that the resulting term is α-equivalent to the expected one, and that the **execution
budget matches the reference exactly** – the same CPU and memory numbers a Cardano node would
charge, under Plutus's reference variant-E builtin cost model and CEK machine costs.

The other 275 are programs the reference rejects: 220 it fails to evaluate, and 55 it fails to
parse. The corpus records no expected term or budget for those, so what is asserted is that Scalus
rejects them the same way.

Budget equality is the part that matters for a transaction builder. An evaluator that agrees on
success but disagrees on cost still produces transactions a node rejects.

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
  `<script type="module">`. `require("scalus")` still works from CommonJS on Node 22.12+ and
  20.19+, which load an ES module from `require()` natively — there is no separate CommonJS
  build, so you get one module instance either way. On older Node it fails with
  `ERR_REQUIRE_ESM`; use `const scalus = await import("scalus")` there.
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
  which is below min-ada, so seeded outputs were unusable. (`withAddresses` is deprecated
  since; see [Emulator](#emulator) for `Emulator.create`.)

## Emulator

The emulator implements Cardano ledger validation locally. Transactions go through the same
rule checks as a real node: phase 1 & 2 validation, and a near-complete suite of ledger rules. This makes it suitable
for automated tests, local development, and any application that needs a self-contained Cardano environment.

### Quick Start

```typescript
import { CardanoInfo, Emulator, Utxo, Value } from "scalus";

const alice = "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw";

// A network is a slot configuration and a set of protocol parameters together, so the
// emulator cannot validate against one network's parameters while doing slot arithmetic
// for another.
const emulator = Emulator.create(CardanoInfo.preview(), {
  utxos: [new Utxo("00".repeat(32), 0, alice, Value.ada(1000n))],
});

// Build and sign a transaction with your favourite builder, then submit the bytes:
const result = emulator.submitTx(txCborBytes);
if (result.isSuccess) {
  console.log(result.txHash);
} else {
  console.log(`rejected by ${result.errorRule}: ${result.error}`);
  console.log(result.logs.join("\n")); // empty unless a Plutus script traced
}
```

`CardanoInfo.mainnet()`, `.preprod()`, `.preview()` and `.custom(network, slotConfig, params)`
cover the networks. Every field of the options object is optional: `utxos`, `slot`,
`stakeRegistrations`, `poolRegistrations`, `drepRegistrations` and `datums`.

> `new Emulator(utxosCbor, slotConfig)`, `Emulator.withState` and `Emulator.withAddresses` are
> deprecated. They take protocol parameters from the slot configuration alone, so a
> `SlotConfig.preview` emulator validated transactions against mainnet's parameters.

### Querying UTxOs

Queries return objects, not CBOR. `getUtxos()` returns everything; `getUtxos(filter)` narrows
it, and the filtering happens inside the ledger, so no object is built for a row the filter
drops.

```typescript
emulator.getUtxos();                                  // everything
emulator.getUtxos({ address: alice });                // one address
emulator.getUtxos({ paymentCredential: keyHashHex }); // any address with this payment part
emulator.getUtxos({ unit: policyId + assetNameHex }); // holders of one asset
emulator.getUtxos({ minLovelace: 5_000_000n, limit: 10 });
emulator.getUtxos({ outRefs: [{ txHash, outputIndex: 0 }] });

const [utxo] = emulator.getUtxos({ address: alice });
utxo.txHash;        // hex
utxo.outputIndex;   // number
utxo.address;       // bech32
utxo.value.coin;    // bigint lovelace
utxo.value.assets;  // Asset[], each with policyId, assetName, quantity and unit
utxo.datumHash;     // string | undefined
utxo.inlineDatum;   // Uint8Array | undefined
utxo.scriptRef;     // Uint8Array | undefined
```

Fields given are ANDed together. `outRefs` is the exception: it matches any of the references
given, which is what a "resolve these inputs" query needs.

A `Utxo` holds the ledger's own input and output, so one a query hands you can be handed
straight back to `evaluateTx` or `addUtxo` with no encoding step in between.

`Utxo`, `Value`, `Asset`, `ProtocolParams` and `CardanoInfo` expose their fields through
accessors on the prototype, so `JSON.stringify`, object spread and a test framework's `toEqual`
all see an empty object. Call `toObject()` and assert on that.

`getUtxosCbor()` still returns the whole set as one CBOR map, for a consumer that wants the
raw ledger encoding.

### Protocol Parameters

```typescript
const params = emulator.getProtocolParameters();
params.txFeePerByte;         // number
params.utxoCostPerByte;      // bigint
params.maxTxExecutionSteps;  // bigint
params.costModels.PlutusV3;  // number[], keyed by language rather than by position

// For an adapter that already parses Blockfrost's shape:
ProtocolParams.fromBlockfrostJson(params.toBlockfrostJson());
```

Quantities that can exceed `Number.MAX_SAFE_INTEGER` are `bigint`; fee rates, sizes,
percentages, counts and slots are `number`.

### Evaluating Scripts Against the Ledger

`evaluateTx` runs every Plutus script a transaction triggers, resolving its inputs against this
emulator's UTxO set, slot config, cost models and protocol version. Nothing has to be passed
in, so nothing can be passed in wrongly.

```typescript
for (const r of emulator.evaluateTx(txCborBytes)) {
  console.log(`${r.tag}[${r.index}]: ${r.budget.memory} mem, ${r.budget.steps} steps`);
}

// Inputs the emulator does not hold yet go in the second argument.
emulator.evaluateTx(txCborBytes, [new Utxo(txHash, 0, scriptAddress, Value.ada(5n))]);
```

A failing script throws `PlutusScriptEvaluationError`, which extends `Error` and carries the
script's trace logs in `.logs`.

### Time Control

```typescript
emulator.getSlot();           // current slot
emulator.setSlot(500);        // jump to an absolute slot, forwards or backwards
emulator.tick(10);            // advance by 10 slots
emulator.getTime();           // POSIX ms at which the current slot starts
emulator.setTime(Date.now());
```

Use this to test validity-interval logic, time-locked scripts, and epoch transitions. No blocks
are produced in between and no rewards are paid out.

### Transaction Lookup

```typescript
emulator.hasTx(txHashHex);                // boolean
emulator.getTransactionStatus(txHashHex); // "Confirmed" | "NotFound"
emulator.getTransaction(txHashHex);       // Uint8Array | undefined
emulator.getAppliedTxs();                 // [{ txHash, slot }, ...], oldest first
```

### Staking and Delegation

Stake queries take a bech32 reward address, so a key credential and a script credential are
told apart by the address itself:

```typescript
emulator.getStakeReward("stake_test1...");   // bigint | undefined
emulator.getDelegation("stake_test1...");    // { poolId?: string, rewards: bigint }
emulator.getStakeDistribution();             // live stake per registered credential
```

### Datum Store

```typescript
emulator.getDatum(datumHashHex);  // Uint8Array | undefined
```

Datums witnessed by accepted transactions are indexed automatically. Pre-seed others with the
`datums` option of `Emulator.create`.

### Editing the Ledger Directly

```typescript
emulator.addUtxo(utxo);                          // seed a UTxO, skipping validation
emulator.removeUtxo({ txHash, outputIndex: 0 }); // take one away
const snap = emulator.snapshot();                // independent copy of the current state
```

`snapshot` copies the UTxOs, registrations and rewards, the datum store, the accepted
transactions and the current slot, so one expensive setup can branch into several test
scenarios without being rebuilt.

### Seeding Stake, Pools, DReps and Datums

Every field of `Emulator.create`'s options object beyond `utxos` seeds ledger state that a
transaction would otherwise have to establish first:

```typescript
const emulator = Emulator.create(CardanoInfo.preview(), {
  utxos: [new Utxo(txHash, 0, alice, Value.ada(1000n))],
  slot: 1_000_000,   // defaults to the slot containing Date.now()
  stakeRegistrations: [
    { credentialType: "key", credentialHash: "abcd…", rewards: 42_000_000n },
    { credentialType: "key", credentialHash: "1234…", rewards: 0n, delegatedTo: poolIdHex },
  ],
  poolRegistrations: [{ params: poolRegCertCbor }],
  drepRegistrations: [
    { credentialType: "key", credentialHash: "ef01…", deposit: 500_000_000n },
  ],
  datums: [{ hash: datumHashHex, datum: datumCborHex }],
});
```

### As a Transaction-Builder Backend

Both MeshJS and lucid-evolution take a provider object, and the emulator answers what either of
them asks: `getProtocolParameters`, `getUtxos(filter)`, `submitTx` and `evaluateTx` cover
lucid's `Provider` and mesh's `IFetcher`/`ISubmitter`/`IEvaluator` between them, so an adapter
is field renaming with no CBOR codec, no protocol-parameter table and no cost model of its own.
`Asset.unit` is the concatenated policy id and asset name both SDKs call a unit, and
`UtxoFilter.paymentCredential` is the query a wallet makes.

Complete, runnable adapters for both are in this package's test suite, at
`__tests__/provider-lucid.test.ts` and `__tests__/provider-mesh.test.ts`. See
[the emulator guide](https://scalus.org/docs/testing/js-emulator) for the walkthrough.

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
