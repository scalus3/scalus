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
  `import { evaluateScript } from "scalus"` over `Scalus.evaluateScript`; since 1.2.0 both are
  deprecated in favour of `evaluator.evaluateScript` and `evaluator.evaluateTx`. The `Scalus`
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
emulator.getUtxos({ paymentCredential: keyHashHex }); // either credential kind
emulator.getUtxos({ paymentCredential: keyHashHex, paymentCredentialType: "key" });
emulator.getUtxos({ paymentCredential: scriptHashHex, paymentCredentialType: "script" });
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

A failing script throws `PlutusScriptEvaluationError`, the same error `evaluator.evaluateTx`
throws; see "When a script fails" below.

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

`uplc` applies arguments to a script, and `evaluator` runs it. Both take CBOR, so whatever your
SDK already produces is what you pass in.

### Apply arguments and evaluate

```typescript
import { uplc, evaluator, EvaluationOptions } from "scalus";

const script = "545301010023357389210753756363657373004981";
const options = EvaluationOptions.mainnet("PlutusV3");

const result = evaluator.evaluateScript(script, ["182a"], options);
// { isSuccess: true, budget: { memory: 1032n, steps: 203598n }, logs: [...] }
```

Scripts and arguments are each a hex `string` or a `Uint8Array`, and the two mix freely:

```typescript
evaluator.evaluateScript(scriptBytes, ["182a", argumentBytes], options);
```

Arguments apply left to right, so `[datum, redeemer, context]` is the order the script sees.

A script may arrive as raw flat, single CBOR or double CBOR; all are accepted, and none is guessed
at (a flat program never starts with a CBOR byte-string header).

To bake parameters into a script, `uplc.applyParamsToScript` has the contract of Lucid's and
Mesh's function of the same name: any script form in, double-CBOR hex out.

```typescript
uplc.applyParamsToScript(blueprintCompiledCode, [paramCbor]);   // double-CBOR hex
```

It is a composition of byte-level primitives, which also build any other form a tool expects:

```typescript
import { uplc, cbor, bytesToHex } from "scalus";

const flat = uplc.applyArgs(uplc.decodeToFlat(script), [paramCbor]);  // flat program bytes
const single = cbor.wrapBytes(flat);                // blueprint compiledCode, script hash input
const double = cbor.wrapBytes(single);              // Lucid and Mesh script objects, .plutus files
bytesToHex(double);                                 // hexToBytes goes the other way
```

CBOR round trips preserve the *value*, not the original bytes. Semantically equal Data can have
more than one encoding, so do not use this path to reproduce a datum hash.

### Feeding it from Lucid, Mesh or the Evolution SDK

Serialize with the SDK you already use, then hand over the bytes. No Scalus-specific Data type,
no JSON step. Each of these produces identical CBOR:

```typescript
import { Data as LucidData, Constr } from "@lucid-evolution/lucid";
import { toPlutusData } from "@meshsdk/core-cst";
import * as EvolutionData from "@evolution-sdk/evolution/Data";

const fromLucid = LucidData.to(new Constr(0, [42n, "deadbeef"]));       // Data.to, not Data.do
const fromMesh = toPlutusData({ alternative: 0, fields: [42n, "deadbeef"] }).toCbor().toString();
const fromEvolution = EvolutionData.toCBORBytes(EvolutionData.constr(0n, [42n]));

evaluator.evaluateScript(script, [fromLucid], options);
```

`LucidData` above is a local import alias for Lucid's `Data`; the SDKs are test dependencies of
this package, never runtime ones.

### Evaluation options

`EvaluationOptions` is both the record type and the factory object, so the same import works in
type and value position:

```typescript
const options: EvaluationOptions = EvaluationOptions.mainnet("PlutusV3");
```

Options are plain records. Copy and adjust them with object spread:

```typescript
const forV2: EvaluationOptions = {
  ...options,
  plutusVersion: "PlutusV2",
  costModel: params.costModels.PlutusV2,
};
```

A field this API does not know is ignored rather than rejected, so spreading a record that
carries extra keys is safe.

| Field | Meaning |
| --- | --- |
| `plutusVersion` | `"PlutusV1"`, `"PlutusV2"` or `"PlutusV3"`; the bytes do not carry it |
| `protocolMajorVersion` | Picks builtin semantics and costing rules |
| `costModel` | That language's parameters, in protocol order |
| `maxBudget` | Optional `{ memory, steps }`, as `number` or `bigint`: the most the script may spend |

There is no implicit default: the first three fields are required. `EvaluationOptions.mainnet(version)`
fills them from the protocol parameters bundled with this release of Scalus, a snapshot rather
than a live query, so pin your own parameters for reproducible tests and supply the target
network's for production.

For custom parameters, pass a `ProtocolParams` handle rather than a plain object:

```typescript
import { CardanoInfo, ProtocolParams } from "scalus";

EvaluationOptions.fromProtocolParams("PlutusV3", CardanoInfo.mainnet().protocolParams);
EvaluationOptions.fromProtocolParams("PlutusV3", ProtocolParams.fromBlockfrostJson(json));
```

`fromBlockfrostJson` is the only JSON reader exported to JavaScript. For any other provider, take
the positional cost array out of its response and write the record yourself:

```typescript
const options: EvaluationOptions = {
  plutusVersion: "PlutusV3",
  protocolMajorVersion: 11,
  costModel: json.cost_models_raw.PlutusV3,
};
```

**No cost-model length is rejected.** A short model configures and prices out what it does not
cover: a parameter the array does not reach costs more than any budget, so a builtin whose
parameters are absent cannot run, rather than being given a plausible cost nobody could tell from
a real one. That is what plutus does, and it is what lets a custom network, an older chain and a
future longer model all configure.

### The validator return rule

`evaluateScript` enforces CIP-117: under Plutus V3 a validator must return unit. Under V1 and V2
any result is accepted, as the ledger accepts it.

The rule is checked *after* the machine stops, so a rejected result still carries the budget the
program really spent and the traces it emitted. That is how you cost a pure on-chain function
that returns something other than unit — read `error.code`, not `isSuccess`:

```typescript
const r = evaluator.evaluateScript(program, [], options);   // program returns (con integer 3)
r.isSuccess;        // false
r.error?.code;      // "INVALID_RETURN_VALUE"
r.budget.steps;     // the real cost, as if it had succeeded
```

It does not build a `ScriptContext`, resolve UTxOs, or validate a transaction. For that, use
`evaluator.evaluateTx` or the `Emulator`.

**Bound untrusted scripts with `maxBudget`.** Without it there is no ceiling, so a script that
does not terminate does not return. With it, a script that spends more stops with
`OUT_OF_BUDGET`, and `budget` says what it spent:

```typescript
const r = evaluator.evaluateScript(script, [context], { ...options, maxBudget: redeemer.exUnits });
if (r.error?.code === "OUT_OF_BUDGET") { /* spent more than the redeemer declared */ }
```

### Failures

A script that fails is an outcome, not an exception: `isSuccess` is `false`, and `error` says why.

```typescript
const result = evaluator.evaluateScript(failingScript, [], options);
result.error;   // { code: "SCRIPT_FAILURE", message: "..." }
result.logs;    // the script's traces, and nothing else
result.budget;  // what it spent before failing
```

| `code` | Meaning |
| --- | --- |
| `SCRIPT_FAILURE` | The script failed, for example by evaluating `error` |
| `BUILTIN_FAILURE` | A builtin rejected its arguments |
| `INVALID_RETURN_VALUE` | A Plutus V3 script returned something other than unit |
| `OUT_OF_BUDGET` | The script spent more than `maxBudget` |
| `INTERNAL_ERROR` | A defect in Scalus, not in your script: please report it |

Branch on `code`; the `message` text is free to improve. The result, `error` included, is plain
data: `JSON.stringify(result)` works, with the budget as decimal strings. The budget spent at each
trace is in the profile: `evaluateScriptProfile` returns `profileJson` whose `traces` array pairs
every message with the cumulative `mem` and `cpu`.

A call that cannot be read throws a `TypeError` instead: a script or an argument that is not hex
or bytes of the right CBOR, or options with a missing or mistyped field. The `uplc` utilities
throw the same way.

`evaluator.evaluateTx` and `evalPlutusScripts` throw on a failing script too, because a
transaction has no partial success; see "When a script fails" under "Evaluate All Scripts in a
Transaction".

### Profiling

`evaluator` does not profile. A profile attributes cost to source lines, which needs the compiler
output that produced the script, so it belongs to the build rather than to the runtime. Profile
from the JVM, where `PlutusVM.evaluateScriptProfile` and `ProfileFormatter` render the full
interactive report.

The deprecated `evaluateScriptProfile(script)` still works and still fills `profileJson`, on a
Plutus V3 mainnet machine only.

### Migrating from the earlier entry points

`applyDataArgToScript`, `evaluateScript`, `evaluateScriptProfile` and `evalPlutusScripts` still
work exactly as before. All four are deprecated; three have a replacement in the explicit API:

| Was | Now |
| --- | --- |
| `applyDataArgToScript(script, json)` | `uplc.applyParamsToScript(script, [cbor])` |
| `evaluateScript(script)` | `evaluator.evaluateScript(script, args, EvaluationOptions.mainnet("PlutusV3"))` |
| `evaluateScriptProfile(script)` | no replacement — `evaluator` does not profile; keep using this, or profile from the JVM |
| `evalPlutusScripts(tx, utxoMap, slotConfig, [v1, v2, v3])` | `evaluator.evaluateTx(tx, utxoPairs, slotConfig, { PlutusV1: v1, PlutusV2: v2, PlutusV3: v3 }, 11)` |

### Evaluate All Scripts in a Transaction

`evaluator.evaluateTx` takes what a transaction builder already holds: the transaction, its
resolved inputs as CBOR `[input, output]` pairs, the slot configuration, and the cost models by
language.

```typescript
import { evaluator } from "scalus";

const budgets = evaluator.evaluateTx(
  txCborHex,                           // string | Uint8Array
  utxos,                               // (string | Uint8Array)[]: one [input, output] pair each
  { zeroTime, zeroSlot, slotLength },  // zeroTime and zeroSlot may be number or bigint
  { PlutusV2: plutusV2Costs, PlutusV3: plutusV3Costs },
  11,                                  // protocol major version
);

for (const r of budgets) {
  console.log(`${r.tag}[${r.index}]: ${r.budget.memory} mem, ${r.budget.steps} steps`);
}
```

A pair is `transaction_unspent_output` from the ledger CDDL. CML's
`TransactionUnspentOutput.to_cbor_bytes()`, CST's `TransactionUnspentOutput.toCbor()` and a CIP-30
wallet's `getUtxos()` all produce it, so nothing needs assembling. A later pair with the same input
replaces an earlier one. The slot configuration is a `SlotConfigLike` and the cost models a
`CostModelsLike`: a `SlotConfig` or a `CostModels` fits, and so does any plain object with those
fields. Extra fields are ignored.

Each `r.tag` is one of `"Spend"`, `"Mint"`, `"Cert"`, `"Reward"`, `"Voting"` or `"Proposing"`,
and `r.index` is the position within that group, counting from 0. An SDK adapter maps those six
names to its own spelling; that table is most of the adapter.

The deprecated `evalPlutusScripts` still works for a caller that holds the UTxO set as one CBOR
map:

```typescript
import { evalPlutusScripts, SlotConfig } from "scalus";

// One cost model per Plutus version, indexed by position: [0] is V1, [1] is V2, [2] is V3.
// Give a model for every version the transaction uses; an earlier version cannot be skipped.
const costModels = [plutusV1Costs, plutusV2Costs, plutusV3Costs];
const redeemers = evalPlutusScripts(txCborBytes, utxoMapCborBytes, SlotConfig.mainnet, costModels);
```

#### When a script fails

Both throw `PlutusScriptEvaluationError`, which extends `Error`. Its message names the redeemer,
the script, the units spent and the traces, so a caller that only prints the message still learns
what failed:

```
Spend[0] failed: Error evaluated
script: 8a8c37e9775e3369118ef4946ce1534ca64690382825207d05c72b17
spent budget: { mem: 1132, steps: 219598 }
logs:
boom
```

The same facts are fields:

```typescript
try {
  evaluator.evaluateTx(tx, utxos, slotConfig, costModels, 11);
} catch (e) {
  if (e instanceof PlutusScriptEvaluationError) {
    e.redeemer;    // { tag: "Spend", index: 0, budget: { memory, steps } }, spent before failing
    e.scriptHash;  // hex
    e.code;        // "SCRIPT_FAILURE" | "BUILTIN_FAILURE" | "INVALID_RETURN_VALUE" | "INTERNAL_ERROR"
    e.logs;        // the script's traces
    e.args;        // the Data arguments it was applied to, each as CBOR hex, in order
  }
}
```

`args` is a property like the others, but not an enumerable one, so `console.log(e)` and
`JSON.stringify(e)` stay readable instead of printing a screenful of script-context hex. Decode it
when you want to see what the script saw; `scriptHash` says which script saw it, and you already
hold that script, in the transaction or in the UTxO you passed.

Anything else that stops evaluation, such as an input no pair resolves or a script the transaction
does not carry, throws a plain `Error`; input that cannot be read throws `TypeError`.

In TypeScript the six fields are declared optional, because the two-argument constructor
`new PlutusScriptEvaluationError(message, logs)` leaves them `undefined`. An error thrown by
`evaluateTx` always carries them, so after the `instanceof` check `e.redeemer!` is safe.

### Plutus Data JSON Format

The deprecated `applyDataArgToScript` takes its argument in the standard Plutus Data JSON
encoding. `uplc.applyParamsToScript` takes CBOR instead; convert before calling it.

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

The same bundle runs in Node 18+ and browsers without a Node crypto polyfill. Optional
filesystem operations require a Node CommonJS host; evaluation, hashing, signature
verification and the Emulator do not require filesystem access.

Run `npm run test:browser` for the automated Chromium regression check used in CI.
It runs the shared evaluator tests and a typed credential query against the bundle.
See [BROWSER-TESTING.md](BROWSER-TESTING.md) for browser setup and interactive tests.

## TypeScript Definitions

`scalus.d.ts` is generated from the Scala sources by `scalus-ts-exporter`
(`sbt scalusCardanoLedgerJS/generateDts`). Do not edit it by hand. CI fails
if it drifts from the Scala facades.

## License

Apache-2.0

## Links

- [Repository](https://github.com/scalus3/scalus)
- [Documentation](https://scalus.org)
