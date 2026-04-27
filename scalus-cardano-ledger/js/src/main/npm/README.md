# Scalus - Cardano Toolkit for JavaScript

Scalus brings a near-complete Cardano node emulator, Plutus script evaluator, and transaction
tooling to JavaScript and TypeScript. It is compiled from the JVM Scalus codebase via Scala.jsю

The core features include a **Node Emulator** -- a local, in-process implementation of a Cardano node, complete with
most of the ledger rules to validate incoming transactions, as well ledger state transitions.

## Installation

```bash
npm install scalus
```

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

Every UTxO returned by the emulator is a CBOR-encoded `Map[TransactionInput, TransactionOutput]`
entry. Decode it using your favourite CBOR codec library.

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
    utxos: utxoMapCbor,           // Uint8Array — CBOR Map[TxIn, TxOut]
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
import { Scalus } from "scalus";

// Scripts are represented as double-CBOR-encoded hex strings
const script = "545301010023357389210753756363657373004981";

// Apply a data argument (Plutus Data JSON format)
const applied = Scalus.applyDataArgToScript(script, JSON.stringify({ int: 42 }));

const result = Scalus.evaluateScript(applied);
// { isSuccess: true, budget: { memory: 1032n, steps: 203598n }, logs: [] }
```

### Evaluate All Scripts in a Transaction

```typescript
import { Scalus, SlotConfig } from "scalus";

const costModels = [plutusV1Costs, plutusV2Costs, plutusV3Costs]; // number[][]

const redeemers = Scalus.evalPlutusScripts(
  txCborBytes,        // Uint8Array
  utxoCborBytes,      // Uint8Array — CBOR Map[TxIn, TxOut]
  SlotConfig.mainnet,
  costModels
);

for (const r of redeemers) {
  console.log(`${r.tag}[${r.index}]: ${r.budget.memory} mem, ${r.budget.steps} steps`);
}
```

On failure, `evalPlutusScripts` throws a `PlutusScriptEvaluationException` with
`.message` and `.logs` (the script's trace output).

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
const slot = cfg.timeToSlot(time);

// Custom config
const custom = new SlotConfig(zeroTime, zeroSlot, slotLength);
```

## Browser Usage

The bundle works in browsers with a CommonJS shim:

```html

<script>
    var module = { exports: {} };
    var exports = module.exports;
</script>
<script src="scalus.js"></script>
<script>
    const { Scalus, SlotConfig, Emulator } = module.exports;
</script>
```

## License

Apache-2.0

## Links

- [Repository](https://github.com/nau/scalus)
- [Documentation](https://scalus.org)
