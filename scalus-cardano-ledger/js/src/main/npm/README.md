# Scalus - Cardano Plutus Script Evaluator for JavaScript

Scalus provides a JavaScript/TypeScript interface for evaluating Cardano Plutus scripts and calculating execution budgets.

## Installation

```bash
npm install scalus
```

## Usage

### Basic Script Evaluation

```javascript
const { Scalus } = require('scalus');

// A simple Plutus script (double-CBOR-encoded hex)
const script = "545301010023357389210753756363657373004981";

// Apply a data argument to the script
const applied = Scalus.applyDataArgToScript(script, JSON.stringify({"int": 42}));

// Evaluate the script
const result = Scalus.evaluateScript(applied);

console.log(result);
// { isSuccess: true, budget: { memory: 1032n, steps: 203598n }, logs: [...] }

if (result.isSuccess) {
    console.log('Memory:', result.budget.memory);
    console.log('Steps:', result.budget.steps);
} else {
    console.log('Script failed. Logs:', result.logs);
}
```

### Data JSON Format

When using `applyDataArgToScript`, the data argument must be a JSON string in Plutus Data format:

```javascript
// Integer
{"int": 42}

// Bytes (hex-encoded)
{"bytes": "deadbeef"}

// List
{"list": [{"int": 1}, {"int": 2}, {"int": 3}]}

// Map (array of key-value objects)
{"map": [{"k": {"int": 1}, "v": {"bytes": "aa"}}]}

// Constructor (for sum types)
{"constructor": 0, "fields": [{"int": 42}]}

// Nested structure
{"constructor": 0, "fields": [
    {"list": [{"int": 1}, {"int": 2}]},
    {"map": [{"k": {"bytes": "abcd"}, "v": {"int": 100}}]}
]}
```

### Evaluate Transaction Scripts

Evaluate all Plutus scripts in a transaction against a UTxO set:

```javascript
const { Scalus, SlotConfig } = require('scalus');
const fs = require('fs');

// Load protocol parameters (e.g., from Blockfrost)
const protocolParams = JSON.parse(fs.readFileSync("protocol-params.json", "utf8"));

// Extract cost models for each Plutus version
const costModels = [
    protocolParams.cost_models_raw.PlutusV1,
    protocolParams.cost_models_raw.PlutusV2,
    protocolParams.cost_models_raw.PlutusV3
];

// Load transaction and UTxO CBOR bytes
const txBytes = new Uint8Array(fs.readFileSync("transaction.cbor"));
const utxoBytes = new Uint8Array(fs.readFileSync("utxo.cbor"));

// Use predefined slot configurations
const slotConfig = SlotConfig.mainnet;  // or SlotConfig.preview, SlotConfig.preprod

// Evaluate all Plutus scripts in the transaction
const redeemers = Scalus.evalPlutusScripts(txBytes, utxoBytes, slotConfig, costModels);

redeemers.forEach(redeemer => {
    console.log(`${redeemer.tag}[${redeemer.index}]:`);
    console.log('  Memory:', redeemer.budget.memory);
    console.log('  Steps:', redeemer.budget.steps);
});
```

### Handling Script Failures

When a Plutus script fails, `evalPlutusScripts` throws a `PlutusScriptEvaluationException` with the error message and script logs:

```javascript
try {
    const redeemers = Scalus.evalPlutusScripts(txBytes, utxoBytes, slotConfig, costModels);
} catch (error) {
    console.log("Error message:", error.message);
    console.log("Script logs:", error.logs);
}
```

### Slot Configuration

Use predefined slot configurations or create custom ones:

```javascript
const { SlotConfig } = require('scalus');

// Predefined configurations
const mainnet = SlotConfig.mainnet;
const preview = SlotConfig.preview;
const preprod = SlotConfig.preprod;

// Convert between slots and POSIX time
const time = mainnet.slotToTime(100000);  // slot -> milliseconds
const slot = mainnet.timeToSlot(time);    // milliseconds -> slot
```

## API Reference

### `Scalus.evaluateScript(doubleCborHex: string): Result`

Evaluates a Plutus script from its double-CBOR-encoded hex representation.

**Returns:** A `Result` object:
- `isSuccess`: boolean indicating success or failure
- `budget`: `ExUnits` with `memory` and `steps` (as BigInt)
- `logs`: Array of log messages

### `Scalus.applyDataArgToScript(doubleCborHex: string, dataJson: string): string`

Applies a data argument to a Plutus script.

**Parameters:**
- `doubleCborHex`: The double-CBOR-encoded hex string of the Plutus script
- `dataJson`: JSON representation of the Data argument (see format above)

**Returns:** The double-CBOR-encoded hex string of the script with the argument applied

### `Scalus.evalPlutusScripts(txCborBytes, utxoCborBytes, slotConfig, costModels): Redeemer[]`

Evaluates all Plutus scripts in a transaction.

**Parameters:**
- `txCborBytes`: `Uint8Array` - CBOR bytes of the transaction
- `utxoCborBytes`: `Uint8Array` - CBOR bytes of the UTxO map
- `slotConfig`: `SlotConfig` - Slot configuration for time conversions
- `costModels`: `number[][]` - Cost models array `[PlutusV1, PlutusV2, PlutusV3]`

**Returns:** Array of `Redeemer` objects:
- `tag`: Redeemer tag (e.g., "Spend", "Mint")
- `index`: Redeemer index
- `budget`: `ExUnits` with computed execution costs

**Throws:** `PlutusScriptEvaluationException` with `.message` and `.logs` on script failure

### `SlotConfig`

Slot configuration for time conversions.

**Static properties:**
- `SlotConfig.mainnet` - Mainnet configuration (Shelley era)
- `SlotConfig.preview` - Preview testnet configuration
- `SlotConfig.preprod` - Preprod testnet configuration

**Methods:**
- `slotToTime(slot: number): number` - Convert slot to POSIX time (milliseconds)
- `timeToSlot(time: number): number` - Convert POSIX time to slot

## Browser Usage

The bundle works in browsers with a CommonJS shim:

```html
<script>
    var module = { exports: {} };
    var exports = module.exports;
</script>
<script src="scalus.js"></script>
<script>
    const Scalus = module.exports.Scalus;
    const SlotConfig = module.exports.SlotConfig;

    // Use the API...
</script>
```

## License

Apache-2.0

## Repository

https://github.com/nau/scalus
