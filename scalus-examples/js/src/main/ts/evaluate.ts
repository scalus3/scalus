import {readFileSync} from "node:fs";
// Import directly from the local build bundle to use the latest version
const {
    Scalus,
    SlotConfig
} = require("../../../../../scalus-cardano-ledger/js/src/main/npm/scalus.js");

const script = "545301010023357389210753756363657373004981";
const applied = Scalus.applyDataArgToScript(script, JSON.stringify({"int": 42}));
const result = Scalus.evaluateScript(applied);
console.log(result);
console.log(result.budget.memory);

const protocolParams = JSON.parse(readFileSync("blockfrost-params-epoch-544.json", "utf8"));

const costModels = [
    protocolParams.cost_models_raw.PlutusV1,
    protocolParams.cost_models_raw.PlutusV2,
    protocolParams.cost_models_raw.PlutusV3
];

// Read CBOR files
const txBytes = Array.from(readFileSync("tx-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"));
const utxoBytes = Array.from(readFileSync("utxo-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"));
const slotConfig = SlotConfig.Mainnet;

// Evaluate all Plutus scripts in the transaction
console.log(Scalus.evalPlutusScripts(txBytes, utxoBytes, slotConfig, costModels));

// A transaction that includes an always-fail script.
const failingTx = Array.from(readFileSync("tx-failing-with-logs.cbor"));
const failingUtxo = Array.from(readFileSync("utxo-failing-with-logs.cbor"));

try {
    const failingRedeemers = Scalus.evalPlutusScripts(failingTx, failingUtxo, slotConfig, costModels);
} catch (error) {
    console.log("Error message:", error.message);
    console.log("Logs:", error.logs);
}
