// The JavaScript side of MidgardReplayBenchmark.evaluateFromCbor: TxEvaluator.evaluate on the
// recorded requests, one evaluator per (cost models, slot configuration, budget).
// Prints the median of 3 timed passes after `warmup` passes. `--results` writes each request's
// redeemer budgets in the format the JVM benchmark writes with -Dscalus.bench.results, and
// `--cache` gives the evaluators a shared ScriptCache of that size.
// Usage: node bench/js/replay.mjs <scalus.js> <dir or sample> [--limit N] [--warmup N] [--results <file>] [--cache <size>]
import fs from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { readRequests } from "./requests.mjs";

const [lib, dir, ...rest] = process.argv.slice(2);
const opt = (name, dflt) => { const i = rest.indexOf(name); return i >= 0 ? rest[i + 1] : dflt; };
const S = await import(pathToFileURL(path.resolve(lib)).href);
const hex = (s) => Uint8Array.from(Buffer.from(s, "hex"));

// Minimal CBOR reader for the cost-model map { language: [int...] }.
function decodeCostModels(bytes) {
    let p = 0;
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    const arg = (info) => info < 24 ? info : info === 24 ? bytes[p++] : info === 25 ? view.getUint16((p += 2) - 2)
        : info === 26 ? view.getUint32((p += 4) - 4) : Number(view.getBigUint64((p += 8) - 8));
    const item = () => {
        const b = bytes[p++], major = b >> 5, n = arg(b & 31);
        if (major === 0) return n;
        if (major === 1) return -1 - n;
        if (major === 4) return Array.from({ length: n }, item);
        if (major === 5) return Object.fromEntries(Array.from({ length: n }, () => [item(), item()]));
        throw new Error(`unexpected CBOR major type ${major}`);
    };
    const m = item();
    return { PlutusV1: m[0], PlutusV2: m[1], PlutusV3: m[2] };
}

const evaluators = new Map();
const cacheSize = Number(opt("--cache", "0"));
const scriptCache = cacheSize > 0 ? new S.ScriptCache(cacheSize) : undefined;
const requests = readRequests(dir, Number(opt("--limit", "Infinity"))).map(({ name, d }) => {
    const key = [d.costModels, d.zeroTime, d.zeroSlot, d.slotLength, d.maxMemory, d.maxSteps].join("|");
    if (!evaluators.has(key)) evaluators.set(key, new S.TxEvaluator({
        slotConfig: { zeroTime: BigInt(d.zeroTime), zeroSlot: BigInt(d.zeroSlot), slotLength: d.slotLength },
        costModels: decodeCostModels(hex(d.costModels)),
        protocolMajorVersion: 11,
        maxBudget: { memory: BigInt(d.maxMemory), steps: BigInt(d.maxSteps) },
        scriptCache,
    }));
    const pairs = d.inputs.map((input, i) => {
        const a = hex(input), b = hex(d.outputs[i]);
        const pair = new Uint8Array(1 + a.length + b.length);
        pair[0] = 0x82; pair.set(a, 1); pair.set(b, 1 + a.length);
        return pair;
    });
    return { name, evaluator: evaluators.get(key), tx: hex(d.tx), pairs };
});

const result = (r) => {
    try {
        return r.evaluator.evaluate(r.tx, r.pairs)
            .map((x) => `${x.tag}:${x.index}:${x.budget.memory}:${x.budget.steps}`).sort().join(" ");
    } catch (e) {
        if (e instanceof S.PlutusScriptEvaluationError) return "SCRIPT_FAILURE";
        throw e;
    }
};
const pass = () => { const t0 = performance.now(); for (const r of requests) result(r); return performance.now() - t0; };

const resultsFile = opt("--results");
if (resultsFile) fs.writeFileSync(resultsFile, requests.map((r) => `${r.name} ${result(r)}\n`).join(""));
for (let i = 0; i < Number(opt("--warmup", "1")); i++) pass();
const times = [pass(), pass(), pass()].sort((a, b) => a - b);
console.log(`requests=${requests.length} evaluators=${evaluators.size} median=${times[1].toFixed(0)} ms passes=${times.map((t) => t.toFixed(0)).join(",")}`);
