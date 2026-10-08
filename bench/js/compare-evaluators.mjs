// Compares transaction evaluators on recorded `eval_phase_two_raw` requests.
//
// Usage:
//   node bench/js/compare-evaluators.mjs --dump <dir or sample> \
//        --engine aiken=/tmp/npmpkgs/lucid-evolution-uplc-0.2.23/package \
//        --engine scalus-0.18.1=/tmp/npmpkgs/scalus-0.18.1/package/scalus.js \
//        --engine master=scalus-cardano-ledger/js/src/main/npm/scalus.js \
//        [--limit 50] [--iters 3] [--top 20]
//
// Each dump file is one JSON request (hex byte fields) plus Aiken's recorded result.
// For every request and engine: one warm-up run, then `iters` timed runs; reports the
// median, and checks that every redeemer's ExUnits equal the recorded Aiken result.
import fs from "node:fs";
import path from "node:path";
import crypto from "node:crypto";
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import { readRequests } from "./requests.mjs";

const args = process.argv.slice(2);
const opt = (name, dflt) => {
    const i = args.indexOf(`--${name}`);
    return i >= 0 ? args[i + 1] : dflt;
};
const engineSpecs = args.flatMap((a, i) => (a === "--engine" ? [args[i + 1]] : []));
const dumpDir = opt("dump", "deep-deposit");
const limit = Number(opt("limit", "1000000"));
const iters = Number(opt("iters", "3"));
const top = Number(opt("top", "0"));

const hex = (s) => Uint8Array.from(Buffer.from(s, "hex"));

// Minimal CBOR decoder: enough for cost models and legacy redeemers.
function cborDecode(bytes) {
    let p = 0;
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    const arg = (info) => {
        if (info < 24) return BigInt(info);
        if (info === 24) return BigInt(bytes[p++]);
        if (info === 25) return BigInt(view.getUint16((p += 2) - 2));
        if (info === 26) return BigInt(view.getUint32((p += 4) - 4));
        if (info === 27) return view.getBigUint64((p += 8) - 8);
        throw new Error(`bad cbor info ${info}`);
    };
    const item = () => {
        const b = bytes[p++];
        const major = b >> 5;
        const info = b & 31;
        if (info === 31) {
            // indefinite length
            const out = [];
            while (bytes[p] !== 0xff) out.push(item());
            p++;
            if (major === 2) return Uint8Array.from(out.flatMap((c) => [...c]));
            if (major === 5) return new Map(Array.from({ length: out.length / 2 }, (_, i) => [out[2 * i], out[2 * i + 1]]));
            return out;
        }
        const n = major === 7 ? 0n : arg(info);
        switch (major) {
            case 0: return n;
            case 1: return -1n - n;
            case 2: return bytes.slice(p, (p += Number(n)));
            case 3: return Buffer.from(bytes.slice(p, (p += Number(n)))).toString("utf8");
            case 4: return Array.from({ length: Number(n) }, item);
            case 5: {
                const m = new Map();
                for (let i = 0; i < Number(n); i++) m.set(item(), item());
                return m;
            }
            case 6: return { tag: n, value: item() };
            default: return info;
        }
    };
    return item();
}

const encodeUint = (major, n) => {
    if (n < 24) return [(major << 5) | n];
    if (n < 256) return [(major << 5) | 24, n];
    if (n < 65536) return [(major << 5) | 25, n >> 8, n & 255];
    return [(major << 5) | 26, (n >>> 24) & 255, (n >> 16) & 255, (n >> 8) & 255, n & 255];
};
const compareBytes = (a, b) => {
    for (let i = 0; i < Math.min(a.length, b.length); i++) if (a[i] !== b[i]) return a[i] - b[i];
    return a.length - b.length;
};
// Same encoding as @lucid-evolution/scalus-uplc's buildUtxoMapCbor.
const utxoMapCbor = (inputs, outputs) => {
    const pairs = inputs.map((input, i) => ({ input, output: outputs[i] })).sort((a, b) => compareBytes(a.input, b.input));
    const parts = [Uint8Array.from(encodeUint(5, pairs.length)), ...pairs.flatMap(({ input, output }) => [input, output])];
    const out = new Uint8Array(parts.reduce((s, x) => s + x.length, 0));
    let o = 0;
    for (const x of parts) out.set(x, (o += x.length) - x.length);
    return out;
};

const TAGS = ["spend", "mint", "cert", "reward", "vote", "propose"];
// Aiken result: legacy redeemer [tag, index, data, [mem, steps]].
const aikenUnits = (redeemersHex) =>
    redeemersHex
        .map((h) => {
            const [tag, index, , [mem, steps]] = cborDecode(hex(h));
            return `${TAGS[Number(tag)]}:${index}=${mem}/${steps}`;
        })
        .sort();

function loadRequests() {
    const seen = new Set();
    const reqs = [];
    for (const { name: f, d } of readRequests(dumpDir)) {
        const key = crypto.createHash("sha256").update(d.tx + d.inputs.join() + d.outputs.join() + d.costModels).digest("hex");
        if (seen.has(key)) continue;
        seen.add(key);
        const costModels = cborDecode(hex(d.costModels));
        const cmArrays = [0n, 1n, 2n].map((lang) => (costModels.get(lang) ?? []).map(Number));
        reqs.push({
            file: f,
            recordedMs: d.ms,
            txBytes: hex(d.tx),
            inputs: d.inputs.map(hex),
            outputs: d.outputs.map(hex),
            costModelsBytes: hex(d.costModels),
            cmArrays,
            d,
            expected: d.error ? ["FAIL"] : aikenUnits(d.redeemers),
        });
    }
    return reqs;
}

async function loadEngine(spec) {
    const [name, location] = spec.split("=");
    const abs = path.resolve(location);
    if (name.startsWith("aiken")) {
        const UPLC = createRequire(import.meta.url)(abs);
        return {
            name,
            run(r) {
                const d = r.d;
                const out = UPLC.eval_phase_two_raw(
                    r.txBytes, r.inputs, r.outputs, r.costModelsBytes,
                    BigInt(d.maxSteps), BigInt(d.maxMemory), BigInt(d.zeroTime), BigInt(d.zeroSlot), d.slotLength,
                );
                return aikenUnits(Array.from(out, (b) => Buffer.from(b).toString("hex")));
            },
        };
    }
    const lib = abs.endsWith(".mjs") || isEsm(abs) ? await import(pathToFileURL(abs).href) : createRequire(import.meta.url)(abs);
    if (name.startsWith("txeval")) {
        // TxEvaluator, one per (cost models, slot configuration, budget), as an SDK keeps it.
        const evaluators = new Map();
        return {
            name,
            run(r) {
                const d = r.d;
                const key = [d.costModels, d.zeroTime, d.zeroSlot, d.slotLength, d.maxMemory, d.maxSteps].join("|");
                let ev = evaluators.get(key);
                if (!ev) evaluators.set(key, ev = new lib.TxEvaluator({
                    slotConfig: { zeroTime: BigInt(d.zeroTime), zeroSlot: BigInt(d.zeroSlot), slotLength: d.slotLength },
                    costModels: { PlutusV1: r.cmArrays[0], PlutusV2: r.cmArrays[1], PlutusV3: r.cmArrays[2] },
                    protocolMajorVersion: 11,
                    maxBudget: { memory: BigInt(d.maxMemory), steps: BigInt(d.maxSteps) },
                }));
                r.pairs ??= r.inputs.map((a, i) => {
                    const b = r.outputs[i], p = new Uint8Array(1 + a.length + b.length);
                    p[0] = 0x82; p.set(a, 1); p.set(b, 1 + a.length);
                    return p;
                });
                const res = ev.evaluate(r.txBytes, r.pairs);
                return Array.from(res, (x) => `${x.tag.toLowerCase()}:${x.index}=${x.budget.memory}/${x.budget.steps}`).sort();
            },
        };
    }
    return {
        name,
        run(r) {
            const d = r.d;
            const slotConfig = new lib.SlotConfig(Number(d.zeroTime), Number(d.zeroSlot), d.slotLength);
            const utxos = utxoMapCbor(r.inputs, r.outputs);
            // Same protocol version inference as @lucid-evolution/scalus-uplc.
            const pv = r.cmArrays[2].length >= 350 ? 11 : 10;
            const res = process.env.PV_OMIT
                ? lib.Scalus.evalPlutusScripts(r.txBytes, utxos, slotConfig, r.cmArrays)
                : lib.Scalus.evalPlutusScripts(r.txBytes, utxos, slotConfig, r.cmArrays, pv);
            return Array.from(res, (x) => `${x.tag.toLowerCase()}:${x.index}=${x.budget.memory}/${x.budget.steps}`).sort();
        },
    };
}

function isEsm(file) {
    const src = fs.readFileSync(file, "utf8");
    return /\bexport\s*\{/.test(src.slice(-4000));
}

const median = (xs) => [...xs].sort((a, b) => a - b)[Math.floor(xs.length / 2)];

const reqs = loadRequests().slice(0, limit);
const failing = reqs.filter((r) => r.expected[0] === "FAIL").length;
const engines = [];
for (const s of engineSpecs) engines.push(await loadEngine(s));
console.log(`${reqs.length} unique requests (${failing} failing), engines: ${engines.map((e) => e.name).join(", ")}, iters=${iters}`);

const totals = Object.fromEntries(engines.map((e) => [e.name, 0]));
const mismatches = Object.fromEntries(engines.map((e) => [e.name, 0]));
const failures = Object.fromEntries(engines.map((e) => [e.name, 0]));
const rows = [];
for (const r of reqs) {
    const row = { file: r.file };
    for (const e of engines) {
        // A failed evaluation is an outcome too: it must fail on every engine, and it is timed.
        const attempt = () => {
            try {
                return e.run(r);
            } catch (err) {
                if (r.expected[0] !== "FAIL" && failures[e.name]++ < 3)
                    console.log(`${e.name} failed on ${r.file}: ${String(err?.message ?? err).slice(0, 300)}`);
                return ["FAIL"];
            }
        };
        const got = attempt(); // warm-up + correctness
        if (got.join() !== r.expected.join()) {
            mismatches[e.name]++;
            if (mismatches[e.name] <= 3) console.log(`${e.name} ExUnits differ on ${r.file}:\n  aiken ${r.expected.join(" ")}\n  ${e.name} ${got.join(" ")}`);
        }
        const times = [];
        for (let i = 0; i < iters; i++) {
            const t0 = performance.now();
            attempt();
            times.push(performance.now() - t0);
        }
        row[e.name] = median(times);
        totals[e.name] += row[e.name];
    }
    rows.push(row);
}

if (top > 0) {
    const key = engines[0].name;
    for (const row of [...rows].sort((a, b) => b[key] - a[key]).slice(0, top))
        console.log(row.file, engines.map((e) => `${e.name}=${row[e.name].toFixed(1)}ms`).join(" "));
}
const base = totals[engines[0].name];
for (const e of engines)
    console.log(
        `${e.name.padEnd(16)} total ${(totals[e.name] / 1000).toFixed(2)}s  x${(totals[e.name] / base).toFixed(2)}  ` +
            `mismatches ${mismatches[e.name]}  failures ${failures[e.name]}`,
    );
