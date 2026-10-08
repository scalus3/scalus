// Reads recorded evaluator requests: one JSON file, optionally gzipped, per `eval_phase_two_raw`
// call. `dirOrSample` is a directory of them, or the name of a sample bundled with the JVM
// benchmark: deep-deposit, mint-authorization or value-conservation.
import fs from "node:fs";
import path from "node:path";
import zlib from "node:zlib";
import { fileURLToPath } from "node:url";

const samples = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../src/main/resources/midgard");

export function readRequests(dirOrSample, limit = Infinity) {
    const dir = fs.existsSync(dirOrSample) ? dirOrSample : path.join(samples, dirOrSample);
    if (!fs.existsSync(dir)) throw new Error(`${dirOrSample} is neither a directory nor a bundled sample`);
    const files = fs.readdirSync(dir).filter((f) => /\.json(\.gz)?$/.test(f)).sort().slice(0, limit);
    if (files.length === 0) throw new Error(`no .json or .json.gz requests in ${dir}`);
    return files.map((name) => {
        const raw = fs.readFileSync(path.join(dir, name));
        const json = name.endsWith(".gz") ? zlib.gunzipSync(raw).toString("utf8") : raw.toString("utf8");
        return { name, d: JSON.parse(json) };
    });
}
