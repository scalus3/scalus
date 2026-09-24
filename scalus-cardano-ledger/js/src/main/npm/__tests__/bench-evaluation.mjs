// __tests__/bench-evaluation.mjs
// A manual benchmark, not a test: run it by hand with `node __tests__/bench-evaluation.mjs`.
// It is deliberately outside Vitest's include pattern, because wall-clock numbers do not belong
// in a pass/fail gate.
//
// What it answers: how much of a repeated evaluation is configuration rather than execution.
// It measures, it does not fix. Any case it makes for caching gets its own reviewed change.

import { performance } from "node:perf_hooks";
import { readFileSync } from "node:fs";
import { createHash } from "node:crypto";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";
import { evaluator, EvaluationOptions } from "../scalus.js";

const here = dirname(fileURLToPath(import.meta.url));
const bundlePath = resolve(here, "../scalus.js");
const bundle = readFileSync(bundlePath);

const script = "545301010023357389210753756363657373004981";
const args = ["182a"];
const WARMUP = 50;
const ITERATIONS = 1000;

/** Runs `body` `ITERATIONS` times after `WARMUP`, and fails loudly rather than timing errors. */
function measure(label, body) {
  for (let i = 0; i < WARMUP; i++) body();
  const start = performance.now();
  for (let i = 0; i < ITERATIONS; i++) {
    const result = body();
    if (!result.isSuccess) throw new Error(`${label}: ${result.error?.message}`);
  }
  const elapsedMs = performance.now() - start;
  return { label, iterations: ITERATIONS, elapsedMs, msPerCall: elapsedMs / ITERATIONS };
}

const reused = EvaluationOptions.mainnet("PlutusV3");

// Reused options versus fresh ones: the difference is what building the configuration costs.
const withReusedOptions = measure("reused options", () =>
  evaluator.evaluateScript(script, args, reused),
);
const withFreshOptions = measure("fresh options per call", () =>
  evaluator.evaluateScript(script, args, EvaluationOptions.mainnet("PlutusV3")),
);

// Repeated calls must not accumulate state. Same budget, no leaked logs.
const first = evaluator.evaluateScript(script, args, reused);
const second = evaluator.evaluateScript(script, args, reused);

const isolation = {
  sameBudget: first.budget.memory === second.budget.memory && first.budget.steps === second.budget.steps,
  sameLogs: JSON.stringify(first.logs) === JSON.stringify(second.logs),
  neverProfiles: first.profileJson === undefined && second.profileJson === undefined,
};

console.log({
  node: process.version,
  bundle: {
    path: bundlePath,
    bytes: bundle.byteLength,
    sha256: createHash("sha256").update(bundle).digest("hex").slice(0, 16),
  },
  warmup: WARMUP,
  measurements: [withReusedOptions, withFreshOptions],
  configurationCostMsPerCall: withFreshOptions.msPerCall - withReusedOptions.msPerCall,
  isolation,
});

if (Object.values(isolation).some((ok) => !ok)) {
  throw new Error(`repeated evaluation shares state: ${JSON.stringify(isolation)}`);
}
