// __tests__/differential.test.ts
//
// Scalus and the Rust/wasm evaluator lucid-evolution ships by default must agree, script by
// script, on the execution budget.
//
// The plutus-conformance corpus already proves Scalus's CEK machine against the reference
// implementation, including exact budgets — but it evaluates bare UPLC terms. It never touches the
// layer an evaluator adapter actually drives: script-context construction, redeemer indexing,
// per-language cost-model selection, the protocol-version switch. That is the only layer where two
// conformant implementations can still disagree, and it is exactly the layer a transaction builder
// depends on.
//
// The calling convention below is copied from lucid-evolution's own `makeDefaultAikenEvaluator`
// (@lucid-evolution/lucid 0.6.2, dist/index.js): inputs and outputs as two parallel CBOR arrays,
// cost models as the ledger's `cost_models` CBOR map, then steps, mem, zeroTime, zeroSlot,
// slotLength.

import { describe, test, expect } from "vitest";
import { Decoder, Encoder } from "cbor-x";
import { eval_phase_two_raw } from "@lucid-evolution/uplc";
import { evalPlutusScripts, SlotConfig } from "../scalus";
import {
  hexToBytes,
  costModels,
  scriptTxCborHex,
  scriptUtxoCborHex,
} from "./fixtures";

const decoder = new Decoder({ mapsAsObjects: false });
// `tagUint8Array: false` matters: by default cbor-x writes a Uint8Array with a typed-array tag
// rather than as a plain CBOR byte string, and the wasm evaluator rejects that with
// "expected bytes (definite length)".
const encoder = new Encoder({ tagUint8Array: false });

/** Split a CBOR map of input -> output into the two parallel arrays the wasm evaluator wants. */
function splitUtxoMap(utxoMapCbor: Uint8Array): {
  inputs: Uint8Array[];
  outputs: Uint8Array[];
} {
  const decoded = decoder.decode(utxoMapCbor) as Map<unknown, unknown>;
  const inputs: Uint8Array[] = [];
  const outputs: Uint8Array[] = [];
  for (const [input, output] of decoded) {
    inputs.push(new Uint8Array(encoder.encode(input)));
    outputs.push(new Uint8Array(encoder.encode(output)));
  }
  return { inputs, outputs };
}

/** The ledger's `cost_models` map: language id -> cost array. */
function costModelsCbor(): Uint8Array {
  return new Uint8Array(
    encoder.encode(
      new Map<number, number[]>([
        [0, costModels.PlutusV1],
        [1, costModels.PlutusV2],
        [2, costModels.PlutusV3],
      ]),
    ),
  );
}

type Budget = { tag: number; index: number; mem: bigint; steps: bigint };

/** A legacy redeemer is CBOR `[tag, index, data, [mem, steps]]`. */
function decodeLegacyRedeemer(bytes: Uint8Array): Budget {
  const [tag, index, , [mem, steps]] = decoder.decode(bytes) as [
    number,
    number,
    unknown,
    [bigint | number, bigint | number],
  ];
  return { tag, index, mem: BigInt(mem), steps: BigInt(steps) };
}

const SCALUS_TAG_ORDER: Record<string, number> = {
  Spend: 0,
  Mint: 1,
  Cert: 2,
  Reward: 3,
  Voting: 4,
  Proposing: 5,
};

describe("Scalus vs @lucid-evolution/uplc", () => {
  test("agree on every redeemer's execution budget", () => {
    const txBytes = hexToBytes(scriptTxCborHex);
    const utxoBytes = hexToBytes(scriptUtxoCborHex);
    const slot = SlotConfig.mainnet;

    const scalusRedeemers = evalPlutusScripts(txBytes, utxoBytes, slot, [
      costModels.PlutusV1,
      costModels.PlutusV2,
      costModels.PlutusV3,
    ]);

    const { inputs, outputs } = splitUtxoMap(utxoBytes);
    const wasmRedeemers = eval_phase_two_raw(
      txBytes,
      inputs,
      outputs,
      costModelsCbor(),
      10_000_000_000n, // initial_budget_n: max tx ex steps
      14_000_000n, //     initial_budget_d: max tx ex mem
      BigInt(slot.zeroTime),
      BigInt(slot.zeroSlot),
      slot.slotLength,
    );

    expect(scalusRedeemers.length).toBe(wasmRedeemers.length);
    expect(scalusRedeemers.length).toBeGreaterThan(0);

    const byKey = (a: Budget, b: Budget) => a.tag - b.tag || a.index - b.index;

    const fromWasm = wasmRedeemers.map(decodeLegacyRedeemer).sort(byKey);
    const fromScalus = scalusRedeemers
      .map((r) => ({
        tag: SCALUS_TAG_ORDER[r.tag],
        index: r.index,
        mem: r.budget.memory,
        steps: r.budget.steps,
      }))
      .sort(byKey);

    for (let i = 0; i < fromScalus.length; i++) {
      const s = fromScalus[i]!;
      const w = fromWasm[i]!;
      expect(s.tag, `redeemer ${i} tag`).toBe(w.tag);
      expect(s.index, `redeemer ${i} index`).toBe(w.index);
      expect(s.mem, `redeemer ${i} memory`).toBe(w.mem);
      expect(s.steps, `redeemer ${i} steps`).toBe(w.steps);
    }
  });
});
