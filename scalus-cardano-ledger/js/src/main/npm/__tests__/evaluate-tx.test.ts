// __tests__/evaluate-tx.test.ts
//
// evaluator.evaluateTx from JavaScript: pairs in, RedeemerBudget[] out, and the error a failing
// script throws. The Scala.js suite covers the same rules on the Scala side; this proves the
// published bundle and its declarations agree with them.

import { describe, expect, test } from "vitest";
import {
  evaluator,
  evalPlutusScripts,
  PlutusScriptEvaluationError,
  SlotConfig,
} from "../scalus";
import {
  costModels,
  failingTxCborHex,
  failingUtxoCborHex,
  hexToBytes,
  scriptTxCborHex,
  scriptUtxoCborHex,
  scriptUtxoPairHex,
} from "./fixtures";

const slotConfig = { zeroTime: 1596059091000, zeroSlot: 4492800, slotLength: 1000 };
const byName = { PlutusV1: costModels.PlutusV1, PlutusV2: costModels.PlutusV2, PlutusV3: costModels.PlutusV3 };
const positional = [costModels.PlutusV1, costModels.PlutusV2, costModels.PlutusV3];

describe("evaluator.evaluateTx", () => {
  test("pairs as hex and as bytes agree with the CBOR map", () => {
    // spec [TX-1] [TX-2] [TX-3] [TX-9]
    const fromHex = evaluator.evaluateTx(scriptTxCborHex, [scriptUtxoPairHex], slotConfig, byName, 11);
    const fromBytes = evaluator.evaluateTx(
      hexToBytes(scriptTxCborHex), [hexToBytes(scriptUtxoPairHex)], slotConfig, byName, 11,
    );
    const fromMap = evalPlutusScripts(
      hexToBytes(scriptTxCborHex), hexToBytes(scriptUtxoCborHex), SlotConfig.mainnet, positional, 11,
    );
    expect(fromHex).toEqual(fromMap);
    expect(fromBytes).toEqual(fromMap);
    expect(fromMap).toEqual([{ tag: "Reward", index: 0, budget: { memory: 32318n, steps: 8754898n } }]);
  });

  test("bigint slot fields and extra fields are accepted", () => {
    // spec [TX-5]
    const slotConfigWithExtra = {
      zeroTime: 1596059091000n,
      zeroSlot: 4492800n,
      slotLength: 1000,
      startEpoch: 208,
    };
    const costModelsWithExtra = { ...byName, extra: true };
    const withBigint = evaluator.evaluateTx(
      scriptTxCborHex, [scriptUtxoPairHex],
      slotConfigWithExtra,
      costModelsWithExtra, 11,
    );
    expect(withBigint).toEqual(evaluator.evaluateTx(scriptTxCborHex, [scriptUtxoPairHex], slotConfig, byName, 11));
  });

  test("unreadable inputs throw TypeError", () => {
    // spec [TX-10]
    const bad = (f: () => unknown) => expect(f).toThrow(TypeError);
    bad(() => evaluator.evaluateTx("zz", [scriptUtxoPairHex], slotConfig, byName, 11));
    bad(() => evaluator.evaluateTx("00", [scriptUtxoPairHex], slotConfig, byName, 11)); // valid hex, not a transaction
    bad(() => evaluator.evaluateTx(scriptTxCborHex, ["zz"], slotConfig, byName, 11));
    bad(() => evaluator.evaluateTx(scriptTxCborHex, ["00"], slotConfig, byName, 11)); // valid hex, not a pair
    bad(() => evaluator.evaluateTx(scriptTxCborHex, [scriptUtxoPairHex], { zeroTime: "0", zeroSlot: 0, slotLength: 1000 } as never, byName, 11));
    bad(() => evaluator.evaluateTx(scriptTxCborHex, [scriptUtxoPairHex], slotConfig, { PlutusV3: "x" } as never, 11));
    bad(() => evaluator.evaluateTx(scriptTxCborHex, [scriptUtxoPairHex], slotConfig, byName, 1.5));
    bad(() => evaluator.evaluateTx(scriptTxCborHex, [scriptUtxoPairHex], slotConfig, byName, 2 ** 40)); // safe integer, too big for Int
  });

  test("an input no pair resolves is a plain Error", () => {
    // spec [TX-12]
    let caught: unknown;
    try { evaluator.evaluateTx(scriptTxCborHex, [], slotConfig, byName, 11); } catch (e) { caught = e; }
    expect(caught).toBeInstanceOf(Error);
    expect(caught).not.toBeInstanceOf(TypeError);
    expect(caught).not.toBeInstanceOf(PlutusScriptEvaluationError);
    expect((caught as Error).message.length).toBeGreaterThan(0);
  });
});

describe("PlutusScriptEvaluationError", () => {
  const thrown = (): PlutusScriptEvaluationError => {
    try {
      evalPlutusScripts(hexToBytes(failingTxCborHex), hexToBytes(failingUtxoCborHex), SlotConfig.mainnet, positional, 11);
    } catch (e) {
      if (e instanceof PlutusScriptEvaluationError) return e;
      throw e;
    }
    throw new Error("the failing fixture did not fail");
  };

  test("is an Error with the four own fields and a message that names the redeemer", () => {
    // spec [ER-1] [ER-2] [ER-3] [ER-4] [ER-5] [ER-6] [ER-7] [ER-8] [MSG-1] [MSG-2] [MSG-4]
    const e = thrown();
    expect(e).toBeInstanceOf(Error);
    expect(e.name).toBe("PlutusScriptEvaluationError");
    expect(["Spend", "Mint", "Cert", "Reward", "Voting", "Proposing"]).toContain(e.redeemer!.tag);
    expect(e.redeemer!.budget.steps).toBeGreaterThan(0n);
    expect(e.scriptHash).toMatch(/^[0-9a-f]{56}$/);
    expect(["SCRIPT_FAILURE", "BUILTIN_FAILURE", "INVALID_RETURN_VALUE", "OUT_OF_BUDGET", "INTERNAL_ERROR"]).toContain(e.code);
    expect(e.logs.length).toBeGreaterThan(0);
    expect(e.message.startsWith(`${e.redeemer!.tag}[${e.redeemer!.index}] failed: `)).toBe(true);
    expect(e.message).toContain(`script: ${e.scriptHash}`);
    expect(e.message).toMatch(/spent budget: \{ mem: \d+, steps: \d+ }/);
    const own = Object.keys(e);
    expect(own).toEqual(expect.arrayContaining(["redeemer", "scriptHash", "code", "logs"]));
    expect(own).not.toContain("args");
    expect(Array.isArray(e.args)).toBe(true);
    // No replacer: the bigint budget serializes through ExUnits.toJSON, and args stays out.
    const json = JSON.stringify(e);
    expect(json).toContain('"budget":{"memory":"');
    expect(json).not.toContain(e.args![0]!);
  });

  test("args carries the Data arguments the script was applied to", () => {
    // spec [ER-10]
    const e = thrown();
    const args: readonly string[] = e.args!;
    expect(args.length).toBeGreaterThan(0);
    for (const arg of args) expect(arg).toMatch(/^[0-9a-f]+$/);
  });

  test("the (message, logs) constructor still works", () => {
    // spec [ER-13]
    const e = new PlutusScriptEvaluationError("m", ["a"]);
    expect(e.message).toBe("m");
    expect(e.logs).toEqual(["a"]);
    expect(e.redeemer).toBeUndefined();
    expect(e.args).toBeUndefined();
  });
});
