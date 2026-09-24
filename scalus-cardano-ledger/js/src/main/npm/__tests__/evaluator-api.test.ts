// __tests__/evaluator-api.test.ts
// The `evaluator` contract as it behaves through the built bundle. The exhaustive
// fixture-driven coverage lives in JEvaluatorTest, where Term-DSL programs are cheap to
// build; what only this file can prove is that the linked artifact and its generated
// declarations carry the same contract.

import { describe, expect, test } from "vitest";
import {
  type EvaluationError,
  EvaluationOptions,
  EvaluationResult,
  Result,
  bytesToHex,
  cbor,
  evaluator,
  uplc,
} from "../scalus";
import { hexToBytes, failScriptHex, successScriptHex } from "./fixtures";

const options = () => EvaluationOptions.mainnet("PlutusV3");

/** `(program 1.0.0 (con integer 3))`: terminates normally, but is not a valid validator. */
const constIntegerHex = "46010000480181";

describe("script and argument representations", () => {
  test("every script form evaluates identically", () => {
    const flat = uplc.decodeToFlat(successScriptHex);
    const single = cbor.wrapBytes(flat);
    const double = cbor.wrapBytes(single);

    for (const script of [flat, single, double, bytesToHex(flat), bytesToHex(single), bytesToHex(double)]) {
      const result = evaluator.evaluateScript(script, ["182a"], options());
      expect(result.isSuccess).toBe(true);
      expect(result.error).toBeUndefined();
    }
  });

  test("arguments mix hex and bytes and apply left to right", () => {
    // failScript is a two-argument lambda; it only reaches its `error` once both apply.
    const result = evaluator.evaluateScript(
      failScriptHex,
      ["182a", hexToBytes("182a")],
      options(),
    );
    expect(result.error?.code).toBe("SCRIPT_FAILURE");
  });
});

describe("result shape", () => {
  test("success carries no error and no deferred fields", () => {
    const result = evaluator.evaluateScript(successScriptHex, ["182a"], options());

    expect(result).toBeInstanceOf(EvaluationResult);
    expect(result).toBeInstanceOf(Result);
    expect(result.isSuccess).toBe(true);
    expect(result.error).toBeUndefined();
    expect(result.budget.memory).toBeGreaterThan(0n);
    expect("traces" in result).toBe(false);
    expect("value" in result).toBe(false);
  });

  test("legacy result construction keeps the four-argument constructor and aliases", () => {
    const first = new EvaluationResult(true, { memory: 0n, steps: 0n }, [], undefined);
    const second = new Result(false, { memory: 0n, steps: 0n }, ["legacy"]);
    expect(first.error).toBeUndefined();
    expect(second.error).toBeUndefined();
    expect("traces" in first).toBe(false);
    expect("traces" in second).toBe(false);
  });
});

describe("failure classification", () => {
  test("a call that cannot be read throws TypeError", () => {
    expect(() => evaluator.evaluateScript(successScriptHex, ["182a"], undefined as never)).toThrow(TypeError);
    expect(() => evaluator.evaluateScript("zz", [], options())).toThrow(TypeError);
    expect(() => evaluator.evaluateScript(successScriptHex, ["182a", "zz"], options())).toThrow(
      /args\[1\]/,
    );
  });

  // code, the call.
  const cases: [EvaluationError["code"], () => EvaluationResult][] = [
    ["SCRIPT_FAILURE", () => evaluator.evaluateScript(failScriptHex, ["182a", "182a"], options())],
    ["INVALID_RETURN_VALUE", () => evaluator.evaluateScript(constIntegerHex, [], options())],
    [
      "OUT_OF_BUDGET",
      () =>
        evaluator.evaluateScript(successScriptHex, ["182a"], {
          ...options(),
          maxBudget: { memory: 1, steps: 1 },
        }),
    ],
  ];

  for (const [code, call] of cases) {
    test(`${code} is a result, with the budget spent and the traces only in logs`, () => {
      const result = call();
      expect(result.isSuccess).toBe(false);
      expect(result.error?.code).toBe(code);
      expect(result.logs).not.toContain(result.error?.message);
      expect(result.budget.memory).toBeGreaterThan(0n);
      // Own and enumerable, and the bigint budget serializes through ExUnits.toJSON.
      expect(Object.keys(result)).toContain("error");
      expect(JSON.parse(JSON.stringify(result)).error.code).toBe(code);
    });
  }
});

describe("the validator return rule", () => {
  test("a non-unit V3 result fails, but keeps the budget and traces it earned", () => {
    // The rule is checked after the machine stops, so this is still how you cost a pure
    // on-chain function: read error.code rather than isSuccess.
    const rejected = evaluator.evaluateScript(constIntegerHex, [], options());
    expect(rejected.isSuccess).toBe(false);
    expect(rejected.error?.code).toBe("INVALID_RETURN_VALUE");
    expect(rejected.budget.memory).toBeGreaterThan(0n);
    expect(rejected.budget.steps).toBeGreaterThan(0n);
  });

  test("a genuine script failure is reported as such, not as a return-value problem", () => {
    expect(evaluator.evaluateScript(failScriptHex, ["182a", "182a"], options()).error?.code).toBe(
      "SCRIPT_FAILURE",
    );
  });
});

describe("options", () => {
  test("unknown fields are ignored, and this evaluator never profiles", () => {
    const plain = evaluator.evaluateScript(successScriptHex, ["182a"], options());
    const withStrayKeys = evaluator.evaluateScript(successScriptHex, ["182a"], {
      ...options(),
      profile: true,
    } as never);

    expect(withStrayKeys.isSuccess).toBe(true);
    expect(withStrayKeys.budget).toEqual(plain.budget);
    expect(withStrayKeys.logs).toEqual(plain.logs);
    expect(plain.profileJson).toBeUndefined();
    expect(withStrayKeys.profileJson).toBeUndefined();
  });

  test("factories hand out independent records the caller may mutate", () => {
    const first = options();
    const second = options();
    expect(first).not.toBe(second);
    expect(first.costModel).not.toBe(second.costModel);

    (first.costModel as number[])[0] = -1;
    expect(second.costModel[0]).not.toBe(-1);
    expect(evaluator.evaluateScript(successScriptHex, ["182a"], second).isSuccess).toBe(
      true,
    );
  });

  test("each language factory evaluates with its own model", () => {
    for (const build of [
      () => EvaluationOptions.mainnet("PlutusV1"),
      () => EvaluationOptions.mainnet("PlutusV2"),
      () => EvaluationOptions.mainnet("PlutusV3"),
    ]) {
      expect(evaluator.evaluateScript(successScriptHex, ["182a"], build()).isSuccess).toBe(
        true,
      );
    }
  });
});

// The browser harness shares this function; running it here too means a silently skipped or
// empty browser block shows up as a Node failure rather than a green browser run.
test("the shared browser test block passes against the same bundle", async () => {
  const { testCborFirstApi } = await import("./shared-tests");
  const results = testCborFirstApi({ uplc, cbor, bytesToHex, evaluator, EvaluationOptions });

  expect(results.length).toBeGreaterThanOrEqual(6);
  expect(results.filter((r) => !r.passed)).toEqual([]);
});
