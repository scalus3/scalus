// __tests__/flat-api.test.ts
// The canonical flat ESM API: top-level functions and the renamed classes.
// The existing scalus.test.ts keeps covering the deprecated `Scalus` namespace object.

import { describe, test, expect } from "vitest";
import {
  applyDataArgToScript,
  evaluateScript,
  evaluateScriptProfile,
  EvaluationResult,
  Result,
  RedeemerBudget,
  Redeemer,
  ExUnits,
} from "../scalus";

const alwaysSucceeds = "545301010023357389210753756363657373004981";

describe("flat top-level functions", () => {
  test("applyDataArgToScript + evaluateScript succeed", () => {
    const applied = applyDataArgToScript(alwaysSucceeds, JSON.stringify({ int: 42 }));
    const result: EvaluationResult = evaluateScript(applied);
    expect(result.isSuccess).toBe(true);
    expect(result.budget.memory).toBeGreaterThan(0n);
    expect(result.profileJson).toBeUndefined();
  });

  test("evaluateScriptProfile populates profileJson", () => {
    const applied = applyDataArgToScript(alwaysSucceeds, JSON.stringify({ int: 42 }));
    const result = evaluateScriptProfile(applied);
    expect(result.isSuccess).toBe(true);
    expect(typeof result.profileJson).toBe("string");
  });
});

describe("deprecated class aliases stay usable", () => {
  test("Result === EvaluationResult, Redeemer === RedeemerBudget", () => {
    expect(Result).toBe(EvaluationResult);
    expect(Redeemer).toBe(RedeemerBudget);
  });

  test("a Result-typed value is an EvaluationResult instance", () => {
    const applied = applyDataArgToScript(alwaysSucceeds, JSON.stringify({ int: 1 }));
    const r: Result = evaluateScript(applied);
    expect(r).toBeInstanceOf(EvaluationResult);
    expect(r.budget).toBeInstanceOf(ExUnits);
  });
});
