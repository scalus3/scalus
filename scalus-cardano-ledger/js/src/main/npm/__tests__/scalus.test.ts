// __tests__/scalus.test.ts
// Vitest tests for Scalus.applyDataArgToScript and Scalus.evaluateScript

import { describe, test, expect } from "vitest";
import { Scalus } from "../scalus";
import {
  testApplyDataArgToScript,
  testEvaluateScript,
  testEvaluateScriptProfile,
} from "./shared-tests";

describe("Scalus.applyDataArgToScript", () => {
  const results = testApplyDataArgToScript(Scalus);

  for (const result of results) {
    test(result.name, () => {
      expect(result.passed, result.message).toBe(true);
    });
  }
});

describe("Scalus.evaluateScript", () => {
  const results = testEvaluateScript(Scalus);

  for (const result of results) {
    test(result.name, () => {
      expect(result.passed, result.message).toBe(true);
    });
  }
});

describe("Scalus.evaluateScriptProfile", () => {
  const results = testEvaluateScriptProfile(Scalus);

  for (const result of results) {
    test(result.name, () => {
      expect(result.passed, result.message).toBe(true);
    });
  }
});

test("Scalus methods work detached, as Math.max does", () => {
  // Scala.js exports object members as prototype methods on `this`; bindExports binds them.
  const script = "545301010023357389210753756363657373004981";
  const { applyDataArgToScript, evaluateScript } = Scalus;
  const applied = applyDataArgToScript(script, JSON.stringify({ int: 42 }));
  expect(applied).toBe(Scalus.applyDataArgToScript(script, JSON.stringify({ int: 42 })));
  expect(evaluateScript(applied).isSuccess).toBe(true);
});
