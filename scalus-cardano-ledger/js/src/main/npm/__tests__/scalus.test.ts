// __tests__/scalus.test.ts
// Vitest tests for Scalus.applyDataArgToScript and Scalus.evaluateScript

import { describe, test, expect } from "vitest";
import { Scalus } from "../scalus";
import { testApplyDataArgToScript, testEvaluateScript } from "./shared-tests";

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
