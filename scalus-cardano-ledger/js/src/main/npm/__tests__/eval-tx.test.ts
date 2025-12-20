// __tests__/eval-tx.test.ts
// Vitest tests for Scalus.evalPlutusScripts

import { describe, test, expect } from "vitest";
import { Scalus, SlotConfig } from "../scalus";
import { testEvalPlutusScripts } from "./shared-tests";

describe("Scalus.evalPlutusScripts", () => {
  const results = testEvalPlutusScripts(Scalus, SlotConfig);

  for (const result of results) {
    test(result.name, () => {
      expect(result.passed, result.message).toBe(true);
    });
  }
});
