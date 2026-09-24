// __tests__/legacy-evaluation.test.ts
// The pre-CBOR-first entry points, pinned exactly as they behave today.
//
// The oracle here is the shipped behaviour, never the new evaluator: these calls keep their own
// decoding, their own default costs, their own broad catch, and their own log text, quirks
// included. A value below that looks wrong for the new API is usually the point.

import { describe, expect, test } from "vitest";
import { readFileSync } from "fs";
import { resolve } from "path";
import {
  applyDataArgToScript,
  evaluateScript,
  evaluateScriptProfile,
  EvaluationResult,
  ExUnits,
  Result,
  Scalus,
} from "../scalus";
import { failScriptHex, successScriptHex } from "./fixtures";

const appliedHex = "581c581a01010032335738920107537563636573730049930102182a0001";

describe("applyDataArgToScript", () => {
  test("takes one JSON argument and returns double-CBOR hex", () => {
    expect(applyDataArgToScript(successScriptHex, JSON.stringify({ int: 42 }))).toBe(appliedHex);
  });

  test("keeps full precision for a JSON integer beyond 2^53", () => {
    // Parsed as a BigInt, not a double: the low digits survive into the encoded constant.
    const applied = applyDataArgToScript(
      successScriptHex,
      '{"int":123456789012345678901234567890}',
    );
    expect(applied.endsWith("018ee90ff6c373e0ee4e3f0ad20001")).toBe(true);
  });

  test("throws on malformed JSON, as it always has", () => {
    expect(() => applyDataArgToScript(successScriptHex, "not json")).toThrow();
  });
});

describe("evaluateScript and evaluateScriptProfile", () => {
  test("succeed on an applied script with the bundled default costs", () => {
    const result = evaluateScript(appliedHex);
    expect(result.isSuccess).toBe(true);
    expect(result.budget.memory).toBe(1032n);
    expect(result.budget.steps).toBe(203598n);
    expect(result.logs).toEqual(["Success"]);
    expect(result.profileJson).toBeUndefined();
  });

  test("profiling adds JSON of the documented schema without changing the outcome", () => {
    const plain = evaluateScript(appliedHex);
    const profiled = evaluateScriptProfile(appliedHex);

    expect(profiled.isSuccess).toBe(plain.isSuccess);
    expect(profiled.budget.memory).toBe(plain.budget.memory);
    expect(profiled.logs).toEqual(plain.logs);
    expect(Object.keys(JSON.parse(profiled.profileJson!))).toEqual(
      expect.arrayContaining(["schemaVersion", "totalBudget", "bySourceLocation", "byFunction", "traces"]),
    );
    expect(Array.isArray(JSON.parse(profiled.profileJson!).traces)).toBe(true); // spec [TB-6]
  });

  test("never throw, and never gain a structured error", () => {
    // Both of these are failures of the *legacy* decoder, which only accepts double CBOR. The
    // single-CBOR fixture is fine for `uplc`/`evaluator` and deliberately still fails here.
    const singleCbor = evaluateScript(failScriptHex);
    expect(singleCbor.isSuccess).toBe(false);
    expect(singleCbor.budget.memory).toBe(0n);
    expect(singleCbor.logs).toEqual([
      "Expected ByteString or Array of bytes but got Int (input position 0)",
    ]);
    expect(singleCbor.error).toBeUndefined();

    const notHex = evaluateScript("zz");
    expect(notHex.isSuccess).toBe(false);
    expect(notHex.logs).toEqual(["`zz` is not a valid hex string"]);
    expect(notHex.error).toBeUndefined();
  });
});

describe("exported shapes", () => {
  test("legacy constructors and aliases retain their call shape", () => {
    const result = new Result(false, new ExUnits(0n, 0n), ["legacy"], undefined);
    expect(Result).toBe(EvaluationResult);
    expect(result.logs).toEqual(["legacy"]);
    expect(result.error).toBeUndefined();
    expect("traces" in result).toBe(false);
  });

  test("the generated declarations point each replaced export at its replacement", () => {
    const dts = readFileSync(resolve(__dirname, "../scalus.d.ts"), "utf8");
    const docOf = (signature: string) => {
      const at = dts.indexOf(signature);
      expect(at, `${signature} is missing from scalus.d.ts`).toBeGreaterThan(-1);
      return dts.slice(dts.lastIndexOf("/**", at), at);
    };

    for (const [signature, replacement] of [
      ["export function applyDataArgToScript(", "uplc.applyParamsToScript"],
      ["export function evaluateScript(", "evaluator.evaluateScript"],
      ["export function evaluateScriptProfile(", "evaluator.evaluateScript"],
      ["export function evalPlutusScripts(", "evaluator.evaluateTx"],
    ] as const) {
      const doc = docOf(signature);
      expect(doc).toContain("@deprecated");
      expect(doc).toContain(replacement);
    }
  });

  test("the Scalus namespace still carries every legacy entry point", () => {
    for (const name of [
      "applyDataArgToScript",
      "evaluateScript",
      "evaluateScriptProfile",
      "evalPlutusScripts",
    ] as const) {
      expect(typeof Scalus[name]).toBe("function");
    }
    expect(Scalus.evaluateScript(appliedHex).isSuccess).toBe(true);
    expect(Scalus.applyDataArgToScript(successScriptHex, JSON.stringify({ int: 42 }))).toBe(
      appliedHex,
    );
  });
});
