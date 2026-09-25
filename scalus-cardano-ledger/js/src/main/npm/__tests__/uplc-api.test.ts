import { expect, test } from "vitest";
import { EvaluationOptions, Scalus, bytesToHex, cbor, dataHash, evaluator, hexToBytes, scriptHash, uplc } from "../scalus";
import { successScriptHex } from "./fixtures";

const double = "545301010023357389210753756363657373004981";

test("the primitives compose into every envelope, and applyParamsToScript is that composition", () => {
  const flat = uplc.decodeToFlat(double);
  const single = cbor.wrapBytes(flat);
  expect(bytesToHex(cbor.wrapBytes(single))).toBe(double);
  expect(cbor.unwrapBytes(single)).toEqual(flat);
  for (const script of [double, single, flat, bytesToHex(flat)]) {
    expect(uplc.decodeToFlat(script)).toEqual(flat);
  }
  const applied = uplc.applyArgs(flat, ["182a"]);
  expect(uplc.applyParamsToScript(double, ["182a"])).toBe(bytesToHex(cbor.wrapBytes(cbor.wrapBytes(applied))));
});

test("malformed Flat is a TypeError wherever the program is decoded", () => {
  for (const script of ["450100004a81", "47010000488102aa", "48010000488101aa"]) {
    expect(() => uplc.applyParamsToScript(script, [])).toThrow(TypeError);
  }
  expect(() => uplc.applyParamsToScript(double, ["0x00"])).toThrow(TypeError);
});

test("hex and envelopes reject what they cannot read", () => {
  expect(hexToBytes("00FF")).toEqual(new Uint8Array([0, 255]));
  expect(() => hexToBytes("zz")).toThrow(TypeError);
  expect(() => cbor.unwrapBytes(new Uint8Array([0]))).toThrow(TypeError);
  // Only envelopes are read: a byte string that is not a program comes back as is.
  expect(uplc.decodeToFlat("4100")).toEqual(new Uint8Array([0]));
});

test("namespace methods work detached, as Math.max does", () => {
  // Scala.js exports object members as prototype methods on `this`; bindExports binds them.
  const { decodeToFlat, applyParamsToScript } = uplc;
  const { wrapBytes } = cbor;
  const { evaluateScript } = evaluator;
  const { applyDataArgToScript } = Scalus;
  const legacyEvaluate = Scalus.evaluateScript;
  expect(bytesToHex(wrapBytes(wrapBytes(decodeToFlat(double))))).toBe(double);
  const applied = applyParamsToScript(successScriptHex, ["182a"]);
  expect(applyDataArgToScript(successScriptHex, JSON.stringify({ int: 42 }))).toBe(applied);
  expect(evaluateScript(successScriptHex, ["182a"], EvaluationOptions.mainnet("PlutusV3")).isSuccess).toBe(true);
  expect(legacyEvaluate(applied).isSuccess).toBe(true);
});

test("scriptHash ignores the CBOR wrapping; dataHash hashes the bytes as given", () => {
  const flat = uplc.decodeToFlat(double);
  const hash = scriptHash({ type: "PlutusV3", script: double });
  expect(hash).toMatch(/^[0-9a-f]{56}$/);
  expect(scriptHash({ type: "PlutusV3", script: flat })).toBe(hash);
  expect(scriptHash({ type: "PlutusV2", script: flat })).not.toBe(hash);
  expect(dataHash("182a")).toMatch(/^[0-9a-f]{64}$/);
  expect(dataHash("19002a")).not.toBe(dataHash("182a"));
  expect(() => scriptHash({ type: "PlutusV9", script: flat } as never)).toThrow(TypeError);
});
