// __tests__/uplc-sdk-compat.test.ts
// The integration claim, checked against the real libraries rather than a hand-written fixture:
// a caller serializes Data with whichever SDK they already use, and Scalus takes those bytes.
//
// Each SDK stays inside its own domain here. Nothing casts a foreign Data value into a Scalus
// one, and nothing routes through JSON: CBOR is the boundary, which is the whole point.

import { Data as LucidData, Constr } from "@lucid-evolution/lucid";
import { toPlutusData } from "@meshsdk/core-cst";
import * as EvolutionData from "@evolution-sdk/evolution/Data";
import { describe, expect, test } from "vitest";
import { EvaluationOptions, evaluator, uplc } from "../scalus";
import { successScriptHex } from "./fixtures";

/** `LucidData` is a local alias for Lucid's `Data`; the method is `Data.to`, not `Data.do`. */
const lucid = LucidData.to;
const mesh = (data: Parameters<typeof toPlutusData>[0]): string =>
  toPlutusData(data).toCbor().toString();
const evolution = EvolutionData.toCBORBytes;

const hex = (bytes: Uint8Array) =>
  Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");

describe("the three SDKs serialize Data to the same CBOR", () => {
  // Each row builds the same value three times, in each SDK's own native domain.
  const shapes: [string, string, string, Uint8Array][] = [
    ["integer", lucid(42n), mesh(42n), evolution(42n)],
    [
      "byte string",
      lucid("deadbeef"),
      mesh("deadbeef"),
      evolution(new Uint8Array([0xde, 0xad, 0xbe, 0xef])),
    ],
    ["list", lucid([1n, 2n]), mesh([1n, 2n]), evolution([1n, 2n])],
    [
      "large integer",
      lucid(123456789012345678901234567890n),
      mesh(123456789012345678901234567890n),
      evolution(123456789012345678901234567890n),
    ],
  ];

  for (const [name, fromLucid, fromMesh, fromEvolution] of shapes) {
    test(name, () => {
      expect(fromMesh).toBe(fromLucid);
      expect(hex(fromEvolution)).toBe(fromLucid);
    });
  }

  test("constructor, built with each SDK's own constructor type", () => {
    expect(mesh({ alternative: 0, fields: [42n, "deadbeef"] })).toBe(
      lucid(new Constr(0, [42n, "deadbeef"])),
    );
    expect(hex(evolution(EvolutionData.constr(0n, [42n])))).toBe(lucid(new Constr(0, [42n])));
  });
});

describe("SDK CBOR feeds the same application and evaluation path", () => {
  const args = [lucid(42n), mesh(42n), evolution(42n)];

  test("every SDK's bytes produce the same applied script", () => {
    const [first, ...rest] = args.map((arg) => uplc.applyParamsToScript(successScriptHex, [arg]));
    for (const applied of rest) expect(applied).toBe(first);
    // And the same script the legacy JSON helper produces, by a different route.
    expect(first).toBe("581c581a01010032335738920107537563636573730049930102182a0001");
  });

  test("every SDK's bytes evaluate", () => {
    for (const arg of args) {
      const result = evaluator.evaluateScript(
        successScriptHex,
        [arg],
        EvaluationOptions.mainnet("PlutusV3"),
      );
      expect(result.isSuccess).toBe(true);
      expect(result.error).toBeUndefined();
    }
  });

  test("an SDK value needs no Scalus-specific adapter, only its own serializer", () => {
    // Deliberately a non-trivial shape: a constructor holding a list and a byte string.
    const redeemer = lucid(new Constr(1, [[1n, 2n], "deadbeef"]));
    const result = evaluator.evaluateScript(
      successScriptHex,
      [redeemer],
      EvaluationOptions.mainnet("PlutusV3"),
    );
    expect(result.isSuccess).toBe(true);
  });
});
