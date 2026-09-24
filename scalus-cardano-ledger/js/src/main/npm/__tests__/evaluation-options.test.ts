import { expect, test } from "vitest";
import { CardanoInfo, EvaluationOptions, ProtocolParams, evaluator } from "../scalus";

// UPLC 1.0.0 works for all three languages.
const alwaysSucceeds = "545301000023357389210753756363657373004981";

test("all factories select independent cost model records", () => {
  const params = CardanoInfo.mainnet().protocolParams;
  const supplied = [
    EvaluationOptions.fromProtocolParams("PlutusV1", params),
    EvaluationOptions.fromProtocolParams("PlutusV2", params),
    EvaluationOptions.fromProtocolParams("PlutusV3", params),
  ];
  const snapshots = [
    EvaluationOptions.mainnet("PlutusV1"),
    EvaluationOptions.mainnet("PlutusV2"),
    EvaluationOptions.mainnet("PlutusV3"),
  ];

  expect(supplied.map((option) => option.plutusVersion)).toEqual([
    "PlutusV1",
    "PlutusV2",
    "PlutusV3",
  ]);
  expect(snapshots.map((option) => option.protocolMajorVersion)).toEqual([11, 11, 11]);
  expect(supplied.map((option) => option.costModel.length)).toEqual([332, 332, 350]);
  expect(snapshots.map((option) => option.costModel.length)).toEqual([332, 332, 350]);
});

test("JSON-loaded Blockfrost parameters feed every factory and evaluator", () => {
  const blockfrost = ProtocolParams.fromBlockfrostJson(
    CardanoInfo.mainnet().protocolParams.toBlockfrostJson(),
  );

  for (const params of [blockfrost]) {
    const options = [
      EvaluationOptions.fromProtocolParams("PlutusV1", params),
      EvaluationOptions.fromProtocolParams("PlutusV2", params),
      EvaluationOptions.fromProtocolParams("PlutusV3", params),
    ];

    expect(options.map((option) => option.plutusVersion)).toEqual([
      "PlutusV1",
      "PlutusV2",
      "PlutusV3",
    ]);
    expect(options.map((option) => option.costModel.length)).toEqual([332, 332, 350]);
    expect(
      options.map((option) =>
        evaluator.evaluateScript(alwaysSucceeds, ["182a"], option).isSuccess
      ),
    ).toEqual([true, true, true]);
  }
});

test("mainnet options have independent cost arrays", () => {
  const a = EvaluationOptions.mainnet("PlutusV3");
  const b = EvaluationOptions.mainnet("PlutusV3");
  const before = b.costModel[0];
  (a.costModel as number[])[0] = 0;
  expect(b.costModel[0]).toBe(before);
  expect(a).not.toBe(b);
});

test("supplied factories return independent cost arrays", () => {
  const params = CardanoInfo.mainnet().protocolParams;
  const a = EvaluationOptions.fromProtocolParams("PlutusV3", params);
  const b = EvaluationOptions.fromProtocolParams("PlutusV3", params);
  const before = params.costModels.PlutusV3[0];
  (a.costModel as number[])[0] = before + 1;
  expect(params.costModels.PlutusV3[0]).toBe(before);
  expect(b.costModel[0]).toBe(before);
});

test("factories reject undefined and plain objects with native TypeErrors", () => {
  const params = CardanoInfo.mainnet().protocolParams;
  expect(() => EvaluationOptions.fromProtocolParams("PlutusV3", undefined as never)).toThrow(TypeError);
  expect(() => EvaluationOptions.fromProtocolParams("PlutusV3", params.toObject() as never)).toThrow(TypeError);
});

test("a plain object literal is an options record, with or without maxBudget", () => {
  const costModel = CardanoInfo.mainnet().protocolParams.costModels.PlutusV3;
  const script = "545301010023357389210753756363657373004981";
  const options: EvaluationOptions = { plutusVersion: "PlutusV3", protocolMajorVersion: 11, costModel };
  expect(evaluator.evaluateScript(script, ["182a"], options).isSuccess).toBe(true);

  // A future protocol configures, costed under the newest rules this build knows.
  expect(evaluator.evaluateScript(script, ["182a"], { ...options, protocolMajorVersion: 12 }).isSuccess).toBe(true);

  // A version that is not a whole number, or predates the language, cannot be costed.
  expect(() => evaluator.evaluateScript(script, ["182a"], { ...options, protocolMajorVersion: 9.5 })).toThrow(TypeError);
  expect(() => evaluator.evaluateScript(script, ["182a"], { ...options, protocolMajorVersion: 8 })).toThrow(TypeError);
});

test("maxBudget stops a script that spends more, and reports what it spent", () => {
  const options = EvaluationOptions.mainnet("PlutusV3");
  const script = "545301010023357389210753756363657373004981";
  const spent = evaluator.evaluateScript(script, ["182a"], options).budget;

  const enough = evaluator.evaluateScript(script, ["182a"], { ...options, maxBudget: spent });
  expect(enough.isSuccess).toBe(true);

  const short = evaluator.evaluateScript(script, ["182a"], {
    ...options,
    maxBudget: { memory: Number(spent.memory), steps: spent.steps - 1n },
  });
  expect(short.isSuccess).toBe(false);
  expect(short.error?.code).toBe("OUT_OF_BUDGET");
  expect(short.budget.steps).toBeGreaterThan(spent.steps - 1n);
});
