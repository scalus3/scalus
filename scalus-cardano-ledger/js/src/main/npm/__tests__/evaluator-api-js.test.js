// __tests__/evaluator-api-js.test.js
// Deliberately plain JavaScript: every call here is one TypeScript would have rejected at
// compile time. Each must throw a TypeError rather than crash oddly or silently succeed.

import { expect, test } from "vitest";
import { evaluator, EvaluationOptions } from "../scalus.js";

const script = "545301010023357389210753756363657373004981";
const options = () => EvaluationOptions.mainnet("PlutusV3");

const rejections = [
  ["missing options", () => evaluator.evaluateScript(script, ["182a"])],
  ["null script", () => evaluator.evaluateScript(null, ["182a"], options())],
  ["numeric script", () => evaluator.evaluateScript(42, [], options())],
  ["non-array args", () => evaluator.evaluateScript(script, null, options())],
  ["numeric args", () => evaluator.evaluateScript(script, 7, options())],
  ["numeric argument", () => evaluator.evaluateScript(script, [42], options())],
  ["empty options object", () => evaluator.evaluateScript(script, ["182a"], {})],
  [
    "unknown language",
    () => evaluator.evaluateScript(script, ["182a"], { ...options(), plutusVersion: "PlutusV9" }),
  ],
  [
    "NaN cost parameter",
    () =>
      evaluator.evaluateScript(script, ["182a"], {
        ...options(),
        costModel: [NaN, ...options().costModel.slice(1)],
      }),
  ],
  [
    "fractional maxBudget",
    () =>
      evaluator.evaluateScript(script, ["182a"], {
        ...options(),
        maxBudget: { memory: 1.5, steps: 1 },
      }),
  ],
];

for (const [name, call] of rejections) {
  test(`${name} throws TypeError`, () => {
    expect(call).toThrow(TypeError);
  });
}

test("a short cost model configures, and prices out what it does not cover", () => {
  // No length is rejected. Everything past the first three parameters is padded with maxBound,
  // so the reported cost saturates instead of being a plausible-looking number.
  const short = { ...options(), costModel: [1, 2, 3] };
  const priced = evaluator.evaluateScript(script, ["182a"], short);
  expect(priced.isSuccess).toBe(true);
  expect(priced.budget.steps).toBe(9223372036854775807n);
});

test("a well-formed untyped call still succeeds", () => {
  // Guards the rows above: they must fail on the bad input, not because this shape is broken.
  const r = evaluator.evaluateScript(script, ["182a"], options());
  expect(r.isSuccess).toBe(true);
  expect(r.error).toBeUndefined();
});
