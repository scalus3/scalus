// __tests__/require.test.ts
// scalus.js is an ES module, and stays one. But a CommonJS codebase must still be able to load it:
// Node >= 22.12 / >= 20.19 can `require()` an ES module directly, so the package advertises a
// `default` condition pointing at the same file. That is deliberately NOT a second, CommonJS build
// — one file means one module instance, one PlutusVM and one set of class identities, so
// `instanceof` keeps working across a mixed CJS/ESM dependency tree.

import { describe, test, expect } from "vitest";
import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";

const require = createRequire(import.meta.url);

describe("CommonJS interoperability", () => {
  test("the exports map lets require() resolve the package", () => {
    const pkg = JSON.parse(
      readFileSync(resolve(__dirname, "../package.json"), "utf8"),
    );
    const root = pkg.exports["."];

    // `require()` matches the first condition it understands. Without one of `require` or
    // `default`, it matches nothing and Node throws ERR_PACKAGE_PATH_NOT_EXPORTED.
    expect(root.default).toBe("./scalus.js");

    // And it must point at the same file `import` does. A separate .cjs build here would be the
    // dual-package hazard: two copies of the runtime, two PlutusVMs, instanceof failing between
    // them.
    expect(root.default).toBe(root.import);
  });

  test("the bundle actually loads under require()", () => {
    const scalus = require(resolve(__dirname, "../scalus.js"));

    expect(typeof scalus.Emulator).toBe("function");
    expect(typeof scalus.evaluateScript).toBe("function");
    expect(typeof scalus.SlotConfig).toBe("function");
  });

  test("require() and a second require() yield the same module instance", () => {
    const a = require(resolve(__dirname, "../scalus.js"));
    const b = require(resolve(__dirname, "../scalus.js"));

    // One instance, so class identity holds and `instanceof` works across callers.
    expect(a.Emulator).toBe(b.Emulator);
  });

  test("a required build evaluates a script, so it is not merely importable", () => {
    const scalus = require(resolve(__dirname, "../scalus.js"));

    // An always-fails script: what matters is that the machine ran and produced a result rather
    // than the module failing to initialise.
    const result = scalus.evaluateScript("450100002261");
    expect(typeof result.isSuccess).toBe("boolean");
    expect(Array.isArray(result.logs)).toBe(true);
  });
});
