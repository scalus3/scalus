// __tests__/bundle-size.test.ts
// Verify that scalus.js does not contain embedded JSON protocol parameter files

import { describe, test, expect } from "vitest";
import { readFileSync } from "fs";
import { resolve } from "path";

describe("scalus.js bundle", () => {
  const bundlePath = resolve(__dirname, "../scalus.js");
  const bundle = readFileSync(bundlePath, "utf8");

  test("should not contain embedded Blockfrost JSON protocol parameters", () => {
    // These fields only exist in the raw Blockfrost JSON, not in the parsed ProtocolParams.
    // Their presence indicates the JSON file was embedded as a string literal.
    expect(bundle).not.toContain('"cost_models_raw"');
    expect(bundle).not.toContain('"decentralisation_param"');
    expect(bundle).not.toContain('"extra_entropy"');
    expect(bundle).not.toContain('"nonce"');
  });

  test("should be smaller than 2MB", () => {
    const sizeInBytes = Buffer.byteLength(bundle, "utf8");
    const twoMB = 2 * 1024 * 1024;
    expect(sizeInBytes).toBeLessThan(twoMB);
  });
});
