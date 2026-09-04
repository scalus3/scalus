// __tests__/bundle-size.test.ts
// What the published bundle must not contain, and how large it is allowed to be.

import { describe, test, expect } from "vitest";
import { readFileSync } from "fs";
import { resolve } from "path";

describe("scalus.js bundle", () => {
    // This is the file `prepareNpmPackage` writes: esbuild's `--outfile` is this exact path, and
    // `runNpmTests` depends on that task, so under sbt these assertions see the build under test
    // and not a stale artifact. A bare `npx vitest run` measures whatever is checked out, which is
    // only meaningful when the committed bundle is current - so keep it committed and current.
    const bundlePath = resolve(__dirname, "../scalus.js");
    const bundle = readFileSync(bundlePath, "utf8");

    test("should not contain embedded Blockfrost JSON protocol parameters", () => {
        // These fields only exist in the raw Blockfrost JSON, not in the parsed ProtocolParams.
        // Their presence indicates the JSON file was embedded as a string literal.
        //
        // "cost_models_raw" used to be one of these markers, but ProtocolParams.toBlockfrostJson
        // (scalus-core) now legitimately emits that key as its own output, so it is expected to
        // appear in the bundle and is no longer evidence of an embedded JSON literal.
        expect(bundle).not.toContain('"decentralisation_param"');
        expect(bundle).not.toContain('"extra_entropy"');
        expect(bundle).not.toContain('"nonce"');
    });

    test("should not link the libraries deliberately kept out of the bundle", () => {
        // Two size levers from docs/internal/JS_BUNDLE_SIZE.md, each worth six figures of linker
        // output, and each undone by a single new reachable member. Both regressions would have
        // been caught here for the price of two string searches.
        expect(bundle).not.toContain("threeten"); // scala-java-time and the IANA tzdb
        // The JVM logging backend. Minification renames identifiers but keeps string literals, and
        // Scala.js writes each linked class's fully qualified name into one, so `scribe.Logger` and
        // friends appear as `scribe.` if and only if the library links. A bare `"scribe"` substring
        // would also match `subscribe` and `describe`, which is a different claim than this one.
        expect(bundle).not.toMatch(/\bscribe\./);
    });

    test("should be smaller than 2.75MB", () => {
        // 2,812,916 bytes as of this commit. The pre-Task-7 baseline was 2,591,052; the difference
        // is `ProtocolParams.fromBlockfrostJson`/`toBlockfrostJson` re-linking upickle and ujson,
        // which is a deliberate, recorded trade (the jsoniter port that would have recovered
        // ~142 KB broke scalus-native and was reverted). The limit guards the next regression, not
        // that one.
        const sizeInBytes = Buffer.byteLength(bundle, "utf8");
        const limit = 2.75 * 1024 * 1024;
        expect(sizeInBytes).toBeLessThan(limit);
    });
});
