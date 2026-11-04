// __tests__/bundle-size.test.ts
// What the published bundle must not contain, and how large it is allowed to be.

import { describe, test, expect } from "vitest";
import { readFileSync } from "fs";
import { resolve } from "path";
import { build } from "esbuild";

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
        // The IANA timezone database, ~590 KB of the minified bundle. `threeten` used to be the
        // marker here and matched nothing: this build's zone data is in package `zonedb.java`, so
        // the assertion passed from the day it was added while the whole database shipped.
        expect(bundle).not.toContain("zonedb");
        // The JVM logging backend. Minification renames identifiers but keeps string literals, and
        // Scala.js writes each linked class's fully qualified name into one, so `scribe.Logger` and
        // friends appear as `scribe.` if and only if the library links. A bare `"scribe"` substring
        // would also match `subscribe` and `describe`, which is a different claim than this one.
        expect(bundle).not.toMatch(/\bscribe\./);
    });

    test("can be bundled for a browser without Node polyfills", async () => {
        const result = await build({
            entryPoints: [bundlePath],
            bundle: true,
            platform: "browser",
            format: "esm",
            write: false,
            metafile: true,
        });
        expect(result.outputFiles[0]!.text).not.toContain("node:crypto");
        expect(Object.values(result.metafile!.outputs).flatMap(o => o.imports)).toEqual([]);
    });

    test("should be smaller than 2.6MB", () => {
        // About 2.19 MiB since the tzdb exclusion (lever 5). `ProtocolParams.fromBlockfrostJson`/
        // `toBlockfrostJson` still re-link upickle and ujson for ~117 KB, which is a deliberate,
        // recorded trade (the jsoniter port that would have recovered it broke scalus-native and
        // was reverted). The limit guards the next regression, not that one.
        //
        // The emulator chain (blocks, mempool, forks, rollback: emulator-chain spec) raised the
        // cap from 2.3 MiB: 2,391,744 bytes before it, 2,474,457 after (+82.7 KB). The facade now
        // reaches EmulatorState's forge, chain selection and rollback, BlockForge, and the ledger's
        // Block/BlockHeader types. `awaitTx` calls the emulator synchronously, so the bundle does
        // not link `scala.concurrent` (17 KB).
        //
        // The ledger rules (epoch boundary, stake snapshots and rewards, GOV, RATIFY and ENACT, the
        // ledger failure names: emulator-ledger-rules spec) raised it from 2.4 MiB: 2,627,400 bytes
        // after them. The emulator runs them on every tick and every submit, so the facade links
        // them all.
        const sizeInBytes = Buffer.byteLength(bundle, "utf8");
        const limit = 2.6 * 1024 * 1024;
        expect(sizeInBytes).toBeLessThan(limit);
    });
});
