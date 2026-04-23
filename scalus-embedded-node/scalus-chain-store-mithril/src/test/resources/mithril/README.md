# Pinned Mithril WASM blob (and its authoritative JS glue)

This directory contains the upstream `@mithril-dev/mithril-client-wasm`
npm release, pinned to a specific version:

| File                                | Version  | SHA-256                                                              |
|-------------------------------------|----------|----------------------------------------------------------------------|
| `mithril_client_wasm_bg.wasm`       | 0.9.11   | `5eaa37178263cf1d7cb4da62f40264f318e89c66318ad1b0b474861dd295c519`   |
| `mithril_client_wasm.js`            | 0.9.11   | `d7a2fda84c9434fd6fec510af823f92e21e5525419d1a39d4553789ddf03389d`   |
| `mithril_client_wasm_bg.debug.wasm` | 0.9.11*  | `b6305304a03aa8f867ebe0c71f9bc1958e68d08e80d73a8d2f59e26da584ee51`   |

`mithril_client_wasm_bg.debug.wasm` is a locally-rebuilt `--dev`
profile variant of 0.9.11. Initially built with
`console_error_panic_hook` enabled to catch panics; later rebuilt
without the hook after investigation showed the panic hook was not
the source of the outstanding Chicory trap. Current version has no
hook. It is NOT the upstream npm artefact (the `*` on the version
denotes this); rebuild instructions live in the commit history and
require `wasm-pack` with a wasm32-capable LLVM + Rust 1.85-era
stable. See `MithrilAsyncRuntime.ClosureHashes.Debug0_9_11` for the
closure-hash mapping this build produces.

Tests load the `.wasm` from the classpath at
`/mithril/mithril_client_wasm_bg.wasm`.

## Why we ship the JS glue alongside

The `.js` file is the **authoritative specification of host-import
semantics** for this pinned version. wasm-bindgen appends a 16-hex
signature hash to every import (e.g. `__wbg_new_ff12d2b041fb48f1`);
the same short name can map to very different JS ctors depending on
the hash (Error vs Promise-executor vs BroadcastChannel). We read the
JS glue to learn which hash means what, then register hash-specific
bridges in `WbindgenAbi`. The glue also documents the three
`wasm_bindgen__convert__closures_____invoke__h*` invoke entry points
and the two `__wbindgen_cast_*`-allocated closure shims.

On pin bump: refresh both files, **re-read the JS glue**, and update
hash-specific handlers in `WbindgenAbi` as needed. Short-name
handlers continue to match via `MithrilWasmRuntime.stripHash`.

## Refresh

```bash
cd /tmp && npm pack @mithril-dev/mithril-client-wasm@<VERSION>
tar xzf mithril-dev-mithril-client-wasm-<VERSION>.tgz
cp package/dist/node/mithril_client_wasm_bg.wasm <this-dir>/
cp package/dist/node/mithril_client_wasm.js      <this-dir>/
shasum -a 256 <this-dir>/*.wasm <this-dir>/*.js
```

Then update the version + SHA-256 entries in this README and any
hash-specific bindings that changed in `WbindgenAbi.pinnedImports`.
