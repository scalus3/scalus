# Pinned Mithril WASM blob (and its authoritative JS glue)

This directory contains the upstream `@mithril-dev/mithril-client-wasm`
npm release, pinned to a specific version:

| File                                | Version  | SHA-256                                                              |
|-------------------------------------|----------|----------------------------------------------------------------------|
| `mithril_client_wasm_bg.wasm`       | 0.9.11   | `5eaa37178263cf1d7cb4da62f40264f318e89c66318ad1b0b474861dd295c519`   |
| `mithril_client_wasm.js`            | 0.9.11   | `d7a2fda84c9434fd6fec510af823f92e21e5525419d1a39d4553789ddf03389d`   |

During the M10b.P2 WASM investigation we also kept a locally-rebuilt
`--dev` / `--release` variant around for debugging, but it was removed
once the canonical NPM blob's async path was proven to work. If you
need to rebuild for diagnostic purposes, use:

```bash
env -i HOME=$HOME PATH=/usr/local/opt/llvm/bin:/usr/local/bin:... \
    CC=/usr/local/opt/llvm/bin/clang AR=/usr/local/opt/llvm/bin/llvm-ar \
    bash -c 'cd ~/packages/input-output-hk/mithril/mithril-client-wasm && \
             wasm-pack build --target nodejs --out-dir /tmp/mithril-local'
```

Then copy the blob + JS into a side path and thread a new
`MithrilAsyncRuntime.ClosureHashes` variant (hashes change per build
due to Rust compiler + wasm-bindgen versioning).

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
