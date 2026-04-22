# Pinned Mithril WASM blob

The `mithril_client_wasm_bg.wasm` in this directory is the upstream
`@mithril-dev/mithril-client-wasm` blob, pinned to a specific npm
release. Tests load it from the classpath at
`/mithril/mithril_client_wasm_bg.wasm`.

| Version | SHA-256                                                            |
|---------|--------------------------------------------------------------------|
| 0.9.11  | `5eaa37178263cf1d7cb4da62f40264f318e89c66318ad1b0b474861dd295c519` |

To refresh:

```bash
curl -sL https://registry.npmjs.org/@mithril-dev/mithril-client-wasm/-/mithril-client-wasm-<VERSION>.tgz \
  | tar xz -C /tmp && \
  cp /tmp/package/dist/node/mithril_client_wasm_bg.wasm <this-dir>/
shasum -a 256 <this-dir>/mithril_client_wasm_bg.wasm
```

Then update the version + SHA-256 in this README and the test's
expected version pin.
