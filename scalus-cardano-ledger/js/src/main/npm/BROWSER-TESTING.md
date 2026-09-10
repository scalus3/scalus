# Browser testing

Install the npm dependencies with `npm ci` (Node 20+). Tests use the generated
`scalus.js`; rebuild it from the repository root with
`sbtn scalusCardanoLedgerJS/prepareNpmPackage` after changing Scala code.

## Automated regression check

Run `npm run test:browser`. This builds the shared harness, starts a temporary
localhost server, launches headless Chrome, and fails unless
`__tests__/browser-smoke.html` reports success. The page exercises shared evaluator
tests and a typed Emulator credential query. The runner removes its browser profile
and stops the server afterward.

Chrome defaults to the standard macOS application path or `google-chrome` on Linux.
Set `CHROME_BIN` to use another Chrome/Chromium executable. The Linux Nix `#ci`
shell includes Chromium and sets `CHROME_BIN` to its pinned executable. CI-JS runs
the browser check inside that shell, without using the runner's installed Chrome.

From the repository root, run the same browser check with:

```bash
nix develop .#ci --command npm --prefix scalus-cardano-ledger/js/src/main/npm run test:browser
```

Chromium is included only on Linux; on macOS, use an installed Chrome application
or set `CHROME_BIN` explicitly.

## Interactive debugging

Run `npm run build:browser-tests`, serve this directory over HTTP, and open either:

- `__tests__/browser-smoke.html`: the same automatically executed checks used by CI.
- `test-browser.html`: the existing interactive runner with shared and legacy test
  buttons and detailed results.

Rebuild the shared harness and refresh the page after changing shared TypeScript
tests. See [README.md](README.md) for package usage.
