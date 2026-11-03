# LEDGER vectors, PV11

`ledger-vectors-pv11.tar.gz` holds 932 LEDGER transitions of the Cardano ledger at protocol
version 11.0, one tx each. `DugiteLedgerConformanceTest` runs them (ledger-rules spec
`docs/superpowers/specs/2026-11-02-emulator-ledger-rules-design.md`, rules `[CV-21]` to
`[CV-25a]`). The same test runs the full corpus when `-Dscalus.dugite.ledgerRules` or
`SCALUS_DUGITE_LEDGER_RULES` names a local `ledger-rules.tar.gz`.

## Source

- Project: Dugite, <https://github.com/michaeljfazio/dugite>, license Apache-2.0.
- Release: `conformance-corpus-v20261101-084724`, asset `ledger-rules.tar.gz`
  (sha256 `018c480caad1abf311b7203998d0e905b743d17ee26213edc4575d027faab996`).
- Generator: the cardano-ledger conformance dump hook (`CONFORMANCE_CBOR_DUMP_PATH`), which
  Dugite patches to dump every case of the Conway Imp test suite.
- cardano-ledger commit `a88b60bd` (master, `cardano-ledger-conway` 1.23.0.0), protocol
  version 11.0 in every case. This is not the production pin of Scalus (1.22.1.0).
- The test names are not in the dump. A case is `test_NNNN` only. No file records the failure
  of a rejected tx.

## Layout

```
LEDGER/test_NNNN/conformance_dump_env.cbor     the LedgerEnv: [slot, [epoch], txIx, pparams, [treasury, reserves]]
LEDGER/test_NNNN/conformance_dump_st.cbor      the [certState, utxoState] before the tx
LEDGER/test_NNNN/conformance_dump_sig.cbor     the tx
LEDGER/test_NNNN/conformance_dump_st_out.cbor  the [certState, utxoState] after the tx; absent when the ledger rejects it
```

The `ctx` files of the release are left out.

## Selection

The corpus has 7 446 LEDGER cases, 93 of them rejected (no `st_out`), and 6 918 distinct
`(env, st, sig)` triples. The sample holds:

- every rejected case (93, all distinct);
- every accepted case whose five files, `ctx` included, total less than 30 000 bytes, the first
  case in numeric order of each `(env, st, sig)` triple (839 of 1 336).

That is 932 cases, 637 123 bytes.

## How it was made

`python3 -I sample.py ledger-rules.tar.gz ledger-vectors-pv11.tar.gz`, with `sample.py`:

```python
# Usage: python3 -I sample.py ledger-rules.tar.gz ledger-vectors-pv11.tar.gz
import gzip, hashlib, io, sys, tarfile

src, out = sys.argv[1], sys.argv[2]
cases, size = {}, {}  # case -> {file: bytes} without ctx; case -> bytes of all five files
with tarfile.open(src, 'r|gz') as t:
    for m in t:
        p = m.name.removeprefix('./').split('/')
        if len(p) != 3 or p[0] != 'LEDGER' or not m.isfile():
            continue
        size[p[1]] = size.get(p[1], 0) + m.size
        if p[2] != 'conformance_dump_ctx.cbor':
            cases.setdefault(p[1], {})[p[2]] = t.extractfile(m).read()

def key(files):
    return b''.join(hashlib.sha256(files[f'conformance_dump_{k}.cbor']).digest()
                    for k in ('env', 'st', 'sig'))

number = lambda name: int(name.removeprefix('test_'))
seen, selected = set(), []
for name in sorted(cases, key=number):
    files = cases[name]
    if 'conformance_dump_st_out.cbor' not in files:
        selected.append(name)  # every rejected case
    elif size[name] < 30_000 and key(files) not in seen:
        seen.add(key(files))
        selected.append(name)  # a small accepted case, the first of its (env, st, sig)
print(len(cases), 'cases,', len(selected), 'selected')

with open(out, 'wb') as raw, gzip.GzipFile(filename='', mode='wb', fileobj=raw, mtime=0) as gz, \
        tarfile.open(fileobj=gz, mode='w', format=tarfile.USTAR_FORMAT) as tar:
    for name in selected:
        for file, data in sorted(cases[name].items()):
            info = tarfile.TarInfo(f'LEDGER/{name}/{file}')
            info.size, info.mode = len(data), 0o644
            tar.addfile(info, io.BytesIO(data))
```

The output is byte-identical between runs (gzip mtime 0, no file name). Each of its 3 635 files
matches its sha256 in the `corpus-manifest.json` of the release (checked 2026-11-03).
