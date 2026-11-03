# NEWEPOCH vectors

`newepoch-vectors.tar.gz` holds 533 NEWEPOCH transitions of the Cardano ledger, one epoch
boundary each. `NewEpochConformanceTest` runs them (ledger-rules spec
`docs/superpowers/specs/2026-11-02-emulator-ledger-rules-design.md`, rules `[CV-9]` to
`[CV-13]`).

## Source

- Project: Dugite, <https://github.com/michaeljfazio/dugite>, license Apache-2.0.
- Release: `conformance-corpus-v20261101-084724`, asset `ledger-rules.tar.gz`
  (sha256 `018c480caad1abf311b7203998d0e905b743d17ee26213edc4575d027faab996`).
- Generator: the cardano-ledger conformance dump hook (`CONFORMANCE_CBOR_DUMP_PATH`), which
  Dugite patches to dump every case of the Conway Imp test suite.
- cardano-ledger commit `a88b60bd` (master, `cardano-ledger-conway` 1.23.0.0), protocol
  version 11.0 in every case. This is not the production pin of Scalus (1.22.1.0).
- The test names are not in the dump. A case is `test_NNNN` only.

## Layout

```
newepoch/NEWEPOCH/test_NNNN/conformance_dump_sig.cbor     the boundary epoch, an EpochNo
newepoch/NEWEPOCH/test_NNNN/conformance_dump_st.cbor      the NewEpochState before the boundary
newepoch/NEWEPOCH/test_NNNN/conformance_dump_st_out.cbor  the NewEpochState after the boundary
```

The `ctx` and `env` files of the release are left out: each is the CBOR `null` (`f6`).

## How it was made

Extract the `NEWEPOCH` directory of `ledger-rules.tar.gz` into `newepoch/`, then:

```bash
COPYFILE_DISABLE=1 tar --no-xattrs -czf newepoch-vectors.tar.gz \
    --exclude='*_ctx.cbor' --exclude='*_env.cbor' newepoch
```

Each extracted file matches its sha256 in the `corpus-manifest.json` of the release
(checked 2026-11-03, all 2665 `NEWEPOCH/` entries).
