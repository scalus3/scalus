# Scalus 0.14.2 Release Announcement

## Discord Version

🚀**Scalus 0.14.2** is on Maven Central🎉

**Scala Native - Full Plutus V3**
BLS12-381 via blst FFI. Pure Scala Keccak-256, SHA3-256, RIPEMD-160. No more Rust dependencies.

**Other highlights:**

- `scalus-secp256k1-jni` replaces bitcoin-s (secp256k1 0.6.0)
- Type-safe `Blueprint.plutusV3` for CIP-57 blueprints
- `ToData`/`FromData` for Conway protocol params
- 108 new Plutus conformance tests

**Fixes:** collateral check, ECDSA on JS, overflow in `NonNegativeInterval`, `Data.Map` empty list,
Java 22+ BLST native access

Changelog: https://github.com/scalus3/scalus/blob/v0.14.2/CHANGELOG.md#0142-2025-12-31
Docs: https://scalus.org

---

## Twitter/X Version

**Single Tweet:**

Scalus 0.14.2: Full Plutus V3 on Scala Native

- BLS12-381 via blst FFI
- Pure Scala Keccak-256, SHA3-256, RIPEMD-160
- New scalus-secp256k1-jni (replaces bitcoin-s)
- Type-safe Blueprint.plutusV3 for CIP-57

https://scalus.org

---

**Thread:**

**1/2**
Scalus 0.14.2 released

Scala Native now has full Plutus V3 support:

- BLS12-381 crypto via blst FFI
- Pure Scala Keccak-256, SHA3-256, RIPEMD-160
- No more Rust wrapper dependencies

**2/2**
Also new:

- `scalus-secp256k1-jni` replaces bitcoin-s
- Type-safe `Blueprint.plutusV3` for CIP-57
- `ToData`/`FromData` for Conway protocol params
- 108 new conformance tests + various fixes

https://scalus.org
