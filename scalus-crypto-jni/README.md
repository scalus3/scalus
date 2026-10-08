# scalus-crypto-jni

JNI bindings for the C crypto libraries that cardano-node links, at the same commits, called the
same way cardano-crypto-class calls them. Scalus uses them on the JVM for Ed25519, secp256k1 and
BLS12-381, so its verdicts match the node's.

## Pinned libraries

Pinned to cardano-node 11.1.3, through its root `iohkNix` input (iohk-nix `74bae4f8`):

| Library | Commit | Version |
|---|---|---|
| libsodium (IOG fork) | `input-output-hk/libsodium@dbb48cce5429cb6585c9034f002568964f1ce567` | 1.0.18 verification code |
| libsecp256k1 | `bitcoin-core/secp256k1@acf5c55ae6a94e5ca847e07def40427547876101` | v0.3.2 |
| blst | `supranational/blst@6d960cd05d6fe2b5bc9ba161edf0c1a131b87c4c` | v0.3.15 |

The pins live in the root `flake.nix`. To bump them, follow "Keeping crypto libraries in sync with
cardano-node" in `CONTRIBUTING.md`.

## API

Package `scalus.crypto.jni`. All inputs and outputs are `byte[]`. A wrong length throws
`IllegalArgumentException`, and so does every input that cardano-crypto-class rejects with an
error: a secp256k1 key or ECDSA signature that does not parse, or a BLS point that does not
uncompress. A `null` argument throws `NullPointerException`. If the native library did not load, every method throws
`IllegalStateException`. The wrappers add no checks of their own: the library decides.

| Class | Methods |
|---|---|
| `CryptoJni` | `isEnabled()`: true if the native library loaded; `requireEnabled()` |
| `Sodium` | `ed25519VerifyDetached(sig, msg, pk)` |
| `Secp256k1` | `ecdsaVerify(msg32, sig64, pk33)`, `schnorrVerify(sig64, msg, pk32)` |
| `Blst` | per group `p1`/`p2`: `Uncompress`, `Compress`, `AddOrDouble`, `Neg`, `Mult`, `IsEqual`, `HashTo`, `Msm`; and `millerLoop`, `fp12Mul`, `fp12IsEqual`, `finalVerify` |

BLS points and Miller-loop results are raw blst structs (`blst_p1` 144 bytes, `blst_p2` 288 bytes,
`blst_fp12` 576 bytes). Pass only arrays that `Blst` returned.

```java
import scalus.crypto.jni.Blst;

byte[] p = Blst.p1HashTo(msg, dst);          // the DST is raw bytes, any value
byte[] compressed = Blst.p1Compress(p);      // 48 bytes
```

## Platforms

`linux_64`, `linux_arm64`, `osx_64`, `osx_arm64`, `windows_64`, loaded with `native-lib-loader`.

| OS | Minimum | Examples |
|---|---|---|
| Linux (glibc) | glibc 2.34 | Ubuntu 22.04+, Debian 12+, RHEL/Rocky 9+, Amazon Linux 2023 |
| macOS | 11.0 (arm64), 10.15 (x64) | |
| Windows | x64 | |

Not supported: Alpine and other musl-based Linux, and glibc older than 2.34 (for example Ubuntu
20.04 or RHEL 8). There the library does not load, and Ed25519, secp256k1 and BLS12-381 fail with
`IllegalStateException`.

CI enforces the minimums: `ci/check-linux.sh` rejects any symbol newer than glibc 2.34, any
library other than libc, and any exported symbol other than the JNI entry points;
`ci/check-macos.sh` checks `minos`. The tests also run in `ubuntu:22.04` and `rockylinux:9`.

## Build

From this directory:

```bash
nix develop ..#ci-crypto --command bash -c 'make && sbt test'        # this platform
nix develop ..#ci-crypto-windows --command bash -c 'make windows'    # Windows x64, Linux host only
```

The tests check the 930 Ed25519 vectors in
`../scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv`, the CIP-49 secp256k1
vectors, and BLS hashing with a non-ASCII DST.

## Release

Push a `crypto-jni-v<version>` tag. The `crypto-jni` workflow builds and tests all 5 platforms and
publishes `org.scalus:scalus-crypto-jni` to Maven Central.
