# scalus-crypto-jni Hardening – Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Before the 0.1.0 release: drop the JDK fallback, fix the review findings, set a platform policy that CI enforces, and add property tests.

**Builds on:** `2026-10-07-scalus-crypto-jni.md`. Its Tasks 1–9 are done (branch `fix/ed25519-libsodium-verify`, 10 commits, unpushed). Its Tasks 10–11 (release, switch to 0.1.0) run **after** this plan.

**Spec:** the decisions below (owner, 2026-10-08) and the 3 review reports summarised in the conversation.

## Decisions

1. **No JDK fallback.** If the native library does not load, Ed25519, secp256k1 and BLS fail with one clear error.
2. **Platform policy (option 2):**
   - Linux glibc ≥ 2.34: Ubuntu 22.04+, Debian 12+, RHEL 9+, Amazon Linux 2023.
   - macOS 11.0+ on arm64, 10.15+ on x64.
   - Windows x64.
   - Alpine/musl is not supported.
   - Measured on 2026-10-08: today's nix build needs glibc 2.33 (`fstat@GLIBC_2.33`). It loads on `ubuntu:22.04` and `rockylinux:9`, and fails on `ubuntu:20.04`.
3. Keep the hand-written JNI (no SWIG, no FFM).

## Global Constraints

- The C wrappers add no checks beyond those cardano-crypto-class makes (see the base plan's "Call shapes").
- TDD for every behaviour change: failing test first.
- In this worktree: one-shot `sbt -Dsbt.supershell=false -Dsbt.log.noformat=true`; never two sbt at once. JNI module: `cd scalus-crypto-jni && nix develop ..#ci-crypto --command bash -c '...'`.
- After any JNI change, republish the snapshot (`sbt 'set ThisBuild / version := "0.1.0-SNAPSHOT"' publishLocal` in the module).
- `sbt scalafmtAll` before every Scalus commit; add files by path; no co-author lines; do not push.

---

### Task 1: remove the JDK fallback (about 30 min)

**Files:** delete `scalus-core/jvm/src/main/scala/scalus/crypto/ed25519/JdkEd25519Verifier.scala` and `scalus-core/jvm/src/test/scala/scalus/crypto/ed25519/JdkEd25519VerifierTest.scala`; modify `JvmEd25519Verifier.scala`.

- [ ] Replace the body of `JvmEd25519Verifier.verify` with a direct call to `Sodium.ed25519VerifyDetached(sig, msg, pk)`. The "native library loaded" check moves into the Java wrappers (Task 2, step 2).
- [ ] Delete both files. Grep for `JdkEd25519Verifier`; nothing may remain.
- [ ] Keep `Ed25519LibsodiumRules` and its test. JS uses them.
- [ ] Run `scalusJVM/testOnly scalus.crypto.ed25519.*` and commit: `refactor(jvm): drop the JDK Ed25519 fallback; the native library is required`.

### Task 2: harden the JNI library (about 2 h)

**Files:** `scalus-crypto-jni/native/*.c`, `jni_util.h`, `onload.c`, the Java classes and tests, `Makefile`.

- [ ] **Tests first** (`BlstTest`, `Secp256k1Test`, `SodiumTest`):
  - `p1Uncompress` of an on-curve point that is not in G1 throws `"BLST_ERROR: point is not in group"`. Take the vector from plutus-conformance `bls12_381_G1_uncompress/out-of-group`.
  - `p1Msm` with an infinity point mixed in: `Msm([g, inf], [3, 5]) == Mult(g, 3)`.
  - `p1Msm` with 0 pairs gives the zero point.
  - `p1HashTo` with an empty DST gives the conformance `hash-empty-dst` point `9019067b…`.
  - A `null` argument throws `NullPointerException` and does not crash the JVM.
  - `Secp256k1.isValidEcdsaSignature`: `r = n` gives false; `r = 0` gives true (it parses).
- [ ] **Java wrappers:** every public method calls `CryptoJni.requireEnabled()` and `Objects.requireNonNull` on each argument. `requireEnabled()` throws `IllegalStateException("scalus-crypto-jni native library not available on <os>/<arch>")`.
- [ ] **New:** `Secp256k1.isValidEcdsaSignature(byte[] sig64)`, which calls `secp256k1_ecdsa_signature_parse_compact`. Cardano raises an error exactly when this fails.
- [ ] **C:**
  - Move `read_all` into `jni_util.h` as `read_bytes`. Use it in all 3 files. Remove the `empty[1]` buffers and the `GetPrimitiveArrayCritical` path.
  - MSM: check every `malloc`, clean up with `goto cleanup`, and throw `OutOfMemoryError`. Read points straight into the `POINT *` buffer.
  - Use `secp256k1_context_static` (it exists in v0.3.2). `onload.c` keeps only `sodium_init`.
  - Make `isValidXOnlyPubKey` return false on a wrong length, like `isValidPubKey`.
- [ ] **Makefile:**
  - Linux: add `-Wl,--exclude-libs,ALL -Wl,-Bsymbolic`, so only the JNI entry points are exported and our calls cannot bind to another libsodium in the process.
  - macOS: `MACOSX_DEPLOYMENT_TARGET=11.0` (arm64) or `10.15` (x64), and `-install_name @rpath/libscalus_crypto.dylib`.
  - Fail with `$(error ...)` if a `*_HOME` variable is unset or `PLATFORM` is empty.
  - Make the target depend on the 3 `.a` files.
  - The macOS target must also reach the 3 libraries. Set it for the nix derivations on Darwin, or the linker warns about objects built for a newer OS.
- [ ] Check: `nm` shows only `Java_*` and `JNI_OnLoad` as exported text symbols (Linux, checked in CI); `otool -l` shows `minos 11.0` on arm64.
- [ ] Run `make && sbt test`, publish the snapshot, and commit: `fix(crypto-jni): check allocations and nulls, export only JNI symbols, target macOS 11`.

### Task 3: CI enforces the platform policy (about 1 h)

**Files:** `.github/workflows/crypto-jni.yml`, `scalus-crypto-jni/README.md`, `CONTRIBUTING.md`.

- [ ] Linux jobs, after `make`:
  - `readelf -d` lists only `libc.so.6` and the `ld-linux` loader;
  - the highest `GLIBC_x` in `objdump -T` is ≤ 2.34 (compare with `sort -V`);
  - exported dynamic symbols are only `Java_*` and `JNI_OnLoad`.
- [ ] New `test-linux-distros` job: matrix `ubuntu:22.04` and `rockylinux:9`, on x64 and arm64 runners. Install JDK 17, download the native artifact, run `sbt test` in the container. This proves the glibc floor on real systems.
- [ ] macOS jobs: `otool -l` shows `minos` ≤ 11.0 (arm64) or ≤ 10.15 (x64).
- [ ] Pin `DeterminateSystems/magic-nix-cache-action` to a commit SHA. Drop `id-token: write` from the build jobs (only publishing needs it). Drop `max-parallel`.
- [ ] Docs: the README "Platforms" section and CONTRIBUTING state the policy (glibc 2.34+, macOS 11/10.15, Windows x64, no Alpine) and how CI enforces it.
- [ ] Validate with `actionlint`, then commit: `ci(crypto-jni): enforce the glibc 2.34 and macOS 11 floors, test on Ubuntu 22.04 and Rocky 9`.

### Task 4: simplify the Scala code (about 2 h)

- [ ] **ECDSA on the JVM:** replace the `BigInt(...) < SECP256K1_ORDER` checks with `require(Secp256k1.isValidEcdsaSignature(sig.bytes), ...)`. JS keeps its `BigInt` check, because noble has no parse step.
- [ ] **One `blsScalar`** in shared `PlatformSpecific` (`n mod r`, 32-byte big-endian), used by JVM and Native.
- [ ] **One JVM MSM helper** for G1 and G2. Copy into arrays sized up front instead of `flatMap(...).toArray`.
- [ ] **`Builtin.scala`:** one `private val ensurable = variant == D || variant == E`, replacing the 3 copies.
- [ ] **BLS types:** the JVM `G1Element`/`G2Element` constructors become `private[builtin]`. `MLResult.hashCode = Arrays.hashCode(value)`, which is valid because `blst_fp12_is_equal` compares raw bytes.
- [ ] **`Ed25519LibsodiumRules`:** compare bytes as libsodium does (`lessThanLE` with a mask for byte 31) instead of `BigInt`. Keep the same API. The rules test and the 930-vector parity test are the gate on all 3 platforms.
- [ ] **JS:**
  - one `bytesToBigInt` (via `Hex.bytesToHex(bytes.reverse)`);
  - one `L`;
  - `concatBytes` for `R ‖ A ‖ M`;
  - move `JsEd25519Verifier` to its own file.
- [ ] Run the BLS, Ed25519, CekBuiltins and conformance tests on JVM, JS and Native, run `mima`, and commit: `refactor: simplify the crypto glue after the move to scalus-crypto-jni`.

### Task 5: fix weak tests (about 1 h)

- [ ] `CekBuiltinsTest`:
  - ECDSA with high-S (`s' = n - s`) gives **False**;
  - `s = n` gives an **error**.
- [ ] `BLS12_381BuiltinsTest`: the hashCode tests compare `add(p, p)` with `scalarMul(2, p)`, not 2 identical `uncompress` results.
- [ ] `BLS12_381ScalarMulBoundTest`: add an independent check, `scalarMul(ub) == scalarMul(ub mod r)` and `scalarMul(lb) == neg(scalarMul((-lb) mod r))`.
- [ ] `Ed25519LibsodiumRulesTest`:
  - a table over all 7 small-order encodings, with and without the sign bit;
  - a point with y < p and the sign bit set is canonical;
  - the public-key pre-checks on their own.
- [ ] Structure:
  - `LibsodiumVerdicts.assertParity(verify)` replaces the 3 copies of the mismatch block;
  - the generator writes `# rows=930 accepts=45`, and both parsers assert it;
  - delete the dead `blstLargeDstCases`.
- [ ] Mutation check for each new assertion: break the code, see the test fail, restore. Commit: `test: cover ECDSA high-S, BLS hashing and Ed25519 pre-check edge cases`.

### Task 6: property tests (about 1.5 h)

Use `AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances`. The repo already has `g1ElementArbitrary`, `g2ElementArbitrary` and `genByteStringOfN`.

- [ ] **`scalus-core/shared/.../uplc/builtin/BLS12_381PropertiesTest.scala`**, for G1 and G2:
  - **MSM equals the sum of `scalarMul`** for 0–5 pairs, with scalars from {0, r, a random value in [-3r, 3r]} and points that may be zero;
  - `uncompress(compress(p)) == p`;
  - `p + neg(p) == zero`;
  - `finalVerify(millerLoop([a]P, Q), millerLoop(P, [b]Q)) == (a mod r == b mod r)`.
  - Use `minSuccessful = 20` on JS, where noble G2 is slow.
- [ ] **`scalus-core/shared/.../crypto/ed25519/Ed25519PropertiesTest.scala`**:
  - `verify(pk, msg, sign(sk, msg))` is true;
  - flipping any single bit of sig, msg or pk makes it false.
  - It runs through `platform` and each platform's `Ed25519Signer`.
- [ ] Not added: BLS associativity and commutativity (blst is the oracle), random-bytes pre-check agreement (proves nothing), secp256k1 r/s sweeps (boundary examples suffice).
- [ ] Run on JVM, JS and Native; time each suite (target under 10 s on JVM). Commit: `test: property tests for BLS12-381 MSM and pairing, and Ed25519 sign-verify`.

### Task 7: docs and cleanup (about 45 min)

- [ ] Fix stale comments:
  - `Ed25519Signer.scala:8` ("JVM: BouncyCastle" → verify with libsodium via scalus-crypto-jni, sign with BouncyCastle);
  - `build.sbt:605` and `flake.nix:223-227,330` (blst-java conflicts);
  - the blst-java crash note in `CONTRIBUTING.md`;
  - `PlatformSpecific.scala` blst-java mentions.
- [ ] CONTRIBUTING "Current state" table: the JVM column becomes "node pins via scalus-crypto-jni"; add the platform policy line.
- [ ] Commit: `docs: update the crypto docs for scalus-crypto-jni and the platform policy`.

---

## After this plan (base plan, unchanged)

1. Owner pushes the branch; the `crypto-jni` workflow must pass on 5 platforms plus the 2 distro containers.
2. Owner tags `crypto-jni-v0.1.0`.
3. Switch `build.sbt` to 0.1.0, add the CHANGELOG entry (breaking `P1`/`P2`/`PT` removal; native library required on the JVM; platform policy), and run the full gate.

**Total for this plan:** about 9 hours.

## Out of scope (separate questions)

- Delete the frozen `scalus-secp256k1-jni` module, its workflow, the `ci-secp` shell and `secp256k1Static`.
- noble 2.x (blocked on paulmillr/noble-curves#259).
- Plutus 1.70 corpus.
- Native linking the node's pins instead of nixpkgs' libraries.
