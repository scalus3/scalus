# scalus-crypto-jni – the Node's Crypto Libraries on the JVM – Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** On the JVM, Ed25519, secp256k1 and BLS12-381 run the exact C libraries that cardano-node 11.1.3
links. They are called the way cardano-crypto-class calls them, through one new JNI artifact,
`org.scalus:scalus-crypto-jni`.

**Architecture:** One native library, `scalus_crypto`, statically links libsodium (IOG fork), libsecp256k1
and blst at the node's pins, built by nix for 5 platforms (Windows x64 cross-compiled with MinGW, as the
node does). Thin Java classes in `scalus.crypto.jni` take and return `byte[]`. Each C wrapper mirrors the
cardano-crypto-class function it replaces, with no extra checks. Scalus's JVM platform switches from
`foundation.icon:blst-java` and `scalus-secp256k1-jni` to it.

**Tech Stack:** C (JNI), nix (`pkgsCross.mingwW64`), Java 11, sbt, JUnit (`junit-interface`), GitHub Actions.

**Builds on:** `2026-10-07-ed25519-libsodium-verify.md`, Tasks 1–4. They are committed on this branch and stay.
This plan replaces its Task 5 and supersedes `2026-10-07-ed25519-libsodium-jni.md` (deleted).

**Branch / worktree:** `fix/ed25519-libsodium-verify`, `.claude/worktrees/ed25519-libsodium`.

## Decisions (owner: confirm before Task 7)

1. **Names and version.** Directory `scalus-crypto-jni/`, artifact `org.scalus:scalus-crypto-jni`, first
   version `0.1.0`, tags `crypto-jni-v*`, Java package `scalus.crypto.jni`, native library `scalus_crypto`.
   The new package avoids duplicate classes if a project still has `scalus-secp256k1-jni` 0.6.0 on its
   classpath. `scalus-secp256k1-jni` is frozen at 0.6.0.
2. **Breaking: `G1Element.apply(P1)`, `G2Element.apply(P2)`, `MLResult.apply(PT)` are removed.** They take
   blst-java types, and blst-java leaves the classpath. Recommended: remove, add MiMa filters, and add a
   **Breaking** CHANGELOG line. The alternative, keeping blst-java only for deprecated overloads, loads a
   second blst into the JVM.
3. **Windows x64 in the first release.** Recommended.

## Normative facts (checked 2026-10-07)

**Pins.** cardano-node 11.1.3 resolves crypto through its **root** `iohkNix` input, which is the lock node
`iohkNix_2` = `input-output-hk/iohk-nix@74bae4f8427769ac0952b245b362f9e05520170c`. The lock node `iohkNix`
(`f444d972`, blst v0.3.14) belongs to `cardano-dev` and does not count.

| Library | Repository | Commit | Tag |
|---|---|---|---|
| libsodium | `input-output-hk/libsodium` | `dbb48cce5429cb6585c9034f002568964f1ce567` | (1.0.18 + VRF) |
| libsecp256k1 | `bitcoin-core/secp256k1` | `acf5c55ae6a94e5ca847e07def40427547876101` | v0.3.2 |
| blst | `supranational/blst` | `6d960cd05d6fe2b5bc9ba161edf0c1a131b87c4c` | v0.3.15 |

**Node build recipes** (iohk-nix `overlays/crypto/` at `74bae4f8`):
- libsodium: `autoreconfHook`; `--enable-static`; MinGW adds `CFLAGS=-fno-stack-protector`; `doCheck = true`.
- secp256k1: `autoreconfHook`; `--enable-benchmark=no --enable-module-recovery`; `doCheck = true`. In
  v0.3.2 the ecdh, extrakeys and schnorrsig modules are on by default (`configure.ac:173-186`).
- blst: `./build.sh -D__BLST_PORTABLE__` (plus `flavour=mingw64` on Windows); `doCheck = true`.

We add only `--with-pic`/`-fPIC` and `--disable-shared`, which change packaging, not code.

**Call shapes** (cardano-crypto-class 2.5.1.0 = cardano-base `060819b5`,
`Cardano/Crypto/EllipticCurve/BLS12_381/Internal.hs`, and `Cardano/Crypto/DSIGN/*.hs`):
- Ed25519: `crypto_sign_ed25519_verify_detached(sig, msg, len, vk) == 0`. Plutus checks lengths 32/64 first.
- ECDSA: parse 33-byte key (error) → `ecdsa_signature_parse_compact` (error only on overflow) → `ecdsa_verify` (False). No S normalization.
- Schnorr: `xonly_pubkey_parse` (error) → `schnorrsig_verify(ctx, sig64, msg, len, pk)` (False).
- BLS `uncompress`: right length, `blst_pX_uncompress` (error if not 0), `from_affine`, `blst_pX_in_gX` (error if not).
- BLS `add`: `blst_pX_add_or_double`. `neg`: copy + `blst_pX_cneg(p, true)`. `equal`: `blst_pX_is_equal`.
- BLS `scalarMul`: `n mod r`, 32-byte big-endian, `blst_scalar_from_bendian`, `blst_pX_mult(out, p, scalar, 256)`.
- BLS `hashToGroup`: Plutus fails if `len(dst) > 255`; then `blst_hash_to_gX(out, msg, len, dst, dstLen, NULL, 0)`.
- BLS MSM: drop pairs whose point is infinity or whose scalar is 0 mod r; none → zero (uncompress of
  `0xc0 00…`); one → `scalarMul`; else `blst_pXs_to_affine` + `blst_pXs_mult_pippenger(…, nbits = 255, …)`.
- Pairing: `millerLoop(p1, p2)` = `blst_miller_loop(ret, affine(p2), affine(p1))`; `mulMlResult` =
  `blst_fp12_mul`; `finalVerify` = `blst_fp12_finalverify`; ML-result equality = `blst_fp12_is_equal`.

## Global Constraints

- **Wrappers add no checks** beyond the ones listed in "Call shapes". Any extra check is a divergence.
- Point and ML-result `byte[]`s are raw blst structs (`blst_p1` 144 B, `blst_p2` 288 B, `blst_fp12` 576 B)
  that only this API produces. Wrappers check their lengths and nothing else.
- Keep `JdkEd25519Verifier` and `Ed25519LibsodiumRules`: JS and the Ed25519 fallback use them. Ed25519 is
  the only operation with a fallback; secp256k1 and BLS require the native library, as today.
- Do not touch `scalus-secp256k1-jni/` or its workflow.
- Nix flakes see only git-tracked files: `git add` before `nix develop`/`nix build`.
- JNI module commands: from `scalus-crypto-jni/`, inside `nix develop ..#ci-crypto`. Scalus commands:
  one-shot `sbt -Dsbt.supershell=false -Dsbt.log.noformat=true` from the worktree root.
- ScalaTest failures print as `[info]`; read `Tests: succeeded N, failed M`.
- `sbt scalafmtAll` before every Scalus commit. Never `git add -A`. No co-author lines.
- **Pushing a `crypto-jni-v*` tag publishes to Maven Central. Only the owner does that (Task 10).** Do not
  push the branch or dispatch workflows without an explicit request.

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `flake.nix` | modify | 3 pinned inputs; static builds for host and MinGW; `ci-crypto`, `ci-crypto-windows` shells |
| `.gitignore` | modify | ignore built natives and `build/` of the new module |
| `scalus-crypto-jni/{build.sbt,project/build.properties,project/plugins.sbt}` | create | standalone Java build, `crypto-jni-v` tags |
| `scalus-crypto-jni/Makefile` | create | host and `windows` targets |
| `scalus-crypto-jni/native/onload.c` | create | `JNI_OnLoad`: `sodium_init`, secp256k1 context |
| `scalus-crypto-jni/native/jni_util.h` | create | shared helpers: exact-length read, new array, throw |
| `scalus-crypto-jni/native/sodium.c` | create | Ed25519 verify |
| `scalus-crypto-jni/native/secp256k1.c` | create | ECDSA, Schnorr, pubkey check |
| `scalus-crypto-jni/native/blst.c` | create | G1, G2, pairing, MSM |
| `scalus-crypto-jni/src/main/java/scalus/crypto/jni/{CryptoJni,Sodium,Secp256k1,Blst}.java` | create | Java API |
| `scalus-crypto-jni/src/test/java/scalus/crypto/jni/{SodiumTest,Secp256k1Test,BlstTest}.java` | create | per-platform checks |
| `scalus-crypto-jni/README.md` | create | API, pins, build |
| `.github/workflows/crypto-jni-release.yml` | create | 5-platform build + test + publish |
| `scalus-core/jvm/.../crypto/ed25519/JvmEd25519Verifier.scala` | create | native Ed25519, else JDK |
| `scalus-core/jvm/.../uplc/builtin/JVMPlatformSpecific.scala` | modify | use `scalus.crypto.jni` |
| `scalus-core/jvm/.../uplc/builtin/bls12_381/{G1Element,G2Element,MLResult}.scala` | modify | hold `Array[Byte]` |
| `scalus-core/jvm/.../crypto/ed25519/JvmEd25519Signer.scala` | modify | `verify` via `JvmEd25519Verifier` |
| `scalus-core/native/.../NativePlatformSpecific.scala` | modify | `blst_pX_add_or_double` (Task 8) |
| `scalus-core/jvm/src/test/.../eval/PlutusConformanceJvmTest.scala`, native counterpart | modify | drop DST skips |
| `build.sbt` | modify | dependencies, MiMa filters |
| `CONTRIBUTING.md`, `CHANGELOG.md` | modify | state table, publishing, changelog |

---

### Task 1: nix – the three libraries at the node's pins

**Files:** `flake.nix`

- [ ] **Step 1: Inputs** (next to `plutus.url`; add `sodium`, `blst`, `secp256k1` to the `outputs` args)

```nix
    # The C crypto libraries exactly as cardano-node 11.1.3 links them: its root iohkNix input
    # (iohk-nix 74bae4f8). Keep in step with CONTRIBUTING.md "Keeping crypto libraries in sync".
    sodium = { url = "github:input-output-hk/libsodium/dbb48cce5429cb6585c9034f002568964f1ce567"; flake = false; };
    secp256k1 = { url = "github:bitcoin-core/secp256k1/acf5c55ae6a94e5ca847e07def40427547876101"; flake = false; };
    blst = { url = "github:supranational/blst/6d960cd05d6fe2b5bc9ba161edf0c1a131b87c4c"; flake = false; };
```

- [ ] **Step 2: Derivations** (after `secp256k1Static`; do not change `secp256k1Static`, the old module uses it)

```nix
      # Mirrors iohk-nix overlays/crypto/*.nix at 74bae4f8. Only packaging differs: static
      # archives with position-independent code, linked into one JNI library.
      nodeSodium = { stdenv, lib, autoreconfHook }: stdenv.mkDerivation {
        pname = "libsodium-vrf"; version = "1.0.18"; src = inputs.sodium;
        nativeBuildInputs = [ autoreconfHook ];
        configureFlags = [ "--enable-static" "--disable-shared" "--with-pic" ]
          ++ lib.optional stdenv.hostPlatform.isMinGW "CFLAGS=-fno-stack-protector";
        enableParallelBuilding = true;
        doCheck = !stdenv.hostPlatform.isMinGW;
      };
      nodeSecp256k1 = { stdenv, lib, autoreconfHook }: stdenv.mkDerivation {
        pname = "secp256k1"; version = "0.3.2"; src = inputs.secp256k1;
        nativeBuildInputs = [ autoreconfHook ];
        configureFlags = [ "--enable-benchmark=no" "--enable-module-recovery"
          "--enable-static" "--disable-shared" "--with-pic" ];
        enableParallelBuilding = true;
        doCheck = !stdenv.hostPlatform.isMinGW;
      };
      nodeBlst = { stdenv, lib }: stdenv.mkDerivation {
        pname = "blst"; version = "0.3.15"; src = inputs.blst;
        buildPhase = ''
          actual=$(grep -m1 -E '^version\s*=' bindings/rust/Cargo.toml | cut -d'"' -f2)
          [ "$actual" = "0.3.15" ] || { echo "blst version mismatch: $actual"; exit 1; }
          ./build.sh -D__BLST_PORTABLE__ -fPIC ${lib.optionalString stdenv.hostPlatform.isWindows "flavour=mingw64"}
        '';
        installPhase = ''
          mkdir -p $out/lib $out/include
          cp libblst.a $out/lib/
          cp bindings/blst.h bindings/blst_aux.h $out/include/
        '';
      };
      mingw = pkgs.pkgsCross.mingwW64;
      cryptoHost = {
        sodium = pkgs.callPackage nodeSodium { };
        secp256k1 = pkgs.callPackage nodeSecp256k1 { };
        blst = pkgs.callPackage nodeBlst { };
      };
      cryptoWindows = {
        sodium = mingw.callPackage nodeSodium { };
        secp256k1 = mingw.callPackage nodeSecp256k1 { };
        blst = mingw.callPackage nodeBlst { };
      };
      # jni_md.h for win32: Linux JDKs ship only the linux variant; jni.h is platform-neutral.
      jniMdWin32 = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/openjdk/jdk11u/jdk-11.0.24+8/src/java.base/windows/native/include/jni_md.h";
        hash = "<output of: nix store prefetch-file --json URL | jq -r .hash>";
      };
```

Compute the `hash` before the first build; never commit the placeholder. If `./build.sh` rejects `-fPIC`
on a platform, pass it as `CFLAGS=-fPIC` instead and record which form worked.

- [ ] **Step 3: Shells**

```nix
        ci-crypto = let jdk = pkgs.openjdk11; in pkgs.mkShell {
          JAVA_HOME = "${jdk}";
          SODIUM_HOME = "${cryptoHost.sodium}";
          SECP256K1_HOME = "${cryptoHost.secp256k1}";
          BLST_HOME = "${cryptoHost.blst}";
          packages = [ jdk (pkgs.sbt.override { jre = jdk; }) pkgs.clang ];
        };
        ci-crypto-windows = let jdk = pkgs.openjdk11; in pkgs.mkShell {
          JAVA_HOME = "${jdk}";
          SODIUM_HOME = "${cryptoWindows.sodium}";
          SECP256K1_HOME = "${cryptoWindows.secp256k1}";
          BLST_HOME = "${cryptoWindows.blst}";
          JNI_MD_WIN32 = "${jniMdWin32}";
          packages = [ jdk mingw.stdenv.cc pkgs.binutils ];
        };
```

- [ ] **Step 4: Build the host libraries** (runs each library's own test suite)

```bash
git add flake.nix
nix develop .#ci-crypto --command bash -c 'ls $SODIUM_HOME/lib/libsodium.a $SECP256K1_HOME/lib/libsecp256k1.a $BLST_HOME/lib/libblst.a; grep Version $SODIUM_HOME/lib/pkgconfig/libsodium.pc $SECP256K1_HOME/lib/pkgconfig/libsecp256k1.pc'
```
Expected: three `.a` files; `Version: 1.0.18` and `Version: 0.3.2`. Windows libraries build in CI (Task 9).

- [ ] **Step 5: Commit** – `build(nix): build the node's libsodium, secp256k1 and blst statically`

---

### Task 2: module skeleton, loader, build

**Files:** `scalus-crypto-jni/{build.sbt,project/*,Makefile,native/onload.c,native/jni_util.h,src/main/java/scalus/crypto/jni/CryptoJni.java}`, `.gitignore`

- [ ] **Step 1: sbt build.** Copy `scalus-secp256k1-jni/project/build.properties` and `project/plugins.sbt`
  unchanged. `build.sbt`:

```scala
// Standalone build for scalus-crypto-jni. Run sbt from this directory. CI publishes with sbt ci-release.
ThisBuild / organization := "org.scalus"
ThisBuild / homepage := Some(url("https://github.com/scalus3/scalus"))
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer("nau", "Alexander Nemish", "anemish@gmail.com", url("https://github.com/nau"))
)
ThisBuild / dynverTagPrefix := "crypto-jni-v"

lazy val root = (project in file("."))
  .settings(
    name := "scalus-crypto-jni",
    crossPaths := false,
    autoScalaLibrary := false,
    javacOptions ++= Seq("--release", "11"),
    libraryDependencies += "org.scijava" % "native-lib-loader" % "2.5.0",
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test
  )
```

- [ ] **Step 2: `native/jni_util.h`**

```c
#ifndef SCALUS_CRYPTO_JNI_UTIL_H
#define SCALUS_CRYPTO_JNI_UTIL_H

#include <jni.h>
#include <stddef.h>

/* Throws IllegalArgumentException with the message. */
static inline void throw_iae(JNIEnv *env, const char *msg) {
    jclass cls = (*env)->FindClass(env, "java/lang/IllegalArgumentException");
    if (cls != NULL) (*env)->ThrowNew(env, cls, msg);
}

/* Copies a Java array of exactly len bytes into buf; throws and returns 0 otherwise. */
static inline int read_exact(JNIEnv *env, jbyteArray a, void *buf, jsize len, const char *what) {
    if (a == NULL || (*env)->GetArrayLength(env, a) != len) {
        throw_iae(env, what);
        return 0;
    }
    (*env)->GetByteArrayRegion(env, a, 0, len, (jbyte *) buf);
    return 1;
}

/* Returns a new Java byte array holding len bytes from buf. */
static inline jbyteArray new_array(JNIEnv *env, const void *buf, jsize len) {
    jbyteArray out = (*env)->NewByteArray(env, len);
    if (out != NULL) (*env)->SetByteArrayRegion(env, out, 0, len, (const jbyte *) buf);
    return out;
}

#endif
```

- [ ] **Step 3: `native/onload.c`**

```c
/*
 * scalus_crypto: JNI bindings for the C crypto libraries that cardano-node links
 * (libsodium IOG fork, libsecp256k1, blst). See README.md for the pinned commits.
 */
#include <jni.h>
#include <sodium.h>
#include <secp256k1.h>

secp256k1_context *scalus_secp256k1_ctx = NULL;

/*
 * Runs once when the JVM loads the library. Initialises libsodium, as cardano-node does at startup,
 * and creates the secp256k1 verification context. A failure fails the load, so
 * CryptoJni.isEnabled() is false.
 */
JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *vm, void *reserved) {
    if (sodium_init() < 0) return JNI_ERR;
    scalus_secp256k1_ctx = secp256k1_context_create(SECP256K1_CONTEXT_VERIFY);
    if (scalus_secp256k1_ctx == NULL) return JNI_ERR;
    return JNI_VERSION_1_8;
}
```

- [ ] **Step 4: `CryptoJni.java`** (Apache header as in `scalus-secp256k1-jni/src/main/java/scalus/crypto/Secp256k1Context.java`)

```java
package scalus.crypto.jni;

import java.io.IOException;
import org.scijava.nativelib.NativeLoader;

/**
 * Loads the scalus_crypto native library: libsodium, libsecp256k1 and blst at the commits
 * cardano-node links. The other classes in this package call {@link #isEnabled()} in their static
 * initialisers, so the library is loaded before their first native call.
 */
public final class CryptoJni {
    private static final boolean enabled;

    static {
        boolean ok = true;
        try {
            NativeLoader.loadLibrary("scalus_crypto");
        } catch (IOException | UnsatisfiedLinkError e) {
            System.err.println("Failed to load scalus_crypto native library: " + e.getMessage());
            ok = false;
        }
        enabled = ok;
    }

    private CryptoJni() {}

    /** @return true if the native library loaded on this platform */
    public static boolean isEnabled() {
        return enabled;
    }
}
```

- [ ] **Step 5: `Makefile`.** Start from `scalus-secp256k1-jni/Makefile` (platform detection, `JAVA_HOME`,
  JNI includes, `info`, `clean`). Change: `LIB_NAME := scalus_crypto`; `SOURCES := $(wildcard native/*.c)`;
  depend also on `native/jni_util.h`; and

```make
CRYPTO_CFLAGS := -I$(SODIUM_HOME)/include -I$(SECP256K1_HOME)/include -I$(BLST_HOME)/include
CRYPTO_LIBS := $(SODIUM_HOME)/lib/libsodium.a $(SECP256K1_HOME)/lib/libsecp256k1.a $(BLST_HOME)/lib/libblst.a

$(RESOURCES_DIR)/$(OUTPUT_LIB): $(SOURCES) native/jni_util.h
	@mkdir -p $(RESOURCES_DIR)
	$(CC) $(CFLAGS) $(JNI_INCLUDE) $(CRYPTO_CFLAGS) $(SHARED_FLAG) -o $@ $(SOURCES) $(CRYPTO_LIBS)

WIN_DIR := src/main/resources/natives/windows_64
windows: $(WIN_DIR)/$(LIB_NAME).dll
$(WIN_DIR)/$(LIB_NAME).dll: $(SOURCES) native/jni_util.h
	@mkdir -p $(WIN_DIR) build/win32-include
	cp "$(JNI_MD_WIN32)" build/win32-include/jni_md.h
	x86_64-w64-mingw32-gcc -O2 -Wall -shared -static -static-libgcc -DSODIUM_STATIC \
		-I"$(JAVA_HOME)/include" -Ibuild/win32-include $(CRYPTO_CFLAGS) \
		-o $@ $(SOURCES) $(CRYPTO_LIBS) -ladvapi32
```

`-ladvapi32`: libsodium's `RtlGenRandom` is pulled in with `#pragma comment(lib, "advapi32.lib")`, which
MinGW ignores. Add `windows` to `.PHONY`; `clean` removes `build/` and both native outputs.

- [ ] **Step 6: `.gitignore`** – add `scalus-crypto-jni/src/main/resources/natives/` and `scalus-crypto-jni/build/`.

- [ ] **Step 7: Build.** `cd scalus-crypto-jni && nix develop ..#ci-crypto --command bash -c 'make && sbt compile'`.
  Expected: `libscalus_crypto.dylib` for `osx_arm64`; `otool -L` on it lists only system libraries.

- [ ] **Step 8: Commit** – `feat(crypto-jni): add the scalus-crypto-jni module and loader`

---

### Task 3: libsodium – Ed25519 verify

**Interfaces – Produces:** `scalus.crypto.jni.Sodium.ed25519VerifyDetached(byte[] sig, byte[] msg, byte[] pk): boolean`;
throws `IllegalArgumentException` unless `sig.length == 64 && pk.length == 32`.

- [ ] **Step 1: Failing test** `src/test/java/scalus/crypto/jni/SodiumTest.java`

```java
package scalus.crypto.jni;

import static org.junit.Assert.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class SodiumTest {
    static final String FIXTURE = "../scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv";

    static byte[] hex(String s) {
        byte[] out = new byte[s.length() / 2];
        for (int i = 0; i < out.length; i++) out[i] = (byte) Integer.parseInt(s.substring(2 * i, 2 * i + 2), 16);
        return out;
    }

    @Test
    public void libraryLoads() {
        assertTrue("scalus_crypto did not load", CryptoJni.isEnabled());
    }

    @Test
    public void givesLibsodiumVerdictOnEveryVector() throws Exception {
        List<String> wrong = new ArrayList<>();
        int rows = 0, accepted = 0;
        for (String line : Files.readAllLines(Paths.get(FIXTURE), StandardCharsets.UTF_8)) {
            if (line.isEmpty() || line.startsWith("#")) continue;
            String[] f = line.split("\t");
            boolean expected = f[5].equals("accept");
            rows++;
            if (expected) accepted++;
            if (Sodium.ed25519VerifyDetached(hex(f[4]), hex(f[3]), hex(f[2])) != expected) wrong.add(f[0] + "#" + f[1]);
        }
        assertEquals(930, rows);
        assertEquals(45, accepted);
        assertTrue(wrong.size() + " mismatches: " + wrong, wrong.isEmpty());
    }

    @Test(expected = IllegalArgumentException.class)
    public void rejectsShortSignature() {
        Sodium.ed25519VerifyDetached(new byte[63], new byte[0], new byte[32]);
    }
}
```

Run `sbt test` → compile error `cannot find symbol: Sodium`.

- [ ] **Step 2: `native/sodium.c`**

```c
#include <sodium.h>
#include "jni_util.h"

/* crypto_sign_ed25519_verify_detached, called as cardano-crypto-class Ed25519DSIGN does. */
JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Sodium_ed25519VerifyDetached0(
    JNIEnv *env, jclass cls, jbyteArray sig, jbyteArray msg, jbyteArray pk) {
    unsigned char sig_buf[crypto_sign_ed25519_BYTES];
    unsigned char pk_buf[crypto_sign_ed25519_PUBLICKEYBYTES];
    static const unsigned char empty[1] = { 0 };
    if (!read_exact(env, sig, sig_buf, sizeof sig_buf, "signature must be 64 bytes")) return JNI_FALSE;
    if (!read_exact(env, pk, pk_buf, sizeof pk_buf, "public key must be 32 bytes")) return JNI_FALSE;
    jsize len = (*env)->GetArrayLength(env, msg);
    jbyte *m = NULL;
    if (len > 0) {
        m = (*env)->GetPrimitiveArrayCritical(env, msg, NULL); /* no JNI calls until released */
        if (m == NULL) return JNI_FALSE;
    }
    int rc = crypto_sign_ed25519_verify_detached(
        sig_buf, len > 0 ? (const unsigned char *) m : empty, (unsigned long long) len, pk_buf);
    if (m != NULL) (*env)->ReleasePrimitiveArrayCritical(env, msg, m, JNI_ABORT);
    return rc == 0 ? JNI_TRUE : JNI_FALSE;
}
```

- [ ] **Step 3: `Sodium.java`**

```java
package scalus.crypto.jni;

/** Ed25519 verification with libsodium at the commit cardano-node links (IOG fork dbb48cce). */
public final class Sodium {
    static { CryptoJni.isEnabled(); }

    private Sodium() {}

    /**
     * {@code crypto_sign_ed25519_verify_detached}. No checks beyond the lengths.
     *
     * @throws IllegalArgumentException unless sig is 64 bytes and pk is 32 bytes
     */
    public static boolean ed25519VerifyDetached(byte[] sig, byte[] msg, byte[] pk) {
        return ed25519VerifyDetached0(sig, msg, pk);
    }

    private static native boolean ed25519VerifyDetached0(byte[] sig, byte[] msg, byte[] pk);
}
```

- [ ] **Step 4:** `make && sbt test` → `SodiumTest` 3 pass. **Commit** – `feat(crypto-jni): Ed25519 verify with the node's libsodium`

---

### Task 4: libsecp256k1 – ECDSA and Schnorr

**Interfaces – Produces** (class `scalus.crypto.jni.Secp256k1`):
`isValidPubKey(byte[] pk): boolean` (33 or 65 bytes), `ecdsaVerify(byte[] msg32, byte[] sig64, byte[] pk33): boolean`,
`isValidXOnlyPubKey(byte[] pk32): boolean`, `schnorrVerify(byte[] sig64, byte[] msg, byte[] pk32): boolean`.

- [ ] **Step 1: Failing test** `Secp256k1Test.java` with these vectors (from `CekBuiltinsTest`):

```java
package scalus.crypto.jni;

import static org.junit.Assert.*;
import static scalus.crypto.jni.SodiumTest.hex;

import org.junit.Test;

public class Secp256k1Test {
    static final byte[] ECDSA_PK = hex("03427d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627");
    static final byte[] MSG = hex("2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824");
    static final String S = "5ffac010a1bd9b9a275ad685ea4052f4bc72c0dc27094422ba9379e7bf44b29b";
    static final String R = "040f5b6a2bb4e024d47eab02d4073da655af77c0cf0efdb19c6771378da175c4";

    @Test public void ecdsaValid() { assertTrue(Secp256k1.ecdsaVerify(MSG, hex(R + S), ECDSA_PK)); }
    @Test public void ecdsaZeroRIsFalse() {
        assertFalse(Secp256k1.ecdsaVerify(MSG, hex("00".repeat(32) + S), ECDSA_PK));
    }
    @Test public void ecdsaBadKeyIsInvalid() {
        assertFalse(Secp256k1.isValidPubKey(hex("FFFF7d3132a06e31bf66791dda478b5ebec79bd045247126396fccdf11e42a3627")));
    }
    // Schnorr: the CIP-49 vector from CekBuiltinsTest (pk 427d31…, msg MSG, sig 4fd97a…) and
    // BIP-340 test vector 15 (empty message). Both checked with the BIP-340 reference code.
    // (An earlier draft used the 9518c1… vector, which is an Ed25519 vector, not Schnorr.)
}
```

- [ ] **Step 2: `native/secp256k1.c`.** Copy the bodies of `isValidPubKey`, `ecdsaVerify` and
  `schnorrVerify` from `scalus-secp256k1-jni/native/scalus_secp256k1.c`, with these changes only:
  - JNI names become `Java_scalus_crypto_jni_Secp256k1_<name>0`.
  - The context is `extern secp256k1_context *scalus_secp256k1_ctx;` (set in `onload.c`); delete
    `pthread_once`, `init_context`, `get_context` and the context-init JNI function.
  - In `schnorrVerify`, a zero-length message must not be passed as a NULL pointer from
    `GetByteArrayElements`: use a static 1-byte buffer when the length is 0, as in `sodium.c`.
  - Add `isValidXOnlyPubKey0`: `secp256k1_xonly_pubkey_parse` on a 32-byte input, true iff it returns 1.
  Keep the call order in "Call shapes"; add no checks.

- [ ] **Step 3: `Secp256k1.java`** – public static wrappers over private `…0` natives, with
  `static { CryptoJni.isEnabled(); }`, Javadoc naming the libsecp256k1 function each wraps.

- [ ] **Step 4:** `make && sbt test` → all pass. **Commit** – `feat(crypto-jni): secp256k1 verify at the node's v0.3.2`

---

### Task 5: blst – G1, G2, pairing, MSM

**Interfaces – Produces** (class `scalus.crypto.jni.Blst`; `X` is `p1` or `p2`; points are raw blst structs):
`XUncompress(byte[] c)`, `XCompress(byte[] p)`, `XAddOrDouble(byte[] a, byte[] b)`, `XNeg(byte[] p)`,
`XMult(byte[] p, byte[] scalarBE32)`, `XIsEqual(byte[] a, byte[] b): boolean`, `XHashTo(byte[] msg, byte[] dst)`,
`XMsm(byte[] points, byte[] scalarsBE)` (concatenated, `n` points and `n` 32-byte scalars);
`millerLoop(byte[] p1, byte[] p2)`, `fp12Mul(byte[] a, byte[] b)`, `fp12IsEqual(byte[] a, byte[] b): boolean`,
`finalVerify(byte[] a, byte[] b): boolean`. Uncompress throws `IllegalArgumentException("BLST_ERROR <n>")`
or `("point not in group")`. Scalars are already reduced mod r by the caller.

- [ ] **Step 1: Failing test** `BlstTest.java`

```java
package scalus.crypto.jni;

import static org.junit.Assert.*;
import static scalus.crypto.jni.SodiumTest.hex;

import org.junit.Test;

public class BlstTest {
    // plutus-conformance bls12_381_G1_hashToGroup/hash-dst-len-255: a 255-byte DST with bytes >= 0x80.
    // blst-java turned this DST into a String and got a different point (supranational/blst#232).
    static final String DST255 = "1234567890".repeat(51); // 255 bytes
    static final String EXPECTED_G1 =
        "931bd1f65dd2d34a55c93d82c20dcacd3a91afa5932fdd7fed06119f8574520c9609d337d680060b4bd2c59f0b60bb54";

    static String toHex(byte[] b) {
        StringBuilder sb = new StringBuilder();
        for (byte x : b) sb.append(String.format("%02x", x));
        return sb.toString();
    }

    @Test public void hashToG1WithBinaryDst() {
        assertEquals(EXPECTED_G1, toHex(Blst.p1Compress(Blst.p1HashTo(hex("3f"), hex(DST255)))));
    }

    @Test public void addingAPointToItselfDoubles() {
        byte[] g = Blst.p1HashTo("g".getBytes(), "DST".getBytes());
        byte[] two = new byte[32];
        two[31] = 2;
        assertTrue(Blst.p1IsEqual(Blst.p1AddOrDouble(g, g), Blst.p1Mult(g, two)));
    }

    @Test(expected = IllegalArgumentException.class)
    public void uncompressRejectsBadEncoding() {
        Blst.p1Uncompress(new byte[48]); // compression bit not set
    }

    @Test public void msmOfOnePairEqualsMult() {
        byte[] g = Blst.p1HashTo("g".getBytes(), "DST".getBytes());
        byte[] three = new byte[32];
        three[31] = 3;
        assertTrue(Blst.p1IsEqual(Blst.p1Msm(g, three), Blst.p1Mult(g, three)));
    }

    @Test public void pairingBilinearity() {
        byte[] p = Blst.p1HashTo("p".getBytes(), "DST".getBytes());
        byte[] q = Blst.p2HashTo("q".getBytes(), "DST".getBytes());
        byte[] five = new byte[32];
        five[31] = 5;
        assertTrue(Blst.finalVerify(Blst.millerLoop(Blst.p1Mult(p, five), q), Blst.millerLoop(p, Blst.p2Mult(q, five))));
    }
}
```

Before relying on `DST255`, check it against the corpus: it must equal the hex of
`plutus-conformance/test-cases/uplc/evaluation/builtin/semantics/bls12_381_G1_hashToGroup/hash-dst-len-255/hash-dst-len-255.uplc`
(second argument). If it differs, copy the exact hex from that file.

- [ ] **Step 2: `native/blst.c`** – one macro instance per group, so G1 and G2 cannot drift apart.

```c
#include <stdlib.h>
#include <string.h>
#include <blst.h>
#include "jni_util.h"

static void throw_blst(JNIEnv *env, BLST_ERROR err) {
    char msg[32];
    snprintf(msg, sizeof msg, "BLST_ERROR %d", (int) err);
    throw_iae(env, msg);
}

/* Reads a whole Java array into a malloc'ed buffer; *len receives its length. NULL on failure. */
static unsigned char *read_all(JNIEnv *env, jbyteArray a, jsize *len) {
    *len = a == NULL ? 0 : (*env)->GetArrayLength(env, a);
    unsigned char *buf = malloc(*len > 0 ? (size_t) *len : 1);
    if (buf != NULL && *len > 0) (*env)->GetByteArrayRegion(env, a, 0, *len, (jbyte *) buf);
    return buf;
}

#define BLST_GROUP(X, G, POINT, AFFINE, COMPRESSED)                                              \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Uncompress0(                      \
    JNIEnv *env, jclass c, jbyteArray in) {                                                      \
    byte buf[COMPRESSED]; AFFINE a; POINT p;                                                     \
    if (!read_exact(env, in, buf, COMPRESSED, "wrong compressed point length")) return NULL;     \
    BLST_ERROR err = blst_##X##_uncompress(&a, buf);                                             \
    if (err != BLST_SUCCESS) { throw_blst(env, err); return NULL; }                              \
    blst_##X##_from_affine(&p, &a);                                                              \
    if (!blst_##X##_in_##G(&p)) { throw_iae(env, "point not in group"); return NULL; }          \
    return new_array(env, &p, sizeof p);                                                         \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Compress0(                        \
    JNIEnv *env, jclass c, jbyteArray in) {                                                      \
    POINT p; byte out[COMPRESSED];                                                               \
    if (!read_exact(env, in, &p, sizeof p, "wrong point length")) return NULL;                  \
    blst_##X##_compress(out, &p);                                                                \
    return new_array(env, out, COMPRESSED);                                                      \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##AddOrDouble0(                     \
    JNIEnv *env, jclass c, jbyteArray a, jbyteArray b) {                                         \
    POINT pa, pb, r;                                                                             \
    if (!read_exact(env, a, &pa, sizeof pa, "wrong point length")) return NULL;                 \
    if (!read_exact(env, b, &pb, sizeof pb, "wrong point length")) return NULL;                 \
    blst_##X##_add_or_double(&r, &pa, &pb);                                                      \
    return new_array(env, &r, sizeof r);                                                         \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Neg0(                             \
    JNIEnv *env, jclass c, jbyteArray in) {                                                      \
    POINT p;                                                                                     \
    if (!read_exact(env, in, &p, sizeof p, "wrong point length")) return NULL;                  \
    blst_##X##_cneg(&p, 1);                                                                      \
    return new_array(env, &p, sizeof p);                                                         \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Mult0(                            \
    JNIEnv *env, jclass c, jbyteArray in, jbyteArray scalar_be) {                                \
    POINT p, r; byte be[32]; blst_scalar s;                                                      \
    if (!read_exact(env, in, &p, sizeof p, "wrong point length")) return NULL;                  \
    if (!read_exact(env, scalar_be, be, 32, "scalar must be 32 bytes")) return NULL;            \
    blst_scalar_from_bendian(&s, be);                                                            \
    blst_##X##_mult(&r, &p, s.b, 256);                                                           \
    return new_array(env, &r, sizeof r);                                                         \
}                                                                                                \
JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Blst_##X##IsEqual0(                           \
    JNIEnv *env, jclass c, jbyteArray a, jbyteArray b) {                                         \
    POINT pa, pb;                                                                                \
    if (!read_exact(env, a, &pa, sizeof pa, "wrong point length")) return JNI_FALSE;            \
    if (!read_exact(env, b, &pb, sizeof pb, "wrong point length")) return JNI_FALSE;            \
    return blst_##X##_is_equal(&pa, &pb) ? JNI_TRUE : JNI_FALSE;                                 \
}                                                                                                \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##HashTo0(                          \
    JNIEnv *env, jclass c, jbyteArray msg, jbyteArray dst) {                                     \
    jsize ml, dl; POINT r;                                                                       \
    unsigned char *m = read_all(env, msg, &ml), *d = read_all(env, dst, &dl);                   \
    if (m == NULL || d == NULL) { free(m); free(d); throw_iae(env, "out of memory"); return NULL; } \
    blst_hash_to_##G(&r, m, (size_t) ml, d, (size_t) dl, NULL, 0);                               \
    free(m); free(d);                                                                            \
    return new_array(env, &r, sizeof r);                                                         \
}                                                                                                \
/* cardano-crypto-class blsMSM: skip infinity points and zero scalars; 0 pairs -> zero,         \
   1 pair -> mult with 256 bits, otherwise Pippenger with nbits = 255. */                        \
JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_##X##Msm0(                             \
    JNIEnv *env, jclass c, jbyteArray points, jbyteArray scalars_be) {                           \
    jsize pl, sl; POINT r;                                                                       \
    unsigned char *pb = read_all(env, points, &pl), *sb = read_all(env, scalars_be, &sl);       \
    if (pb == NULL || sb == NULL) { free(pb); free(sb); throw_iae(env, "out of memory"); return NULL; } \
    size_t n = (size_t) pl / sizeof(POINT);                                                      \
    if ((size_t) pl != n * sizeof(POINT) || (size_t) sl != n * 32) {                             \
        free(pb); free(sb); throw_iae(env, "points and scalars do not match"); return NULL; }   \
    POINT *ps = malloc((n ? n : 1) * sizeof(POINT));                                             \
    blst_scalar *ss = malloc((n ? n : 1) * sizeof(blst_scalar));                                 \
    size_t k = 0;                                                                                \
    static const blst_scalar zero_scalar;                                                        \
    for (size_t i = 0; i < n; i++) {                                                             \
        memcpy(&ps[k], pb + i * sizeof(POINT), sizeof(POINT));                                  \
        blst_scalar_from_bendian(&ss[k], sb + i * 32);                                           \
        if (blst_##X##_is_inf(&ps[k])) continue;                                                 \
        if (memcmp(ss[k].b, zero_scalar.b, sizeof ss[k].b) == 0) continue;                       \
        k++;                                                                                     \
    }                                                                                            \
    if (k == 0) {                                                                                \
        byte inf[COMPRESSED] = { 0xc0 }; AFFINE a;                                               \
        blst_##X##_uncompress(&a, inf);                                                          \
        blst_##X##_from_affine(&r, &a);                                                          \
    } else if (k == 1) {                                                                         \
        blst_##X##_mult(&r, &ps[0], ss[0].b, 256);                                               \
    } else {                                                                                     \
        const POINT **pp = malloc(k * sizeof(POINT *));                                          \
        const byte **sp = malloc(k * sizeof(byte *));                                            \
        AFFINE *aff = malloc(k * sizeof(AFFINE));                                                \
        limb_t *scratch = malloc(blst_##X##s_mult_pippenger_scratch_sizeof(k));                  \
        for (size_t i = 0; i < k; i++) { pp[i] = &ps[i]; sp[i] = ss[i].b; }                      \
        blst_##X##s_to_affine(aff, pp, k);                                                       \
        const AFFINE *ap[2] = { aff, NULL };                                                     \
        blst_##X##s_mult_pippenger(&r, ap, k, sp, 255, scratch);                                 \
        free(pp); free(sp); free(aff); free(scratch);                                            \
    }                                                                                            \
    free(ps); free(ss); free(pb); free(sb);                                                      \
    return new_array(env, &r, sizeof r);                                                         \
}

BLST_GROUP(p1, g1, blst_p1, blst_p1_affine, 48)
BLST_GROUP(p2, g2, blst_p2, blst_p2_affine, 96)

JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_millerLoop0(
    JNIEnv *env, jclass c, jbyteArray p1, jbyteArray p2) {
    blst_p1 a; blst_p2 b; blst_p1_affine aa; blst_p2_affine ba; blst_fp12 r;
    if (!read_exact(env, p1, &a, sizeof a, "wrong G1 point length")) return NULL;
    if (!read_exact(env, p2, &b, sizeof b, "wrong G2 point length")) return NULL;
    blst_p1_to_affine(&aa, &a);
    blst_p2_to_affine(&ba, &b);
    blst_miller_loop(&r, &ba, &aa);
    return new_array(env, &r, sizeof r);
}

JNIEXPORT jbyteArray JNICALL Java_scalus_crypto_jni_Blst_fp12Mul0(
    JNIEnv *env, jclass c, jbyteArray x, jbyteArray y) {
    blst_fp12 a, b, r;
    if (!read_exact(env, x, &a, sizeof a, "wrong ML result length")) return NULL;
    if (!read_exact(env, y, &b, sizeof b, "wrong ML result length")) return NULL;
    blst_fp12_mul(&r, &a, &b);
    return new_array(env, &r, sizeof r);
}

JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Blst_fp12IsEqual0(
    JNIEnv *env, jclass c, jbyteArray x, jbyteArray y) {
    blst_fp12 a, b;
    if (!read_exact(env, x, &a, sizeof a, "wrong ML result length")) return JNI_FALSE;
    if (!read_exact(env, y, &b, sizeof b, "wrong ML result length")) return JNI_FALSE;
    return blst_fp12_is_equal(&a, &b) ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jboolean JNICALL Java_scalus_crypto_jni_Blst_finalVerify0(
    JNIEnv *env, jclass c, jbyteArray x, jbyteArray y) {
    blst_fp12 a, b;
    if (!read_exact(env, x, &a, sizeof a, "wrong ML result length")) return JNI_FALSE;
    if (!read_exact(env, y, &b, sizeof b, "wrong ML result length")) return JNI_FALSE;
    return blst_fp12_finalverify(&a, &b) ? JNI_TRUE : JNI_FALSE;
}
```

Notes for the implementer: check every blst prototype used here against `$BLST_HOME/include/blst.h`
(v0.3.15) and fix the call, not the semantics, if one differs. `#include <stdio.h>` for `snprintf`.
Compare the MSM steps with `blsMSM` in cardano-base `060819b5`
`cardano-crypto-class/src/Cardano/Crypto/EllipticCurve/BLS12_381/Internal.hs` line by line.

- [ ] **Step 3: `Blst.java`** – `static { CryptoJni.isEnabled(); }`; one public static method per entry in
  "Interfaces", each calling its private `…0` native. Javadoc states the struct sizes and that inputs must
  come from this class.

- [ ] **Step 4:** `make && sbt test` → `BlstTest` 5 pass. **Commit** – `feat(crypto-jni): blst at the node's v0.3.15 with byte[] DSTs`

---

### Task 6: Scalus – snapshot dependency and Ed25519

- [ ] **Step 1:** `cd scalus-crypto-jni && nix develop ..#ci-crypto --command sbt 'set ThisBuild / version := "0.1.0-SNAPSHOT"' publishLocal`.
  In `build.sbt`, next to each `scalus-secp256k1-jni` dependency (lines 538 and 1222), add
  `libraryDependencies += "org.scalus" % "scalus-crypto-jni" % "0.1.0-SNAPSHOT"`. Leave the old ones until Task 7.
- [ ] **Step 2:** Move the fixture loader out of `Ed25519LibsodiumParityTest` into
  `scalus-core/shared/src/test/scala/scalus/crypto/ed25519/LibsodiumVerdicts.scala`
  (`object LibsodiumVerdicts { case class Vector(...); lazy val all: Seq[Vector]; def mismatches(verify: Vector => Boolean): Seq[String] }`,
  same parsing code as today), and make the parity test use it.
- [ ] **Step 3:** `JdkEd25519VerifierTest` (JVM): asserts `CryptoJni.isEnabled()`, and that
  `LibsodiumVerdicts.mismatches(v => JdkEd25519Verifier.verify(v.pk.bytes, v.msg.bytes, v.sig.bytes))` is empty.
- [ ] **Step 4:** `JvmEd25519Verifier.scala`:

```scala
package scalus.crypto.ed25519

import scalus.crypto.jni.{CryptoJni, Sodium}

/** Ed25519 on the JVM with the Cardano node's verdicts: the node's libsodium when the native
  * library has loaded, else [[JdkEd25519Verifier]] (same verdicts on the test vectors, ~8x slower).
  */
private[scalus] object JvmEd25519Verifier {
    private val native: Boolean = CryptoJni.isEnabled()

    def verify(pk: Array[Byte], msg: Array[Byte], sig: Array[Byte]): Boolean =
        if native then Sodium.ed25519VerifyDetached(sig, msg, pk)
        else JdkEd25519Verifier.verify(pk, msg, sig)
}
```

  `JVMPlatformSpecific.verifyEd25519Signature` and `JvmEd25519Signer.verify` call it.
- [ ] **Step 5:** `sbt "scalusJVM/testOnly scalus.crypto.ed25519.*"` → all pass; the uncommitted
  `VerifySpeedScratch` now times the native path (target ≤ 75 µs). **Commit** (not `build.sbt`) –
  `feat(jvm): verify Ed25519 with the node's libsodium`

---

### Task 7: Scalus – secp256k1 and BLS on scalus-crypto-jni

- [ ] **Step 1: secp256k1.** In `JVMPlatformSpecific`, replace `scalus.crypto.{NativeSecp256k1, Secp256k1Context}`
  with `scalus.crypto.jni.{CryptoJni, Secp256k1}`. Same `require`s; `Secp256k1Context.isEnabled` →
  `CryptoJni.isEnabled()`; the Schnorr key check uses `Secp256k1.isValidXOnlyPubKey(pk.bytes)` (the
  `0x02`-prefix trick goes).
- [ ] **Step 2: BLS types.** `G1Element(private[builtin] val value: Array[Byte])` (a `blst_p1`); equality
  `Blst.p1IsEqual`; `hashCode` over `Blst.p1Compress(value)`; `toCompressedByteString` via `p1Compress`;
  `apply(ByteString)` via `Blst.p1Uncompress`. Same for `G2Element` (`p2`). `MLResult(value: Array[Byte])`
  with equality `Blst.fp12IsEqual` and `hashCode` over the bytes. Remove the `apply(P1|P2|PT)` overloads (Decision 2).
- [ ] **Step 3: BLS builtins.** Scalar helper in `JVMPlatformSpecific`:

```scala
    /** n mod r as 32 big-endian bytes, as cardano-crypto-class scalarFromInteger does. */
    private def scalarBE(n: BigInt): Array[Byte] = {
        val reduced = n.bigInteger.mod(PlatformSpecific.bls12_381_scalar_period.bigInteger).toByteArray
        val out = new Array[Byte](32)
        val len = math.min(reduced.length, 32)
        System.arraycopy(reduced, reduced.length - len, out, 32 - len, len)
        out
    }
```

  Then: `add` → `p1AddOrDouble`; `scalarMul` → `p1Mult(p.value, scalarBE(s))`; `neg` → `p1Neg`;
  `uncompress` keeps its length and compression-bit `require`s, then `G1Element(Blst.p1Uncompress(bs.bytes))`;
  `hashToGroup` keeps `require(dst.size <= 255)`, then `Blst.p1HashTo(bs.bytes, dst.bytes)` (no String);
  `multiScalarMul` → `Blst.p1Msm(points.flatMap(_.value).toArray, scalars.flatMap(scalarBE).toArray)`;
  `millerLoop` → `Blst.millerLoop`; `mulMlResult` → `fp12Mul`; `finalVerify` → `Blst.finalVerify`. G2 the
  same with `p2`. Delete `PippengerMSM` if nothing else uses it (`grep -rn PippengerMSM scalus-*`).
- [ ] **Step 4: Dependencies.** Remove `foundation.icon:blst-java` and `scalus-secp256k1-jni` from `build.sbt`
  (both places). `grep -rn "supranational\|scalus.crypto.NativeSecp256k1\|Secp256k1Context" --include=*.scala .`
  must find nothing outside `scalus-secp256k1-jni/`.
- [ ] **Step 5: Un-skip.** `PlutusConformanceJvmTest`: delete the `blstLargeDstCases` override.
- [ ] **Step 6: Gates.**

```bash
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusJVM/testOnly scalus.uplc.eval.PlutusConformanceJvmTest scalus.uplc.CekBuiltinsTest scalus.crypto.*" | tee /tmp/crypto-jvm.log
grep -E "Tests: succeeded|FAILED" /tmp/crypto-jvm.log
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true jvm/test
```
Expected: conformance passes **including** `hash-dst-len-255` (G1, G2) and `large-dst`; nothing ignored
for BLS. Then `sbt mima`: add the reported filters for the removed `apply(P1|P2|PT)` and changed
`value` types to `build.sbt` with a comment naming this change.
- [ ] **Step 7: Commit** (scalafmtAll; not `build.sbt` while it points at the snapshot) –
  `feat(jvm): run secp256k1 and BLS12-381 on the node's libraries`

---

### Task 8: Scala Native – call BLS the way the node does

**Files:** `scalus-core/native/src/main/scala/scalus/uplc/builtin/NativePlatformSpecific.scala`,
`scalus-core/native/src/test/scala/scalus/uplc/eval/PlutusConformanceNativeTest.scala`, a shared test.

- [ ] **Step 1: Failing shared test** (in `CekBuiltinsTest` or a new BLS test): for G1 and G2,
  `add(p, p) == scalarMul(2, p)` with `p` = `hashToGroup("p", "DST")`. Run on Native.
- [ ] **Step 2:** Replace `blst_p1_add`/`blst_p2_add` with `blst_p1_add_or_double`/`blst_p2_add_or_double`
  (extern declarations and the 2 call sites). Keep everything else.
- [ ] **Step 3:** Delete the `blstLargeDstCases` override in `PlutusConformanceNativeTest` and its comment.
  Run `sbt "scalusNative/testOnly scalus.uplc.eval.PlutusConformanceNativeTest scalus.uplc.CekBuiltinsTest"`.
  If a DST case fails, put the skip back with a comment that states the measured failure, and report it.
- [ ] **Step 4: Commit** – `fix(native): add BLS points with add_or_double, as cardano-crypto-class does`

---

### Task 9: release workflow, 5 platforms

**Files:** `.github/workflows/crypto-jni-release.yml`; any workflow that path-ignores `scalus-secp256k1-jni`
(`grep -rn "scalus-secp256k1-jni" .github/workflows`) gets the same entry for `scalus-crypto-jni`.

- [ ] **Step 1:** Copy `secp256k1-jni-release.yml` and change:
  - trigger tags `crypto-jni-v*`; `workflow_dispatch` input `publish` (boolean, default false); the
    publish job runs `if: startsWith(github.ref, 'refs/tags/crypto-jni-v') || inputs.publish`;
  - shells `.#ci-crypto`; directory `scalus-crypto-jni`; build step `make && sbt test`;
  - a `build-windows` job on `ubuntu-latest`: `nix develop .#ci-crypto-windows --command bash -c "cd scalus-crypto-jni && make windows"`,
    then `objdump -p …/windows_64/scalus_crypto.dll | grep 'DLL Name'`, failing on anything outside
    `KERNEL32|msvcrt|api-ms-win-crt|ucrtbase|ADVAPI32|bcrypt`; upload `native-windows_64`;
  - a `test-windows` job on `windows-latest`: `actions/setup-java@v4` (temurin 11), `sbt/setup-sbt@v1`,
    download `native-windows_64` into `scalus-crypto-jni/src/main/resources/natives/windows_64/`,
    `cd scalus-crypto-jni && sbt test` (shell: bash);
  - `publish` needs all build/test jobs; the organise loop includes `windows_64`; `find` also lists `*.dll`.
- [ ] **Step 2: README** `scalus-crypto-jni/README.md`: purpose (the node's crypto on the JVM), the pin
  table from this plan, API per class, platforms, build (`nix develop ..#ci-crypto`, `make`, `make windows`),
  release (`crypto-jni-v*`). CONTRIBUTING.md: a "Publishing scalus-crypto-jni" section; mark the
  `scalus-secp256k1-jni` section as frozen at 0.6.0.
- [ ] **Step 3: Commit** – `ci(crypto-jni): build and test 5 platforms, publish on crypto-jni-v tags`
- [ ] **Step 4: STOP.** Ask the owner to push the branch and dispatch the workflow with `publish = false`.
  All 5 platforms must pass `sbt test`.

---

### Task 10: release 0.1.0 – OWNER ONLY

The owner tags `crypto-jni-v0.1.0` on the reviewed commit and pushes the tag. Wait for
`https://repo1.maven.org/maven2/org/scalus/scalus-crypto-jni/0.1.0/`.

---

### Task 11: switch to 0.1.0 and finish

- [ ] **Step 1:** `build.sbt` → `"0.1.0"`; `rm -rf ~/.ivy2/local/org.scalus/scalus-crypto-jni/0.1.0-SNAPSHOT`;
  check the jar lists 5 natives (`unzip -l … | grep natives/`).
- [ ] **Step 2:** CONTRIBUTING.md "Current state": JVM column = the node's pins via `scalus-crypto-jni 0.1.0`;
  Native column unchanged (system libraries) unless changed meanwhile.
- [ ] **Step 3:** CHANGELOG `## Unreleased`:

```markdown
### Fixed
- On the JVM, Ed25519, secp256k1 and BLS12-381 run the C libraries that cardano-node 11.1.3 links
  (libsodium IOG fork, libsecp256k1 v0.3.2, blst v0.3.15) through the new `scalus-crypto-jni`, which also
  supports Windows x64. Ed25519 accepts exactly the signatures the node accepts, and
  `bls12_381_G{1,2}_hashToGroup` gives the node's point for any DST.
- JavaScript Ed25519 verification gives libsodium's verdicts.
- Scala Native adds BLS12-381 points with `add_or_double`, as the node does.

### Changed
- **Breaking:** `G1Element.apply(P1)`, `G2Element.apply(P2)` and `MLResult.apply(PT)` are removed;
  blst-java is no longer a dependency. Build points with `G1Element(ByteString)` or the builtins.
- **Where the native library does not load, Ed25519 verification on the JVM needs JDK 15 or later.**
```

- [ ] **Step 4: Full gate** – `sbt -Dsbt.supershell=false -Dsbt.log.noformat=true scalafmtCheckAll quick scalusJS/test scalusNative/test mima`.
- [ ] **Step 5:** Delete the speed scratch files; commit `build.sbt`, `CHANGELOG.md`, `CONTRIBUTING.md`
  (`fix(deps): use scalus-crypto-jni 0.1.0`); `sbt shutdown`. Report; do not push or merge without a request.

---

## Separate parity gaps found while planning (not in this plan)

- **`bls12_381_G{1,2}_scalarMul` bound.** In semantics variants D and E (PV11), Plutus uses `scalarMulE`,
  which fails for a scalar outside ±2^4095 (plutus 1.63.0.0 `Default/Builtins.hs:1893`, `G1.hs:113`). Scalus
  enforces the bound only for `multiScalarMul` (`Builtin.scala:78-83`).
- **Plutus version.** The node runs plutus-core 1.70.0.0; `flake.nix` pins 1.63.0.0 for `uplc` and the corpus.
- **Scala Native** links nixpkgs' libsodium, libsecp256k1 and blst, not the node's pins.
