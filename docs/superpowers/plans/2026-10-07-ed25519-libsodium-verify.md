# Ed25519 Verification with libsodium Verdicts – Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** On JVM and JS, `verifyEd25519Signature` and `Ed25519Signer.verify` accept a signature only if
libsodium 1.0.18 (the Cardano node's verifier) accepts it.

**Architecture:** One shared object holds libsodium's pre-checks as byte logic. Each platform then
checks the cofactorless equation: the JDK `Ed25519` provider on JVM, `@noble/curves` point arithmetic
on JS. Native already calls libsodium and does not change. A shared test runs 930 vectors with
libsodium's verdicts on all three platforms.

**Tech Stack:** Scala 3, JDK `java.security` EdDSA (JDK 15+), `@noble/curves` 1.9.7, `@noble/hashes`
1.8.0, ScalaTest, Python 3 + libsodium (fixture generation only).

**Branch / worktree:** `fix/ed25519-libsodium-verify` in `.claude/worktrees/ed25519-libsodium`, from
`master` at `31531c14d`. The `plutus-conformance` symlink is already in place.

**Spec:** no spec file. The normative text is the bug report "Scalus accepts signatures the Cardano
node rejects" (2026-10-06) and libsodium 1.0.18 source, summarised in "Rules" below.

## Open decision (resolve before Task 3)

**JDK runtime floor.** `Signature.getInstance("Ed25519")` exists only on JDK 15+. The Scala 3.3 build
compiles with `--release 11` (`build.sbt:154`), so today's artifact runs on JDK 11.

- **(a) Recommended:** require JDK 15+ for Ed25519 verification. On older JDKs, the first verify
  throws `NoSuchAlgorithmException`. It never returns `false` silently.
- (b) On JDK < 15, fall back to bcprov + pre-checks. This leaves 63 CCTV disagreements (reporter's
  measurement), so the bug stays open on those JDKs.

This plan implements (a).

## Rules (normative)

libsodium 1.0.18 `crypto_sign/ed25519/ref10/open.c:31-54` accepts `(pk, msg, sig)` only if all hold.
`R = sig[0..32)`, `S = sig[32..64)`, `A = pk`, all little-endian.

1. `S < L`, where `L = 2^252 + 27742317777372353535851937790883648493` (`sc25519_is_canonical`).
2. `R` is not small-order: R is not one of 7 encodings, compared with bit 7 of byte 31 masked
   (`ed25519_ref10.c:1019-1070`):
   ```
   0000000000000000000000000000000000000000000000000000000000000000
   0100000000000000000000000000000000000000000000000000000000000000
   26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05
   c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a
   ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f
   edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f
   eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f
   ```
3. `A` is canonical: its y (A with bit 255 cleared) is `< p = 2^255 - 19` (`ed25519_ref10.c:1002-1016`).
4. `A` is not small-order (same list as rule 2).
5. `A` decodes to a curve point.
6. `h = SHA-512(R ‖ A ‖ msg) mod L`, hashed over the **received** bytes of R and A.
7. `encode([S]B − [h]A) == R`, byte for byte. This is the cofactorless equation.

**Derived rule 2b:** `R` is canonical (y < p). This does not change any verdict. `encode(...)` in rule
7 is always canonical, so a non-canonical R can never match. We add it so that a JDK which decodes R
leniently cannot accept a non-canonical R.

**Why the pre-checks are mandatory:** the JDK provider alone accepts speccheck case 2 (small-order R).
noble with `zip215: false` is still cofactored. Do not "simplify" by removing rules 1–4.

## Global Constraints

- New objects are `private[scalus]`. No public API changes, so MiMa is unaffected.
- Do not change `sign`, `signExtended`, `derivePublicKey`, or `Ed25519MathPlatform`. bcprov stays for
  them and for Blake2b, Keccak, RIPEMD-160, SHA3.
- Never catch `Exception` or `GeneralSecurityException` around the JDK calls. Catch only
  `InvalidKeyException`, `InvalidKeySpecException`, and `SignatureException`. A missing algorithm must
  throw.
- In this worktree, use one-shot `sbt` with `-Dsbt.supershell=false -Dsbt.log.noformat=true`, never
  `sbtn`. The session already runs inside the nix devshell; do not wrap in `nix develop`.
- ScalaTest failures print as `[info]`, not `[error]`. Read the summary line (`Tests: succeeded N,
  failed M`).
- Run `sbt scalafmtAll` before every commit. Never `git add -A`; add files by path.
- Commit messages: conventional style, no co-author or "generated with" lines.
- **The branch is red between Task 1 and Task 4.** The parity test fails on JVM and JS until their
  tasks land. This is expected on the feature branch.

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `scalus-core/shared/src/test/resources/ed25519/generate_libsodium_verdicts.py` | create | Regenerates the fixture from pinned vectors + libsodium |
| `scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv` | create (generated) | 930 vectors with libsodium's verdict |
| `scalus-core/shared/src/test/scala/scalus/crypto/ed25519/Ed25519LibsodiumParityTest.scala` | create | Platform verdicts == fixture verdicts |
| `scalus-core/shared/src/main/scala/scalus/crypto/ed25519/Ed25519LibsodiumRules.scala` | create | Rules 1–4 + 2b as byte logic |
| `scalus-core/shared/src/test/scala/scalus/crypto/ed25519/Ed25519LibsodiumRulesTest.scala` | create | Boundary tests for each rule |
| `scalus-core/jvm/src/main/scala/scalus/crypto/ed25519/JdkEd25519Verifier.scala` | create | Pre-checks + JDK cofactorless verify |
| `scalus-core/jvm/src/main/scala/scalus/uplc/builtin/JVMPlatformSpecific.scala:60-71` | modify | Builtin calls `JdkEd25519Verifier` |
| `scalus-core/jvm/src/main/scala/scalus/crypto/ed25519/JvmEd25519Signer.scala:31-42` | modify | `verify` calls `JdkEd25519Verifier` |
| `scalus-core/js/src/main/scala/scalus/crypto/ed25519/JsEd25519Signer.scala` | modify | Add facade methods, `JsEd25519Verifier`, rewire `verify` |
| `scalus-core/js/src/main/scala/scalus/uplc/builtin/JSPlatformSpecific.scala:52-60,115-118` | modify | Builtin calls `JsEd25519Verifier`; drop unused facade |
| `CHANGELOG.md` | modify | Unreleased entry |

`VerifiedSignaturesInWitnessesValidator` and the JIT (`BuiltinSnippets.scala:575`) call
`platform.verifyEd25519Signature` / `Builtins.verifyEd25519Signature`. They need no change.

---

### Task 1: libsodium verdict fixture and parity test

**Files:**
- Create: `scalus-core/shared/src/test/resources/ed25519/generate_libsodium_verdicts.py`
- Create: `scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv`
- Create: `scalus-core/shared/src/test/scala/scalus/crypto/ed25519/Ed25519LibsodiumParityTest.scala`

**Interfaces:**
- Consumes: `scalus.uplc.builtin.platform.verifyEd25519Signature`, `platform.readFile`, the
  platform `given Ed25519Signer` (top-level in package `scalus.crypto.ed25519`).
- Produces: the fixture file at the path above, format `source\tid\tpk\tmsg\tsig\tverdict`, `#` comment
  lines.

- [ ] **Step 1: Write the generator**

```python
#!/usr/bin/env python3
"""Regenerate libsodium-verdicts.tsv: Ed25519 vectors with libsodium's verdict.

Usage: generate_libsodium_verdicts.py <path to libsodium.dylib or .so> > libsodium-verdicts.tsv

The Cardano node verifies with libsodium 1.0.18 code (IOG fork dbb48cce). Later releases up to
1.0.22 give the same verdicts on these vectors.
"""
import ctypes
import json
import sys
import urllib.request

CCTV = ("https://raw.githubusercontent.com/C2SP/CCTV/"
        "50a8ecf2a220f4c8bdc4f085789b8e85c26829e7/ed25519/ed25519vectors.json")
SPECCHECK = ("https://raw.githubusercontent.com/novifinancial/ed25519-speccheck/"
             "65519336fda78a3d016e947df6d82848aca0c9da/cases.json")

RFC8032_PK = "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
REPRO_MSG = b"scalus ed25519 repro".hex()
# From the bug report: RFC 8032 test 1, the same with its last byte flipped, then two signatures by
# the RFC 8032 test 1 key with R = identity and with R carrying an order-8 component.
ISSUE = [
    ("rfc8032-test1", RFC8032_PK, "",
     "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"),
    ("rfc8032-test1-flipped", RFC8032_PK, "",
     "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100a"),
    ("r-identity", RFC8032_PK, REPRO_MSG,
     "0100000000000000000000000000000000000000000000000000000000000000943d85c895b02c2a57afbba668e5641527063f11dff33f5a6d2de65e6b45f00e"),
    ("r-order8-component", RFC8032_PK, REPRO_MSG,
     "c37aece30145ac8d15385f30be8d9e303d770583f5e5008c47125818b45a51c4033e7c23808064f3eb008b4907e907ae6ed15ffba46f98cdf96408f67988a002"),
]


def main():
    lib = ctypes.CDLL(sys.argv[1])
    assert lib.sodium_init() >= 0
    lib.sodium_version_string.restype = ctypes.c_char_p

    def verify(pk, msg, sig):
        return lib.crypto_sign_ed25519_verify_detached(
            sig, msg, ctypes.c_ulonglong(len(msg)), pk) == 0

    rows = [("cctv", str(v["number"]), v["key"], v["msg"].encode().hex(), v["sig"])
            for v in json.load(urllib.request.urlopen(CCTV))]
    rows += [("speccheck", str(i), v["pub_key"], v["message"], v["signature"])
             for i, v in enumerate(json.load(urllib.request.urlopen(SPECCHECK)))]
    rows += [("issue", name, pk, msg, sig) for name, pk, msg, sig in ISSUE]

    print(f"# Generated by generate_libsodium_verdicts.py with libsodium "
          f"{lib.sodium_version_string().decode()} crypto_sign_ed25519_verify_detached")
    print(f"# CCTV {CCTV}")
    print(f"# speccheck {SPECCHECK}")
    print("# source\tid\tpk\tmsg\tsig\tverdict")
    for row in rows:
        ok = verify(bytes.fromhex(row[2]), bytes.fromhex(row[3]), bytes.fromhex(row[4]))
        print("\t".join(row + ("accept" if ok else "reject",)))


if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Generate the fixture**

```bash
SODIUM=$(nix build --no-link --print-out-paths nixpkgs#libsodium)/lib/libsodium.dylib
python3 scalus-core/shared/src/test/resources/ed25519/generate_libsodium_verdicts.py "$SODIUM" \
  > scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv
grep -vc '^#' scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv   # expect 930
grep -c 'accept$' scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv # expect 45
grep '^issue' scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv | cut -f2,6
```

Expected: 930 rows, 45 accept (CCTV 43, speccheck 1 = case 3, issue 1). Issue rows: only
`rfc8032-test1` is `accept`. These numbers were checked with libsodium 1.0.22 on 2026-10-07. If they
differ, stop and report.

- [ ] **Step 3: Write the parity test**

```scala
package scalus.crypto.ed25519

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString}

/** Scalus must accept exactly the Ed25519 signatures that the Cardano node accepts. The node
  * verifies with libsodium 1.0.18; the fixture holds libsodium's verdict for every vector.
  */
class Ed25519LibsodiumParityTest extends AnyFunSuite {
    private case class Vector(
        source: String,
        id: String,
        pk: ByteString,
        msg: ByteString,
        sig: ByteString,
        accept: Boolean
    ) {
        def name: String = s"$source#$id"
    }

    private val vectors: Seq[Vector] = {
        val path = "scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv"
        new String(platform.readFile(path), "UTF-8").linesIterator
            .filterNot(line => line.isEmpty || line.startsWith("#"))
            .map { line =>
                val Array(source, id, pk, msg, sig, verdict) = line.split('\t'): @unchecked
                Vector(
                  source,
                  id,
                  ByteString.fromHex(pk),
                  ByteString.fromHex(msg),
                  ByteString.fromHex(sig),
                  verdict == "accept"
                )
            }
            .toSeq
    }

    private def mismatches(verify: Vector => Boolean): Seq[String] =
        vectors.collect {
            case v if verify(v) != v.accept =>
                s"${v.name}: libsodium ${if v.accept then "accepts" else "rejects"}"
        }

    test("fixture is complete") {
        assert(vectors.size == 930)
        assert(vectors.count(_.accept) == 45)
    }

    test("verifyEd25519Signature gives libsodium's verdict on every vector") {
        val wrong = mismatches(v => platform.verifyEd25519Signature(v.pk, v.msg, v.sig))
        assert(wrong.isEmpty, s"${wrong.size} mismatches:\n${wrong.mkString("\n")}")
    }

    test("Ed25519Signer.verify gives libsodium's verdict on every vector") {
        val signer = summon[Ed25519Signer]
        val wrong = mismatches(v =>
            signer.verify(
              VerificationKey.unsafeFromByteString(v.pk),
              v.msg,
              Signature.unsafeFromByteString(v.sig)
            )
        )
        assert(wrong.isEmpty, s"${wrong.size} mismatches:\n${wrong.mkString("\n")}")
    }
}
```

If `summon[Ed25519Signer]` does not resolve in shared test code, add
`import scalus.crypto.ed25519.given` and check each platform defines the `given` at top level
(`JsEd25519Signer.scala:129`, `JvmEd25519Signer.scala:89`, `NativeEd25519Signer.scala:132`).

- [ ] **Step 4: Run on all three platforms and record the failures**

```bash
T=scalus.crypto.ed25519.Ed25519LibsodiumParityTest
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true \
  "scalusJVM/testOnly $T" "scalusJS/testOnly $T" "scalusNative/testOnly $T" 2>&1 | tee /tmp/parity-before.log
grep -E "mismatches|Tests: succeeded" /tmp/parity-before.log
```

Expected:
- JVM: `fixture is complete` passes. Both verify tests fail with **169 + 3 + 2 = 174** mismatches (CCTV, speccheck, the two forged issue vectors).
- JS: both verify tests fail with **783 + 8 + 2 = 793** mismatches.
- Native: all three pass. This proves that the fixture and the test harness are correct.

If Native fails, the fixture or the loader is wrong. Fix that before you continue.

- [ ] **Step 5: Commit**

```bash
sbt scalafmtAll
git add scalus-core/shared/src/test/resources/ed25519/generate_libsodium_verdicts.py \
  scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv \
  scalus-core/shared/src/test/scala/scalus/crypto/ed25519/Ed25519LibsodiumParityTest.scala
git commit -m "test(crypto): check Ed25519 verdicts against libsodium on 930 vectors

The vectors are C2SP CCTV, ed25519-speccheck and the bug report's repro. JVM and
JS fail: they accept signatures that libsodium, and so the Cardano node, rejects."
```

---

### Task 2: shared libsodium pre-checks

**Files:**
- Create: `scalus-core/shared/src/main/scala/scalus/crypto/ed25519/Ed25519LibsodiumRules.scala`
- Test: `scalus-core/shared/src/test/scala/scalus/crypto/ed25519/Ed25519LibsodiumRulesTest.scala`

**Interfaces:**
- Produces: `private[scalus] object Ed25519LibsodiumRules` with
  - `def passesPreChecks(pk: Array[Byte], sig: Array[Byte]): Boolean` – rules 1, 2, 2b, 3, 4.
    Requires `pk.length == 32` and `sig.length == 64`.
  - `def isSmallOrder(point: Array[Byte]): Boolean` – 32-byte encoding.
  - `def isCanonicalPoint(point: Array[Byte]): Boolean` – 32-byte encoding, y < p.
  - `def isCanonicalScalar(scalar: Array[Byte]): Boolean` – 32-byte little-endian, < L.

- [ ] **Step 1: Write the failing test**

```scala
package scalus.crypto.ed25519

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class Ed25519LibsodiumRulesTest extends AnyFunSuite {
    import Ed25519LibsodiumRules.*

    private def hex(s: String): Array[Byte] = ByteString.fromHex(s).bytes

    // L = 2^252 + 27742317777372353535851937790883648493, little-endian
    private val L = hex("edd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010")
    private val LMinus1 = hex("ecd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010")
    private val basePoint = hex("5866666666666666666666666666666666666666666666666666666666666666")
    private val rfc8032Pk = hex("d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a")

    test("scalar is canonical below L only") {
        assert(isCanonicalScalar(LMinus1))
        assert(!isCanonicalScalar(L))
        assert(!isCanonicalScalar(Array.fill(32)(0xff.toByte)))
        assert(isCanonicalScalar(new Array[Byte](32)))
    }

    test("small-order list matches with and without the sign bit") {
        val order8 = hex("c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a")
        val order8Signed = hex("c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa")
        assert(isSmallOrder(order8))
        assert(isSmallOrder(order8Signed))
        assert(isSmallOrder(hex("0100000000000000000000000000000000000000000000000000000000000000")))
        assert(isSmallOrder(hex("eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")))
        assert(!isSmallOrder(basePoint))
        assert(!isSmallOrder(rfc8032Pk))
    }

    test("point is canonical when y < p") {
        val pMinus1 = hex("ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
        val p = hex("edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")
        val pSigned = hex("edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
        assert(isCanonicalPoint(pMinus1))
        assert(!isCanonicalPoint(p))
        assert(!isCanonicalPoint(pSigned))
        assert(isCanonicalPoint(basePoint))
    }

    test("pre-checks accept RFC 8032 test 1 and reject R = identity") {
        val good = hex(
          "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
        )
        val identityR = hex(
          "0100000000000000000000000000000000000000000000000000000000000000943d85c895b02c2a57afbba668e5641527063f11dff33f5a6d2de65e6b45f00e"
        )
        assert(passesPreChecks(rfc8032Pk, good))
        assert(!passesPreChecks(rfc8032Pk, identityR))
        assert(!passesPreChecks(rfc8032Pk, good.take(32) ++ L))
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusJVM/testOnly scalus.crypto.ed25519.Ed25519LibsodiumRulesTest"`
Expected: compile error `Not found: Ed25519LibsodiumRules`.

- [ ] **Step 3: Implement**

```scala
package scalus.crypto.ed25519

import scalus.uplc.builtin.ByteString

/** The checks libsodium 1.0.18 makes before it evaluates the Ed25519 equation
  * (`crypto_sign/ed25519/ref10/open.c:31-39`).
  *
  * The Cardano node verifies Ed25519 with libsodium, so Scalus rejects what libsodium rejects. Run
  * these checks before the cofactorless equation; the equation alone accepts a small-order R.
  */
private[scalus] object Ed25519LibsodiumRules {
    private val L: BigInt = BigInt(2).pow(252) + BigInt("27742317777372353535851937790883648493")
    private val P: BigInt = BigInt(2).pow(255) - 19

    /** libsodium's list (`ed25519_ref10.c:1022-1053`), compared with the sign bit masked. */
    private val smallOrderEncodings: Seq[Array[Byte]] = Seq(
      "0000000000000000000000000000000000000000000000000000000000000000",
      "0100000000000000000000000000000000000000000000000000000000000000",
      "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
      "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
      "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
      "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
      "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
    ).map(ByteString.fromHex(_).bytes)

    private def littleEndian(bytes: Array[Byte]): BigInt = BigInt(1, bytes.reverse)

    private def withoutSignBit(point: Array[Byte]): Array[Byte] =
        point.updated(31, (point(31) & 0x7f).toByte)

    def isCanonicalScalar(scalar: Array[Byte]): Boolean = littleEndian(scalar) < L

    def isCanonicalPoint(point: Array[Byte]): Boolean = littleEndian(withoutSignBit(point)) < P

    def isSmallOrder(point: Array[Byte]): Boolean = {
        val masked = withoutSignBit(point)
        smallOrderEncodings.exists(_.sameElements(masked))
    }

    /** Rules 1-4 of libsodium, plus "R is canonical". The last one changes no verdict: libsodium
      * compares R with a canonical encoding, so a non-canonical R never matches.
      */
    def passesPreChecks(pk: Array[Byte], sig: Array[Byte]): Boolean = {
        val r = sig.take(32)
        isCanonicalScalar(sig.drop(32)) &&
        !isSmallOrder(r) && isCanonicalPoint(r) &&
        isCanonicalPoint(pk) && !isSmallOrder(pk)
    }
}
```

- [ ] **Step 4: Run the test on JVM, JS and Native**

```bash
T=scalus.crypto.ed25519.Ed25519LibsodiumRulesTest
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusJVM/testOnly $T" "scalusJS/testOnly $T" "scalusNative/testOnly $T"
```
Expected: 4 tests pass on each platform.

- [ ] **Step 5: Commit**

```bash
sbt scalafmtAll
git add scalus-core/shared/src/main/scala/scalus/crypto/ed25519/Ed25519LibsodiumRules.scala \
  scalus-core/shared/src/test/scala/scalus/crypto/ed25519/Ed25519LibsodiumRulesTest.scala
git commit -m "feat(crypto): add libsodium's Ed25519 pre-checks as shared byte logic"
```

---

### Task 3: JVM – pre-checks + JDK cofactorless verify

**Files:**
- Create: `scalus-core/jvm/src/main/scala/scalus/crypto/ed25519/JdkEd25519Verifier.scala`
- Modify: `scalus-core/jvm/src/main/scala/scalus/uplc/builtin/JVMPlatformSpecific.scala:4-5,60-71`
- Modify: `scalus-core/jvm/src/main/scala/scalus/crypto/ed25519/JvmEd25519Signer.scala:31-42`

**Interfaces:**
- Consumes: `Ed25519LibsodiumRules.passesPreChecks(pk: Array[Byte], sig: Array[Byte]): Boolean`.
- Produces: `private[scalus] object JdkEd25519Verifier { def verify(pk: Array[Byte], msg: Array[Byte], sig: Array[Byte]): Boolean }`.

- [ ] **Step 1: Implement the verifier**

```scala
package scalus.crypto.ed25519

import java.security.spec.{InvalidKeySpecException, X509EncodedKeySpec}
import java.security.{InvalidKeyException, KeyFactory, SignatureException, Signature as JdkSignature}

/** Ed25519 verification that gives libsodium 1.0.18's verdicts, as the Cardano node does.
  *
  * [[Ed25519LibsodiumRules.passesPreChecks]] runs first. The JDK provider then checks the
  * cofactorless equation. Both are needed: the JDK alone accepts a small-order R, and bcprov uses the
  * cofactored equation. Needs JDK 15 or later.
  */
private[scalus] object JdkEd25519Verifier {

    /** DER prefix of an X.509 SubjectPublicKeyInfo for Ed25519 (RFC 8410). */
    private val x509Prefix: Array[Byte] =
        Array(0x30, 0x2a, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x70, 0x03, 0x21, 0x00).map(_.toByte)

    // Look the algorithm up once: on JDK < 15 this throws NoSuchAlgorithmException at first use,
    // instead of every verify returning false.
    KeyFactory.getInstance("Ed25519")
    JdkSignature.getInstance("Ed25519")

    def verify(pk: Array[Byte], msg: Array[Byte], sig: Array[Byte]): Boolean =
        Ed25519LibsodiumRules.passesPreChecks(pk, sig) && {
            try
                val key = KeyFactory
                    .getInstance("Ed25519")
                    .generatePublic(X509EncodedKeySpec(x509Prefix ++ pk))
                val verifier = JdkSignature.getInstance("Ed25519")
                verifier.initVerify(key)
                verifier.update(msg)
                verifier.verify(sig)
            catch
                case _: InvalidKeyException | _: InvalidKeySpecException | _: SignatureException =>
                    false
        }
}
```

- [ ] **Step 2: Wire the builtin**

In `JVMPlatformSpecific.scala`, replace the body of `verifyEd25519Signature` (lines 60–71) with:

```scala
    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        require(pk.size == 32, s"Invalid public key length ${pk.size}")
        require(sig.size == 64, s"Invalid signature length ${sig.size}")
        JdkEd25519Verifier.verify(pk.bytes, msg.bytes, sig.bytes)
```

Change the import on line 7 to `import scalus.crypto.ed25519.{JdkEd25519Verifier, JvmEd25519Signer, SigningKey}`.
Delete the imports on lines 4–5 (`Ed25519PublicKeyParameters`, `Ed25519Signer`) if nothing else uses them
(`grep -n "Ed25519PublicKeyParameters\|Ed25519Signer" JVMPlatformSpecific.scala`).

- [ ] **Step 3: Wire `JvmEd25519Signer.verify`**

Replace lines 31–42 with:

```scala
    override def verify(
        verificationKey: VerificationKey,
        message: ByteString,
        signature: Signature
    ): Boolean =
        JdkEd25519Verifier.verify(verificationKey.bytes, message.bytes, signature.bytes)
```

Remove the `Ed25519PublicKeyParameters` import only if `derivePublicKey` no longer uses it (it does
– keep it).

- [ ] **Step 4: Run the parity test on JVM**

```bash
T=scalus.crypto.ed25519.Ed25519LibsodiumParityTest
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusJVM/testOnly $T scalus.crypto.ed25519.*" 2>&1 | tee /tmp/parity-jvm.log
grep -E "mismatches|Tests: succeeded" /tmp/parity-jvm.log
```

Expected: 0 mismatches, all tests pass.

**This is the gate.** The JDK's behaviour comes from the reporter's measurement, not ours. If any row
mismatches, do not patch around it. Look up each failing CCTV row's `flags` in the CCTV JSON (pinned
URL in the fixture header), report them with the mismatch list, and stop.

- [ ] **Step 5: Run the other JVM tests that touch Ed25519**

```bash
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusJVM/testOnly scalus.uplc.CekBuiltinsTest scalus.TutorialTest scalus.crypto.*" \
  "scalusCardanoLedgerJVM/testOnly *VerifiedSignatures* *Wallet* *Hd*"
```
Expected: all pass.

- [ ] **Step 6: Commit**

```bash
sbt scalafmtAll
git add scalus-core/jvm/src/main/scala/scalus/crypto/ed25519/JdkEd25519Verifier.scala \
  scalus-core/jvm/src/main/scala/scalus/uplc/builtin/JVMPlatformSpecific.scala \
  scalus-core/jvm/src/main/scala/scalus/crypto/ed25519/JvmEd25519Signer.scala
git commit -m "fix(jvm): verify Ed25519 with libsodium's rules, as the Cardano node does

bcprov uses the cofactored equation and accepts a small-order R, so Scalus
accepted 172 of the 926 CCTV and speccheck vectors that the node rejects. Verification now runs
libsodium's pre-checks and then the JDK's cofactorless check (JDK 15+)."
```

---

### Task 4: JS – pre-checks + noble cofactorless verify

**Files:**
- Modify: `scalus-core/js/src/main/scala/scalus/crypto/ed25519/JsEd25519Signer.scala`
- Modify: `scalus-core/js/src/main/scala/scalus/uplc/builtin/JSPlatformSpecific.scala:52-60,115-118`

**Interfaces:**
- Consumes: `Ed25519LibsodiumRules.passesPreChecks`; `JsEd25519Signer.bytesToBigInt` (little-endian
  `Array[Byte] => js.BigInt`, line 53, today `private`).
- Produces: `private[scalus] object JsEd25519Verifier { def verify(pk: Array[Byte], msg: Array[Byte], sig: Array[Byte]): Boolean }`.

- [ ] **Step 1: Extend the noble facade** in `JsEd25519Signer.scala`

```scala
@js.native
private trait NobleExtendedPointCompanion extends js.Object:
    val BASE: NobleExtendedPoint = js.native
    def fromHex(bytes: Uint8Array, zip215: Boolean): NobleExtendedPoint = js.native

@js.native
private trait NobleExtendedPoint extends js.Object:
    def multiply(scalar: js.BigInt): NobleExtendedPoint = js.native
    def multiplyUnsafe(scalar: js.BigInt): NobleExtendedPoint = js.native
    def subtract(other: NobleExtendedPoint): NobleExtendedPoint = js.native
    def toRawBytes(): Uint8Array = js.native
```

Change `private def bytesToBigInt` (line 53) to `private[ed25519] def bytesToBigInt`.

- [ ] **Step 2: Add the verifier** in the same file, after `object JsEd25519Signer`

```scala
/** Ed25519 verification that gives libsodium 1.0.18's verdicts, as the Cardano node does.
  *
  * noble's `ed25519.verify` uses the cofactored equation even with `zip215: false`, so this checks
  * the cofactorless one: `encode([S]B - [h]A)` must equal R byte for byte.
  */
private[scalus] object JsEd25519Verifier:
    private val L: js.BigInt = NobleEd25519.ed25519.CURVE.n

    def verify(pk: Array[Byte], msg: Array[Byte], sig: Array[Byte]): Boolean =
        Ed25519LibsodiumRules.passesPreChecks(pk, sig) && {
            try
                val r = sig.take(32)
                val s = JsEd25519Signer.bytesToBigInt(sig.drop(32))
                // zip215 = false: reject y >= p and points off the curve
                val a = NobleEd25519.ed25519.ExtendedPoint.fromHex(pk.toUint8Array, false)
                // h over the received bytes of R and A, not re-encoded ones
                val h = JsEd25519Signer.bytesToBigInt(
                  NobleSha512.sha512((r ++ pk ++ msg).toUint8Array).toByteArray
                ) % L
                // multiplyUnsafe: multiply rejects a zero scalar
                val rCheck = NobleEd25519.ed25519.ExtendedPoint.BASE
                    .multiplyUnsafe(s)
                    .subtract(a.multiplyUnsafe(h))
                rCheck.toRawBytes().toByteArray.sameElements(r)
            catch case _: js.JavaScriptException => false
        }
```

Rewire `JsEd25519Signer.verify` (line 112) to:

```scala
    override def verify(
        verificationKey: VerificationKey,
        message: ByteString,
        signature: Signature
    ): Boolean =
        JsEd25519Verifier.verify(verificationKey.bytes, message.bytes, signature.bytes)
```

Remove `def verify` from `NobleEd25519Trait` if nothing else calls it.

- [ ] **Step 3: Wire the builtin** in `JSPlatformSpecific.scala`

```scala
    override def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        require(pk.size == 32, s"Invalid public key length ${pk.size}")
        require(sig.size == 64, s"Invalid signature length ${sig.size}")
        JsEd25519Verifier.verify(pk.bytes, msg.bytes, sig.bytes)
```

Import `scalus.crypto.ed25519.JsEd25519Verifier`. Delete the now unused `Ed25519Curves` object and
`Ed25519` trait (lines 52–60); confirm with `grep -n "Ed25519Curves" -r scalus-core/js/src`.

- [ ] **Step 4: Run the parity and rules tests on JS**

```bash
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusJS/testOnly scalus.crypto.ed25519.*" 2>&1 | tee /tmp/parity-js.log
grep -E "mismatches|Tests: succeeded" /tmp/parity-js.log
```

Expected: 0 mismatches. If rows mismatch, report their CCTV flags and stop, as in Task 3 Step 4.

- [ ] **Step 5: Check the JS API surface did not change**

```bash
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusJS/test" "scalusCardanoLedgerJS/test"
```
Expected: pass. If `checkDtsUpToDate` complains, the change leaked into the exported API. The new
objects are `private[scalus]`, so it must not.

- [ ] **Step 6: Commit**

```bash
sbt scalafmtAll
git add scalus-core/js/src/main/scala/scalus/crypto/ed25519/JsEd25519Signer.scala \
  scalus-core/js/src/main/scala/scalus/uplc/builtin/JSPlatformSpecific.scala
git commit -m "fix(js): verify Ed25519 with libsodium's rules, as the Cardano node does

noble's default ZIP-215 rules accepted 791 of the 926 CCTV and speccheck vectors that the node
rejects. Verification now runs libsodium's pre-checks and the cofactorless
equation on noble's point arithmetic."
```

---

### Task 5: performance, changelog, full gate

**Files:**
- Modify: `CHANGELOG.md`

- [ ] **Step 1: Measure JVM verify speed before and after**

Create a throwaway test (do not commit) at
`scalus-core/jvm/src/test/scala/scalus/crypto/ed25519/VerifySpeedScratch.scala`:

```scala
package scalus.crypto.ed25519

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString}

class VerifySpeedScratch extends AnyFunSuite {
    test("10k verifies") {
        val pk = ByteString.fromHex("d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a")
        val sig = ByteString.fromHex(
          "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
        )
        val msg = ByteString.empty
        for _ <- 1 to 5000 do platform.verifyEd25519Signature(pk, msg, sig) // warm-up
        val t0 = System.nanoTime()
        for _ <- 1 to 10000 do assert(platform.verifyEd25519Signature(pk, msg, sig))
        println(f"VERIFY_US ${(System.nanoTime() - t0) / 10000 / 1000.0}%.1f")
    }
}
```

Run it twice: once **before Task 3** (bcprov baseline; execute this step right after Task 2), and
once after Task 4. Do not stash or check out another branch to get the baseline. Command:
`sbt -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusJVM/testOnly scalus.crypto.ed25519.VerifySpeedScratch" | grep VERIFY_US`

Record both numbers in the final report. If the JDK path is more than 2x slower, try caching the
`KeyFactory` in a `ThreadLocal` (its thread safety is not documented) and measure again. Delete the
scratch file afterwards.

- [ ] **Step 2: Changelog**

Add under a `## Unreleased` heading at the top of `CHANGELOG.md` (create it if absent), section
`### Fixed`:

```markdown
- `verifyEd25519Signature` and `Ed25519Signer.verify` on the JVM and in JavaScript accept exactly the
  signatures libsodium accepts, as the Cardano node does. Before, they accepted some signatures with
  a small-order or mixed-order R, so a script could pass in Scalus and fail on the node.
- **Ed25519 verification on the JVM needs JDK 15 or later.**
```

- [ ] **Step 3: Full gate**

```bash
sbt -Dsbt.supershell=false -Dsbt.log.noformat=true scalafmtCheckAll quick "scalusJS/test" "scalusNative/test" mima
```
(`sbtn ci` cannot pass on master; this is the real gate.)

Expected: all green. `mima` reports no problems.

- [ ] **Step 4: Commit and shut down**

```bash
git add CHANGELOG.md
git commit -m "docs: changelog for libsodium-compatible Ed25519 verification"
sbt shutdown 2>/dev/null || true
```

Report: commits on the branch, parity results per platform, the speed numbers, and the open JDK
floor decision. Do not push or merge without an explicit request.
