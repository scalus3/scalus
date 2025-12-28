# scalus-secp256k1-jni

JNI bindings for [libsecp256k1](https://github.com/bitcoin-core/secp256k1) v0.6.0.

Provides ECDSA and Schnorr signature verification for Cardano CIP-49 compatibility.

## Features

- **ECDSA Verification**: Verify ECDSA signatures on secp256k1 curve
- **Schnorr Verification**: BIP-340 compliant Schnorr signature verification with arbitrary message lengths
- **Public Key Validation**: Validate compressed (33 bytes) and uncompressed (65 bytes) public keys

## Supported Platforms

- Linux x86_64 (`linux_64`)
- Linux ARM64 (`linux_arm64`)
- macOS x86_64 (`osx_64`)
- macOS ARM64/Apple Silicon (`osx_arm64`)

## Usage

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "org.scalus" % "scalus-secp256k1-jni" % "0.6.0"
```

Check if the native library is available:

```java
import scalus.crypto.Secp256k1Context;

if (Secp256k1Context.isEnabled()) {
    // Native library loaded successfully
}
```

Verify signatures:

```java
import scalus.crypto.NativeSecp256k1;

// ECDSA verification
boolean valid = NativeSecp256k1.ecdsaVerify(msg32, sig64, pubKey33);

// Schnorr verification (BIP-340, supports arbitrary message lengths)
boolean valid = NativeSecp256k1.schnorrVerify(sig64, msg, pubKey32);
```

## Building from Source

### Prerequisites

- JDK 11 or later
- libsecp256k1 development headers
- GCC or Clang

### Install secp256k1

**Linux (Debian/Ubuntu):**
```bash
sudo apt-get install libsecp256k1-dev
```

**macOS:**
```bash
brew install secp256k1
```

### Build

```bash
make
sbt compile
```

## Publishing

This is a standalone sbt project with its own `build.sbt`. It uses `sbt-ci-release` with `dynverTagPrefix` to version independently from the main Scalus project.

### Automated Release (CI)

1. Update the version by creating and pushing a tag:
   ```bash
   git tag secp256k1-jni-v0.7.0
   git push origin secp256k1-jni-v0.7.0
   ```

2. The GitHub Actions workflow `secp256k1-jni-release.yml` will:
   - Build native libraries for all platforms (linux_64, linux_arm64, osx_64, osx_arm64)
   - Package them into a JAR
   - Publish to Maven Central via `sbt ci-release`

### Manual Release (Local)

```bash
cd scalus-secp256k1-jni
sbt ci-release
```

Required environment variables:
- `PGP_PASSPHRASE` - GPG key passphrase
- `PGP_SECRET` - Base64-encoded GPG private key
- `SONATYPE_USERNAME` - Sonatype Central Portal token name
- `SONATYPE_PASSWORD` - Sonatype Central Portal token password

## License

Apache License 2.0
