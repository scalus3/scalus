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

## License

Apache License 2.0
