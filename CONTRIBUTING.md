# Contributing to Scalus

## Pre-requisites

- Java 11+, sbt 1.x
- Cardano `uplc` CLI tool and Nix

## Env setup with Nix

Please ensure that Nix is installed (see https://nixos.org/download/#download-nix).

Verify that your user is marked as trusted-users in /etc/nix/nix.conf.

Run :

```bash
nix develop
```

## Build

Before committing changes, make sure that the code is formatted, compiles and the tests pass:

```bash
sbtn precommit
```

## Scalus Plugin Development

During compiler plugin development you want to automatically recompile the dependencies of the
plugin.

For faster development make sure that in `build.sbt`:

1. `scalacOptions` contains `-Xplugin` with the path to the plugin jar with the dummy argument.

```scala
Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
```

1. scalusPlugin project version is not manually set

This line should be commented out in scalusPlugin project settings:

```scala
version := "0.6.2-SNAPSHOT"
,
```

### Debugging Scalus Plugin during compilation

* Run sbt with the following command:

```bash
sbtn -J-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005 compile
```

This makes the compiler wait for a debugger to attach on port 5005.

* Set Breakpoints in IntelliJ
* In IntelliJ, create a Remote Debug configuration (host: localhost, port: 5005) and start it.
* Once attached, resume execution to hit your breakpoints.

## Scalus Website

    cd scalus-site

### Install

    yarn install

### Development

    yarn dev

### Generate static html

    yarn build

## Serve static htmlgit

    npx serve -s out

### Deploy to GitHub Pages

Run GitHub Actions "Deploy site" workflow.

## Run benchmarks

Measurement of throughput:

```bash
sbtn 'bench/jmh:run -i 1 -wi 1 -f 1 -t 1 .*'
```

Where `.*` is a regexp for benchmark names.

Profiling with [async-profiler](https://github.com/async-profiler/async-profiler) that should be
downloaded from
[nightly builds](https://github.com/async-profiler/async-profiler/releases/tag/nightly) and unpacked
to some directory,
like `/opt/async-profiler` for Linux in the command bellow:

```bash
sbtn 'bench/jmh:run -prof "async:event=cycles;=dir=target/async-reports;interval=1000000;output=flamegraph;libPath=/opt/async-profiler/lib/libasyncProfiler.so" -jvmArgsAppend "-XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints" -f 1 -wi 1 -i 1 -t 1 .*'
```

On MacOS use this command in sbt shell:

```bash
bench/jmh:run -prof "async:event=itimer;dir=target/async-reports;interval=1000000;output=flamegraph;libPath=/nix/store/w1pihmrx6ivkk4njx85m659gh55cjbck-async-profiler-4.0/lib/libasyncProfiler.dylib" -jvmArgsAppend "-XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints"   -f 1 -wi 1 -i 1 -t 1 .*
```

Resulting interactive flame graphs will be stored in the `bench/target/async-reports` subdirectory
of the project.

For benchmarking of allocations use `event=alloc` instead of `event=cycles` option in the command
above.

## Publishing scalus-secp256k1-jni to Maven Central

The `scalus-secp256k1-jni` library is a standalone project in the `scalus-secp256k1-jni/` directory with its own `build.sbt`. It provides JNI bindings for libsecp256k1 and is versioned independently from Scalus using `secp256k1-jni-v*` tags.

### Release Process

1. Create and push a version tag:
   ```bash
   git tag secp256k1-jni-v0.7.0
   git push origin secp256k1-jni-v0.7.0
   ```

2. The `secp256k1-jni-release.yml` GitHub Actions workflow will automatically:
   - Build native libraries for linux_64, linux_arm64, osx_64, osx_arm64
   - Package them into a single JAR with `native-lib-loader`
   - Publish to Maven Central via `sbt ci-release`

3. After publishing, update the dependency version in `build.sbt`:
   ```scala
   libraryDependencies += "org.scalus" % "scalus-secp256k1-jni" % "0.7.0"
   ```

### Building Native Libraries Locally

```bash
cd scalus-secp256k1-jni
make
```

This requires libsecp256k1 development headers installed on your system.

## Publishing Scalus JS library to NPM

```sbt
scalusCardanoLedgerJS / prepareNpmPackag
```

This will create a `scalus-opt-bundle.js` package in the `scalus-cardano-ledger/js/src/main/npm`
directory.

Login to NPM:

```bash
npm login
```

Update the version in `scalus-cardano-ledger/js/src/main/npm/package.json` and publish it to NPM:

```bash
npm publish --access public
```

## Updating Plutus Version

The project uses the Plutus `uplc` tool for conformance testing. The Nix flake has two devShells:

- **`default`** - Uses the full `plutus` flake input (slower to evaluate due to haskell-nix)
- **`ci`** - Uses pre-built `uplc` via `fetchClosure` for fast CI startup (~5s vs ~90s)

### When to Update

Update when bumping the Plutus version in `flake.nix` (e.g., changing `plutus.url`).

### Update Process

1. Update the `plutus.url` in `flake.nix` to the new version:
   ```nix
   plutus.url = "github:IntersectMBO/plutus/1.XX.0.0";
   ```

2. Enter the default dev shell to build the new uplc:
   ```bash
   nix develop
   ```

3. Get the store path for the new uplc binary:
   ```bash
   nix path-info $(which uplc)
   ```
   This will output something like:
   ```
   /nix/store/001ff7xjx4kmbkkckpyzdxa46xfqjbgd-plutus-executables-exe-uplc-1.56.0.0
   ```

4. Update `uplcCached` in `flake.nix` with the new store path:
   ```nix
   uplcCached = builtins.fetchClosure {
     fromStore = "https://cache.iog.io";
     fromPath = /nix/store/NEW-HASH-HERE-plutus-executables-exe-uplc-X.XX.0.0;
     inputAddressed = true;
   };
   ```

5. Update `plutusConformanceSrc` with the new version tag and hash:
   ```nix
   plutusConformanceSrc = builtins.fetchTarball {
     url = "https://github.com/IntersectMBO/plutus/archive/refs/tags/1.XX.0.0.tar.gz";
     sha256 = "0000000000000000000000000000000000000000000000000000";  # Use placeholder, nix will tell you the correct hash
   };
   ```

6. Test the CI shell:
   ```bash
   nix develop .#ci
   ```

### Requirements

The `fetch-closure` experimental feature must be enabled. Add to `~/.config/nix/nix.conf`:
```
experimental-features = nix-command flakes fetch-closure
accept-flake-config = true
```

## Notes on issues

### BLS12-381 signature library

If you stumbled upon such failure during `sbt precommit`:

```
# A fatal error has been detected by the Java Runtime Environment:
...
# Problematic frame:
# C  [libblst.so+0x2daa1]  sqrx_mont_384+0x41
```

you can resolve it if you will locally build Java binding of `blst` native library from:

[https://github.com/supranational/blst/tree/master/bindings/java](https://github.com/supranational/blst/tree/master/bindings/java)

and place resulted JAR into Coursier's cache at

```
~/.cache/coursier/v1/https/repo1.maven.org/maven2/foundation/icon/blst-java/0.3.2/blst-java-0.3.2.jar 
```

Note please that for this you will need have installed [SWIG](https://swig.org/) toolchain.

## Scala 3 Code Style

We use [Scalafmt](https://scalameta.org/scalafmt/) for code formatting. Please make sure that your
code is formatted before committing. You can run `sbt scalafmtAll` to format all code in the
project.

The `.scalafmt.conf` file in the root of the repository contains the formatting settings.

We don't enforce but recommend to stick to the following Scala 3 coding conventions:

- Use `{}` for top level definitions (classes, objects, traits, enums, etc.).
- Use `{}` for function bodies that span multiple lines.
- Use indentation-based syntax for `if`, `match`, `try`, `for` constructs unless they span multiple
  lines so it's more readable with `{}`.
- Use `then` keyword in `if` expressions.
- Use `do` keyword in `while` loops.

### Example

```scala
// Top level definition with {}
object Example {
  // Function body with {}
  def exampleFunction(x: Int): Int = {
    if x > 0 then x * 2
    else
      val y = -x
      y * 2
  }

  def describe(x: Any): String = x match
    case 1 => "one"
    case "hello" => "greeting"
    case _ => "something else"
}
```