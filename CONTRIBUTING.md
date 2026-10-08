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

## Agent artifacts: plugin, skills, llms.txt

AI coding agents get Scalus knowledge from three places. Each has its own update path.

| Artifact | Source | Reaches users when |
|---|---|---|
| `scalus` Claude Code plugin (skills, session-start routing) | `scalus-skills/` | `version` in `scalus-skills/.claude-plugin/plugin.json` changes and users update the plugin |
| `llms.txt`, `llms-full.txt`, `llms-examples.txt` | `scalus-site/content/`, `scalus-examples/` | the Deploy Site workflow runs (`yarn build` generates them) |
| `llms-api.txt` | public API of the **latest release tag** | the Deploy Site workflow runs |

The `.claude/skills/` copies in the `scalus3/hello.g8` and `scalus3/validator.g8` templates are
retired: the templates enable the plugin instead (`.claude/settings.json`).

### When you change a skill

1. Edit `scalus-skills/skills/<skill>/SKILL.md`. Put contract knowledge here, not in `Claude.md`.
2. Bump `version` in `scalus-skills/.claude-plugin/plugin.json` in the same commit: patch for
   wording, minor for a new rule, a new skill or a hook change. The `Skills` workflow
   (`scripts/check-skills-version.sh`) fails when `scalus-skills/` changes without a bump.
3. Test the working copy in a Scalus project: `claude --plugin-dir ./scalus-skills`. For the
   session-start hook, also run
   `CLAUDE_PROJECT_DIR=$PWD bash scalus-skills/hooks/session-start` and check that it prints JSON.
4. After the push, update your own install: `/plugin marketplace update scalus`, then update
   `scalus@scalus` from `/plugin`.

### When you release

1. Update version-bound text in the skills (for example "needs Scalus 1.2.0 or newer") if the
   release adds or removes API the skills name. Bump the plugin version if you change anything.
2. After the tag is pushed and published, run the **Deploy Site** workflow with
   `api_mode: regenerate`. It regenerates the Scaladoc, `llms-api.txt` from the new tag, and the
   other `llms*.txt` files from `master`.
3. Check that the first line of https://scalus.org/llms-api.txt names the new tag.
4. Bump the Scalus version in the `hello.g8` and `validator.g8` templates.

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

## Keeping crypto libraries in sync with cardano-node

Scalus must give the same verdict as the Cardano node for every signature check and every crypto
builtin. A different library version, or a different way of calling it, can change a verdict. For
example, libsodium and bcprov disagree on 174 of 930 Ed25519 test vectors. A script can then pass in
Scalus and fail on the node.

So Scalus runs the same C crypto libraries, at the same commits, as the **latest production
cardano-node release**. Check this on every cardano-node release and every Plutus release.

### Current state

Last checked against cardano-node 11.1.3 (plutus-core 1.70.0.0) on 2026-10-07.

| Library | cardano-node 11.1.3 | Scalus JVM | Scalus Native |
|---|---|---|---|
| libsodium (Ed25519) | `input-output-hk/libsodium` `dbb48cce5429cb6585c9034f002568964f1ce567` (1.0.18 code) | same, via `scalus-crypto-jni` | system libsodium (nixpkgs) |
| libsecp256k1 | v0.3.2 `acf5c55ae6a94e5ca847e07def40427547876101` | same, via `scalus-crypto-jni` | system libsecp256k1 (nixpkgs) |
| blst | v0.3.15 `6d960cd05d6fe2b5bc9ba161edf0c1a131b87c4c` | same, via `scalus-crypto-jni` | system blst (nixpkgs) |
| Plutus conformance corpus | plutus-core 1.70.0.0 | 1.63.0.0 (`plutus` input in `flake.nix`) | same |

The JVM needs `scalus-crypto-jni`'s native library: Linux with glibc 2.34+, macOS 11+ (arm64) or
10.15+ (x64), or Windows x64. JavaScript uses `@noble/curves` and cannot link these libraries.
There, the vector, conformance and property tests are the only guard. Scala Native still links the
system (nixpkgs) libraries, not the node's pins. This is a known gap, recorded in the table above;
the parity, conformance and property tests guard it.

### On each cardano-node release

1. Read the node's pins. Resolve them from the **root** `iohkNix` input. The lock file also holds
   other `iohkNix` nodes (for example from `cardano-dev`) with other pins.

   ```bash
   NODE=11.1.3
   curl -sL https://raw.githubusercontent.com/IntersectMBO/cardano-node/$NODE/flake.lock |
     jq -r '.nodes as $n | $n[$n.root.inputs.iohkNix].inputs | to_entries[]
       | select(.key | test("sodium|secp256k1|blst"))
       | "\(.key)\t\($n[.value].locked.owner)/\($n[.value].locked.repo)\t\($n[.value].locked.rev)\t\($n[.value].original.ref // "")"'
   ```

2. Read the Haskell package versions from the release notes. You need `plutus-core`,
   `cardano-crypto-class` and `cardano-crypto-praos`. Each row links a CHANGELOG at the exact commit.

   ```bash
   gh release view $NODE -R IntersectMBO/cardano-node --json body --jq .body |
     grep -E '^\| (plutus-core|plutus-ledger-api|cardano-crypto-class|cardano-crypto-praos) '
   ```

3. Read every commit since the last check in the places that decide verdicts. Look for pin changes,
   new checks before or after a library call, changed argument handling, and new builtins.

   | Repository | Paths |
   |---|---|
   | `IntersectMBO/cardano-node` | `flake.lock` |
   | `input-output-hk/iohk-nix` | `overlays/crypto/`, `flake.lock` (build flags, pins) |
   | `IntersectMBO/cardano-base` | `cardano-crypto-class/`, `cardano-crypto-praos/` |
   | `IntersectMBO/plutus` | `plutus-core/plutus-core/src/PlutusCore/Crypto/`, `plutus-core/plutus-core/src/PlutusCore/Default/Builtins.hs`, `plutus-conformance/` |

   ```bash
   git -C ../plutus log --oneline <last-checked>..<new> -- plutus-core/plutus-core/src/PlutusCore/Crypto
   ```

4. If a pin or a call changed:
   1. Bump the library in the JNI build and in the Native build.
   2. Regenerate the Ed25519 fixture with the new libsodium
      (`scalus-core/shared/src/test/resources/ed25519/generate_libsodium_verdicts.py`).
   3. Bump the `plutus` input in `flake.nix` so the conformance corpus matches.
   4. Run the vector and conformance tests on JVM, JS and Native.
   5. Release the JNI artifact, then bump it in `build.sbt`.
   6. Add a CHANGELOG entry.

5. Update the "Current state" table, even if nothing changed. The date shows the check happened.

## Publishing scalus-crypto-jni to Maven Central

`scalus-crypto-jni/` is a standalone project with its own `build.sbt`. It binds libsodium,
libsecp256k1 and blst at cardano-node's pins (see its README). It is versioned on its own with
`crypto-jni-v*` tags, and the main CI ignores it.

The `crypto-jni` workflow builds and tests 5 platforms (Linux and macOS, x64 and arm64, and
Windows x64 cross-compiled with MinGW) on every push that changes the module. It also enforces the
platform policy: Linux glibc 2.34+ (tested in `ubuntu:22.04` and `rockylinux:9`), macOS 11.0+ on
arm64 and 10.15+ on x64, and Windows x64. Alpine/musl is not supported. If a nixpkgs bump raises
the glibc floor, `scalus-crypto-jni/ci/check-linux.sh` fails and names the symbol.

1. Run the workflow by hand with `publish = false`, and check that all 5 platforms pass.
2. Tag and push:
   ```bash
   git tag crypto-jni-v0.1.0
   git push origin crypto-jni-v0.1.0
   ```
3. When the version is on Maven Central, update `build.sbt`:
   ```scala
   libraryDependencies += "org.scalus" % "scalus-crypto-jni" % "0.1.0"
   ```

Build locally from `scalus-crypto-jni/` with `nix develop ..#ci-crypto --command bash -c 'make && sbt test'`.

## Publishing scalus-secp256k1-jni to Maven Central

**Frozen at 0.6.0.** Scalus uses `scalus-crypto-jni` instead. This section is kept for reference.


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
scalusCardanoLedgerJS / prepareNpmPackage
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

## Supported Scala versions

The versions live in `build.sbt` as `supportedScalaVersions` (cross-built and tested everywhere)
and `pluginScalaVersions` (the versions a `scalus-plugin_<version>` is published for):

| Version | Role |
|---------|------|
| 3.3.8   | Default. The only build that publishes the `_3` library artifacts. |
| 3.8.4   | Cross-built and tested. |
| 3.9.0   | Cross-built and tested. The next LTS line. |
| 3.3.7   | Compiler plugin only. Downstream projects are still pinned to it. |

Each of those gets its own `scalus-plugin_<version>` artifact, because a Scala 3 compiler plugin has
to match the compiler exactly (`CrossVersion.full`). 3.3.7 is the plugin-only case: `scalus-core`
cross-builds there too, but only so the 3.3.7 plugin has something to be tested against, and its CI
job is the cheaper `scalus.compiler.*` canary rather than a full run.

The `_3` library artifacts share a single coordinate across all of Scala 3, so exactly one build may
publish them, and it has to be the **oldest** supported compiler: a 3.3.x compiler refuses TASTy
emitted by 3.8.x/3.9.x, while newer compilers read 3.3.x TASTy without trouble. `publishOnlyLts`
enforces that. A Scala 3.9.0 project therefore consumes the 3.3.8-built artifact, and does not need
one built by its own compiler.

Build and test one version locally with `sbt ci-jvm-3_3_8`, `sbt ci-jvm-3_8_4`,
`sbt ci-jvm-3_9_0` or `sbt ci-jvm-3_3_7`. The CI-JVM workflow runs one job per version, named
after it.

### Adding or bumping a version

1. Edit the `scala3*Version` val in `build.sbt`. The build refuses to load if the `ci-jvm-*` aliases
   no longer spell out `pluginScalaVersions`, so this step cannot be half-done.
2. Rename the matching `ci-jvm-<version>` alias and update its `++` argument.
3. Rename the matrix entry in `.github/workflows/ci-jvm.yml`. The job name is the status-check name,
   so the repository's required checks have to be updated to match.
4. Version-specific compiler-plugin sources live in `scalus-plugin/src/main/scala-3.3` and
   `scala-3.8`; the selector in `build.sbt` picks `scala-3.8` for 3.5 and later. A new version needs
   a new directory only if the `StandardPlugin` registration hook changes again.
5. Budgets and compiled script sizes differ between compiler generations.
   `ScalaCompilerVersion.baseline` picks `pre38` on the 3.3.x LTS and `since38` on 3.8 and later. If
   a new version desugars differently again, the affected pins in `scalus-examples` need a third arm
   rather than a silent re-pin.
