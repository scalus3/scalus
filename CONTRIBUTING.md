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

## Releasing Scalus

A push of a `v<X.Y.Z>` tag starts the **Release** workflow (`.github/workflows/release.yml`). It
runs `sbt ci-release` and publishes every artifact to Maven Central. Scalus uses `early-semver`: a
patch release has only fixes, a minor release can add API and remove old deprecated API.

Do the steps in order. Each step tells you which file or command to use.

### Before the tag

1. **Check the Cardano versions.** Use the latest *production* cardano-node release, not a
   pre-release. Read its pins:

   ```bash
   NODE=11.1.3
   gh release view $NODE -R IntersectMBO/cardano-node --json body --jq .body |
     grep -E '^\| (plutus-core|plutus-ledger-api|cardano-ledger-conway|cardano-crypto-class) '
   ```

   - **Crypto libraries** (libsodium, libsecp256k1, blst): follow
     [On each cardano-node release](#on-each-cardano-node-release). If a pin changed, release
     `scalus-crypto-jni` first and bump it in `build.sbt`.
   - **Plutus and the conformance corpus**: the `plutus` input in `flake.nix` gives the
     `plutus-conformance` test cases. Set it to the node's `plutus-core` version, run
     `nix flake update plutus`, then run the conformance tests. Read the Plutus CHANGELOG for new
     builtins and cost model changes (`scalus-core/shared/src/main/resources/builtinCostModel*.json`).
   - **cardano-ledger**: read the `cardano-ledger-conway` CHANGELOG for rule changes that the
     Emulator and `scalus.cardano.ledger.rules` must follow.
   - Update the [Current state](#current-state) table, even if nothing changed.

2. **Update the dependencies.** Take only versions that were published at least 14 days ago. A
   security fix can skip the wait. Scala Steward applies the same rule (`.scala-steward.conf`), so
   its open PRs are the list for `build.sbt` and `project/plugins.sbt`:

   ```bash
   gh pr list --author scala-steward
   ```

   Apply the updates on master by hand, then close the PRs. Also check what Steward does not cover:
   the actions in `.github/workflows/`, `package.json` in the root and in `scalus-site/`, and the
   `nixpkgs` input in `flake.lock`. Step 1 already covers the `plutus` input. Do a breaking major,
   such as a new `@noble/curves` major, in its own commit.

3. **Remove old deprecated API** (minor releases only). Release `1.N.0` deletes every
   `@deprecated` member whose version is `1.(N-2).x` or older. For example, 1.4.0 deletes the
   members deprecated in 1.0.x, 1.1.x and 1.2.x. List them:

   ```bash
   git grep -nE '@deprecated\(.*"1\.[0-2]\.[0-9]+"\)' -- '*.scala'
   ```

   Each deletion breaks MiMa. Add a `mimaBinaryIssueFilters` entry with a comment for each one.
   List each deletion in the CHANGELOG `Removed` section, in bold, as a migration hazard.

4. **Write the CHANGELOG.** See "Changelog" in `AGENTS.md`. Rename `## Unreleased` to
   `## X.Y.Z (YYYY-MM-DD)`. Compare it with `git log v<last>..HEAD --oneline`, so that no
   user-visible change is missing.

5. **Check and improve the skills** (`scalus-skills/`):
   1. Read the new CHANGELOG section. For each added, deprecated or removed API, check if a skill
      must name it, for example a new intrinsic that is cheaper than the old pattern.
   2. Search the skills for each deprecated or removed name, for example
      `git grep -n lovelaceAmount -- scalus-skills/`. Replace it, unless the skill names it as an API
      to avoid.
   3. Do [When you release](#when-you-release), step 1.
   4. Test the working copy: create a project from `validator.g8`, run
      `claude --plugin-dir <scalus>/scalus-skills` in it, and ask for a small validator with
      tests. Make sure that the agent loads the skills and uses the current API.

6. **Prepare the npm package.** Set `version` in
   `scalus-cardano-ledger/js/src/main/npm/package.json` to `X.Y.Z`. You publish it after the tag,
   see [Publishing Scalus JS library to NPM](#publishing-scalus-js-library-to-npm).

7. **Run the full gate** in the nix devshell:

   ```bash
   sbt --batch ci   # ci-jvm;ci-js;ci-native: format, mima, all platforms
   sbt it           # integration tests, needs Docker
   ```

   `ci-jvm` runs only Scala 3.3.8. Make sure that the CI-JVM workflow is green for all Scala
   versions on the commit that you tag. In `it`, Blockfrost tests fail with a preprod-only key.
   That is not a release blocker.

8. **Get to 0 compiler warnings.** Warnings do not fail CI, so they collect between releases.
   Count them in the logs of the CI runs on the commit that you tag, for all jobs:

   ```bash
   gh run view <run-id> --log | perl -pe 's/\e\[[0-9;]*m//g' | grep -cE '\[warn\] -- .*Warning:'
   ```

   Fix each warning, or suppress an intentional one with `@nowarn("msg=...")` and a comment.

9. **Review the MiMa filters.** Each filter added since the last release must have a comment
   that explains the break. Each break must be in the CHANGELOG.

### Tag and publish

1. Tag the commit and push the tag:

   ```bash
   git tag vX.Y.Z
   git push origin vX.Y.Z
   ```

2. Monitor the **Release** workflow. It can take up to 90 minutes. If you run it again and get
   "component already exists", the first run already published the artifacts.
3. Make sure that the artifacts are on Maven Central. This can take 30 minutes after the workflow
   ends:

   ```bash
   curl -s https://repo1.maven.org/maven2/org/scalus/scalus_3/maven-metadata.xml | grep X.Y.Z
   ```

4. Publish the npm package. See [Publishing Scalus JS library to NPM](#publishing-scalus-js-library-to-npm).
5. Create the GitHub release with the CHANGELOG section of this version as the notes:

   ```bash
   gh release create vX.Y.Z --title vX.Y.Z --notes-file <changelog-section.md>
   ```

### After the release

1. **Move the MiMa baseline.** Set `scalusStableVersion` in `build.sbt` to `X.Y.Z`. Delete the
   per-symbol `mimaBinaryIssueFilters` that the new baseline makes obsolete. Keep the wildcard
   policy filters. Run `sbtn mima`. This value also sets the Scaladoc version and
   `scalus.utils.BuildInfo.scalusVersion`.
2. **Update the version in the docs.** Update the install snippets in `README.md` ("Use Scalus").
   Search `scalus-site/content/` for the old version.
3. **Update the g8 templates.** Bump the Scalus version in `hello.g8` and `validator.g8`:
   `build.sbt`, `project/plugins.sbt`, `project.scala` and the `//> using` lines in the `.scala`
   files.
4. **Update scalus-starter** ([scalus3/scalus-starter](https://github.com/scalus3/scalus-starter)).
   Bump the Scalus version in `build.sbt`, `project/plugins.sbt` and `CLAUDE.md`. Then run
   `sbt "clean; test; integration/test"`. The integration tests need Docker. If they fail with
   `container name "/scalus-yaci-devkit" is already in use`, remove the old container with
   `docker rm -f scalus-yaci-devkit`. Fix the deprecation warnings, so that the starter shows the
   current API.
5. **Deploy the site.** Follow [When you release](#when-you-release), steps 2 and 3.
6. **Open the next version.** Add an empty `## Unreleased` section to the top of `CHANGELOG.md`.

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

1. Edit `scalus-skills/skills/<skill>/SKILL.md`. Put contract knowledge here, not in `AGENTS.md`.
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

Last checked against cardano-node 11.1.3 (plutus-core 1.70.0.0) on 2026-10-20.

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

The npm `scalus` version is the same as the library version. The package is in
`scalus-cardano-ledger/js/src/main/npm/`.

1. Set `version` in `package.json` to the library version.
2. Build the bundle and run the npm tests. This runs `prepareNpmPackage`, which regenerates
   `scalus.js` and `scalus.d.ts`. Do not edit `scalus.d.ts` by hand:

   ```bash
   sbtn scalusCardanoLedgerJS/runNpmTests
   ```

3. Check the package contents. It must have exactly 4 files: `README.md`, `package.json`,
   `scalus.d.ts` and `scalus.js`:

   ```bash
   npm pack --dry-run
   ```

4. Publish. The account uses 2FA for writes, so use `npm login` and a one-time code, or an
   Automation token:

   ```bash
   npm publish --access public
   ```

5. Check the registry, not the npmjs.com page, which shows old data for some time:

   ```bash
   npm view scalus dist-tags
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
