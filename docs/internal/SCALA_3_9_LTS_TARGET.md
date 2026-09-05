# Supported Scala versions: add 3.9.0 (next LTS)

Branch `scala-3.9-lts`. Scala 3.9.0 was released 2026-09-03 and is the next Long
Term Support line after 3.3.x.

## The supported set

```scala
val scala3LtsVersion = "3.3.8"
val scala3NextVersion = "3.8.4"
val scala3NextLtsVersion = "3.9.0"
val scala3LtsPrevVersion = "3.3.7"   // compiler plugin only
val supportedScalaVersions = Seq(scala3LtsVersion, scala3NextVersion, scala3NextLtsVersion)
val pluginScalaVersions = scala3LtsPrevVersion +: supportedScalaVersions
```

`supportedScalaVersions` is the single `crossScalaVersions` value for the
cross-built projects. `pluginScalaVersions` adds 3.3.7 and is used by
`scalusPlugin` and `scalus` core only: downstream projects such as hydrozoa are
still pinned to Scala 3.3.7, and a compiler plugin has to match the compiler
exactly, so `scalus-plugin_3.3.7` keeps being published. Core cross-builds there
purely so that plugin variant has something to be tested against.

The one project deliberately outside the set is `scalus-sbt-plugin`
(`Seq("2.12.21", scala3NextVersion)`) - sbt 2.x pins its own Scala 3 version.

## What gets published

| Artifact | Built by | Why |
|---|---|---|
| `scalus-plugin_3.3.7`, `_3.3.8`, `_3.8.4`, `_3.9.0` | `pluginScalaVersions` | A compiler plugin must match the compiler exactly. `CrossVersion.full` gives each its own coordinate. |
| `scalus_3`, `scalus-cardano-ledger_3`, ... | 3.3.8 only | One coordinate for all of Scala 3, so exactly one build may publish it. |

The `_3` publisher has to be the compiler whose TASTy every supported compiler
can read. A 3.3.x compiler refuses TASTy emitted by 3.8.x/3.9.x, while newer
compilers read 3.3.x TASTy fine, so publishing from 3.3.8 is what keeps every
supported consumer able to read the artifact - 3.3.7 included, since 3.3.7 and
3.3.8 are the same TASTy version (28.3). Publishing from the LTS also keeps the
JDK 11 bytecode floor (`jvmReleaseTarget` emits `-release 11` only for the
`3.3.` prefix).

## CI

One job per supported version, named by the version:

| sbt alias | CI-JVM matrix job | Contents |
|---|---|---|
| `ci-jvm-3_3_8` (also `ci-jvm`) | `3.3.8` | full `jvm/Test/compile` + `jvm/test` + `mima`, and the format checks - those are version-independent so they run in this job only |
| `ci-jvm-3_3_7` | `3.3.7` | plugin-only canary: `scalusPlugin/Test/compile` + the `scalus.compiler.*` compile-and-evaluate suite |
| `ci-jvm-3_8_4` | `3.8.4` | `crossVersionCiTasks` |
| `ci-jvm-3_9_0` | `3.9.0` | `crossVersionCiTasks` |

`crossVersionCiTasks` is one shared string. It must NOT use the `jvm` aggregate:
modules that don't list the cross version (`scalusUplcJitCompiler`,
`scalusUtxoCell`, `bench`) fall back to the default LTS and then fail to read the
cross-version TASTy of scalus-core.

Bumping a patch version means editing the alias name, its `++` argument, the
`scala3*Version` val, and the matrix entry. That churn is the accepted cost of
having the version visible in the job name.

## Results

No gate failed. **No source change was needed to support 3.9.0** - the whole
change is build configuration plus comments.

Measured on the final commit, rebased on `origin/master`:

| Gate | Result |
|---|---|
| `++3.9.0 scalusPlugin/compile` | clean, 14 s, **0 warnings**. The `scala-3.8` `PluginCompat` dir works unchanged against `scala3-compiler` 3.9.0. |
| `ci-jvm-3_3_7` | 7 targets, **566** `scalus.compiler.*` tests pass on the 3.3.7 compiler. |
| `ci-jvm-3_3_8` | 31 targets, **5607** tests, MiMa clean, format clean. |
| `ci-jvm-3_8_4` | 18 targets, **4322** tests (3731 + 591). |
| `ci-jvm-3_9_0` | 18 targets, **4322** tests - the same counts as 3.8.4. |
| `++3.9.0 scalusJS/test` | linked to `scalus-core/js/target/scala-3.9.0/scalus-test-fastopt`, **3174** tests pass. |
| `++3.9.0 scalusNative/Test/compile` | clean. |

**14 817 tests across the four JVM jobs, 0 failures, 0 errors, 0 sbt reloads**, with
every compile target under `target/scala-<the job's version>`.

A note on the code review: it was run before 3.3.7 was restored as a
plugin-only version. That restore reinstated the previous `ci-jvm-lts-prev`
canary verbatim under the new name, so it is not new logic; everything else the
review raised was fixed and is covered by the runs above.

### The baselines held

The important empirical result: **`scalusExamplesJVM/test` passed on 3.9.0 with no
re-pinning**. `ScalaCompilerVersion.hasLeanDesugaring` is `minor >= 8`, so 3.9.0
takes the `since38` baselines, and every pinned ExUnits value and script size
matched. The claim that the next LTS inherits the leaner desugaring is now
measured rather than assumed, and is recorded in that class's scaladoc. No third
baseline generation is needed.

This survived a real perturbation: while the branch was open, master landed a UPLC
codegen change (the free Scalus tag, and the UPLC version PlutusV1/V2 programs
declare) that re-pinned about fourteen example baselines. After rebasing, 3.9.0
reports exactly the same test counts as 3.8.4, so the re-pinned `since38` values
are right for both.

### Third-party compatibility

| Question | Answer |
|---|---|
| Scala.js | `scala3-library_sjs1_3:3.9.0` needs **Scala.js 1.22.0**, exactly our `sbt-scalajs` pin. (3.8.4 was built against 1.20.2.) No bump. |
| Scala Native | `nscplugin_3.9.0:0.5.12` and `scala3lib_native0.5_3:3.9.0+0.5.12` are published, and `sbt-scala-native` is 0.5.12. |
| JDK | 3.9.0 needs JDK 17+ to run its compiler. The `ci` nix devshell pins JDK 21. |

### Warnings

22 warnings on 3.9.0: 18 deprecation warnings in
`StaticArgumentTransformationTest.scala` from the existing
`sir.StaticArgumentTransformation` -> `sir.transform.StaticArgumentTransformation`
move, 1 `[E198]` unused symbol, 3 others. 3.9.0 produced no warning category the
3.3.8 build does not also produce.

## Verified: a Scala 3.9.0 project consumes the 3.3.8-built artifact

The published `_3` artifact is built by Scala 3.3.8 with `-release 11`. That is
enough for Scala 3.9.0 consumers; JDK 17 target bytecode is **not** needed.
Measured against the real Maven Central artifact (no `publishLocal`):

`org.scalus:scalus_3:1.1.1` contains class files of major version **55 (Java 11)**
and TASTy version **28.3.0**, tooling string `Scala 3.3.8`:

```
$ xxd -l 32 scalus/InteropApi.tasty
5ca1 ab1f 9c83 808b 5363 616c 6120 332e   \.......Scala 3.
332e 3800 ...                             3.8.
        ^^ ^^ ^^  = nat 28 . 3 . 0
$ for f in *.class; do xxd -p -l 8 $f | cut -c13-16; done | sort -u
0037     # 0x37 = 55 = Java 11
```

A throwaway sbt project on `scalaVersion := "3.9.0"` depending on
`"org.scalus" %% "scalus" % "1.1.1"` compiles and runs it:

```scala
case class Point(x: BigInt, y: BigInt) derives FromData, ToData
val d: Data = p.toData                       // inline extension, read from 3.3.8 TASTy
val back = summon[FromData[Point]](d)
// jvm=21.0.8 data=<0, [3, 4]> roundtrip=Point(3,4) ok=true
```

That single program exercises all three risky layers at once:

1. **Macros** - `derives FromData, ToData` runs library macros *compiled by 3.3.8*
   inside the 3.9.0 compiler.
2. **TASTy inlining** - `Data.toData` is an `inline` extension, so 3.9.0 must read
   and inline a method body from 3.3.8 TASTy. TASTy is backward compatible: a
   newer compiler reads older TASTy, which is the direction we depend on.
3. **Bytecode** - Java class files are backward compatible, so major-version-55
   classes load unchanged on the JDK 21 that ran the test.

The resolved classpath shows `scala3-library_3-3.9.0` (evicting the 3.3.8 one
`scalus_3` declares) alongside `scalus_3-1.1.1`. Standard, safe eviction: the
Scala 3 library is binary compatible across 3.x.

The same program on `scalaVersion := "3.3.7"` also compiles and runs against that
3.3.8-built artifact, which is the case that matters for downstream projects still
pinned to 3.3.7: they need `scalus-plugin_3.3.7`, but not a 3.3.7-built library.

**The one thing a 3.9.0 project still needs from us** is `scalus-plugin_3.9.0`,
which is required for on-chain `@Compile` code and does not exist on Maven Central
yet (404 as of 2026-09-05). That is exactly what this branch adds to the release.

## Release-time note

`ci-release` runs `+publishSigned`, so it now walks three cross versions.
`publishOnlyLts` sets `publish / skip` for the library modules on 3.8.4 and
3.9.0, and sbt's `publishTask` is a `taskDyn` that short-circuits before
packaging when skip is set, so the extra cost should be roughly one extra
compiler-plugin build. The release job's `timeout-minutes: 90` (a tag release
already needs "well over 40") is worth watching on the next tag rather than
assumed safe.

## Deliberately not done: the JDK 17 floor

Raising the published bytecode floor from JDK 11 to 17 was considered and
dropped. It is a consumer-facing break that MiMa cannot catch: `jvmReleaseTarget`
still emits `-release 11` for the 3.3.x line, `javacOptions` still pins
`--release 11`, the `ci` devshell stays on JDK 21, and README/CONTRIBUTING still
say Java 11+. If it is revisited, the pieces are:

1. `jvmReleaseTarget` -> `-release 17` unconditionally.
2. `ThisBuild / javacOptions` -> `--release 17`.
3. `flake.nix` `ci` shell `openjdk21` -> `openjdk17` (so test code cannot
   silently use a post-17 API; `-release` only constrains `Compile`).
4. README + CONTRIBUTING "Java 11+" -> "Java 17+", plus a CHANGELOG note.
5. `ci-secp` stays on JDK 11 unless the secp256k1 JNI floor moves too.

Nothing forces this. 3.3.8 itself still supports JDK 8+; only the *compilers* for
3.8.4/3.9.0 need 17+, and CI already runs 21.

## Gotcha found while running this

**Editing `build.sbt` while an sbt batch run is in flight silently invalidates the
run.** sbt hot-reloads on the change (`"build source files have changed"` ->
`"Reloading sbt..."`) and the reload **resets the session's `++<version>`** back to
the default. One JS/Native run reported "All tests passed" with exit 0 while
actually running on 3.3.8; the only symptom was `target/scala-3.3.8/` in the log.
Verify a cross-version run by the target directory, not the exit code:

```bash
grep -o "target/scala-3\.[0-9.]*" run.log | sort | uniq -c   # only the intended version
grep -c "Reloading sbt" run.log                              # must be 0
```

## Open question left alone

Dropping 3.8.4 now that 3.9.0 is supported would remove one CI job and one
published plugin artifact. 3.8.4 is a non-LTS line that 3.9.0 supersedes, and
both share the `since38` baseline generation, so the coverage overlap is real.
Kept by decision.
