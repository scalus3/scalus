import com.typesafe.tools.mima.core.*
import sbt.internal.util.ManagedLogger
import sbtwelcome.*
import java.net.URI
import scala.scalanative.build.*

// =============================================================================
// GLOBAL SETTINGS
// =============================================================================

Global / onChangedBuildSource := ReloadOnSourceChanges
autoCompilerPlugins := true

val scalusStableVersion = "1.1.0"
// The MiMa-checked stable surface is scalus-core, scalus-cardano-ledger and
// scalus-bloxbean-cardano-client-lib (see docs/superpowers/specs/2026-07-28-1.0.0-m1-release-plan-design.md).
// Re-baseline at each milestone: bump scalusStableVersion after the release artifacts are on
// Maven Central and delete the then-obsolete mimaBinaryIssueFilters. Between releases,
// intentional breaks require a reviewed mimaBinaryIssueFilters entry with a comment.
val scalusCompatibleVersion = scalusStableVersion

// Bloxbean Cardano Client Library versions
val cardanoClientLibVersion = "0.7.2"
val yaciVersion = "0.4.5"
val yaciCardanoTestVersion = "0.1.0"
val scalatestVersion = "3.2.20"
val scalatestPlusScalacheckVersion = "3.2.19.0"
val borerVersion = "1.17.0"
val slf4jVersion = "2.0.18"
val magnoliaVersion = "1.3.23"
val pprintVersion = "0.9.6"
val monocleVersion = "3.3.0"
val jsoniterScalaVersion = "2.40.1"

//ThisBuild / scalaVersion := "3.8.0-RC1-bin-SNAPSHOT"
//ThisBuild / scalaVersion := "3.3.7-RC1-bin-SNAPSHOT"
//ThisBuild / scalaVersion := "3.7.3-RC1-bin-SNAPSHOT"
// LTS is the default build version; the next series is used to cross-build the
// compiler plugin (which depends on the unstable scala3-compiler internal API).
val scala3LtsVersion = "3.3.8"
// Previous LTS patch. The compiler plugin (and scalus-core, to test it) still cross-build here so
// downstream projects pinned to 3.3.7 keep a published scalus-plugin_3.3.7. `publishOnlyLts` keeps
// the (compiler-version-independent) `_3` library artifacts published from the current LTS only.
val scala3LtsPrevVersion = "3.3.7"
val scala3NextVersion = "3.8.4"
ThisBuild / scalaVersion := scala3LtsVersion
ThisBuild / organization := "org.scalus"
ThisBuild / organizationName := "Scalus"
ThisBuild / organizationHomepage := Some(url("https://scalus.org/"))
ThisBuild / developers := List(
  Developer(
    id = "atlanter",
    name = "Alexander Nemish",
    email = "anemish@gmail.com",
    url = url("https://github.com/nau")
  )
)

ThisBuild / description := "Scalus - DApps Development Platform for Cardano"
ThisBuild / licenses := List(
  "Apache 2" -> new URI("http://www.apache.org/licenses/LICENSE-2.0.txt").toURL
)
ThisBuild / homepage := Some(url("https://github.com/scalus3/scalus"))
ThisBuild / versionScheme := Some("early-semver")
Test / publishArtifact := false

// BSP and semantic features
ThisBuild / semanticdbEnabled := true

// Pass JAVA_OPTS environment variable to forked test JVMs
// This allows configuring test JVM options via flake.nix or shell environment
//ThisBuild / Test / javaOptions ++= sys.env.get("JAVA_OPTS").toSeq.flatMap(_.split("\\s+"))

// Java version-specific JVM options
val javaVersion = sys.props("java.specification.version").toInt
// Enable native access for BLST JNI library (Java 22+)
ThisBuild / Test / javaOptions ++= (if (javaVersion >= 22) Seq("--enable-native-access=ALL-UNNAMED")
                                    else Nil)
// Suppress sun.misc.Unsafe deprecation warnings from Scala 3.3.x lazy vals (Java 23+)
ThisBuild / Test / javaOptions ++= (if (javaVersion >= 23)
                                        Seq("--sun-misc-unsafe-memory-access=allow")
                                    else Nil)
ThisBuild / run / javaOptions ++= (if (javaVersion >= 23)
                                       Seq("--sun-misc-unsafe-memory-access=allow")
                                   else Nil)

// Pin published Java bytecode to JDK 11 so artifacts stay loadable on JDK 11 regardless of the
// (newer) JDK used to build/publish – cardano-client-lib and the downstream consumers that bridge
// through scalus-bloxbean-cardano-client-lib target JDK 11. Only the Java sources need this; Scala
// already defaults to -Xtarget:8. Without it, building on JDK 21 emitted v65 Java classes.
ThisBuild / javacOptions ++= Seq("--release", "11")

// Improve incremental compilation
ThisBuild / incOptions := {
    incOptions.value
        .withLogRecompileOnMacro(false)
        .withUseOptimizedSealed(true)
}

// BSP development workflow optimizations
ThisBuild / watchBeforeCommand := Watch.clearScreen
ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger
ThisBuild / watchForceTriggerOnAnyChange := true

// Enable parallel execution
ThisBuild / parallelExecution := true
Global / concurrentRestrictions := Seq(
  Tags.limitAll(java.lang.Runtime.getRuntime.availableProcessors())
)

Compile / doc / scalacOptions ++= Seq(
  "-groups",
  "-project-version",
  scalusStableVersion,
  "-project-footer",
  "Lantr.io"
)

// =============================================================================
// COMMON SETTINGS
// =============================================================================

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-explain",
  "-Wunused:imports",
//  "-Wunused:params",
  "-Xcheck-macros"
  //  "-rewrite",
  //  "-source:future-migration"
) // ++ profilingScalacOptions

// Published JVM artifacts pin the emitted bytecode so the (newer) build JDK doesn't raise the
// runtime floor, and so shared source can't reference an API newer than that floor. The 3.3 LTS
// line targets JDK 11 (cardano-client-lib's floor – the LTS artifacts are what JDK 11 consumers
// use); Scala 3.8.x cannot emit below JDK 17 (its compiler requires 17), so those variants target
// 17. Compile-scoped (test code may still use newer APIs); JVM only – -release is rejected on JS/Native.
val jvmReleaseTarget = Compile / scalacOptions ++= {
    if (scalaVersion.value.startsWith("3.3.")) Seq("-release", "11") else Seq("-release", "17")
}

// Library artifacts use the binary `_3` (and `_sjs1_3` / `_native0.5_3`) suffix, so the 3.3 LTS
// and 3.8.x cross-builds publish to the SAME coordinates and overwrite each other – the 3.8.x
// build (JDK 17) would clobber the JDK 11 LTS one. Publish only the LTS build (it is Scala-3
// binary-compatible, so 3.8.x consumers can use it); 3.8.x stays cross-built for CI but unpublished.
// The compiler plugin is exempt: it uses CrossVersion.full, so its 3.8.x variants are distinct artifacts.
val publishOnlyLts = publish / skip := (scalaVersion.value != scala3LtsVersion)

val fs2Version = "3.12.2"

// Compilation profiling options for analyzing compilation time
lazy val profilingScalacOptions = Seq(
  "-Vprofile", // Basic compilation profiling with file complexity
  "-Vprofile-sorted-by:complexity", // Sort by complexity to identify slow files
  "-Vprofile-details:10",
  //  "-Yprofile-enabled",             // Enable advanced profiling
  //  "-Yprofile-trace:trace.log"                // Generate trace files for perfetto.dev visualization

)

lazy val copySharedFiles = taskKey[Unit]("Copy shared files")
lazy val prepareNpmPackage = taskKey[Unit]("Make an copy scalus bundle.js to npm directory")
lazy val runNpmTests = taskKey[Unit]("Run npm TypeScript tests")
lazy val generateDts = taskKey[Unit]("Generate scalus.d.ts from the Scala.js facades' TASTy")
lazy val checkDtsUpToDate = taskKey[Unit]("Fail if the committed scalus.d.ts is stale")
lazy val installNpmTestDeps =
    taskKey[File](
      "Install npm deps required by Scala.js tests (Node resolves them via node_modules walk-up)"
    )

// Scoped to ThisBuild so it is evaluated exactly once per sbt run, even though every JS
// project's tests depend on it. Defining it per-project would let sbt run several `npm
// install` processes concurrently in the same directory, which corrupts the esbuild binary
// install (ETXTBSY) – the marker check below is not enough because all of them race past it
// before any writes the marker.
ThisBuild / installNpmTestDeps := {
    val base = (LocalRootProject / baseDirectory).value
    val log = streams.value.log
    val nodeModules = base / "node_modules"
    val marker = nodeModules / ".scalus-test-deps-installed"
    val pkgJson = base / "package.json"
    if (!marker.exists() || pkgJson.lastModified() > marker.lastModified()) {
        log.info(s"Installing npm test dependencies in $base ...")
        val code = scala.sys.process.Process("npm" :: "install" :: Nil, base).!
        if (code != 0) sys.error("npm install for Scala.js test dependencies failed")
        IO.touch(marker)
    }
    nodeModules
}

// Shared settings for every Scala.js (cross) project.
//
// We emit standard ECMAScript modules and no longer use scalajs-bundler/webpack. npm
// dependencies (@noble/curves, @noble/hashes) are left as bare `import` specifiers in the
// linker output and resolved from node_modules, not bundled in. For tests, Node resolves
// those specifiers from the repo-root node_modules (it walks parent directories up from the
// linked test module), so we install them there before running tests.
lazy val jsModuleSettings: Seq[Def.Setting[?]] = Seq(
  scalaJSUseMainModuleInitializer := false,
  // withMinify is the Scala.js-native replacement for the now-deprecated Closure
  // Compiler. It's a no-op on top of esbuild's --minify for our current bundle, but we
  // keep it on as the officially-supported minification path for ESModule output.
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule).withMinify(true) },
  Test / executeTests := (Test / executeTests).dependsOn(ThisBuild / installNpmTestDeps).value,
  Test / testOnly := (Test / testOnly).dependsOn(ThisBuild / installNpmTestDeps).evaluated
)

// Scalus Compiler Plugin Dependency
lazy val PluginDependency: List[Def.Setting[?]] = List(scalacOptions ++= {
    val jar = (scalusPlugin / Compile / packageBin).value
    // add plugin timestamp to compiler options to trigger recompile of
    // main after editing the plugin. (Otherwise a 'clean' is needed.)

    // NOTE: uncomment for faster Scalus Plugin development
    // this will recompile the plugin when the jar is modified
    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
    // Seq(s"-Xplugin:${jar.getAbsolutePath}")
})

// =============================================================================
// AGGREGATE PROJECTS
// =============================================================================

lazy val root: Project = project
    .in(file("."))
    .aggregate(
      scalusPlugin,
      scalus.js,
      scalus.jvm,
      scalus.native,
      scalusUplcJitCompiler,
      scalusTsExporter,
      scalusTsExporterFixtures,
      scalusCardanoLedger.jvm,
      scalusCardanoLedger.js,
      scalusTestkit.js,
      scalusTestkit.jvm,
      scalusStreamingFs2.js,
      scalusStreamingFs2.jvm,
      scalusExamples.js,
      scalusExamples.jvm,
      scalusUtxoCell.js,
      scalusUtxoCell.jvm,
      scalusDesignPatterns,
      bench,
      `scalus-bloxbean-cardano-client-lib`,
      scalusEthereumKzgCeremony,
      scalusSbtPlugin,
      docs
    )
    .settings(
      name := "scalus",
      publish / skip := true,
    )

// all JVM projects are aggregated in the jvm project just for convenience
lazy val jvm: Project = project
    .in(file("jvm"))
    .aggregate(
      scalusPlugin,
      scalus.jvm,
      scalusUplcJitCompiler,
      scalusTsExporter,
      scalusCardanoLedger.jvm,
      scalusTestkit.jvm,
      scalusStreamingFs2.jvm,
      scalusExamples.jvm,
      scalusUtxoCell.jvm,
      scalusDesignPatterns,
      bench,
      llmApiGen,
      `scalus-bloxbean-cardano-client-lib`,
      scalusEthereumKzgCeremony,
    )
    .settings(
      publish / skip := true
    )

// all JS projects are aggregated in the js project just for convenience
lazy val js: Project = project
    .in(file("js"))
    .aggregate(
      scalus.js,
      scalusCardanoLedger.js,
      scalusTestkit.js,
      scalusStreamingFs2.js,
      scalusExamples.js,
      scalusUtxoCell.js,
    )
    .settings(
      publish / skip := true
    )

// all Native projects are aggregated in the native project just for convenience
lazy val native: Project = project
    .in(file("native"))
    .aggregate(
      scalus.native,
    )
    .settings(
      publish / skip := true
    )

// =============================================================================
// PROJECTS
// =============================================================================

// Scala 3 Compiler Plugin for Scalus
lazy val scalusPlugin = project
    .in(file("scalus-plugin"))
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      name := "scalus-plugin",
      crossVersion := CrossVersion.full,
      // A Scala 3 compiler plugin must match the compiler version of every version we support.
      // Includes the previous LTS (3.3.7) so downstream projects on 3.3.7 get a published plugin.
      crossScalaVersions := Seq(scala3LtsPrevVersion, scala3LtsVersion, scala3NextVersion),
      scalacOptions ++= commonScalacOptions,
      // Plugin links scala3-compiler; the 3.8.x line is JDK-17 bytecode, the 3.3 LTS line is JDK 11.
      jvmReleaseTarget,
//      scalacOptions += "-Wunused:all",
      // Manually set a fixed version to avoid recompilation on every commit
      // as sbt-ci-release plugin increments the version on every commit
      // thus recompiling the plugin and all dependent projects
      // COMMENT THIS LINE TO ENABLE VERSION INCREMENT during Scalus plugin development
      // COMMENT THIS LINE when doing plugin development
      // UPDATE VERSION after changes to the plugin
      // version := "0.13.0+597-4eafe96f+20251217-1256-SNAPSHOT",
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % scalatestPlusScalacheckVersion % "test",
      libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value // % "provided"
    )
    .settings(
      /*
       Include common sources in the plugin
       we can't add the scalus project as a dependency because this is a Scala compiler plugin
       and apparently it's not supported
       Another option is to use sbt-assembly to create a fat jar with all the dependencies
       I copy the shared files to the plugin project because when I use managedSources in the plugin
       IntelliJ IDEA only sees these files being used in the plugin project and not in the main project
       This breaks navigation and refactoring in the main project.
       By copying the shared files to the plugin project, IntelliJ IDEA sees them as used in the plugin project
       */
      copySharedFiles := {
          val sharedFiles = Seq(
            "scalus/uplc/builtin/BuiltinList.scala",
            "scalus/uplc/builtin/BuiltinValue.scala",
            "scalus/uplc/builtin/ByteStringFlatInstance.scala",
            "scalus/uplc/builtin/Data.scala",
            "scalus/compiler/sir/SIR.scala",
            "scalus/compiler/sir/SIRDefaultOptions.scala",
            "scalus/compiler/sir/SIRMacro.scala",
            "scalus/compiler/sir/SIRType.scala",
            "scalus/compiler/sir/SIRToExpr.scala",
            "scalus/compiler/sir/SIRBuiltins.scala",
            "scalus/compiler/sir/SIRUnify.scala",
            "scalus/compiler/sir/SIRHashCodeInRec.scala",
            "scalus/compiler/sir/RemoveRecursivity.scala",
            "scalus/compiler/sir/RenamingTypeVars.scala",
            "scalus/serialization/flat/Flat.scala",
            "scalus/serialization/flat/FlatCodec.scala",
            "scalus/serialization/flat/FlatInstances.scala",
            "scalus/serialization/flat/HashConsed.scala",
            "scalus/serialization/flat/HashConsedFlat.scala",
            "scalus/uplc/Constant.scala",
            "scalus/uplc/DefaultFun.scala",
            "scalus/uplc/DefaultUni.scala",
            "scalus/uplc/TypeScheme.scala",
            "scalus/utils/Hex.scala",
            "scalus/utils/ScalusSourcePos.scala",
          )

          val baseDir =
              baseDirectory.value / ".." / "scalus-core" / "shared" / "src" / "main" / "scala"
          val targetDir = (Compile / sourceDirectory).value / "shared" / "scala"
          val log = streams.value.log
          copyFiles(sharedFiles, baseDir, targetDir, log)
          log.info(s"Copied shared files to target $targetDir")
      },
//      Compile / managedSources ++= {
//          val baseDir = baseDirectory.value / ".." / "shared" / "src" / "main" / "scala"
//          sharedFiles.map(file => baseDir / file)
//      },
      Compile / unmanagedSourceDirectories += (Compile / sourceDirectory).value / "shared" / "scala",
      // Version-specific sources: the StandardPlugin phase-registration hook differs between the
      // 3.3.x LTS (`init`) and 3.5+/3.8.x (`initialize(using Context)`). See PluginCompat.
      Compile / unmanagedSourceDirectories += {
          val srcDir = (Compile / sourceDirectory).value
          CrossVersion.partialVersion(scalaVersion.value) match {
              case Some((3, minor)) if minor >= 5 => srcDir / "scala-3.8"
              case _                              => srcDir / "scala-3.3"
          }
      },
      cleanFiles += (Compile / sourceDirectory).value / "shared",
      // Ensure shared files are copied before any source inspection
      Compile / sourceGenerators += Def.task {
          copySharedFiles.value
          Seq.empty[File]
      }.taskValue,
      Compile / compile := (Compile / compile).dependsOn(copySharedFiles).value
    )

// Scalus Core and Standard Library for JVM and JS
lazy val scalus = crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("scalus-core"))
    .settings(
      name := "scalus",
      publishOnlyLts,
      scalaVersion := scalaVersion.value,
      // Includes the previous LTS (3.3.7) so the 3.3.7 plugin canary can build core + run
      // scalus.compiler.* against it. publishOnlyLts still publishes the `_3` artifact from the LTS.
      crossScalaVersions := Seq(scala3LtsPrevVersion, scala3LtsVersion, scala3NextVersion),
      scalacOptions ++= commonScalacOptions,
      scalacOptions += "-Xmax-inlines:100", // needed for upickle derivation of CostModel
      // scalacOptions += "-P:scalus:debugLevel=1",

      // Improve incremental compilation for cross-platform builds
      Compile / incOptions := {
          incOptions.value
              .withApiDebug(false)
              .withRelationsDebug(false)
              .withRecompileOnMacroDef(false)
      },
      // scalacOptions += "-Yretain-trees",
      mimaPreviousArtifacts := Set(organization.value %%% name.value % scalusCompatibleVersion),
      mimaBinaryIssueFilters ++= Seq(
        // Compiler-internal packages: no supported external implementors or instantiators;
        // excluded from the binary-compat promise (README: "compiler internals carry no
        // compatibility promise"; interop style guide: SIR compiler out of scope). Everything
        // user-facing stays checked - the `scalus.compiler` entry points, the `scalus.compiler.sir`
        // types appearing in `compile`'s signature and in plugin-generated bytecode, and all
        // MIXED packages (scalus.uplc, scalus.uplc.eval, scalus.serialization.flat, scalus.utils)
        // get per-symbol filters only, never wildcards. Known caveat: the wildcard also hides
        // deletion of the `lowering.simple` backend objects referenced by `sir.toUplc`.
        ProblemFilters.exclude[Problem]("scalus.compiler.sir.lowering.*"),
        ProblemFilters.exclude[Problem]("scalus.compiler.sir.transform.*"),
        ProblemFilters.exclude[Problem]("scalus.compiler.sir.linking.*"),
        ProblemFilters.exclude[Problem]("scalus.compiler.intrinsics.*"),
        ProblemFilters.exclude[Problem]("scalus.uplc.builtin.internal.*"),
        // scalus.uplc.internal: public utilitarian tooling (UPLC source-map renderer, profile
        // report writer) whose contract is the on-disk artifact formats, not the Scala API.
        ProblemFilters.exclude[Problem]("scalus.uplc.internal.*"),
        // TxInfo.redeemers changed from SortedMap to AssocMap. Redeemer keys are positional -
        // the ledger's map is `Map (PlutusPurpose AsIx era) _` and AsIx keeps only the index -
        // so no content-based Ord can track their order, and a sorted map's short-circuiting
        // lookup silently missed present keys. AssocMap does a linear Eq scan, as
        // PlutusTx.AssocMap and Aiken's Pairs do. Deliberate breaking change; the on-chain Data
        // encoding is unchanged, since both carry @UplcRepr(PackedDataMap).
        ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scalus.cardano.onchain.plutus.v2.TxInfo.apply"
        ),
        ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scalus.cardano.onchain.plutus.v2.TxInfo.copy"
        ),
        ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scalus.cardano.onchain.plutus.v2.TxInfo.this"
        ),
        ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scalus.cardano.onchain.plutus.v3.TxInfo.apply"
        ),
        ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scalus.cardano.onchain.plutus.v3.TxInfo.copy"
        ),
        ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scalus.cardano.onchain.plutus.v3.TxInfo.this"
        ),
        ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "scalus.cardano.onchain.plutus.v2.TxInfo._10"
        ),
        ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "scalus.cardano.onchain.plutus.v2.TxInfo.copy$default$10"
        ),
        ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "scalus.cardano.onchain.plutus.v2.TxInfo.redeemers"
        ),
        ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "scalus.cardano.onchain.plutus.v3.TxInfo.<init>$default$10"
        ),
        ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "scalus.cardano.onchain.plutus.v3.TxInfo._10"
        ),
        ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "scalus.cardano.onchain.plutus.v3.TxInfo.copy$default$10"
        ),
        ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "scalus.cardano.onchain.plutus.v3.TxInfo.redeemers"
        ),
        // StaticArgumentTransformation moved to scalus.compiler.sir.transform. Its public surface
        // (`apply`, `SatSuffix`) is preserved by a @deprecated forwarder at the old location, so
        // no filter is needed for it. These four are its Scala-*private* nested helper classes.
        // `private` on a nested class does not survive to the JVM: the class becomes its own
        // class file, whose access_flags cannot express ACC_PRIVATE, and scalac marks it public
        // in the InnerClasses attribute too - Scala enforces template-privacy from TASTy at
        // compile time, not via JVM flags. MiMa reads bytecode, so it sees a public class
        // disappear. No Scala caller could ever have referenced them.
        ProblemFilters.exclude[MissingClassProblem]("scalus.compiler.sir.StaticArgumentTransformation$Analysis"),
        ProblemFilters.exclude[MissingClassProblem]("scalus.compiler.sir.StaticArgumentTransformation$Lam"),
        ProblemFilters.exclude[MissingClassProblem]("scalus.compiler.sir.StaticArgumentTransformation$Lam$"),
        ProblemFilters.exclude[MissingClassProblem]("scalus.compiler.sir.StaticArgumentTransformation$Rewriter"),
        // Deleted: both compared only the GovAction constructor ordinal, so distinct proposals
        // compared equal and a SortedSet would have silently dropped one - a violation of the
        // Ordering contract, not merely a weak order. Neither was used: proposalProcedures is a
        // TaggedOrderedSet, which preserves submitter order and never sorts.
        ProblemFilters.exclude[DirectMissingMethodProblem](
          "scalus.cardano.ledger.GovAction.given_Ordering_GovAction"
        ),
        ProblemFilters.exclude[DirectMissingMethodProblem](
          "scalus.cardano.ledger.ProposalProcedure.given_Ordering_ProposalProcedure"
        ),
        // Scala.js only: the JS SlotConfig no longer has these members. Every public member of
        // a @JSExportTopLevel js.Object is a linker export root, so one mentioning
        // java.time.Instant kept the ~800 KB IANA timezone database in scalus.js, for an API
        // JavaScript callers cannot use anyway. The JVM and Native SlotConfig still have them;
        // shared code converts through POSIX milliseconds. See docs/internal/JS_BUNDLE_SIZE.md.
        ProblemFilters.exclude[DirectMissingMethodProblem](
          "scalus.cardano.ledger.SlotConfig.slotToInstant"
        ),
        ProblemFilters.exclude[DirectMissingMethodProblem](
          "scalus.cardano.ledger.SlotConfig.instantToSlot"
        ),
        // The upickle ReadWriter vals no longer run in their enclosing object's constructor, so
        // touching a domain companion no longer builds a JSON codec, which is what kept upickle
        // in scalus.js. No API member changed; this package object simply no longer needs a
        // static initializer. See docs/internal/JS_BUNDLE_SIZE.md.
        ProblemFilters.exclude[DirectMissingMethodProblem](
          "scalus.uplc.eval.CostModel#package.<clinit>"
        )
      ),

      // enable when debug compilation of tests
      Test / scalacOptions += "-color:never",
      PluginDependency,
      libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0",
      libraryDependencies += "org.typelevel" %%% "cats-parse" % "1.1.0",
      libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.4.4",
      libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.4.3",
      libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % jsoniterScalaVersion,
      libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % jsoniterScalaVersion % "compile",
      libraryDependencies ++= Seq(
        "io.bullet" %%% "borer-core" % borerVersion,
        "io.bullet" %%% "borer-derivation" % borerVersion
      ),
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % magnoliaVersion % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % scalatestPlusScalacheckVersion % "test",
      libraryDependencies ++= Seq(
        "dev.optics" %%% "monocle-core" % monocleVersion,
        "dev.optics" %%% "monocle-macro" % monocleVersion,
      ),
      buildInfoKeys ++= Seq[BuildInfoKey](
        "scalusVersion" -> scalusStableVersion
      ),
      buildInfoPackage := "scalus.utils"
    )
    .configure { project =>
        project.enablePlugins(BuildInfoPlugin)
    }
    .jvmSettings(
      jvmReleaseTarget,
      Test / fork := true,
      // Run forked tests from project root so paths are consistent across platforms
      Test / baseDirectory := (LocalRootProject / baseDirectory).value,
      // Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-S", "-8077211454138081902"),
      Test / testOptions += Tests.Argument("-oF"),
      Test / testOptions += Tests.Argument("-l", "scalus.testing.Benchmark"),
      libraryDependencies += "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
      // Negative-compilation tests (ByNameParamErrorTest) drive dotc in-process with the
      // packaged Scalus plugin and the full scalus-core classpath, handed to the forked
      // test JVM as system properties.
      libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Test,
      Test / javaOptions ++= Seq(
        s"-Dscalus.plugin.jar=${(scalusPlugin / Compile / packageBin).value.getAbsolutePath}",
        s"-Dscalus.test.classpath=${(Test / fullClasspath).value.files.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)}"
      ),
      libraryDependencies += "org.bouncycastle" % "bcprov-jdk18on" % "1.85.2",
      libraryDependencies += "foundation.icon" % "blst-java" % "0.3.2",
      libraryDependencies += "org.scalus" % "scalus-secp256k1-jni" % "0.6.0",
      // Ethereum KZG ceremony JSON is in scalus-ethereum-kzg-ceremony resources, needed for benchmark tests
      Test / unmanagedResourceDirectories += (LocalRootProject / baseDirectory).value / "scalus-ethereum-kzg-ceremony" / "src" / "main" / "resources"
    )
    .jsSettings(jsModuleSettings *)
    .jsSettings(
      // Add JS-specific settings here
      // Disable doc due to scaladoc NPE bug on JS platform
      Compile / doc / sources := Seq.empty,
      Test / doc / sources := Seq.empty
    )
    .nativeSettings(
      // Scala Native 0.5.12 supports 3.8.4, so Native tracks the same versions as JVM/JS.
      // Run the next-version native tests with `++3.8.4 scalusNative/test`.
      crossScalaVersions := Seq(scala3LtsVersion, scala3NextVersion),
      // Each native test group runs as its own statically-linked binary with its own
      // immix-GC heap. Running them in parallel exhausted RAM on 16GB CI runners and the
      // OOM-killer took down the job (SIGKILL/137). Serialize so only one native test
      // process is live at a time.
      Test / parallelExecution := false,
      // Disable doc due to scaladoc NPE bug on Native platform
      Compile / doc / sources := Seq.empty,
      Test / doc / sources := Seq.empty,
      nativeConfig ~= {
          _.withBuildTarget(BuildTarget.libraryStatic)
//              .withLTO(LTO.thin)
              .withMode(Mode.releaseFast)
              .withGC(GC.immix)
      },
      // Set library path for Scala Native test execution to find libblst at runtime.
      // BLST_NATIVE_LIB_PATH is provided by flake.nix separately from DYLD_LIBRARY_PATH/LD_LIBRARY_PATH
      // to avoid conflicts with blst-java on JVM (see flake.nix shellHook comment for details).
      Test / envVars ++= {
          val blstPath = sys.env.getOrElse("BLST_NATIVE_LIB_PATH", "")
          if (blstPath.nonEmpty) {
              val isMac = sys.props.get("os.name").exists(_.toLowerCase.contains("mac"))
              val pathVar = if (isMac) "DYLD_LIBRARY_PATH" else "LD_LIBRARY_PATH"
              val existingPath = sys.env.getOrElse(pathVar, "")
              val newPath = if (existingPath.nonEmpty) s"$blstPath:$existingPath" else blstPath
              Map(pathVar -> newPath)
          } else Map.empty
      }
    )

// Scalus UPLC JIT Compiler - experimental JIT compiler for UPLC
lazy val scalusUplcJitCompiler = project
    .in(file("scalus-uplc-jit-compiler"))
    .dependsOn(scalus.jvm % "compile->compile;test->test")
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      name := "scalus-uplc-jit-compiler",
      scalaVersion := scalaVersion.value,
      scalacOptions ++= commonScalacOptions,
      Test / fork := true,
      Test / javaOptions ++= Seq(
        "-Xss64m", // Increase stack size to 64MB for JIT compilation of deeply nested UPLC terms
        "-Xmx4g" // Increase heap to 4GB for large compilations
      ),
      // Skip scalus.jvm compilation when -DskipScalusRecompile=true
      scalus.jvm / Compile / skip := sys.props.get("skipScalusRecompile").contains("true"),
      scalus.jvm / Test / skip := sys.props.get("skipScalusRecompile").contains("true"),
      scalusPlugin / Compile / skip := sys.props.get("skipScalusRecompile").contains("true"),
      libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
      libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
      libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
      // Exclude benchmark-tagged tests from default test runs
      Test / testOptions += Tests.Argument("-l", "scalus.testing.Benchmark"),
      // Full stack traces for test failures (helps debug deep lowering errors)
      Test / testOptions += Tests.Argument("-oF"),
      inConfig(Test)(PluginDependency),
      publish / skip := true
    )

// TypeScript definitions generator: reads TASTy of the Scala.js modules and
// emits scalus.d.ts. See docs/superpowers/specs/2026-08-03-scalajs-typescript-definitions-generator-design.md
lazy val scalusTsExporter = project
    .in(file("scalus-ts-exporter"))
    .disablePlugins(MimaPlugin)
    .settings(
      name := "scalus-ts-exporter",
      scalacOptions ++= commonScalacOptions,
      libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
      libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
      // Main calls sys.exit(1) when the generator finds export errors. sbt's TrapExit
      // SecurityManager is gone on modern JDKs, so an in-process run would take the whole
      // sbt JVM (and, under sbtn, the server) down with it. Forking turns that exit into a
      // plain non-zero exit code, which `runner.run` reports as a task failure.
      Compile / run / fork := true,
      Test / fork := true,
      Test / javaOptions ++= Seq(
        s"-Dtsexport.fixtures.classes=${(scalusTsExporterFixtures / Compile / classDirectory).value.getAbsolutePath}",
        s"-Dtsexport.fixtures.classpath=${(scalusTsExporterFixtures / Compile / fullClasspath).value.map(_.data.getAbsolutePath).mkString(java.io.File.pathSeparator)}",
        s"-Dtsexport.sourceroot=${(ThisBuild / baseDirectory).value.getAbsolutePath}"
      ),
      // GoldenTest type-checks the generated fixtures with `node_modules/.bin/tsc` and
      // `assume`s the check away when tsc is missing, which scalatest reports as a canceled
      // test inside a green suite. ci-jvm runs these tests through the `jvm` aggregate, which
      // otherwise never installs the root node_modules, so the tsc check silently never ran.
      // Depend on the install task explicitly (same idiom as jsModuleSettings).
      Test / executeTests := (Test / executeTests).dependsOn(ThisBuild / installNpmTestDeps).value,
      Test / testOnly := (Test / testOnly).dependsOn(ThisBuild / installNpmTestDeps).evaluated,
      publish / skip := true
    )

// Scala.js fixture code exercising every export shape the generator supports.
// Compiled only so its TASTy can be inspected by scalusTsExporter tests.
lazy val scalusTsExporterFixtures = project
    .in(file("scalus-ts-exporter/fixtures"))
    .enablePlugins(ScalaJSPlugin)
    .disablePlugins(MimaPlugin)
    .settings(
      name := "scalus-ts-exporter-fixtures",
      scalacOptions ++= commonScalacOptions,
      publish / skip := true
    )

// Scalus Testkit library for testing Scalus applications
lazy val scalusTestkit = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-testkit"))
    .dependsOn(scalus % "compile->compile", scalusCardanoLedger)
    .settings(
      name := "scalus-testkit",
      publishOnlyLts,
      scalaVersion := scalaVersion.value,
      crossScalaVersions := Seq(scala3LtsVersion, scala3NextVersion),
      scalacOptions ++= commonScalacOptions,
      scalacOptions += "-Xmax-inlines:100", // needed for Arbitrary[Certificate] = autoDerived

      // Improve incremental compilation for cross-platform builds
      Compile / incOptions := {
          incOptions.value
              .withApiDebug(false)
              .withRelationsDebug(false)
              .withRecompileOnMacroDef(false)
      },
      Test / scalacOptions += "-color:never",
      // Copy shared test files from scalus-core to managed sources
      Compile / sourceGenerators += Def.task {
          val baseDir =
              (scalus.jvm / crossProjectBaseDirectory).value / "shared" / "src" / "test" / "scala"
          val targetDir = (Compile / sourceManaged).value
          val files = Seq(
            "scalus/testing/ArbitraryDerivation.scala",
            "scalus/uplc/test/ArbitraryInstances.scala",
            "scalus/ledger/api/v1/ArbitraryInstances.scala",
            "scalus/ledger/api/v2/ArbitraryInstances.scala",
            "scalus/ledger/api/v3/ArbitraryInstances.scala",
            "scalus/cardano/address/ArbitraryInstances.scala",
            "scalus/cardano/ledger/ArbitraryInstances.scala",
            "scalus/testing/kit/EvalTestKit.scala",
          )
          copyFiles(files, baseDir, targetDir, streams.value.log)
          files.map(targetDir / _)
      }.taskValue,
      PluginDependency,
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % magnoliaVersion,
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % scalatestPlusScalacheckVersion,
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion,
      libraryDependencies += "io.github.dotty-cps-async" %%% "dotty-cps-async" % "1.3.4",
      libraryDependencies += "io.github.dotty-cps-async" %%% "dotty-cps-async-logic" % "1.3.4",
      // Copy Party.scala and TestUtil.scala from cardano-ledger test sources
      Compile / sourceGenerators += Def.task {
          val baseDir =
              (scalusCardanoLedger.jvm / crossProjectBaseDirectory).value / "shared" / "src" / "test" / "scala"
          val targetDir = (Compile / sourceManaged).value
          val files = Seq(
            "scalus/testing/kit/Party.scala",
            "scalus/testing/kit/TestUtil.scala"
          )
          copyFiles(files, baseDir, targetDir, streams.value.log)
          files.map(targetDir / _)
      }.taskValue,
    )
    .jvmSettings(
      jvmReleaseTarget,
      // ScalusTest.generateKeyPair references the platform-specific KeyPairGenerator,
      // which lives in scalus-core's JVM test sources. Copy it into the published jar
      // (it isn't part of the shared test sources copied above).
      Compile / sourceGenerators += Def.task {
          val baseDir =
              (scalus.jvm / crossProjectBaseDirectory).value / "jvm" / "src" / "test" / "scala"
          val targetDir = (Compile / sourceManaged).value
          val files = Seq("scalus/testing/kit/KeyPairGenerator.scala")
          copyFiles(files, baseDir, targetDir, streams.value.log)
          files.map(targetDir / _)
      }.taskValue,
      // Add Yaci DevKit dependencies for integration testing
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion,
      libraryDependencies += "com.bloxbean.cardano" % "yaci-cardano-test" % yaciCardanoTestVersion,
      libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.26",
      libraryDependencies += "org.slf4j" % "slf4j-simple" % slf4jVersion % Test
    )
    .jsSettings(jsModuleSettings *)
    .jsSettings(
      // JS counterpart of the KeyPairGenerator copy above (uses @noble/curves).
      Compile / sourceGenerators += Def.task {
          val baseDir =
              (scalus.js / crossProjectBaseDirectory).value / "js" / "src" / "test" / "scala"
          val targetDir = (Compile / sourceManaged).value
          val files = Seq("scalus/testing/kit/KeyPairGenerator.scala")
          copyFiles(files, baseDir, targetDir, streams.value.log)
          files.map(targetDir / _)
      }.taskValue
    )

// fs2 adapter for the streaming facade: a ScalusAsyncStream instance and nothing else.
// Deliberately tiny – the buffering and fan-out semantics live in scalus-cardano-ledger so that
// every adapter shares one implementation of them rather than one interpretation each.
lazy val scalusStreamingFs2 = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-streaming-fs2"))
    .dependsOn(scalusCardanoLedger)
    .settings(
      name := "scalus-streaming-fs2",
      publishOnlyLts,
      crossScalaVersions := Seq(scala3LtsVersion, scala3NextVersion),
      scalacOptions ++= commonScalacOptions,
      libraryDependencies += "co.fs2" %%% "fs2-core" % fs2Version,
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
    )
    .jvmSettings(jvmReleaseTarget)
    .jsSettings(jsModuleSettings *)

lazy val scalusExamples = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-examples"))
    .dependsOn(scalus, scalusTestkit)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .enablePlugins(ScalusSbtPlugin)
    .settings(
      crossScalaVersions := Seq(scala3LtsVersion, scala3NextVersion),
      PluginDependency,
      scalacOptions ++= commonScalacOptions,
      publish / skip := true,
      libraryDependencies += "io.bullet" %%% "borer-derivation" % borerVersion,
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % magnoliaVersion % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % scalatestPlusScalacheckVersion % "test",
      libraryDependencies += "com.lihaoyi" %%% "pprint" % pprintVersion % "test",
      // Exclude integration tests and benchmarks from default test runs
      Test / testOptions += Tests.Argument("-l", "scalus.testing.IntegrationTest"),
      Test / testOptions += Tests.Argument("-l", "scalus.testing.Benchmark")
    )
    .configurePlatform(JVMPlatform)(
      _.dependsOn(
        scalusDesignPatterns,
        scalusEthereumKzgCeremony
      )
    )
    .jvmSettings(
      Test / fork := true,
      // Expose the compiler version to tests so they can pick version-specific baselines
      // (budgets / script sizes drift between the 3.3.x LTS and 3.8.x). See ScalaCompilerVersion.
      Test / javaOptions += s"-Dscalus.test.scalaVersion=${scalaVersion.value}"
    )
    .jsSettings(jsModuleSettings *)
    .jsSettings(
      Test / envVars := sys.env.toMap // for HTLC integration tests
    )

lazy val scalusUtxoCell = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-utxo-cell"))
    .dependsOn(
      scalus % "compile->compile;compile->test",
      scalusCardanoLedger % "compile->compile;test->test"
    )
    .disablePlugins(MimaPlugin)
    .settings(
      name := "scalus-utxo-cell",
      scalacOptions ++= commonScalacOptions,
      PluginDependency,
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
      libraryDependencies += "com.lihaoyi" %%% "pprint" % pprintVersion % "test",
      libraryDependencies += "io.github.dotty-cps-async" %%% "dotty-cps-async" % "1.3.4",
      publish / skip := true
    )
    .jvmSettings(Test / fork := true)
    .jsSettings(jsModuleSettings *)

lazy val scalusDesignPatterns = project
    .in(file("scalus-design-patterns"))
    .dependsOn(scalus.jvm, scalusTestkit.jvm)
    // MiMa disabled until the first release establishes a baseline artifact to compare against.
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      name := "scalus-design-patterns",
      crossScalaVersions := Seq(scala3LtsVersion, scala3NextVersion),
      PluginDependency,
      scalacOptions ++= commonScalacOptions,
      jvmReleaseTarget,
      publishOnlyLts,
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % scalatestPlusScalacheckVersion % "test",
      Test / fork := true
      //// enable if need speedup
      // trackInternalDependencies := TrackLevel.TrackIfMissing,
    )

// Bloxbean Cardano Client Lib integration and Tx Evaluator implementation
lazy val `scalus-bloxbean-cardano-client-lib` = project
    .in(file("bloxbean-cardano-client-lib"))
    .dependsOn(scalus.jvm, scalusCardanoLedger.jvm)
    .settings(
      crossScalaVersions := Seq(scala3LtsVersion, scala3NextVersion),
      publishOnlyLts,
      scalacOptions ++= commonScalacOptions,
      jvmReleaseTarget,
      mimaPreviousArtifacts := Set(organization.value %% name.value % scalusCompatibleVersion),
      mimaBinaryIssueFilters ++= Seq(
        // Removed: it ordered staking credentials by raw hash bytes, ignoring whether the
        // credential is a script or a key, which is an order no node emits. Withdrawal
        // ordering now goes through ledgerOrderedWithdrawals / getWithdrawals in Interop.
        ProblemFilters.exclude[DirectMissingMethodProblem](
          "scalus.bloxbean.Interop#package.given_Ordering_StakingHash"
        )
      ),
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion,
      libraryDependencies += "org.slf4j" % "slf4j-api" % slf4jVersion,
      libraryDependencies += "org.slf4j" % "slf4j-simple" % slf4jVersion % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % cardanoClientLibVersion % "test",
      libraryDependencies += "com.bloxbean.cardano" % "yaci" % yaciVersion % "test",
      libraryDependencies += "io.bullet" %%% "borer-derivation" % borerVersion,
      libraryDependencies += "com.bloxbean.cardano" % "yaci-cardano-test" % yaciCardanoTestVersion % "test",
      libraryDependencies += "com.lihaoyi" %%% "pprint" % pprintVersion % "test",
      Test / fork := true, // needed for BlocksValidation to run in sbt
      inConfig(Test)(PluginDependency)
    )

// Documentation
lazy val docs = project // documentation project
    .in(file("scalus-docs")) // important: it must not be docs/
    .dependsOn(scalus.jvm)
    .enablePlugins(ScalaUnidocPlugin)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      publish / skip := true,
      moduleName := "scalus-docs",
      ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
        scalus.jvm,
        scalusCardanoLedger.jvm,
        scalusTestkit.jvm
      ),
      ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "scalus-site" / "public" / "api",
      PluginDependency
    )

// Benchmarks for Cardano Plutus VM Evaluator
lazy val bench = project
    .in(file("bench"))
    .dependsOn(
      scalus.jvm,
      scalusUplcJitCompiler,
      scalusCardanoLedger.jvm,
      // Depend on test scope to use ResourcesUtxoResolver and test resources (block/UTxO CBOR files)
      `scalus-bloxbean-cardano-client-lib` % "compile->compile;compile->test"
    )
    .enablePlugins(JmhPlugin)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      name := "scalus-bench",
      PluginDependency,
      publish / skip := true,
      packageBin / skip := true,
      // Increase stack size for JIT compilation and deeply nested UPLC terms
      Jmh / javaOptions ++= Seq(
        "-Xss64m", // Increase stack size to 64MB (default is usually 1MB)
        "-Xmx4g" // Increase heap to 4GB for large compilations
      ),
      // Fix JMH compilation issues - disable incremental compilation
      Jmh / incOptions := (Jmh / incOptions).value.withEnabled(false),
      run / fork := true,
      libraryDependencies += "org.slf4j" % "slf4j-simple" % slf4jVersion,
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion,
      libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.22.1",
      libraryDependencies += "io.bullet" %%% "borer-core" % borerVersion,
      libraryDependencies += "io.bullet" %%% "borer-derivation" % borerVersion
    )

// Generates scalus-site/public/llms-api.txt - the LLM-facing public API cheatsheet
lazy val llmApiGen = project
    .in(file("llm-api-gen"))
    .dependsOn(scalus.jvm, scalusCardanoLedger.jvm, scalusTestkit.jvm)
    .disablePlugins(MimaPlugin)
    .settings(
      name := "llm-api-gen",
      publish / skip := true,
      run / fork := true,
      libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value
    )

lazy val generateLlmsApi = taskKey[Unit]("Generate scalus-site/public/llms-api.txt")
generateLlmsApi := Def.taskDyn {
    val dirs = Seq(
      (scalus.jvm / Compile / classDirectory).value,
      (scalusCardanoLedger.jvm / Compile / classDirectory).value,
      (scalusTestkit.jvm / Compile / classDirectory).value
    ).map(_.getAbsolutePath)
    val outFile =
        ((ThisBuild / baseDirectory).value / "scalus-site" / "public" / "llms-api.txt").getAbsolutePath
    val argLine = (Seq(outFile, version.value) ++ dirs).mkString(" ")
    (llmApiGen / Compile / runMain).toTask(s" scalus.llmapi.LlmApiGen $argLine")
}.value

// Cardano Ledger domain model and CBOR serialization
lazy val scalusCardanoLedger = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-cardano-ledger"))
    .dependsOn(scalus % "compile->compile;test->test")
    .settings(
      name := "scalus-cardano-ledger",
      mimaPreviousArtifacts := Set(organization.value %%% name.value % scalusCompatibleVersion),
      mimaBinaryIssueFilters ++= Seq(
        // DefaultImpl is a private nested class (MiMa still sees its members); the protected
        // evalScript hook now takes the TransactionHash instead of a pre-encoded hex String,
        // so the default evaluation path skips hex encoding entirely.
        ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scalus.cardano.ledger.PlutusScriptEvaluator#DefaultImpl.evalScript"
        ),
        // The streaming hub's internals churn while the provider implementations are built –
        // `AppliedBlock` has already gained a field. Scoped to `.internal` on purpose: CLAUDE.md
        // reserves wildcards for wholly-internal packages, and a wildcard over the whole
        // `...node.stream` package would also silence a genuine break in the public facade
        // (`BlockchainStreamProvider`, `StreamCapabilities`, `SubscriptionOptions`, …).
        // The proposal does declare the public facade unfrozen for a release or two as well; when
        // a break there is actually needed, it gets its own filter naming the symbol, so the thing
        // being broken is visible in review rather than pre-authorised in bulk.
        ProblemFilters.exclude[Problem]("scalus.cardano.node.stream.internal.*")
      ),
      crossScalaVersions := Seq(scala3LtsVersion, scala3NextVersion),
      scalacOptions ++= commonScalacOptions,
      scalacOptions += "-Xmax-inlines:100", // needed for upickle derivation of CostModel
      libraryDependencies ++= Seq(
        "io.bullet" %%% "borer-core" % borerVersion,
        "io.bullet" %%% "borer-derivation" % borerVersion
      ),
      // For tx builder
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion,
      libraryDependencies += "com.outr" %%% "scribe" % "3.19.0", // logging
      libraryDependencies ++= Seq(
        "dev.optics" %%% "monocle-core" % monocleVersion,
        "dev.optics" %%% "monocle-macro" % monocleVersion,
      ),
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % magnoliaVersion % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % scalatestPlusScalacheckVersion % "test",
      libraryDependencies += "com.lihaoyi" %%% "pprint" % pprintVersion % "test",
      libraryDependencies += "com.softwaremill.sttp.client4" %%% "core" % "4.0.26",
      inConfig(Test)(PluginDependency),
      publishOnlyLts
    )
    .jvmSettings(
      jvmReleaseTarget,
      // For conformance test vector extraction
      libraryDependencies += "org.apache.commons" % "commons-compress" % "1.28.0" % "test"
    )
    .jsSettings(jsModuleSettings *)
    .jsSettings(
      // JS-only facade, so these belong here and not in the shared settings: submitTx (both
      // overloads) and getDelegation narrowed their return types from js.Dynamic to typed
      // js.Object traits (spec 2026-08-03 TS definitions generator, decision "Typed returns +
      // MiMa filters"). Runtime shape unchanged. Verified required: without them
      // `scalusCardanoLedgerJS/mimaReportBinaryIssues` reports 3 problems against
      // org.scalus:scalus-cardano-ledger_sjs1_3:1.1.0.
      mimaBinaryIssueFilters ++= Seq(
        ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "scalus.cardano.node.JEmulator.submitTx"
        ),
        ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "scalus.cardano.node.JEmulator.getDelegation"
        )
      ),
      // Publish the Scala.js ESModule output as a single-file ESM bundle (scalus.js).
      // The Scala.js linker emits standard ES modules; we run esbuild over the linker
      // output to collapse any internal chunks into one file and minify. @noble/* are
      // inlined into the bundle so scalus.js is fully self-contained – it can be loaded
      // directly in a browser `<script type="module">` with no import map or npm install.
      prepareNpmPackage := {
          (Compile / fullLinkJS).value
          val linkerOutputDir = (Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
          val nodeModules = (ThisBuild / installNpmTestDeps).value
          val npmDir = (Compile / sourceDirectory).value / "npm"
          val log = streams.value.log
          val esbuild = nodeModules / ".bin" / "esbuild"
          val entry = linkerOutputDir / "main.js"
          val outFile = npmDir / "scalus.js"
          val cmd = Seq(
            esbuild.getAbsolutePath,
            entry.getAbsolutePath,
            "--bundle",
            "--format=esm",
            "--platform=node",
            "--minify",
            "--legal-comments=none",
            s"--outfile=${outFile.getAbsolutePath}"
          )
          log.info(cmd.mkString(" "))
          val code = scala.sys.process.Process(cmd).!
          if (code != 0) sys.error("esbuild bundling of scalus.js failed")
          log.info(s"Wrote ESM bundle to $outFile (${outFile.length} bytes)")
      },
      runNpmTests := {
          import scala.sys.process.*
          val npmDir = (Compile / sourceDirectory).value / "npm"
          val log = streams.value.log
          log.info("Installing npm dependencies...")
          val installExitCode = Process("npm" :: "install" :: Nil, npmDir).!
          if (installExitCode != 0) {
              throw new RuntimeException("npm install failed")
          }
          log.info("Running TypeScript tests...")
          val testExitCode = Process("npm" :: "test" :: Nil, npmDir).!
          if (testExitCode != 0) {
              throw new RuntimeException("npm tests failed")
          }
      },
      runNpmTests := runNpmTests.dependsOn(prepareNpmPackage).value,
      // Generate scalus.d.ts from the facades' TASTy (scalus-core JS + this module).
      // The file is committed; checkDtsUpToDate gates drift in ci-js.
      // `Compile / fullClasspath` already forces `Compile / compile` (it contains the
      // project's own exportedProducts), so the classpaths below are all the dependency
      // this task needs. Arguments are passed to the runner as a real Seq[String]: the
      // `runMain` command line splits on whitespace, which breaks on any checkout path (or
      // classpath jar) containing a space.
      generateDts := {
          val _ = (scalus.js / Compile / compile).value
          val coreClasses = (scalus.js / Compile / classDirectory).value.getAbsolutePath
          val ledgerClasses = (Compile / classDirectory).value.getAbsolutePath
          val cp = (Compile / fullClasspath).value
              .map(_.data.getAbsolutePath)
              .mkString(java.io.File.pathSeparator)
          val out = ((Compile / sourceDirectory).value / "npm" / "scalus.d.ts").getAbsolutePath
          val srcRoot = (ThisBuild / baseDirectory).value.getAbsolutePath
          val args = List(
            "--tasty-root",
            coreClasses,
            "--tasty-root",
            ledgerClasses,
            "--classpath",
            cp,
            "--output",
            out,
            "--source-root",
            srcRoot
          )
          // `run / runner` (not the config-scoped `runner`) is the forked one, see the
          // `Compile / run / fork` setting on scalusTsExporter.
          val exporterCp = (scalusTsExporter / Runtime / fullClasspath).value.map(_.data)
          (scalusTsExporter / Compile / run / runner).value
              .run("scalus.tsexport.Main", exporterCp, args, streams.value.log)
              .failed
              .foreach(cause => sys.error(s"scalus-ts-exporter failed: ${cause.getMessage}"))
      },
      // The generated file is committed, so this gate must compare it with HEAD, not with the
      // index: plain `git diff --exit-code` reports no difference for a staged change and for
      // an untracked file, so an unreviewed (or never committed) scalus.d.ts passed it. Being
      // unable to run the check at all (no git, no repository) is a different failure from a
      // stale file and says so, instead of blaming the file.
      checkDtsUpToDate := {
          generateDts.value
          val out = (Compile / sourceDirectory).value / "npm" / "scalus.d.ts"
          val root = (ThisBuild / baseDirectory).value
          val quiet = scala.sys.process.ProcessLogger(_ => (), _ => ())
          def cannotCheck(reason: String): Nothing =
              sys.error(
                s"cannot check whether ${out.getName} is up to date: $reason. " +
                    s"Compare $out with HEAD by hand."
              )
          def git(args: String*): Int =
              try scala.sys.process.Process("git" +: args, root).!(quiet)
              catch {
                  case scala.util.control.NonFatal(e) =>
                      cannotCheck(s"running git in $root failed (${e.getMessage})")
              }
          if (git("rev-parse", "--git-dir") != 0)
              cannotCheck(s"$root is not a git checkout")
          git("ls-files", "--error-unmatch", "--", out.getAbsolutePath) match {
              case 0 => ()
              case 1 =>
                  sys.error(
                    s"${out.getName} is not tracked by git. Run `git add $out` and commit the " +
                        "generated file."
                  )
              case code => cannotCheck(s"`git ls-files` exited with $code")
          }
          git("diff", "--exit-code", "HEAD", "--", out.getAbsolutePath) match {
              case 0 => ()
              case 1 =>
                  sys.error(
                    s"${out.getName} is out of date. Run scalusCardanoLedgerJS/generateDts and " +
                        "commit the result."
                  )
              case code => cannotCheck(s"`git diff HEAD` exited with $code")
          }
      },
      prepareNpmPackage := prepareNpmPackage.dependsOn(generateDts).value
    )

// sbt plugin for blueprint generation
lazy val scalusSbtPlugin = project
    .in(file("scalus-sbt-plugin"))
    .enablePlugins(SbtPlugin)
    .disablePlugins(MimaPlugin)
    .settings(
      name := "scalus-sbt-plugin",
      sbtPlugin := true,
      scalaVersion := "2.12.21",
      // Cross-build for sbt 1 (Scala 2.12) and sbt 2 (Scala 3). A single sbt 1.x launcher
      // builds and publishes both axes via pluginCrossBuild. The sbt 1.x baseline is the
      // minimum version consumers may use; sbt2-compat 0.2.0 requires sbt >= 1.9.
      crossScalaVersions := Seq("2.12.21", scala3NextVersion),
      pluginCrossBuild / sbtVersion := {
          scalaBinaryVersion.value match {
              case "2.12" => "1.9.0" // minimum sbt 1.x baseline
              case _      => "2.0.0" // sbt 2.x
          }
      },
      // shared-source shim so one source set compiles against both sbt 1 and sbt 2 APIs
      addSbtPlugin("com.github.sbt" % "sbt2-compat" % "0.2.0"),
      scalacOptions ++= Seq("-deprecation", "-feature"),
      libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    )

// Ethereum KZG ceremony trusted setup for bilinear accumulators
lazy val scalusEthereumKzgCeremony = project
    .in(file("scalus-ethereum-kzg-ceremony"))
    .dependsOn(scalus.jvm)
    .disablePlugins(MimaPlugin)
    .settings(
      crossScalaVersions := Seq(scala3LtsVersion, scala3NextVersion),
      name := "scalus-ethereum-kzg-ceremony",
      scalacOptions ++= commonScalacOptions,
      libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % jsoniterScalaVersion,
      libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterScalaVersion % "compile",
    )

lazy val scalusCardanoLedgerIt = project
    .in(file("scalus-cardano-ledger-it"))
    .dependsOn(
      scalusCardanoLedger.jvm % "compile->compile;test->test",
      `scalus-bloxbean-cardano-client-lib`,
      scalusExamples.jvm
    )
    .settings(
      name := "scalus-cardano-ledger-it",
      scalacOptions ++= commonScalacOptions,
      publish / skip := true,
      Test / fork := true,
      Test / testOptions += Tests.Argument("-oF"),
      // Forward SCALUS_TEST_ENV to forked test JVM
      Test / envVars ++= sys.env.get("SCALUS_TEST_ENV").map("SCALUS_TEST_ENV" -> _).toMap,
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion % "test",
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % cardanoClientLibVersion % "test",
      libraryDependencies += "com.bloxbean.cardano" % "yaci" % yaciVersion % "test",
      libraryDependencies += "com.bloxbean.cardano" % "yaci-cardano-test" % yaciCardanoTestVersion % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % scalatestVersion % "test",
      libraryDependencies += "org.slf4j" % "slf4j-simple" % slf4jVersion % "test",
      libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.4.3" % "test",
      libraryDependencies += "com.lihaoyi" %% "requests" % "0.9.3" % "test",
      libraryDependencies += "org.bouncycastle" % "bcprov-jdk18on" % "1.85.2" % "test",
      libraryDependencies += "foundation.icon" % "blst-java" % "0.3.2",
      libraryDependencies += "org.scalus" % "scalus-secp256k1-jni" % "0.6.0",
      libraryDependencies += "com.lihaoyi" %%% "pprint" % pprintVersion % "test",
      // Testcontainers for Yaci DevKit integration tests
      libraryDependencies += "com.dimafeng" %% "testcontainers-scala-core" % "0.44.1" % "test",
      libraryDependencies += "com.dimafeng" %% "testcontainers-scala-scalatest" % "0.44.1" % "test",
      inConfig(Test)(PluginDependency)
    )

// =============================================================================
// UTILS
// =============================================================================

def copyFiles(files: Seq[String], baseDir: File, targetDir: File, log: ManagedLogger): Unit = {
    files.foreach { file =>
        val source = baseDir / file
        val target = targetDir / file
        if (source.exists) {
            if (!target.exists) {
                IO.copyFile(source, target)
            } else if (source.lastModified() > target.lastModified()) {
                IO.copyFile(source, target)
            }
        } else {
            log.error(s"Shared file $file does not exist in $baseDir")
        }
    }
}

// =============================================================================
// COMMAND ALIASES
// =============================================================================

// ABI compatibility gate for the stable surface: scalus-core, scalus-cardano-ledger and
// scalus-bloxbean-cardano-client-lib, checked against scalusCompatibleVersion.
// Both cross-projects declare mimaPreviousArtifacts with %%%, so their Scala.js artifacts are
// part of that stable surface as well, and JS-only code (scalus.cardano.node.JEmulator) can
// break on its own. The JS variants are therefore checked here too. That makes ci-jvm compile
// the Scala.js classes; there is no linking and no Node run involved.
addCommandAlias(
  "mima",
  "scalusJVM/mimaReportBinaryIssues;" +
      "scalusJS/mimaReportBinaryIssues;" +
      "scalusCardanoLedgerJVM/mimaReportBinaryIssues;" +
      "scalusCardanoLedgerJS/mimaReportBinaryIssues;" +
      "scalus-bloxbean-cardano-client-lib/mimaReportBinaryIssues"
)
addCommandAlias(
  "quick",
  "scalafmtAll;scalafmtSbt;jvm/Test/compile;scalusCardanoLedgerIt/Test/compile;jvm/testQuick"
)
addCommandAlias(
  "cleanpile",
  "clean;jvm/Test/compile;scalusCardanoLedgerIt/Test/compile"
)
addCommandAlias(
  "precommit",
  "clean;docs/clean;scalafmtAll;scalafmtSbt;jvm/Test/compile;scalusCardanoLedgerIt/Test/compile;jvm/test"
)
addCommandAlias(
  "ci",
  "clean;docs/clean;scalafmtCheckAll;scalafmtSbtCheck;Test/compile;scalusCardanoLedgerIt/Test/compile;Test/nativeLink;test;mima"
)
addCommandAlias(
  "ci-jvm",
  // Full build/test on the default LTS. Includes format/mima checks (version-independent, so they
  // run only here). Runs in parallel with `ci-jvm-next` as separate CI-JVM matrix jobs.
  "clean;docs/clean;scalafmtCheckAll;scalafmtSbtCheck;jvm/Test/compile;scalusCardanoLedgerIt/Test/compile;jvm/test;mima"
)
addCommandAlias(
  "ci-jvm-lts-prev",
  // Previous-LTS (3.3.7) canary: prove the plugin builds against the 3.3.7 compiler and still emits
  // correct contracts, via the scalus.compiler.* compile-and-evaluate suite. Cheaper than a full
  // re-test – 3.3.7 and 3.3.8 share the `pre38` desugaring generation (verified byte-identical).
  "++3.3.7;clean;scalusPlugin/Test/compile;scalusJVM/testOnly scalus.compiler.*"
)
addCommandAlias(
  "ci-jvm-next",
  // Cross-build/test on Scala 3.8.4 (scala3NextVersion). Requires JDK 17+ – the `ci` nix devshell
  // pins JDK 21. We must NOT use the `jvm` aggregate here: modules that don't list 3.8.4
  // (scalusUplcJitCompiler, scalusUtxoCell, bench) fall back to the LTS and then fail to read the
  // 3.8.4 TASTy of scalus-core they depend on. So target only the modules that list 3.8.4 and whose
  // dependency closure is entirely 3.8.4-capable.
  "++3.8.4;clean;scalusPlugin/Test/compile;scalusJVM/Test/compile;scalusCardanoLedgerJVM/Test/compile;" +
      "scalusTestkitJVM/Test/compile;scalusExamplesJVM/Test/compile;scalusDesignPatterns/Test/compile;" +
      "scalus-bloxbean-cardano-client-lib/Test/compile;scalusEthereumKzgCeremony/Test/compile;" +
      "scalusJVM/test;scalusExamplesJVM/test"
)
addCommandAlias(
  "ci-js",
  "clean;js/Test/compile;js/test;scalusTsExporter/test;scalusCardanoLedgerJS/checkDtsUpToDate;scalusCardanoLedgerJS/runNpmTests"
)
addCommandAlias(
  "ci-native",
  "clean;native/Test/compile;native/test"
)
addCommandAlias("benchmark", "bench/Jmh/run -i 1 -wi 1 -f 1 -t 1 .*")
addCommandAlias(
  "benchmark-jit",
  "bench/Jmh/run -i 5 -wi 4 -f 1 -t 1 -rff last-bench-result.txt  .*(JIT|Cek).*"
)
addCommandAlias(
  "benchmark-cek",
  "bench/Jmh/run -i 3 -wi 3 -f 1 -t 1 .*CekJVMBenchmark"
)
addCommandAlias(
  "benchmark-hybrid",
  "bench/Jmh/run -i 3 -wi 3 -f 1 -t 1 .*JITHybridBenchmark"
)
addCommandAlias(
  "it",
  "clean;scalusCardanoLedgerIt/clean;scalusCardanoLedgerIt/Test/compile;scalusCardanoLedgerIt/test"
)

// =============================================================================
// WELCOME LOGO AND USEFUL TASKS
// =============================================================================

logo :=
    s"""
     |  ${scala.Console.RED}███████╗ ██████╗ █████╗ ██╗     ██╗   ██╗███████╗
     |  ${scala.Console.RED}██╔════╝██╔════╝██╔══██╗██║     ██║   ██║██╔════╝
     |  ${scala.Console.RED}███████╗██║     ███████║██║     ██║   ██║███████╗
     |  ${scala.Console.RED}╚════██║██║     ██╔══██║██║     ██║   ██║╚════██║
     |  ${scala.Console.RED}███████║╚██████╗██║  ██║███████╗╚██████╔╝███████║
     |  ${scala.Console.RED}╚══════╝ ╚═════╝╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚══════╝
     |
     |  Version: ${version.value} ${scala.Console.YELLOW}Scala ${scalaVersion.value}${scala.Console.RESET}
     |
     |""".stripMargin

usefulTasks := Seq(
  UsefulTask("~compile", "Compile with file-watch enabled"),
  UsefulTask("quick", "Format all, compile and quick test everything on JVM"),
  UsefulTask("precommit", "Format all, clean compile and test everything on JVM"),
  UsefulTask("ci", "Clean compile, check formatting and test everything, build docs, run MiMa"),
  UsefulTask("benchmark", "Run benchmarks"),
  UsefulTask("mima", "Check binary compatibility with the previous version using MiMa"),
)
