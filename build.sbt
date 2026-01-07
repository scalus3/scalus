import com.typesafe.tools.mima.core.{IncompatibleMethTypeProblem, IncompatibleResultTypeProblem, MissingClassProblem, ProblemFilters}
import sbt.internal.util.ManagedLogger
import sbtwelcome.*

import java.net.URI
import scala.scalanative.build.*

// =============================================================================
// GLOBAL SETTINGS
// =============================================================================

Global / onChangedBuildSource := ReloadOnSourceChanges
autoCompilerPlugins := true

val scalusStableVersion = "0.14.0"
val scalusCompatibleVersion = scalusStableVersion

// Bloxbean Cardano Client Library versions
val cardanoClientLibVersion = "0.7.1"
val yaciVersion = "0.3.8"
val yaciCardanoTestVersion = "0.1.0"

//ThisBuild / scalaVersion := "3.8.0-RC1-bin-SNAPSHOT"
//ThisBuild / scalaVersion := "3.3.7-RC1-bin-SNAPSHOT"
//ThisBuild / scalaVersion := "3.7.3-RC1-bin-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.7"
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

// Scalus Compiler Plugin Dependency
lazy val PluginDependency: List[Def.Setting[?]] = List(scalacOptions ++= {
    val jar = (scalusPlugin / Compile / packageBin).value
    // add plugin timestamp to compiler options to trigger recompile of
    // main after editing the plugin. (Otherwise a 'clean' is needed.)

    // NOTE: uncomment for faster Scalus Plugin development
    // this will recompile the plugin when the jar is modified
    //    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
    Seq(s"-Xplugin:${jar.getAbsolutePath}")
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
      scalusCardanoLedger.jvm,
      scalusCardanoLedger.js,
      scalusTestkit.js,
      scalusTestkit.jvm,
      scalusExamples.js,
      scalusExamples.jvm,
      scalusDesignPatterns,
      bench,
      `scalus-bloxbean-cardano-client-lib`,
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
      scalusPluginTests,
      scalusUplcJitCompiler,
      scalusCardanoLedger.jvm,
      scalusTestkit.jvm,
      scalusExamples.jvm,
      scalusDesignPatterns,
      bench,
      `scalus-bloxbean-cardano-client-lib`,
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
      scalusExamples.js,
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
      scalacOptions ++= commonScalacOptions,
//      scalacOptions += "-Wunused:all",
      // Manually set a fixed version to avoid recompilation on every commit
      // as sbt-ci-release plugin increments the version on every commit
      // thus recompiling the plugin and all dependent projects
      // COMMENT THIS LINE TO ENABLE VERSION INCREMENT during Scalus plugin development
      // COMMENT THIS LINE when doing plugin development
      // UPDATE VERSION after changes to the plugin
      // version := "0.13.0+597-4eafe96f+20251217-1256-SNAPSHOT",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test",
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
            "scalus/builtin/BuiltinList.scala",
            "scalus/builtin/BuiltinValue.scala",
            "scalus/builtin/ByteStringFlatInstance.scala",
            "scalus/builtin/Data.scala",
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
            "scalus/serialization/flat/package.scala",
            "scalus/serialization/flat/FlatInstances.scala",
            "scalus/serialization/flat/HashConsed.scala",
            "scalus/serialization/flat/HashConsedFlat.scala",
            "scalus/uplc/Constant.scala",
            "scalus/uplc/DefaultFun.scala",
            "scalus/uplc/DefaultUni.scala",
            "scalus/uplc/TypeScheme.scala",
            "scalus/utils/Hex.scala",
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
      cleanFiles += (Compile / sourceDirectory).value / "shared",
      // Ensure shared files are copied before any source inspection
      Compile / sourceGenerators += Def.task {
          copySharedFiles.value
          Seq.empty[File]
      }.taskValue,
      Compile / compile := (Compile / compile).dependsOn(copySharedFiles).value
    )

// Used only for Scalus compiler plugin development
// I use it to not recompile all the tests in the main project
// TODO remove or comment out
lazy val scalusPluginTests = project
    .in(file("scalus-plugin-tests"))
    .dependsOn(scalus.jvm)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      name := "scalus-plugin-tests",
      publish / skip := true,
      PluginDependency,
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test"
    )

// Scalus Core and Standard Library for JVM and JS
lazy val scalus = crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("scalus-core"))
    .settings(
      name := "scalus",
      scalaVersion := scalaVersion.value,
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

      // enable when debug compilation of tests
      Test / scalacOptions += "-color:never",
      PluginDependency,
      libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0",
      libraryDependencies += "org.typelevel" %%% "cats-parse" % "1.1.0",
      libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.4.4",
      libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.4.1",
      libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.38.6",
      libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.38.6" % "compile",
      libraryDependencies ++= Seq(
        "io.bullet" %%% "borer-core" % "1.16.2",
        "io.bullet" %%% "borer-derivation" % "1.16.2"
      ),
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.18" % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test",
      libraryDependencies ++= Seq(
        "dev.optics" %%% "monocle-core" % "3.3.0",
        "dev.optics" %%% "monocle-macro" % "3.3.0",
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
      Test / fork := true,
      // Run forked tests from project root so paths are consistent across platforms
      Test / baseDirectory := (LocalRootProject / baseDirectory).value,
      // Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-S", "-8077211454138081902"),
      Test / testOptions += Tests.Argument("-oF"),
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17" % "provided",
      libraryDependencies += "org.bouncycastle" % "bcprov-jdk18on" % "1.83",
      libraryDependencies += "foundation.icon" % "blst-java" % "0.3.2",
      libraryDependencies += "org.scalus" % "scalus-secp256k1-jni" % "0.6.0"
    )
    .jsSettings(
      // Add JS-specific settings here
      Compile / npmDependencies += "@noble/curves" -> "1.9.1",
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
          // Use .mjs extension.
//              .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
      },
      scalaJSUseMainModuleInitializer := false
    )
    .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }
    .nativeSettings(
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
    .dependsOn(scalus.jvm % "compile->compile")
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
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      inConfig(Test)(PluginDependency),
      publish / skip := true
    )

// Scalus Testkit library for testing Scalus applications
lazy val scalusTestkit = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-testkit"))
    .dependsOn(scalus, scalusCardanoLedger)
    .settings(
      name := "scalus-testkit",
      scalaVersion := scalaVersion.value,
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
          val sharedFiles = Seq(
            "scalus/testing/ArbitraryDerivation.scala",
            "scalus/uplc/test/ArbitraryInstances.scala",
            "scalus/ledger/api/v1/ArbitraryInstances.scala",
            "scalus/ledger/api/v2/ArbitraryInstances.scala",
            "scalus/ledger/api/v3/ArbitraryInstances.scala",
            "scalus/cardano/address/ArbitraryInstances.scala",
            "scalus/cardano/ledger/ArbitraryInstances.scala",
            "scalus/prelude/StdlibTestKit.scala",
            "scalus/testing/kit/TestKit.scala"
          )
          val baseDir =
              (scalus.jvm / crossProjectBaseDirectory).value / "shared" / "src" / "test" / "scala"
          val targetDir = (Compile / sourceManaged).value
          val log = streams.value.log
          sharedFiles.flatMap { file =>
              val source = baseDir / file
              val target = targetDir / file
              if (source.exists) {
                  IO.copyFile(source, target)
                  Some(target)
              } else {
                  log.error(s"Shared file $file does not exist in $baseDir")
                  None
              }
          }
      }.taskValue,
      PluginDependency,
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.18",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19",
    )
    .jvmSettings(
      // Copy Party.scala (JVM-only due to Bloxbean dependencies)
      Compile / sourceGenerators += Def.task {
          val source =
              (scalusCardanoLedger.jvm / Test / sourceDirectory).value / "scala" / "scalus" / "testing" / "kit" / "Party.scala"
          val target =
              (Compile / sourceManaged).value / "scalus" / "testing" / "kit" / "Party.scala"
          if (source.exists) {
              IO.copyFile(source, target)
              Seq(target)
          } else {
              streams.value.log.error(s"Party.scala does not exist at $source")
              Seq.empty
          }
      }.taskValue,
      // Add Yaci DevKit dependencies for integration testing
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion,
      libraryDependencies += "com.bloxbean.cardano" % "yaci-cardano-test" % yaciCardanoTestVersion,
      libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.13",
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17" % Test
    )
    .jsSettings(
      Compile / npmDependencies += "@noble/curves" -> "1.9.1",
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
      },
      scalaJSUseMainModuleInitializer := false
    )
    .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }

lazy val scalusExamples = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-examples"))
    .dependsOn(scalus, scalusTestkit)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      PluginDependency,
      scalacOptions ++= commonScalacOptions,
      publish / skip := true,
      libraryDependencies += "io.bullet" %%% "borer-derivation" % "1.16.2",
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.18" % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test",
      // Exclude integration tests from default test runs (require external services)
      Test / testOptions += Tests.Argument("-l", "scalus.testing.IntegrationTest")
    )
    .configurePlatform(JVMPlatform)(_.dependsOn(`scalus-bloxbean-cardano-client-lib`))
    .jvmSettings(
      Test / fork := true,
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % cardanoClientLibVersion
    )
    .jsSettings(
      Compile / npmDependencies += "@noble/curves" -> "1.9.1",
      Test / envVars := sys.env.toMap, // for HTLC integration tests
      scalaJSUseMainModuleInitializer := false,
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
      }
    )
    .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }

lazy val scalusDesignPatterns = project
    .in(file("scalus-design-patterns"))
    .dependsOn(scalus.jvm, scalusTestkit.jvm)
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      PluginDependency,
      scalacOptions ++= commonScalacOptions,
      publish / skip := true,
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test",
      Test / fork := true
      //// enable if need speedup
      // trackInternalDependencies := TrackLevel.TrackIfMissing,
    )

// Bloxbean Cardano Client Lib integration and Tx Evaluator implementation
lazy val `scalus-bloxbean-cardano-client-lib` = project
    .in(file("bloxbean-cardano-client-lib"))
    .dependsOn(scalus.jvm, scalusCardanoLedger.jvm)
    .settings(
      publish / skip := false,
      scalacOptions ++= commonScalacOptions,
      mimaPreviousArtifacts := Set(organization.value %% name.value % scalusCompatibleVersion),
      mimaBinaryIssueFilters ++= Seq.empty,
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion,
      libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.17",
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17" % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % cardanoClientLibVersion % "test",
      libraryDependencies += "com.bloxbean.cardano" % "yaci" % yaciVersion % "test",
      libraryDependencies += "io.bullet" %%% "borer-derivation" % "1.16.2",
      libraryDependencies += "com.bloxbean.cardano" % "yaci-cardano-test" % yaciCardanoTestVersion % "test",
      libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.9.6" % "test",
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
        `scalus-bloxbean-cardano-client-lib`,
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
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17",
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion,
      libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.20.1",
      libraryDependencies += "io.bullet" %%% "borer-core" % "1.16.2",
      libraryDependencies += "io.bullet" %%% "borer-derivation" % "1.16.2"
    )

// Cardano Ledger domain model and CBOR serialization
lazy val scalusCardanoLedger = crossProject(JSPlatform, JVMPlatform)
    .in(file("scalus-cardano-ledger"))
    .dependsOn(scalus % "compile->compile;test->test")
    .disablePlugins(MimaPlugin) // disable Migration Manager for Scala
    .settings(
      name := "scalus-cardano-ledger",
      scalacOptions ++= commonScalacOptions,
      scalacOptions += "-Xmax-inlines:100", // needed for upickle derivation of CostModel
      libraryDependencies ++= Seq(
        "io.bullet" %%% "borer-core" % "1.16.2",
        "io.bullet" %%% "borer-derivation" % "1.16.2"
      ),
      // For tx builder
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion,
      libraryDependencies += "com.outr" %%% "scribe" % "3.17.0", // logging
      libraryDependencies ++= Seq(
        "dev.optics" %%% "monocle-core" % "3.3.0",
        "dev.optics" %%% "monocle-macro" % "3.3.0",
      ),
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion % "test",
      libraryDependencies += "com.softwaremill.magnolia1_3" %%% "magnolia" % "1.3.18" % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % "test",
      libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.9.6" % "test",
      libraryDependencies += "com.softwaremill.sttp.client4" %%% "core" % "4.0.13",
      inConfig(Test)(PluginDependency),
      publish / skip := false
    )
    .jvmSettings(
      // For conformance test vector extraction
      libraryDependencies += "org.apache.commons" % "commons-compress" % "1.28.0" % "test"
    )
    .jsSettings(
      Compile / npmDependencies += "@noble/curves" -> "1.9.1",
      // Lucid Evolution and CML for transaction signing
      Compile / npmDependencies += "@lucid-evolution/wallet" -> "0.1.72",
      Compile / npmDependencies += "@anastasia-labs/cardano-multiplatform-lib-nodejs" -> "6.0.2-3",
      // copy scalus.js and scalus.js.map to npm directory for publishing
      prepareNpmPackage := {
          val bundle = (Compile / fullOptJS / webpack).value
          val npmDir = (Compile / sourceDirectory).value / "npm"
          val log = streams.value.log
          bundle.foreach { f =>
              val sourceJs = f.data.file
              val targetJs = npmDir / "scalus.js"
              IO.copyFile(sourceJs, targetJs)
              log.info(s"Copied ${sourceJs} to ${targetJs}")
              // Also copy source map if it exists
              val sourceMap = new File(sourceJs.getParentFile, sourceJs.getName + ".map")
              if (sourceMap.exists()) {
                  val targetMap = npmDir / "scalus.js.map"
                  IO.copyFile(sourceMap, targetMap)
                  log.info(s"Copied ${sourceMap} to ${targetMap}")
              }
          }
      },
      runNpmTests := {
          import scala.sys.process._
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
      // use custom webpack config to export scalus as a commonjs2 module
      // otherwise it won't export the module correctly
      webpackConfigFile := Some(sourceDirectory.value / "main" / "webpack" / "webpack.config.js"),
      scalaJSUseMainModuleInitializer := false,
      scalaJSLinkerConfig ~= {
          _.withModuleKind(ModuleKind.CommonJSModule)
      }
    )
    .jsConfigure { project => project.enablePlugins(ScalaJSBundlerPlugin) }

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
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-lib" % cardanoClientLibVersion % "test",
      libraryDependencies += "com.bloxbean.cardano" % "cardano-client-backend-blockfrost" % cardanoClientLibVersion % "test",
      libraryDependencies += "com.bloxbean.cardano" % "yaci" % yaciVersion % "test",
      libraryDependencies += "com.bloxbean.cardano" % "yaci-cardano-test" % yaciCardanoTestVersion % "test",
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17" % "test",
      libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.3.0" % "test",
      libraryDependencies += "com.lihaoyi" %% "requests" % "0.9.0" % "test",
      libraryDependencies += "org.bouncycastle" % "bcprov-jdk18on" % "1.83" % "test",
      libraryDependencies += "foundation.icon" % "blst-java" % "0.3.2",
      libraryDependencies += "org.scalus" % "scalus-secp256k1-jni" % "0.6.0",
      libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.9.6" % "test",
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

// We only check ABI compatibility for scalus-bloxbean-cardano-client-lib project for now
// because it's used by CCL and we want to avoid breaking changes
addCommandAlias(
  "mima",
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
  "clean;docs/clean;scalusPluginTests/clean;scalafmtAll;scalafmtSbt;jvm/Test/compile;scalusCardanoLedgerIt/Test/compile;scalusPluginTests/test;jvm/test"
)
addCommandAlias(
  "ci",
  "clean;docs/clean;scalusPluginTests/clean;scalafmtCheckAll;scalafmtSbtCheck;Test/compile;scalusCardanoLedgerIt/Test/compile;scalusPluginTests/Test/compile;Test/nativeLink;test;mima"
)
addCommandAlias(
  "ci-jvm",
  "clean;docs/clean;scalusPluginTests/clean;scalafmtCheckAll;scalafmtSbtCheck;jvm/Test/compile;scalusCardanoLedgerIt/Test/compile;scalusPluginTests/Test/compile;jvm/test;mima"
)
addCommandAlias(
  "ci-js",
  "clean;js/Test/compile;js/test;scalusCardanoLedgerJS/runNpmTests"
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
