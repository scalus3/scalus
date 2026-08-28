package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

class GoldenTest extends AnyFunSuite {

    private def goldenDir =
        Paths.get(InspectorFixture.sourceRoot, "scalus-ts-exporter/src/test/resources/golden")

    test("fixtures generate the committed golden d.ts") {
        val cfg = Main.Config(
          tastyRoots = List(InspectorFixture.fixtureClasses),
          classpath = InspectorFixture.fixtureClasspath,
          output = "unused",
          sourceRoot = InspectorFixture.sourceRoot,
          excludes = List("tsfixtures.Bad")
        )
        Main.run(cfg) match
            case Left(errs) => fail(errs.map(_.render).mkString("\n"))
            case Right(text) =>
                val golden = goldenDir.resolve("fixtures.d.ts")
                assume(
                  Files.exists(golden),
                  s"golden file missing: $golden (seed it from this output)"
                )
                val expected = Files.readString(golden)
                if text != expected then {
                    // dump the actual output so an intended change can be copied over the golden
                    val actual = Paths
                        .get(InspectorFixture.sourceRoot, "scalus-ts-exporter/target")
                        .resolve("fixtures.d.ts.actual")
                    Files.createDirectories(actual.getParent)
                    Files.writeString(actual, text)
                    fail(
                      s"generated output differs from golden; if intended, copy $actual over $golden"
                    )
                }
    }

    test("golden d.ts + consumer.ts type-check with tsc") {
        val root = Paths.get(InspectorFixture.sourceRoot)
        val tsc = root.resolve("node_modules/.bin/tsc")
        assume(Files.exists(tsc), "tsc not installed; run npm install at the repo root")
        val cmd = List(
          tsc.toString,
          "--noEmit",
          "--strict",
          "--target",
          "es2020",
          "--moduleResolution",
          "bundler",
          "--module",
          "esnext",
          goldenDir.resolve("fixtures.d.ts").toString,
          goldenDir.resolve("consumer.ts").toString
        )
        val proc = new ProcessBuilder(cmd*).inheritIO().start()
        assert(proc.waitFor() == 0, "tsc found type errors in golden output")
    }

    test("CLI arg parsing") {
        assert(Main.parse(List("--output", "x")) == Left("at least one --tasty-root is required"))
        assert(Main.parse(List("--tasty-root", "a")) == Left("--output is required"))
        assert(Main.parse(List("--bogus")) == Left("unknown argument: --bogus"))
        val cfg = Main
            .parse(
              List(
                "--tasty-root",
                "a",
                "--tasty-root",
                "b",
                "--classpath",
                s"x${java.io.File.pathSeparator}y",
                "--output",
                "out.d.ts",
                "--source-root",
                "/root",
                "--exclude",
                "p1",
                "--exclude",
                "p2"
              )
            )
            .toOption
            .get
        assert(cfg.tastyRoots == List("a", "b"))
        assert(cfg.classpath == List("x", "y"))
        assert(cfg.output == "out.d.ts")
        assert(cfg.sourceRoot == "/root")
        assert(cfg.excludes == List("p1", "p2"))
    }
}
