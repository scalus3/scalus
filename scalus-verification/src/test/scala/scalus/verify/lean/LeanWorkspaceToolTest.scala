package scalus.verify.lean

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, LeanProofs, UplcBlaster}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The workspace a project's proofs get from [[LeanWorkspaceTool]]. All but the last test write
  * files and run no Lean.
  */
class LeanWorkspaceToolTest extends AnyFunSuite with LeanProofs {

    /** Runs `body` with a directory of its own, which is removed after. It has `.git`, as the root
      * of a project has, so what is not the project's own goes into its `.scalus`, and into no
      * directory above it.
      */
    private def inDirectory[A](body: Path => A): A = {
        val root = Files.createTempDirectory("scalus-lean-workspace")
        try
            Files.createDirectory(root.resolve(".git"))
            body(root)
        finally Directories.remove(root)
    }

    private def library(root: Path): Path = root.resolve(".scalus").resolve("ScalusProofs")

    private def text(file: Path): String = Files.readString(file, StandardCharsets.UTF_8)

    private val sources = LeanProofs.librarySources

    test("a workspace is created, with Scalus's Lean library in `.scalus` of the project") {
        inDirectory { root =>
            val workspace = root.resolve("Htlc")
            val outcome = LeanWorkspaceTool.init(workspace)
            assert(outcome.created)
            assert(outcome.notes.isEmpty)

            val lakefile = text(workspace.resolve("lakefile.lean"))
            assert(lakefile.contains("package «Htlc» where"))
            assert(lakefile.contains("lean_lib «Htlc» where"))
            assert(lakefile.contains("globs := #[.andSubmodules `Htlc]"))
            // The directory those modules are in: Lake does not build without it.
            assert(Files.isDirectory(workspace.resolve("Htlc")))
            assert(lakefile.contains("""require ScalusProofs from "../.scalus/ScalusProofs""""))
            assert(lakefile.contains("""packagesDir := "../.scalus/ScalusProofs/.lake/packages""""))
            assert(text(workspace.resolve("Htlc.lean")).startsWith("import ScalusProofs.Run\n"))
            assert(text(workspace.resolve(".gitignore")) == "/.lake\n")
            // Lake writes the manifest, with the revisions the library's manifest pins.
            assert(!Files.exists(workspace.resolve("lake-manifest.json")))

            // The library is the one in Scalus's sources, file by file, so a check in the
            // workspace rests on the same Lean as one in the sources.
            val unpacked = library(root)
            val files = List(
              "lakefile.lean",
              "lean-toolchain",
              "lake-manifest.json",
              "ScalusProofs.lean",
              "ScalusProofs/Run.lean"
            )
            files.foreach(file =>
                assert(text(unpacked.resolve(file)) == text(sources.resolve(file)))
            )
            assert(LeanWorkspace.toolchain(workspace) == LeanWorkspace.toolchain(sources))
            assert(
              LeanWorkspace.environment(unpacked)("ScalusProofs") ==
                  LeanWorkspace.environment(sources)("ScalusProofs")
            )
            assert(text(root.resolve(".scalus").resolve(".gitignore")) == "*\n")
            assert(outcome.written.toSet.contains(workspace.resolve("lakefile.lean")))
            assert(outcome.written.toSet.contains(unpacked.resolve("ScalusProofs/Run.lean")))
        }
    }

    test("a second call writes nothing") {
        inDirectory { root =>
            LeanWorkspaceTool.init(root.resolve("Htlc"))
            val again = LeanWorkspaceTool.init(root.resolve("Htlc"))
            assert(!again.created)
            assert(again.written.isEmpty)
            assert(again.notes.isEmpty)
        }
    }

    test("the workspaces of a project have one library, in its `.scalus`") {
        inDirectory { root =>
            val htlc = root.resolve("contracts/src/test/lean/Htlc")
            val vesting = root.resolve("offchain/lean/Vesting")
            val first = LeanWorkspaceTool.init(htlc)
            val second = LeanWorkspaceTool.init(vesting)

            assert(Files.isRegularFile(library(root).resolve("ScalusProofs/Run.lean")))
            assert(!Files.exists(htlc.getParent.resolve(".scalus")))
            assert(!Files.exists(vesting.getParent.resolve(".scalus")))
            assert(
              text(htlc.resolve("lakefile.lean")).contains(
                """require ScalusProofs from "../../../../../.scalus/ScalusProofs""""
              )
            )
            assert(
              text(vesting.resolve("lakefile.lean")).contains(
                """packagesDir := "../../../.scalus/ScalusProofs/.lake/packages""""
              )
            )
            // The second finds the library there, and writes its own files only.
            assert(first.written.exists(_.startsWith(library(root))))
            assert(second.written.forall(_.startsWith(vesting)))
            assert(second.created && second.notes.isEmpty)
        }
    }

    test("a library that a workspace requires from outside a `.scalus` is not written") {
        inDirectory { root =>
            // As a workspace in Scalus's own sources, which requires the library from them.
            val elsewhere = root.resolve("lib/ScalusProofs")
            List("lean-toolchain", "lake-manifest.json", "ScalusProofs/Run.lean").foreach { file =>
                Files.createDirectories(elsewhere.resolve(file).getParent)
                Files.copy(sources.resolve(file), elsewhere.resolve(file))
            }
            val run = elsewhere.resolve("ScalusProofs/Run.lean")
            Files.writeString(run, "-- the sources as they are now\n")
            val workspace = root.resolve("Example")
            Files.createDirectories(workspace)
            Files.writeString(
              workspace.resolve("lakefile.lean"),
              "package «Example» where\nrequire ScalusProofs from \"../lib/ScalusProofs\"\n"
            )
            Files.copy(sources.resolve("lean-toolchain"), workspace.resolve("lean-toolchain"))

            val outcome = LeanWorkspaceTool.init(workspace)
            assert(!outcome.created)
            assert(outcome.written.isEmpty)
            assert(outcome.notes.isEmpty)
            assert(text(run) == "-- the sources as they are now\n")
            assert(!Files.exists(root.resolve(".scalus")))
            assert(!Files.exists(elsewhere.getParent.resolve(".gitignore")))
        }
    }

    test("a workspace whose lakefile requires the library by no path is said so") {
        inDirectory { root =>
            val workspace = root.resolve("Example")
            Files.createDirectories(workspace)
            Files.writeString(
              workspace.resolve("lakefile.lean"),
              "package «Example» where\nrequire ScalusProofs from git \"https://example.org/x\"\n"
            )
            val outcome = LeanWorkspaceTool.init(workspace)
            assert(!outcome.created)
            assert(outcome.written.isEmpty)
            assert(outcome.notes.exists(_.contains("does not require ScalusProofs from a path")))
            assert(!Files.exists(root.resolve(".scalus")))
        }
    }

    test("a workspace that is there is left as it is, and what disagrees is said") {
        inDirectory { root =>
            val workspace = root.resolve("Htlc")
            LeanWorkspaceTool.init(workspace)
            val lakefile = workspace.resolve("lakefile.lean")
            val changed = text(lakefile) + "\n-- the project's own\n"
            Files.writeString(lakefile, changed)
            Files.writeString(workspace.resolve("lean-toolchain"), "leanprover/lean4:v4.0.0\n")
            val manifest = text(sources.resolve("lake-manifest.json"))
            val revision = LeanWorkspace.pinned(sources)("Blaster")
            Files.writeString(
              workspace.resolve("lake-manifest.json"),
              manifest.replace(revision, "0" * revision.length)
            )

            val outcome = LeanWorkspaceTool.init(workspace)
            assert(!outcome.created)
            assert(outcome.written.isEmpty)
            assert(text(lakefile) == changed)
            assert(text(workspace.resolve("lean-toolchain")) == "leanprover/lean4:v4.0.0\n")
            assert(outcome.noteLines.asScala.toList == outcome.notes)
            assert(outcome.doneLines.asScala.toList == outcome.done)
            assert(outcome.done.forall(line => !outcome.notes.exists(line.contains)))
            outcome.notes match
                case List(lean, blaster) =>
                    assert(lean.contains("leanprover/lean4:v4.0.0"), lean)
                    assert(lean.contains(LeanWorkspace.toolchain(sources)), lean)
                    assert(blaster.contains("Blaster") && blaster.contains("lake update"), blaster)
                case other => fail(s"expected a note on Lean and one on Blaster, got $other")
        }
    }

    test("the library beside a workspace is brought up to date") {
        inDirectory { root =>
            val workspace = root.resolve("Htlc")
            LeanWorkspaceTool.init(workspace)
            val run = library(root).resolve("ScalusProofs/Run.lean")
            val gone = library(root).resolve("ScalusProofs/Gone.lean")
            val built = library(root).resolve(".lake/build/kept")
            Files.writeString(run, "-- of another version\n")
            Files.writeString(gone, "-- a module the library no longer has\n")
            Files.createDirectories(built.getParent)
            Files.writeString(built, "what Lake built")

            val outcome = LeanWorkspaceTool.init(workspace)
            assert(outcome.written == List(run))
            assert(text(run) == text(sources.resolve("ScalusProofs/Run.lean")))
            assert(!Files.exists(gone))
            assert(Files.exists(built))
        }
    }

    test("a directory whose name Lean cannot import, or has a library of, is no workspace") {
        inDirectory { root =>
            val dashed = intercept[IllegalArgumentException] {
                LeanWorkspaceTool.init(root.resolve("my-proofs"))
            }
            assert(dashed.getMessage.contains("my-proofs"))
            // A package of Scalus's Lean library, a library of Lean itself, and one that differs
            // from such a name in the case of its letters only.
            List("Blaster", "ScalusProofs", "Lean", "Std", "plutuscore").foreach { name =>
                val taken = intercept[IllegalArgumentException] {
                    LeanWorkspaceTool.init(root.resolve(name))
                }
                assert(taken.getMessage.contains(name))
            }
            // Nothing is written for a name that is refused, the library neither.
            val there = Files.list(root)
            try assert(there.iterator().asScala.toList == List(root.resolve(".git")))
            finally there.close()
        }
    }

    test("a workspace created over files that were there says what disagrees") {
        inDirectory { root =>
            val workspace = root.resolve("Htlc")
            Files.createDirectories(workspace)
            Files.writeString(workspace.resolve("lean-toolchain"), "leanprover/lean4:v4.0.0\n")

            val outcome = LeanWorkspaceTool.init(workspace)
            assert(outcome.created)
            assert(Files.exists(workspace.resolve("lakefile.lean")))
            assert(text(workspace.resolve("lean-toolchain")) == "leanprover/lean4:v4.0.0\n")
            assert(!outcome.written.contains(workspace.resolve("lean-toolchain")))
            outcome.notes match
                case List(lean) => assert(lean.contains("leanprover/lean4:v4.0.0"), lean)
                case other      => fail(s"expected a note on Lean, got $other")
        }
    }

    test("a suite's workspace is made ready, and one that disagrees fails the test") {
        inDirectory { root =>
            val workspace = readyWorkspace(root.resolve("Htlc"))
            assert(workspace == root.resolve("Htlc").toAbsolutePath.normalize)
            assert(Files.isRegularFile(library(root).resolve("ScalusProofs/Run.lean")))

            Files.writeString(workspace.resolve("lean-toolchain"), "leanprover/lean4:v4.0.0\n")
            val failed = intercept[org.scalatest.exceptions.TestFailedException] {
                readyWorkspace(workspace)
            }
            assert(failed.getMessage.contains("does not agree with Scalus's Lean library"))
            assert(failed.getMessage.contains("leanprover/lean4:v4.0.0"))
        }
    }

    test("a created workspace builds with Lake, and a statement is proved in it") {
        requireLean()
        inDirectory { root =>
            // Below the project's root, so its lakefile reaches the library through `..`.
            val workspace = readyWorkspace(root.resolve("contracts/src/test/lean/Created"))
            // What Lake cloned and built for the library, in the workspace `requireLean` found
            // built, serves the library unpacked here, whose sources are the same: only the
            // workspace's own modules are built.
            Files.createSymbolicLink(
              library(root).resolve(".lake"),
              leanWorkspace.resolve(".lake").toRealPath()
            )
            def lakeBuild(): String = {
                val build = new ProcessBuilder("lake", "build")
                    .directory(workspace.toFile)
                    .redirectErrorStream(true)
                    .start()
                val output = new String(build.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
                assert(build.waitFor() == 0, output)
                output
            }
            // As it was created, without a module of the project's own.
            lakeBuild()
            // With a proof written by hand, which nothing imports.
            val proof = workspace.resolve("Created").resolve("Proof.lean")
            Files.writeString(proof, "theorem one_is_one : (1 : Nat) = 1 := rfl\n")
            val output = lakeBuild()
            assert(LeanProofs.isBuilt(workspace))
            assert(
              Files.isRegularFile(workspace.resolve(".lake/build/lib/lean/Created/Proof.olean")),
              output
            )
            assert(LeanWorkspace.pinned(workspace) == LeanWorkspace.pinned(sources))
            assert(LeanWorkspaceTool.init(workspace).notes.isEmpty)

            val servers = LeanServers.in(workspace)
            try
                val verifier = Verifier.empty
                val statement = verifier.statement(forAll[BigInt](x => x + BigInt(0) == x))
                verifier.verify(statement, UplcBlaster(Budget.LeanSteps(40), servers)) match
                    case VerificationResult.Proven(_) =>
                    case other                        => fail(s"expected a proof, got $other")
            finally servers.close()
        }
    }
}
