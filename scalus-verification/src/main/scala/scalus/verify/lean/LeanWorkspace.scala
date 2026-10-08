package scalus.verify.lean

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalus.verify.Fingerprint

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** What a Lean workspace is made of, read from its files: no Lean runs for it. */
object LeanWorkspace {

    /** A package of a workspace's manifest: one that is cloned has a revision, and one that is
      * required by its path has a directory.
      */
    final case class Package(name: String, rev: Option[String], dir: Option[String])

    private final case class Manifest(name: String, packages: List[Package])
    private given JsonValueCodec[Manifest] = JsonCodecMaker.make

    private def manifest(workspace: Path): Option[Manifest] = {
        val file = workspace.resolve("lake-manifest.json")
        Option.when(Files.isRegularFile(file))(readFromArray[Manifest](Files.readAllBytes(file)))
    }

    /** The Lean that `workspace` pins. */
    def toolchain(workspace: Path): String =
        Files.readString(workspace.resolve("lean-toolchain")).trim

    /** The packages of the manifest of `workspace`: none where it has no manifest. */
    def packages(workspace: Path): List[Package] = manifest(workspace).fold(Nil)(_.packages)

    /** The revisions at which the manifest of `workspace` pins the packages it clones, by their
      * names.
      */
    def pinned(workspace: Path): Map[String, String] =
        packages(workspace).collect { case Package(name, Some(revision), _) =>
            name -> revision
        }.toMap

    /** The workspaces that `workspace` requires by their paths, by the names of their packages. */
    def required(workspace: Path): Map[String, Path] =
        packages(workspace).collect { case Package(name, _, Some(directory)) =>
            name -> workspace.resolve(directory).normalize
        }.toMap

    /** What the result of a check in `workspace` rests on, on Lean's side, by the names of its
      * parts: the toolchain, the revision of every package that is cloned, and a hash of the
      * sources of the workspace's own package and of every package it requires by its path.
      *
      * A check that is proved with one of them changed is another proof: a fix of Blaster, or of
      * Scalus's own Lean library, must not leave the proofs that were made before it.
      */
    def environment(workspace: Path): Map[String, String] = {
        val own = manifest(workspace).fold(workspace.getFileName.toString)(_.name)
        val sourced = required(workspace) + (own -> workspace)
        Map("toolchain" -> toolchain(workspace)) ++
            pinned(workspace).view.mapValues(_.take(12)) ++
            sourced.view.mapValues(sources)
    }

    /** A hash of the Lean sources of the package in `directory`: every `.lean` file that is not
      * under `.lake`, by its path in the package and its text.
      */
    private def sources(directory: Path): String = {
        val walked = Files.walk(directory)
        val files =
            try
                walked
                    .iterator()
                    .asScala
                    .filter { file =>
                        val relative = directory.relativize(file)
                        file.toString.endsWith(".lean") && Files.isRegularFile(file) &&
                        !relative.iterator().asScala.exists(_.toString == ".lake")
                    }
                    .toList
                    .sortBy(file => directory.relativize(file).toString)
            finally walked.close()
        val text = files.map { file =>
            s"${directory.relativize(file)}\n${Files.readString(file, StandardCharsets.UTF_8)}"
        }
        Fingerprint.hash(text.mkString("\n"))
    }
}
