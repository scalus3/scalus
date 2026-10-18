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

    /** The packages of a manifest that is given as its bytes. */
    private[lean] def packagesIn(manifest: Array[Byte]): List[Package] =
        readFromArray[Manifest](manifest).packages

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
        val read = manifest(workspace)
        val packages = read.fold(Nil)(_.packages)
        val own = read.fold(workspace.getFileName.toString)(_.name)
        val cloned = packages.collect { case Package(name, Some(revision), _) =>
            name -> revision.take(12)
        }
        val sourced = packages.collect { case Package(name, _, Some(directory)) =>
            name -> workspace.resolve(directory).normalize
        } :+ (own -> workspace)
        Map("toolchain" -> toolchain(workspace)) ++ cloned ++
            sourced.map((name, directory) => name -> sources(directory, name))
    }

    /** A hash of the Lean sources of the package `name` in `directory`: its `lakefile.lean`, and
      * the modules of the library of its name, `<name>.lean` and those under `<name>`, by their
      * paths in the package and their texts.
      *
      * Other Lean files of the directory are not the package's: a check that is kept there to be
      * run by hand, or a file someone tries something in, changes no result.
      */
    private def sources(directory: Path, name: String): String = {
        val modules = directory.resolve(name)
        val below =
            if Files.isDirectory(modules) then
                val walked = Files.walk(modules)
                try walked.iterator().asScala.filter(Files.isRegularFile(_)).toList
                finally walked.close()
            else Nil
        val files =
            (directory.resolve("lakefile.lean") :: directory.resolve(s"$name.lean") :: below)
                .filter(file => file.toString.endsWith(".lean") && Files.isRegularFile(file))
                .sortBy(file => directory.relativize(file).toString)
        val text = files.map { file =>
            s"${directory.relativize(file)}\n${Files.readString(file, StandardCharsets.UTF_8)}"
        }
        Fingerprint.hash(text.mkString("\n"))
    }
}
