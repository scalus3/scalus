package scalus.verify.lean

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Creates the Lean workspace of a project's proofs, and brings Scalus's Lean library to it. No
  * Lean runs for it, and no network is needed.
  *
  * A workspace is a Lake package in the project's sources, one for a contract or one for the
  * project. The checks of its statements run in it, and proofs written by hand belong there. What
  * is not the project's own is in `.scalus`, once for the project: Scalus's Lean library, as this
  * version of Scalus has it, and the packages the library requires, which Lake clones and builds
  * there for every workspace of the project.
  * {{{
  * .scalus/ScalusProofs/          Scalus's Lean library, unpacked from the jar; not committed
  * contracts/src/test/lean/Htlc/  a workspace: lakefile.lean, lean-toolchain, Htlc.lean, Htlc/
  * }}}
  * A workspace is created where its directory has no lakefile, and is the project's from then on:
  * nothing in it is written again. Its lakefile says where the library is, and the library there is
  * brought up to date on every call, as it is Scalus's.
  */
object LeanWorkspaceTool {

    /** The directory of a project that has what is not the project's own. */
    val sharedDirectory: String = ".scalus"

    /** The name of Scalus's Lean library, and of its directory in [[sharedDirectory]]. */
    val libraryName: String = "ScalusProofs"

    /** What [[init]] did for `workspace`: whether it created it, the files it wrote, there and of
      * the library, and where the workspace does not agree with the library. That is the project's
      * to settle.
      */
    final case class Outcome(
        workspace: Path,
        created: Boolean,
        written: List[Path],
        notes: List[String]
    ) {

        /** What was done, in lines, without the notes. */
        def done: List[String] = {
            val head =
                if created then s"Created the Lean workspace $workspace"
                else s"The Lean workspace $workspace is there, and is left as it is"
            val next =
                if created then
                    List(
                      "Build it with `lake build` there. The first build clones and builds what " +
                          "Scalus's Lean library requires, which takes minutes."
                    )
                else Nil
            head :: written.map(file => s"  wrote $file") ::: next
        }

        /** The outcome in lines, for a log. */
        def report: String = (done ::: notes.map(note => s"  note: $note")).mkString("\n")

        /** [[done]] and [[notes]] for a build tool, which reads them by reflection and says the
          * notes louder.
          */
        def doneLines: java.util.List[String] = done.asJava
        def noteLines: java.util.List[String] = notes.asJava
    }

    /** The libraries of Lean itself. Lean looks for a module in the workspace's own package first,
      * so a workspace of one of these names would stand in the way of that library.
      */
    private val leanLibraries = List("Init", "Std", "Lean", "Lake")

    /** Makes the workspace `workspace` ready: creates it where its directory has no lakefile, and
      * brings Scalus's Lean library up to date where the workspace's lakefile requires it from.
      *
      * A workspace that is created gets the name of its directory for its package, which has to be
      * a name Lean can import, and requires the library from `.scalus` of its project
      * ([[sharedDirectory]]): the nearest directory above it that has `.git`, or without one, the
      * directory the workspace is in. So the workspaces of a project have one library, and Lake
      * clones and builds its packages once.
      *
      * Only a library in a `.scalus` is written. A workspace that requires the library from another
      * path, as one in Scalus's own sources does, is only looked at.
      *
      * The suites of a build call this at the same time. A file is written at once, so a reader has
      * the old file or the new one, and two calls that write the same file leave it whole.
      */
    def init(workspace: Path): Outcome = synchronized {
        val directory = workspace.toAbsolutePath.normalize
        val name = Option(directory.getFileName).map(_.toString).getOrElse("")
        val parent = Option(directory.getParent).getOrElse(
          throw new IllegalArgumentException(
            s"a Lean workspace needs a directory above it: $directory"
          )
        )
        val lakefile = directory.resolve("lakefile.lean")
        val created = !Files.exists(lakefile) && !Files.exists(directory.resolve("lakefile.toml"))
        if created then
            checkName(name)
            val library = project(parent).resolve(sharedDirectory).resolve(libraryName)
            val unpacked = unpack(library)
            val path = directory.relativize(library).toString.replace('\\', '/')
            val written = List(
              lakefile -> lakefileText(name, path),
              directory.resolve("lean-toolchain") -> s"${LeanWorkspace.toolchain(library)}\n",
              directory.resolve(s"$name.lean") -> moduleText(name),
              directory.resolve(".gitignore") -> "/.lake\n"
            ).filter((file, text) => writeIfAbsent(file, text)).map(_._1)
            Files.createDirectories(directory.resolve(name))
            // A file that was there is left, as a `lean-toolchain`, and may not be the library's.
            Outcome(directory, true, written ::: unpacked, disagreements(directory, library))
        else
            required(lakefile) match
                case None =>
                    val note = s"its lakefile does not require $libraryName from a path, so " +
                        "Scalus's Lean library is not brought to it"
                    Outcome(directory, false, Nil, List(note))
                case Some(library) =>
                    val ours = library.getParent != null &&
                        library.getParent.getFileName.toString == sharedDirectory
                    val unpacked = if ours then unpack(library) else Nil
                    // The directory of the modules its library is made of: Lake does not build
                    // without it, and a checkout does not have it while it is empty.
                    if Files.readString(lakefile).contains(s".andSubmodules `$name]") then
                        Files.createDirectories(directory.resolve(name))
                    Outcome(directory, false, unpacked, disagreements(directory, library))
    }

    /** `init <directory>` makes the workspace in the directory ready and prints what was done. */
    def main(args: Array[String]): Unit = args.toList match
        case "init" :: directory :: Nil => println(init(Path.of(directory)).report)
        case _ =>
            Console.err.println("usage: LeanWorkspaceTool init <directory of the workspace>")
            sys.exit(2)

    private def checkName(name: String): Unit = {
        if !name.matches("[A-Za-z][A-Za-z0-9_]*") then
            throw new IllegalArgumentException(
              s"the directory of a Lean workspace names its package, and `$name` is no name Lean " +
                  "can import: it has letters, digits and `_`, and starts with a letter"
            )
        val packages = LeanWorkspace.packagesIn(resource("lake-manifest.json")).map(_.name)
        val taken = leanLibraries ::: libraryName :: packages
        // Also where the letters differ in case only: a file system may not tell them apart.
        if taken.exists(_.equalsIgnoreCase(name)) then
            throw new IllegalArgumentException(
              s"`$name` cannot name a Lean workspace: Lean, or Scalus's Lean library, has a " +
                  "library of that name"
            )
    }

    /** The directory of the project that a workspace in `parent` is of: the nearest one, from
      * `parent` upwards, that has `.git`, as the root of a repository or of a worktree does.
      * Without one, `parent`.
      */
    private def project(parent: Path): Path =
        Iterator
            .iterate(parent)(_.getParent)
            .takeWhile(_ != null)
            .find(directory => Files.exists(directory.resolve(".git")))
            .getOrElse(parent)

    private val requirement = ("""require\s+«?""" + libraryName + """»?\s+from\s+"([^"]+)"""").r

    /** Where `lakefile` requires the library from, where it is a `lakefile.lean` that does so by a
      * path.
      */
    private def required(lakefile: Path): Option[Path] =
        if !Files.isRegularFile(lakefile) then None
        else
            requirement
                .findFirstMatchIn(Files.readString(lakefile, StandardCharsets.UTF_8))
                .map(found => lakefile.getParent.resolve(found.group(1)).normalize)

    private val resources = "/scalus/verify/lean/library/"

    private def resource(name: String): Array[Byte] = {
        val stream = getClass.getResourceAsStream(resources + name)
        if stream == null then
            throw new IllegalStateException(
              s"Scalus's Lean library is not on the classpath: no resource $resources$name"
            )
        Using.resource(stream)(_.readAllBytes())
    }

    /** Writes the library into `library` as the jar has it, and returns the files that changed. A
      * file that is as the jar has it is not written, so Lake builds nothing again for it. A module
      * the jar no longer has is removed. What Lake built there, in `.lake`, stays. The directory
      * above is `.scalus`, which git is told to leave out.
      */
    private def unpack(library: Path): List[Path] = {
        val ignored = library.getParent.resolve(".gitignore")
        val ignoring = if writeIfAbsent(ignored, "*\n") then List(ignored) else Nil
        val names = new String(resource("index"), StandardCharsets.UTF_8).linesIterator
            .filter(_.nonEmpty)
            .toList
        val written = names.flatMap { name =>
            val file = library.resolve(name)
            val bytes = resource(name)
            if Files.isRegularFile(file) && java.util.Arrays.equals(Files.readAllBytes(file), bytes)
            then None
            else
                write(file, bytes)
                Some(file)
        }
        val modules = library.resolve(libraryName)
        if Files.isDirectory(modules) then
            val kept = names.map(library.resolve(_)).toSet
            val there = Using.resource(Files.walk(modules))(_.iterator().asScala.toList)
            there
                .filter(file => Files.isRegularFile(file) && file.toString.endsWith(".lean"))
                .filterNot(kept.contains)
                .foreach(Files.deleteIfExists)
        ignoring ::: written
    }

    /** Writes `bytes` as `file` at once: into a file beside it, which then takes its place. */
    private def write(file: Path, bytes: Array[Byte]): Unit = {
        Files.createDirectories(file.getParent)
        val pid = ProcessHandle.current.pid
        val beside = file.resolveSibling(s".${file.getFileName}.$pid.${System.nanoTime}.tmp")
        try
            Files.write(beside, bytes)
            Files.move(
              beside,
              file,
              StandardCopyOption.ATOMIC_MOVE,
              StandardCopyOption.REPLACE_EXISTING
            )
        finally Files.deleteIfExists(beside)
    }

    /** Writes `text` into `file` where there is no such file, and says whether it did. */
    private def writeIfAbsent(file: Path, text: String): Boolean =
        if Files.exists(file) then false
        else
            write(file, text.getBytes(StandardCharsets.UTF_8))
            true

    /** Where a workspace does not agree with the library it requires from `library`: its Lean, and
      * the revisions of the packages its manifest pins. A check imports the library, so they have
      * to be the library's.
      */
    private def disagreements(directory: Path, library: Path): List[String] =
        if !Files.isRegularFile(library.resolve("lean-toolchain")) then
            List(s"its lakefile requires $libraryName from $library, where it is not")
        else
            val expected = LeanWorkspace.toolchain(library)
            val toolchain = directory.resolve("lean-toolchain")
            val lean =
                if !Files.isRegularFile(toolchain) then
                    List(s"it has no lean-toolchain: the library's is $expected")
                else
                    val found = LeanWorkspace.toolchain(directory)
                    if found == expected then Nil
                    else List(s"its lean-toolchain is $found, and the library's is $expected")
            val pinned = LeanWorkspace.pinned(directory)
            val revisions =
                if !Files.isRegularFile(directory.resolve("lake-manifest.json")) then Nil
                else
                    LeanWorkspace.pinned(library).toList.sortBy(_._1).flatMap { (name, revision) =>
                        pinned.get(name) match
                            case Some(`revision`) => None
                            case Some(other) =>
                                Some(
                                  s"its manifest pins $name at ${other.take(12)}, and the " +
                                      s"library's at ${revision.take(12)}: run `lake update` in it"
                                )
                            case None =>
                                Some(s"its manifest does not pin $name: run `lake update` in it")
                    }
            lean ::: revisions

    private def lakefileText(name: String, library: String): String =
        s"""import Lake
           |open Lake DSL
           |
           |-- The Lean workspace of $name. Scalus runs the checks of its statements here, and proofs
           |-- written by hand belong here. Scalus created it, and it is yours from here on.
           |package «$name» where
           |  -- As in the workspace of Scalus's Lean library.
           |  moreGlobalServerArgs := #["--threads=4"]
           |  -- Blaster and PlutusCore are those of Scalus's Lean library: cloned and built beside it
           |  -- once, for every workspace of the project.
           |  packagesDir := "$library/.lake/packages"
           |
           |-- Scalus's Lean library, as the version of Scalus in the build has it. Scalus writes it
           |-- where this path says, as long as that is in a `.scalus`.
           |require $libraryName from "$library"
           |
           |@[default_target]
           |lean_lib «$name» where
           |  -- Every module under `$name/` is built, so a proof written by hand is checked without
           |  -- being imported from `$name.lean`.
           |  globs := #[.andSubmodules `$name]
           |""".stripMargin

    private def moduleText(name: String): String =
        s"""import $libraryName.Run
           |
           |/-!
           |$name. What a check of Scalus imports is imported here, so building this library builds
           |everything such a check needs. Proofs written by hand go into modules under `$name/`,
           |which `lake build` builds with it.
           |-/
           |""".stripMargin
}
