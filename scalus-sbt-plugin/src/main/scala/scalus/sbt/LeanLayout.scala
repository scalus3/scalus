package scalus.sbt

import java.io.File
import java.util.Locale

/** Pure helpers for the Lean workspace of a project's proofs.
  *
  * Deliberately sbt-free so it unit-tests without an sbt harness and compiles unchanged under Scala
  * 2.12 (sbt 1) and Scala 3 (sbt 2).
  */
object LeanLayout {

    /** The name of the Lean package of the project `projectName`, which is also the name of the
      * workspace's directory: the project's words, each with a capital, as one word. Lean imports a
      * package by its name, so it has letters and digits only and starts with a letter:
      * `my-contracts` is `MyContracts`. A name without a letter is `Proofs`.
      */
    def packageName(projectName: String): String = {
        val words = projectName.split("[^A-Za-z0-9]+").filter(_.nonEmpty)
        val joined = words.map(capitalized).mkString.dropWhile(c => !Character.isLetter(c))
        if (joined.isEmpty) "Proofs" else capitalized(joined)
    }

    private def capitalized(word: String): String =
        word.substring(0, 1).toUpperCase(Locale.ROOT) + word.substring(1)

    /** Whether `classpath` has the tool of `scalus-verification` that makes a workspace ready,
      * `scalus.verify.lean.LeanWorkspaceTool`: its class file, which is looked up and not loaded.
      */
    def hasLeanTool(classpath: Seq[File]): Boolean = {
        val cl = new java.net.URLClassLoader(classpath.map(_.toURI.toURL).toArray, null)
        try cl.getResource("scalus/verify/lean/LeanWorkspaceTool$.class") != null
        finally cl.close()
    }

    /** The ScalaTest layer of `scalus-verification`: a suite of proofs extends it. */
    val ProofsSuite: String = "scalus.verify.uplcblaster.LeanProofsSuite"

    /** The tag of a test about a statement that Lean does not finish, which `verify` leaves out. */
    val UnfinishedTag: String = "scalus.verify.uplcblaster.Unfinished"

    /** Whether `directory` is a Lean workspace: a Lake package, which has a lakefile. */
    def isWorkspace(directory: File): Boolean =
        new File(directory, "lakefile.lean").isFile || new File(directory, "lakefile.toml").isFile

    /** The Lean workspaces in `directory`, by name: the directories in it that are Lake packages.
      * The workspaces of a project are those beside its `leanWorkspace`, in `src/test/lean`: its
      * own, and any other it keeps there, as Scalus's examples keep one for each example.
      */
    def workspaces(directory: File): Seq[File] =
        Option(directory.listFiles).toSeq.flatten
            .filter(file => file.isDirectory && isWorkspace(file))
            .sortBy(_.getName)

    /** The library of proofs of `workspace`, `<Package>Proofs`: the statements Scalus wrote under
      * `<Package>/Generated`, and the proofs written by hand of them under `<Package>/Proofs`.
      */
    def proofsLibrary(workspace: File): String = s"${workspace.getName}Proofs"

    /** Whether the lakefile of `workspace` declares its library of proofs ([[proofsLibrary]]). A
      * workspace that Scalus created does. One of Lean of its own, which Scalus writes no
      * statements into, need not.
      */
    def hasProofsLibrary(workspace: File): Boolean = {
        val library = java.util.regex.Pattern.quote(proofsLibrary(workspace))
        def text(file: File): String =
            new String(java.nio.file.Files.readAllBytes(file.toPath), "UTF-8")
        val lean = new File(workspace, "lakefile.lean")
        val toml = new File(workspace, "lakefile.toml")
        if (lean.isFile) s"(?m)lean_lib\\s+«?$library»?(\\s|$$)".r.findFirstIn(text(lean)).isDefined
        else if (toml.isFile)
            s"""(?m)^\\s*name\\s*=\\s*["']$library["']""".r.findFirstIn(text(toml)).isDefined
        else false
    }

    /** The directory of the proofs written by hand in `workspace`, `<Package>/Proofs`, which the
      * workspace's library `<Package>Proofs` builds.
      */
    def proofsDirectory(workspace: File): File =
        new File(new File(workspace, workspace.getName), "Proofs")

    /** Whether `workspace` has a proof written by hand: a Lean module under [[proofsDirectory]]. */
    def hasProofsByHand(workspace: File): Boolean = {
        def has(directory: File): Boolean =
            Option(directory.listFiles).toSeq.flatten.exists(file =>
                if (file.isDirectory) has(file) else file.getName.endsWith(".lean")
            )
        has(proofsDirectory(workspace))
    }

    /** The tests of `names` that `patterns` select, as `verify` takes them, and the patterns that
      * select none. A pattern is a test's name, with `*` for any characters, as `testOnly` takes it;
      * one without a `.` also selects the tests whose class has it as its simple name, so
      * `HtlcVerificationTest` selects `scalus.examples.htlc.HtlcVerificationTest`. A quote around a
      * pattern, which a shell or sbt's client can leave on it, is not part of it.
      */
    def selectTests(patterns: Seq[String], names: Seq[String]): (Seq[String], Seq[String]) = {
        def matches(raw: String, name: String): Boolean = {
            // No test's name has a quote: one is the shell's, which reached sbt with the name.
            val pattern = raw.stripPrefix("\"").stripSuffix("\"")
            val regex = pattern.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString(".*")
            name.matches(regex) ||
            (!pattern.contains(".") && name.substring(name.lastIndexOf('.') + 1).matches(regex))
        }
        val selected = names.filter(name => patterns.exists(matches(_, name))).distinct.sorted
        val unmatched = patterns.filterNot(pattern => names.exists(matches(pattern, _)))
        (selected, unmatched)
    }

    /** The arguments of `testOnly` for `verify`: the suites `suites`, without the tests tagged
      * [[UnfinishedTag]], which wait for their time limit.
      */
    def verifyArguments(suites: Seq[String]): String =
        suites.mkString(" ", " ", s" -- -l $UnfinishedTag")
}
