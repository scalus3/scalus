package scalus.sbt

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
}
