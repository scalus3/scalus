package scalus.uplc.jit

import scalus.uplc.DeBruijnedProgram
import scalus.uplc.eval.{CountingBudgetSpender, NoLogger}

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Selects the Scalus corpus scripts from the scripts that the example tests dumped (see
  * `scripts/export-scalus-corpus.sh`).
  *
  * Usage: `ScalusCorpusSelect <dump dir> <out dir>`. The dump dir has one subdirectory per example
  * package with files `<script hash>-<language>-<tag>-<index>.flat`. Keeps the PlutusV3 scripts
  * that the CEK machine of [[JitCorpus.vm]] evaluates successfully, at most [[PerPackage]] distinct
  * script hashes per package, the largest first, and copies them to
  * `<out dir>/<package>-<tag>-<hash prefix>.flat`.
  */
object ScalusCorpusSelect {

    private val PerPackage = 1

    def main(args: Array[String]): Unit = {
        val dump = Paths.get(args(0))
        val out = Paths.get(args(1))
        Files.createDirectories(out)
        val packages = Files.list(dump).iterator().asScala.filter(Files.isDirectory(_)).toList
        for pkg <- packages.sortBy(_.getFileName.toString) do
            val candidates = flatFiles(pkg).filter(_.getFileName.toString.contains("-PlutusV3-"))
            val ok = candidates.flatMap { f =>
                val program = DeBruijnedProgram.fromFlatEncoded(Files.readAllBytes(f))
                val budget = CountingBudgetSpender()
                Try(JitCorpus.vm.evaluateDeBruijnedTerm(program.term, budget, NoLogger)).toOption
                    .map(_ => (f, JitCorpus.nodes(program.term)))
            }
            val chosen = ok
                .sortBy(-_._2)
                .distinctBy((f, _) => hash(f))
                .take(PerPackage)
            println(
              s"${pkg.getFileName}: ${candidates.size} PlutusV3 scripts, ${ok.size} succeed, " +
                  s"chosen ${chosen.map((f, n) => s"${f.getFileName} ($n nodes)").mkString(", ")}"
            )
            for (f, _) <- chosen do
                val parts = f.getFileName.toString.stripSuffix(".flat").split('-')
                val name = s"${pkg.getFileName}-${parts(2).toLowerCase}-${parts(0).take(8)}.flat"
                Files.copy(f, out.resolve(name), StandardCopyOption.REPLACE_EXISTING)
    }

    private def flatFiles(dir: Path): List[Path] =
        Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".flat")).toList

    private def hash(f: Path): String = f.getFileName.toString.takeWhile(_ != '-')
}
