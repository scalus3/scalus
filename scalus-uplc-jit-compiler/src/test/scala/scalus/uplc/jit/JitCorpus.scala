package scalus.uplc.jit

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.uplc.eval.PlutusVM
import scalus.uplc.{DeBruijnedProgram, Term}

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The scripts the JIT is measured and checked on (spec
  * `2026-10-19-uplc-jit-simplification-design`, section 4.1): the 89 `plutus_use_cases` scripts of
  * the Cardano Plutus VM benchmark and the exported Scalus validators
  * (`scripts/export-scalus-corpus.sh`). Each is a fully applied program.
  *
  * Paths are relative to the repository root, the working directory of the forked tests and of the
  * bench; `-Djit.corpus.root=<dir>` points elsewhere.
  */
object JitCorpus {

    /** One corpus script: `name` is its file name without `.flat`. */
    final case class Script(name: String, program: DeBruijnedProgram) {
        lazy val term: Term = program.toProgram.term
    }

    private def root: Path = Paths.get(sys.props.getOrElse("jit.corpus.root", "."))

    val PlutusUseCasesDir = "scalus-core/jvm/src/test/resources/data/plutus_use_cases"
    val ScalusDir = "scalus-uplc-jit-compiler/src/test/resources/corpus/scalus"

    /** The machine of the JIT's scope: PlutusV3, builtin semantics variant E, protocol version 11.
      */
    val vm: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    def load(dir: String): List[Script] =
        Files
            .list(root.resolve(dir))
            .iterator()
            .asScala
            .filter(_.getFileName.toString.endsWith(".flat"))
            .toList
            .sortBy(_.getFileName.toString)
            .map { f =>
                Script(
                  f.getFileName.toString.stripSuffix(".flat"),
                  DeBruijnedProgram.fromFlatEncoded(Files.readAllBytes(f))
                )
            }

    def plutusUseCases: List[Script] = load(PlutusUseCasesDir)

    def scalus: List[Script] = load(ScalusDir)

    def all: List[Script] = plutusUseCases ++ scalus

    /** The scripts the bench times (owner's choice, 2026-10-19): the whole corpus does not reach a
      * steady state in one JVM (see the spec, implementation status). Small, medium and large
      * `plutus_use_cases` scripts, and Scalus validators from the smallest to the largest. A name
      * is a prefix of a file name.
      */
    val BenchSubset: List[String] = List(
      "coop-6",
      "guardrail-unsorted-large",
      "pubkey-1",
      "auction_1-1",
      "uniswap-1",
      "auction_1-3",
      "multisig-sm-02",
      "stablecoin_1-5",
      "htlc-spend",
      "editablenft-spend",
      "amm-mint",
      "setbench-spend",
      "midgard-spend",
      "linkedlist-spend"
    )

    def benchSubset: List[Script] = {
        val scripts = all
        BenchSubset.map(p =>
            scripts.find(_.name.startsWith(p)).getOrElse(throw new NoSuchElementException(p))
        )
    }

    /** Term nodes of `t`, the size measure of the staging compiler. */
    def nodes(t: Term): Long = StagingCompilerThread.nodeCount(t)
}
