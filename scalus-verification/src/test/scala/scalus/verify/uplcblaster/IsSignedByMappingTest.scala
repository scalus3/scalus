package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.v3.{PubKeyHash, TxInfo}
import scalus.uplc.PlutusV3

import java.nio.file.{Files, Path}

/** The program that `ScalusProofs.Mappings.IsSignedBy` is a lemma of is the one Scalus compiles now
  * from `TxInfo.isSignedBy`: where the compiler gives another, the lemma is of a program no
  * contract has, and is to be proved again of the new one.
  */
class IsSignedByMappingTest extends AnyFunSuite {

    /** The module of the lemma in Scalus's Lean library. */
    private val module: Path = LeanProofs.inSources(
      "scalus-verification",
      "src",
      "main",
      "lean",
      "ScalusProofs",
      "Mappings",
      "IsSignedBy.lean"
    )

    private val begin = "-- The program, written by scalus.verify.uplcblaster.LeanPrograms:"
    private val end = "-- The end of the program."

    test("the library's isSignedBy is the program Scalus compiles now") {
        val compiled =
            PlutusV3.compile((tx: TxInfo, pk: PubKeyHash) => tx.isSignedBy(pk))(using
              UplcBlaster.options
            )
        val written = LeanPrograms.program(compiled.program)
        val text = if Files.isRegularFile(module) then Files.readString(module) else ""
        val from = text.indexOf(begin)
        val until = text.indexOf(end)
        // The module lays the program out as it reads best: only the words are compared.
        def words(text: String): String = text.trim.replaceAll("\\s+", " ")
        val kept =
            if from < 0 || until < from then None
            else Some(words(text.substring(from + begin.length, until)))
        assert(
          kept.contains(words(written)),
          s"the compiled isSignedBy is not the program of $module: prove the lemma of\n$written"
        )
    }
}
