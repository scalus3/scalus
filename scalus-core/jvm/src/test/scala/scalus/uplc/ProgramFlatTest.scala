package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.test.ArbitraryInstances
import scalus.utils.Utils

class ProgramFlatTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    test("Program flat encoding is identical to Plutus") {
        forAll { (p: Program) =>
            val plutus = Utils.bytesToHex(UplcCli.uplcToFlat(p.show))
            val scalus = Utils.bytesToHex(p.flatEncoded)
            if plutus != scalus then
                // Emit the full failing program so a (flaky) CI failure is reproducible:
                // hex strings untruncated (assert's diff elides them) plus the AST to rebuild it.
                val firstDiffByte = plutus.zip(scalus).takeWhile((a, b) => a == b).length / 2
                val ast =
                    try p.toString
                    catch case _: Throwable => "<toString overflowed>"
                fail(
                  s"flat encoding mismatch: version=${p.version} firstDiffByte=$firstDiffByte " +
                      s"plutusLen=${plutus.length / 2} scalusLen=${scalus.length / 2}\n" +
                      s"plutus=$plutus\nscalus=$scalus\nAST=$ast"
                )
        }
    }

    test("Program flat decode(encode(p)) is identical to p") {
        forAll { (p: Program) =>
            // deBruijn first to get the indexes right
            val program = p.deBruijnedProgram.toProgram
            val decoded = Program.fromFlatEncoded(p.flatEncoded)
            assert(program α_== decoded)
        }
    }
}
