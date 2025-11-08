package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.uplc.test.ArbitraryInstances
import scalus.utils.Utils

class ProgramFlatTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    test("Program flat encoding is identical to Plutus") {
        forAll { (p: Program) =>
            val str = p.show
            val bytes = UplcCli.uplcToFlat(str)
            assert(Utils.bytesToHex(bytes) == Utils.bytesToHex(p.flatEncoded), p.showHighlighted)
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
