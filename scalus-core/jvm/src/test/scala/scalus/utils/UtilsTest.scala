package scalus.utils

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.Language
import scalus.uplc.{Program, Term}
import scalus.uplc.Term.*
import scalus.uplc.test.ArbitraryInstances

import java.nio.file.Files

class UtilsTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances
    with scalus.cardano.onchain.plutus.v1.ArbitraryInstances {

    test("programToPlutusFileContent/readPlutusFileContent work") {
        val term = Arbitrary.arbitrary[Term].sample.get
        val debruijnedProgram = term.plutusV2.deBruijnedProgram
        val undebuijnedProgram = debruijnedProgram.toProgram
        val f = Files.createTempFile("test", ".plutus").toFile
        val path = f.getAbsolutePath
        f.deleteOnExit()
        Utils.writePlutusFile(path, debruijnedProgram, Language.PlutusV2)
        val program2 = Utils.readPlutusFile(path)
        assert(undebuijnedProgram == program2)
    }

    test("readPlutusFile always-fails.plutus work") {
        val alwaysFails =
            """{"type":"PlutusScriptV2","description":"","cborHex":"4746010000222601"}"""
        val program = Utils.readPlutusFileContent(alwaysFails)
        assert(program == Program((1, 0, 0), λ("i0", "i1", "i2")(Term.Error)))
        val serialized = Utils.programToPlutusFileContent(
          program.deBruijnedProgram,
          Language.PlutusV2
        )
        assert(serialized == alwaysFails)
    }
}
