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
        assert(program == Program((1, 0, 0), λ("i0", "i1", "i2")(Term.Error())))
        val serialized = Utils.programToPlutusFileContent(
          program.deBruijnedProgram,
          Language.PlutusV2
        )
        assert(serialized == alwaysFails)
    }

    test("readPlutusFileContent rejects unknown envelope types") {
        val content =
            """{"type":"PaymentSigningKeyShelley_ed25519","description":"","cborHex":"4746010000222601"}"""
        assertThrows[IllegalArgumentException](Utils.readPlutusFileContent(content))
    }

    test("readPlutusFileContent accepts a 1.1.0 program in a PlutusScriptV1 envelope") {
        // Plutus Core 1.1.0 was PlutusV3-only until the van Rossem hard fork, which introduced it
        // for PlutusV1 and PlutusV2 as well. Such a script runs only at protocol version 11 and
        // later, but it is valid and must be readable.
        val program = Program((1, 1, 0), λ("i0")(Term.Error())).deBruijnedProgram
        val content = Utils.programToPlutusFileContent(program, Language.PlutusV1)
        assert(Utils.readPlutusFileContent(content).version == (1, 1, 0))
    }

    test("readPlutusFileContent rejects an unknown Plutus Core version") {
        val program = Program((2, 0, 0), λ("i0")(Term.Error())).deBruijnedProgram
        val content = Utils.programToPlutusFileContent(program, Language.PlutusV3)
        assertThrows[IllegalArgumentException](Utils.readPlutusFileContent(content))
    }
}
