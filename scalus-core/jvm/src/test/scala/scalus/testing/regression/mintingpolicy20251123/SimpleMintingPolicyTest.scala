package scalus.testing.regression.mintingpolicy20251123

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.ledger.api.v3.{TxId, TxOutRef}

class SimpleMintingPolicyTest extends AnyFunSuite {

    test("Minimal minting policy triggers IndexOutOfBoundsException: 41 in DeBruijn conversion") {
        import scalus.uplc.DeBruijn

        // Create a simple UTxO reference to use as parameter
        val txId = TxId(ByteString.fromHex("ba387607dfdc714bc27d84daf7b41463ad8a77a5219bf8877960c1527fcc5465"))
        val utxoRef = TxOutRef(txId, BigInt(1))

        // Compile and apply the minting policy with the UTxO reference
        val compiledProgram = SimpleMintingPolicyContract.compileAndApply(utxoRef)

        // If we get here without exception, the bug is fixed
        assert(compiledProgram != null, "Program should compile and apply successfully")

        // The error happens when trying to convert FROM DeBruijn indices back to named variables
        // This is what the script evaluator does
        // This should trigger IndexOutOfBoundsException: 41 at DeBruijn.scala:57
        val namedTerm = DeBruijn.fromDeBruijnTerm(compiledProgram.term)

        println(s"DeBruijn fromDeBruijnTerm conversion successful!")

        // If we reach here, the bug is fixed!
        // Originally this threw IndexOutOfBoundsException: 41
    }

    test("Show SIR representation for debugging") {
        import scalus.show
        println("=== SIR representation ===")
        println(SimpleMintingPolicy.compiledValidator.show)
    }

    test("Show UPLC before apply for debugging") {
        val program = SimpleMintingPolicy.compiledValidator.toUplcOptimized().plutusV3
        println("=== UPLC before apply ===")
        println(program.pretty.render(100))
    }

    test("Show UPLC after apply for debugging") {
        val txId = TxId(ByteString.fromHex("ba387607dfdc714bc27d84daf7b41463ad8a77a5219bf8877960c1527fcc5465"))
        val utxoRef = TxOutRef(txId, BigInt(1))
        val program = SimpleMintingPolicy.compiledValidator.toUplcOptimized().plutusV3
        val utxoData = utxoRef.toData
        val appliedProgram = program $ utxoData

        println("=== UPLC after apply ===")
        println(appliedProgram.pretty.render(100))
    }
}
