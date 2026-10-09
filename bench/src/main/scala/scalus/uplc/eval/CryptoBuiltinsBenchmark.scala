package scalus.uplc.eval

import org.openjdk.jmh.annotations.*
import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion}
import scalus.uplc.{DeBruijnedProgram, Program, Term}

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

/** Evaluates Plutus conformance programs that call the crypto builtins, each once per operation:
  * the work the builtin's implementation does, plus a few machine steps. Needs the
  * `plutus-conformance` corpus linked at the repository root. `testCase` may also be the path of
  * any `.uplc` file.
  *
  * Moving to scalus-crypto-jni (the node's libsodium, secp256k1 and blst) measured, against bcprov,
  * scalus-secp256k1-jni and blst-java on an M3 Max: Ed25519 51 -> 31 us, ECDSA 24 -> 21 us, Schnorr
  * 24 -> 21 us, G1 x a full 255-bit scalar 74 -> 57 us, G1 x 44 6.6 -> 57 us. The last is by
  * design: like cardano-crypto-class, the binding multiplies with all 256 bits whatever the scalar,
  * where blst-java passed only the scalar's bytes (see `blst.c`).
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class CryptoBuiltinsBenchmark {

    @Param(
      Array(
        "verifyEd25519Signature/test-vector-01",
        "verifyEcdsaSecp256k1Signature/test-vector-01",
        "verifySchnorrSecp256k1Signature/test-vector-01",
        "bls12_381_G1_scalarMul/mul-44",
        "bls12_381_G2_scalarMul/mul-44",
        "bls12_381_G1_hashToGroup/hash",
        "bls12_381_millerLoop/random-pairing",
        "bls12_381-cardano-crypto-tests/signature/augmented",
        "blake2b_256/blake2b_256-length-200"
      )
    )
    @nowarn("msg=unset private variable")
    private var testCase: String = ""

    private var program: DeBruijnedProgram = null
    private val vm = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    @Setup
    def setup(): Unit = {
        // a conformance case, or a .uplc file of your own
        val file =
            if testCase.endsWith(".uplc") then Path.of(testCase)
            else
                val dir = Path.of(
                  "../plutus-conformance/test-cases/uplc/evaluation/builtin/semantics",
                  testCase
                )
                dir.resolve(dir.getFileName.toString + ".uplc")
        program =
            Program.parseUplc(Files.readString(file)).fold(e => sys.error(e), _.deBruijnedProgram)
        // every case evaluates successfully: one that fails would measure the error path
        evaluate()
    }

    @Benchmark
    def evaluate(): Term =
        // not evaluateScript: a PlutusV3 script must return unit, and these programs return True
        vm.evaluateDeBruijnedTerm(
          program.term,
          RestrictingBudgetSpender(ExUnits.enormous),
          NoLogger
        )
}
