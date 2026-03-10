package scalus.examples.setbench

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}
import scalus.crypto.trie.FusedMerklePatriciaForestry as Mpf16b
import scalus.crypto.trie.MerklePatriciaForestry as Mpf16o
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.{
    Proof as Mpf16oProof,
    ProofStep as Mpf16oStep
}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{B, Constr, I}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.PlutusV3
import scalus.uplc.Term.asTerm

/** Benchmark: fusing optimization (MPF-16 unfused vs fused).
  *
  * Run with: sbt "scalusExamplesJVM/testOnly *FusingBenchmarkTest"
  */
class FusingBenchmarkTest extends AnyFunSuite {

    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = false,
      optimizeUplc = true,
      debug = false
    )

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val exPrices = ExUnitPrices(
      priceMemory = NonNegativeInterval(0.0577, precision = 15),
      priceSteps = NonNegativeInterval(0.0000721, precision = 15)
    )

    private def feeLovelace(eu: ExUnits): Long = eu.fee(exPrices).value

    private val MaxN = 32000
    private val SampleSize = 10

    private val allElements: Vector[(ByteString, ByteString)] = {
        val rng = new scala.util.Random(42)
        Vector.tabulate(MaxN) { i =>
            val key = ByteString.fromString(s"element-${rng.nextInt()}-$i")
            val value = ByteString.fromString(s"value-$i")
            (key, value)
        }
    }

    // --- Proof encoding: MPF-16 unfused (Data) ---

    private def mpf16oStepToData(step: Mpf16oStep): Data = step match
        case Mpf16oStep.Branch(skip, neighbors) =>
            Constr(0, PList(I(skip), B(neighbors)))
        case Mpf16oStep.Fork(skip, neighbor) =>
            val neighborData =
                Constr(0, PList(I(neighbor.nibble), B(neighbor.prefix), B(neighbor.root)))
            Constr(1, PList(I(skip), neighborData))
        case Mpf16oStep.Leaf(skip, key, value) =>
            Constr(2, PList(I(skip), B(key), B(value)))

    private def mpf16oProofToData(proof: Mpf16oProof): Data = {
        val steps = proof.toScalaList.map(mpf16oStepToData)
        Data.List(steps.foldRight(PList.Nil: PList[Data]) { (d, acc) => PList.Cons(d, acc) })
    }

    // --- Compiled UPLC programs ---

    private val mpf16oHasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.*
            val trie = MerklePatriciaForestry(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    private val mpf16bHasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry.*
            val trie = FusedMerklePatriciaForestry(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    private val mpf16oInsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.*
            val trie = MerklePatriciaForestry(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    private val mpf16bInsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry.*
            val trie = FusedMerklePatriciaForestry(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // --- Measurement ---

    private def measureTrieOp(
        program: PlutusV3[?],
        root: ByteString,
        key: ByteString,
        value: ByteString,
        proofData: Data
    ): ExUnits = {
        val applied = program.program.term $
            B(root).asTerm $
            B(key).asTerm $
            B(value).asTerm $
            proofData.asTerm
        applied.evaluateDebug match
            case Result.Success(_, exunits, _, _) => exunits
            case Result.Failure(ex, _, _, _)      => fail(s"trie op failed: ${ex.getMessage}")
    }

    private val collectionSizes = Seq(30, 100, 1000, 32000)

    // ============================================================
    // Fusing optimization — MPF-16 unfused vs fused
    // ============================================================

    test("has() fusing: MPF-16 unfused vs FusedMPF-16") {
        info("")
        info("=== has() fusing optimization: MPF-16 (Data) vs FusedMPF-16 (binary) ===")
        info("")

        val hdr =
            f"${"N"}%6s | ${"MPF-16 cpu"}%14s | ${"FusedMPF-16 cpu"}%14s | ${"ratio"}%6s | ${"MPF-16 proof(B)"}%15s | ${"Fused proof(B)"}%14s"
        info(hdr)
        info("-" * hdr.length)

        for n <- collectionSizes do
            val elems = allElements.take(n)
            val mpf16o = Mpf16o.fromList(elems)
            val mpf16b = Mpf16b.fromList(elems)

            val sampleSize = math.min(SampleSize, n)
            val sampleIndices =
                new scala.util.Random(123).shuffle((0 until n).toList).take(sampleSize)

            var totalO = ExUnits(0, 0)
            var totalB = ExUnits(0, 0)
            var totalProofO = 0L
            var totalProofB = 0L

            for idx <- sampleIndices do
                val (key, value) = elems(idx)
                val proofO = mpf16oProofToData(mpf16o.proveMembership(key))
                val proofB = B(mpf16b.proveMembership(key))
                totalProofO += proofO.toCbor.length
                totalProofB += proofB.toCbor.length

                val budgetO = measureTrieOp(mpf16oHasProgram, mpf16o.rootHash, key, value, proofO)
                val budgetB = measureTrieOp(mpf16bHasProgram, mpf16b.rootHash, key, value, proofB)
                totalO = ExUnits(totalO.memory + budgetO.memory, totalO.steps + budgetO.steps)
                totalB = ExUnits(totalB.memory + budgetB.memory, totalB.steps + budgetB.steps)

            val avgO = ExUnits(totalO.memory / sampleSize, totalO.steps / sampleSize)
            val avgB = ExUnits(totalB.memory / sampleSize, totalB.steps / sampleSize)
            val avgPO = (totalProofO / sampleSize).toInt
            val avgPB = (totalProofB / sampleSize).toInt
            val ratio = avgB.steps.toDouble / avgO.steps
            info(
              f"${n}%6d | ${avgO.steps}%14d | ${avgB.steps}%14d | ${ratio}%6.3f | ${avgPO}%15d | ${avgPB}%14d"
            )
    }

    test("insert() fusing: MPF-16 unfused vs FusedMPF-16") {
        info("")
        info("=== insert() fusing optimization: MPF-16 (Data) vs FusedMPF-16 (binary) ===")
        info("")

        val hdr =
            f"${"N"}%6s | ${"MPF-16 cpu"}%14s | ${"FusedMPF-16 cpu"}%14s | ${"ratio"}%6s | ${"MPF-16 fee"}%10s | ${"Fused fee"}%10s"
        info(hdr)
        info("-" * hdr.length)

        for n <- collectionSizes do
            val elems = allElements.take(n)
            val mpf16o = Mpf16o.fromList(elems)
            val mpf16b = Mpf16b.fromList(elems)

            val sampleSize = math.min(SampleSize, n)
            val sampleIndices =
                new scala.util.Random(456).shuffle((0 until n).toList).take(sampleSize)

            var totalO = ExUnits(0, 0)
            var totalB = ExUnits(0, 0)

            for idx <- sampleIndices do
                val (key, value) = elems(idx)
                val mpf16oWithout = mpf16o.delete(key)
                val mpf16bWithout = mpf16b.delete(key)

                val budgetO = measureTrieOp(
                  mpf16oInsertProgram,
                  mpf16oWithout.rootHash,
                  key,
                  value,
                  mpf16oProofToData(mpf16oWithout.proveNonMembership(key))
                )
                val budgetB = measureTrieOp(
                  mpf16bInsertProgram,
                  mpf16bWithout.rootHash,
                  key,
                  value,
                  B(mpf16bWithout.proveNonMembership(key))
                )
                totalO = ExUnits(totalO.memory + budgetO.memory, totalO.steps + budgetO.steps)
                totalB = ExUnits(totalB.memory + budgetB.memory, totalB.steps + budgetB.steps)

            val avgO = ExUnits(totalO.memory / sampleSize, totalO.steps / sampleSize)
            val avgB = ExUnits(totalB.memory / sampleSize, totalB.steps / sampleSize)
            val ratio = avgB.steps.toDouble / avgO.steps
            info(
              f"${n}%6d | ${avgO.steps}%14d | ${avgB.steps}%14d | ${ratio}%6.3f | ${feeLovelace(avgO)}%10d | ${feeLovelace(avgB)}%10d"
            )
    }
}
