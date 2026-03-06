package scalus.examples.setbench

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}
import scalus.crypto.trie.FusedMerklePatriciaForestry as Mpf16b
import scalus.crypto.trie.FusedMerklePatriciaForestry2 as Mpf2b
import scalus.crypto.trie.MerklePatriciaForestry as Mpf16o
import scalus.crypto.trie.{MerklePatriciaForestry64 as Mpf64o}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.{
    Proof as Mpf16oProof,
    ProofStep as Mpf16oStep
}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry64.{
    Proof as Mpf64oProof,
    ProofStep as Mpf64oStep
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

/** Benchmark: radix comparison (MPF-16 vs MPF-64, both unfused) and fusing optimization (MPF-16
  * unfused vs fused).
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

    // --- Proof encoding: MPF-64 unfused (Data) ---

    private def mpf64oStepToData(step: Mpf64oStep): Data = step match
        case Mpf64oStep.Branch(skip, neighbors) =>
            Constr(0, PList(I(skip), B(neighbors)))
        case Mpf64oStep.Fork(skip, neighbor) =>
            val neighborData =
                Constr(0, PList(I(neighbor.sixit), B(neighbor.prefix), B(neighbor.root)))
            Constr(1, PList(I(skip), neighborData))
        case Mpf64oStep.Leaf(skip, key, value) =>
            Constr(2, PList(I(skip), B(key), B(value)))

    private def mpf64oProofToData(proof: Mpf64oProof): Data = {
        val steps = proof.toScalaList.map(mpf64oStepToData)
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

    private val mpf64oHasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry64
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry64.*
            val trie = MerklePatriciaForestry64(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    private val mpf16bHasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry.*
            val trie = FusedMerklePatriciaForestry(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    private val mpf2bHasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry2
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry2.*
            val trie = FusedMerklePatriciaForestry2(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    private val mpf2bInsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry2
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry2.*
            val trie = FusedMerklePatriciaForestry2(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    private val mpf16oInsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.*
            val trie = MerklePatriciaForestry(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    private val mpf64oInsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry64
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry64.*
            val trie = MerklePatriciaForestry64(unBData(rootD))
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
    // Part 1: Radix comparison — MPF-16 vs MPF-64 (both unfused)
    // ============================================================

    test("has() radix comparison: MPF-16 vs MPF-64 (both unfused)") {
        info("")
        info("=== has() radix comparison: MPF-16 vs MPF-64 (both unfused, Data proofs) ===")
        info("")

        val hdr =
            f"${"N"}%6s | ${"MPF-16 cpu"}%14s | ${"MPF-64 cpu"}%14s | ${"64/16"}%6s | ${"MPF-16 proof(B)"}%15s | ${"MPF-64 proof(B)"}%15s"
        info(hdr)
        info("-" * hdr.length)

        for n <- collectionSizes do
            val elems = allElements.take(n)
            val mpf16 = Mpf16o.fromList(elems)
            val mpf64 = Mpf64o.fromList(elems)

            val sampleSize = math.min(SampleSize, n)
            val sampleIndices =
                new scala.util.Random(123).shuffle((0 until n).toList).take(sampleSize)

            var total16 = ExUnits(0, 0)
            var total64 = ExUnits(0, 0)
            var totalProof16 = 0L
            var totalProof64 = 0L

            for idx <- sampleIndices do
                val (key, value) = elems(idx)
                val proof16 = mpf16oProofToData(mpf16.proveMembership(key))
                val proof64 = mpf64oProofToData(mpf64.proveMembership(key))
                totalProof16 += proof16.toCbor.length
                totalProof64 += proof64.toCbor.length

                val budget16 =
                    measureTrieOp(mpf16oHasProgram, mpf16.rootHash, key, value, proof16)
                val budget64 =
                    measureTrieOp(mpf64oHasProgram, mpf64.rootHash, key, value, proof64)
                total16 = ExUnits(total16.memory + budget16.memory, total16.steps + budget16.steps)
                total64 = ExUnits(total64.memory + budget64.memory, total64.steps + budget64.steps)

            val avg16 = ExUnits(total16.memory / sampleSize, total16.steps / sampleSize)
            val avg64 = ExUnits(total64.memory / sampleSize, total64.steps / sampleSize)
            val avgP16 = (totalProof16 / sampleSize).toInt
            val avgP64 = (totalProof64 / sampleSize).toInt
            val ratio = avg64.steps.toDouble / avg16.steps
            info(
              f"${n}%6d | ${avg16.steps}%14d | ${avg64.steps}%14d | ${ratio}%6.3f | ${avgP16}%15d | ${avgP64}%15d"
            )
    }

    test("insert() radix comparison: MPF-16 vs MPF-64 (both unfused)") {
        info("")
        info("=== insert() radix comparison: MPF-16 vs MPF-64 (both unfused, Data proofs) ===")
        info("")

        val hdr =
            f"${"N"}%6s | ${"MPF-16 cpu"}%14s | ${"MPF-64 cpu"}%14s | ${"64/16"}%6s | ${"MPF-16 fee"}%10s | ${"MPF-64 fee"}%10s"
        info(hdr)
        info("-" * hdr.length)

        for n <- collectionSizes do
            val elems = allElements.take(n)
            val mpf16 = Mpf16o.fromList(elems)
            val mpf64 = Mpf64o.fromList(elems)

            val sampleSize = math.min(SampleSize, n)
            val sampleIndices =
                new scala.util.Random(456).shuffle((0 until n).toList).take(sampleSize)

            var total16 = ExUnits(0, 0)
            var total64 = ExUnits(0, 0)

            for idx <- sampleIndices do
                val (key, value) = elems(idx)
                val mpf16Without = mpf16.delete(key)
                val mpf64Without = mpf64.delete(key)

                val budget16 = measureTrieOp(
                  mpf16oInsertProgram,
                  mpf16Without.rootHash,
                  key,
                  value,
                  mpf16oProofToData(mpf16Without.proveNonMembership(key))
                )
                val budget64 = measureTrieOp(
                  mpf64oInsertProgram,
                  mpf64Without.rootHash,
                  key,
                  value,
                  mpf64oProofToData(mpf64Without.proveNonMembership(key))
                )
                total16 = ExUnits(total16.memory + budget16.memory, total16.steps + budget16.steps)
                total64 = ExUnits(total64.memory + budget64.memory, total64.steps + budget64.steps)

            val avg16 = ExUnits(total16.memory / sampleSize, total16.steps / sampleSize)
            val avg64 = ExUnits(total64.memory / sampleSize, total64.steps / sampleSize)
            val ratio = avg64.steps.toDouble / avg16.steps
            info(
              f"${n}%6d | ${avg16.steps}%14d | ${avg64.steps}%14d | ${ratio}%6.3f | ${feeLovelace(avg16)}%10d | ${feeLovelace(avg64)}%10d"
            )
    }

    // ============================================================
    // Part 2: Fusing optimization — MPF-16 unfused vs fused
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

    // ============================================================
    // Part 3: Fused radix comparison — FusedMPF-2 vs FusedMPF-16
    // ============================================================

    test("has() fused radix: FusedMPF-2 vs FusedMPF-16") {
        info("")
        info("=== has() fused radix comparison: FusedMPF-2 vs FusedMPF-16 ===")
        info("")

        val hdr =
            f"${"N"}%6s | ${"Fused-2 cpu"}%14s | ${"Fused-16 cpu"}%14s | ${"2/16"}%6s | ${"Fused-2 proof(B)"}%16s | ${"Fused-16 proof(B)"}%17s"
        info(hdr)
        info("-" * hdr.length)

        for n <- collectionSizes do
            val elems = allElements.take(n)
            val mpf2 = Mpf2b.fromList(elems)
            val mpf16 = Mpf16b.fromList(elems)

            val sampleSize = math.min(SampleSize, n)
            val sampleIndices =
                new scala.util.Random(123).shuffle((0 until n).toList).take(sampleSize)

            var total2 = ExUnits(0, 0)
            var total16 = ExUnits(0, 0)
            var totalProof2 = 0L
            var totalProof16 = 0L

            for idx <- sampleIndices do
                val (key, value) = elems(idx)
                val proof2 = B(mpf2.proveMembership(key))
                val proof16 = B(mpf16.proveMembership(key))
                totalProof2 += proof2.toCbor.length
                totalProof16 += proof16.toCbor.length

                val budget2 =
                    measureTrieOp(mpf2bHasProgram, mpf2.rootHash, key, value, proof2)
                val budget16 =
                    measureTrieOp(mpf16bHasProgram, mpf16.rootHash, key, value, proof16)
                total2 = ExUnits(total2.memory + budget2.memory, total2.steps + budget2.steps)
                total16 = ExUnits(total16.memory + budget16.memory, total16.steps + budget16.steps)

            val avg2 = ExUnits(total2.memory / sampleSize, total2.steps / sampleSize)
            val avg16 = ExUnits(total16.memory / sampleSize, total16.steps / sampleSize)
            val avgP2 = (totalProof2 / sampleSize).toInt
            val avgP16 = (totalProof16 / sampleSize).toInt
            val ratio = avg2.steps.toDouble / avg16.steps
            info(
              f"${n}%6d | ${avg2.steps}%14d | ${avg16.steps}%14d | ${ratio}%6.3f | ${avgP2}%16d | ${avgP16}%17d"
            )
    }

    test("insert() fused radix: FusedMPF-2 vs FusedMPF-16") {
        info("")
        info("=== insert() fused radix comparison: FusedMPF-2 vs FusedMPF-16 ===")
        info("")

        val hdr =
            f"${"N"}%6s | ${"Fused-2 cpu"}%14s | ${"Fused-16 cpu"}%14s | ${"2/16"}%6s | ${"Fused-2 fee"}%11s | ${"Fused-16 fee"}%12s"
        info(hdr)
        info("-" * hdr.length)

        for n <- collectionSizes do
            val elems = allElements.take(n)
            val mpf2 = Mpf2b.fromList(elems)
            val mpf16 = Mpf16b.fromList(elems)

            val sampleSize = math.min(SampleSize, n)
            val sampleIndices =
                new scala.util.Random(456).shuffle((0 until n).toList).take(sampleSize)

            var total2 = ExUnits(0, 0)
            var total16 = ExUnits(0, 0)

            for idx <- sampleIndices do
                val (key, value) = elems(idx)
                val mpf2Without = mpf2.delete(key)
                val mpf16Without = mpf16.delete(key)

                val budget2 = measureTrieOp(
                  mpf2bInsertProgram,
                  mpf2Without.rootHash,
                  key,
                  value,
                  B(mpf2Without.proveNonMembership(key))
                )
                val budget16 = measureTrieOp(
                  mpf16bInsertProgram,
                  mpf16Without.rootHash,
                  key,
                  value,
                  B(mpf16Without.proveNonMembership(key))
                )
                total2 = ExUnits(total2.memory + budget2.memory, total2.steps + budget2.steps)
                total16 = ExUnits(total16.memory + budget16.memory, total16.steps + budget16.steps)

            val avg2 = ExUnits(total2.memory / sampleSize, total2.steps / sampleSize)
            val avg16 = ExUnits(total16.memory / sampleSize, total16.steps / sampleSize)
            val ratio = avg2.steps.toDouble / avg16.steps
            info(
              f"${n}%6d | ${avg2.steps}%14d | ${avg16.steps}%14d | ${ratio}%6.3f | ${feeLovelace(avg2)}%11d | ${feeLovelace(avg16)}%12d"
            )
    }
}
