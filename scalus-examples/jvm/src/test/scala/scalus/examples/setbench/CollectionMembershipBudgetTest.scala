package scalus.examples.setbench

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}
import scalus.crypto.trie.FusedMerklePatriciaForestry as Mpf16b
import scalus.crypto.trie.FusedMerklePatriciaForestry64 as Mpf64b
import scalus.crypto.trie.MerklePatriciaForestry as Mpf16o
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.{Proof as Mpf16oProof, ProofStep as Mpf16oStep}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.crypto.accumulator.EthereumKzgCeremony
import scalus.crypto.accumulator.BilinearAccumulatorProver.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{B, Constr, I}
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.{Constant, PlutusV3, Term}
import scalus.uplc.Term.asTerm

/** Budget comparison for collection membership verification across multiple sizes: MPF16o
  * (Aiken-compatible), MPF16b (binary proofs), and bilinear accumulator (G1, Ethereum KZG
  * ceremony).
  *
  * Note: the Aiken MPF validator is a full spending validator (takes ScriptContext) and cannot be
  * measured as a standalone UPLC function. See `SetBenchEmulatorTest` for Aiken MPF benchmarks.
  *
  * Tagged with `scalus.testing.Benchmark` — excluded from default test runs. Run with:
  * {{{
  * sbtn "scalusExamplesJVM/testOnly *CollectionMembershipBudgetTest"
  * }}}
  */
class CollectionMembershipBudgetTest extends AnyFunSuite {
    import scalus.testing.Benchmark

    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = false,
      optimizeUplc = true,
      debug = false
    )

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // Mainnet execution unit prices (Chang hard fork)
    private val exPrices = ExUnitPrices(
      priceMemory = NonNegativeInterval(0.0577, precision = 15),
      priceSteps = NonNegativeInterval(0.0000721, precision = 15)
    )

    private def feeLovelace(eu: ExUnits): Long = eu.fee(exPrices).value

    private val MaxN = 100000
    private val SampleSize = 10

    /** Shared element pool — all collection sizes draw from this. */
    private val allElements: Vector[(ByteString, ByteString)] = {
        val rng = new scala.util.Random(42)
        Vector.tabulate(MaxN) { i =>
            val key = ByteString.fromString(s"element-${rng.nextInt()}-$i")
            val value = ByteString.fromString(s"value-$i")
            (key, value)
        }
    }

    private lazy val accElements: Vector[BigInt] = allElements.map { (k, _) =>
        byteStringToInteger(true, blake2b_256(k))
    }

    private lazy val ceremony = EthereumKzgCeremony.loadCeremony()

    private lazy val accSetup: Setup = {
        val t0 = System.nanoTime()
        val s = Setup.fromPoints(ceremony.g1Monomial.toVector, ceremony.g2Monomial.toVector)
        val ms = (System.nanoTime() - t0) / 1_000_000
        info(s"Accumulator setup created in ${ms} ms")
        s
    }

    // --- Proof encoding helpers ---

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

    private val mpf64bHasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry64
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry64.*
            val trie = FusedMerklePatriciaForestry64(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // G1 Accumulator: full on-chain verifyMembership (includes getG2Commitment)
    @annotation.nowarn("msg=unused import")
    private val accFullProgram = PlutusV3.compile {
        (g2_0: G2Element, g2_1: G2Element, acc: G1Element, element: BigInt, proof: G1Element) =>
            import scalus.cardano.onchain.plutus.crypto.accumulator.G1Accumulator
            import scalus.cardano.onchain.plutus.prelude.List
            val crs = List(g2_0, g2_1)
            val subset = List(element)
            G1Accumulator.verifyMembership(crs, acc, subset, proof)
    }

    // G1 Accumulator: pairing only (G2 commitment pre-computed off-chain)
    private val accPairingProgram = PlutusV3.compile {
        (acc: G1Element, g2: G2Element, proof: G1Element, commitment: G2Element) =>
            bls12_381_finalVerify(
              bls12_381_millerLoop(acc, g2),
              bls12_381_millerLoop(proof, commitment)
            )
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

    private val mpf64bInsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry64
            import scalus.cardano.onchain.plutus.crypto.trie.FusedMerklePatriciaForestry64.*
            val trie = FusedMerklePatriciaForestry64(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // --- Measurement helpers ---

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

    /** Average budget per system for a given collection size and operation. */
    private case class AvgBudget(
        mpf16o: ExUnits,
        mpf16b: ExUnits,
        mpf64b: ExUnits,
        proofSize16o: Int = 0,
        proofSize16b: Int = 0,
        proofSize64b: Int = 0
    )

    /** Build tries of the given size, sample elements, measure has() budgets, print table, return
      * averages.
      */
    private def runHasBudget(n: Int, seed: Int): AvgBudget = {
        val elems = allElements.take(n)
        val mpf16o = Mpf16o.fromList(elems)
        val mpf16b = Mpf16b.fromList(elems)
        val mpf64b = Mpf64b.fromList(elems)

        val sampleSize = math.min(SampleSize, n)
        val sampleIndices =
            new scala.util.Random(seed).shuffle((0 until n).toList).take(sampleSize)

        var totalMpf16o = ExUnits(0, 0)
        var totalMpf16b = ExUnits(0, 0)
        var totalMpf64b = ExUnits(0, 0)
        var totalProof16o = 0L
        var totalProof16b = 0L
        var totalProof64b = 0L

        val header =
            f"${"Element"}%-12s | ${"MPF16o fee"}%10s | ${"MPF16b fee"}%10s | ${"MPF64b fee"}%10s"
        val sep = "-" * header.length
        info(header)
        info(sep)

        for idx <- sampleIndices do
            val (key, value) = elems(idx)

            val proof16oData = mpf16oProofToData(mpf16o.proveMembership(key))
            val proof16bData = B(mpf16b.proveMembership(key))
            val proof64bData = B(mpf64b.proveMembership(key))

            totalProof16o += proof16oData.toCbor.length
            totalProof16b += proof16bData.toCbor.length
            totalProof64b += proof64bData.toCbor.length

            val mpf16oBudget =
                measureTrieOp(mpf16oHasProgram, mpf16o.rootHash, key, value, proof16oData)
            val mpf16bBudget =
                measureTrieOp(mpf16bHasProgram, mpf16b.rootHash, key, value, proof16bData)
            val mpf64bBudget =
                measureTrieOp(mpf64bHasProgram, mpf64b.rootHash, key, value, proof64bData)

            totalMpf16o = ExUnits(
              totalMpf16o.memory + mpf16oBudget.memory,
              totalMpf16o.steps + mpf16oBudget.steps
            )
            totalMpf16b = ExUnits(
              totalMpf16b.memory + mpf16bBudget.memory,
              totalMpf16b.steps + mpf16bBudget.steps
            )
            totalMpf64b = ExUnits(
              totalMpf64b.memory + mpf64bBudget.memory,
              totalMpf64b.steps + mpf64bBudget.steps
            )

            info(
              f"element-$idx%-12d | ${feeLovelace(mpf16oBudget)}%10d | ${feeLovelace(mpf16bBudget)}%10d | ${feeLovelace(mpf64bBudget)}%10d"
            )

        info(sep)
        val avgMpf16o = ExUnits(totalMpf16o.memory / sampleSize, totalMpf16o.steps / sampleSize)
        val avgMpf16b = ExUnits(totalMpf16b.memory / sampleSize, totalMpf16b.steps / sampleSize)
        val avgMpf64b = ExUnits(totalMpf64b.memory / sampleSize, totalMpf64b.steps / sampleSize)
        val avgP16o = (totalProof16o / sampleSize).toInt
        val avgP16b = (totalProof16b / sampleSize).toInt
        val avgP64b = (totalProof64b / sampleSize).toInt
        info(
          f"${"AVERAGE"}%-12s | ${feeLovelace(avgMpf16o)}%10d | ${feeLovelace(avgMpf16b)}%10d | ${feeLovelace(avgMpf64b)}%10d"
        )
        info(
          f"Avg proof CBOR bytes:   MPF16o=${avgP16o}%5d  MPF16b=${avgP16b}%5d  MPF64b=${avgP64b}%5d"
        )
        info(f"MPF16b/MPF16o cpu ratio: ${totalMpf16b.steps.toDouble / totalMpf16o.steps}%.3f")
        info(f"MPF64b/MPF16b cpu ratio: ${totalMpf64b.steps.toDouble / totalMpf16b.steps}%.3f")

        AvgBudget(avgMpf16o, avgMpf16b, avgMpf64b, avgP16o, avgP16b, avgP64b)
    }

    /** Build tries of the given size, sample elements, measure insert() budgets, print table,
      * return averages.
      */
    private def runInsertBudget(n: Int, seed: Int): AvgBudget = {
        val elems = allElements.take(n)
        val mpf16o = Mpf16o.fromList(elems)
        val mpf16b = Mpf16b.fromList(elems)
        val mpf64b = Mpf64b.fromList(elems)

        val sampleSize = math.min(SampleSize, n)
        val sampleIndices =
            new scala.util.Random(seed).shuffle((0 until n).toList).take(sampleSize)

        var totalMpf16o = ExUnits(0, 0)
        var totalMpf16b = ExUnits(0, 0)
        var totalMpf64b = ExUnits(0, 0)

        val header =
            f"${"Element"}%-12s | ${"MPF16o fee"}%10s | ${"MPF16b fee"}%10s | ${"MPF64b fee"}%10s"
        val sep = "-" * header.length
        info(header)
        info(sep)

        for idx <- sampleIndices do
            val (key, value) = elems(idx)
            val mpf16oWithout = mpf16o.delete(key)
            val mpf16bWithout = mpf16b.delete(key)
            val mpf64bWithout = mpf64b.delete(key)

            val mpf16oBudget = measureTrieOp(
              mpf16oInsertProgram,
              mpf16oWithout.rootHash,
              key,
              value,
              mpf16oProofToData(mpf16oWithout.proveNonMembership(key))
            )
            val mpf16bBudget = measureTrieOp(
              mpf16bInsertProgram,
              mpf16bWithout.rootHash,
              key,
              value,
              B(mpf16bWithout.proveNonMembership(key))
            )
            val mpf64bBudget = measureTrieOp(
              mpf64bInsertProgram,
              mpf64bWithout.rootHash,
              key,
              value,
              B(mpf64bWithout.proveNonMembership(key))
            )

            totalMpf16o = ExUnits(
              totalMpf16o.memory + mpf16oBudget.memory,
              totalMpf16o.steps + mpf16oBudget.steps
            )
            totalMpf16b = ExUnits(
              totalMpf16b.memory + mpf16bBudget.memory,
              totalMpf16b.steps + mpf16bBudget.steps
            )
            totalMpf64b = ExUnits(
              totalMpf64b.memory + mpf64bBudget.memory,
              totalMpf64b.steps + mpf64bBudget.steps
            )

            info(
              f"element-$idx%-12d | ${feeLovelace(mpf16oBudget)}%10d | ${feeLovelace(mpf16bBudget)}%10d | ${feeLovelace(mpf64bBudget)}%10d"
            )

        info(sep)
        val avgMpf16o = ExUnits(totalMpf16o.memory / sampleSize, totalMpf16o.steps / sampleSize)
        val avgMpf16b = ExUnits(totalMpf16b.memory / sampleSize, totalMpf16b.steps / sampleSize)
        val avgMpf64b = ExUnits(totalMpf64b.memory / sampleSize, totalMpf64b.steps / sampleSize)
        info(
          f"${"AVERAGE"}%-12s | ${feeLovelace(avgMpf16o)}%10d | ${feeLovelace(avgMpf16b)}%10d | ${feeLovelace(avgMpf64b)}%10d"
        )
        info(f"MPF16b/MPF16o cpu ratio: ${totalMpf16b.steps.toDouble / totalMpf16o.steps}%.3f")
        info(f"MPF64b/MPF16b cpu ratio: ${totalMpf64b.steps.toDouble / totalMpf16b.steps}%.3f")

        AvgBudget(avgMpf16o, avgMpf16b, avgMpf64b)
    }

    private def saveBudgetResults(op: String, results: Seq[(Int, AvgBudget)]): Unit = {
        val outDir = new java.io.File("target/bench-results")
        outDir.mkdirs()
        val timestamp = java.time.LocalDateTime.now().toString.replace(":", "-")
        val outFile = new java.io.File(outDir, s"budget-$op-$timestamp.json")
        val json = results
            .map { (n, avg) =>
                s"""  {"n":$n,"mpf16o":{"cpu":${avg.mpf16o.steps},"mem":${avg.mpf16o.memory},"fee":${feeLovelace(avg.mpf16o)},"proof":${avg.proofSize16o}},"mpf16b":{"cpu":${avg.mpf16b.steps},"mem":${avg.mpf16b.memory},"fee":${feeLovelace(avg.mpf16b)},"proof":${avg.proofSize16b}},"mpf64b":{"cpu":${avg.mpf64b.steps},"mem":${avg.mpf64b.memory},"fee":${feeLovelace(avg.mpf64b)},"proof":${avg.proofSize64b}}}"""
            }
            .mkString("[\n", ",\n", "\n]")
        java.nio.file.Files.writeString(outFile.toPath, json)
        info(s"Results saved to ${outFile.getPath}")
    }

    // --- Tests ---

    private val collectionSizes = Seq(30, 100, 1000, 32000, 100000)

    test("has() budget scaling: N=30, 100, 1K, 32K", Benchmark) {
        val results = for n <- collectionSizes yield
            info("")
            info(s"=== Collection size: $n ===")
            n -> runHasBudget(n, seed = 123)

        info("")
        info("=== has() budget summary (averages) ===")
        val hdr =
            f"${"N"}%6s | ${"MPF16o fee"}%10s | ${"MPF16b fee"}%10s | ${"MPF64b fee"}%10s"
        info(hdr)
        info("-" * hdr.length)
        for (n, avg) <- results do
            info(
              f"${n}%6d | ${feeLovelace(avg.mpf16o)}%10d | ${feeLovelace(avg.mpf16b)}%10d | ${feeLovelace(avg.mpf64b)}%10d"
            )

        info("")
        info("=== has() average proof size (CBOR bytes) ===")
        val proofHdr =
            f"${"N"}%6s | ${"MPF16o"}%7s | ${"MPF16b"}%7s | ${"MPF64b"}%7s"
        info(proofHdr)
        info("-" * proofHdr.length)
        for (n, avg) <- results do
            info(
              f"${n}%6d | ${avg.proofSize16o}%7d | ${avg.proofSize16b}%7d | ${avg.proofSize64b}%7d"
            )

        info("")
        info("=== has() raw CPU/mem ===")
        val cpuHdr =
            f"${"N"}%6s | ${"MPF16o cpu"}%12s | ${"MPF16o mem"}%10s | ${"MPF16b cpu"}%12s | ${"MPF16b mem"}%10s | ${"MPF64b cpu"}%12s | ${"MPF64b mem"}%10s"
        info(cpuHdr)
        info("-" * cpuHdr.length)
        for (n, avg) <- results do
            info(
              f"${n}%6d | ${avg.mpf16o.steps}%12d | ${avg.mpf16o.memory}%10d | ${avg.mpf16b.steps}%12d | ${avg.mpf16b.memory}%10d | ${avg.mpf64b.steps}%12d | ${avg.mpf64b.memory}%10d"
            )

        saveBudgetResults("has", results)
    }

    test("insert() budget scaling: N=30, 100, 1K, 32K", Benchmark) {
        val results = for n <- collectionSizes yield
            info("")
            info(s"=== Collection size: $n ===")
            n -> runInsertBudget(n, seed = 456)

        info("")
        info("=== insert() budget summary (averages) ===")
        val hdr =
            f"${"N"}%6s | ${"MPF16o fee"}%10s | ${"MPF16b fee"}%10s | ${"MPF64b fee"}%10s"
        info(hdr)
        info("-" * hdr.length)
        for (n, avg) <- results do
            info(
              f"${n}%6d | ${feeLovelace(avg.mpf16o)}%10d | ${feeLovelace(avg.mpf16b)}%10d | ${feeLovelace(avg.mpf64b)}%10d"
            )

        info("")
        info("=== insert() raw CPU/mem ===")
        val cpuHdr2 =
            f"${"N"}%6s | ${"MPF16o cpu"}%12s | ${"MPF16o mem"}%10s | ${"MPF16b cpu"}%12s | ${"MPF16b mem"}%10s | ${"MPF64b cpu"}%12s | ${"MPF64b mem"}%10s"
        info(cpuHdr2)
        info("-" * cpuHdr2.length)
        for (n, avg) <- results do
            info(
              f"${n}%6d | ${avg.mpf16o.steps}%12d | ${avg.mpf16o.memory}%10d | ${avg.mpf16b.steps}%12d | ${avg.mpf16b.memory}%10d | ${avg.mpf64b.steps}%12d | ${avg.mpf64b.memory}%10d"
            )

        saveBudgetResults("insert", results)
    }

    test("32K accumulator budget", Benchmark) {
        val AccN = 32000
        val elems32k = allElements.take(AccN)
        val mpf16o = Mpf16o.fromList(elems32k)
        val mpf16b = Mpf16b.fromList(elems32k)
        val mpf64b = Mpf64b.fromList(elems32k)

        val sampleIndices =
            new scala.util.Random(123).shuffle((0 until AccN).toList).take(SampleSize)

        // Average MPF has() for comparison
        var totalMpf16o = ExUnits(0, 0)
        var totalMpf16b = ExUnits(0, 0)
        var totalMpf64b = ExUnits(0, 0)
        for idx <- sampleIndices do
            val (key, value) = elems32k(idx)
            val budgetO = measureTrieOp(
              mpf16oHasProgram,
              mpf16o.rootHash,
              key,
              value,
              mpf16oProofToData(mpf16o.proveMembership(key))
            )
            totalMpf16o = ExUnits(
              totalMpf16o.memory + budgetO.memory,
              totalMpf16o.steps + budgetO.steps
            )
            val budgetB = measureTrieOp(
              mpf16bHasProgram,
              mpf16b.rootHash,
              key,
              value,
              B(mpf16b.proveMembership(key))
            )
            totalMpf16b = ExUnits(
              totalMpf16b.memory + budgetB.memory,
              totalMpf16b.steps + budgetB.steps
            )
            val budget64 = measureTrieOp(
              mpf64bHasProgram,
              mpf64b.rootHash,
              key,
              value,
              B(mpf64b.proveMembership(key))
            )
            totalMpf64b = ExUnits(
              totalMpf64b.memory + budget64.memory,
              totalMpf64b.steps + budget64.steps
            )
        val avgMpf16o = ExUnits(totalMpf16o.memory / SampleSize, totalMpf16o.steps / SampleSize)
        val avgMpf16b = ExUnits(totalMpf16b.memory / SampleSize, totalMpf16b.steps / SampleSize)
        val avgMpf64b = ExUnits(totalMpf64b.memory / SampleSize, totalMpf64b.steps / SampleSize)

        // Accumulator
        val accElems32k = accElements.take(AccN)
        val accumulator = {
            val t0 = System.nanoTime()
            val acc = accumulateG1(accSetup, accElems32k)
            val ms = (System.nanoTime() - t0) / 1_000_000
            info(s"G1 accumulator built in ${ms} ms ($AccN elements)")
            acc
        }

        val sampleElement = accElems32k(sampleIndices.head)
        val t0 = System.nanoTime()
        val accProof = membershipProofG1(accSetup, accElems32k, Vector(sampleElement))
        val proofMs = (System.nanoTime() - t0) / 1_000_000
        info(s"Membership proof generated in ${proofMs} ms")

        assert(verifyMembershipG1(accSetup, accumulator, Vector(sampleElement), accProof))

        val g2_0 = ceremony.g2Monomial(0)
        val g2_1 = ceremony.g2Monomial(1)

        val accFullApplied = accFullProgram.program.term $
            Term.Const(Constant.BLS12_381_G2_Element(g2_0)) $
            Term.Const(Constant.BLS12_381_G2_Element(g2_1)) $
            Term.Const(Constant.BLS12_381_G1_Element(accumulator)) $
            sampleElement.asTerm $
            Term.Const(Constant.BLS12_381_G1_Element(accProof))

        val accFullBudget = accFullApplied.evaluateDebug match
            case Result.Success(_, exunits, _, _) => exunits
            case Result.Failure(ex, _, _, _) =>
                fail(s"accumulator full check failed: ${ex.getMessage}")

        val commitment = scalus.cardano.onchain.plutus.crypto.accumulator.Poly
            .getG2Commitment(PList(g2_0, g2_1), PList(sampleElement))

        val accPairingApplied = accPairingProgram.program.term $
            Term.Const(Constant.BLS12_381_G1_Element(accumulator)) $
            Term.Const(Constant.BLS12_381_G2_Element(g2_0)) $
            Term.Const(Constant.BLS12_381_G1_Element(accProof)) $
            Term.Const(Constant.BLS12_381_G2_Element(commitment))

        val accPairingBudget = accPairingApplied.evaluateDebug match
            case Result.Success(_, exunits, _, _) => exunits
            case Result.Failure(ex, _, _, _) =>
                fail(s"accumulator pairing check failed: ${ex.getMessage}")

        info("")
        info("=== Summary: Single-element membership in 32K collection ===")
        info(
          f"  MPF16o average:        mem=${avgMpf16o.memory}%10d  cpu=${avgMpf16o.steps}%14d  fee=${feeLovelace(avgMpf16o)}%,8d lovelace"
        )
        info(
          f"  MPF16b average:        mem=${avgMpf16b.memory}%10d  cpu=${avgMpf16b.steps}%14d  fee=${feeLovelace(avgMpf16b)}%,8d lovelace"
        )
        info(
          f"  MPF64b average:        mem=${avgMpf64b.memory}%10d  cpu=${avgMpf64b.steps}%14d  fee=${feeLovelace(avgMpf64b)}%,8d lovelace"
        )
        info(
          f"  Accumulator (full):    mem=${accFullBudget.memory}%10d  cpu=${accFullBudget.steps}%14d  fee=${feeLovelace(accFullBudget)}%,8d lovelace"
        )
        info(
          f"  Accumulator (pairing): mem=${accPairingBudget.memory}%10d  cpu=${accPairingBudget.steps}%14d  fee=${feeLovelace(accPairingBudget)}%,8d lovelace"
        )
        info(
          f"  MPF16b/MPF16o ratio:   cpu=${avgMpf16b.steps.toDouble / avgMpf16o.steps}%.3f"
        )
        info(
          f"  MPF64b/MPF16b ratio:   cpu=${avgMpf64b.steps.toDouble / avgMpf16b.steps}%.3f"
        )
        info(
          f"  Acc(full)/MPF16b ratio: mem=${accFullBudget.memory.toDouble / avgMpf16b.memory}%.3f  cpu=${accFullBudget.steps.toDouble / avgMpf16b.steps}%.3f"
        )
    }
}
