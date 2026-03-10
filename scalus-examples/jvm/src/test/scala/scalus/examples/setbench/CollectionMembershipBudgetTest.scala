package scalus.examples.setbench

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}
import scalus.crypto.trie.FusedMerklePatriciaForestry as Mpf16b
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
        proofSize16o: Int = 0,
        proofSize16b: Int = 0
    )

    /** Build tries of the given size, sample elements, measure has() budgets, print table, return
      * averages.
      */
    private def runHasBudget(n: Int, seed: Int): AvgBudget = {
        val elems = allElements.take(n)
        val trie16o = Mpf16o.fromList(elems)
        val trie16b = Mpf16b.fromList(elems)

        val sampleSize = math.min(SampleSize, n)
        val sampleIndices =
            new scala.util.Random(seed).shuffle((0 until n).toList).take(sampleSize)

        var total16o = ExUnits(0, 0)
        var total16b = ExUnits(0, 0)
        var totalProof16o = 0L
        var totalProof16b = 0L

        for idx <- sampleIndices do
            val (key, value) = elems(idx)

            val proof16oData = mpf16oProofToData(trie16o.proveMembership(key))
            val proof16bData = B(trie16b.proveMembership(key))

            totalProof16o += proof16oData.toCbor.length
            totalProof16b += proof16bData.toCbor.length

            val b16o = measureTrieOp(mpf16oHasProgram, trie16o.rootHash, key, value, proof16oData)
            val b16b = measureTrieOp(mpf16bHasProgram, trie16b.rootHash, key, value, proof16bData)

            total16o = ExUnits(total16o.memory + b16o.memory, total16o.steps + b16o.steps)
            total16b = ExUnits(total16b.memory + b16b.memory, total16b.steps + b16b.steps)

        val avg16o = ExUnits(total16o.memory / sampleSize, total16o.steps / sampleSize)
        val avg16b = ExUnits(total16b.memory / sampleSize, total16b.steps / sampleSize)
        val avgP16o = (totalProof16o / sampleSize).toInt
        val avgP16b = (totalProof16b / sampleSize).toInt

        info(f"  MPF-16o: cpu=${avg16o.steps}%,14d  mem=${avg16o.memory}%,10d  fee=${feeLovelace(avg16o)}%,8d  proof=${avgP16o}%4dB")
        info(f"  MPF-16b: cpu=${avg16b.steps}%,14d  mem=${avg16b.memory}%,10d  fee=${feeLovelace(avg16b)}%,8d  proof=${avgP16b}%4dB")
        info(f"  MPF-16b/16o cpu: ${total16b.steps.toDouble / total16o.steps}%.3f")

        AvgBudget(avg16o, avg16b, avgP16o, avgP16b)
    }

    /** Build tries of the given size, sample elements, measure insert() budgets, print table,
      * return averages.
      */
    private def runInsertBudget(n: Int, seed: Int): AvgBudget = {
        val elems = allElements.take(n)
        val trie16o = Mpf16o.fromList(elems)
        val trie16b = Mpf16b.fromList(elems)

        val sampleSize = math.min(SampleSize, n)
        val sampleIndices =
            new scala.util.Random(seed).shuffle((0 until n).toList).take(sampleSize)

        var total16o = ExUnits(0, 0)
        var total16b = ExUnits(0, 0)

        for idx <- sampleIndices do
            val (key, value) = elems(idx)
            val w16o = trie16o.delete(key)
            val w16b = trie16b.delete(key)

            val b16o = measureTrieOp(mpf16oInsertProgram, w16o.rootHash, key, value, mpf16oProofToData(w16o.proveNonMembership(key)))
            val b16b = measureTrieOp(mpf16bInsertProgram, w16b.rootHash, key, value, B(w16b.proveNonMembership(key)))

            total16o = ExUnits(total16o.memory + b16o.memory, total16o.steps + b16o.steps)
            total16b = ExUnits(total16b.memory + b16b.memory, total16b.steps + b16b.steps)

        val avg16o = ExUnits(total16o.memory / sampleSize, total16o.steps / sampleSize)
        val avg16b = ExUnits(total16b.memory / sampleSize, total16b.steps / sampleSize)

        info(f"  MPF-16o: cpu=${avg16o.steps}%,14d  mem=${avg16o.memory}%,10d  fee=${feeLovelace(avg16o)}%,8d")
        info(f"  MPF-16b: cpu=${avg16b.steps}%,14d  mem=${avg16b.memory}%,10d  fee=${feeLovelace(avg16b)}%,8d")
        info(f"  MPF-16b/16o cpu: ${total16b.steps.toDouble / total16o.steps}%.3f")

        AvgBudget(avg16o, avg16b)
    }

    private def saveBudgetResults(op: String, results: Seq[(Int, AvgBudget)]): Unit = {
        val outDir = new java.io.File("target/bench-results")
        outDir.mkdirs()
        val timestamp = java.time.LocalDateTime.now().toString.replace(":", "-")
        val outFile = new java.io.File(outDir, s"budget-$op-$timestamp.json")
        def j(name: String, eu: ExUnits, proof: Int) =
            s""""$name":{"cpu":${eu.steps},"mem":${eu.memory},"fee":${feeLovelace(eu)},"proof":$proof}"""
        val json = results
            .map { (n, avg) =>
                s"""  {"n":$n,${j("mpf16o", avg.mpf16o, avg.proofSize16o)},${j("mpf16b", avg.mpf16b, avg.proofSize16b)}}"""
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

        printBudgetSummary("has()", results)
        saveBudgetResults("has", results)
    }

    test("insert() budget scaling: N=30, 100, 1K, 32K", Benchmark) {
        val results = for n <- collectionSizes yield
            info("")
            info(s"=== Collection size: $n ===")
            n -> runInsertBudget(n, seed = 456)

        printBudgetSummary("insert()", results)
        saveBudgetResults("insert", results)
    }

    private def printBudgetSummary(op: String, results: Seq[(Int, AvgBudget)]): Unit = {
        info("")
        info(s"=== $op budget summary: CPU / mem / fee / proof ===")
        val variants = Seq(
          ("MPF-16o", (a: AvgBudget) => a.mpf16o, (a: AvgBudget) => a.proofSize16o),
          ("MPF-16b", (a: AvgBudget) => a.mpf16b, (a: AvgBudget) => a.proofSize16b),
        )
        val hdr = f"${"N"}%6s | ${"Variant"}%-8s | ${"CPU"}%14s | ${"Memory"}%10s | ${"Fee"}%8s | ${"Proof (B)"}%10s"
        info(hdr)
        info("-" * hdr.length)
        for (n, avg) <- results do
            for (name, euFn, proofFn) <- variants do
                val eu = euFn(avg)
                info(f"${n}%6d | ${name}%-8s | ${eu.steps}%,14d | ${eu.memory}%,10d | ${feeLovelace(eu)}%,8d | ${proofFn(avg)}%10d")
    }

    test("32K accumulator budget", Benchmark) {
        val AccN = 32000
        val elems32k = allElements.take(AccN)
        val mpf16o = Mpf16o.fromList(elems32k)
        val mpf16b = Mpf16b.fromList(elems32k)

        val sampleIndices =
            new scala.util.Random(123).shuffle((0 until AccN).toList).take(SampleSize)

        // Average MPF has() for comparison
        var totalMpf16o = ExUnits(0, 0)
        var totalMpf16b = ExUnits(0, 0)
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
        val avgMpf16o = ExUnits(totalMpf16o.memory / SampleSize, totalMpf16o.steps / SampleSize)
        val avgMpf16b = ExUnits(totalMpf16b.memory / SampleSize, totalMpf16b.steps / SampleSize)

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
          f"  Accumulator (full):    mem=${accFullBudget.memory}%10d  cpu=${accFullBudget.steps}%14d  fee=${feeLovelace(accFullBudget)}%,8d lovelace"
        )
        info(
          f"  Accumulator (pairing): mem=${accPairingBudget.memory}%10d  cpu=${accPairingBudget.steps}%14d  fee=${feeLovelace(accPairingBudget)}%,8d lovelace"
        )
        info(
          f"  MPF16b/MPF16o ratio:   cpu=${avgMpf16b.steps.toDouble / avgMpf16o.steps}%.3f"
        )
        info(
          f"  Acc(full)/MPF16b ratio: mem=${accFullBudget.memory.toDouble / avgMpf16b.memory}%.3f  cpu=${accFullBudget.steps.toDouble / avgMpf16b.steps}%.3f"
        )
    }
}
