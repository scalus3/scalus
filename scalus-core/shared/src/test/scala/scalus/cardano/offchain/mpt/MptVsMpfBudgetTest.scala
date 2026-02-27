package scalus.cardano.offchain.mpt

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.offchain.mpf.MerklePatriciaForestry as Mpf16
import scalus.cardano.offchain.mpf256.MerklePatriciaForestry as Mpf256
import scalus.cardano.offchain.mpfo.MerklePatriciaForestry as Mpf16o
import scalus.cardano.offchain.mpt.MerklePatriciaTrie as Mpt2
import scalus.cardano.offchain.mpq.MerklePatriciaQuad as Mpq4
import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry.{Proof as Mpf16Proof, ProofStep as Mpf16Step}
import scalus.cardano.onchain.plutus.mpf256.MerklePatriciaForestry.{Proof as Mpf256Proof, ProofStep as Mpf256Step}
import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry.{Proof as Mpf16oProof, ProofStep as Mpf16oStep}
import scalus.cardano.onchain.plutus.mpt.MerklePatriciaTrie.{Proof as Mpt2Proof, ProofStep as Mpt2Step}
import scalus.cardano.onchain.plutus.mpq.MerklePatriciaQuad.{Proof as Mpq4Proof, ProofStep as Mpq4Step}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{B, Constr, I}
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.PlutusV3
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.cardano.ledger.ExUnits

/** Budget comparison test for Radix-16 MPF vs MPF16o (original) vs Radix-2 MPT vs Radix-4 MPQ on
  * the same fruit dataset.
  */
class MptVsMpfBudgetTest extends AnyFunSuite { // scalastyle:ignore

    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = false,
      optimizeUplc = true,
      debug = false
    )

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val fruitEntries: Seq[(String, String)] = Seq(
      "apple[uid: 58]" -> "🍎",
      "apricot[uid: 0]" -> "🤷",
      "banana[uid: 218]" -> "🍌",
      "blueberry[uid: 0]" -> "🫐",
      "cherry[uid: 0]" -> "🍒",
      "coconut[uid: 0]" -> "🥥",
      "cranberry[uid: 0]" -> "🤷",
      "fig[uid: 68267]" -> "🤷",
      "grapefruit[uid: 0]" -> "🤷",
      "grapes[uid: 0]" -> "🍇",
      "guava[uid: 344]" -> "🤷",
      "kiwi[uid: 0]" -> "🥝",
      "kumquat[uid: 0]" -> "🤷",
      "lemon[uid: 0]" -> "🍋",
      "lime[uid: 0]" -> "🤷",
      "mango[uid: 0]" -> "🥭",
      "orange[uid: 0]" -> "🍊",
      "papaya[uid: 0]" -> "🤷",
      "passionfruit[uid: 0]" -> "🤷",
      "peach[uid: 0]" -> "🍑",
      "pear[uid: 0]" -> "🍐",
      "pineapple[uid: 12577]" -> "🍍",
      "plum[uid: 15492]" -> "🤷",
      "pomegranate[uid: 0]" -> "🤷",
      "raspberry[uid: 0]" -> "🤷",
      "strawberry[uid: 2532]" -> "🍓",
      "tangerine[uid: 11]" -> "🍊",
      "tomato[uid: 83468]" -> "🍅",
      "watermelon[uid: 0]" -> "🍉",
      "yuzu[uid: 0]" -> "🤷"
    )

    private val fruitBs: Seq[(ByteString, ByteString)] = fruitEntries.map { case (k, v) =>
        (ByteString.fromString(k), ByteString.fromString(v))
    }

    private val mpf16 = Mpf16.fromList(fruitBs)
    private val mpf256 = Mpf256.fromList(fruitBs)
    private val mpf16o = Mpf16o.fromList(fruitBs)
    private val mpt2 = Mpt2.fromList(fruitBs)
    private val mpq4 = Mpq4.fromList(fruitBs)

    // --- Proof analysis helpers ---

    private case class ProofStats(steps: Int, totalBytes: Int)

    private def analyzeMpf16Proof(proof: Mpf16Proof): ProofStats = {
        val steps = proof.toScalaList
        val totalBytes = steps.map {
            case Mpf16Step.Branch(_, neighbors) => lengthOfByteString(neighbors).toInt
            case Mpf16Step.Fork(_, neighbor) =>
                8 + 1 + 32 // nibble (8 bytes in Data) + prefixLen (1 byte encoding) + root (32 bytes)
            case Mpf16Step.Leaf(_, key, value) =>
                lengthOfByteString(key).toInt + lengthOfByteString(value).toInt
        }.sum
        ProofStats(steps.size, totalBytes)
    }

    private def analyzeMpf256Proof(proof: Mpf256Proof): ProofStats = {
        val steps = proof.toScalaList
        val totalBytes = steps.map {
            case Mpf256Step.Branch(_, neighbors) => lengthOfByteString(neighbors).toInt
            case Mpf256Step.Fork(_, neighbor) =>
                8 + 1 + 32 // index (8 bytes in Data) + prefixLen (1 byte encoding) + root (32 bytes)
            case Mpf256Step.Leaf(_, key, value) =>
                lengthOfByteString(key).toInt + lengthOfByteString(value).toInt
        }.sum
        ProofStats(steps.size, totalBytes)
    }

    private def analyzeMpf16oProof(proof: Mpf16oProof): ProofStats = {
        val steps = proof.toScalaList
        val totalBytes = steps.map {
            case Mpf16oStep.Branch(_, neighbors) => lengthOfByteString(neighbors).toInt
            case Mpf16oStep.Fork(_, neighbor) =>
                8 + lengthOfByteString(
                  neighbor.prefix
                ).toInt + 32 // nibble (8 bytes in Data) + prefix bytes + root (32 bytes)
            case Mpf16oStep.Leaf(_, key, value) =>
                lengthOfByteString(key).toInt + lengthOfByteString(value).toInt
        }.sum
        ProofStats(steps.size, totalBytes)
    }

    private def analyzeMpt2Proof(proof: Mpt2Proof): ProofStats = {
        val steps = proof.toScalaList
        val totalBytes = steps.map {
            case Mpt2Step.Fork(_, neighborSkipLen, neighborRoot) =>
                1 + lengthOfByteString(neighborRoot).toInt // 1 byte for skip len encoding
            case Mpt2Step.Leaf(_, key, value) =>
                lengthOfByteString(key).toInt + lengthOfByteString(value).toInt
        }.sum
        ProofStats(steps.size, totalBytes)
    }

    private def analyzeMpq4Proof(proof: Mpq4Proof): ProofStats = {
        val steps = proof.toScalaList
        val totalBytes = steps.map {
            case Mpq4Step.Branch(_, neighbors) => lengthOfByteString(neighbors).toInt
            case Mpq4Step.Fork(_, neighbor) =>
                8 + 1 + 32 // dibit (8 bytes in Data) + prefixLen (1 byte encoding) + root (32 bytes)
            case Mpq4Step.Leaf(_, key, value) =>
                lengthOfByteString(key).toInt + lengthOfByteString(value).toInt
        }.sum
        ProofStats(steps.size, totalBytes)
    }

    // --- Data encoding helpers for UPLC budget measurement ---

    private def mpf16StepToData(step: Mpf16Step): Data = step match
        case Mpf16Step.Branch(skip, neighbors) =>
            Constr(0, PList(I(skip), B(neighbors)))
        case Mpf16Step.Fork(skip, neighbor) =>
            val neighborData =
                Constr(0, PList(I(neighbor.nibble), I(neighbor.prefixLen), B(neighbor.halfLeft), B(neighbor.halfRight)))
            Constr(1, PList(I(skip), neighborData))
        case Mpf16Step.Leaf(skip, key, value) =>
            Constr(2, PList(I(skip), B(key), B(value)))

    private def mpf16ProofToData(proof: Mpf16Proof): Data = {
        // List[ProofStep] is encoded as Data.List (via listToData), not nested Constr
        val steps = proof.toScalaList.map(mpf16StepToData)
        Data.List(steps.foldRight(PList.Nil: PList[Data]) { (d, acc) => PList.Cons(d, acc) })
    }

    private def mpf256StepToData(step: Mpf256Step): Data = step match
        case Mpf256Step.Branch(skip, neighbors) =>
            Constr(0, PList(I(skip), B(neighbors)))
        case Mpf256Step.Fork(skip, neighbor) =>
            val neighborData =
                Constr(0, PList(I(neighbor.index), I(neighbor.prefixLen), B(neighbor.root)))
            Constr(1, PList(I(skip), neighborData))
        case Mpf256Step.Leaf(skip, key, value) =>
            Constr(2, PList(I(skip), B(key), B(value)))

    private def mpf256ProofToData(proof: Mpf256Proof): Data = {
        val steps = proof.toScalaList.map(mpf256StepToData)
        Data.List(steps.foldRight(PList.Nil: PList[Data]) { (d, acc) => PList.Cons(d, acc) })
    }

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

    private def mpt2StepToData(step: Mpt2Step): Data = step match
        case Mpt2Step.Fork(skip, neighborSkipLen, neighborRoot) =>
            Constr(0, PList(I(skip), I(neighborSkipLen), B(neighborRoot)))
        case Mpt2Step.Leaf(skip, key, value) =>
            Constr(1, PList(I(skip), B(key), B(value)))

    private def mpt2ProofToData(proof: Mpt2Proof): Data = {
        val steps = proof.toScalaList.map(mpt2StepToData)
        Data.List(steps.foldRight(PList.Nil: PList[Data]) { (d, acc) => PList.Cons(d, acc) })
    }

    private def mpq4StepToData(step: Mpq4Step): Data = step match
        case Mpq4Step.Branch(skip, neighbors) =>
            Constr(0, PList(I(skip), B(neighbors)))
        case Mpq4Step.Fork(skip, neighbor) =>
            val neighborData =
                Constr(0, PList(I(neighbor.dibit), I(neighbor.prefixLen), B(neighbor.root)))
            Constr(1, PList(I(skip), neighborData))
        case Mpq4Step.Leaf(skip, key, value) =>
            Constr(2, PList(I(skip), B(key), B(value)))

    private def mpq4ProofToData(proof: Mpq4Proof): Data = {
        val steps = proof.toScalaList.map(mpq4StepToData)
        Data.List(steps.foldRight(PList.Nil: PList[Data]) { (d, acc) => PList.Cons(d, acc) })
    }

    // --- Compiled UPLC programs ---

    // MPF16: has(key, value, proof) == root
    private val mpf16HasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry
            import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry.*
            val trie = MerklePatriciaForestry(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // MPF256: has(key, value, proof) == root
    private val mpf256HasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpf256.MerklePatriciaForestry
            import scalus.cardano.onchain.plutus.mpf256.MerklePatriciaForestry.*
            val trie = MerklePatriciaForestry(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // MPF16o: has(key, value, proof) == root
    private val mpf16oHasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry
            import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry.*
            val trie = MerklePatriciaForestry(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // MPT2: has(key, value, proof) == root
    private val mpt2HasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpt.MerklePatriciaTrie
            import scalus.cardano.onchain.plutus.mpt.MerklePatriciaTrie.*
            val trie = MerklePatriciaTrie(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // MPQ4: has(key, value, proof) == root
    private val mpq4HasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpq.MerklePatriciaQuad
            import scalus.cardano.onchain.plutus.mpq.MerklePatriciaQuad.*
            val trie = MerklePatriciaQuad(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // MPF16: insert (exercises both excluding and including)
    private val mpf16InsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry
            import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry.*
            val trie = MerklePatriciaForestry(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // MPF256: insert (exercises both excluding and including)
    private val mpf256InsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpf256.MerklePatriciaForestry
            import scalus.cardano.onchain.plutus.mpf256.MerklePatriciaForestry.*
            val trie = MerklePatriciaForestry(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // MPF16o: insert (exercises both excluding and including)
    private val mpf16oInsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry
            import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry.*
            val trie = MerklePatriciaForestry(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // MPT2: insert (exercises both excluding and including)
    private val mpt2InsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpt.MerklePatriciaTrie
            import scalus.cardano.onchain.plutus.mpt.MerklePatriciaTrie.*
            val trie = MerklePatriciaTrie(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // MPQ4: insert (exercises both excluding and including)
    private val mpq4InsertProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.mpq.MerklePatriciaQuad
            import scalus.cardano.onchain.plutus.mpq.MerklePatriciaQuad.*
            val trie = MerklePatriciaQuad(unBData(rootD))
            trie.insert(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    private def measureHas(
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
            case Result.Failure(ex, _, _, _)      => fail(s"has() failed: ${ex.getMessage}")
    }

    private def measureInsert(
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
            case Result.Failure(ex, _, _, _)      => fail(s"insert() failed: ${ex.getMessage}")
    }

    test("proof structure comparison: MPF16 vs MPF256 vs MPF16o vs MPT2 vs MPQ4") {
        val header =
            f"${"Key"}%-30s | ${"MPF16 steps"}%12s | ${"MPF16 bytes"}%12s | ${"MPF256 steps"}%13s | ${"MPF256 bytes"}%13s | ${"MPF16o steps"}%13s | ${"MPF16o bytes"}%13s | ${"MPT2 steps"}%12s | ${"MPT2 bytes"}%12s | ${"MPQ4 steps"}%12s | ${"MPQ4 bytes"}%12s"
        val sep = "-" * header.length
        info(header)
        info(sep)

        var totalMpf16Steps = 0
        var totalMpf16Bytes = 0
        var totalMpf256Steps = 0
        var totalMpf256Bytes = 0
        var totalMpf16oSteps = 0
        var totalMpf16oBytes = 0
        var totalMpt2Steps = 0
        var totalMpt2Bytes = 0
        var totalMpq4Steps = 0
        var totalMpq4Bytes = 0

        for ((key, _), (name, _)) <- fruitBs.zip(fruitEntries) do
            val mpf16Stats = analyzeMpf16Proof(mpf16.proveExists(key))
            val mpf256Stats = analyzeMpf256Proof(mpf256.proveExists(key))
            val mpf16oStats = analyzeMpf16oProof(mpf16o.proveExists(key))
            val mpt2Stats = analyzeMpt2Proof(mpt2.proveExists(key))
            val mpq4Stats = analyzeMpq4Proof(mpq4.proveExists(key))

            totalMpf16Steps += mpf16Stats.steps
            totalMpf16Bytes += mpf16Stats.totalBytes
            totalMpf256Steps += mpf256Stats.steps
            totalMpf256Bytes += mpf256Stats.totalBytes
            totalMpf16oSteps += mpf16oStats.steps
            totalMpf16oBytes += mpf16oStats.totalBytes
            totalMpt2Steps += mpt2Stats.steps
            totalMpt2Bytes += mpt2Stats.totalBytes
            totalMpq4Steps += mpq4Stats.steps
            totalMpq4Bytes += mpq4Stats.totalBytes

            info(
              f"${name.take(30)}%-30s | ${mpf16Stats.steps}%12d | ${mpf16Stats.totalBytes}%12d | ${mpf256Stats.steps}%13d | ${mpf256Stats.totalBytes}%13d | ${mpf16oStats.steps}%13d | ${mpf16oStats.totalBytes}%13d | ${mpt2Stats.steps}%12d | ${mpt2Stats.totalBytes}%12d | ${mpq4Stats.steps}%12d | ${mpq4Stats.totalBytes}%12d"
            )

        info(sep)
        info(
          f"${"TOTAL"}%-30s | $totalMpf16Steps%12d | $totalMpf16Bytes%12d | $totalMpf256Steps%13d | $totalMpf256Bytes%13d | $totalMpf16oSteps%13d | $totalMpf16oBytes%13d | $totalMpt2Steps%12d | $totalMpt2Bytes%12d | $totalMpq4Steps%12d | $totalMpq4Bytes%12d"
        )
        info(
          f"${"AVERAGE"}%-30s | ${totalMpf16Steps.toDouble / 30}%12.1f | ${totalMpf16Bytes.toDouble / 30}%12.1f | ${totalMpf256Steps.toDouble / 30}%13.1f | ${totalMpf256Bytes.toDouble / 30}%13.1f | ${totalMpf16oSteps.toDouble / 30}%13.1f | ${totalMpf16oBytes.toDouble / 30}%13.1f | ${totalMpt2Steps.toDouble / 30}%12.1f | ${totalMpt2Bytes.toDouble / 30}%12.1f | ${totalMpq4Steps.toDouble / 30}%12.1f | ${totalMpq4Bytes.toDouble / 30}%12.1f"
        )
    }

    test("on-chain cross-verification: all systems verify same data") {
        val mpf16OnChain = mpf16.toOnChain
        val mpf256OnChain = mpf256.toOnChain
        val mpf16oOnChain = mpf16o.toOnChain
        val mpt2OnChain = mpt2.toOnChain
        val mpq4OnChain = mpq4.toOnChain

        for (key, value) <- fruitBs do
            val mpf16Proof = mpf16.proveExists(key)
            assert(mpf16OnChain.has(key, value, mpf16Proof), s"MPF16 has() failed")

            val mpf256Proof = mpf256.proveExists(key)
            assert(mpf256OnChain.has(key, value, mpf256Proof), s"MPF256 has() failed")

            val mpf16oProof = mpf16o.proveExists(key)
            assert(mpf16oOnChain.has(key, value, mpf16oProof), s"MPF16o has() failed")

            val mpt2Proof = mpt2.proveExists(key)
            assert(mpt2OnChain.has(key, value, mpt2Proof), s"MPT2 has() failed")

            val mpq4Proof = mpq4.proveExists(key)
            assert(mpq4OnChain.has(key, value, mpq4Proof), s"MPQ4 has() failed")

            // Exclusion cross-verification (delete + reinsert)
            val mpf16Inserted =
                mpf16.delete(key).toOnChain.insert(key, value, mpf16.delete(key).proveMissing(key))
            assert(mpf16Inserted.root == mpf16.rootHash, s"MPF16 insert failed")

            val mpf256Inserted =
                mpf256
                    .delete(key)
                    .toOnChain
                    .insert(key, value, mpf256.delete(key).proveMissing(key))
            assert(mpf256Inserted.root == mpf256.rootHash, s"MPF256 insert failed")

            val mpf16oInserted =
                mpf16o
                    .delete(key)
                    .toOnChain
                    .insert(key, value, mpf16o.delete(key).proveMissing(key))
            assert(mpf16oInserted.root == mpf16o.rootHash, s"MPF16o insert failed")

            val mpt2Inserted =
                mpt2.delete(key).toOnChain.insert(key, value, mpt2.delete(key).proveMissing(key))
            assert(mpt2Inserted.root == mpt2.rootHash, s"MPT2 insert failed")

            val mpq4Inserted =
                mpq4.delete(key).toOnChain.insert(key, value, mpq4.delete(key).proveMissing(key))
            assert(mpq4Inserted.root == mpq4.rootHash, s"MPQ4 insert failed")
    }

    test("UPLC budget comparison: has (inclusion proof)") {
        val header =
            f"${"Key"}%-30s | ${"MPF16 mem"}%10s | ${"MPF16 cpu"}%14s | ${"MPF256 mem"}%11s | ${"MPF256 cpu"}%14s | ${"MPF16o mem"}%11s | ${"MPF16o cpu"}%14s | ${"MPT2 mem"}%10s | ${"MPT2 cpu"}%14s | ${"MPQ4 mem"}%10s | ${"MPQ4 cpu"}%14s"
        val sep = "-" * header.length
        info(header)
        info(sep)

        var totalMpf16 = ExUnits(0, 0)
        var totalMpf256 = ExUnits(0, 0)
        var totalMpf16o = ExUnits(0, 0)
        var totalMpt2 = ExUnits(0, 0)
        var totalMpq4 = ExUnits(0, 0)

        for ((key, value), (name, _)) <- fruitBs.zip(fruitEntries) do
            val mpf16Budget = measureHas(
              mpf16HasProgram,
              mpf16.rootHash,
              key,
              value,
              mpf16ProofToData(mpf16.proveExists(key))
            )
            val mpf256Budget = measureHas(
              mpf256HasProgram,
              mpf256.rootHash,
              key,
              value,
              mpf256ProofToData(mpf256.proveExists(key))
            )
            val mpf16oBudget = measureHas(
              mpf16oHasProgram,
              mpf16o.rootHash,
              key,
              value,
              mpf16oProofToData(mpf16o.proveExists(key))
            )
            val mpt2Budget = measureHas(
              mpt2HasProgram,
              mpt2.rootHash,
              key,
              value,
              mpt2ProofToData(mpt2.proveExists(key))
            )
            val mpq4Budget = measureHas(
              mpq4HasProgram,
              mpq4.rootHash,
              key,
              value,
              mpq4ProofToData(mpq4.proveExists(key))
            )

            totalMpf16 = ExUnits(
              totalMpf16.memory + mpf16Budget.memory,
              totalMpf16.steps + mpf16Budget.steps
            )
            totalMpf256 = ExUnits(
              totalMpf256.memory + mpf256Budget.memory,
              totalMpf256.steps + mpf256Budget.steps
            )
            totalMpf16o = ExUnits(
              totalMpf16o.memory + mpf16oBudget.memory,
              totalMpf16o.steps + mpf16oBudget.steps
            )
            totalMpt2 =
                ExUnits(totalMpt2.memory + mpt2Budget.memory, totalMpt2.steps + mpt2Budget.steps)
            totalMpq4 =
                ExUnits(totalMpq4.memory + mpq4Budget.memory, totalMpq4.steps + mpq4Budget.steps)

            info(
              f"${name.take(30)}%-30s | ${mpf16Budget.memory}%10d | ${mpf16Budget.steps}%14d | ${mpf256Budget.memory}%11d | ${mpf256Budget.steps}%14d | ${mpf16oBudget.memory}%11d | ${mpf16oBudget.steps}%14d | ${mpt2Budget.memory}%10d | ${mpt2Budget.steps}%14d | ${mpq4Budget.memory}%10d | ${mpq4Budget.steps}%14d"
            )

        info(sep)
        info(
          f"${"TOTAL"}%-30s | ${totalMpf16.memory}%10d | ${totalMpf16.steps}%14d | ${totalMpf256.memory}%11d | ${totalMpf256.steps}%14d | ${totalMpf16o.memory}%11d | ${totalMpf16o.steps}%14d | ${totalMpt2.memory}%10d | ${totalMpt2.steps}%14d | ${totalMpq4.memory}%10d | ${totalMpq4.steps}%14d"
        )
        info(
          f"${"AVERAGE"}%-30s | ${totalMpf16.memory / 30}%10d | ${totalMpf16.steps / 30}%14d | ${totalMpf256.memory / 30}%11d | ${totalMpf256.steps / 30}%14d | ${totalMpf16o.memory / 30}%11d | ${totalMpf16o.steps / 30}%14d | ${totalMpt2.memory / 30}%10d | ${totalMpt2.steps / 30}%14d | ${totalMpq4.memory / 30}%10d | ${totalMpq4.steps / 30}%14d"
        )
        info("")
        info(f"MPF256/MPF16 memory ratio: ${totalMpf256.memory.toDouble / totalMpf16.memory}%.3f")
        info(f"MPF256/MPF16 cpu ratio:    ${totalMpf256.steps.toDouble / totalMpf16.steps}%.3f")
        info(f"MPF16o/MPF16 memory ratio: ${totalMpf16o.memory.toDouble / totalMpf16.memory}%.3f")
        info(f"MPF16o/MPF16 cpu ratio:    ${totalMpf16o.steps.toDouble / totalMpf16.steps}%.3f")
        info(f"MPT2/MPF16 memory ratio:   ${totalMpt2.memory.toDouble / totalMpf16.memory}%.3f")
        info(f"MPT2/MPF16 cpu ratio:      ${totalMpt2.steps.toDouble / totalMpf16.steps}%.3f")
        info(f"MPQ4/MPF16 memory ratio:   ${totalMpq4.memory.toDouble / totalMpf16.memory}%.3f")
        info(f"MPQ4/MPF16 cpu ratio:      ${totalMpq4.steps.toDouble / totalMpf16.steps}%.3f")
    }

    test("UPLC budget comparison: insert (exclusion + inclusion proof)") {
        val header =
            f"${"Key"}%-30s | ${"MPF16 mem"}%10s | ${"MPF16 cpu"}%14s | ${"MPF256 mem"}%11s | ${"MPF256 cpu"}%14s | ${"MPF16o mem"}%11s | ${"MPF16o cpu"}%14s | ${"MPT2 mem"}%10s | ${"MPT2 cpu"}%14s | ${"MPQ4 mem"}%10s | ${"MPQ4 cpu"}%14s"
        val sep = "-" * header.length
        info(header)
        info(sep)

        var totalMpf16 = ExUnits(0, 0)
        var totalMpf256 = ExUnits(0, 0)
        var totalMpf16o = ExUnits(0, 0)
        var totalMpt2 = ExUnits(0, 0)
        var totalMpq4 = ExUnits(0, 0)

        for ((key, value), (name, _)) <- fruitBs.zip(fruitEntries) do
            val mpf16Without = mpf16.delete(key)
            val mpf256Without = mpf256.delete(key)
            val mpf16oWithout = mpf16o.delete(key)
            val mpt2Without = mpt2.delete(key)
            val mpq4Without = mpq4.delete(key)

            val mpf16Budget = measureInsert(
              mpf16InsertProgram,
              mpf16Without.rootHash,
              key,
              value,
              mpf16ProofToData(mpf16Without.proveMissing(key))
            )
            val mpf256Budget = measureInsert(
              mpf256InsertProgram,
              mpf256Without.rootHash,
              key,
              value,
              mpf256ProofToData(mpf256Without.proveMissing(key))
            )
            val mpf16oBudget = measureInsert(
              mpf16oInsertProgram,
              mpf16oWithout.rootHash,
              key,
              value,
              mpf16oProofToData(mpf16oWithout.proveMissing(key))
            )
            val mpt2Budget = measureInsert(
              mpt2InsertProgram,
              mpt2Without.rootHash,
              key,
              value,
              mpt2ProofToData(mpt2Without.proveMissing(key))
            )
            val mpq4Budget = measureInsert(
              mpq4InsertProgram,
              mpq4Without.rootHash,
              key,
              value,
              mpq4ProofToData(mpq4Without.proveMissing(key))
            )

            totalMpf16 = ExUnits(
              totalMpf16.memory + mpf16Budget.memory,
              totalMpf16.steps + mpf16Budget.steps
            )
            totalMpf256 = ExUnits(
              totalMpf256.memory + mpf256Budget.memory,
              totalMpf256.steps + mpf256Budget.steps
            )
            totalMpf16o = ExUnits(
              totalMpf16o.memory + mpf16oBudget.memory,
              totalMpf16o.steps + mpf16oBudget.steps
            )
            totalMpt2 =
                ExUnits(totalMpt2.memory + mpt2Budget.memory, totalMpt2.steps + mpt2Budget.steps)
            totalMpq4 =
                ExUnits(totalMpq4.memory + mpq4Budget.memory, totalMpq4.steps + mpq4Budget.steps)

            info(
              f"${name.take(30)}%-30s | ${mpf16Budget.memory}%10d | ${mpf16Budget.steps}%14d | ${mpf256Budget.memory}%11d | ${mpf256Budget.steps}%14d | ${mpf16oBudget.memory}%11d | ${mpf16oBudget.steps}%14d | ${mpt2Budget.memory}%10d | ${mpt2Budget.steps}%14d | ${mpq4Budget.memory}%10d | ${mpq4Budget.steps}%14d"
            )

        info(sep)
        info(
          f"${"TOTAL"}%-30s | ${totalMpf16.memory}%10d | ${totalMpf16.steps}%14d | ${totalMpf256.memory}%11d | ${totalMpf256.steps}%14d | ${totalMpf16o.memory}%11d | ${totalMpf16o.steps}%14d | ${totalMpt2.memory}%10d | ${totalMpt2.steps}%14d | ${totalMpq4.memory}%10d | ${totalMpq4.steps}%14d"
        )
        info(
          f"${"AVERAGE"}%-30s | ${totalMpf16.memory / 30}%10d | ${totalMpf16.steps / 30}%14d | ${totalMpf256.memory / 30}%11d | ${totalMpf256.steps / 30}%14d | ${totalMpf16o.memory / 30}%11d | ${totalMpf16o.steps / 30}%14d | ${totalMpt2.memory / 30}%10d | ${totalMpt2.steps / 30}%14d | ${totalMpq4.memory / 30}%10d | ${totalMpq4.steps / 30}%14d"
        )
        info("")
        info(f"MPF256/MPF16 memory ratio: ${totalMpf256.memory.toDouble / totalMpf16.memory}%.3f")
        info(f"MPF256/MPF16 cpu ratio:    ${totalMpf256.steps.toDouble / totalMpf16.steps}%.3f")
        info(f"MPF16o/MPF16 memory ratio: ${totalMpf16o.memory.toDouble / totalMpf16.memory}%.3f")
        info(f"MPF16o/MPF16 cpu ratio:    ${totalMpf16o.steps.toDouble / totalMpf16.steps}%.3f")
        info(f"MPT2/MPF16 memory ratio:   ${totalMpt2.memory.toDouble / totalMpf16.memory}%.3f")
        info(f"MPT2/MPF16 cpu ratio:      ${totalMpt2.steps.toDouble / totalMpf16.steps}%.3f")
        info(f"MPQ4/MPF16 memory ratio:   ${totalMpq4.memory.toDouble / totalMpf16.memory}%.3f")
        info(f"MPQ4/MPF16 cpu ratio:      ${totalMpq4.steps.toDouble / totalMpf16.steps}%.3f")
    }
}
