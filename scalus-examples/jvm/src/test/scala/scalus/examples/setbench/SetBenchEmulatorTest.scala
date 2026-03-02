package scalus.examples.setbench

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.offchain.mpfb.MerklePatriciaForestry as Mpf16b
import scalus.cardano.offchain.mpfo.MerklePatriciaForestry as Mpf16o
import scalus.crypto.accumulator.BilinearAccumulatorProver.*
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.{PlutusV3, Program}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.await

/** Benchmark for set membership operations through the emulator, measuring real transaction costs:
  * fee, execution units, tx size, and off-chain proof generation time.
  *
  * Tagged with `scalus.testing.Benchmark` — excluded from default test runs. Run with:
  * {{{
  * sbtn "scalusExamplesJVM/testOnly *SetBenchEmulatorTest -- -n scalus.testing.Benchmark"
  * }}}
  */
class SetBenchEmulatorTest extends AnyFunSuite with ScalusTest {
    import SetBenchEmulatorTest.*

    private given env: CardanoInfo = TestUtil.testEnvironment

    private val SampleSize = 10
    private val K = 2_000_000L

    /** Generate `count` deterministic (key, value) pairs. Uses a fixed seed so results are
      * reproducible, but each call creates a fresh vector of exactly the right size.
      */
    private def generateElements(count: Int): Vector[(ByteString, ByteString)] = {
        val rng = new scala.util.Random(42)
        Vector.tabulate(count) { i =>
            val key = ByteString.fromString(s"element-${rng.nextInt()}-$i")
            val value = ByteString.fromString(s"value-$i")
            (key, value)
        }
    }

    private lazy val ceremony: CollectionMembershipBudgetTest.EthereumCeremony =
        CollectionMembershipBudgetTest.loadCeremony()

    private lazy val accSetup: Setup = {
        val t0 = System.nanoTime()
        val s = Setup.fromPoints(ceremony.g1Monomial.toVector, ceremony.g2Monomial.toVector)
        val ms = (System.nanoTime() - t0) / 1_000_000
        info(s"Accumulator setup created in $ms ms")
        s
    }

    private val txHelper = SetBenchTransactions(env)

    private val allResults = collection.mutable.ArrayBuffer[BenchResult]()

    // --- MPF withdraw benchmark ---

    private def benchMpfWithdraw(
        variant: String,
        n: Int,
        contract: PlutusV3[Data => Unit],
        buildTrie: Vector[(ByteString, ByteString)] => MpfTrie
    ): Unit = {
        val elems = generateElements(n)

        val t0 = System.nanoTime()
        var trie = buildTrie(elems)
        val buildMs = (System.nanoTime() - t0) / 1_000_000
        info(s"$variant trie built in $buildMs ms (N=$n)")

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

        // Publish script as reference UTxO
        val publishTx =
            txHelper.publishScript(aliceUtxos, contract, Bob.address, Alice.address, Alice.signer)
        assert(emulator.submit(publishTx).await().isRight, "Publish script failed")
        val refScriptUtxo = findRefScriptUtxo(publishTx)

        aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get
        val lockAmount = (SampleSize + 5) * K
        val lockTx = txHelper.lock(
          utxos = aliceUtxos,
          contract = contract,
          totalLovelace = lockAmount,
          initialRoot = trie.rootHash,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(emulator.submit(lockTx).await().isRight, "Lock tx failed")

        var contractUtxo = findContractUtxo(lockTx, contract)
        var remaining = lockAmount

        val sampleIndices =
            new scala.util.Random(42).shuffle((0 until n).toList).take(SampleSize)

        var totalFee = 0L
        var totalCpu = 0L
        var totalMem = 0L
        var totalTxSize = 0
        var totalProofSize = 0
        var totalProofMs = 0L

        for (idx, i) <- sampleIndices.zipWithIndex do
            val (key, value) = elems(idx)

            val proofT0 = System.nanoTime()
            val proofData = trie.proveExistsData(key)
            val proofMs = (System.nanoTime() - proofT0) / 1_000_000
            totalProofMs += proofMs
            val proofSize = proofData.toCbor.length

            trie = trie.delete(key)
            val newRoot = trie.rootHash
            remaining -= K
            val newDatum = SetBenchDatum(BigInt(remaining), newRoot)
            val redeemer = SetBenchRedeemer.Withdraw(key, value, proofData).toData

            val sponsorUtxos = emulator.findUtxos(Alice.address).await().toOption.get
            val tx = txHelper.withdraw(
              utxos = sponsorUtxos,
              contractUtxo = contractUtxo,
              refScriptUtxo = refScriptUtxo,
              contract = contract,
              redeemer = redeemer,
              newDatum = newDatum,
              k = K,
              withdrawTo = Bob.address,
              sponsor = Alice.address,
              signer = Alice.signer
            )

            val result = emulator.submit(tx).await()
            assert(result.isRight, s"Withdraw $i failed: $result")

            val (fee, exUnits, txSize) = extractMetrics(tx)
            totalFee += fee
            totalCpu += exUnits.steps
            totalMem += exUnits.memory
            totalTxSize += txSize
            totalProofSize += proofSize

            if i % 10 == 0 || i == SampleSize - 1 then
                info(
                  f"  [$i] fee=$fee%,d cpu=${exUnits.steps}%,d mem=${exUnits.memory}%,d txSize=$txSize proof=$proofSize%,dB ${proofMs}ms"
                )

            contractUtxo = findContractUtxo(tx, contract)

        val avg = BenchResult(
          variant,
          "withdraw",
          n,
          totalFee / SampleSize,
          totalCpu / SampleSize,
          totalMem / SampleSize,
          totalTxSize / SampleSize,
          totalProofSize / SampleSize,
          totalProofMs / SampleSize,
          buildMs
        )
        allResults += avg
        info(
          f"  AVG: fee=${avg.avgFee}%,d cpu=${avg.avgCpu}%,d mem=${avg.avgMem}%,d txSize=${avg.avgTxSize} proof=${avg.avgProofSize}B ${avg.avgProofGenMs}ms build=${avg.buildTimeMs}ms"
        )
    }

    // --- MPF deposit benchmark ---

    private def benchMpfDeposit(
        variant: String,
        n: Int,
        contract: PlutusV3[Data => Unit],
        buildTrie: Vector[(ByteString, ByteString)] => MpfTrie
    ): Unit = {
        val allElems = generateElements(n + SampleSize)
        val elems = allElems.take(n)
        val newElems = allElems.slice(n, n + SampleSize)

        val t0 = System.nanoTime()
        var trie = buildTrie(elems)
        val buildMs = (System.nanoTime() - t0) / 1_000_000
        info(s"$variant trie built in $buildMs ms (N=$n)")

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

        // Publish script as reference UTxO
        val publishTx =
            txHelper.publishScript(aliceUtxos, contract, Bob.address, Alice.address, Alice.signer)
        assert(emulator.submit(publishTx).await().isRight, "Publish script failed")
        val refScriptUtxo = findRefScriptUtxo(publishTx)

        aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get
        val lockAmount = 10 * K
        val lockTx = txHelper.lock(
          utxos = aliceUtxos,
          contract = contract,
          totalLovelace = lockAmount,
          initialRoot = trie.rootHash,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(emulator.submit(lockTx).await().isRight, "Lock tx failed")

        var contractUtxo = findContractUtxo(lockTx, contract)
        var remaining = lockAmount

        var totalFee = 0L
        var totalCpu = 0L
        var totalMem = 0L
        var totalTxSize = 0
        var totalProofSize = 0
        var totalProofMs = 0L

        for (elem, i) <- newElems.zipWithIndex do
            val (key, value) = elem

            val proofT0 = System.nanoTime()
            val proofData = trie.proveMissingData(key)
            val proofMs = (System.nanoTime() - proofT0) / 1_000_000
            totalProofMs += proofMs
            val proofSize = proofData.toCbor.length

            trie = trie.insert(key, value)
            val newRoot = trie.rootHash
            remaining += K
            val newDatum = SetBenchDatum(BigInt(remaining), newRoot)
            val redeemer = SetBenchRedeemer.Deposit(key, value, proofData).toData

            val sponsorUtxos = emulator.findUtxos(Alice.address).await().toOption.get
            val tx = txHelper.deposit(
              utxos = sponsorUtxos,
              contractUtxo = contractUtxo,
              refScriptUtxo = refScriptUtxo,
              contract = contract,
              redeemer = redeemer,
              newDatum = newDatum,
              k = K,
              sponsor = Alice.address,
              signer = Alice.signer
            )

            val result = emulator.submit(tx).await()
            assert(result.isRight, s"Deposit $i failed: $result")

            val (fee, exUnits, txSize) = extractMetrics(tx)
            totalFee += fee
            totalCpu += exUnits.steps
            totalMem += exUnits.memory
            totalTxSize += txSize
            totalProofSize += proofSize

            if i % 10 == 0 || i == SampleSize - 1 then
                info(
                  f"  [$i] fee=$fee%,d cpu=${exUnits.steps}%,d mem=${exUnits.memory}%,d txSize=$txSize proof=$proofSize%,dB ${proofMs}ms"
                )

            contractUtxo = findContractUtxo(tx, contract)

        val avg = BenchResult(
          variant,
          "deposit",
          n,
          totalFee / SampleSize,
          totalCpu / SampleSize,
          totalMem / SampleSize,
          totalTxSize / SampleSize,
          totalProofSize / SampleSize,
          totalProofMs / SampleSize,
          buildMs
        )
        allResults += avg
        info(
          f"  AVG: fee=${avg.avgFee}%,d cpu=${avg.avgCpu}%,d mem=${avg.avgMem}%,d txSize=${avg.avgTxSize} proof=${avg.avgProofSize}B ${avg.avgProofGenMs}ms build=${avg.buildTimeMs}ms"
        )
    }

    // --- Accumulator withdraw benchmark ---

    private def benchAccWithdraw(n: Int): Unit = {
        val variant = "Acc (G1)"
        val contract = AccContract.withErrorTraces
        val accElems = generateElements(n).map { (k, _) =>
            byteStringToInteger(true, blake2b_256(k))
        }

        val t0 = System.nanoTime()
        var fullSet = accElems
        val accumulator = accumulateG1(accSetup, fullSet)
        val buildMs = (System.nanoTime() - t0) / 1_000_000
        info(s"$variant accumulator built in $buildMs ms (N=$n)")

        val compressedAcc = bls12_381_G1_compress(accumulator)

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

        // Publish script as reference UTxO
        val publishTx =
            txHelper.publishScript(aliceUtxos, contract, Bob.address, Alice.address, Alice.signer)
        assert(emulator.submit(publishTx).await().isRight, "Publish script failed")
        val refScriptUtxo = findRefScriptUtxo(publishTx)

        aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get
        val lockAmount = (SampleSize + 5) * K
        val lockTx = txHelper.lock(
          utxos = aliceUtxos,
          contract = contract,
          totalLovelace = lockAmount,
          initialRoot = compressedAcc,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(emulator.submit(lockTx).await().isRight, "Lock tx failed")

        var contractUtxo = findContractUtxo(lockTx, contract)
        var remaining = lockAmount

        val sampleIndices =
            new scala.util.Random(42).shuffle((0 until n).toList).take(SampleSize)

        var totalFee = 0L
        var totalCpu = 0L
        var totalMem = 0L
        var totalTxSize = 0
        var totalProofSize = 0
        var totalProofMs = 0L

        for (idx, i) <- sampleIndices.zipWithIndex do
            val element = fullSet(idx)

            val proofT0 = System.nanoTime()
            val proof = membershipProofG1(accSetup, fullSet, Vector(element))
            val proofMs = (System.nanoTime() - proofT0) / 1_000_000
            totalProofMs += proofMs

            val compressedProof = bls12_381_G1_compress(proof)
            val proofSize = 48 // BLS G1 compressed point is always 48 bytes
            val newRoot = compressedProof // proof IS the new accumulator after removal
            remaining -= K

            // Remove element from full set for subsequent proofs
            fullSet = fullSet.patch(idx, Nil, 1)

            val newDatum = SetBenchDatum(BigInt(remaining), newRoot)
            val redeemer = AccWithdrawRedeemer(element, compressedProof).toData

            val sponsorUtxos = emulator.findUtxos(Alice.address).await().toOption.get
            val tx = txHelper.withdraw(
              utxos = sponsorUtxos,
              contractUtxo = contractUtxo,
              refScriptUtxo = refScriptUtxo,
              contract = contract,
              redeemer = redeemer,
              newDatum = newDatum,
              k = K,
              withdrawTo = Bob.address,
              sponsor = Alice.address,
              signer = Alice.signer
            )

            val result = emulator.submit(tx).await()
            assert(result.isRight, s"Acc withdraw $i failed: $result")

            val (fee, exUnits, txSize) = extractMetrics(tx)
            totalFee += fee
            totalCpu += exUnits.steps
            totalMem += exUnits.memory
            totalTxSize += txSize
            totalProofSize += proofSize

            if i % 10 == 0 || i == SampleSize - 1 then
                info(
                  f"  [$i] fee=$fee%,d cpu=${exUnits.steps}%,d mem=${exUnits.memory}%,d txSize=$txSize proof=${proofSize}B ${proofMs}ms"
                )

            contractUtxo = findContractUtxo(tx, contract)

        val avg = BenchResult(
          variant,
          "withdraw",
          n,
          totalFee / SampleSize,
          totalCpu / SampleSize,
          totalMem / SampleSize,
          totalTxSize / SampleSize,
          totalProofSize / SampleSize,
          totalProofMs / SampleSize,
          buildMs
        )
        allResults += avg
        info(
          f"  AVG: fee=${avg.avgFee}%,d cpu=${avg.avgCpu}%,d mem=${avg.avgMem}%,d txSize=${avg.avgTxSize} proof=${avg.avgProofSize}B ${avg.avgProofGenMs}ms build=${avg.buildTimeMs}ms"
        )
    }

    // --- Aiken MPF benchmark ---

    private lazy val aikenScript: Script.PlutusV3 = {
        val fname = "/scalus/examples/AikenMpfData/plutus.json"
        val inputStream = this.getClass.getResourceAsStream(fname)
        if inputStream == null then throw new RuntimeException(s"Resource not found: $fname")
        val blueprint = Blueprint.fromJson(inputStream)
        val program = blueprint.validators.head.compiledCode.map(Program.fromCborHex).get
        Script.PlutusV3(program.cborByteString)
    }

    private lazy val aikenAddress: Address =
        Address(env.network, Credential.ScriptHash(aikenScript.scriptHash))

    private def benchAikenWithdraw(
        n: Int,
        buildTrie: Vector[(ByteString, ByteString)] => MpfTrie
    ): Unit = {
        val variant = "Aiken MPF"
        val elems = generateElements(n)

        val t0 = System.nanoTime()
        var trie = buildTrie(elems)
        val buildMs = (System.nanoTime() - t0) / 1_000_000
        info(s"$variant trie built in $buildMs ms (N=$n)")

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

        val publishTx =
            txHelper.publishScript(aliceUtxos, aikenScript, Bob.address, Alice.address, Alice.signer)
        assert(emulator.submit(publishTx).await().isRight, "Publish script failed")
        val refScriptUtxo = findRefScriptUtxo(publishTx)

        aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get
        val lockAmount = (SampleSize + 5) * K
        val lockTx = txHelper.lock(
          aliceUtxos, aikenAddress, lockAmount, trie.rootHash, Alice.address, Alice.signer
        )
        assert(emulator.submit(lockTx).await().isRight, "Lock tx failed")

        var contractUtxo = findContractUtxoByAddress(lockTx, aikenAddress)
        var remaining = lockAmount

        val sampleIndices =
            new scala.util.Random(42).shuffle((0 until n).toList).take(SampleSize)

        var totalFee = 0L
        var totalCpu = 0L
        var totalMem = 0L
        var totalTxSize = 0
        var totalProofSize = 0
        var totalProofMs = 0L

        for (idx, i) <- sampleIndices.zipWithIndex do
            val (key, value) = elems(idx)

            val proofT0 = System.nanoTime()
            val proofData = trie.proveExistsData(key)
            val proofMs = (System.nanoTime() - proofT0) / 1_000_000
            totalProofMs += proofMs
            val proofSize = proofData.toCbor.length

            trie = trie.delete(key)
            remaining -= K
            val newDatum = SetBenchDatum(BigInt(remaining), trie.rootHash)
            val redeemer = SetBenchRedeemer.Withdraw(key, value, proofData).toData

            val sponsorUtxos = emulator.findUtxos(Alice.address).await().toOption.get
            val tx = txHelper.withdraw(
              sponsorUtxos, contractUtxo, refScriptUtxo, aikenAddress, redeemer,
              newDatum, K, Bob.address, Alice.address, Alice.signer
            )

            val result = emulator.submit(tx).await()
            assert(result.isRight, s"Withdraw $i failed: $result")

            val (fee, exUnits, txSize) = extractMetrics(tx)
            totalFee += fee
            totalCpu += exUnits.steps
            totalMem += exUnits.memory
            totalTxSize += txSize
            totalProofSize += proofSize

            if i % 10 == 0 || i == SampleSize - 1 then
                info(
                  f"  [$i] fee=$fee%,d cpu=${exUnits.steps}%,d mem=${exUnits.memory}%,d txSize=$txSize proof=$proofSize%,dB ${proofMs}ms"
                )

            contractUtxo = findContractUtxoByAddress(tx, aikenAddress)

        val avg = BenchResult(
          variant, "withdraw", n,
          totalFee / SampleSize, totalCpu / SampleSize, totalMem / SampleSize,
          totalTxSize / SampleSize, totalProofSize / SampleSize, totalProofMs / SampleSize, buildMs
        )
        allResults += avg
        info(
          f"  AVG: fee=${avg.avgFee}%,d cpu=${avg.avgCpu}%,d mem=${avg.avgMem}%,d txSize=${avg.avgTxSize} proof=${avg.avgProofSize}B ${avg.avgProofGenMs}ms build=${avg.buildTimeMs}ms"
        )
    }

    private def benchAikenDeposit(
        n: Int,
        buildTrie: Vector[(ByteString, ByteString)] => MpfTrie
    ): Unit = {
        val variant = "Aiken MPF"
        val allElems = generateElements(n + SampleSize)
        val elems = allElems.take(n)
        val newElems = allElems.slice(n, n + SampleSize)

        val t0 = System.nanoTime()
        var trie = buildTrie(elems)
        val buildMs = (System.nanoTime() - t0) / 1_000_000
        info(s"$variant trie built in $buildMs ms (N=$n)")

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

        val publishTx =
            txHelper.publishScript(aliceUtxos, aikenScript, Bob.address, Alice.address, Alice.signer)
        assert(emulator.submit(publishTx).await().isRight, "Publish script failed")
        val refScriptUtxo = findRefScriptUtxo(publishTx)

        aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get
        val lockAmount = 10 * K
        val lockTx = txHelper.lock(
          aliceUtxos, aikenAddress, lockAmount, trie.rootHash, Alice.address, Alice.signer
        )
        assert(emulator.submit(lockTx).await().isRight, "Lock tx failed")

        var contractUtxo = findContractUtxoByAddress(lockTx, aikenAddress)
        var remaining = lockAmount

        var totalFee = 0L
        var totalCpu = 0L
        var totalMem = 0L
        var totalTxSize = 0
        var totalProofSize = 0
        var totalProofMs = 0L

        for (elem, i) <- newElems.zipWithIndex do
            val (key, value) = elem

            val proofT0 = System.nanoTime()
            val proofData = trie.proveMissingData(key)
            val proofMs = (System.nanoTime() - proofT0) / 1_000_000
            totalProofMs += proofMs
            val proofSize = proofData.toCbor.length

            trie = trie.insert(key, value)
            remaining += K
            val newDatum = SetBenchDatum(BigInt(remaining), trie.rootHash)
            val redeemer = SetBenchRedeemer.Deposit(key, value, proofData).toData

            val sponsorUtxos = emulator.findUtxos(Alice.address).await().toOption.get
            val tx = txHelper.deposit(
              sponsorUtxos, contractUtxo, refScriptUtxo, aikenAddress, redeemer,
              newDatum, K, Alice.address, Alice.signer
            )

            val result = emulator.submit(tx).await()
            assert(result.isRight, s"Deposit $i failed: $result")

            val (fee, exUnits, txSize) = extractMetrics(tx)
            totalFee += fee
            totalCpu += exUnits.steps
            totalMem += exUnits.memory
            totalTxSize += txSize
            totalProofSize += proofSize

            if i % 10 == 0 || i == SampleSize - 1 then
                info(
                  f"  [$i] fee=$fee%,d cpu=${exUnits.steps}%,d mem=${exUnits.memory}%,d txSize=$txSize proof=$proofSize%,dB ${proofMs}ms"
                )

            contractUtxo = findContractUtxoByAddress(tx, aikenAddress)

        val avg = BenchResult(
          variant, "deposit", n,
          totalFee / SampleSize, totalCpu / SampleSize, totalMem / SampleSize,
          totalTxSize / SampleSize, totalProofSize / SampleSize, totalProofMs / SampleSize, buildMs
        )
        allResults += avg
        info(
          f"  AVG: fee=${avg.avgFee}%,d cpu=${avg.avgCpu}%,d mem=${avg.avgMem}%,d txSize=${avg.avgTxSize} proof=${avg.avgProofSize}B ${avg.avgProofGenMs}ms build=${avg.buildTimeMs}ms"
        )
    }

    // --- Helpers ---

    private def findContractUtxo(
        tx: Transaction,
        contract: PlutusV3[Data => Unit]
    ): Utxo = findContractUtxoByAddress(tx, contract.address(env.network))

    private def findContractUtxoByAddress(tx: Transaction, scriptAddr: Address): Utxo = {
        tx.utxos
            .find { case (_, txOut) => txOut.address == scriptAddr }
            .map(Utxo(_))
            .getOrElse(fail("No contract UTxO found in transaction"))
    }

    private def findRefScriptUtxo(tx: Transaction): Utxo =
        tx.utxos
            .find { case (_, txOut) => txOut.scriptRef.isDefined }
            .map(Utxo(_))
            .getOrElse(fail("No reference script UTxO found"))

    private def extractMetrics(tx: Transaction): (Long, ExUnits, Int) = {
        val fee = tx.body.value.fee.value
        val exUnits = tx.witnessSet.redeemers
            .map(_.value.toSeq.head.exUnits)
            .getOrElse(ExUnits(0, 0))
        val txSize = tx.toCbor.length
        (fee, exUnits, txSize)
    }

    // --- Tests ---

    // --- N=32K ---

    test("MPF-16o withdraw N=32K", Benchmark) {
        info("=== MPF-16o withdraw N=32000 ===")
        benchMpfWithdraw("MPF-16o", 32000, Mpf16oContract.withErrorTraces, MpfTrie.wrap16o)
    }

    test("MPF-16b withdraw N=32K", Benchmark) {
        info("=== MPF-16b withdraw N=32000 ===")
        benchMpfWithdraw("MPF-16b", 32000, Mpf16bContract.withErrorTraces, MpfTrie.wrap16b)
    }

    test("MPF-16o-light withdraw N=32K", Benchmark) {
        info("=== MPF-16o-light withdraw N=32000 ===")
        benchMpfWithdraw("MPF-16o-light", 32000, Mpf16oLightContract.withErrorTraces, MpfTrie.wrap16o)
    }

    test("MPF-16b-light withdraw N=32K", Benchmark) {
        info("=== MPF-16b-light withdraw N=32000 ===")
        benchMpfWithdraw("MPF-16b-light", 32000, Mpf16bLightContract.withErrorTraces, MpfTrie.wrap16b)
    }

    test("Aiken MPF withdraw N=32K", Benchmark) {
        info("=== Aiken MPF withdraw N=32000 ===")
        benchAikenWithdraw(32000, MpfTrie.wrap16o)
    }

    test("Accumulator withdraw N=32K", Benchmark) {
        info("=== Accumulator withdraw N=32000 ===")
        benchAccWithdraw(32000)
    }

    test("MPF-16o deposit N=32K", Benchmark) {
        info("=== MPF-16o deposit N=32000 ===")
        benchMpfDeposit("MPF-16o", 32000, Mpf16oContract.withErrorTraces, MpfTrie.wrap16o)
    }

    test("MPF-16b deposit N=32K", Benchmark) {
        info("=== MPF-16b deposit N=32000 ===")
        benchMpfDeposit("MPF-16b", 32000, Mpf16bContract.withErrorTraces, MpfTrie.wrap16b)
    }

    test("MPF-16o-light deposit N=32K", Benchmark) {
        info("=== MPF-16o-light deposit N=32000 ===")
        benchMpfDeposit("MPF-16o-light", 32000, Mpf16oLightContract.withErrorTraces, MpfTrie.wrap16o)
    }

    test("MPF-16b-light deposit N=32K", Benchmark) {
        info("=== MPF-16b-light deposit N=32000 ===")
        benchMpfDeposit("MPF-16b-light", 32000, Mpf16bLightContract.withErrorTraces, MpfTrie.wrap16b)
    }

    test("Aiken MPF deposit N=32K", Benchmark) {
        info("=== Aiken MPF deposit N=32000 ===")
        benchAikenDeposit(32000, MpfTrie.wrap16o)
    }

    // --- N=100K ---

    test("MPF-16o withdraw N=100K", Benchmark) {
        info("=== MPF-16o withdraw N=100000 ===")
        benchMpfWithdraw("MPF-16o", 100000, Mpf16oContract.withErrorTraces, MpfTrie.wrap16o)
    }

    test("MPF-16b withdraw N=100K", Benchmark) {
        info("=== MPF-16b withdraw N=100000 ===")
        benchMpfWithdraw("MPF-16b", 100000, Mpf16bContract.withErrorTraces, MpfTrie.wrap16b)
    }

    test("MPF-16o deposit N=100K", Benchmark) {
        info("=== MPF-16o deposit N=100000 ===")
        benchMpfDeposit("MPF-16o", 100000, Mpf16oContract.withErrorTraces, MpfTrie.wrap16o)
    }

    test("MPF-16b deposit N=100K", Benchmark) {
        info("=== MPF-16b deposit N=100000 ===")
        benchMpfDeposit("MPF-16b", 100000, Mpf16bContract.withErrorTraces, MpfTrie.wrap16b)
    }

    // --- N=1M ---

    test("MPF-16o withdraw N=1M", Benchmark) {
        info("=== MPF-16o withdraw N=1000000 ===")
        benchMpfWithdraw("MPF-16o", 1000000, Mpf16oContract.withErrorTraces, MpfTrie.wrap16o)
    }

    test("MPF-16b withdraw N=1M", Benchmark) {
        info("=== MPF-16b withdraw N=1000000 ===")
        benchMpfWithdraw("MPF-16b", 1000000, Mpf16bContract.withErrorTraces, MpfTrie.wrap16b)
    }

    test("MPF-16o deposit N=1M", Benchmark) {
        info("=== MPF-16o deposit N=1000000 ===")
        benchMpfDeposit("MPF-16o", 1000000, Mpf16oContract.withErrorTraces, MpfTrie.wrap16o)
    }

    test("MPF-16b deposit N=1M", Benchmark) {
        info("=== MPF-16b deposit N=1000000 ===")
        benchMpfDeposit("MPF-16b", 1000000, Mpf16bContract.withErrorTraces, MpfTrie.wrap16b)
    }

    test("Summary table", Benchmark) {
        if allResults.isEmpty then info("No results collected (run individual benchmarks first)")
        else
            info("")
            info("=== Benchmark Summary (averages) ===")
            val hdr =
                f"${"N"}%6s | ${"Variant"}%-10s | ${"Op"}%-8s | ${"Fee"}%12s | ${"CPU"}%14s | ${"Memory"}%10s | ${"Tx Size"}%8s | ${"Proof (B)"}%10s | ${"Build (ms)"}%10s"
            info(hdr)
            info("-" * hdr.length)
            for r <- allResults do
                info(
                  f"${r.n}%6d | ${r.variant}%-10s | ${r.op}%-8s | ${r.avgFee}%,12d | ${r.avgCpu}%,14d | ${r.avgMem}%,10d | ${r.avgTxSize}%8d | ${r.avgProofSize}%10d | ${r.buildTimeMs}%10d"
                )
    }
}

object SetBenchEmulatorTest {
    private[setbench] object Benchmark extends Tag("scalus.testing.Benchmark")

    private[setbench] case class BenchResult(
        variant: String,
        op: String,
        n: Int,
        avgFee: Long,
        avgCpu: Long,
        avgMem: Long,
        avgTxSize: Int,
        avgProofSize: Int,
        avgProofGenMs: Long,
        buildTimeMs: Long
    )

    /** Wrapper to unify Mpf16 and Mpf16o off-chain trie APIs. Proof conversion to Data is done
      * inside the wrapper so the caller doesn't need variant-specific ToData imports.
      */
    private[setbench] trait MpfTrie {
        def rootHash: ByteString
        def proveExistsData(key: ByteString): Data
        def proveMissingData(key: ByteString): Data
        def delete(key: ByteString): MpfTrie
        def insert(key: ByteString, value: ByteString): MpfTrie
    }

    private[setbench] object MpfTrie {

        def wrap16o(elems: Vector[(ByteString, ByteString)]): MpfTrie =
            Mpf16oWrapper(Mpf16o.fromList(elems))

        def wrap16b(elems: Vector[(ByteString, ByteString)]): MpfTrie =
            Mpf16bWrapper(Mpf16b.fromList(elems))

        private case class Mpf16oWrapper(trie: Mpf16o) extends MpfTrie {
            import scalus.cardano.onchain.plutus.mpfo.MerklePatriciaForestry.*
            def rootHash: ByteString = trie.rootHash
            def proveExistsData(key: ByteString): Data = trie.proveExists(key).toData
            def proveMissingData(key: ByteString): Data = trie.proveMissing(key).toData
            def delete(key: ByteString): MpfTrie = Mpf16oWrapper(trie.delete(key))
            def insert(key: ByteString, value: ByteString): MpfTrie =
                Mpf16oWrapper(trie.insert(key, value))
        }

        private case class Mpf16bWrapper(trie: Mpf16b) extends MpfTrie {
            def rootHash: ByteString = trie.rootHash
            def proveExistsData(key: ByteString): Data = Data.B(trie.proveExistsBinary(key))
            def proveMissingData(key: ByteString): Data = Data.B(trie.proveMissingBinary(key))
            def delete(key: ByteString): MpfTrie = Mpf16bWrapper(trie.delete(key))
            def insert(key: ByteString, value: ByteString): MpfTrie =
                Mpf16bWrapper(trie.insert(key, value))
        }
    }
}
