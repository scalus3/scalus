package scalus.examples.setbench

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.offchain.amt.AppendOnlyMerkleTree as OffChainAmt
import scalus.cardano.txbuilder.*
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.await

/** Benchmark for AMT (Append-Only Merkle Tree) operations through the emulator.
  *
  * Tagged with `scalus.testing.Benchmark` — excluded from default test runs. Run with:
  * {{{
  * sbtn "scalusExamplesJVM/testOnly *AmtBenchEmulatorTest -- -n scalus.testing.Benchmark"
  * }}}
  */
class AmtBenchEmulatorTest extends AnyFunSuite with ScalusTest {
    import AmtBenchEmulatorTest.*

    private given env: CardanoInfo = TestUtil.testEnvironment

    private val SampleSize = 10
    private val K = 2_000_000L
    private val MaxN = 100_000 + SampleSize

    private lazy val allKeys: Vector[ByteString] = {
        val rng = new scala.util.Random(42)
        Vector.tabulate(MaxN) { i =>
            ByteString.fromString(s"element-${rng.nextInt()}-$i")
        }
    }

    private val txHelper = AmtTransactions(env)

    private val allResults = collection.mutable.ArrayBuffer[BenchResult]()

    // --- AMT withdraw benchmark ---

    private def benchAmtWithdraw(n: Int): Unit = {
        val variant = "AMT"
        val contract = AmtContract.withErrorTraces
        val keys = allKeys.take(n)
        val depth = OffChainAmt.depthForSize(n)

        val t0 = System.nanoTime()
        var tree = OffChainAmt.empty(depth)
        for key <- keys do tree = tree.append(key)
        val buildMs = (System.nanoTime() - t0) / 1_000_000
        info(s"$variant tree built in $buildMs ms (N=$n, D=$depth)")

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

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
          initialRoot = tree.rootHash,
          size = tree.size,
          depth = depth,
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
            val key = keys(idx)

            val proofT0 = System.nanoTime()
            val proofBytes = tree.proveMembership(key)
            val proofMs = (System.nanoTime() - proofT0) / 1_000_000
            totalProofMs += proofMs
            val proofData = Data.B(proofBytes)
            val proofSize = proofData.toCbor.length

            remaining -= K
            val newDatum = AmtDatum(BigInt(remaining), tree.rootHash, BigInt(tree.size), BigInt(depth))
            val redeemer = AmtRedeemer.Withdraw(key, proofData).toData

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
          depth,
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

    // --- AMT deposit benchmark ---

    private def benchAmtDeposit(n: Int): Unit = {
        val variant = "AMT"
        val contract = AmtContract.withErrorTraces
        val keys = allKeys.take(n)
        val depth = OffChainAmt.depthForSize(n)

        val t0 = System.nanoTime()
        var tree = OffChainAmt.empty(depth)
        for key <- keys do tree = tree.append(key)
        val buildMs = (System.nanoTime() - t0) / 1_000_000
        info(s"$variant tree built in $buildMs ms (N=$n, D=$depth)")

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

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
          initialRoot = tree.rootHash,
          size = tree.size,
          depth = depth,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(emulator.submit(lockTx).await().isRight, "Lock tx failed")

        var contractUtxo = findContractUtxo(lockTx, contract)
        var remaining = lockAmount

        // pick random existing members to deposit for
        val sampleIndices =
            new scala.util.Random(42).shuffle((0 until n).toList).take(SampleSize)

        var totalFee = 0L
        var totalCpu = 0L
        var totalMem = 0L
        var totalTxSize = 0
        var totalProofSize = 0
        var totalProofMs = 0L

        for (idx, i) <- sampleIndices.zipWithIndex do
            val key = keys(idx)

            val proofT0 = System.nanoTime()
            val proofBytes = tree.proveMembership(key)
            val proofMs = (System.nanoTime() - proofT0) / 1_000_000
            totalProofMs += proofMs
            val proofData = Data.B(proofBytes)
            val proofSize = proofData.toCbor.length

            remaining += K
            val newDatum = AmtDatum(BigInt(remaining), tree.rootHash, BigInt(tree.size), BigInt(depth))
            val redeemer = AmtRedeemer.Deposit(key, proofData).toData

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
          depth,
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

    // --- AMT add benchmark ---

    private def benchAmtAdd(n: Int): Unit = {
        val variant = "AMT"
        val contract = AmtContract.withErrorTraces
        val depth = OffChainAmt.depthForSize(n + SampleSize)

        // build tree with n elements, then benchmark appending SampleSize more
        val keys = allKeys.take(n + SampleSize)

        val t0 = System.nanoTime()
        var tree = OffChainAmt.empty(depth)
        for i <- 0 until n do tree = tree.append(keys(i))
        val buildMs = (System.nanoTime() - t0) / 1_000_000
        info(s"$variant tree built in $buildMs ms (N=$n, D=$depth)")

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

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
          initialRoot = tree.rootHash,
          size = tree.size,
          depth = depth,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(emulator.submit(lockTx).await().isRight, "Lock tx failed")

        var contractUtxo = findContractUtxo(lockTx, contract)
        val remaining = lockAmount // doesn't change for Add

        var totalFee = 0L
        var totalCpu = 0L
        var totalMem = 0L
        var totalTxSize = 0
        var totalProofSize = 0
        var totalProofMs = 0L

        for i <- 0 until SampleSize do
            val key = keys(n + i)

            val proofT0 = System.nanoTime()
            val proofBytes = tree.proveAppend()
            val proofMs = (System.nanoTime() - proofT0) / 1_000_000
            totalProofMs += proofMs
            val proofData = Data.B(proofBytes)
            val proofSize = proofData.toCbor.length

            tree = tree.append(key)
            val newDatum =
                AmtDatum(BigInt(remaining), tree.rootHash, BigInt(tree.size), BigInt(depth))
            val redeemer = AmtRedeemer.Add(key, proofData).toData

            val sponsorUtxos = emulator.findUtxos(Alice.address).await().toOption.get
            val tx = txHelper.add(
              utxos = sponsorUtxos,
              contractUtxo = contractUtxo,
              refScriptUtxo = refScriptUtxo,
              contract = contract,
              redeemer = redeemer,
              newDatum = newDatum,
              sponsor = Alice.address,
              signer = Alice.signer
            )

            val result = emulator.submit(tx).await()
            assert(result.isRight, s"Add $i failed: $result")

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
          "add",
          n,
          depth,
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

    // --- Helpers ---

    private def findContractUtxo(
        tx: Transaction,
        contract: PlutusV3[Data => Unit]
    ): Utxo = {
        val scriptAddr = contract.address(env.network)
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

    test("AMT withdraw N=10", Benchmark) {
        info("=== AMT withdraw N=10 ===")
        benchAmtWithdraw(10)
    }

    test("AMT withdraw N=30", Benchmark) {
        info("=== AMT withdraw N=30 ===")
        benchAmtWithdraw(30)
    }

    test("AMT withdraw N=100", Benchmark) {
        info("=== AMT withdraw N=100 ===")
        benchAmtWithdraw(100)
    }

    test("AMT withdraw N=10K", Benchmark) {
        info("=== AMT withdraw N=10000 ===")
        benchAmtWithdraw(10000)
    }

    test("AMT withdraw N=32K", Benchmark) {
        info("=== AMT withdraw N=32000 ===")
        benchAmtWithdraw(32000)
    }

    test("AMT withdraw N=100K", Benchmark) {
        info("=== AMT withdraw N=100000 ===")
        benchAmtWithdraw(100000)
    }

    test("AMT deposit N=10", Benchmark) {
        info("=== AMT deposit N=10 ===")
        benchAmtDeposit(10)
    }

    test("AMT deposit N=30", Benchmark) {
        info("=== AMT deposit N=30 ===")
        benchAmtDeposit(30)
    }

    test("AMT deposit N=100", Benchmark) {
        info("=== AMT deposit N=100 ===")
        benchAmtDeposit(100)
    }

    test("AMT deposit N=10K", Benchmark) {
        info("=== AMT deposit N=10000 ===")
        benchAmtDeposit(10000)
    }

    test("AMT deposit N=32K", Benchmark) {
        info("=== AMT deposit N=32000 ===")
        benchAmtDeposit(32000)
    }

    test("AMT deposit N=100K", Benchmark) {
        info("=== AMT deposit N=100000 ===")
        benchAmtDeposit(100000)
    }

    test("AMT add N=10", Benchmark) {
        info("=== AMT add N=10 ===")
        benchAmtAdd(10)
    }

    test("AMT add N=30", Benchmark) {
        info("=== AMT add N=30 ===")
        benchAmtAdd(30)
    }

    test("AMT add N=100", Benchmark) {
        info("=== AMT add N=100 ===")
        benchAmtAdd(100)
    }

    test("AMT add N=10K", Benchmark) {
        info("=== AMT add N=10000 ===")
        benchAmtAdd(10000)
    }

    test("AMT add N=32K", Benchmark) {
        info("=== AMT add N=32000 ===")
        benchAmtAdd(32000)
    }

    test("AMT add N=100K", Benchmark) {
        info("=== AMT add N=100000 ===")
        benchAmtAdd(100000)
    }

    test("AMT summary table", Benchmark) {
        if allResults.isEmpty then info("No results collected (run individual benchmarks first)")
        else
            info("")
            info("=== AMT Benchmark Summary (averages) ===")
            val hdr =
                f"${"N"}%6s | ${"D"}%3s | ${"Op"}%-8s | ${"Fee (lovelace)"}%15s | ${"CPU"}%14s | ${"Memory"}%10s | ${"Tx Size"}%8s | ${"Proof (B)"}%10s | ${"Build (ms)"}%10s"
            info(hdr)
            info("-" * hdr.length)
            for r <- allResults do
                info(
                  f"${r.n}%6d | ${r.depth}%3d | ${r.op}%-8s | ${r.avgFee}%,15d | ${r.avgCpu}%,14d | ${r.avgMem}%,10d | ${r.avgTxSize}%8d | ${r.avgProofSize}%10d | ${r.buildTimeMs}%10d"
                )
    }
}

object AmtBenchEmulatorTest {
    private[setbench] object Benchmark extends Tag("scalus.testing.Benchmark")

    private[setbench] case class BenchResult(
        variant: String,
        op: String,
        n: Int,
        depth: Int,
        avgFee: Long,
        avgCpu: Long,
        avgMem: Long,
        avgTxSize: Int,
        avgProofSize: Int,
        avgProofGenMs: Long,
        buildTimeMs: Long
    )

    /** Transaction builder for AMT contract operations. */
    private[setbench] case class AmtTransactions(env: CardanoInfo) {
        private val builder = TxBuilder(env)

        def publishScript(
            utxos: Utxos,
            contract: PlutusV3[Data => Unit],
            holder: Address,
            sponsor: Address,
            signer: TransactionSigner
        ): Transaction = {
            val scriptOutput = TransactionOutput(
              holder,
              Value.lovelace(10_000_000L),
              None,
              Some(ScriptRef(contract.script))
            )
            builder
                .output(scriptOutput)
                .complete(availableUtxos = utxos, sponsor = sponsor)
                .sign(signer)
                .transaction
        }

        def lock(
            utxos: Utxos,
            contract: PlutusV3[Data => Unit],
            totalLovelace: Long,
            initialRoot: ByteString,
            size: Int,
            depth: Int,
            sponsor: Address,
            signer: TransactionSigner
        ): Transaction = {
            val datum = AmtDatum(BigInt(totalLovelace), initialRoot, BigInt(size), BigInt(depth))
            builder
                .payTo(contract.address(env.network), Value.lovelace(totalLovelace), datum)
                .complete(availableUtxos = utxos, sponsor = sponsor)
                .sign(signer)
                .transaction
        }

        def withdraw(
            utxos: Utxos,
            contractUtxo: Utxo,
            refScriptUtxo: Utxo,
            contract: PlutusV3[Data => Unit],
            redeemer: Data,
            newDatum: AmtDatum,
            k: Long,
            withdrawTo: Address,
            sponsor: Address,
            signer: TransactionSigner
        ): Transaction = {
            val newLovelace = contractUtxo.output.value.coin.value - k
            builder
                .references(refScriptUtxo, contract)
                .spend(contractUtxo, redeemer)
                .payTo(withdrawTo, Value.lovelace(k))
                .payTo(contract.address(env.network), Value.lovelace(newLovelace), newDatum)
                .complete(availableUtxos = utxos, sponsor = sponsor)
                .sign(signer)
                .transaction
        }

        def deposit(
            utxos: Utxos,
            contractUtxo: Utxo,
            refScriptUtxo: Utxo,
            contract: PlutusV3[Data => Unit],
            redeemer: Data,
            newDatum: AmtDatum,
            k: Long,
            sponsor: Address,
            signer: TransactionSigner
        ): Transaction = {
            val newLovelace = contractUtxo.output.value.coin.value + k
            builder
                .references(refScriptUtxo, contract)
                .spend(contractUtxo, redeemer)
                .payTo(contract.address(env.network), Value.lovelace(newLovelace), newDatum)
                .complete(availableUtxos = utxos, sponsor = sponsor)
                .sign(signer)
                .transaction
        }

        def add(
            utxos: Utxos,
            contractUtxo: Utxo,
            refScriptUtxo: Utxo,
            contract: PlutusV3[Data => Unit],
            redeemer: Data,
            newDatum: AmtDatum,
            sponsor: Address,
            signer: TransactionSigner
        ): Transaction = {
            // Add doesn't change lovelace — same amount continues
            val sameLovelace = contractUtxo.output.value.coin.value
            builder
                .references(refScriptUtxo, contract)
                .spend(contractUtxo, redeemer)
                .payTo(contract.address(env.network), Value.lovelace(sameLovelace), newDatum)
                .complete(availableUtxos = utxos, sponsor = sponsor)
                .sign(signer)
                .transaction
        }
    }
}
