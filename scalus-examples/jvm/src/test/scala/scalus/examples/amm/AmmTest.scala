package scalus.examples.amm

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator}
import scalus.cardano.node.Emulator
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.ScalusTest
import scalus.testing.kit.TestUtil.genesisHash
import scalus.uplc.builtin.ByteString
import scalus.utils.await

class AmmTest extends AnyFunSuite, ScalusTest, ScalaCheckPropertyChecks {
    import AmmTest.{*, given}

    test(s"AmmValidator size: ${AmmContract.script.script.size} bytes") {
        info(s"Validator size: ${AmmContract.script.script.size} bytes")
    }

    test("createPool: creates empty pool with zero reserves") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val tx = txCreator.createEmptyPool(utxos, 5_000_000L, Alice.address, Alice.signer)
        assert(provider.submit(tx).await().isRight, "createPool should succeed")
        val poolUtxo = Utxo(tx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val datum = txCreator.readPoolDatum(poolUtxo)
        assert(datum == AmmDatum(BigInt(0), BigInt(0), BigInt(0)))
    }

    test("deposit to an empty pool mints LP = isqrt(x0 * x1)") {
        val (provider, txCreator) = createSetup()

        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val createTx = txCreator.createEmptyPool(utxos1, 5_000_000L, Alice.address, Alice.signer)
        provider.submit(createTx).await()

        val poolUtxo = Utxo(createTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val depositTx =
            txCreator.deposit(utxos2, poolUtxo, x0 = 1000L, x1 = 4000L, Alice.address, Alice.signer)
        assert(provider.submit(depositTx).await().isRight, "first deposit should succeed")

        val newPool = Utxo(depositTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val datum = txCreator.readPoolDatum(newPool)
        assert(datum.r0 == BigInt(1000))
        assert(datum.r1 == BigInt(4000))
        assert(datum.lpSupply == BigInt(Math.sqrt(1000 * 4000).toInt))
    }

    test("deposit to a non-empty pool mints LP proportionally, maintains the constant product") {
        val (provider, txCreator) = createSetup()
        val poolUtxo1 = initPoolWith(provider, txCreator, x0 = 1000L, x1 = 4000L)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val depositTx =
            txCreator.deposit(utxos, poolUtxo1, x0 = 1000L, x1 = 4000L, Alice.address, Alice.signer)
        assert(provider.submit(depositTx).await().isRight, "second deposit should succeed")

        val newPool = Utxo(depositTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val datum = txCreator.readPoolDatum(newPool)
        assert(datum.r0 == BigInt(2000))
        assert(datum.r1 == BigInt(8000))
        assert(datum.lpSupply == BigInt(4000)) // doubled LP supply
    }

    test("redeem: burns LP, receives proportional t0 and t1") {
        val (provider, txCreator) = createSetup()
        val poolUtxo1 = initPoolWith(provider, txCreator, x0 = 1000L, x1 = 4000L)
        val initialDatum = txCreator.readPoolDatum(poolUtxo1)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        // Redeem half the LP supply
        val halfLp = (initialDatum.lpSupply / 2).toLong
        val redeemTx = txCreator.redeem(utxos, poolUtxo1, halfLp, Alice.address, Alice.signer)
        assert(provider.submit(redeemTx).await().isRight, "redeem should succeed")

        val newPool = Utxo(redeemTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val newDatum = txCreator.readPoolDatum(newPool)
        assert(newDatum.lpSupply == initialDatum.lpSupply - BigInt(halfLp))
        assert(
          newDatum.r0 == initialDatum.r0 - (BigInt(
            halfLp
          ) * initialDatum.r0 / initialDatum.lpSupply)
        )
        assert(
          newDatum.r1 == initialDatum.r1 - (BigInt(
            halfLp
          ) * initialDatum.r1 / initialDatum.lpSupply)
        )
    }

    test("swap t0 to t1 correctly") {
        val (provider, txCreator) = createSetup()
        val poolUtxo = initPoolWith(provider, txCreator, x0 = 10_000L, x1 = 40_000L)
        val d = txCreator.readPoolDatum(poolUtxo)

        val amountIn = 1000L
        val (expectedOut, _) = txCreator.swapQuote(poolUtxo, t0In = true, amountIn)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val swapTx = txCreator.swap(
          utxos,
          poolUtxo,
          t0In = true,
          amountIn,
          minAmountOut = 1L,
          Alice.address,
          Alice.signer
        )
        assert(provider.submit(swapTx).await().isRight, "swap t0 to t1 should succeed")

        val newPool = Utxo(swapTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val newDatum = txCreator.readPoolDatum(newPool)
        assert(newDatum.r0 == d.r0 + BigInt(amountIn))
        assert(newDatum.r1 == d.r1 - BigInt(expectedOut))
        assert(newDatum.lpSupply == d.lpSupply)
        // k-invariant
        assert(newDatum.r0 * newDatum.r1 >= d.r0 * d.r1, "k-invariant must hold")
    }

    test("swap t1 to t0: correct output, datum updated, invariant holds") {
        val (provider, txCreator) = createSetup()
        val poolUtxo = initPoolWith(provider, txCreator, x0 = 10_000L, x1 = 40_000L)
        val d = txCreator.readPoolDatum(poolUtxo)

        val amountIn = 4000L
        val (expectedOut, _) = txCreator.swapQuote(poolUtxo, t0In = false, amountIn)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val swapTx = txCreator.swap(
          utxos,
          poolUtxo,
          t0In = false,
          amountIn,
          minAmountOut = 1L,
          Alice.address,
          Alice.signer
        )
        assert(provider.submit(swapTx).await().isRight, "swap t1 to t0 should succeed")

        val newPool = Utxo(swapTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val newDatum = txCreator.readPoolDatum(newPool)
        assert(newDatum.r1 == d.r1 + BigInt(amountIn))
        assert(newDatum.r0 == d.r0 - BigInt(expectedOut))
        assert(newDatum.lpSupply == d.lpSupply)
        assert(newDatum.r0 * newDatum.r1 >= d.r0 * d.r1, "k-invariant must hold")
    }

    test("FAIL: deposit with wrong ratio") {
        val (provider, txCreator) = createSetup()
        val poolUtxo = initPoolWith(provider, txCreator, x0 = 1000L, x1 = 4000L)

        val (_, badTxCreator) = createSetup(EvaluatorMode.Validate)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val badTx = badTxCreator.deposit(
          utxos,
          poolUtxo,
          x0 = 1000L,
          x1 = 1000L, // 1000/1000 != initial 1000/4000
          Alice.address,
          Alice.signer
        )
        assertSubmitFails(provider, badTx)
    }

    test("FAIL: swap below minAmountOut") {
        val (provider, txCreator) = createSetup()
        val poolUtxo = initPoolWith(provider, txCreator, x0 = 10_000L, x1 = 40_000L)

        val (_, badTxCreator) = createSetup(EvaluatorMode.Validate)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val badTx = badTxCreator.swap(
          utxos,
          poolUtxo,
          t0In = true,
          1000L,
          minAmountOut = 999_999L, // too large
          Alice.address,
          Alice.signer
        )
        assertSubmitFails(provider, badTx)
    }

    test("property: k-invariant holds for all valid swaps") {
        forAll(swapGen) { (r0, r1, amountIn, t0In) =>
            val (provider, txCreator) = createSetup()
            val poolUtxo = initPoolWith(provider, txCreator, x0 = r0, x1 = r1)
            val d = txCreator.readPoolDatum(poolUtxo)

            val (amountOut, _) = txCreator.swapQuote(poolUtxo, t0In, amountIn)
            whenever(amountOut > 0) {
                val (newR0, newR1) =
                    if t0In then (d.r0 + BigInt(amountIn), d.r1 - BigInt(amountOut))
                    else (d.r0 - BigInt(amountOut), d.r1 + BigInt(amountIn))
                assert(newR0 * newR1 >= d.r0 * d.r1, "k-invariant must hold after swap")
            }
        }
    }

    test("property: deposit-then-redeem yields back at most what was deposited") {
        // Generate a scale factor k and deposit exactly (k*r0, k*r1) so the ratio check
        // x0 * r1 == x1 * r0 holds by construction (k*r0 * r1 == k*r1 * r0).
        forAll(depositGen) { (r0, r1, k) =>
            val x0 = r0 * k
            val x1 = r1 * k
            val (provider, txCreator) = createSetup()
            val poolUtxo = initPoolWith(provider, txCreator, x0 = r0, x1 = r1)
            val dBefore = txCreator.readPoolDatum(poolUtxo)

            val utxos = provider.findUtxos(Alice.address).await().toOption.get
            val depositTx = txCreator.deposit(utxos, poolUtxo, x0, x1, Alice.address, Alice.signer)
            val depositOk = provider.submit(depositTx).await()
            whenever(depositOk.isRight) {
                val afterDeposit =
                    Utxo(depositTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
                val dAfterDeposit = txCreator.readPoolDatum(afterDeposit)
                val lpMinted = (dAfterDeposit.lpSupply - dBefore.lpSupply).toLong

                val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
                val redeemTx =
                    txCreator.redeem(utxos2, afterDeposit, lpMinted, Alice.address, Alice.signer)
                val redeemOk = provider.submit(redeemTx).await()
                whenever(redeemOk.isRight) {
                    val afterRedeem =
                        Utxo(redeemTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
                    val dAfterRedeem = txCreator.readPoolDatum(afterRedeem)
                    val got0 = (dAfterDeposit.r0 - dAfterRedeem.r0).toLong
                    val got1 = (dAfterDeposit.r1 - dAfterRedeem.r1).toLong
                    assert(got0 <= x0, s"got back more t0 than deposited: $got0 > $x0")
                    assert(got1 <= x1, s"got back more t1 than deposited: $got1 > $x1")
                }
            }
        }
    }

}

object AmmTest extends ScalusTest {
    given env: CardanoInfo = scalus.testing.kit.TestUtil.testEnvironment

    val t0PolicyId: PolicyId = ScriptHash.fromArray(Array.fill(28)(0x01.toByte))
    val t1PolicyId: PolicyId = ScriptHash.fromArray(Array.fill(28)(0x02.toByte))
    val t0Name: AssetName = AssetName(ByteString.fromString("T0"))
    val t1Name: AssetName = AssetName(ByteString.fromString("T1"))

    val ammParams: AmmParams = AmmParams(
      t0 = (t0PolicyId, t0Name.bytes),
      t1 = (t1PolicyId, t1Name.bytes),
      feeNumerator = BigInt(997),
      feeDenominator = BigInt(1000)
    )

    private val compiledContract = AmmContract.withErrorTraces

    private def aliceTokenUtxos: Map[TransactionInput, TransactionOutput] = {
        val tokenValue =
            Value.asset(t0PolicyId, t0Name, 1_000_000L, Coin.ada(100)) +
                Value.asset(t1PolicyId, t1Name, 1_000_000L)
        Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput(Alice.address, Value.ada(10_000)),
          TransactionInput(genesisHash, 1) -> TransactionOutput(Alice.address, Value.ada(10_000)),
          TransactionInput(genesisHash, 2) -> TransactionOutput(Alice.address, tokenValue),
          TransactionInput(genesisHash, 3) -> TransactionOutput(Alice.address, Value.ada(10_000)),
          TransactionInput(genesisHash, 4) -> TransactionOutput(Bob.address, Value.ada(10_000))
        )
    }

    def createSetup(
        evaluatorMode: EvaluatorMode = EvaluatorMode.EvaluateAndComputeCost
    ): (Emulator, AmmOffchain) = {
        val evaluator = evaluatorMode match
            case EvaluatorMode.EvaluateAndComputeCost =>
                PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost)
            case _ =>
                PlutusScriptEvaluator.constMaxBudget(env)

        val provider = Emulator(
          initialUtxos = aliceTokenUtxos,
          initialContext =
              Context.testMainnet().copy(evaluatorMode = EvaluatorMode.EvaluateAndComputeCost),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

        val txCreator = AmmOffchain(
          env = env,
          evaluator = evaluator,
          contract = compiledContract,
          params = ammParams
        )

        (provider, txCreator)
    }

    // creates an empty pool and immediately makes a deposit tx with x0 and x1
    def initPoolWith(provider: Emulator, txCreator: AmmOffchain, x0: Long, x1: Long): Utxo = {
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val createTx = txCreator.createEmptyPool(utxos1, 5_000_000L, Alice.address, Alice.signer)
        provider.submit(createTx).await()

        val emptyPool = Utxo(createTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val depositTx = txCreator.deposit(utxos2, emptyPool, x0, x1, Alice.address, Alice.signer)
        provider.submit(depositTx).await()
        Utxo(depositTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
    }

    def assertSubmitFails(provider: Emulator, tx: Transaction): Unit =
        provider.submit(tx).await() match {
            case Left(_)  => ()
            case Right(_) => fail("Expected transaction submission to fail but it succeeded")
        }

    val swapGen: Gen[(Long, Long, Long, Boolean)] =
        for
            r0 <- Gen.chooseNum(10_000L, 1_000_000L)
            r1 <- Gen.chooseNum(10_000L, 1_000_000L)
            amountIn <- Gen.chooseNum(1L, r0 / 10)
            t0In <- Gen.oneOf(true, false)
        yield (r0, r1, amountIn, t0In)

    val depositGen: Gen[(Long, Long, Long)] =
        for
            r0 <- Gen.chooseNum(10L, 1_000L)
            r1 <- Gen.chooseNum(10L, 1_000L)
            k <- Gen.chooseNum(1L, 10L)
        yield (r0, r1, k)
}
