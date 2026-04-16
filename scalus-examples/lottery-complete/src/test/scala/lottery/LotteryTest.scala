package lottery

import lottery.onchain.*
import lottery.offchain.LotteryTransactions
import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.Options
import scalus.uplc.builtin.Builtins.sha2_256
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.cardano.txbuilder.RedeemerPurpose
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.testing.kit.Party.{Alice, Bob, Eve}
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

import java.time.Instant
import scala.util.Try

/** Comprehensive test suite for the lottery validator.
  *
  * Tests cover all actions (Reveal, Lose, Timeout) in both player orderings, along with negative
  * cases for wrong preimages, tampered secrets/deadlines, early timeouts, and odd-sum losses.
  *
  * Each test:
  *   1. Creates a fresh Emulator with funded player addresses
  *   2. Builds and submits transactions via LotteryTransactions
  *   3. Verifies both direct Scala evaluation and full emulator submission
  */
class LotteryTest extends AnyFunSuite, ScalusTest {
    import LotteryTest.{*, given}

    // --- Validator size ---

    test(s"Lottery validator size is ${LotteryContract.compiled.script.script.size} bytes") {
        info(s"Validator size: ${LotteryContract.compiled.script.script.size} bytes")
    }

    // --- Reveal from Empty state ---

    test("P1 reveals valid preimage from Empty state") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val revealTx = txCreator.revealPlayerOne(
          utxos = utxos,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, revealTx, lotteryUtxo._1)
    }

    test("P2 reveals valid preimage from Empty state") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val revealTx = txCreator.revealPlayerTwo(
          utxos = utxos,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage2,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, revealTx, lotteryUtxo._1)
    }

    // --- Wrong preimage ---

    test("FAIL: P1 reveal with wrong preimage") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val revealTx = txCreator.revealPlayerOne(
          utxos = utxos,
          lotteryUtxo = lotteryUtxo,
          preimage = wrongPreimage,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, revealTx, lotteryUtxo._1, "Fraudulent attempt")
    }

    test("FAIL: P2 reveal with wrong preimage") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val revealTx = txCreator.revealPlayerTwo(
          utxos = utxos,
          lotteryUtxo = lotteryUtxo,
          preimage = wrongPreimage,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, revealTx, lotteryUtxo._1, "Fraudulent attempt")
    }

    // --- Tampered secrets / deadline ---

    test("FAIL: P1 reveal attempts to change player two secret") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val maliciousSecret = sha2_256(genByteStringOfN(20).sample.get)

        val revealTx = txCreator.revealPlayerOne(
          utxos = utxos,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = maliciousSecret,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, revealTx, lotteryUtxo._1, "Player two secret must not change")
    }

    test("FAIL: P2 reveal attempts to change deadline") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val revealTx = txCreator.revealPlayerTwo(
          utxos = utxos,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage2,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = afterDeadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, revealTx, lotteryUtxo._1, "Reveal deadline must not change")
    }

    // --- Second reveal (win) ---

    test("P2 reveals with even sum (32 + 16 = 48) -- P2 wins") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P1 reveals first (32 bytes)
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p1RevealTx).await()

        val lotteryUtxo2 = Utxo(p1RevealTx.utxos.find(_._2.address == scriptAddress).get)

        // P2 reveals (16 bytes) -> 32 + 16 = 48 (even) -> P2 wins
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage2,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, p2RevealTx, lotteryUtxo2._1)
    }

    test("P1 reveals with even sum after P2 revealed -- P1 wins") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P2 reveals first (16 bytes)
        val utxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage2,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p2RevealTx).await()

        val lotteryUtxo2 = Utxo(p2RevealTx.utxos.find(_._2.address == scriptAddress).get)

        // P1 reveals (32 bytes) -> 16 + 32 = 48 (even) -> P1 wins
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, p1RevealTx, lotteryUtxo2._1)
    }

    // --- Odd sum (Unlucky) ---

    test("FAIL: P2 reveals with odd sum (32 + 17 = 49) -- Unlucky") {
        val provider = createProvider()

        val oddPreimage = genByteStringOfN(17).sample.get
        val oddSecret = sha2_256(oddPreimage)

        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get

        val initiateTx = txCreator.initiateLottery(
          playerOneUtxos = aliceUtxos,
          playerTwoUtxos = bobUtxos,
          betAmount = betAmount,
          playerOnePkh = Alice.addrKeyHash,
          playerTwoPkh = Bob.addrKeyHash,
          secret1 = validSecret1,
          secret2 = oddSecret,
          revealDeadline = deadline.toEpochMilli,
          changeAddress = Alice.address,
          playerOneSigner = Alice.signer,
          playerTwoSigner = Bob.signer
        )
        provider.submit(initiateTx).await()
        val lotteryUtxo = Utxo(initiateTx.utxos.find(_._2.address == scriptAddress).get)

        // P1 reveals (32 bytes)
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = oddSecret,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p1RevealTx).await()

        val lotteryUtxo2 = Utxo(p1RevealTx.utxos.find(_._2.address == scriptAddress).get)

        // P2 reveals (17 bytes) -> 32 + 17 = 49 (odd) -> Unlucky
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = oddPreimage,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = oddSecret,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, p2RevealTx, lotteryUtxo2._1, "Unlucky")
    }

    // --- Concede (Lose) ---

    test("P2 concedes after P1 revealed -- P1 gets pot") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p1RevealTx).await()
        val lotteryUtxo2 = Utxo(p1RevealTx.utxos.find(_._2.address == scriptAddress).get)

        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val loseTx = txCreator.lose(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage2,
          loserPkh = Bob.addrKeyHash,
          winnerAddress = Alice.address,
          winnerOutputIdx = BigInt(0),
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, loseTx, lotteryUtxo2._1)
    }

    test("P1 concedes after P2 revealed -- P2 gets pot") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        val utxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage2,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p2RevealTx).await()
        val lotteryUtxo2 = Utxo(p2RevealTx.utxos.find(_._2.address == scriptAddress).get)

        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val loseTx = txCreator.lose(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage1,
          loserPkh = Alice.addrKeyHash,
          winnerAddress = Bob.address,
          winnerOutputIdx = BigInt(0),
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, loseTx, lotteryUtxo2._1)
    }

    test("FAIL: P2 lose with wrong preimage") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p1RevealTx).await()
        val lotteryUtxo2 = Utxo(p1RevealTx.utxos.find(_._2.address == scriptAddress).get)

        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val loseTx = txCreator.lose(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = wrongPreimage,
          loserPkh = Bob.addrKeyHash,
          winnerAddress = Alice.address,
          winnerOutputIdx = BigInt(0),
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, loseTx, lotteryUtxo2._1, "Fraudulent attempt")
    }

    // --- Timeout ---

    test("P1 timeout after P1 revealed but P2 didn't reveal") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p1RevealTx).await()
        val lotteryUtxo2 = Utxo(p1RevealTx.utxos.find(_._2.address == scriptAddress).get)

        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val timeoutTx = txCreator.timeout(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage1,
          claimantPkh = Alice.addrKeyHash,
          payeeAddress = Alice.address,
          sponsor = Alice.address,
          validFrom = afterDeadline,
          signer = Alice.signer
        )

        provider.setSlot(afterSlot)
        assertSuccess(provider, timeoutTx, lotteryUtxo2._1)
    }

    test("P2 timeout after P2 revealed but P1 didn't reveal") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        val utxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage2,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p2RevealTx).await()
        val lotteryUtxo2 = Utxo(p2RevealTx.utxos.find(_._2.address == scriptAddress).get)

        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val timeoutTx = txCreator.timeout(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage2,
          claimantPkh = Bob.addrKeyHash,
          payeeAddress = Bob.address,
          sponsor = Bob.address,
          validFrom = afterDeadline,
          signer = Bob.signer
        )

        provider.setSlot(afterSlot)
        assertSuccess(provider, timeoutTx, lotteryUtxo2._1)
    }

    test("FAIL: Timeout before deadline") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        val utxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage2,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p2RevealTx).await()
        val lotteryUtxo2 = Utxo(p2RevealTx.utxos.find(_._2.address == scriptAddress).get)

        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val timeoutTx = txCreator.timeout(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage2,
          claimantPkh = Bob.addrKeyHash,
          payeeAddress = Bob.address,
          sponsor = Bob.address,
          validFrom = beforeDeadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, timeoutTx, lotteryUtxo2._1, "Deadline not reached")
    }

    test("FAIL: P1 timeout with wrong preimage") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p1RevealTx).await()
        val lotteryUtxo2 = Utxo(p1RevealTx.utxos.find(_._2.address == scriptAddress).get)

        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val timeoutTx = txCreator.timeout(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = wrongPreimage,
          claimantPkh = Alice.addrKeyHash,
          payeeAddress = Alice.address,
          sponsor = Alice.address,
          validFrom = afterDeadline,
          signer = Alice.signer
        )

        provider.setSlot(afterSlot)
        assertFailure(provider, timeoutTx, lotteryUtxo2._1, "Fraudulent attempt")
    }

    // --- Second reveal with wrong preimage ---

    test("FAIL: P2 second reveal with wrong preimage") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )
        provider.setSlot(beforeSlot)
        provider.submit(p1RevealTx).await()
        val lotteryUtxo2 = Utxo(p1RevealTx.utxos.find(_._2.address == scriptAddress).get)

        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = wrongPreimage,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, p2RevealTx, lotteryUtxo2._1, "Fraudulent attempt")
    }

    // --- Boundary: reveal at deadline ---

    test("P1 reveal succeeds at deadline (boundary)") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val revealTx = txCreator.revealPlayerOne(
          utxos = utxos,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          sponsor = Alice.address,
          validTo = afterDeadline,
          signer = Alice.signer
        )

        provider.setSlot(deadlineSlot)
        assertSuccess(provider, revealTx, lotteryUtxo._1)
    }
}

object LotteryTest extends ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val compiledContract = LotteryContract.compiled.withErrorTraces
    val scriptAddress = compiledContract.address(env.network)

    private val txCreator = LotteryTransactions(
      env = env,
      evaluator = PlutusScriptEvaluator.constMaxBudget(env),
      contract = compiledContract
    )

    // Time constants
    private val deadlineSlot: SlotNo = 10
    private val beforeSlot: SlotNo = deadlineSlot - 1
    private val afterSlot: SlotNo = deadlineSlot + 1
    private val deadline: Instant = env.slotConfig.slotToInstant(deadlineSlot)
    private val beforeDeadline: Instant = env.slotConfig.slotToInstant(beforeSlot)
    private val afterDeadline: Instant = env.slotConfig.slotToInstant(afterSlot)

    // Test preimages and secrets
    val validPreimage1: Preimage = genByteStringOfN(32).sample.get
    val validSecret1: Secret = sha2_256(validPreimage1)
    val validPreimage2: Preimage = genByteStringOfN(16).sample.get
    val validSecret2: Secret = sha2_256(validPreimage2)
    val wrongPreimage: Preimage = genByteStringOfN(12).sample.get

    private val betAmount: Coin = Coin(10_000_000L)

    private def createProvider(): Emulator = {
        val initialUtxos = Map(
          TransactionInput(TestUtil.genesisHash, 0) -> TransactionOutput(
            Alice.address,
            Value.ada(5000)
          ),
          TransactionInput(TestUtil.genesisHash, 1) -> TransactionOutput(
            Alice.address,
            Value.ada(5000)
          ),
          TransactionInput(TestUtil.genesisHash, 2) -> TransactionOutput(
            Bob.address,
            Value.ada(5000)
          ),
          TransactionInput(TestUtil.genesisHash, 3) -> TransactionOutput(
            Bob.address,
            Value.ada(5000)
          ),
          TransactionInput(TestUtil.genesisHash, 4) -> TransactionOutput(
            Eve.address,
            Value.ada(10000)
          )
        )
        Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet()
        )
    }

    private def createAndSubmitInitiateTx(provider: Emulator): (Transaction, Utxo) = {
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get

        val initiateTx = txCreator.initiateLottery(
          playerOneUtxos = aliceUtxos,
          playerTwoUtxos = bobUtxos,
          betAmount = betAmount,
          playerOnePkh = Alice.addrKeyHash,
          playerTwoPkh = Bob.addrKeyHash,
          secret1 = validSecret1,
          secret2 = validSecret2,
          revealDeadline = deadline.toEpochMilli,
          changeAddress = Alice.address,
          playerOneSigner = Alice.signer,
          playerTwoSigner = Bob.signer
        )
        provider.submit(initiateTx).await() match {
            case Left(value) => throw AssertionError(s"Couldn't submit initiate tx: $value")
            case Right(_)    => ()
        }
        val lotteryUtxo = initiateTx.utxos.find(_._2.address == scriptAddress).get
        (initiateTx, Utxo(lotteryUtxo))
    }

    private def assertSuccess(
        provider: Emulator,
        tx: Transaction,
        lotteryInput: TransactionInput
    ): Unit = {
        val scriptContext = getScriptContext(provider, tx, lotteryInput)
        val directResult = Try(LotteryContract.compiled.code(scriptContext.toData))
        val evalResult = compiledContract(scriptContext.toData).program.evaluateDebug
        assert(evalResult.isSuccess, s"UPLC evaluation failed: ${evalResult.logs.mkString(", ")}")
        val submissionResult = provider.submit(tx).await()
        directResult.failed.foreach(ex =>
            throw AssertionError(s"Direct validator call failed: ${ex.getMessage}")
        )
        assert(submissionResult.isRight, s"Emulator submission failed: $submissionResult")
    }

    private def assertFailure(
        provider: Emulator,
        tx: Transaction,
        lotteryInput: TransactionInput,
        expectedError: String
    ): Unit = {
        val scriptContext = getScriptContext(provider, tx, lotteryInput)
        val directResult = Try(LotteryContract.compiled.code(scriptContext.toData))
        val submissionResult = provider.submit(tx).await()

        assert(directResult.isFailure, "Direct validator call should have failed but succeeded")
        submissionResult match {
            case Left(SubmitError.ScriptFailure(message, _, _, _)) =>
                assert(
                  message.contains(expectedError),
                  s"Expected error '$expectedError' but got '$message'"
                )
            case Left(SubmitError.ValidationError(message, _)) =>
                assert(
                  message.contains(expectedError),
                  s"Expected error '$expectedError' but got '$message'"
                )
            case Left(other) =>
                throw AssertionError(s"Expected ScriptFailure or ValidationError but got: $other")
            case Right(_) =>
                throw AssertionError("Emulator submission should have failed but succeeded")
        }
    }

    private def getScriptContext(
        provider: Emulator,
        tx: Transaction,
        lotteryInput: TransactionInput
    ): ScriptContext = {
        val utxos = {
            val body = tx.body.value
            val allInputs =
                (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
            provider.findUtxos(allInputs).await().toOption.get
        }
        tx.getScriptContextV3(utxos, RedeemerPurpose.ForSpend(lotteryInput))
    }
}
