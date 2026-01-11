package scalus.examples.lottery

import org.junit.jupiter.api.Assertions.fail
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha2_256
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.cardano.txbuilder.RedeemerPurpose
import scalus.ledger.api.v3.ScriptContext
import scalus.testing.kit.Party.{Alice, Bob, Eve}
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class LotteryValidatorTest extends AnyFunSuite, ScalusTest {
    import LotteryValidatorTest.{*, given}

    test(s"Lottery validator size is ${LotteryContract.script.script.size} bytes") {
        info(s"Validator size: ${LotteryContract.script.script.size} bytes")
    }

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
          validTo = afterDeadline, // validTo is exclusive, so use slot 11 to include slot 10
          signer = Alice.signer
        )

        provider.setSlot(deadlineSlot)
        assertSuccess(provider, revealTx, lotteryUtxo._1)
    }

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

    test("FAIL: P1 reveal attempts to change player two secret") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        // Create a different secret that P1 will try to substitute
        val maliciousSecret = sha2_256(genByteStringOfN(20).sample.get)

        val revealTx = txCreator.revealPlayerOne(
          utxos = utxos,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1,
          playerOnePkh = Alice.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = maliciousSecret, // Trying to change P2's secret!
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

        // Try to extend the deadline
        val extendedDeadline = afterDeadline.toEpochMilli

        val revealTx = txCreator.revealPlayerTwo(
          utxos = utxos,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage2,
          playerTwoPkh = Bob.addrKeyHash,
          playerOneSecret = validSecret1,
          playerTwoSecret = validSecret2,
          revealDeadline = extendedDeadline, // Trying to change deadline!
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, revealTx, lotteryUtxo._1, "Reveal deadline must not change")
    }

    test("P2 reveals with even sum (32 + 16 = 48)") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P1 reveals first (32 bytes)
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1, // 32 bytes
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

        // P2 reveals (16 bytes) -> 32 + 16 = 48 (even) -> succeeds
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage2, // 16 bytes
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

    // Timeout Tests

    test("P2 timeout after P2 revealed but P1 didn't reveal") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P2 reveals first
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

        // P2 (who revealed) claims timeout pot after P1 failed to reveal before deadline
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val timeoutTx = txCreator.timeout(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage2, // P2's preimage (the one who revealed)
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

        // P2 reveals first
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

        // P2 tries to timeout before deadline - should fail
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val timeoutTx = txCreator.timeout(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage2, // P2's preimage (the one who revealed)
          claimantPkh = Bob.addrKeyHash,
          payeeAddress = Bob.address,
          sponsor = Bob.address,
          validFrom = beforeDeadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertFailure(provider, timeoutTx, lotteryUtxo2._1, "Deadline not reached")
    }

    // Lose/Concede

    test("P2 concedes after P1 revealed - P1 gets pot") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P1 reveals first
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

        // P2 concedes - gives pot to P1
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val loseTx = txCreator.lose(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage2,
          loserPkh = Bob.addrKeyHash,
          winnerAddress = Alice.address,
          winnerOutputIdx = BigInt(0), // Assuming first output is to Alice
          sponsor = Bob.address,
          validTo = deadline,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, loseTx, lotteryUtxo2._1)
    }

    test("P1 timeout after P1 revealed but P2 didn't reveal") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P1 reveals first
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

        // P1 (who revealed) claims timeout pot after P2 failed to reveal before deadline
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val timeoutTx = txCreator.timeout(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage1, // P1's preimage (the one who revealed)
          claimantPkh = Alice.addrKeyHash,
          payeeAddress = Alice.address,
          sponsor = Alice.address,
          validFrom = afterDeadline,
          signer = Alice.signer
        )

        provider.setSlot(afterSlot)
        assertSuccess(provider, timeoutTx, lotteryUtxo2._1)
    }

    test("FAIL: P2 reveals with odd sum (32 + 17 = 49) - Unlucky") {
        val provider = createProvider()

        // Create odd-length preimage (17 bytes) for P2
        val oddPreimage = genByteStringOfN(17).sample.get
        val oddSecret = sha2_256(oddPreimage)

        // Create lottery with P1 having 32-byte preimage and P2 having 17-byte preimage
        val aliceUtxos = provider.findUtxos(address = Alice.address).await().toOption.get
        val bobUtxos = provider.findUtxos(address = Bob.address).await().toOption.get

        val initiateTx = txCreator.initiateLottery(
          playerOneUtxos = aliceUtxos,
          playerTwoUtxos = bobUtxos,
          betAmount = betAmount,
          playerOnePkh = Alice.addrKeyHash,
          playerTwoPkh = Bob.addrKeyHash,
          secret1 = validSecret1, // 32-byte preimage
          secret2 = oddSecret, // 17-byte preimage
          revealDeadline = deadline.toEpochMilli,
          changeAddress = Alice.address,
          playerOneSigner = Alice.signer,
          playerTwoSigner = Bob.signer
        )
        provider.submit(initiateTx).await()
        val lotteryUtxo = Utxo(initiateTx.utxos.find(_._2.address == scriptAddress).get)

        // P1 reveals first (32 bytes)
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage1, // 32 bytes
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

        // P2 reveals (17 bytes) -> 32 + 17 = 49 (odd) -> fails with "Unlucky"
        val utxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = oddPreimage, // 17 bytes
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

    test("P1 concedes after P2 revealed - P2 gets pot") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P2 reveals first
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

        // P1 concedes - gives pot to P2
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val loseTx = txCreator.lose(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage1,
          loserPkh = Alice.addrKeyHash,
          winnerAddress = Bob.address,
          winnerOutputIdx = BigInt(0), // Assuming first output is to Bob
          sponsor = Alice.address,
          validTo = deadline,
          signer = Alice.signer
        )

        provider.setSlot(beforeSlot)
        assertSuccess(provider, loseTx, lotteryUtxo2._1)
    }

    test("FAIL: P2 second reveal with wrong preimage") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P1 reveals first
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

        // P2 tries to reveal with wrong preimage
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

    test("FAIL: P2 lose with wrong preimage") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P1 reveals first
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

        // P2 tries to concede with wrong preimage
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

    test("FAIL: P1 timeout with wrong preimage") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P1 reveals first
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

        // P1 tries to timeout with wrong preimage
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

    test("P1 reveals with even sum (32 + 16 = 48) after P2 revealed") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P2 reveals first (16 bytes)
        val utxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val p2RevealTx = txCreator.revealPlayerTwo(
          utxos = utxos1,
          lotteryUtxo = lotteryUtxo,
          preimage = validPreimage2, // 16 bytes
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

        // P1 reveals (32 bytes) -> 16 + 32 = 48 (even) -> succeeds
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val p1RevealTx = txCreator.revealPlayerOne(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage1, // 32 bytes
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
}

object LotteryValidatorTest extends ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val compiledContract = LotteryContract.withErrorTraces
    private val scriptAddress = compiledContract.address(env.network)

    private val txCreator = LotteryTransactionCreator(
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

    // Bet amount
    private val betAmount: Coin = Coin(10_000_000L)

    private def createProvider(): Emulator = {
        // Create multiple UTXOs per player so they have funds after initiate tx
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val initialUtxos = Map(
          // Alice gets 2 UTXOs
          TransactionInput(genesisHash, 0) -> TransactionOutput
              .Babbage(Alice.address, Value.ada(5000)),
          TransactionInput(genesisHash, 1) -> TransactionOutput.Babbage(
            Alice.address,
            Value.ada(5000)
          ),
          // Bob gets 2 UTXOs
          TransactionInput(genesisHash, 2) -> TransactionOutput
              .Babbage(Bob.address, Value.ada(5000)),
          TransactionInput(genesisHash, 3) -> TransactionOutput.Babbage(
            Bob.address,
            Value.ada(5000)
          ),
          // Eve gets 1 UTXO
          TransactionInput(genesisHash, 4) -> TransactionOutput.Babbage(
            Eve.address,
            Value.ada(10000)
          )
        )
        Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet(),
        )
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

    private def createAndSubmitInitiateTx(provider: Emulator): (Transaction, Utxo) = {
        val aliceUtxos = provider.findUtxos(address = Alice.address).await().toOption.get
        val bobUtxos = provider.findUtxos(address = Bob.address).await().toOption.get

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
            case Left(value) => fail(s"Couldn't submit initiate tx, error: ${value}")
            case Right(_)    => ()
        }
        val lotteryUtxo = initiateTx.utxos.find { case (_, txOut) =>
            txOut.address == scriptAddress
        }.get
        (initiateTx, Utxo(lotteryUtxo))
    }

    private def assertSuccess(
        provider: Emulator,
        tx: Transaction,
        lotteryInput: TransactionInput
    ): Unit = {
        val scriptContext = getScriptContext(provider, tx, lotteryInput)
        val directResult = Try(LotteryContract.code(scriptContext.toData))
        val submissionResult = provider.submit(tx).await()

        assert(directResult.isSuccess, s"Direct validator call failed: ${directResult.failed.get}")
        assert(submissionResult.isRight, s"Emulator submission failed: $submissionResult")
    }

    private def assertFailure(
        provider: Emulator,
        tx: Transaction,
        lotteryInput: TransactionInput,
        expectedError: String
    ): Unit = {
        val scriptContext = getScriptContext(provider, tx, lotteryInput)
        val directResult = Try(LotteryContract.code(scriptContext.toData))
        val submissionResult = provider.submit(tx).await()

        assert(directResult.isFailure, "Direct validator call should have failed but succeeded")
        submissionResult match {
            case Left(nodeError: SubmitError.NodeError) =>
                assert(
                  nodeError.message.endsWith(expectedError),
                  s"Expected error '$expectedError' but got '${nodeError.message}'"
                )
            case Left(other) =>
                throw AssertionError(s"Expected NodeError but got: $other")
            case Right(_) =>
                throw AssertionError("Emulator submission should have failed but succeeded")
        }
    }
}
