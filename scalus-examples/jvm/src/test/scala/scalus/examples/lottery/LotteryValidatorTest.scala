package scalus.examples.lottery

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Builtins.sha2_256
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.cardano.txbuilder.{RedeemerPurpose, TxBuilder}
import scalus.cardano.onchain.plutus.v3.PubKeyHash
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.testing.kit.Party.{Alice, Bob, Eve}
import scalus.testing.kit.TestUtil.{genesisHash, getScriptContextV3}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.utils.await

import java.time.Instant
import scala.util.Try

class LotteryValidatorTest extends AnyFunSuite, ScalusTest {
    import LotteryValidatorTest.{*, given}

    test(s"Lottery validator size is ${LotteryContract.compiled.script.script.size} bytes") {
        info(s"Validator size: ${LotteryContract.compiled.script.script.size} bytes")
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
        assertSuccess(
          provider,
          revealTx,
          lotteryUtxo._1,
          ExUnits(memory = 78497, steps = 36_837_286)
        )
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
        assertSuccess(
          provider,
          revealTx,
          lotteryUtxo._1,
          ExUnits(memory = 78497, steps = 36_792_110)
        )
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
        assertSuccess(
          provider,
          revealTx,
          lotteryUtxo._1,
          ExUnits(memory = 78497, steps = 36_837_286)
        )
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
        assertSuccess(
          provider,
          p2RevealTx,
          lotteryUtxo2._1,
          ExUnits(memory = 42163, steps = 16_978_217)
        )
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
        assertSuccess(
          provider,
          timeoutTx,
          lotteryUtxo2._1,
          ExUnits(memory = 59305, steps = 24_945_709)
        )
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
        assertSuccess(
          provider,
          loseTx,
          lotteryUtxo2._1,
          ExUnits(memory = 55943, steps = 24_848_222)
        )
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
        assertSuccess(
          provider,
          timeoutTx,
          lotteryUtxo2._1,
          ExUnits(memory = 59073, steps = 24_877_222)
        )
    }

    test("FAIL: a third party cannot steal the pot via Timeout using the public preimage") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // P1 reveals, publishing validPreimage1 on-chain (it is now public to everyone).
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

        // Eve (neither player) supplies the now-public preimage and directs the pot to herself
        // after the deadline. The validator must reject this: a Timeout must pay the revealer (P1).
        val utxos2 = provider.findUtxos(Eve.address).await().toOption.get
        val theftTx = txCreator.timeout(
          utxos = utxos2,
          lotteryUtxo = lotteryUtxo2,
          preimage = validPreimage1,
          claimantPkh = Eve.addrKeyHash,
          payeeAddress = Eve.address,
          sponsor = Eve.address,
          validFrom = afterDeadline,
          signer = Eve.signer
        )
        provider.setSlot(afterSlot)
        assertFailure(provider, theftTx, lotteryUtxo2._1, "Timeout must pay the revealer")
    }

    test("FAIL: one Timeout payout cannot settle two lottery pots (double satisfaction)") {
        val provider = createProvider()
        // Alice revealed first in two games and Bob answered in neither.
        val game1 = createAndRevealPlayerOne(provider)
        val game2 = createAndRevealPlayerOne(provider)

        // After the deadline Eve spends both pots with the public preimage, pays Alice one pot
        // and keeps the other as change. Each script run would see a 10 ADA output to Alice.
        val utxos = provider.findUtxos(Eve.address).await().toOption.get
        val theftTx = TxBuilder(env, twoScriptsEvaluator)
            .spend(game1, Action.Timeout(validPreimage1), compiledContract)
            .spend(game2, Action.Timeout(validPreimage1), compiledContract)
            .payTo(Alice.address, game1.output.value)
            .validFrom(afterDeadline)
            .complete(availableUtxos = utxos, Eve.address)
            .sign(Eve.signer)
            .transaction
        provider.setSlot(afterSlot)
        assertFailure(provider, theftTx, game1.input, LotteryValidator.OneLotteryInput)
    }

    test("FAIL: one Lose payout cannot settle two lottery pots (double satisfaction)") {
        val provider = createProvider()
        // Alice revealed first in two games, and Bob lost both.
        val game1 = createAndRevealPlayerOne(provider)
        val game2 = createAndRevealPlayerOne(provider)

        // Bob concedes both games, but both redeemers name the same single payout to Alice. Lose
        // checks no signature, so Eve (Bob's funds went into the two bets) submits the transaction
        // and keeps the second pot as change.
        val utxos = provider.findUtxos(Eve.address).await().toOption.get
        val loseTx = TxBuilder(env, twoScriptsEvaluator)
            .spend(game1, Action.Lose(validPreimage2, BigInt(0)), compiledContract)
            .spend(game2, Action.Lose(validPreimage2, BigInt(0)), compiledContract)
            .payTo(Alice.address, game1.output.value)
            .validTo(deadline)
            .complete(availableUtxos = utxos, Eve.address)
            .sign(Eve.signer)
            .transaction
        provider.setSlot(beforeSlot)
        assertFailure(provider, loseTx, game1.input, LotteryValidator.OneLotteryInput)
    }

    test("FAIL: P2-revealed Timeout payout cannot settle two lottery pots (double satisfaction)") {
        val provider = createProvider()
        // Bob revealed first in two games and Alice answered in neither.
        val game1 = createAndRevealPlayerTwo(provider)
        val game2 = createAndRevealPlayerTwo(provider)

        val utxos = provider.findUtxos(Eve.address).await().toOption.get
        val theftTx = TxBuilder(env, twoScriptsEvaluator)
            .spend(game1, Action.Timeout(validPreimage2), compiledContract)
            .spend(game2, Action.Timeout(validPreimage2), compiledContract)
            .payTo(Bob.address, game1.output.value)
            .validFrom(afterDeadline)
            .complete(availableUtxos = utxos, Eve.address)
            .sign(Eve.signer)
            .transaction
        provider.setSlot(afterSlot)
        assertFailure(provider, theftTx, game1.input, LotteryValidator.OneLotteryInput)
    }

    test("FAIL: P2-revealed Lose payout cannot settle two lottery pots (double satisfaction)") {
        val provider = createProvider()
        // Bob revealed first in two games, and Alice lost both.
        val game1 = createAndRevealPlayerTwo(provider)
        val game2 = createAndRevealPlayerTwo(provider)

        val utxos = provider.findUtxos(Eve.address).await().toOption.get
        val loseTx = TxBuilder(env, twoScriptsEvaluator)
            .spend(game1, Action.Lose(validPreimage1, BigInt(0)), compiledContract)
            .spend(game2, Action.Lose(validPreimage1, BigInt(0)), compiledContract)
            .payTo(Bob.address, game1.output.value)
            .validTo(deadline)
            .complete(availableUtxos = utxos, Eve.address)
            .sign(Eve.signer)
            .transaction
        provider.setSlot(beforeSlot)
        assertFailure(provider, loseTx, game1.input, LotteryValidator.OneLotteryInput)
    }

    test("FAIL: P1 first reveal cannot take part of the pot") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        // Alice reveals honestly but leaves only 2 ADA of the 20 ADA pot in the lottery and keeps
        // the rest as change. Lose and Timeout would then pay out only the 2 ADA.
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val drainTx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .spend(lotteryUtxo, Action.RevealPlayerOne(validPreimage1), compiledContract)
            .requireSignature(Alice.addrKeyHash)
            .payTo(scriptAddress, Value.ada(2), revealedState(playerOneRevealed))
            .validTo(deadline)
            .complete(availableUtxos = utxos, Alice.address)
            .sign(Alice.signer)
            .transaction
        provider.setSlot(beforeSlot)
        assertFailure(provider, drainTx, lotteryUtxo.input, LotteryValidator.PotMustStay)
    }

    test("FAIL: P2 first reveal cannot take part of the pot") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)

        val utxos = provider.findUtxos(Bob.address).await().toOption.get
        val drainTx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .spend(lotteryUtxo, Action.RevealPlayerTwo(validPreimage2), compiledContract)
            .requireSignature(Bob.addrKeyHash)
            .payTo(scriptAddress, Value.ada(2), revealedState(playerTwoRevealed))
            .validTo(deadline)
            .complete(availableUtxos = utxos, Bob.address)
            .sign(Bob.signer)
            .transaction
        provider.setSlot(beforeSlot)
        assertFailure(provider, drainTx, lotteryUtxo.input, LotteryValidator.PotMustStay)
    }

    // The revealed datum is larger than the Empty one, so its min-ADA can be higher than the pot:
    // a reveal must be able to add ADA, or a pot locked near min-ADA could never be revealed.
    test("P1 first reveal may add ADA to the pot") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val revealTx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .spend(lotteryUtxo, Action.RevealPlayerOne(validPreimage1), compiledContract)
            .requireSignature(Alice.addrKeyHash)
            .payTo(
              scriptAddress,
              lotteryUtxo.output.value + Value.ada(1),
              revealedState(playerOneRevealed)
            )
            .validTo(deadline)
            .complete(availableUtxos = utxos, Alice.address)
            .sign(Alice.signer)
            .transaction
        provider.setSlot(beforeSlot)
        assertAccepted(provider, revealTx, lotteryUtxo.input)
    }

    test("P2 first reveal may add ADA to the pot") {
        val provider = createProvider()
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get
        val revealTx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .spend(lotteryUtxo, Action.RevealPlayerTwo(validPreimage2), compiledContract)
            .requireSignature(Bob.addrKeyHash)
            .payTo(
              scriptAddress,
              lotteryUtxo.output.value + Value.ada(1),
              revealedState(playerTwoRevealed)
            )
            .validTo(deadline)
            .complete(availableUtxos = utxos, Bob.address)
            .sign(Bob.signer)
            .transaction
        provider.setSlot(beforeSlot)
        assertAccepted(provider, revealTx, lotteryUtxo.input)
    }

    test("FAIL: two first reveals cannot share one continuing output") {
        val provider = createProvider()
        val (_, game1) = createAndSubmitInitiateTx(provider)
        val (_, game2) = createAndSubmitInitiateTx(provider)

        // Alice reveals in both games with one continuing output for one pot and keeps the other
        // pot as change
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val revealTx = TxBuilder(env, twoScriptsEvaluator)
            .spend(game1, Action.RevealPlayerOne(validPreimage1), compiledContract)
            .spend(game2, Action.RevealPlayerOne(validPreimage1), compiledContract)
            .requireSignature(Alice.addrKeyHash)
            .payTo(scriptAddress, game1.output.value, revealedState(playerOneRevealed))
            .validTo(deadline)
            .complete(availableUtxos = utxos, Alice.address)
            .sign(Alice.signer)
            .transaction
        provider.setSlot(beforeSlot)
        assertFailure(provider, revealTx, game1.input, LotteryValidator.OneLotteryInput)
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
        assertSuccess(
          provider,
          loseTx,
          lotteryUtxo2._1,
          ExUnits(memory = 55711, steps = 24_779_735)
        )
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
        assertSuccess(
          provider,
          p1RevealTx,
          lotteryUtxo2._1,
          ExUnits(memory = 41931, steps = 16_909_730)
        )
    }
}

object LotteryValidatorTest extends ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private val compiledContract = LotteryContract.compiled.withErrorTraces
    private val scriptAddress = compiledContract.address(env.network)

    private val txCreator = LotteryTransactions(
      env = env,
      evaluator = PlutusScriptEvaluator.constMaxBudget(env),
      contract = compiledContract
    )

    // Skips script evaluation in the builder for a transaction with two lottery inputs. Each run
    // gets half the transaction limit, so the emulator runs the scripts instead of failing the
    // transaction on its total budget.
    private val twoScriptsEvaluator = PlutusScriptEvaluator.const(
      ExUnits(
        memory = env.protocolParams.maxTxExecutionUnits.memory / 2,
        steps = env.protocolParams.maxTxExecutionUnits.steps / 2
      )
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
        val initialUtxos = Map(
          // Alice gets 2 UTXOs
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(5000)),
          Input(genesisHash, 1) -> Output(
            Alice.address,
            Value.ada(5000)
          ),
          // Bob gets 2 UTXOs
          Input(genesisHash, 2) -> Output(Bob.address, Value.ada(5000)),
          Input(genesisHash, 3) -> Output(
            Bob.address,
            Value.ada(5000)
          ),
          // Eve gets 1 UTXO
          Input(genesisHash, 4) -> Output(Eve.address, Value.ada(10000))
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
            case Left(value) => fail(s"Couldn't submit initiate tx, error: $value")
            case Right(_)    => ()
        }
        val lotteryUtxo = initiateTx.utxos.find { case (_, txOut) =>
            txOut.address == scriptAddress
        }.get
        (initiateTx, Utxo(lotteryUtxo))
    }

    /** Starts a game between Alice and Bob, lets Alice reveal, and returns the lottery UTxO. */
    private def createAndRevealPlayerOne(provider: Emulator): Utxo = {
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
        assert(provider.submit(revealTx).await().isRight, "P1 reveal must succeed")
        Utxo(revealTx.utxos.find(_._2.address == scriptAddress).get)
    }

    /** Starts a game between Alice and Bob, lets Bob reveal, and returns the lottery UTxO.
      *
      * The initiate transaction sends Bob's change to Alice, so Eve pays the reveal fee.
      */
    private def createAndRevealPlayerTwo(provider: Emulator): Utxo = {
        val (_, lotteryUtxo) = createAndSubmitInitiateTx(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get
        val revealTx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .spend(lotteryUtxo, Action.RevealPlayerTwo(validPreimage2), compiledContract)
            .requireSignature(Bob.addrKeyHash)
            .payTo(scriptAddress, lotteryUtxo.output.value, revealedState(playerTwoRevealed))
            .validTo(deadline)
            .complete(availableUtxos = utxos, Eve.address)
            .sign(Bob.signer)
            .sign(Eve.signer)
            .transaction
        provider.setSlot(beforeSlot)
        val result = provider.submit(revealTx).await()
        assert(result.isRight, s"P2 reveal must succeed: $result")
        Utxo(revealTx.utxos.find(_._2.address == scriptAddress).get)
    }

    private val playerOneRevealed = LotteryState.PlayerOneRevealed(
      BigInt(validPreimage1.bytes.length),
      PubKeyHash(Alice.addrKeyHash)
    )
    private val playerTwoRevealed = LotteryState.PlayerTwoRevealed(
      BigInt(validPreimage2.bytes.length),
      PubKeyHash(Bob.addrKeyHash)
    )

    /** The datum of an honest first reveal. */
    private def revealedState(lotteryState: LotteryState): State = State(
      playerOneSecret = validSecret1,
      playerTwoSecret = validSecret2,
      revealDeadline = deadline.toEpochMilli,
      lotteryState = lotteryState
    )

    private def assertSuccess(
        provider: Emulator,
        tx: Transaction,
        lotteryInput: TransactionInput,
        expectedBudget: ExUnits
    ): Unit = {
        val scriptContext = getScriptContext(provider, tx, lotteryInput)
        val directResult = Try(LotteryContract.compiled.code(scriptContext.toData))
        val evalResult = compiledContract(scriptContext.toData).program.evaluateDebug
        assert(evalResult.isSuccess, s"UPLC evaluation failed: ${evalResult.logs.mkString(", ")}")
        assert(evalResult.budget == expectedBudget)
        val submissionResult = provider.submit(tx).await()

        directResult.failed.foreach(ex => fail(s"Direct validator call failed: ${ex.getMessage}"))
        assert(submissionResult.isRight, s"Emulator submission failed: $submissionResult")
    }

    /** Like [[assertSuccess]], for a transaction whose budget no other test pins. */
    private def assertAccepted(
        provider: Emulator,
        tx: Transaction,
        lotteryInput: TransactionInput
    ): Unit = {
        val scriptContext = getScriptContext(provider, tx, lotteryInput)
        val evalResult = compiledContract(scriptContext.toData).program.evaluateDebug
        assert(evalResult.isSuccess, s"UPLC evaluation failed: ${evalResult.logs.mkString(", ")}")
        val submissionResult = provider.submit(tx).await()
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
}
