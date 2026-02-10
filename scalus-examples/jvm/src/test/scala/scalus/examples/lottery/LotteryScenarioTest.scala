package scalus.examples.lottery

import cps.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockchainReader, Emulator}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.*
import scalus.testing.kit.Party
import scalus.testing.kit.Party.*
import scalus.uplc.builtin.Builtins.sha2_256
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

import java.time.Instant
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

/** Scenario exploration test for lottery contract.
  *
  * Explores 5 concurrent lottery games with overlapping players. Each game has unique preimages and
  * secrets. Non-deterministic branching explores reveal, lose, timeout, and wait actions across all
  * games, checking invariants after each step.
  */
class LotteryScenarioTest extends AnyFunSuite {
    import LotteryScenarioTest.*

    test(
      "explore: lottery invariants hold under non-deterministic actions with 5 concurrent games"
    ) {
        val emulator = createEmulator()
        val scenario = async[Scenario] {
            // Create all 5 games
            val gameInfos = games.zipWithIndex.map { case (g, idx) =>
                createGameS(g).await
            }

            Scenario
                .explore(maxDepth = 5) { _ =>
                    async[Scenario] {
                        // Pick a random game
                        val gameIdx = Scenario.sample(Gen.choose(0, 4)).await
                        val info = gameInfos(gameIdx)

                        val action = Scenario
                            .choices(
                              "reveal_p1",
                              "reveal_p2",
                              "lose",
                              "timeout",
                              "wait_short",
                              "wait_long"
                            )
                            .await

                        action match
                            case "reveal_p1"  => tryRevealP1S(info).await
                            case "reveal_p2"  => tryRevealP2S(info).await
                            case "lose"       => tryLoseS(info).await
                            case "timeout"    => tryTimeoutS(info).await
                            case "wait_short" => Scenario.sleep(5).await
                            case "wait_long"  => Scenario.sleep(30).await

                        // Check invariants for ALL games
                        val reader = Scenario.snapshotReader.await
                        gameInfos.foreach { gi =>
                            checkGameInvariantsS(reader, gi).await
                        }
                    }
                }
                .await
        }

        val results = Await.result(Scenario.runAll(emulator)(scenario), Duration(180, "s"))
        val violations = results.flatMap(_._2)
        assert(
          violations.isEmpty,
          s"Found violations: ${violations.map(v => s"${v.message} at ${v.location}")}"
        )
    }
}

object LotteryScenarioTest {
    import Scenario.futureToScenarioConversion

    private val compiledContract = LotteryContract.withErrorTraces
    private val lotteryScript = compiledContract.script
    private val network = Network.Mainnet
    private val scriptAddress = compiledContract.address(network)

    // Deadline and slot timing
    private val deadlineSlot: Long = 20L
    private val beforeDeadlineSlot: Long = deadlineSlot - 5

    // =========================================================================
    // Game definitions — 5 games with overlapping players
    // =========================================================================

    case class GameDef(
        player1: Party,
        player2: Party,
        preimage1Len: Int,
        preimage2Len: Int
    )

    case class GameInfo(
        player1: Party,
        player2: Party,
        preimage1: Preimage,
        preimage2: Preimage,
        secret1: Secret,
        secret2: Secret,
        betAmount: Long,
        revealDeadline: Long // slot-time millis
    )

    private def genByteStringOfN(n: Int): Gen[ByteString] =
        Gen.containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))

    // Games with overlapping players:
    // Game 0: Alice vs Bob (even sum: 32+16=48)
    // Game 1: Alice vs Charles (odd sum: 32+17=49)
    // Game 2: Bob vs Charles (even sum: 16+20=36)
    // Game 3: Dave vs Eve (odd sum: 32+15=47)
    // Game 4: Dave vs Alice (even sum: 20+16=36)
    private val games: IndexedSeq[GameDef] = IndexedSeq(
      GameDef(Alice, Bob, 32, 16),
      GameDef(Alice, Charles, 32, 17),
      GameDef(Bob, Charles, 16, 20),
      GameDef(Dave, Eve, 32, 15),
      GameDef(Dave, Alice, 20, 16)
    )

    private val betAmount = 5_000_000L

    // Generate preimages and secrets for each game
    private val preimagesAndSecrets: IndexedSeq[(Preimage, Preimage, Secret, Secret)] =
        games.map { g =>
            val p1 = genByteStringOfN(g.preimage1Len).sample.get
            val p2 = genByteStringOfN(g.preimage2Len).sample.get
            (p1, p2, sha2_256(p1), sha2_256(p2))
        }

    // =========================================================================
    // Emulator setup
    // =========================================================================

    private def createEmulator(): Emulator = {
        val parties = Seq(Alice, Bob, Charles, Dave, Eve)
        val utxosPerParty = 6
        val addresses = parties.flatMap(p => Seq.fill(utxosPerParty)(p.address(network)))
        val emulator =
            Emulator.withAddresses(addresses, Value.lovelace(50_000_000L))
        emulator.setSlot(beforeDeadlineSlot)
        emulator
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private def findGameUtxo(
        reader: BlockchainReader,
        secrets: (Secret, Secret)
    ): Future[Option[Utxo]] = {
        given scala.concurrent.ExecutionContext = reader.executionContext
        reader.findUtxos(scriptAddress).map(_.getOrElse(Map.empty)).map { utxos =>
            utxos
                .find { (_, output) =>
                    output.inlineDatum.exists { d =>
                        scala.util
                            .Try {
                                val state = d.to[State]
                                state.playerOneSecret == secrets._1 && state.playerTwoSecret == secrets._2
                            }
                            .getOrElse(false)
                    }
                }
                .map((i, o) => Utxo(i, o))
        }
    }

    private def getGameDatum(utxo: Utxo): State =
        utxo.output.requireInlineDatum.to[State]

    // =========================================================================
    // Game creation
    // =========================================================================

    private def createGameS(gameDef: GameDef): Scenario[GameInfo] =
        async[Scenario] {
            val reader = Scenario.snapshotReader.await
            given scala.concurrent.ExecutionContext = reader.executionContext

            val idx = games.indexOf(gameDef)
            val (preimage1, preimage2, secret1, secret2) = preimagesAndSecrets(idx)
            val deadline = reader.cardanoInfo.slotConfig.slotToTime(deadlineSlot)

            val p1Addr = gameDef.player1.address(network)
            val p2Addr = gameDef.player2.address(network)

            val p1Utxos = reader.findUtxos(p1Addr).await.getOrElse(Map.empty)
            val p2Utxos = reader.findUtxos(p2Addr).await.getOrElse(Map.empty)

            val datum = State(
              playerOneSecret = secret1,
              playerTwoSecret = secret2,
              revealDeadline = deadline,
              lotteryState = LotteryState.Empty
            )

            val p1Utxo = Utxo(p1Utxos.head)
            val p2Utxo = Utxo(p2Utxos.head)
            val allUtxos = p1Utxos ++ p2Utxos

            val tx = TxBuilder(reader.cardanoInfo)
                .spend(p1Utxo)
                .spend(p2Utxo)
                .payTo(scriptAddress, Value.lovelace(betAmount * 2), datum)
                .complete(availableUtxos = allUtxos, sponsor = p1Addr)
                .sign(gameDef.player1.signer)
                .sign(gameDef.player2.signer)
                .transaction

            Scenario.submit(tx).await match
                case Right(_)  => ()
                case Left(err) => throw RuntimeException(s"Failed to create game $idx: $err")

            GameInfo(
              player1 = gameDef.player1,
              player2 = gameDef.player2,
              preimage1 = preimage1,
              preimage2 = preimage2,
              secret1 = secret1,
              secret2 = secret2,
              betAmount = betAmount * 2,
              revealDeadline = deadline
            )
        }

    // =========================================================================
    // Actions
    // =========================================================================

    private def revealP1S(info: GameInfo): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        given scala.concurrent.ExecutionContext = reader.executionContext

        val utxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
            .getOrElse(throw RuntimeException("Game not found"))

        val p1Addr = info.player1.address(network)
        val p1Pkh = info.player1.addrKeyHash

        val newLotteryState = LotteryState.PlayerOneRevealed(
          BigInt(info.preimage1.bytes.length),
          PubKeyHash(p1Pkh)
        )
        val newState = State(
          playerOneSecret = info.secret1,
          playerTwoSecret = info.secret2,
          revealDeadline = info.revealDeadline,
          lotteryState = newLotteryState
        )

        val redeemer = Action.RevealPlayerOne(info.preimage1)

        val tx = TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript, Set(p1Pkh))
            .payTo(scriptAddress, utxo.output.value, newState)
            .validTo(Instant.ofEpochMilli(info.revealDeadline))
            .complete(reader, p1Addr)
            .await
            .sign(info.player1.signer)
            .transaction

        Scenario.submit(tx).await match
            case Right(_)  => ()
            case Left(err) => throw RuntimeException(s"RevealP1 failed: $err")
    }

    private def revealP2S(info: GameInfo): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        given scala.concurrent.ExecutionContext = reader.executionContext

        val utxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
            .getOrElse(throw RuntimeException("Game not found"))

        val p2Addr = info.player2.address(network)
        val p2Pkh = info.player2.addrKeyHash

        val newLotteryState = LotteryState.PlayerTwoRevealed(
          BigInt(info.preimage2.bytes.length),
          PubKeyHash(p2Pkh)
        )
        val newState = State(
          playerOneSecret = info.secret1,
          playerTwoSecret = info.secret2,
          revealDeadline = info.revealDeadline,
          lotteryState = newLotteryState
        )

        val redeemer = Action.RevealPlayerTwo(info.preimage2)

        val tx = TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript, Set(p2Pkh))
            .payTo(scriptAddress, utxo.output.value, newState)
            .validTo(Instant.ofEpochMilli(info.revealDeadline))
            .complete(reader, p2Addr)
            .await
            .sign(info.player2.signer)
            .transaction

        Scenario.submit(tx).await match
            case Right(_)  => ()
            case Left(err) => throw RuntimeException(s"RevealP2 failed: $err")
    }

    private def secondRevealP1S(info: GameInfo): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        given scala.concurrent.ExecutionContext = reader.executionContext

        val utxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
            .getOrElse(throw RuntimeException("Game not found"))

        val p1Addr = info.player1.address(network)
        val p1Pkh = info.player1.addrKeyHash
        val redeemer = Action.RevealPlayerOne(info.preimage1)

        val tx = TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript, Set(p1Pkh))
            .payTo(p1Addr, utxo.output.value)
            .validTo(Instant.ofEpochMilli(info.revealDeadline))
            .complete(reader, p1Addr)
            .await
            .sign(info.player1.signer)
            .transaction

        Scenario.submit(tx).await match
            case Right(_)  => ()
            case Left(err) => throw RuntimeException(s"SecondRevealP1 failed: $err")
    }

    private def secondRevealP2S(info: GameInfo): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        given scala.concurrent.ExecutionContext = reader.executionContext

        val utxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
            .getOrElse(throw RuntimeException("Game not found"))

        val p2Addr = info.player2.address(network)
        val p2Pkh = info.player2.addrKeyHash
        val redeemer = Action.RevealPlayerTwo(info.preimage2)

        val tx = TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript, Set(p2Pkh))
            .payTo(p2Addr, utxo.output.value)
            .validTo(Instant.ofEpochMilli(info.revealDeadline))
            .complete(reader, p2Addr)
            .await
            .sign(info.player2.signer)
            .transaction

        Scenario.submit(tx).await match
            case Right(_)  => ()
            case Left(err) => throw RuntimeException(s"SecondRevealP2 failed: $err")
    }

    private def loseS(info: GameInfo): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        given scala.concurrent.ExecutionContext = reader.executionContext

        val utxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
            .getOrElse(throw RuntimeException("Game not found"))
        val datum = getGameDatum(utxo)

        val (loserParty, loserPreimage, winnerAddr) = datum.lotteryState match
            case LotteryState.PlayerOneRevealed(_, _) =>
                (info.player2, info.preimage2, info.player1.address(network))
            case LotteryState.PlayerTwoRevealed(_, _) =>
                (info.player1, info.preimage1, info.player2.address(network))
            case _ => throw RuntimeException("Cannot lose from Empty state")

        val loserAddr = loserParty.address(network)
        val loserPkh = loserParty.addrKeyHash

        val redeemer: Transaction => Data = { (tx: Transaction) =>
            val winnerOutputIdx =
                tx.body.value.outputs.indexWhere(_.value.address == winnerAddr)
            Action.Lose(loserPreimage, BigInt(winnerOutputIdx)).toData
        }

        val tx = TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript, Set(loserPkh))
            .payTo(winnerAddr, utxo.output.value)
            .complete(reader, loserAddr)
            .await
            .sign(loserParty.signer)
            .transaction

        Scenario.submit(tx).await match
            case Right(_)  => ()
            case Left(err) => throw RuntimeException(s"Lose failed: $err")
    }

    private def timeoutS(info: GameInfo): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        given scala.concurrent.ExecutionContext = reader.executionContext

        val utxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
            .getOrElse(throw RuntimeException("Game not found"))
        val datum = getGameDatum(utxo)

        val (claimantParty, claimantPreimage) = datum.lotteryState match
            case LotteryState.PlayerOneRevealed(_, _) =>
                (info.player1, info.preimage1)
            case LotteryState.PlayerTwoRevealed(_, _) =>
                (info.player2, info.preimage2)
            case _ => throw RuntimeException("Cannot timeout from Empty state")

        val claimantAddr = claimantParty.address(network)
        val claimantPkh = claimantParty.addrKeyHash
        val redeemer = Action.Timeout(claimantPreimage)

        val tx = TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript, Set(claimantPkh))
            .payTo(claimantAddr, utxo.output.value)
            .validFrom(Instant.ofEpochMilli(info.revealDeadline + 1000))
            .complete(reader, claimantAddr)
            .await
            .sign(claimantParty.signer)
            .transaction

        Scenario.submit(tx).await match
            case Right(_)  => ()
            case Left(err) => throw RuntimeException(s"Timeout failed: $err")
    }

    // =========================================================================
    // Try-action wrappers (precondition checking)
    // =========================================================================

    private def tryActionS(
        actionName: String,
        shouldSucceed: Scenario[Boolean],
        action: Scenario[Unit]
    ): Scenario[Unit] = async[Scenario] {
        val expected = shouldSucceed.await
        Scenario.scenarioLogicMonad
            .flatMapTry(action) {
                case Success(_) =>
                    Scenario.check(expected, s"$actionName succeeded but preconditions not met")
                case Failure(ex) =>
                    Scenario.check(
                      !expected,
                      s"$actionName failed but preconditions were met: ${ex.getMessage}"
                    )
            }
            .await
    }

    private def tryRevealP1S(info: GameInfo): Scenario[Unit] = tryActionS(
      "reveal_p1",
      shouldSucceed = async[Scenario] {
          val reader = Scenario.snapshotReader.await
          val maybeUtxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
          val slotTime =
              reader.cardanoInfo.slotConfig.slotToTime(reader.currentSlot.await)
          maybeUtxo.exists { utxo =>
              val datum = getGameDatum(utxo)
              val beforeDeadline = slotTime < info.revealDeadline
              datum.lotteryState match
                  case LotteryState.Empty => beforeDeadline
                  case LotteryState.PlayerTwoRevealed(p2Len, _) =>
                      val totalLen = p2Len.toInt + info.preimage1.bytes.length
                      beforeDeadline && (totalLen % 2 == 0)
                  case _ => false
          }
      },
      action = async[Scenario] {
          val reader = Scenario.snapshotReader.await
          val maybeUtxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
          maybeUtxo match
              case Some(utxo) =>
                  val datum = getGameDatum(utxo)
                  datum.lotteryState match
                      case LotteryState.Empty                   => revealP1S(info).await
                      case LotteryState.PlayerTwoRevealed(_, _) => secondRevealP1S(info).await
                      case _ => throw RuntimeException("Cannot reveal P1 in this state")
              case None => throw RuntimeException("Game not found")
      }
    )

    private def tryRevealP2S(info: GameInfo): Scenario[Unit] = tryActionS(
      "reveal_p2",
      shouldSucceed = async[Scenario] {
          val reader = Scenario.snapshotReader.await
          val maybeUtxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
          val slotTime =
              reader.cardanoInfo.slotConfig.slotToTime(reader.currentSlot.await)
          maybeUtxo.exists { utxo =>
              val datum = getGameDatum(utxo)
              val beforeDeadline = slotTime < info.revealDeadline
              datum.lotteryState match
                  case LotteryState.Empty => beforeDeadline
                  case LotteryState.PlayerOneRevealed(p1Len, _) =>
                      val totalLen = p1Len.toInt + info.preimage2.bytes.length
                      beforeDeadline && (totalLen % 2 == 0)
                  case _ => false
          }
      },
      action = async[Scenario] {
          val reader = Scenario.snapshotReader.await
          val maybeUtxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
          maybeUtxo match
              case Some(utxo) =>
                  val datum = getGameDatum(utxo)
                  datum.lotteryState match
                      case LotteryState.Empty                   => revealP2S(info).await
                      case LotteryState.PlayerOneRevealed(_, _) => secondRevealP2S(info).await
                      case _ => throw RuntimeException("Cannot reveal P2 in this state")
              case None => throw RuntimeException("Game not found")
      }
    )

    private def tryLoseS(info: GameInfo): Scenario[Unit] = tryActionS(
      "lose",
      shouldSucceed = async[Scenario] {
          val reader = Scenario.snapshotReader.await
          val maybeUtxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
          maybeUtxo.exists { utxo =>
              val datum = getGameDatum(utxo)
              datum.lotteryState match
                  case LotteryState.PlayerOneRevealed(_, _) => true
                  case LotteryState.PlayerTwoRevealed(_, _) => true
                  case _                                    => false
          }
      },
      action = loseS(info)
    )

    private def tryTimeoutS(info: GameInfo): Scenario[Unit] = tryActionS(
      "timeout",
      shouldSucceed = async[Scenario] {
          val reader = Scenario.snapshotReader.await
          val slotTime =
              reader.cardanoInfo.slotConfig.slotToTime(reader.currentSlot.await)
          val pastDeadline = slotTime > info.revealDeadline
          val maybeUtxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
          pastDeadline && maybeUtxo.exists { utxo =>
              val datum = getGameDatum(utxo)
              datum.lotteryState match
                  case LotteryState.PlayerOneRevealed(_, _) => true
                  case LotteryState.PlayerTwoRevealed(_, _) => true
                  case _                                    => false
          }
      },
      action = timeoutS(info)
    )

    // =========================================================================
    // Invariant checking
    // =========================================================================

    private def checkGameInvariantsS(reader: BlockchainReader, info: GameInfo): Scenario[Unit] =
        async[Scenario] {
            val maybeUtxo = findGameUtxo(reader, (info.secret1, info.secret2)).await
            maybeUtxo match
                case Some(utxo) =>
                    val datum = getGameDatum(utxo)
                    Scenario
                        .check(
                          datum.playerOneSecret == info.secret1,
                          "playerOneSecret must not change"
                        )
                        .await
                    Scenario
                        .check(
                          datum.playerTwoSecret == info.secret2,
                          "playerTwoSecret must not change"
                        )
                        .await
                    Scenario
                        .check(
                          datum.revealDeadline == info.revealDeadline,
                          "revealDeadline must not change"
                        )
                        .await
                    Scenario
                        .check(
                          utxo.output.value.coin.value >= info.betAmount,
                          "pot value must be at least original bet"
                        )
                        .await
                case None =>
                    () // game consumed (completed) is ok
        }
}
