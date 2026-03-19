package scalus.examples.lottery

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockchainReader, Emulator}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.cardano.wallet.hd.{HdAccount, HdKeyPair}
import scalus.crypto.ed25519.given
import scalus.cardano.txbuilder.TxBuilderException
import scalus.testing.*
import scalus.uplc.builtin.Builtins.sha2_256
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}

import java.time.Instant
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** ScalaCheck Commands property-based test for lottery contract with many concurrent games.
  *
  * Creates 10 lottery games with overlapping HD-derived participants. Uses
  * ContractScalaCheckCommands to generate random sequences of actions (reveal, lose, timeout,
  * wait), verifying invariants hold after each successful transaction.
  */
class LotteryScalaCheckCommandTest extends AnyFunSuite {
    import LotteryScalaCheckCommandTest.*

    private given ExecutionContext = ExecutionContext.global

    test("lottery: invariants hold under random action sequences with many concurrent games") {
        val (emulator, gameInfos) = createEmulatorWithGames()
        val step = makeLotteryStep(gameInfos)

        val commands = ContractScalaCheckCommands(emulator, step) { (reader, state) =>
            Future.successful {
                val props = state.games.values.map { gameOnChain =>
                    val datum = gameOnChain.datum
                    val info = gameOnChain.info
                    Prop(datum.playerOneSecret == info.secret1) :| "secret1 unchanged" &&
                    Prop(datum.playerTwoSecret == info.secret2) :| "secret2 unchanged" &&
                    Prop(datum.revealDeadline == info.revealDeadline) :| "deadline unchanged" &&
                    Prop(
                      gameOnChain.utxo.output.value.coin.value >= info.betAmount
                    ) :| "pot >= bet"
                }
                if props.isEmpty then Prop.passed
                else props.reduce(_ && _)
            }
        }

        val result = org.scalacheck.Test.check(
          org.scalacheck.Test.Parameters.default
              .withMinSuccessfulTests(15)
              .withMaxDiscardRatio(20),
          commands.property()
        )

        assert(result.passed, s"Property test failed: $result")
    }
}

object LotteryScalaCheckCommandTest {
    private val compiledContract = LotteryContract.compiled.withErrorTraces
    private val lotteryScript = compiledContract.script
    private val network = Network.Mainnet
    private val scriptAddress = compiledContract.address(network)

    private val deadlineSlot: Long = 50L
    private val beforeDeadlineSlot: Long = deadlineSlot - 10
    private val betAmount = 5_000_000L

    // =========================================================================
    // Participants (10 players via HD derivation)
    // =========================================================================

    case class Participant(index: Int, account: HdAccount) {
        lazy val addrKeyHash: AddrKeyHash = account.paymentKeyHash

        val address: ShelleyAddress =
            ShelleyAddress(
              network,
              ShelleyPaymentPart.Key(account.paymentKeyHash),
              ShelleyDelegationPart.Null
            )

        lazy val signer: TransactionSigner = new TransactionSigner(Set(account.paymentKeyPair))
    }

    private val mnemonic: String =
        "test test test test test test test test test test test test " +
            "test test test test test test test test test test test sauce"

    private val numParticipants = 10

    private val participants: IndexedSeq[Participant] = {
        val masterKey = HdKeyPair.masterFromMnemonic(mnemonic, "")
        val purposeKey = masterKey.deriveHardened(1852)
        val coinTypeKey = purposeKey.deriveHardened(1815)

        (0 until numParticipants).map { i =>
            val accountKey = coinTypeKey.deriveHardened(i)
            Participant(i, new HdAccount(i, accountKey))
        }
    }

    // =========================================================================
    // Game definitions — 10 games with overlapping players
    // =========================================================================

    case class GameInfo(
        secret1: Secret,
        secret2: Secret,
        preimage1: Preimage,
        preimage2: Preimage,
        player1: Participant,
        player2: Participant,
        betAmount: Long,
        revealDeadline: Long // slot-time millis
    )

    case class GameOnChain(
        utxo: Utxo,
        datum: State,
        info: GameInfo
    )

    case class LotteryTestState(
        games: Map[(Secret, Secret), GameOnChain]
    )

    private def genByteStringOfN(n: Int): Gen[ByteString] =
        Gen.containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))

    // Pairing: player[i] vs player[(i*3+1) % N] gives varied overlap
    private val gameDefs: IndexedSeq[(Int, Int, Int, Int)] = (0 until 10).map { i =>
        val p1Idx = i % numParticipants
        val p2Idx = (i * 3 + 1) % numParticipants
        val p2Fixed = if p2Idx == p1Idx then (p1Idx + 1) % numParticipants else p2Idx
        val len1 = 32
        val len2 = if i % 3 == 0 then 16 else if i % 3 == 1 then 17 else 20
        (p1Idx, p2Fixed, len1, len2)
    }

    private val gamePreimages: IndexedSeq[(Preimage, Preimage, Secret, Secret)] =
        gameDefs.map { case (_, _, len1, len2) =>
            val p1 = genByteStringOfN(len1).sample.get
            val p2 = genByteStringOfN(len2).sample.get
            (p1, p2, sha2_256(p1), sha2_256(p2))
        }

    // =========================================================================
    // Helpers
    // =========================================================================

    private def findAllGameUtxos(
        reader: BlockchainReader
    )(using ExecutionContext): Future[Map[(Secret, Secret), Utxo]] =
        reader.findUtxos(scriptAddress).map(_.getOrElse(Map.empty)).map { utxos =>
            utxos.flatMap { (input, output) =>
                output.inlineDatum.flatMap { d =>
                    scala.util.Try {
                        val state = d.to[State]
                        (state.playerOneSecret, state.playerTwoSecret) -> Utxo(input, output)
                    }.toOption
                }
            }
        }

    // =========================================================================
    // Actors
    // =========================================================================

    /** Actor that reveals player 1's preimage in a game where no one has revealed yet. */
    class RevealP1Actor(info: GameInfo) extends ContractTestActor[LotteryTestState] {
        override def name: String = s"reveal-p1-${info.player1.index}"

        override def actions(reader: BlockchainReader, state: LotteryTestState)(using
            ExecutionContext
        ): Future[Seq[StepAction]] =
            reader.currentSlot.flatMap { currentSlot =>
                val slotTime = reader.cardanoInfo.slotConfig.slotToTime(currentSlot)
                val secrets = (info.secret1, info.secret2)
                state.games.get(secrets) match
                    case Some(game)
                        if game.datum.lotteryState == LotteryState.Empty &&
                            slotTime < info.revealDeadline =>
                        buildRevealP1Tx(reader, game.utxo, info)
                            .map(tx => Seq(StepAction.Submit(tx)))
                            .recover { case _: TxBuilderException => Seq.empty }
                    case _ => Future.successful(Seq.empty)
            }
    }

    /** Actor that reveals player 2's preimage in a game where no one has revealed yet. */
    class RevealP2Actor(info: GameInfo) extends ContractTestActor[LotteryTestState] {
        override def name: String = s"reveal-p2-${info.player2.index}"

        override def actions(reader: BlockchainReader, state: LotteryTestState)(using
            ExecutionContext
        ): Future[Seq[StepAction]] =
            reader.currentSlot.flatMap { currentSlot =>
                val slotTime = reader.cardanoInfo.slotConfig.slotToTime(currentSlot)
                val secrets = (info.secret1, info.secret2)
                state.games.get(secrets) match
                    case Some(game)
                        if game.datum.lotteryState == LotteryState.Empty &&
                            slotTime < info.revealDeadline =>
                        buildRevealP2Tx(reader, game.utxo, info)
                            .map(tx => Seq(StepAction.Submit(tx)))
                            .recover { case _: TxBuilderException => Seq.empty }
                    case _ => Future.successful(Seq.empty)
            }
    }

    /** Actor that reveals the second player after one has already revealed, or claims a lose. */
    class SecondRevealOrLoseActor(
        info: GameInfo,
        player: Participant,
        preimage: Preimage,
        opponentAddr: ShelleyAddress,
        checkRevealed: LotteryState => Option[BigInt]
    ) extends ContractTestActor[LotteryTestState] {
        override def name: String = s"second-reveal-or-lose-${player.index}"

        override def actions(reader: BlockchainReader, state: LotteryTestState)(using
            ExecutionContext
        ): Future[Seq[StepAction]] =
            reader.currentSlot.flatMap { currentSlot =>
                val slotTime = reader.cardanoInfo.slotConfig.slotToTime(currentSlot)
                val secrets = (info.secret1, info.secret2)
                state.games.get(secrets) match
                    case Some(game) if slotTime < info.revealDeadline =>
                        checkRevealed(game.datum.lotteryState) match
                            case Some(revealedLen) =>
                                val txs = Seq.newBuilder[Future[Option[Transaction]]]
                                val totalLen = revealedLen.toInt + preimage.bytes.length
                                if totalLen % 2 == 0 then
                                    val buildSecondReveal =
                                        if player == info.player1 then buildSecondRevealP1Tx
                                        else buildSecondRevealP2Tx
                                    txs += buildSecondReveal(reader, game.utxo, info)
                                        .map(Some(_))
                                        .recover { case _: TxBuilderException => None }
                                txs += buildLoseTx(
                                  reader,
                                  game.utxo,
                                  info,
                                  loser = player,
                                  loserPreimage = preimage,
                                  winnerAddr = opponentAddr
                                ).map(Some(_)).recover { case _: TxBuilderException => None }
                                Future
                                    .sequence(txs.result())
                                    .map(_.flatten.map(StepAction.Submit(_)))
                            case None => Future.successful(Seq.empty)
                    case _ => Future.successful(Seq.empty)
            }
    }

    /** Actor that claims a timeout after the deadline has passed. */
    class TimeoutActor(info: GameInfo, claimant: Participant, claimantPreimage: Preimage)
        extends ContractTestActor[LotteryTestState] {
        override def name: String = s"timeout-${claimant.index}"

        override def actions(reader: BlockchainReader, state: LotteryTestState)(using
            ExecutionContext
        ): Future[Seq[StepAction]] =
            reader.currentSlot.flatMap { currentSlot =>
                val slotTime = reader.cardanoInfo.slotConfig.slotToTime(currentSlot)
                val secrets = (info.secret1, info.secret2)
                state.games.get(secrets) match
                    case Some(game)
                        if slotTime > info.revealDeadline &&
                            game.datum.lotteryState != LotteryState.Empty =>
                        buildTimeoutTx(reader, game.utxo, info, claimant, claimantPreimage)
                            .map(tx => Seq(StepAction.Submit(tx)))
                            .recover { case _: TxBuilderException => Seq.empty }
                    case _ => Future.successful(Seq.empty)
            }
    }

    // =========================================================================
    // Step (built from actors)
    // =========================================================================

    private def makeLotteryStep(
        allGameInfos: IndexedSeq[GameInfo]
    ): ContractStepVariations[LotteryTestState] = {
        val gameInfoBySecrets: Map[(Secret, Secret), GameInfo] =
            allGameInfos.map(g => (g.secret1, g.secret2) -> g).toMap

        // Limit to 3 active games per step to keep the action space manageable
        val activeGameInfos = allGameInfos.take(3)

        val actors: Seq[ContractTestActor[LotteryTestState]] = activeGameInfos.flatMap { info =>
            Seq(
              new RevealP1Actor(info),
              new RevealP2Actor(info),
              new SecondRevealOrLoseActor(
                info,
                player = info.player2,
                preimage = info.preimage2,
                opponentAddr = info.player1.address,
                checkRevealed = {
                    case LotteryState.PlayerOneRevealed(len, _) => Some(len)
                    case _                                      => None
                }
              ),
              new SecondRevealOrLoseActor(
                info,
                player = info.player1,
                preimage = info.preimage1,
                opponentAddr = info.player2.address,
                checkRevealed = {
                    case LotteryState.PlayerTwoRevealed(len, _) => Some(len)
                    case _                                      => None
                }
              ),
              new TimeoutActor(info, info.player1, info.preimage1),
              new TimeoutActor(info, info.player2, info.preimage2)
            )
        }

        ContractStepVariations.fromActors[LotteryTestState](
          extract = reader =>
              findAllGameUtxos(reader).map { utxoMap =>
                  val games = utxoMap.flatMap { case (secrets, utxo) =>
                      gameInfoBySecrets.get(secrets).map { info =>
                          val datum = utxo.output.requireInlineDatum.to[State]
                          secrets -> GameOnChain(utxo, datum, info)
                      }
                  }
                  LotteryTestState(games)
              },
          actors = actors,
          delays = _ => Seq(5L, 30L)
        )
    }

    // =========================================================================
    // Transaction builders
    // =========================================================================

    private def buildRevealP1Tx(
        reader: BlockchainReader,
        utxo: Utxo,
        info: GameInfo
    )(using ExecutionContext): Future[Transaction] = {
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

        TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript)
            .requireSignature(p1Pkh)
            .payTo(scriptAddress, utxo.output.value, newState)
            .validTo(Instant.ofEpochMilli(info.revealDeadline))
            .complete(reader, info.player1.address)
            .map(_.sign(info.player1.signer).transaction)
    }

    private def buildRevealP2Tx(
        reader: BlockchainReader,
        utxo: Utxo,
        info: GameInfo
    )(using ExecutionContext): Future[Transaction] = {
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

        TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript)
            .requireSignature(p2Pkh)
            .payTo(scriptAddress, utxo.output.value, newState)
            .validTo(Instant.ofEpochMilli(info.revealDeadline))
            .complete(reader, info.player2.address)
            .map(_.sign(info.player2.signer).transaction)
    }

    private def buildSecondRevealP1Tx(
        reader: BlockchainReader,
        utxo: Utxo,
        info: GameInfo
    )(using ExecutionContext): Future[Transaction] = {
        val p1Pkh = info.player1.addrKeyHash
        val redeemer = Action.RevealPlayerOne(info.preimage1)

        TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript)
            .requireSignature(p1Pkh)
            .payTo(info.player1.address, utxo.output.value)
            .validTo(Instant.ofEpochMilli(info.revealDeadline))
            .complete(reader, info.player1.address)
            .map(_.sign(info.player1.signer).transaction)
    }

    private def buildSecondRevealP2Tx(
        reader: BlockchainReader,
        utxo: Utxo,
        info: GameInfo
    )(using ExecutionContext): Future[Transaction] = {
        val p2Pkh = info.player2.addrKeyHash
        val redeemer = Action.RevealPlayerTwo(info.preimage2)

        TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript)
            .requireSignature(p2Pkh)
            .payTo(info.player2.address, utxo.output.value)
            .validTo(Instant.ofEpochMilli(info.revealDeadline))
            .complete(reader, info.player2.address)
            .map(_.sign(info.player2.signer).transaction)
    }

    private def buildLoseTx(
        reader: BlockchainReader,
        utxo: Utxo,
        info: GameInfo,
        loser: Participant,
        loserPreimage: Preimage,
        winnerAddr: ShelleyAddress
    )(using ExecutionContext): Future[Transaction] = {
        val loserPkh = loser.addrKeyHash

        val redeemer: Transaction => Data = { (tx: Transaction) =>
            val winnerOutputIdx =
                tx.body.value.outputs.indexWhere(_.value.address == winnerAddr)
            Action.Lose(loserPreimage, BigInt(winnerOutputIdx)).toData
        }

        TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript)
            .requireSignature(loserPkh)
            .payTo(winnerAddr, utxo.output.value)
            .complete(reader, loser.address)
            .map(_.sign(loser.signer).transaction)
    }

    private def buildTimeoutTx(
        reader: BlockchainReader,
        utxo: Utxo,
        info: GameInfo,
        claimant: Participant,
        claimantPreimage: Preimage
    )(using ExecutionContext): Future[Transaction] = {
        val claimantPkh = claimant.addrKeyHash
        val redeemer = Action.Timeout(claimantPreimage)

        TxBuilder(reader.cardanoInfo)
            .spend(utxo, redeemer, lotteryScript)
            .requireSignature(claimantPkh)
            .payTo(claimant.address, utxo.output.value)
            .validFrom(Instant.ofEpochMilli(info.revealDeadline + 1000))
            .complete(reader, claimant.address)
            .map(_.sign(claimant.signer).transaction)
    }

    // =========================================================================
    // Setup
    // =========================================================================

    private def createEmulatorWithGames(): (Emulator, IndexedSeq[GameInfo]) = {
        given ExecutionContext = ExecutionContext.global

        val addresses = participants.flatMap(p => Seq.fill(3)(p.address))
        val emulator = Emulator.withAddresses(addresses, Value.lovelace(50_000_000L))
        emulator.setSlot(beforeDeadlineSlot)

        val deadline = emulator.cardanoInfo.slotConfig.slotToTime(deadlineSlot)

        val gameInfos = gameDefs.zipWithIndex.map { case ((p1Idx, p2Idx, _, _), gameIdx) =>
            val p1 = participants(p1Idx)
            val p2 = participants(p2Idx)
            val (preimage1, preimage2, secret1, secret2) = gamePreimages(gameIdx)

            val datum = State(
              playerOneSecret = secret1,
              playerTwoSecret = secret2,
              revealDeadline = deadline,
              lotteryState = LotteryState.Empty
            )

            val p1Utxos = Await.result(
              emulator.findUtxos(p1.address).map(_.getOrElse(Map.empty)),
              Duration.Inf
            )
            val p2Utxos = Await.result(
              emulator.findUtxos(p2.address).map(_.getOrElse(Map.empty)),
              Duration.Inf
            )

            val p1Utxo = Utxo(p1Utxos.head)
            val p2Utxo = Utxo(p2Utxos.head)
            val allUtxos = p1Utxos ++ p2Utxos

            val tx = TxBuilder(emulator.cardanoInfo)
                .spend(p1Utxo)
                .spend(p2Utxo)
                .payTo(scriptAddress, Value.lovelace(betAmount * 2), datum)
                .complete(availableUtxos = allUtxos, sponsor = p1.address)
                .sign(p1.signer)
                .sign(p2.signer)
                .transaction

            val submitResult = Await.result(emulator.submit(tx), Duration.Inf)
            assert(submitResult.isRight, s"Game $gameIdx creation failed: $submitResult")

            GameInfo(
              secret1 = secret1,
              secret2 = secret2,
              preimage1 = preimage1,
              preimage2 = preimage2,
              player1 = p1,
              player2 = p2,
              betAmount = betAmount * 2,
              revealDeadline = deadline
            )
        }

        (emulator, gameInfos)
    }
}
