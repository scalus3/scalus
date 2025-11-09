package scalus.examples.betting

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.BuilderContext
import scalus.examples.TestUtil
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.uplc.eval.Result
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.uplc.Program
import scalus.cardano.node.LedgerProvider
import scalus.cardano.ledger.rules.*
import scalus.testing.kit.{Mock, ScalusTest}

class BettingTransactionTest extends AnyFunSuite, ScalusTest:

    private val env = TestUtil.testEnvironment

    private val compiledContract = BettingContract.debugCompiledContract
    private val scriptAddress = compiledContract.address(env.network)

    private val player1 = Mock.mockPubKeyHash(8)
    private val player2 = Mock.mockPubKeyHash(16)
    private val oracle = Mock.mockPubKeyHash(32)

    extension (key: PubKeyHash) def address = TestUtil.createTestAddress(key.hash.toHex)

    private val gen = BigInt(64)

    private val provider: LedgerProvider = LedgerProvider(
      initialUtxos = Map(
        Mock.mockTxInput(gen, 0) ->
            TransactionOutput.Babbage(
              address = player1.address,
              value = Value.ada(1_000_000L)
            ),
        Mock.mockTxInput(gen, 1) ->
            TransactionOutput.Babbage(
              address = player2.address,
              value = Value.ada(1_000_000L)
            ),
        Mock.mockTxInput(gen, 2) ->
            TransactionOutput.Babbage(
              address = oracle.address,
              value = Value.ada(1_000_000L)
            )
      ),
      context = Context.testMainnet(),
      validators =
          LedgerProvider.defaultValidators - MissingKeyHashesValidator - ProtocolParamsViewHashesMatchValidator - MissingRequiredDatumsValidator,
      mutators = LedgerProvider.defaultMutators - PlutusScriptsTransactionMutator
    )

    private val betAmount = 1000L
    private val expiration: PosixTime = 1753939940

    private val initBetting: Transaction =
        val tx = new Transactions(
          BuilderContext.withDummyEvaluator(
            env,
            TestUtil.createTestWallet(provider, player1.address)
          ),
          compiledContract
        ).init(
          betAmount,
          player1,
          oracle,
          expiration
        ).toOption
            .get
        assert(provider.submit(tx).isRight)
        tx

    private val initDatum = BetDatum(
      player1,
      PubKeyHash(hex""),
      oracle,
      expiration
    )

    private def initUtxo(
        ledgerProvider: LedgerProvider
    ): Either[RuntimeException, Utxo] = ledgerProvider
        .findUtxo(
          address = scriptAddress,
          transactionId = Some(initBetting.id),
          datum = Some(DatumOption.Inline(initDatum.toData)),
          minAmount = Some(Coin(betAmount))
        )

    // FIXME: assert(initUtxo(provider).toOption.get._2.value.coin == Coin(betAmount))

    private val joinBetting: (Transaction, Result) =
        val snapshot = provider.snapshot()
        val tx = new Transactions(
          BuilderContext.withDummyEvaluator(
            env,
            TestUtil.createTestWallet(snapshot, player2.address)
          ),
          compiledContract
        ).join(
          betAmount,
          player1,
          player2,
          oracle,
          expiration,
          initUtxo(snapshot).toOption.get // ???: provider
        ).toOption
            .get
        (tx, runValidator(tx, snapshot, initUtxo(snapshot))) // ???: provider

    private val joinDatum = initDatum.copy(player2 = player2)

    private def joinUtxo(
        ledgerProvider: LedgerProvider
    ): Either[RuntimeException, Utxo] = ledgerProvider
        .findUtxo(
          address = scriptAddress,
          transactionId = Some(joinBetting._1.id),
          datum = Some(DatumOption.Inline(joinDatum.toData)),
          minAmount = Some(Coin(betAmount + betAmount))
        )

    // FIXME: assert(joinUtxo(provider).toOption.get._2.value.coin == Coin(betAmount))

    private def winBetting(isJoinWin: Boolean, time: PosixTime): (Transaction, Result) =
        val snapshot = provider.snapshot()
        val tx = new Transactions(
          BuilderContext.withDummyEvaluator(
            env,
            TestUtil.createTestWallet(snapshot, oracle.address)
          ),
          compiledContract
        ).win(
          isJoinWin,
          player1,
          player2,
          oracle,
          joinUtxo(snapshot).toOption.get // ???: provider
        ).toOption
            .get
        (tx, runValidator(tx, snapshot, joinUtxo(snapshot), Some(time))) // ???: provider

    private def runValidator(
        tx: Transaction,
        snapshot: LedgerProvider,
        betUtxo: Either[RuntimeException, Utxo],
        time: Option[PosixTime] = None,
    ) =
        assert(betUtxo.isRight)

        time.foreach: posixTime =>
            snapshot.setSlot(env.slotConfig.timeToSlot(posixTime.toLong))

        assert(snapshot.submit(tx).isRight)

        // FIXME: assert(betUtxo.isLeft)

        val inputs =
            val body = tx.body.value
            (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet

        val utxos = provider.findUtxos(inputs).toOption.get

        val scriptContext =
            TestUtil.getScriptContextV3(tx, utxos, betUtxo.toOption.get._1, RedeemerTag.Spend, env)

        val allScripts = AllResolvedScripts.allResolvedPlutusScriptsMap(tx, utxos).toOption.get
        val script = scriptAddress.scriptHashOption.flatMap(allScripts.get).get
        val program = Program.fromCborByteString(script.script)

        program.runWithDebug(scriptContext)
