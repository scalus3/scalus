package scalus.examples.betting

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.txbuilder.{BuilderContext, ExpectedSigner}
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.testing.kit.{Mock, MockLedgerApi, ScalusTest, TestUtil}
import scalus.uplc.Program
import scalus.uplc.eval.Result

class BettingValidatorTransactionTest extends AnyFunSuite, ScalusTest:

    private val env = TestUtil.testEnvironment

    private val compiledContract = BettingContract.debugCompiledContract
    private val scriptAddress = compiledContract.address(env.network)

    private val player1 = Mock.mockPubKeyHash(8)
    private val player2 = Mock.mockPubKeyHash(16)
    private val oracle = Mock.mockPubKeyHash(32)
    private val deployment = Mock.mockPubKeyHash(64)
    private val deploymentAddress = deployment.address

    extension (key: PubKeyHash) def address = TestUtil.createTestAddress(key.hash.toHex)

    private val gen = BigInt(64)

    private val provider: MockLedgerApi = MockLedgerApi(
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
              value = Value.ada(1_000_000_000L)
            ),
        Mock.mockTxInput(gen, 3) ->
            TransactionOutput.Babbage(
              address = deploymentAddress,
              value = Value.ada(1_000_000_000L)
            )
      ),
      context = Context.testMainnet(),
      validators =
          MockLedgerApi.defaultValidators - MissingKeyHashesValidator - ProtocolParamsViewHashesMatchValidator - MissingRequiredDatumsValidator,
      mutators = MockLedgerApi.defaultMutators - PlutusScriptsTransactionMutator
    )

    extension (txb: Either[String, Transaction])

        def assertTx(ledgerProvider: MockLedgerApi): Transaction = txb.left
            .map: error =>
                fail(error)
            .toOption // https://github.com/scala/scala3/issues/17216
            .flatMap: tx =>
                for () <- ledgerProvider.submit(tx).toOption yield tx
            .getOrElse:
                fail("Tx haven't been submitted")

    private val deployBetting: Transaction =
        new Transactions(
          BuilderContext.withNoopEvaluator(
            env,
            TestUtil.createTestWallet(provider, oracle.address)
          ),
          compiledContract
        ).deploy(
          deploymentAddress,
          provider.findUtxo(oracle.address).getOrElse(fail("There's no test wallet funds"))
        ).assertTx(provider)

    private def scriptUtxo(
        ledgerProvider: MockLedgerApi
    ): Either[RuntimeException, Utxo] = ledgerProvider
        .findUtxo(
          address = deploymentAddress,
          transactionId = Some(deployBetting.id),
          datum = None,
          minAmount = None
        )

    val currentScriptUtxo = scriptUtxo(provider)

    assert(currentScriptUtxo.isRight)
    println(currentScriptUtxo.toOption.get._2.value.coin)

    private val betAmount = 10000_000L
    private val expiration: PosixTime = 1753939940

    private val initBetting: Transaction =
        new Transactions(
          BuilderContext.withNoopEvaluator(
            env,
            TestUtil.createTestWallet(provider, player1.address)
          ),
          compiledContract
        ).init(
          betAmount,
          player1,
          oracle,
          expiration,
          currentScriptUtxo.toOption.get
        ).assertTx(provider)

    private val initDatum = Config(
      player1,
      PubKeyHash(hex""),
      oracle,
      expiration
    )

    private def initUtxo(
        ledgerProvider: MockLedgerApi
    ): Either[RuntimeException, Utxo] = ledgerProvider
        .findUtxo(
          address = scriptAddress,
          transactionId = Some(initBetting.id),
          datum = Some(DatumOption.Inline(initDatum.toData)),
          minAmount = Some(Coin(betAmount))
        )

    private val currentInitUtxo = initUtxo(provider)

    // TODO: test fee constrains
    println(
      scalus.cardano.ledger.utils.ScriptFeeComparison.compareAll(
        BettingValidator.validate,
        Data.unit,
        Some(DatumOption.Inline(initDatum.toData)),
        BuilderContext.withNoopEvaluator(
          env,
          TestUtil.createTestWallet(provider, oracle.address)
        ),
        Set(ExpectedSigner(AddrKeyHash.fromByteString(deployment.hash))),
        Value.lovelace(betAmount)
      )
    )

    assert(currentInitUtxo.toOption.get._2.value.coin == Coin(betAmount))

    private val joinBetting: (Transaction, Result) =
        val snapshot = provider // .snapshot()
        val tx = new Transactions(
          BuilderContext.withNoopEvaluator(
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
          currentScriptUtxo.toOption.get,
          currentInitUtxo.toOption.get
        ).toOption
            .get
        (tx, runValidator(tx, snapshot, initUtxo))

    private val joinDatum = initDatum.copy(player2 = player2)

    private def joinUtxo(
        ledgerProvider: MockLedgerApi
    ): Either[RuntimeException, Utxo] = ledgerProvider
        .findUtxo(
          address = scriptAddress,
          transactionId = Some(joinBetting._1.id),
          datum = Some(DatumOption.Inline(joinDatum.toData)),
          minAmount = Some(Coin(betAmount + betAmount))
        )

    val currentJoinUtxo = joinUtxo(provider)

    assert(currentJoinUtxo.toOption.get._2.value.coin == Coin(betAmount + betAmount))

    private def winBetting(isJoinWin: Boolean, time: PosixTime): (Transaction, Result) =
        val snapshot = provider // .snapshot()
        val tx = new Transactions(
          BuilderContext.withNoopEvaluator(
            env,
            TestUtil.createTestWallet(snapshot, oracle.address)
          ),
          compiledContract
        ).win(
          isJoinWin,
          player1,
          player2,
          oracle,
          currentScriptUtxo.toOption.get,
          currentJoinUtxo.toOption.get // ???: provider
        ).toOption
            .get
        (tx, runValidator(tx, snapshot, joinUtxo, Some(time))) // ???: provider

    private def runValidator(
        tx: Transaction,
        snapshot: MockLedgerApi,
        betUtxo: MockLedgerApi => Either[RuntimeException, Utxo],
        time: Option[PosixTime] = None,
    ) =
        val utxo = betUtxo(snapshot)
        assert(utxo.isRight)

        // time.foreach: posixTime =>
        //     snapshot.setSlot(env.slotConfig.timeToSlot(posixTime.toLong))

        val inputs =
            val body = tx.body.value
            (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet

        val utxos = provider.findUtxos(inputs).toOption.getOrElse(fail("Tx UTxOs already spent"))

        val scriptContext =
            TestUtil.getScriptContextV3(tx, utxos, utxo.toOption.get._1, RedeemerTag.Spend, env)

        val allScripts = AllResolvedScripts.allResolvedPlutusScriptsMap(tx, utxos).toOption.get
        val script = scriptAddress.scriptHashOption.flatMap(allScripts.get).get
        val program = Program.fromCborByteString(script.script)

        val res = program.runWithDebug(scriptContext)

        // TestUtil.runValidator(
        //   BettingContract.defaultCompiledContract.program,
        //   tx,
        //   utxos,
        //   snapshot.wallet,
        //   scriptInput
        // )

        res match
            case Result.Failure(exception, budget, costs, logs) =>
                logs.foreach(println)
                fail(s"Script failed to proceed: ${exception.getMessage}")
            case Result.Success(term, budget, costs, logs) =>
                assert(snapshot.submit(tx).isRight)
                assert(betUtxo(snapshot).isLeft)
                res
