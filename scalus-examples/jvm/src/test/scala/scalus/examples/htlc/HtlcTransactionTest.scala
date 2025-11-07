package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.ByteString
import scalus.builtin.ToData.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.utils.ScriptFeeComparison
import scalus.cardano.ledger.utils.ScriptFeeComparison.{ComparisonResult, FeeComparison}
import scalus.cardano.txbuilder.{BuilderContext, ExpectedSigner}
import scalus.examples.htlc.Action.Reveal
import scalus.ledger.api.v1.PosixTime
import scalus.sir.TargetLoweringBackend.SirToUplcV3Lowering
import scalus.uplc.eval.Result
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.uplc.Program
import scalus.cardano.node.LedgerProvider
import scalus.cardano.ledger.rules.*
import scalus.testing.kit.{ScalusTest, TestUtil}

class HtlcTransactionTest extends AnyFunSuite, ScalusTest {
    private val env = TestUtil.testEnvironmentWithoutEvaluator
    private val compiledContract = HtlcContract.debugCompiledContract

    private val committerAddress = TestUtil.createTestAddress("a" * 56)
    private val receiverAddress = TestUtil.createTestAddress("b" * 56)

    private val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
    private val provider: LedgerProvider = LedgerProvider(
      initialUtxos = Map(
        TransactionInput(genesisHash, 0) ->
            TransactionOutput.Babbage(
              address = committerAddress,
              value = Value.lovelace(1_000_000_000L)
            ),
        TransactionInput(genesisHash, 1) ->
            TransactionOutput.Babbage(
              address = receiverAddress,
              value = Value.lovelace(1_000_000_000L)
            )
      ),
      context = Context.testMainnet(),
      validators =
          LedgerProvider.defaultValidators - MissingKeyHashesValidator - ProtocolParamsViewHashesMatchValidator - MissingRequiredDatumsValidator,
      mutators = LedgerProvider.defaultMutators - PlutusScriptsTransactionMutator
    )

    private val committerPkh = ByteString.fromArray(committerAddress.payment.asHash.bytes)
    private val receiverPkh = ByteString.fromArray(receiverAddress.payment.asHash.bytes)
    private val wrongCommitterPkh =
        ByteString.fromArray(TestUtil.createTestAddress("c" * 56).payment.asHash.bytes)
    private val wrongReceiverPkh =
        ByteString.fromArray(TestUtil.createTestAddress("d" * 56).payment.asHash.bytes)

    private val lockAmount: Long = 100_000_000L
    private val amount: Long = 50_000_000L

    private val slot: SlotNo = 10
    private val beforeSlot: SlotNo = slot - 1
    private val afterSlot: SlotNo = slot + 1
    private val timeout: PosixTime = env.slotConfig.slotToTime(slot)
    private val beforeTimeout: PosixTime = env.slotConfig.slotToTime(beforeSlot)
    private val afterTimeout: PosixTime = env.slotConfig.slotToTime(afterSlot)

    private val validPreimage: ByteString = genByteStringOfN(32).sample.get
    private val wrongPreimage = genByteStringOfN(12).sample.get
    private val validImage: ByteString = sha3_256(validPreimage)

    private val scriptAddress = compiledContract.address(env.network)
    private val datum = ContractDatum(
      committerPkh,
      receiverPkh,
      validImage,
      timeout
    ).toData

    private val lockHtlc: Transaction = {
        val wallet = TestUtil.createTestWallet(provider, committerAddress)
        val context = BuilderContext(env, wallet)
        val value = Value.lovelace(lockAmount)

        val tx = new Transactions(context, compiledContract)
            .lock(value, committerPkh, receiverPkh, validImage, timeout)
            .toOption
            .get

        assert(provider.submit(tx).isRight)
        tx
    }

    private val htlcUtxo = provider
        .findUtxo(
          address = scriptAddress,
          transactionId = Some(lockHtlc.id),
          datum = Some(DatumOption.Inline(datum)),
          minAmount = Some(Coin(lockAmount))
        )
        .toOption
        .get

    assert(htlcUtxo._2.value.coin == Coin(lockAmount))

    private def revealHtlc(
        preimage: ByteString,
        receiverPkh: ByteString,
        time: PosixTime
    ): (Transaction, Result) = {
        val snapshot = provider.snapshot()
        val wallet = TestUtil.createTestWallet(snapshot, receiverAddress)
        val context = BuilderContext(env, wallet)
        val tx = new Transactions(context, compiledContract)
            .reveal(htlcUtxo, preimage, receiverAddress, receiverPkh, time)
            .toOption
            .get

        val result = runValidator(tx, snapshot, time)
        (tx, result)
    }

    private def timeoutHtlc(
        committerPkh: ByteString,
        time: PosixTime
    ): (Transaction, Result) = {
        val snapshot = provider.snapshot()
        val wallet = TestUtil.createTestWallet(snapshot, committerAddress)
        val context = BuilderContext(env, wallet)
        val tx = new Transactions(context, compiledContract)
            .timeout(htlcUtxo, committerAddress, committerPkh, time)
            .toOption
            .get

        val result = runValidator(tx, snapshot, time)
        (tx, result)
    }

    private def runValidator(tx: Transaction, snapshot: LedgerProvider, time: PosixTime) = {
        assert(
          snapshot
              .findUtxo(
                address = scriptAddress,
                transactionId = Some(lockHtlc.id),
                datum = Some(DatumOption.Inline(datum)),
                minAmount = Some(Coin(lockAmount))
              )
              .isRight
        )

        snapshot.setSlot(env.slotConfig.timeToSlot(time.toLong))
        assert(snapshot.submit(tx).isRight)

        assert(
          snapshot
              .findUtxo(
                address = scriptAddress,
                transactionId = Some(lockHtlc.id),
                datum = Some(DatumOption.Inline(datum)),
                minAmount = Some(Coin(lockAmount))
              )
              .isLeft
        )

        val inputs = {
            val body = tx.body.value
            (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
        }

        val utxos = provider.findUtxos(inputs).toOption.get

        val scriptContext =
            TestUtil.getScriptContextV3(tx, utxos, htlcUtxo._1, RedeemerTag.Spend, env)

        val allScripts = AllResolvedScripts.allResolvedPlutusScriptsMap(tx, utxos).toOption.get
        val script = scriptAddress.scriptHashOption.flatMap(allScripts.get).get
        val program = Program.fromCborByteString(script.script)

        program.runWithDebug(scriptContext)
    }

    test("receiver reveals preimage before timeout") {
        val (revealTx, result) = revealHtlc(validPreimage, receiverPkh, beforeTimeout)

        assert(result.isSuccess)

//        val receiverCoinValue =
//            TestUtil.findUtxoByAddressAndDatum(revealTx, receiverAddress).map(_._2.value.coin.value)
//        assert(
//          receiverCoinValue.exists(_ >= lockAmount),
//          s"expected receiver coin value >= $lockAmount, found: ${receiverCoinValue.map(_.toString).getOrElse("none")}"
//        )
    }

    test("receiver fails with wrong preimage") {
        val (_, result) = revealHtlc(wrongPreimage, receiverPkh, beforeTimeout)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverPreimage))
    }

    test("receiver fails with wrong receiver pubkey hash") {
        val (_, result) = revealHtlc(validPreimage, wrongReceiverPkh, beforeTimeout)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.UnsignedReceiverTransaction))
    }

    test("receiver fails after timeout") {
        val (_, result) = revealHtlc(validPreimage, receiverPkh, afterTimeout)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverTimePoint))
    }

    test("committer reclaims after timeout") {
        val (timeoutTx, result) = timeoutHtlc(committerPkh, afterTimeout)

        assert(result.isSuccess)

//        val committerCoinValue =
//            TestUtil
//                .findUtxoByAddressAndDatum(timeoutTx, committerAddress)
//                .map(_._2.value.coin.value)
//        assert(
//          committerCoinValue.exists(_ >= lockAmount),
//          s"expected committer coin value >= $lockAmount, found: ${committerCoinValue.map(_.toString).getOrElse("none")}"
//        )
    }

    test("committer fails before timeout") {
        val (_, result) = timeoutHtlc(committerPkh, beforeTimeout)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidCommitterTimePoint))
    }

    test("committer fails with wrong committer pubkey hash") {
        val (_, result) = timeoutHtlc(wrongCommitterPkh, afterTimeout)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.UnsignedCommitterTransaction))
    }

    test("has smaller fees on v3 backend") {
        val testDatum = ContractDatum(
          committer = committerPkh,
          receiver = receiverPkh,
          image = validImage,
          timeout = timeout
        ).toData

        val matrix = ScriptFeeComparison.compareAll(
          HtlcValidator.validate,
          Reveal(validPreimage).toData,
          Some(Inline(testDatum)),
          BuilderContext(
            TestUtil.testEnvironmentWithEvaluator,
            TestUtil.createTestWallet(receiverAddress, amount)
          ),
          additionalSigners = Set(ExpectedSigner(AddrKeyHash.fromByteString(receiverPkh)))
        )
        val releaseV3 = matrix.collectFirst {
            case (
                  Compiler.Options(SirToUplcV3Lowering, _, true, _, _),
                  ComparisonResult.Ok(result, _)
                ) =>
                result
        }.get

        val otherBackends = matrix.collect {
            case (options, ComparisonResult.Ok(comparisonResult, _))
                if options.targetLoweringBackend != SirToUplcV3Lowering =>
                comparisonResult
        }
        assert(otherBackends.nonEmpty)
        assert(otherBackends.forall(_.directFee > releaseV3.directFee))
    }
}
