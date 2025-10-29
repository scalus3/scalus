package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.ByteString
import scalus.builtin.ToData.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.utils.ScriptFeeComparison
import scalus.cardano.ledger.utils.ScriptFeeComparison.{ComparisonResult, FeeComparison}
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.{BuilderContext, Datum, ExpectedSigner}
import scalus.examples.TestUtil
import scalus.examples.htlc.Action.Reveal
import scalus.ledger.api.v1.PosixTime
import scalus.sir.TargetLoweringBackend.SirToUplcV3Lowering
import scalus.testkit.ScalusTest
import scalus.uplc.eval.Result

class HtlcTransactionTest extends AnyFunSuite, ScalusTest {

    private val env = TestUtil.testEnvironment
    private val compiledContract = HtlcContract.debugCompiledContract

    private val committerAddress = TestUtil.createTestAddress("a" * 56)
    private val receiverAddress = TestUtil.createTestAddress("b" * 56)

    private val committerPkh = ByteString.fromArray(committerAddress.payment.asHash.bytes)
    private val receiverPkh = ByteString.fromArray(receiverAddress.payment.asHash.bytes)
    private val wrongCommitterPkh =
        ByteString.fromArray(TestUtil.createTestAddress("c" * 56).payment.asHash.bytes)
    private val wrongReceiverPkh =
        ByteString.fromArray(TestUtil.createTestAddress("d" * 56).payment.asHash.bytes)

    private val lockAmount: Long = 100_000_000L
    private val amount: Long = 50_000_000L

    private val timeout: PosixTime = 1_745_261_347_000L
    private val beforeTimeout: PosixTime = 1_745_261_346_000L
    private val afterTimeout: PosixTime = 1_745_261_348_000L

    private val validPreimage: ByteString = genByteStringOfN(32).sample.get
    private val wrongPreimage = genByteStringOfN(12).sample.get
    private val validImage: ByteString = sha3_256(validPreimage)

    private val lockHtlc: Transaction = {
        val wallet = TestUtil.createTestWallet(committerAddress, lockAmount + amount)
        val context = BuilderContext(env, wallet)
        val value = Value.lovelace(lockAmount)
        new Transactions(context, compiledContract)
            .lock(value, committerPkh, receiverPkh, validImage, timeout)
            .toOption
            .get
    }

    private val htlcUtxo = TestUtil
        .findUtxoByAddress(
          lockHtlc,
          Address(env.network, Credential.ScriptHash(compiledContract.script.scriptHash))
        )
        .get

    private def revealHtlc(
        preimage: ByteString,
        receiverPkh: ByteString,
        time: PosixTime
    ): (Transaction, Result) = {
        val wallet = TestUtil.createTestWallet(receiverAddress, amount)
        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(time.toLong)
        val tx = new Transactions(context, compiledContract)
            .reveal(htlcUtxo, preimage, receiverAddress, receiverPkh, validityStartSlot)
            .toOption
            .get

        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        val result = runValidator(tx, utxos)

        (tx, result)
    }

    private def timeoutHtlc(
        committerPkh: ByteString,
        time: PosixTime
    ): (Transaction, Result) = {
        val wallet = TestUtil.createTestWallet(committerAddress, amount)
        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(time.toLong)
        val tx = new Transactions(context, compiledContract)
            .timeout(htlcUtxo, committerAddress, committerPkh, validityStartSlot)
            .toOption
            .get

        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        val result = runValidator(tx, utxos)

        (tx, result)
    }

    private def runValidator(tx: Transaction, utxo: Utxos) = {
        val scriptContext =
            TestUtil.getScriptContextV3(tx, utxo, htlcUtxo._1, RedeemerTag.Spend, env)
        compiledContract.program.runWithDebug(scriptContext)
    }

    test("receiver reveals preimage before timeout") {
        val (revealTx, result) = revealHtlc(validPreimage, receiverPkh, beforeTimeout)

        assert(result.isSuccess)

        val receiverCoinValue =
            TestUtil.findUtxoByAddress(revealTx, receiverAddress).map(_._2.value.coin.value)
        assert(
          receiverCoinValue.exists(_ >= lockAmount),
          s"expected receiver coin value >= $lockAmount, found: ${receiverCoinValue.map(_.toString).getOrElse("none")}"
        )
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

        val committerCoinValue =
            TestUtil.findUtxoByAddress(timeoutTx, committerAddress).map(_._2.value.coin.value)
        assert(
          committerCoinValue.exists(_ >= lockAmount),
          s"expected committer coin value >= $lockAmount, found: ${committerCoinValue.map(_.toString).getOrElse("none")}"
        )
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
