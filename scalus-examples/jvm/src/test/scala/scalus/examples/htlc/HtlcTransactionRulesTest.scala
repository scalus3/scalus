package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.testkit.ScalusTest
import scalus.ledger.api.v1.PosixTime
import scalus.builtin.ByteString
import scalus.examples.TestUtil
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.builtin.ToData.*
import scalus.cardano.txbuilder.BuilderContext
import scalus.cardano.ledger.rules.*

class HtlcTransactionRulesTest extends AnyFunSuite, ScalusTest {
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

    private val scriptAddress =
        Address(env.network, Credential.ScriptHash(compiledContract.script.scriptHash))
    private val datum = ContractDatum(
      committerPkh,
      receiverPkh,
      validImage,
      timeout
    ).toData

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
        .findUtxoByAddressAndDatum(lockHtlc, scriptAddress, Some(DatumOption.Inline(datum)))
        .get

    private def revealHtlc(
        preimage: ByteString,
        receiverPkh: ByteString,
        time: PosixTime
    ): Either[TransactionException, Unit] = {
        val wallet = TestUtil.createTestWallet(receiverAddress, amount)
        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(time.toLong)
        val tx = new Transactions(context, compiledContract)
            .reveal(htlcUtxo, preimage, receiverAddress, receiverPkh, validityStartSlot)
            .toOption
            .get

        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        CardanoMutator.transit(Context(), State(utxos = utxos), tx).map(_ => ())
    }

    private def timeoutHtlc(
        committerPkh: ByteString,
        time: PosixTime
    ): Either[TransactionException, Unit] = {
        val wallet = TestUtil.createTestWallet(committerAddress, amount)
        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(time.toLong)
        val tx = new Transactions(context, compiledContract)
            .timeout(htlcUtxo, committerAddress, committerPkh, validityStartSlot)
            .toOption
            .get

        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        CardanoMutator.transit(Context(), State(utxos = utxos), tx).map(_ => ())
    }

    ignore("receiver reveals preimage before timeout") {
        val result = revealHtlc(validPreimage, receiverPkh, beforeTimeout)
        assert(result.isRight)
    }

    ignore("receiver fails with wrong preimage") {
        val result = revealHtlc(wrongPreimage, receiverPkh, beforeTimeout)

        assert(result.isLeft)
//        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverPreimage))
    }

    ignore("receiver fails with wrong receiver pubkey hash") {
        val result = revealHtlc(validPreimage, wrongReceiverPkh, beforeTimeout)

        assert(result.isLeft)
//        assert(result.logs.last.contains(HtlcValidator.UnsignedReceiverTransaction))
    }

    ignore("receiver fails after timeout") {
        val result = revealHtlc(validPreimage, receiverPkh, afterTimeout)

        assert(result.isLeft)
//        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverTimePoint))
    }

    ignore("committer reclaims after timeout") {
        val result = timeoutHtlc(committerPkh, afterTimeout)
        assert(result.isRight)
    }

    ignore("committer fails before timeout") {
        val result = timeoutHtlc(committerPkh, beforeTimeout)

        assert(result.isLeft)
//        assert(result.logs.last.contains(HtlcValidator.InvalidCommitterTimePoint))
    }

    ignore("committer fails with wrong committer pubkey hash") {
        val result = timeoutHtlc(wrongCommitterPkh, afterTimeout)

        assert(result.isLeft)
//        assert(result.logs.last.contains(HtlcValidator.UnsignedCommitterTransaction))
    }
}
