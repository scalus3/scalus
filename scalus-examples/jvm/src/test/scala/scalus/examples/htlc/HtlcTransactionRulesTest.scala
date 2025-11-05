package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.ledger.api.v1.PosixTime
import scalus.builtin.ByteString
import scalus.examples.TestUtil
import scalus.cardano.ledger.*
import scalus.builtin.ToData.*
import scalus.cardano.txbuilder.BuilderContext
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.LedgerProvider
import scalus.testing.kit.ScalusTest

class HtlcTransactionRulesTest extends AnyFunSuite, ScalusTest {
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
    ): Either[RuntimeException, Unit] = {
        val snapshot = provider.snapshot()
        val wallet = TestUtil.createTestWallet(snapshot, receiverAddress)
        val context = BuilderContext(env, wallet)
        val tx = new Transactions(context, compiledContract)
            .reveal(htlcUtxo, preimage, receiverAddress, receiverPkh, time)
            .toOption
            .get

        snapshot.setSlot(env.slotConfig.timeToSlot(time.toLong))
        snapshot.submit(tx)
    }

    private def timeoutHtlc(
        committerPkh: ByteString,
        time: PosixTime
    ): Either[RuntimeException, Unit] = {
        val snapshot = provider.snapshot()
        val wallet = TestUtil.createTestWallet(snapshot, committerAddress)
        val context = BuilderContext(env, wallet)
        val tx = new Transactions(context, compiledContract)
            .timeout(htlcUtxo, committerAddress, committerPkh, time)
            .toOption
            .get

        snapshot.setSlot(env.slotConfig.timeToSlot(time.toLong))
        snapshot.submit(tx)
    }

    test("receiver reveals preimage before timeout") {
        val result = revealHtlc(validPreimage, receiverPkh, beforeTimeout)
        assert(result.isRight)
    }

//    ignore("receiver fails with wrong preimage") {
//        val result = revealHtlc(wrongPreimage, receiverPkh, beforeTimeout)
//
//        assert(result.nonEmpty)
////        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverPreimage))
//    }
//
//    ignore("receiver fails with wrong receiver pubkey hash") {
//        val result = revealHtlc(validPreimage, wrongReceiverPkh, beforeTimeout)
//
//        assert(result.nonEmpty)
////        assert(result.logs.last.contains(HtlcValidator.UnsignedReceiverTransaction))
//    }
//
//    ignore("receiver fails after timeout") {
//        val result = revealHtlc(validPreimage, receiverPkh, afterTimeout)
//
//        assert(result.nonEmpty)
////        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverTimePoint))
//    }
//
    test("committer reclaims after timeout") {
        val result = timeoutHtlc(committerPkh, afterTimeout)
        assert(result.isRight)
    }
//
//    ignore("committer fails before timeout") {
//        val result = timeoutHtlc(committerPkh, beforeTimeout)
//
//        assert(result.nonEmpty)
////        assert(result.logs.last.contains(HtlcValidator.InvalidCommitterTimePoint))
//    }
//
//    ignore("committer fails with wrong committer pubkey hash") {
//        val result = timeoutHtlc(wrongCommitterPkh, afterTimeout)
//
//        assert(result.nonEmpty)
////        assert(result.logs.last.contains(HtlcValidator.UnsignedCommitterTransaction))
//    }
}
