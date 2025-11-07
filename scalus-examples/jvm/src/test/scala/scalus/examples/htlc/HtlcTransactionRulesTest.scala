package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.testing.kit.ScalusTest
import scalus.ledger.api.v1.PosixTime
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.examples.TestUtil
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.BuilderContext
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.{LedgerProvider, Provider}

class HtlcTransactionRulesTest extends AnyFunSuite, ScalusTest {
    private val env = TestUtil.testEnvironmentWithoutEvaluator
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

    private def provider(): Provider = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        LedgerProvider(
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
    }

    private def lockHtlc(provider: Provider): Transaction = {
        val wallet = TestUtil.createTestWallet(provider, committerAddress)
        val context = BuilderContext(env, wallet)
        val value = Value.lovelace(lockAmount)
        new Transactions(context, compiledContract)
            .lock(value, committerPkh, receiverPkh, validImage, timeout)
            .toOption
            .get
    }

    private def revealHtlc(
        provider: Provider,
        lockUtxo: (TransactionInput, TransactionOutput),
        preimage: ByteString,
        receiverPkh: ByteString,
        time: PosixTime
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(provider, receiverAddress)
        val context = BuilderContext(env, wallet)
        new Transactions(context, compiledContract)
            .reveal(lockUtxo, preimage, receiverAddress, receiverPkh, time)
            .toOption
            .get
    }

    private def timeoutHtlc(
        provider: Provider,
        lockUtxo: (TransactionInput, TransactionOutput),
        committerPkh: ByteString,
        time: PosixTime
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(provider, committerAddress)
        val context = BuilderContext(env, wallet)
        new Transactions(context, compiledContract)
            .timeout(lockUtxo, committerAddress, committerPkh, time)
            .toOption
            .get
    }

    test("receiver reveals preimage before timeout") {
        val provider = this.provider()

        val lockTx = lockHtlc(provider)
        assert(provider.submit(lockTx).isRight)

        val lockUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(Coin(lockAmount))
            )
            .toOption
            .get
        assert(lockUtxo._2.value.coin == Coin(lockAmount))

        val revealTx = revealHtlc(provider, lockUtxo, validPreimage, receiverPkh, beforeTimeout)
        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout.toLong))
        assert(provider.submit(revealTx).isRight)
    }

    test("committer reclaims after timeout") {
        val provider = this.provider()

        val lockTx = lockHtlc(provider)
        assert(provider.submit(lockTx).isRight)

        val lockUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              datum = Some(DatumOption.Inline(datum)),
              minAmount = Some(Coin(lockAmount))
            )
            .toOption
            .get
        assert(lockUtxo._2.value.coin == Coin(lockAmount))

        val timeoutTx = timeoutHtlc(provider, lockUtxo, committerPkh, afterTimeout)
        provider.setSlot(env.slotConfig.timeToSlot(afterTimeout.toLong))
        assert(provider.submit(timeoutTx).isRight)
    }
}
