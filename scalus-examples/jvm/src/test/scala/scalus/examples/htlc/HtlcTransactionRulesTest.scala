package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.{platform, ByteString}
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.Provider
import scalus.cardano.txbuilder.{BuilderContext, TransactionSigner}
import scalus.examples.TestUtil
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.testing.kit.{MockLedgerApi, ScalusTest}

class HtlcTransactionRulesTest extends AnyFunSuite, ScalusTest {
    private val env = TestUtil.testEnvironment
    private val compiledContract = HtlcContract.debugCompiledContract

    private val committerKeyPair @ (committerPrivateKey, committerPublicKey) = generateKeyPair()
    private val receiverKeyPair @ (receiverPrivateKey, receiverPublicKey) = generateKeyPair()
    private val wrongCommitterKeyPair @ (wrongCommitterPrivateKey, wrongCommitterPublicKey) =
        generateKeyPair()
    private val wrongReceiverKeyPair @ (wrongReceiverPrivateKey, wrongReceiverPublicKey) =
        generateKeyPair()

    private val committerPkh = AddrKeyHash(platform.blake2b_224(committerPublicKey))
    private val receiverPkh = AddrKeyHash(platform.blake2b_224(receiverPublicKey))
    private val wrongCommitterPkh = AddrKeyHash(platform.blake2b_224(wrongCommitterPublicKey))
    private val wrongReceiverPkh = AddrKeyHash(platform.blake2b_224(wrongReceiverPublicKey))

    private val committerAddress = TestUtil.createTestAddress(committerPkh)
    private val receiverAddress = TestUtil.createTestAddress(receiverPkh)

    private val lockAmount: Long = 100_000_000L
    private val amount: Long = 50_000_000L

    private val slot: SlotNo = 10
    private val beforeSlot: SlotNo = slot - 1
    private val afterSlot: SlotNo = slot + 1
    private val timeout: PosixTime = env.slotConfig.slotToTime(slot)
    private val beforeTimeout: PosixTime = env.slotConfig.slotToTime(beforeSlot)
    private val afterTimeout: PosixTime = env.slotConfig.slotToTime(afterSlot)

    private val validPreimage: Preimage = genByteStringOfN(32).sample.get
    private val wrongPreimage: Preimage = genByteStringOfN(12).sample.get
    private val validImage: Image = sha3_256(validPreimage)

    private val scriptAddress = compiledContract.address(env.network)

    private val datum = Config(
      PubKeyHash(committerPkh),
      PubKeyHash(receiverPkh),
      validImage,
      timeout
    ).toData

    private val transactionSigner = TransactionSigner(
      Set(committerKeyPair, receiverKeyPair, wrongCommitterKeyPair, wrongReceiverKeyPair)
    )

    private def createProvider(): MockLedgerApi = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        MockLedgerApi(
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
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }

    private def lockHtlc(provider: Provider): Transaction = {
        val wallet = TestUtil.createTestWallet(provider, committerAddress)
        val context = BuilderContext.withConstMaxBudgetEvaluator(env, wallet)
        val value = Value.lovelace(lockAmount)
        val tx = new Transactions(context, compiledContract)
            .lock(value, committerPkh, receiverPkh, validImage, timeout)
            .toOption
            .get

        transactionSigner.sign(tx, provider.findUtxos(committerAddress).toOption.get).toOption.get
    }

    private def revealHtlc(
        provider: Provider,
        lockUtxo: Utxo,
        preimage: Preimage,
        receiverPkh: AddrKeyHash,
        time: PosixTime
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(provider, receiverAddress)
        val context = BuilderContext.withConstMaxBudgetEvaluator(env, wallet)
        val tx = new Transactions(context, compiledContract)
            .reveal(lockUtxo, preimage, receiverAddress, receiverPkh, time)
            .toOption
            .get

        transactionSigner
            .sign(
              tx,
              provider.findUtxos(receiverAddress).toOption.get ++
                  provider
                      .findUtxos(
                        address = scriptAddress,
                        transactionId = Some(lockUtxo._1.transactionId),
                        datum = Some(DatumOption.Inline(datum)),
                        minAmount = Some(Coin(lockAmount))
                      )
                      .toOption
                      .get
            )
            .toOption
            .get
    }

    private def timeoutHtlc(
        provider: Provider,
        lockUtxo: Utxo,
        committerPkh: AddrKeyHash,
        time: PosixTime
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(provider, committerAddress)
        val context = BuilderContext.withConstMaxBudgetEvaluator(env, wallet)
        val tx = new Transactions(context, compiledContract)
            .timeout(lockUtxo, committerAddress, committerPkh, time)
            .toOption
            .get

        transactionSigner
            .sign(
              tx,
              provider.findUtxos(committerAddress).toOption.get ++
                  provider
                      .findUtxos(
                        address = scriptAddress,
                        transactionId = Some(lockUtxo._1.transactionId),
                        datum = Some(DatumOption.Inline(datum)),
                        minAmount = Some(Coin(lockAmount))
                      )
                      .toOption
                      .get
            )
            .toOption
            .get
    }

    test("receiver reveals preimage before timeout") {
        val provider = createProvider()

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

    test("receiver fails with wrong preimage") {
        val provider = createProvider()

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

        val revealTx = revealHtlc(provider, lockUtxo, wrongPreimage, receiverPkh, beforeTimeout)
        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout.toLong))
        provider.submit(revealTx) match
            case Left(err) => assert(err.getMessage.endsWith(HtlcValidator.InvalidReceiverPreimage))
            case Right(_)  => fail("Transaction should have failed")
    }

    ignore("receiver fails with wrong receiver pubkey hash") {
        val provider = createProvider()

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

        val revealTx =
            revealHtlc(provider, lockUtxo, validPreimage, wrongReceiverPkh, beforeTimeout)
        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout.toLong))
        provider.submit(revealTx) match
            case Left(err) =>
                println(revealTx.body.value.requiredSigners)
                println(err)
                assert(err.getMessage.endsWith(HtlcValidator.UnsignedReceiverTransaction))
            case Right(_) => fail("Transaction should have failed")
    }

    test("receiver fails after timeout") {
        val provider = createProvider()

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

        val revealTx =
            revealHtlc(provider, lockUtxo, validPreimage, receiverPkh, afterTimeout)
        provider.setSlot(env.slotConfig.timeToSlot(afterTimeout.toLong))
        provider.submit(revealTx) match
            case Left(err) =>
                assert(err.getMessage.endsWith(HtlcValidator.InvalidReceiverTimePoint))
            case Right(_) => fail("Transaction should have failed")
    }

    test("committer reclaims after timeout") {
        val provider = this.createProvider()

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

    ignore("committer fails with wrong committer pubkey hash") {
        val provider = this.createProvider()

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

        val timeoutTx = timeoutHtlc(provider, lockUtxo, wrongCommitterPkh, afterTimeout)
        provider.setSlot(env.slotConfig.timeToSlot(afterTimeout.toLong))
        provider.submit(timeoutTx) match
            case Left(err) =>
                println(err)
                assert(err.getMessage.endsWith(HtlcValidator.UnsignedCommitterTransaction))
            case Right(_) => fail("Transaction should have failed")
    }

    test("committer fails before timeout") {
        val provider = this.createProvider()

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

        val timeoutTx = timeoutHtlc(provider, lockUtxo, committerPkh, beforeTimeout)
        provider.setSlot(env.slotConfig.timeToSlot(beforeTimeout.toLong))
        provider.submit(timeoutTx) match
            case Left(err) =>
                assert(err.getMessage.endsWith(HtlcValidator.InvalidCommitterTimePoint))
            case Right(_) => fail("Transaction should have failed")
    }
}
