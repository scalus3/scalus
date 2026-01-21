package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.RedeemerPurpose.ForSpend
import scalus.cardano.txbuilder.txBuilder
import scalus.testing.assertions.Expected.{Success, SuccessAny}
import scalus.testing.assertions.{Expected, ResultAssertions}
import scalus.testing.kit.Party.{Alice, Bob, Eve}
import scalus.testing.kit.{ScalusTest, TestUtil, TxTestKit}
import scalus.uplc.Compiled
import scalus.utils.{await, showHighlighted}

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class HtlcTest extends AnyFunSuite, ScalusTest, TxTestKit {

    private given env: CardanoInfo = TestUtil.testEnvironment
    private val contract = HtlcContract.compiled.withErrorTraces

    /** Transaction creator with real script evaluation */
    private val txCreator = HtlcTransactions(
      env = env,
      contract = contract
    )

    private val slot: SlotNo = 10
    private val beforeSlot: SlotNo = slot - 1
    private val afterSlot: SlotNo = slot + 1
    private val timeout: Instant = env.slotConfig.slotToInstant(slot)
    private val beforeTimeout: Instant = env.slotConfig.slotToInstant(beforeSlot)
    private val afterTimeout: Instant = env.slotConfig.slotToInstant(afterSlot)

    val validPreimage: Preimage = genByteStringOfN(32).sample.get
    val wrongPreimage: Preimage = genByteStringOfN(12).sample.get
    private val image: Image = sha3_256(validPreimage)

    private def createProvider(): Emulator =
        Emulator.withAddresses(Seq(Alice.address, Bob.address, Eve.address))

    private def lock(provider: Emulator): Utxo = {
        val utxos = provider.findUtxos(address = Alice.address).await().toOption.get

        val lockTx = txCreator.lock(
          utxos = utxos,
          value = Value.ada(10),
          sponsor = Alice.address,
          committer = Alice.addrKeyHash,
          receiver = Bob.addrKeyHash,
          image = image,
          timeout = timeout,
          signer = Alice.signer
        )
        assert(provider.submit(lockTx).await().isRight)
        val lockedUtxo = lockTx.utxos.find { case (_, txOut) =>
            txOut.address == contract.address(env.network)
        }.get
        Utxo(lockedUtxo)
    }

    // Transaction assertions provided by TxTestKit

    test(s"HTLC validator size is ${HtlcContract.compiled.script.script.size} bytes") {
        assert(HtlcContract.compiled.script.script.size == 569)
    }

    test("VALIDATOR: receiver reveals preimage before timeout") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.utxos

        val tx = txBuilder
            .spend(
              lockedUtxo,
              redeemer = Action.Reveal(validPreimage),
              script = contract.script,
              requiredSigners = Set(Bob.addrKeyHash)
            )
            .payTo(Bob.address, Value.ada(10))
            .validTo(timeout)
            .draft

        println(tx.showHighlighted)

        import scalus.testing.kit.TestUtil.getScriptContextV3
        val sc = tx.getScriptContextV3(utxos, ForSpend(lockedUtxo.input))

        println(sc)

        assertExpected(SuccessAny)(contract(sc.toData))
    }

    test("SCRIPT CONTEXT: receiver reveals preimage before timeout") {
        import scalus.builtin.ByteString.hex
        import scalus.builtin.Data.toData
        import scalus.ledger.api.v1.{IntervalBound, Value}
        import scalus.ledger.api.v2.OutputDatum
        import scalus.ledger.api.v3.*
        import scalus.prelude.{List as PList, *}

        val timeoutPosix = BigInt(timeout.toEpochMilli)
        val redeemer = Action.Reveal(validPreimage).toData
        val datum = Config(
          PubKeyHash(Alice.addrKeyHash),
          PubKeyHash(Bob.addrKeyHash),
          image,
          timeoutPosix
        ).toData

        val sc = ScriptContext(
          txInfo = TxInfo(
            inputs = PList(
              TxInInfo(
                TxOutRef(
                  TxId(hex"33b2dfafd54bc2478bab834185041b44bc567d9bced9cdc9018f7322711c08c1"),
                  0
                ),
                TxOut(
                  Address(Credential.ScriptCredential(contract.script.scriptHash), Option.None),
                  Value.lovelace(10_000_000),
                  OutputDatum.OutputDatum(datum)
                )
              )
            ),
            outputs = PList(
              TxOut(
                Address(Credential.PubKeyCredential(PubKeyHash(Bob.addrKeyHash)), Option.None),
                Value.lovelace(10_000_000)
              )
            ),
            validRange =
                Interval(IntervalBound.negInf, IntervalBound.finiteExclusive(timeoutPosix)),
            signatories = PList(PubKeyHash(Bob.addrKeyHash)),
            redeemers = SortedMap.singleton(
              ScriptPurpose.Spending(
                TxOutRef(
                  TxId(hex"33b2dfafd54bc2478bab834185041b44bc567d9bced9cdc9018f7322711c08c1"),
                  BigInt(0)
                )
              ),
              redeemer
            ),
            id = TxId(hex"3612aa9eee38d20971027a9082e3a4e82c44c44f88b00416fe506e976a9f22dc")
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            TxOutRef(
              TxId(hex"33b2dfafd54bc2478bab834185041b44bc567d9bced9cdc9018f7322711c08c1"),
              BigInt(0)
            ),
            Option.Some(datum)
          )
        )

        assertExpected(SuccessAny)(contract(sc.toData))
    }

    def assertExpected[A](expected: Expected)(contract: Compiled[A]): Unit = {
        val jvmResult = Try(contract.code)
        val uplcResult = contract.program.evaluateDebug

        // Assert UPLC result matches expected
        ResultAssertions.assertResult(expected, uplcResult)

        // Assert JVM result aligns with expected
        expected match
            case Expected.SuccessAny | Expected.Success(_) | Expected.SuccessSame =>
                assert(
                  jvmResult.isSuccess,
                  s"JVM execution failed but expected success: ${jvmResult}"
                )
            case Expected.Failure(_) =>
                assert(
                  jvmResult.isFailure,
                  s"JVM execution succeeded but expected failure"
                )
    }

    test("receiver reveals preimage before timeout") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        val revealTx = txCreator.reveal(
          utxos = utxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = Bob.address,
          sponsor = Bob.address,
          preimage = validPreimage,
          receiverPkh = Bob.addrKeyHash,
          validTo = timeout,
          signer = Bob.signer
        )

        provider.setSlot(beforeSlot)
        val result = provider.submit(revealTx).await()
        assert(result.isRight, s"Emulator submission failed: $result")
    }

    test("receiver fails with wrong preimage") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        assertTxFail(HtlcValidator.InvalidReceiverPreimage) {
            txCreator.reveal(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Bob.address,
              sponsor = Bob.address,
              preimage = wrongPreimage,
              receiverPkh = Bob.addrKeyHash,
              validTo = timeout,
              signer = Bob.signer
            )
        }
    }

    test("receiver fails with wrong receiver pubkey hash") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        assertTxFail(HtlcValidator.UnsignedReceiverTransaction) {
            txCreator.reveal(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Eve.address,
              sponsor = Eve.address,
              preimage = validPreimage,
              receiverPkh = Eve.addrKeyHash, // Wrong receiver PKH (should be Bob)
              validTo = timeout,
              signer = Eve.signer
            )
        }
    }

    test("receiver fails after timeout") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Bob.address).await().toOption.get

        assertTxFail(HtlcValidator.InvalidReceiverTimePoint) {
            txCreator.reveal(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Bob.address,
              sponsor = Bob.address,
              preimage = validPreimage,
              receiverPkh = Bob.addrKeyHash,
              validTo = afterTimeout,
              signer = Bob.signer
            )
        }
    }

    test("committer reclaims after timeout") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val timeoutTx = txCreator.timeout(
          utxos = utxos,
          lockedUtxo = lockedUtxo,
          payeeAddress = Alice.address,
          sponsor = Alice.address,
          committerPkh = Alice.addrKeyHash,
          validFrom = afterTimeout,
          signer = Alice.signer
        )

        provider.setSlot(afterSlot)
        assertTxSuccess(provider, timeoutTx)
    }

    test("committer fails with wrong committer pubkey hash") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Eve.address).await().toOption.get

        assertTxFail(HtlcValidator.UnsignedCommitterTransaction) {
            txCreator.timeout(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Eve.address,
              sponsor = Eve.address,
              committerPkh = Eve.addrKeyHash, // Wrong committer PKH (should be Alice)
              validFrom = afterTimeout,
              signer = Eve.signer
            )
        }
    }

    test("committer fails before timeout") {
        val provider = createProvider()
        val lockedUtxo = lock(provider)
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        assertTxFail(HtlcValidator.InvalidCommitterTimePoint) {
            txCreator.timeout(
              utxos = utxos,
              lockedUtxo = lockedUtxo,
              payeeAddress = Alice.address,
              sponsor = Alice.address,
              committerPkh = Alice.addrKeyHash,
              validFrom = beforeTimeout,
              signer = Alice.signer
            )
        }
    }
}
