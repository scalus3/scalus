package scalus.examples.vesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.node.{Emulator, NodeSubmitError}
import scalus.cardano.txbuilder.RedeemerPurpose
import scalus.testing.kit.Party.{Alice, Bob, Eve}
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.eval.Result
import scalus.utils.await

class VestingTransactionTest extends AnyFunSuite, ScalusTest {

    private given env: CardanoInfo = TestUtil.testEnvironment
    private val contract = VestingContract.withErrorTraces

    private val txCreator = VestingTransactions(
      env = env,
      contract = contract
    )

    // Time model: vesting starts at slot 10, lasts 100 slots (100 seconds)
    private val vestingStartSlot: SlotNo = 10
    private val vestingDurationSlots: Long = 100
    private val startTimestamp: Long = env.slotConfig.slotToTime(vestingStartSlot)
    private val duration: Long = vestingDurationSlots * env.slotConfig.slotLength

    private val lockAmount: Long = 20_000_000L // 20 ADA

    private val scriptAddress = contract.address(env.network)

    private def createProvider: Emulator =
        Emulator.withAddresses(Seq(Alice.address, Bob.address, Eve.address))

    private def runValidator(
        provider: Emulator,
        tx: Transaction,
        scriptInput: TransactionInput
    ): Result = {
        val utxos = {
            val body = tx.body.value
            val allInputs =
                (body.inputs.toSet.view ++ body.collateralInputs.toSet.view ++ body.referenceInputs.toSet.view).toSet
            provider.findUtxos(allInputs).await().toOption.get
        }
        val scriptContext = tx.getScriptContextV3(utxos, RedeemerPurpose.ForSpend(scriptInput))
        val allResolvedPlutusScriptsMap =
            AllResolvedScripts.allResolvedPlutusScriptsMap(tx, utxos).toOption.get
        val plutusScript =
            scriptAddress.scriptHashOption.flatMap(allResolvedPlutusScriptsMap.get).get
        val program = plutusScript.deBruijnedProgram.toProgram
        program.runWithDebug(scriptContext)
    }

    private def lock(provider: Emulator): Utxo = {
        val utxos = provider.findUtxos(address = Alice.address).await().toOption.get

        val lockTx = txCreator.lock(
          utxos = utxos,
          value = Value.lovelace(lockAmount),
          sponsor = Alice.address,
          beneficiary = Bob.addrKeyHash,
          startTimestamp = startTimestamp,
          duration = duration,
          signer = Alice.signer
        )
        assert(provider.submit(lockTx).await().isRight)

        val lockedUtxo = lockTx.utxos.find { case (_, txOut) =>
            txOut.address == contract.address(env.network)
        }.get
        Utxo(lockedUtxo)
    }

    /** Asserts that submitting a transaction to the emulator fails with a script error containing
      * the expected message.
      *
      * Since VestingTransactions uses constMaxBudget (script is not evaluated during building),
      * validation errors are detected at emulator submission time rather than build time.
      */
    private def assertSubmitScriptFail(provider: Emulator, expectedError: String)(
        tx: Transaction
    ): Unit = {
        val result = provider.submit(tx).await()
        result match
            case Right(_) =>
                fail(s"Transaction submission should have failed but succeeded")
            case Left(sf: NodeSubmitError.ScriptFailure) =>
                if !sf.logs.exists(_.contains(expectedError)) then
                    fail(
                      s"Expected error containing '$expectedError' but got logs: ${sf.logs.mkString(", ")}"
                    )
            case Left(other) =>
                fail(s"Expected ScriptFailure but got: $other")
    }

    test("Lock creates correct UTxO") {
        val provider = createProvider
        val lockedUtxo = lock(provider)

        assert(lockedUtxo.output.value.coin.value == lockAmount)
        assert(lockedUtxo.output.address == contract.address(env.network))

        val datum = lockedUtxo.output.requireInlineDatum.to[Config]
        assert(datum.beneficiary == scalus.cardano.onchain.plutus.v1.PubKeyHash(Bob.addrKeyHash))
        assert(datum.startTimestamp == BigInt(startTimestamp))
        assert(datum.duration == BigInt(duration))
        assert(datum.initialAmount == BigInt(lockAmount))
    }

    test("Full withdrawal after vesting ends") {
        val provider = createProvider
        val lockedUtxo = lock(provider)

        // Advance past vesting end
        val afterEndSlot: SlotNo = vestingStartSlot + vestingDurationSlots + 1
        provider.setSlot(afterEndSlot)

        val utxos = provider.findUtxos(Bob.address).await().toOption.get
        val validFrom = env.slotConfig.slotToInstant(afterEndSlot)

        val withdrawTx = txCreator.withdraw(
          utxos = utxos,
          vestingUtxo = lockedUtxo,
          amount = lockAmount,
          beneficiaryAddress = Bob.address,
          beneficiaryPkh = Bob.addrKeyHash,
          sponsor = Bob.address,
          validFrom = validFrom,
          signer = Bob.signer
        )

        val result = runValidator(provider, withdrawTx, lockedUtxo.input)
        assert(result.isSuccess, s"Validator failed: $result")
        assert(result.budget == ExUnits(memory = 457064, steps = 132_276335))

        val submitResult = provider.submit(withdrawTx).await()
        assert(submitResult.isRight, s"Full withdrawal failed: $submitResult")
    }

    test("Partial 50% withdrawal at midpoint") {
        val provider = createProvider
        val lockedUtxo = lock(provider)

        // Advance to midpoint
        val midSlot: SlotNo = vestingStartSlot + vestingDurationSlots / 2
        provider.setSlot(midSlot)

        val utxos = provider.findUtxos(Bob.address).await().toOption.get
        val validFrom = env.slotConfig.slotToInstant(midSlot)
        val withdrawAmount = lockAmount / 2

        val withdrawTx = txCreator.withdraw(
          utxos = utxos,
          vestingUtxo = lockedUtxo,
          amount = withdrawAmount,
          beneficiaryAddress = Bob.address,
          beneficiaryPkh = Bob.addrKeyHash,
          sponsor = Bob.address,
          validFrom = validFrom,
          signer = Bob.signer
        )

        val result = runValidator(provider, withdrawTx, lockedUtxo.input)
        assert(result.isSuccess, s"Validator failed: $result")
        assert(result.budget == ExUnits(memory = 565716, steps = 165_745443))

        val submitResult = provider.submit(withdrawTx).await()
        assert(submitResult.isRight, s"Partial withdrawal failed: $submitResult")

        // Verify continuing output with preserved datum
        val continuingUtxo = withdrawTx.utxos.find { case (_, txOut) =>
            txOut.address == contract.address(env.network)
        }.get
        assert(continuingUtxo._2.value.coin.value == lockAmount - withdrawAmount)
        val continuingDatum = continuingUtxo._2.requireInlineDatum.to[Config]
        assert(continuingDatum.initialAmount == BigInt(lockAmount))
    }

    test("Fail: withdraw before vesting starts") {
        val provider = createProvider
        val lockedUtxo = lock(provider)

        // Before vesting starts
        val beforeStartSlot: SlotNo = vestingStartSlot - 1
        provider.setSlot(beforeStartSlot)

        val utxos = provider.findUtxos(Bob.address).await().toOption.get
        val validFrom = env.slotConfig.slotToInstant(beforeStartSlot)

        val tx = txCreator.withdraw(
          utxos = utxos,
          vestingUtxo = lockedUtxo,
          amount = lockAmount,
          beneficiaryAddress = Bob.address,
          beneficiaryPkh = Bob.addrKeyHash,
          sponsor = Bob.address,
          validFrom = validFrom,
          signer = Bob.signer
        )
        assertSubmitScriptFail(provider, "Declared amount does not match calculated amount")(tx)
    }

    test("Fail: withdraw more than available") {
        val provider = createProvider
        val lockedUtxo = lock(provider)

        // At 25% of vesting
        val quarterSlot: SlotNo = vestingStartSlot + vestingDurationSlots / 4
        provider.setSlot(quarterSlot)

        val utxos = provider.findUtxos(Bob.address).await().toOption.get
        val validFrom = env.slotConfig.slotToInstant(quarterSlot)

        // Try to withdraw 50% when only 25% is available
        val tx = txCreator.withdraw(
          utxos = utxos,
          vestingUtxo = lockedUtxo,
          amount = lockAmount / 2,
          beneficiaryAddress = Bob.address,
          beneficiaryPkh = Bob.addrKeyHash,
          sponsor = Bob.address,
          validFrom = validFrom,
          signer = Bob.signer
        )
        assertSubmitScriptFail(provider, "Declared amount does not match calculated amount")(tx)
    }

    test("Fail: wrong signer") {
        val provider = createProvider
        val lockedUtxo = lock(provider)

        // After vesting ends
        val afterEndSlot: SlotNo = vestingStartSlot + vestingDurationSlots + 1
        provider.setSlot(afterEndSlot)

        val utxos = provider.findUtxos(Eve.address).await().toOption.get
        val validFrom = env.slotConfig.slotToInstant(afterEndSlot)

        val tx = txCreator.withdraw(
          utxos = utxos,
          vestingUtxo = lockedUtxo,
          amount = lockAmount,
          beneficiaryAddress = Eve.address,
          beneficiaryPkh = Eve.addrKeyHash,
          sponsor = Eve.address,
          validFrom = validFrom,
          signer = Eve.signer
        )
        assertSubmitScriptFail(provider, "No signature from beneficiary")(tx)
    }
}
