package scalus.examples.vault

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{platform, ByteString}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.{RedeemerPurpose, TransactionSigner}
import scalus.examples.vault.State
import scalus.testing.kit.TestUtil.getScriptContextV3
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.Program
import scalus.uplc.eval.Result
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

class VaultTransactionTest extends AnyFunSuite, ScalusTest {

    private given env: CardanoInfo = TestUtil.testEnvironment
    private val contract = VaultContract.withErrorTraces
    private val scriptAddress = contract.address(env.network)

    // Generate real key pairs
    private val ownerKeyPair @ (ownerPrivateKey, ownerPublicKey) = generateKeyPair()
    private val ownerSigner = TransactionSigner(Set(ownerKeyPair))
    private val ownerPkh = AddrKeyHash(platform.blake2b_224(ownerPublicKey))
    private val ownerAddress = TestUtil.createTestAddress(ownerPkh)
    private val changeAddress = TestUtil.createTestAddress("a" * 56)

    private val defaultInitialAmount: Coin = Coin.ada(10)
    private val defaultWaitTime: Long = 10_000L
    private val commissionAmount = Coin(2_000_000L)

    // Transaction creator factories
    private def transactionCreatorFor(signer: TransactionSigner) = VaultTransactionCreator(
      env = env,
      evaluator = PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost),
      signer = signer,
      contract = contract
    )

    private def transactionCreatorWithConstEvaluatorFor(signer: TransactionSigner) =
        VaultTransactionCreator(
          env = env,
          evaluator = PlutusScriptEvaluator.constMaxBudget(env),
          signer = signer,
          contract = contract
        )

    // Provider factory
    private def createProvider(): Emulator = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                TransactionOutput(
                  address = ownerAddress,
                  value = Value.lovelace(100_000_000L)
                )
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }

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
        val program = Program.fromCborByteString(plutusScript.script)

        program.runWithDebug(scriptContext)
    }

    test("vault withdrawal request") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(defaultInitialAmount + commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(utxos, defaultInitialAmount, defaultWaitTime, ownerAddress, ownerAddress)
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              minAmount = Some(defaultInitialAmount)
            )
            .await()
            .toOption
            .get

        assert(vaultUtxo._2.value.coin == defaultInitialAmount)

        val currentSlot = 1000L
        val currentTime = env.slotConfig.slotToTime(currentSlot)

        val withdrawTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .withdraw(utxos, utxos, vaultUtxo, ownerAddress, currentTime)
        }

        val result = runValidator(provider, withdrawTx, vaultUtxo._1)
        assert(result.isSuccess)

        provider.setSlot(currentSlot)

        assert(provider.submit(withdrawTx).await().isRight)

        val newVaultUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(withdrawTx.id),
              minAmount = Some(defaultInitialAmount)
            )
            .await()
            .toOption
            .get

        newVaultUtxo.output match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                val newDatum = d.to[State]
                assert(
                  newDatum.status == Status.Pending,
                  s"Vault state should be Pending, got ${newDatum.status}"
                )
                assert(
                  newDatum.amount == defaultInitialAmount.value,
                  "Vault amount should remain unchanged"
                )
                assert(
                  newDatum.finalizationDeadline > 0,
                  "Finalization deadline should be set"
                )
            case _ => fail("Vault output should have inline datum")
        }
    }

    test("vault deposit adds funds") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(defaultInitialAmount + commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(utxos, defaultInitialAmount, defaultWaitTime, ownerAddress, ownerAddress)
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              minAmount = Some(defaultInitialAmount)
            )
            .await()
            .toOption
            .get

        val depositAmount = Value.lovelace(5_000_000L)

        val depositTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(depositAmount.coin + commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .deposit(utxos, utxos, vaultUtxo, depositAmount, ownerAddress)
        }

        val result = runValidator(provider, depositTx, vaultUtxo._1)
        assert(result.isSuccess, s"Deposit should succeed: $result")
        assert(result.budget.steps <= 197_512436L)
        assert(result.budget.memory <= 744520)

        assert(provider.submit(depositTx).await().isRight)

        val newVaultUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(depositTx.id),
              minAmount = Some(defaultInitialAmount + depositAmount.coin)
            )
            .await()
            .toOption
            .get

        newVaultUtxo._2 match {
            case TransactionOutput.Babbage(_, value, Some(DatumOption.Inline(d)), _) =>
                val newDatum = d.to[State]
                assert(
                  newDatum.status == Status.Idle,
                  s"Vault state should remain Idle, got ${newDatum.status}"
                )
                assert(
                  newDatum.amount == BigInt((defaultInitialAmount + depositAmount.coin).value),
                  s"Vault amount should be ${(defaultInitialAmount + depositAmount.coin).value}, got ${newDatum.amount}"
                )
                assert(
                  value.coin.value == (defaultInitialAmount + depositAmount.coin).value,
                  s"Vault value should match datum amount"
                )
            case _ => fail("Vault output should have inline datum")
        }
    }

    test("vault finalization fails when vault is in Idle state") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(defaultInitialAmount + commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(utxos, defaultInitialAmount, defaultWaitTime, ownerAddress, ownerAddress)
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              minAmount = Some(defaultInitialAmount)
            )
            .await()
            .toOption
            .get

        val currentTime = env.slotConfig.slotToTime(1000L)

        val finalizeTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(ownerSigner)
                .finalize(utxos, utxos, vaultUtxo, ownerAddress, ownerAddress, currentTime)
        }

        val result = runValidator(provider, finalizeTx, vaultUtxo._1)

        assert(result.isFailure, "Finalize on Idle vault should fail")
        assert(result.logs.last.contains(VaultValidator.ContractMustBePending))
    }

    test("vault finalization succeeds after withdrawal request") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(defaultInitialAmount + commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(utxos, defaultInitialAmount, defaultWaitTime, ownerAddress, ownerAddress)
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              minAmount = Some(defaultInitialAmount)
            )
            .await()
            .toOption
            .get

        val withdrawSlot = 1000L
        val withdrawTime = env.slotConfig.slotToTime(withdrawSlot)

        val withdrawTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .withdraw(utxos, utxos, vaultUtxo, ownerAddress, withdrawTime)
        }

        val withdrawResult = runValidator(provider, withdrawTx, vaultUtxo._1)
        assert(withdrawResult.isSuccess, s"Withdraw should succeed: $withdrawResult")

        provider.setSlot(withdrawSlot)
        assert(provider.submit(withdrawTx).await().isRight)

        val pendingVaultUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(withdrawTx.id),
              minAmount = Some(defaultInitialAmount)
            )
            .await()
            .toOption
            .get

        // Calculate finalization time (after wait time)
        val finalizeSlot = withdrawSlot + (defaultWaitTime / env.slotConfig.slotLength) + 1
        val finalizeTime = env.slotConfig.slotToTime(finalizeSlot)

        val finalizeTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .finalize(
                  utxos,
                  utxos,
                  pendingVaultUtxo,
                  ownerAddress,
                  ownerAddress,
                  finalizeTime
                )
        }

        val finalizeResult = runValidator(provider, finalizeTx, pendingVaultUtxo._1)
        assert(finalizeResult.isSuccess, s"Finalize should succeed: $finalizeResult")

        provider.setSlot(finalizeSlot)
        assert(provider.submit(finalizeTx).await().isRight)

        val scriptOutputs = finalizeTx.body.value.outputs.filter(_.value.address.hasScript)
        assert(scriptOutputs.isEmpty, "Finalize should close vault (no script outputs)")

        val ownerOutputs = finalizeTx.body.value.outputs.filter { output =>
            output.value.address == ownerAddress
        }
        assert(ownerOutputs.nonEmpty, "Finalize should send funds to owner")

        val ownerReceivedValue = ownerOutputs.map(_.value.value.coin.value).sum
        assert(
          ownerReceivedValue >= defaultInitialAmount.value,
          s"Owner should receive at least the vault amount, got $ownerReceivedValue"
        )
    }

    test("vault finalization fails before wait time elapses") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(defaultInitialAmount + commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(utxos, defaultInitialAmount, defaultWaitTime, ownerAddress, ownerAddress)
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(lockTx.id),
              minAmount = Some(defaultInitialAmount)
            )
            .await()
            .toOption
            .get

        val withdrawSlot = 1000L
        val withdrawTime = env.slotConfig.slotToTime(withdrawSlot)

        val withdrawTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .withdraw(utxos, utxos, vaultUtxo, ownerAddress, withdrawTime)
        }

        val withdrawResult = runValidator(provider, withdrawTx, vaultUtxo._1)
        assert(withdrawResult.isSuccess, s"Withdraw should succeed: $withdrawResult")

        provider.setSlot(withdrawSlot)
        assert(provider.submit(withdrawTx).await().isRight)

        val pendingVaultUtxo = provider
            .findUtxo(
              address = scriptAddress,
              transactionId = Some(withdrawTx.id),
              minAmount = Some(defaultInitialAmount)
            )
            .await()
            .toOption
            .get

        // Try to finalize just after withdrawal (before wait time)
        val earlySlot = withdrawSlot + 1
        val earlyTime = env.slotConfig.slotToTime(earlySlot)

        val finalizeTx = {
            val utxos = provider
                .findUtxos(
                  address = ownerAddress,
                  minRequiredTotalAmount = Some(commissionAmount)
                )
                .await()
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(ownerSigner)
                .finalize(
                  utxos,
                  utxos,
                  pendingVaultUtxo,
                  ownerAddress,
                  ownerAddress,
                  earlyTime
                )
        }

        val finalizeResult = runValidator(provider, finalizeTx, pendingVaultUtxo._1)

        assert(finalizeResult.isFailure, "Finalize before wait time should fail")
        assert(finalizeResult.logs.last.contains(VaultValidator.DeadlineNotPassed))
    }
}
