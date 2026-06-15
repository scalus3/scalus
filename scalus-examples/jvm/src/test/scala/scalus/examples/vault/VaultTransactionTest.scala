package scalus.examples.vault

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.platform
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.{RedeemerPurpose, TransactionSigner}
import scalus.examples.vault.State
import scalus.testing.kit.TestUtil.{genesisHash, getScriptContextV3}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.eval.Result
import scalus.utils.await

class VaultTransactionTest extends AnyFunSuite, ScalusTest {

    private given env: CardanoInfo = TestUtil.testEnvironment
    private val contract = VaultContract.compiled.withErrorTraces
    private val scriptAddress = contract.address(env.network)

    // Generate real key pairs
    private val ownerKeyPair @ (ownerPrivateKey, ownerPublicKey) = generateKeyPair()
    private val ownerSigner = TransactionSigner(Set(ownerKeyPair))
    private val ownerPkh = AddrKeyHash(platform.blake2b_224(ownerPublicKey))
    private val ownerAddress = TestUtil.createTestAddress(ownerPkh)

    // Separate recovery key — the credential allowed to cancel a pending withdrawal.
    private val recoveryKeyPair @ (_, recoveryPublicKey) = generateKeyPair()
    private val recoverySigner = TransactionSigner(Set(recoveryKeyPair))
    private val recoveryPkh = AddrKeyHash(platform.blake2b_224(recoveryPublicKey))
    private val recoveryAddress = TestUtil.createTestAddress(recoveryPkh)

    private val defaultInitialAmount: Coin = Coin.ada(10)
    private val defaultWaitTime: Long = 10_000L
    private val commissionAmount = Coin(2_000_000L)

    // Transaction creator factories
    private def transactionCreatorFor(signer: TransactionSigner) = VaultTransactions(
      env = env,
      evaluator = PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost),
      signer = signer,
      contract = contract
    )

    private def transactionCreatorWithConstEvaluatorFor(signer: TransactionSigner) =
        VaultTransactions(
          env = env,
          evaluator = PlutusScriptEvaluator.constMaxBudget(env),
          signer = signer,
          contract = contract
        )

    // Provider factory
    private def createProvider(): Emulator = {
        Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) ->
                Output(address = ownerAddress, value = Value.ada(100)),
            // Fund the recovery key holder so they can pay fees for a cancel without the owner key.
            Input(genesisHash, 1) ->
                Output(address = recoveryAddress, value = Value.ada(100))
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
        val program = plutusScript.deBruijnedProgram.toProgram

        program.runWithDebug(scriptContext)
    }

    test("vault withdrawal request") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(defaultInitialAmount + commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(
                  utxos,
                  defaultInitialAmount,
                  defaultWaitTime,
                  ownerAddress,
                  recoveryAddress,
                  ownerAddress
                )
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress &&
                  u.input.transactionId == lockTx.id &&
                  u.output.value.coin >= defaultInitialAmount
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

        assert(vaultUtxo.output.value.coin == defaultInitialAmount)

        val currentSlot = 1000L
        // validity upper bound is one slot ahead so the tx is still valid when submitted at currentSlot
        val validityEndTime = env.slotConfig.slotToTime(currentSlot + 1)

        val withdrawTx = {
            val utxos = provider
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .withdraw(utxos, utxos, vaultUtxo, ownerAddress, validityEndTime)
        }

        val result = runValidator(provider, withdrawTx, vaultUtxo.input)
        assert(result.isSuccess)
        // TODO: review after changing PairData representations
        // assert(result.budget == ExUnits(memory = 264301, steps = 78_973160))
        assert(
          result.budget == (ExUnits(memory = 191122L, steps = 64733791L))
        )

        provider.setSlot(currentSlot)

        assert(provider.submit(withdrawTx).await().isRight)

        val newVaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress &&
                  u.input.transactionId == withdrawTx.id &&
                  u.output.value.coin >= defaultInitialAmount
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

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
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(defaultInitialAmount + commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(
                  utxos,
                  defaultInitialAmount,
                  defaultWaitTime,
                  ownerAddress,
                  recoveryAddress,
                  ownerAddress
                )
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress &&
                  u.input.transactionId == lockTx.id &&
                  u.output.value.coin >= defaultInitialAmount
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

        val depositAmount = Value.lovelace(5_000_000L)

        val depositTx = {
            val utxos = provider
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(depositAmount.coin + commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .deposit(utxos, utxos, vaultUtxo, depositAmount, ownerAddress)
        }

        val result = runValidator(provider, depositTx, vaultUtxo.input)
        assert(result.isSuccess, s"Deposit should succeed: $result")
        assert(
          result.budget == (ExUnits(memory = 217125L, steps = 70557940L))
        )

        assert(provider.submit(depositTx).await().isRight)

        val newVaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress &&
                  u.input.transactionId == depositTx.id &&
                  u.output.value.coin >= defaultInitialAmount + depositAmount.coin
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

        newVaultUtxo.output match {
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
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(defaultInitialAmount + commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(
                  utxos,
                  defaultInitialAmount,
                  defaultWaitTime,
                  ownerAddress,
                  recoveryAddress,
                  ownerAddress
                )
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress &&
                  u.input.transactionId == lockTx.id &&
                  u.output.value.coin >= defaultInitialAmount
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

        val currentTime = env.slotConfig.slotToTime(1000L)

        val finalizeTx = {
            val utxos = provider
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorWithConstEvaluatorFor(ownerSigner)
                .finalize(utxos, utxos, vaultUtxo, ownerAddress, ownerAddress, currentTime)
        }

        val result = runValidator(provider, finalizeTx, vaultUtxo.input)

        assert(result.isFailure, "Finalize on Idle vault should fail")
        assert(result.logs.last.contains(VaultValidator.ContractMustBePending))
    }

    test("vault finalization succeeds after withdrawal request") {
        val provider = createProvider()

        val lockTx = {
            val utxos = provider
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(defaultInitialAmount + commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(
                  utxos,
                  defaultInitialAmount,
                  defaultWaitTime,
                  ownerAddress,
                  recoveryAddress,
                  ownerAddress
                )
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress &&
                  u.input.transactionId == lockTx.id &&
                  u.output.value.coin >= defaultInitialAmount
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

        val withdrawSlot = 1000L
        val withdrawTime = env.slotConfig.slotToTime(withdrawSlot + 1)

        val withdrawTx = {
            val utxos = provider
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .withdraw(utxos, utxos, vaultUtxo, ownerAddress, withdrawTime)
        }

        val withdrawResult = runValidator(provider, withdrawTx, vaultUtxo.input)
        assert(withdrawResult.isSuccess, s"Withdraw should succeed: $withdrawResult")
        // TODO: review after changing PairData representations
        // assert(withdrawResult.budget == ExUnits(memory = 264301, steps = 78_973160))
        assert(
          withdrawResult.budget == (ExUnits(memory = 191122L, steps = 64733791L))
        )

        provider.setSlot(withdrawSlot)
        assert(provider.submit(withdrawTx).await().isRight)

        val pendingVaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress &&
                  u.input.transactionId == withdrawTx.id &&
                  u.output.value.coin >= defaultInitialAmount
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

        // Calculate finalization time (after wait time; deadline is anchored to withdrawSlot + 1)
        val finalizeSlot = withdrawSlot + 1 + (defaultWaitTime / env.slotConfig.slotLength) + 1
        val finalizeTime = env.slotConfig.slotToTime(finalizeSlot)

        val finalizeTx = {
            val utxos = provider
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(commissionAmount)
                .execute()
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

        val finalizeResult = runValidator(provider, finalizeTx, pendingVaultUtxo.input)
        assert(finalizeResult.isSuccess, s"Finalize should succeed: $finalizeResult")
        // TODO: review after changing PairData representations
        // assert(finalizeResult.budget == ExUnits(memory = 324554, steps = 92_685932))
        assert(
          finalizeResult.budget == (ExUnits(memory = 216106, steps = 67544582))
        )

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
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(defaultInitialAmount + commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .lock(
                  utxos,
                  defaultInitialAmount,
                  defaultWaitTime,
                  ownerAddress,
                  recoveryAddress,
                  ownerAddress
                )
        }

        assert(provider.submit(lockTx).await().isRight)

        val vaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress &&
                  u.input.transactionId == lockTx.id &&
                  u.output.value.coin >= defaultInitialAmount
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

        val withdrawSlot = 1000L
        val withdrawTime = env.slotConfig.slotToTime(withdrawSlot + 1)

        val withdrawTx = {
            val utxos = provider
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(commissionAmount)
                .execute()
                .await()
                .toOption
                .get

            transactionCreatorFor(ownerSigner)
                .withdraw(utxos, utxos, vaultUtxo, ownerAddress, withdrawTime)
        }

        val withdrawResult = runValidator(provider, withdrawTx, vaultUtxo.input)
        assert(withdrawResult.isSuccess, s"Withdraw should succeed: $withdrawResult")

        provider.setSlot(withdrawSlot)
        assert(provider.submit(withdrawTx).await().isRight)

        val pendingVaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress &&
                  u.input.transactionId == withdrawTx.id &&
                  u.output.value.coin >= defaultInitialAmount
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

        // Try to finalize just after withdrawal (before wait time)
        val earlySlot = withdrawSlot + 1
        val earlyTime = env.slotConfig.slotToTime(earlySlot)

        val finalizeTx = {
            val utxos = provider
                .queryUtxos { u => u.output.address == ownerAddress }
                .minTotal(commissionAmount)
                .execute()
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

        val finalizeResult = runValidator(provider, finalizeTx, pendingVaultUtxo.input)

        assert(finalizeResult.isFailure, "Finalize before wait time should fail")
        assert(finalizeResult.logs.last.contains(VaultValidator.DeadlineNotPassed))
    }

    /** Locks a vault and moves it to Pending via a withdrawal request; returns the pending UTxO. */
    private def lockAndRequest(provider: Emulator): Utxo = {
        val lockUtxos = provider
            .queryUtxos { u => u.output.address == ownerAddress }
            .minTotal(defaultInitialAmount + commissionAmount)
            .execute()
            .await()
            .toOption
            .get
        val lockTx = transactionCreatorFor(ownerSigner)
            .lock(
              lockUtxos,
              defaultInitialAmount,
              defaultWaitTime,
              ownerAddress,
              recoveryAddress,
              ownerAddress
            )
        assert(provider.submit(lockTx).await().isRight)
        val vaultUtxo = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress && u.input.transactionId == lockTx.id
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )

        val withdrawSlot = 1000L
        val withdrawTime = env.slotConfig.slotToTime(withdrawSlot + 1)
        val withdrawUtxos = provider
            .queryUtxos { u => u.output.address == ownerAddress }
            .minTotal(commissionAmount)
            .execute()
            .await()
            .toOption
            .get
        val withdrawTx = transactionCreatorFor(ownerSigner)
            .withdraw(withdrawUtxos, withdrawUtxos, vaultUtxo, ownerAddress, withdrawTime)
        provider.setSlot(withdrawSlot)
        assert(provider.submit(withdrawTx).await().isRight)
        Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress && u.input.transactionId == withdrawTx.id
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )
    }

    test("recovery key cancels a pending withdrawal") {
        val provider = createProvider()
        val pendingVaultUtxo = lockAndRequest(provider)

        val utxos = provider
            .queryUtxos { u => u.output.address == recoveryAddress }
            .minTotal(commissionAmount)
            .execute()
            .await()
            .toOption
            .get
        val cancelTx = transactionCreatorFor(recoverySigner)
            .cancel(utxos, utxos, pendingVaultUtxo, recoveryPkh, recoveryAddress)

        val result = runValidator(provider, cancelTx, pendingVaultUtxo.input)
        assert(result.isSuccess, s"Recovery-key cancel should succeed: $result")
        assert(provider.submit(cancelTx).await().isRight)

        val idleVault = Utxo(
          provider
              .queryUtxos { u =>
                  u.output.address == scriptAddress && u.input.transactionId == cancelTx.id
              }
              .execute()
              .await()
              .toOption
              .get
              .head
        )
        idleVault.output match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                assert(d.to[State].status == Status.Idle, "Cancel should return the vault to Idle")
            case _ => fail("Vault output should have inline datum")
        }
    }

    test("owner cannot cancel a withdrawal — only the recovery key can") {
        val provider = createProvider()
        val pendingVaultUtxo = lockAndRequest(provider)

        val utxos = provider
            .queryUtxos { u => u.output.address == ownerAddress }
            .minTotal(commissionAmount)
            .execute()
            .await()
            .toOption
            .get
        // The owner signs and points the cancel at their own key — the threat model is a stolen
        // owner key, so the validator must reject this and require the separate recovery key.
        val cancelTx = transactionCreatorWithConstEvaluatorFor(ownerSigner)
            .cancel(utxos, utxos, pendingVaultUtxo, ownerPkh, ownerAddress)

        val result = runValidator(provider, cancelTx, pendingVaultUtxo.input)
        assert(result.isFailure, "Owner-signed cancel must fail")
        assert(result.logs.last.contains(VaultValidator.RecoveryKeyMustSign))
    }
}
