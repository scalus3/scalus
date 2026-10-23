package scalus.examples.vault

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.node.{Emulator, SubmitError}
import scalus.cardano.txbuilder.{RedeemerPurpose, TransactionSigner, TxBuilder}
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

    // Skips script evaluation in the builder for a transaction with two vault inputs. Each run
    // gets half the transaction limit, so the emulator runs the scripts instead of failing the
    // transaction on its total budget.
    private val twoScriptsEvaluator = PlutusScriptEvaluator.const(
      ExUnits(
        memory = env.protocolParams.maxTxExecutionUnits.memory / 2,
        steps = env.protocolParams.maxTxExecutionUnits.steps / 2
      )
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
        assert(
          result.budget == (ExUnits(memory = 88441, steps = 39_154_861))
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
          result.budget == (ExUnits(memory = 75913, steps = 33_608_789))
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
        assert(
          withdrawResult.budget == (ExUnits(memory = 88441, steps = 39_154_861))
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
        assert(
          finalizeResult.budget == (ExUnits(memory = 87125, steps = 36_121_069))
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
        val vaultUtxo = lockVault(provider, defaultInitialAmount)

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

    /** Locks a fresh Idle vault of `amount` for the owner; returns its UTxO. */
    private def lockVault(provider: Emulator, amount: Coin): Utxo = {
        val utxos = provider
            .queryUtxos { u => u.output.address == ownerAddress }
            .minTotal(amount + commissionAmount)
            .execute()
            .await()
            .toOption
            .get
        val lockTx = transactionCreatorFor(ownerSigner)
            .lock(utxos, amount, defaultWaitTime, ownerAddress, recoveryAddress, ownerAddress)
        assert(provider.submit(lockTx).await().isRight)
        Utxo(
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
    }

    test("Fail: one deposit output cannot satisfy two vault inputs (double satisfaction)") {
        val provider = createProvider()
        // Two vaults of one owner with one wait time: their datums differ only in `amount`.
        val vaultA = lockVault(provider, Coin.ada(10))
        val vaultB = lockVault(provider, Coin.ada(5))

        // A non-owner spends both with Deposit and pays back one output of max(A, B) + 1
        // lovelace. Each input alone sees a valid deposit; the attacker keeps about B.
        val mergedAmount = Coin.ada(10) + Coin(1)
        val mergedDatum = State(
          owner = ByteString.fromArray(ownerPkh.bytes),
          recoveryKey = ByteString.fromArray(recoveryPkh.bytes),
          status = Status.Idle,
          amount = BigInt(mergedAmount.value),
          waitTime = defaultWaitTime,
          finalizationDeadline = BigInt(0)
        )
        val utxos = provider
            .queryUtxos { u => u.output.address == recoveryAddress }
            .minTotal(commissionAmount)
            .execute()
            .await()
            .toOption
            .get
        val attackTx = TxBuilder(env, twoScriptsEvaluator)
            .spend(utxos)
            .collaterals(utxos)
            .spend(vaultA, Action.Deposit, contract)
            .spend(vaultB, Action.Deposit, contract)
            .payTo(scriptAddress, Value(mergedAmount), mergedDatum)
            .build(changeTo = recoveryAddress)
            .sign(recoverySigner)
            .transaction

        for vault <- Seq(vaultA, vaultB) do
            val result = runValidator(provider, attackTx, vault.input)
            assert(result.isFailure, s"Deposit spending two vaults must fail: $result")
            assert(result.logs.last.contains(VaultValidator.MultipleVaultInputs))
        provider.submit(attackTx).await() match
            case Left(SubmitError.ScriptFailure(message, logs, _, _)) =>
                assert(
                  (message +: logs).exists(_.contains(VaultValidator.MultipleVaultInputs)),
                  s"Unexpected script failure: $message"
                )
            case other => fail(s"The emulator must reject the scripts, got $other")
    }

    /** An owner-signed InitiateWithdrawal of a fresh vault, whose continuing datum is
      * `tamper(honest Pending datum)`, fails with `expectedError`. The transaction is built without
      * evaluation, so the validator decides.
      */
    private def assertInitiateRejected(tamper: State => State, expectedError: String): Unit =
        assertInitiateRejected(tamper, identity, expectedError)

    /** As above, and the continuing output holds `tamperValue(vault value)`. */
    private def assertInitiateRejected(
        tamper: State => State,
        tamperValue: Value => Value,
        expectedError: String
    ): Unit = {
        val result = runInitiate(tamper, tamperValue)
        assert(result.isFailure, s"Tampered withdrawal request must fail: $result")
        assert(result.logs.last.contains(expectedError))
    }

    /** Runs the vault script on a withdrawal request whose continuing output holds
      * `tamperValue(vault value)` and the datum `tamper(honest Pending datum)`.
      */
    private def runInitiate(tamper: State => State, tamperValue: Value => Value): Result = {
        val provider = createProvider()
        val vault = lockVault(provider, defaultInitialAmount)
        val validityEndTime = env.slotConfig.slotToTime(1001L)
        val datum = State(
          owner = ByteString.fromArray(ownerPkh.bytes),
          recoveryKey = ByteString.fromArray(recoveryPkh.bytes),
          status = Status.Pending,
          amount = BigInt(defaultInitialAmount.value),
          waitTime = defaultWaitTime,
          finalizationDeadline = BigInt(validityEndTime) + defaultWaitTime
        )
        val utxos = provider
            .queryUtxos { u => u.output.address == ownerAddress }
            .minTotal(commissionAmount)
            .execute()
            .await()
            .toOption
            .get
        val tx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .spend(utxos)
            .collaterals(utxos)
            .spend(vault, Action.InitiateWithdrawal, contract)
            .requireSignature(ownerPkh)
            .validTo(java.time.Instant.ofEpochMilli(validityEndTime))
            .payTo(scriptAddress, tamperValue(vault.output.value), tamper(datum))
            .build(changeTo = ownerAddress)
            .sign(ownerSigner)
            .transaction
        runValidator(provider, tx, vault.input)
    }

    test("Fail: a withdrawal request cannot set the wait time to zero") {
        // A stolen owner key writes waitTime = 0; cancel keeps it, so a second request
        // could be finalized in the next slot, leaving the recovery key no window.
        assertInitiateRejected(_.copy(waitTime = BigInt(0)), VaultValidator.WaitTimeChanged)
    }

    test("Fail: a withdrawal request cannot change the datum amount") {
        // A lower amount lets the cancel signer take the difference and makes finalize fail.
        assertInitiateRejected(
          d => d.copy(amount = d.amount - 5_000_000),
          VaultValidator.VaultAmountChanged
        )
    }

    test("Fail: a withdrawal request cannot take value out of the vault") {
        // The datum is honest, but the continuing output holds 5 ADA less: a stolen owner key
        // would take that 5 ADA in the request itself, before any wait time.
        assertInitiateRejected(identity, _ - Value.ada(5), VaultValidator.ValueNotConserved)
    }

    test("a withdrawal request may add lovelace that the datum amount records") {
        // The Pending datum is larger than the Idle one and can need more min-ADA, so the owner
        // must be able to top up the vault in the request.
        val result = runInitiate(d => d.copy(amount = d.amount + 1_000_000), _ + Value.ada(1))
        assert(result.isSuccess, s"Withdrawal request with a top-up must succeed: $result")
    }

    test("Fail: a withdrawal request cannot add lovelace that the datum amount does not record") {
        // Cancel and finalize pay out the datum amount, so unrecorded lovelace would be stranded
        assertInitiateRejected(identity, _ + Value.ada(1), VaultValidator.VaultAmountChanged)
    }
}
