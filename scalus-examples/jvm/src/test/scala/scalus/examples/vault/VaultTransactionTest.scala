package scalus.examples.vault

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{BuilderContext, Wallet}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.eval.{ExCPU, ExMemory}

class VaultTransactionTest extends AnyFunSuite, ScalusTest {

    private val env = TestUtil.testEnvironmentWithoutEvaluator

    private val ownerAddress = TestUtil.createTestAddress("a" * 56)

    private val defaultInitialAmount: BigInt = BigInt(10_000_000L)
    private val defaultWaitTime: BigInt = BigInt(10_000L)

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false
    )

    def lockVault(amount: BigInt, waitTime: BigInt = defaultWaitTime): Transaction = {
        val wallet = TestUtil.createTestWallet(ownerAddress, amount + 50_000_000L)
        val context = BuilderContext(env, wallet)
        Transactions(context)
            .lock(Value(Coin(amount.toLong)), waitTime, ownerAddress)
            .getOrElse(???)
    }

    def withdrawVault(
        vaultUtxo: (TransactionInput, TransactionOutput),
        validityStartSlot: Long
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(ownerAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        new Transactions(context).withdraw(vaultUtxo, validityStartSlot).getOrElse(???)
    }

    def depositVault(
        vaultUtxo: (TransactionInput, TransactionOutput),
        depositAmount: BigInt
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(ownerAddress, depositAmount + 50_000_000L)
        val context = BuilderContext(env, wallet)
        new Transactions(context)
            .deposit(vaultUtxo, Value(Coin(depositAmount.toLong)))
            .getOrElse(???)
    }

    def finalizeVault(
        vaultUtxo: (TransactionInput, TransactionOutput),
        overrideValiditySlot: Option[Long] = None
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(ownerAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        new Transactions(context)
            .finalize(vaultUtxo, ownerAddress, overrideValiditySlot)
            .fold(
              error => throw new Exception(s"Finalize failed: $error"),
              tx => tx
            )
    }

    def runValidator(tx: Transaction, utxo: Utxos, wallet: Wallet, scriptInput: TransactionInput) =
        TestUtil.runValidator(
          VaultContract.defaultCompiledContract.program,
          tx,
          utxo,
          wallet,
          scriptInput
        )

    test("vault withdrawal request") {
        val lockTx = lockVault(defaultInitialAmount)
        val vaultUtxo = TestUtil.getScriptUtxo(lockTx)

        val currentSlot = 1000L
        val withdrawTx = withdrawVault(vaultUtxo, currentSlot)

        val wallet = TestUtil.createTestWallet(ownerAddress, 50_000_000L)
        val utxos: Utxos = Map(vaultUtxo) ++ wallet.utxo

        val result = runValidator(withdrawTx, utxos, wallet, vaultUtxo._1)
        assert(result.isSuccess)

        val newVaultUtxo = TestUtil.getScriptUtxo(withdrawTx)
        newVaultUtxo._2 match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                val newDatum = d.to[State]
                assert(
                  newDatum.status == Status.Pending,
                  s"Vault state should be Pending, got ${newDatum.status}"
                )
                assert(
                  newDatum.amount == defaultInitialAmount,
                  "Vault amount should remain unchanged"
                )
                assert(
                  newDatum.finalizationDeadline > 0,
                  "Finalization deadline should be set"
                )
            case _ => fail("unreachable")
        }
    }

    test("vault deposit adds funds") {
        val lockTx = lockVault(defaultInitialAmount)
        val vaultUtxo = TestUtil.getScriptUtxo(lockTx)

        val depositAmount = BigInt(5_000_000L)
        val depositTx = depositVault(vaultUtxo, depositAmount)

        val wallet = TestUtil.createTestWallet(ownerAddress, depositAmount + 50_000_000L)
        val utxos: Utxos = Map(vaultUtxo) ++ wallet.utxo

        val result = runValidator(depositTx, utxos, wallet, vaultUtxo._1)
        assert(result.isSuccess, s"Deposit should succeed: $result")
        assert(result.budget.cpu <= ExCPU(197_512436L))
        assert(result.budget.memory <= ExMemory(744520))

        val newVaultUtxo = TestUtil.getScriptUtxo(depositTx)
        newVaultUtxo._2 match {
            case TransactionOutput.Babbage(_, value, Some(DatumOption.Inline(d)), _) =>
                val newDatum = d.to[State]
                assert(
                  newDatum.status == Status.Idle,
                  s"Vault state should remain Idle, got ${newDatum.status}"
                )
                assert(
                  newDatum.amount == defaultInitialAmount + depositAmount,
                  s"Vault amount should be ${defaultInitialAmount + depositAmount}, got ${newDatum.amount}"
                )
                assert(
                  value.coin.value == (defaultInitialAmount + depositAmount).toLong,
                  s"Vault value should match datum amount"
                )
            case _ => fail("Vault output should have inline datum")
        }
    }

    test("vault finalization fails when vault is in Idle state") {
        val lockTx = lockVault(defaultInitialAmount)
        val vaultUtxo = TestUtil.getScriptUtxo(lockTx)

        val wallet = TestUtil.createTestWallet(ownerAddress, 50_000_000L)
        val utxos: Utxos = Map(vaultUtxo) ++ wallet.utxo
        val context = BuilderContext(env, wallet)
        val finalizeTx = new Transactions(context).finalize(vaultUtxo, ownerAddress).getOrElse(???)

        val result = runValidator(finalizeTx, utxos, wallet, vaultUtxo._1)

        assert(result.isFailure, "Finalize on Idle vault should fail during transaction building")
        assert(result.logs.last.contains(VaultValidator.ContractMustBePending))
    }

    test("vault finalization succeeds after withdrawal request") {
        val lockTx = lockVault(defaultInitialAmount)
        val vaultUtxo = TestUtil.getScriptUtxo(lockTx)

        val withdrawSlot = 1000L
        val withdrawTx = withdrawVault(vaultUtxo, withdrawSlot)

        val withdrawWallet = TestUtil.createTestWallet(ownerAddress, 50_000_000L)
        val withdrawUtxos: Utxos = Map(vaultUtxo) ++ withdrawWallet.utxo

        val withdrawScriptContext =
            TestUtil.getScriptContextV3(withdrawTx, withdrawUtxos, vaultUtxo._1)
        val withdrawResult = runValidator(withdrawTx, withdrawUtxos, withdrawWallet, vaultUtxo._1)
        assert(withdrawResult.isSuccess, s"Withdraw should succeed: $withdrawResult")

        val pendingVaultUtxo = TestUtil.getScriptUtxo(withdrawTx)

        val finalizeTx = finalizeVault(pendingVaultUtxo)

        val finalizeWallet = TestUtil.createTestWallet(ownerAddress, 50_000_000L)
        val finalizeUtxos: Utxos = Map(pendingVaultUtxo) ++ finalizeWallet.utxo

        val finalizeResult =
            runValidator(finalizeTx, finalizeUtxos, finalizeWallet, pendingVaultUtxo._1)
        assert(finalizeResult.isSuccess, s"Finalize should succeed: $finalizeResult")

        val scriptOutputs = finalizeTx.body.value.outputs.filter(_.value.address.hasScript)
        assert(scriptOutputs.isEmpty, "Finalize should close vault (no script outputs)")

        val ownerOutputs = finalizeTx.body.value.outputs.filter { output =>
            output.value.address == ownerAddress
        }
        assert(ownerOutputs.nonEmpty, "Finalize should send funds to owner")

        val ownerReceivedValue = ownerOutputs.head.value.value.coin.value
        assert(
          ownerReceivedValue >= defaultInitialAmount.toLong,
          s"Owner should receive at least the vault amount, got $ownerReceivedValue"
        )
    }

    test("vault finalization fails before wait time elapses") {
        val lockTx = lockVault(defaultInitialAmount)
        val vaultUtxo = TestUtil.getScriptUtxo(lockTx)

        val withdrawSlot = 1000L
        val withdrawTx = withdrawVault(vaultUtxo, withdrawSlot)

        val withdrawWallet = TestUtil.createTestWallet(ownerAddress, 50_000_000L)
        val withdrawUtxos = Map(vaultUtxo) ++ withdrawWallet.utxo

        val withdrawScriptContext =
            TestUtil.getScriptContextV3(withdrawTx, withdrawUtxos, vaultUtxo._1)
        val withdrawResult =
            VaultContract.defaultCompiledContract.program.runWithDebug(withdrawScriptContext)
        assert(withdrawResult.isSuccess, s"Withdraw should succeed: $withdrawResult")

        val pendingVaultUtxo = TestUtil.getScriptUtxo(withdrawTx)

        // premature finalization
        val wallet = TestUtil.createTestWallet(ownerAddress, 50_000_000L)
        val utxos: Utxos = Map(pendingVaultUtxo) ++ wallet.utxo
        val context = BuilderContext(env, wallet)
        val earlySlot = Some(withdrawSlot + 1) // Just after withdrawal, before wait time
        val finalizeTx =
            new Transactions(context)
                .finalize(pendingVaultUtxo, ownerAddress, earlySlot)
                .getOrElse(???)

        val finalizeScriptContext =
            TestUtil.getScriptContextV3(finalizeTx, utxos, pendingVaultUtxo._1)
        val finalizeResult =
            VaultContract.defaultCompiledContract.program.runWithDebug(finalizeScriptContext)

        assert(finalizeResult.isFailure, "Finalize before wait time should fail")
        assert(finalizeResult.logs.last.contains(VaultValidator.DeadlineNotPassed))
    }
}
