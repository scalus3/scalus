package scalus.examples.vesting

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.api.model.{Amount, Result, Utxo}
import com.bloxbean.cardano.client.backend.api.{DefaultProtocolParamsSupplier, DefaultUtxoSupplier}
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.common.CardanoConstants.LOVELACE
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.function.helper.*
import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx, Tx}
import scalus.*
import scalus.bloxbean.Interop.toPlutusData
import scalus.bloxbean.ScalusTransactionEvaluator
import scalus.builtin.Data.*
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.SlotConfig
import scalus.ledger.api.v1.PubKeyHash
import scalus.Compiler.compileWithOptions

import java.util.Optional
import scala.util.control.Breaks.*

object VestingOffChain:
    private val mnemonic = sys.env("VESTING_MNEMONIC")
    private val blockfrostApiKey = sys.env("BLOCKFROST_API_KEY")

    private val network = Networks.preview()
    private val sender = Account.createFromMnemonic(network, mnemonic)

    private val backendService =
        BFBackendService(Constants.BLOCKFROST_PREVIEW_URL, blockfrostApiKey)

    private val utxoSupplier = DefaultUtxoSupplier(backendService.getUtxoService)
    private val protocolParamsSupplier = DefaultProtocolParamsSupplier(
      backendService.getEpochService
    )

    private inline def compiled(using options: scalus.Compiler.Options) = {
        compileWithOptions(options, VestingValidator.validate)
    }

    private val script = PlutusV3Script
        .builder()
        .cborHex(compiled.toUplc(true).plutusV3.doubleCborHex)
        .build()

    private val scriptAddress = AddressProvider.getEntAddress(script, network)

    private val scriptAddressBech32 = scriptAddress.toBech32()

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    def lock(amountAda: Long, datum: Data): Unit = {
        val tx = Tx()
            .from(sender.getBaseAddress.getAddress)
            .payToContract(scriptAddressBech32, Amount.ada(amountAda), toPlutusData(datum))

        val signedTx = QuickTxBuilder(backendService)
            .compose(tx)
            .withSigner(SignerProviders.signerFrom(sender))
            .buildAndSign()

        println("Submitting Lock TX...")
        val result: Result[String] =
            backendService.getTransactionService.submitTransaction(signedTx.serialize)

        if result.isSuccessful() then {
            println(s"Lock Tx submitted successfully: ${result.getValue}")
            println("Waiting for UTxO to appear at script address...")
        } else {
            println(s"Lock Tx submission failed: ${result.getResponse}")
            sys.exit(1)
        }
    }

    def waitForUtxo(datum: Data): Utxo = {
        var scriptUtxo: Optional[Utxo] = Optional.empty()
        var attempts = 0
        val maxAttempts = 20
        breakable {
            while attempts < maxAttempts do {
                println(s"Searching for UTXO... (Attempt ${attempts + 1}/$maxAttempts)")
                scriptUtxo = ScriptUtxoFinders.findFirstByDatumHashUsingDatum(
                  utxoSupplier,
                  scriptAddressBech32,
                  toPlutusData(datum)
                )
                if scriptUtxo.isPresent then {
                    println("Found UTXO!")
                    break
                }
                attempts += 1
                Thread.sleep(15 * 1000)
            }
        }
        scriptUtxo.orElseThrow(() =>
            new NoSuchElementException(
              s"UTXO with datum not found at $scriptAddressBech32 after $maxAttempts attempts."
            )
        )
    }

    def unlock(datum: Data, redeemer: Action, beneficiaryPubKeyHash: ByteString): Unit = {
        val scriptUtxo = waitForUtxo(datum)

        val claimAmount = scriptUtxo.getAmount
            .stream()
            .filter(_.getUnit == LOVELACE)
            .findFirst()
            .orElseThrow()

        val protocolParams = backendService.getEpochService.getProtocolParameters.getValue
        val currentSlot = backendService.getBlockService.getLatestBlock.getValue.getSlot

        val remainingAmount = claimAmount.getQuantity.subtract(redeemer.amount.bigInteger)

        // payToContract only when partial withdrawal, no payToContract when full withdrawal/all money withdrawn
        val scriptTx = if remainingAmount.compareTo(java.math.BigInteger.ZERO) > 0 then {
            println("Building partial withdrawal transaction...")
            ScriptTx()
                .collectFrom(scriptUtxo, toPlutusData(redeemer.toData))
                .payToAddress(
                  sender.baseAddress(),
                  Amount.lovelace(redeemer.amount.bigInteger)
                )
                .payToContract(
                  scriptAddressBech32,
                  Amount.lovelace(remainingAmount),
                  toPlutusData(datum)
                )
                .attachSpendingValidator(script)
        } else {
            println("Building full withdrawal transaction...")
            ScriptTx()
                .collectFrom(scriptUtxo, toPlutusData(redeemer.toData))
                .payToAddress(sender.baseAddress(), claimAmount)
                .attachSpendingValidator(script)
        }

        val signedTx = QuickTxBuilder(backendService)
            .compose(scriptTx)
            .feePayer(sender.baseAddress())
            .withSigner(SignerProviders.signerFrom(sender))
            .validFrom(currentSlot)
            .validTo(currentSlot + 1200)
            .withRequiredSigners(beneficiaryPubKeyHash.bytes)
            .ignoreScriptCostEvaluationError(false)
            .withTxEvaluator(
              ScalusTransactionEvaluator(
                slotConfig = SlotConfig.preview,
                protocolParams = protocolParams,
                utxoSupplier = utxoSupplier
              )
            )
            .buildAndSign()

        val result = backendService.getTransactionService.submitTransaction(signedTx.serialize)
        println(s"Result is: ${result}")
    }

    def main(args: Array[String]): Unit = {
        val beneficiary = ByteString.fromArray(sender.hdKeyPair().getPublicKey.getKeyHash)
        val beneficiaryPKH = PubKeyHash(beneficiary)

        val lockAmountADA = 14L
        val lockAmoutLovelace = lockAmountADA * 1_000_000L
        val unlockAmountLovelace = lockAmoutLovelace

        val latestBlock = backendService.getBlockService.getLatestBlock.getValue
        val currentTime = latestBlock.getTime * 1000L

        val startTime = currentTime - (3600L * 1000) // Vesting started 1 hour ago
        val duration = 5L * 60 * 1000 // Vesting lasted 5 minutes

        val datum = Config(beneficiaryPKH, startTime, duration, lockAmoutLovelace)

        val redeemer = Action(unlockAmountLovelace)

        println(
          s"Datum:\n  Start: ${datum.startTimestamp}\n  Duration: ${datum.duration}\n  Amount: ${datum.initialAmount}"
        )

        lock(lockAmountADA, datum.toData)
        Thread.sleep(45 * 1000)
        unlock(datum.toData, redeemer, beneficiary)
    }
