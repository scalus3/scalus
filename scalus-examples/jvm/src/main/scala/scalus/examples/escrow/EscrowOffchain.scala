package scalus.examples.escrow

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.{AddressProvider, Credential}
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
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compileWithOptions, Options}
import scalus.ledger.api.v1.PubKeyHash

import java.math.BigInteger
import java.util.Optional
import scala.util.control.Breaks.*

object EscrowOffChain:
    private val blockfrostApiKey = sys.env("BLOCKFROST_API_KEY")
    private val network = Networks.preview()

    private val sellerMnemonic = sys.env("SELLER_MNEMONIC")
    private val buyerMnemonic = sys.env("BUYER_MNEMONIC")

    private val seller = Account.createFromMnemonic(network, sellerMnemonic)
    private val buyerAccount = Account.createFromMnemonic(network, buyerMnemonic)

    private val backendService =
        BFBackendService(Constants.BLOCKFROST_PREVIEW_URL, blockfrostApiKey)

    private val utxoSupplier = DefaultUtxoSupplier(backendService.getUtxoService)
    private val protocolParamsSupplier = DefaultProtocolParamsSupplier(
      backendService.getEpochService
    )

    private inline def compiled(using Options) =
        compileWithOptions(summon[Options], EscrowValidator.validate)
    private inline def doubleCborHex(using Options) =
        compiled.toUplc(true).plutusV3.doubleCborHex

    private val script = PlutusV3Script
        .builder()
        .cborHex(doubleCborHex)
        .build()

    private val scriptAddress = AddressProvider.getEntAddress(script, network)

    private val scriptAddressBech32 = scriptAddress.toBech32()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    def initialize(
        buyerPubKeyHash: ByteString,
        paymentAmountAda: Long,
        initialDepositAda: Long = 2L
    ): Unit = {
        val sellerPubKeyHash = ByteString.fromArray(seller.hdKeyPair().getPublicKey.getKeyHash)
        val buyerPKH = PubKeyHash(buyerPubKeyHash)
        val sellerPKH = PubKeyHash(sellerPubKeyHash)
        val paymentAmountLovelace = paymentAmountAda * 1_000_000L
        val initializationAmountLovelace = initialDepositAda * 1_000_000L

        val datum =
            Config(sellerPKH, buyerPKH, paymentAmountLovelace, initializationAmountLovelace)

        val tx = Tx()
            .from(seller.getBaseAddress.getAddress)
            .payToContract(
              scriptAddressBech32,
              Amount.ada(initialDepositAda),
              toPlutusData(datum.toData)
            )

        val signedTx = QuickTxBuilder(backendService)
            .compose(tx)
            .withSigner(SignerProviders.signerFrom(seller))
            .buildAndSign()

        println("Submitting Initialize Escrow TX...")
        val result: Result[String] =
            backendService.getTransactionService.submitTransaction(signedTx.serialize)
        println(s"Result is: ${result}")

        if result.isSuccessful() then {
            println(s"Initialize Tx submitted successfully: ${result.getValue}")
            println("Waiting for UTxO to appear at script address...")
        } else {
            println(s"Initialize Tx submission failed: ${result.getResponse}")
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
              s"UTXO with datum not found at $scriptAddressBech32."
            )
        )
    }

    def waitForUtxoWithAmount(datum: Data, expectedAmount: BigInteger): Utxo = {
        var scriptUtxo: Optional[Utxo] = Optional.empty()
        var attempts = 0
        val maxAttempts = 20
        breakable {
            while attempts < maxAttempts do {
                println(
                  s"Searching for UTXO with ${expectedAmount} lovelace... (Attempt ${attempts + 1}/$maxAttempts)"
                )

                val allUtxos = utxoSupplier.getPage(
                  scriptAddressBech32,
                  50,
                  0,
                  com.bloxbean.cardano.client.api.common.OrderEnum.asc
                )

                val targetDatum = toPlutusData(datum)
                scriptUtxo = allUtxos
                    .stream()
                    .filter(utxo => {

                        val datumMatches = utxo.getInlineDatum != null &&
                            utxo.getInlineDatum.equals(targetDatum.serializeToHex())

                        val amountMatches = utxo.getAmount
                            .stream()
                            .filter(_.getUnit == LOVELACE)
                            .findFirst()
                            .map(_.getQuantity.equals(expectedAmount))
                            .orElse(false)

                        datumMatches && amountMatches
                    })
                    .findFirst()

                if scriptUtxo.isPresent then {
                    val amount = scriptUtxo
                        .get()
                        .getAmount
                        .stream()
                        .filter(_.getUnit == LOVELACE)
                        .findFirst()
                        .get()
                        .getQuantity
                    println(s"Found UTXO with ${amount} lovelace!")
                    break
                }
                attempts += 1
                Thread.sleep(15 * 1000)
            }
        }
        scriptUtxo.orElseThrow(() =>
            new NoSuchElementException(
              s"UTXO with datum and amount ${expectedAmount} not found at $scriptAddressBech32 after $maxAttempts attempts."
            )
        )
    }

    def deposit(buyerAccount: Account, datum: Data, escrowDatum: Config): Unit = {
        // Look for UTXO with only initialization amount (not yet deposited to)
        val scriptUtxo = waitForUtxoWithAmount(datum, escrowDatum.initializationAmount.bigInteger)
        println("scriptUtxo: " + scriptUtxo)

        val currentAmount = scriptUtxo.getAmount
            .stream()
            .filter(_.getUnit == LOVELACE)
            .findFirst()
            .orElseThrow()

        val totalRequiredAmount =
            escrowDatum.escrowAmount.bigInteger.add(escrowDatum.initializationAmount.bigInteger)
        val action = Action.Deposit

        val scriptTx = ScriptTx()
            .collectFrom(scriptUtxo, toPlutusData(action.toData))
            .payToContract(
              scriptAddressBech32,
              Amount.lovelace(totalRequiredAmount),
              toPlutusData(datum)
            )
            .payToAddress(
              buyerAccount.baseAddress(),
              Amount.lovelace(BigInteger.valueOf(1000000)) // 1 ADA minimum output for buyer change
            )
            .attachSpendingValidator(script)

        val protocolParams = backendService.getEpochService.getProtocolParameters.getValue
        val currentSlot = backendService.getBlockService.getLatestBlock.getValue.getSlot

        val signedTx = QuickTxBuilder(backendService)
            .compose(scriptTx)
            .feePayer(buyerAccount.baseAddress())
            .withSigner(SignerProviders.signerFrom(buyerAccount))
            .validFrom(currentSlot)
            .validTo(currentSlot + 1200)
            .withRequiredSigners(
              ByteString.fromArray(buyerAccount.hdKeyPair().getPublicKey.getKeyHash).bytes
            )
            .ignoreScriptCostEvaluationError(false)
            .withTxEvaluator(
              ScalusTransactionEvaluator(
                slotConfig = SlotConfig.preview,
                protocolParams = protocolParams,
                utxoSupplier = utxoSupplier
              )
            )
            .buildAndSign()

        println("Submitting Deposit TX...")
        val result = backendService.getTransactionService.submitTransaction(signedTx.serialize)
        println(s"Result is: ${result}")
    }

    def pay(buyerAccount: Account, datum: Data, escrowDatum: Config): Unit = {
        // Look for UTXO with full amount (escrow + initialization)
        val fullAmount = (escrowDatum.escrowAmount + escrowDatum.initializationAmount).bigInteger
        val scriptUtxo = waitForUtxoWithAmount(datum, fullAmount)
        println("scriptUtxo: " + scriptUtxo)

        val contractAmount = scriptUtxo.getAmount
            .stream()
            .filter(_.getUnit == LOVELACE)
            .findFirst()
            .orElseThrow()

        val action = Action.Pay

        val scriptTx = ScriptTx()
            .collectFrom(scriptUtxo, toPlutusData(action.toData))
            .payToAddress(
              seller.baseAddress(),
              Amount.lovelace(
                (escrowDatum.escrowAmount + escrowDatum.initializationAmount).bigInteger
              )
            )
            .payToAddress(
              AddressProvider
                  .getEntAddress(
                    Credential.fromKey(escrowDatum.buyer.hash.bytes),
                    network
                  )
                  .toBech32(),
              Amount.lovelace(BigInteger.valueOf(1000000)) // 1 ADA minimum output
            )
            .attachSpendingValidator(script)

        val protocolParams = backendService.getEpochService.getProtocolParameters.getValue
        val currentSlot = backendService.getBlockService.getLatestBlock.getValue.getSlot

        val signedTx = QuickTxBuilder(backendService)
            .compose(scriptTx)
            .feePayer(buyerAccount.baseAddress())
            .withSigner(SignerProviders.signerFrom(buyerAccount))
            .validFrom(currentSlot)
            .validTo(currentSlot + 1200)
            .withRequiredSigners(
              ByteString.fromArray(buyerAccount.hdKeyPair().getPublicKey.getKeyHash).bytes
            )
            .ignoreScriptCostEvaluationError(false)
            .withTxEvaluator(
              ScalusTransactionEvaluator(
                slotConfig = SlotConfig.preview,
                protocolParams = protocolParams,
                utxoSupplier = utxoSupplier
              )
            )
            .buildAndSign()

        println("Submitting Pay TX...")
        val result = backendService.getTransactionService.submitTransaction(signedTx.serialize)
        println(s"Result is: ${result}")
    }

    def refund(datum: Data, escrowDatum: Config, buyerAddress: String): Unit = {
        // Look for UTXO with full amount (escrow + initialization)
        val fullAmount = (escrowDatum.escrowAmount + escrowDatum.initializationAmount).bigInteger
        val scriptUtxo = waitForUtxoWithAmount(datum, fullAmount)
        println("scriptUtxo: " + scriptUtxo)

        val contractAmount = scriptUtxo.getAmount
            .stream()
            .filter(_.getUnit == LOVELACE)
            .findFirst()
            .orElseThrow()

        val action = Action.Refund

        val scriptTx = ScriptTx()
            .collectFrom(scriptUtxo, toPlutusData(action.toData))
            .payToAddress(
              buyerAddress,
              Amount.lovelace(escrowDatum.escrowAmount.bigInteger)
            )
            .payToAddress(
              seller.baseAddress(),
              Amount.lovelace(escrowDatum.initializationAmount.bigInteger)
            )
            .attachSpendingValidator(script)

        val protocolParams = backendService.getEpochService.getProtocolParameters.getValue
        val currentSlot = backendService.getBlockService.getLatestBlock.getValue.getSlot

        val sellerPubKeyHash = ByteString.fromArray(seller.hdKeyPair().getPublicKey.getKeyHash)

        val signedTx = QuickTxBuilder(backendService)
            .compose(scriptTx)
            .feePayer(seller.baseAddress())
            .withSigner(SignerProviders.signerFrom(seller))
            .validFrom(currentSlot)
            .validTo(currentSlot + 1200)
            .withRequiredSigners(sellerPubKeyHash.bytes)
            .ignoreScriptCostEvaluationError(false)
            .withTxEvaluator(
              ScalusTransactionEvaluator(
                slotConfig = SlotConfig.preview,
                protocolParams = protocolParams,
                utxoSupplier = utxoSupplier
              )
            )
            .buildAndSign()

        println("Submitting Refund TX...")
        val result = backendService.getTransactionService.submitTransaction(signedTx.serialize)
        println(s"Result is: ${result}")
    }

    def main(args: Array[String]): Unit = {
        val buyerPubKeyHash = ByteString.fromArray(buyerAccount.hdKeyPair().getPublicKey.getKeyHash)

        val paymentAmountAda = 10L
        val initialDepositAda = 2L

        val sellerPubKeyHash = ByteString.fromArray(seller.hdKeyPair().getPublicKey.getKeyHash)
        val buyerPKH = PubKeyHash(buyerPubKeyHash)
        val sellerPKH = PubKeyHash(sellerPubKeyHash)
        val paymentAmountLovelace = paymentAmountAda * 1_000_000L
        val initializationAmountLovelace = initialDepositAda * 1_000_000L

        val datum =
            Config(sellerPKH, buyerPKH, paymentAmountLovelace, initializationAmountLovelace)

        println(s"Escrow Datum:")
        println(s"  Buyer: ${buyerPKH}")
        println(s"  Seller: ${sellerPKH}")
        println(s"  Payment Amount: ${paymentAmountLovelace} lovelace")
        println(s"  Initial Deposit: ${initialDepositAda} ADA")

        println("Step 1: Seller initializes escrow contract")
        initialize(buyerPubKeyHash, paymentAmountAda, initialDepositAda)
        Thread.sleep(45 * 1000)

        println("Step 2: Buyer deposits payment")
        deposit(buyerAccount, datum.toData, datum)
        println("Waiting longer for deposit transaction to be confirmed...")
        Thread.sleep(120 * 1000)

        println("Step 3: Buyer releases payment to seller")
        pay(buyerAccount, datum.toData, datum)

        // println("Step 3: Seller refunds to buyer")
        // refund(datum.toData, datum, buyerAccount.baseAddress())
    }
