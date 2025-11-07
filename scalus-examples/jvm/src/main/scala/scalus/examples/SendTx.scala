package scalus.examples

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.DefaultProtocolParamsSupplier
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.common.CardanoConstants.LOVELACE
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.function.helper.*
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.quicktx.QuickTxBuilder
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.quicktx.Tx
import scalus.*
import scalus.bloxbean.Interop.toPlutusData
import scalus.bloxbean.ScalusTransactionEvaluator
import scalus.builtin.{platform, ByteString, Data, given}
import scalus.utils.Utils

object SendTx:

    private def mnemonic = System.getenv("MNEMONIC")
    private def blockfrostApiKey = System.getenv("BLOCKFROST_API_KEY")

    private val network: Network = Networks.preview()
    private val sender = Account.createFromMnemonic(network, mnemonic)

    private val backendService =
        new BFBackendService(Constants.BLOCKFROST_PREVIEW_URL, blockfrostApiKey)

    private val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)
    private val protocolParamsSupplier = new DefaultProtocolParamsSupplier(
      backendService.getEpochService
    )

    private def publishLockingTx(scriptAddressBech32: String, datum: Data) = {
        val tx = new Tx()
            .from(sender.getBaseAddress.getAddress)
            .payToContract(scriptAddressBech32, Amount.ada(10), toPlutusData(datum))

        val quickTxBuilder = QuickTxBuilder(backendService)
        val signedTx = quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.signerFrom(sender))
            .buildAndSign()

        println(signedTx)

        backendService.getTransactionService.submitTransaction(signedTx.serialize)
    }

    private def spendLockedTx(
        script: PlutusScript,
        scriptAddressBech32: String,
        datum: Data,
        redeemer: Data,
        pubKeyHashBytes: ByteString
    ) = {
        val senderAddress = sender.getBaseAddress.toBech32
        val scriptUtxo = ScriptUtxoFinders
            .findFirstByDatumHashUsingDatum(
              utxoSupplier,
              scriptAddressBech32,
              toPlutusData(datum)
            )
            .orElseThrow

        val claimAmount = scriptUtxo.getAmount.stream
            .filter(amount => LOVELACE.equals(amount.getUnit))
            .findFirst
            .orElseThrow

        val scriptTx = new ScriptTx()
            .collectFrom(scriptUtxo, toPlutusData(redeemer))
            .payToAddress(sender.baseAddress(), claimAmount)
            .attachSpendingValidator(script)

        val quickTxBuilder = QuickTxBuilder(backendService)
        val protocolParams = backendService.getEpochService.getProtocolParameters().getValue
        val signedTx = quickTxBuilder
            .compose(scriptTx)
            .feePayer(sender.baseAddress())
            .withSigner(SignerProviders.signerFrom(sender))
            .withTxEvaluator(ScalusTransactionEvaluator(protocolParams, utxoSupplier))
            .withRequiredSigners(pubKeyHashBytes.bytes)
            .ignoreScriptCostEvaluationError(false)
            .buildAndSign()
        println(signedTx)
        backendService.getTransactionService.submitTransaction(signedTx.serialize)
    }

    def main(args: Array[String]): Unit =
        val cborHex = OptimizedPreimage.doubleCborHex
        val script = PlutusV2Script.builder().cborHex(cborHex).build()
        val scriptAddress = AddressProvider.getEntAddress(script, network)
        val scriptAddressBech32 = scriptAddress.toBech32()
        val preimage = "Scalus rocks!"
        val preimageBytes = ByteString.fromString(preimage)
        val preimageHash = platform.sha2_256(preimageBytes)
        val pubKeyHashBytes = ByteString.fromArray(sender.hdKeyPair().getPublicKey.getKeyHash)
        val pubKeyHash = pubKeyHashBytes.toHex
        import scalus.builtin.Data.toData
        val datum = (preimageHash, pubKeyHashBytes).toData
        val datumCbor = datum.toCborByteString
        val datumHash = platform.blake2b_256(datumCbor)
        val datumHashHex = datumHash.toHex
        val redeemer = preimageBytes.toData
        println(s"Script SIR")
        println(OptimizedPreimage.compiledOptimizedPreimageValidator.pretty.render(100))
        println(s"Script double CBOR: ${OptimizedPreimage.doubleCborHex}")
        println(s"Script $network Address: ${scriptAddressBech32}")
        println(s"Script Hash: ${Utils.bytesToHex(script.getScriptHash())}")
        println(
          s"Preimage: $preimage, Hex: ${preimageBytes.toHex}, Hash: $preimageHash"
        )
        println(s"PubKeyHash : $pubKeyHash")
        println(s"Datum: $datum, CBOR: ${datumCbor.toHex}")
        println(s"Datum Hash: $datumHashHex")

        def lock(): Unit = println(
          publishLockingTx(scriptAddressBech32 = scriptAddressBech32, datum = datum)
        )
        def spend(): Unit = println(
          spendLockedTx(
            scriptAddressBech32 = scriptAddressBech32,
            script = script,
            datum = datum,
            redeemer = redeemer,
            pubKeyHashBytes = pubKeyHashBytes
          )
        )

//        lock()
//        spend()
