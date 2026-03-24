package scalus.cardano.deploy

import scalus.cardano.address.Address
import scalus.cardano.blueprint.Contract
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.JvmEd25519Signer
import scalus.uplc.Program
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

/** Deploys a compiled Cardano smart contract as a reference script UTXO.
  */
object Deployer {

    private val Timeout = 30.seconds

    /** Deploy a Contract's script as a reference script UTXO. Blocks the calling thread.
      *
      * @param contractClassName
      *   fully qualified class name of the Contract singleton, including the trailing "$" (e.g.
      *   "myapp.MyContract$")
      * @param network
      *   Cardano network: "preview", "preprod", or "mainnet"
      * @param blockfrostApiKey
      *   Blockfrost API key
      * @param mnemonic
      *   BIP-39 mnemonic for signing
      * @param targetAddress
      *   Bech32 address where the reference script UTXO will be created
      * @return
      *   transaction hash hex string
      */
    def deploy(
        contractClassName: String,
        network: String,
        blockfrostApiKey: String,
        mnemonic: String,
        targetAddress: String
    ): String = {
        val cls = Class.forName(contractClassName)
        val contract = cls.getField("MODULE$").get(null).asInstanceOf[Contract]

        val validator = contract.blueprint.validators.headOption
            .getOrElse(
              throw IllegalStateException(s"Contract $contractClassName has no validators")
            )
        val compiledCodeHex = validator.compiledCode
            .getOrElse(
              throw IllegalStateException(s"Contract $contractClassName has no compiledCode")
            )
        val language =
            contract.blueprint.preamble.plutusVersion.getOrElse(Language.PlutusV3)
        val program = Program.fromCborHex(compiledCodeHex)
        val script: Script = language match
            case Language.PlutusV1 => Script.PlutusV1(program.cborByteString)
            case Language.PlutusV2 => Script.PlutusV2(program.cborByteString)
            case Language.PlutusV3 => Script.PlutusV3(program.cborByteString)
            case other => throw IllegalArgumentException(s"Unsupported language: $other")

        given ExecutionContext = ExecutionContext.global
        val provider = (network match
            case "preview" => BlockfrostProvider.preview(blockfrostApiKey)
            case "preprod" => BlockfrostProvider.preprod(blockfrostApiKey)
            case "mainnet" => BlockfrostProvider.mainnet(blockfrostApiKey)
            case other     => throw IllegalArgumentException(s"Unknown network: $other")
        ).await(Timeout)

        given scalus.crypto.ed25519.Ed25519Signer = JvmEd25519Signer
        val account = HdAccount.fromMnemonic(mnemonic)
        val senderAddress = account.baseAddress(provider.network)

        val address = Address.fromBech32(targetAddress)
        val scriptRef = Some(ScriptRef(script))
        val draftOutput = TransactionOutput.Babbage(address, Value.lovelace(0), None, scriptRef)
        val minCoin = MinCoinSizedTransactionOutput.ensureMinAda(
          Sized(draftOutput),
          provider.cardanoInfo.protocolParams
        )
        val output = TransactionOutput.Babbage(address, Value(minCoin), None, scriptRef)

        val tx = TxBuilder(provider.cardanoInfo)
            .output(output)
            .complete(provider, senderAddress)
            .await(Timeout)
            .sign(new TransactionSigner(Set(account.paymentKeyPair)))
            .transaction

        provider.submit(tx).await(Timeout) match
            case Right(txHash) => txHash.toHex
            case Left(error)   => throw RuntimeException(s"Deploy failed: ${error.message}")
    }
}
